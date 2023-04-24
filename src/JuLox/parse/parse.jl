"""Parsing -- from Tokens to ParseStream to AST. Adapted from JuliaSyntax.jl on 2023-04-09."""

module Parse
using Fractal: JuLox
using Fractal.JuLox.Tokenize: Tokenize, RawToken, startbyte, endbyte
using Fractal.JuLox: Kind, @K_str, @KSet_str, is_whitespace, is_error, kind, span
import Fractal.JuLox: kind, _token_error_descriptions

include("_parse_stream.jl")
include("_syntax_trees.jl")


#-------------------------------------------------------------------------------
# Parser.

function consume(ps::ParseStream, expected::Kind)
    if peek(ps) == expected
        bump(ps)
        return true
    else
        recover(ps, "Expected '$(convert(String, expected))' not found.")
        return false
    end
end

"""Recover from invalid synax by trying to find the start of the next statement."""
# NOTE: Since we aren't throwing exceptions in the `parse_` functions, we need to use
# the JuliaSyntax.jl appraoch and call `recover` in all places where we would throw
# and exception, rather than calling it once in a big wrapping try-catch block.
function recover(ps::ParseStream, error_msg::String)
    mark = position(ps)
    while (k = peek(ps)) != K"EndMarker"

        # If we hit a semicolon, we probably start a statement just after.
        if k == K";"
            bump(ps)
            break
        end

        # If we hit one of these keywords, we probably start a statement with it.
        if k ∈ KSet"class fun var for if while print return"
            break
        end

        # Keep bumping and dumping, otherwise!
        bump(ps)
    end
    emit(ps, mark, K"error"; error=error_msg)
    return nothing
end

function parse_toplevel(ps::ParseStream)
    mark = position(ps)
    while peek(ps) != K"EndMarker"
        parse_declaration(ps)
    end
    emit(ps, mark, K"toplevel")
end

function parse_declaration(ps::ParseStream)
    if peek(ps) == K"var"
        parse_var_declaration(ps)
    else
        parse_statement(ps)
    end
end

function parse_var_declaration(ps::ParseStream)
    mark = position(ps)
    bump(ps)  # K"var" token
    if consume(ps, K"Identifier")

        # Handle initializer.
        if peek(ps) == K"="
            bump(ps)
            parse_expression(ps)
        end

        consume(ps, K";") && emit(ps, mark, K"var_decl_statement")
    end
end

function parse_statement(ps::ParseStream)
    k = peek(ps)
    if k == K"{"
        parse_block(ps)
    elseif k == K"print"
        parse_print_statement(ps)
    else
        parse_expression_statement(ps)
    end
end

function parse_block(ps::ParseStream)
    mark = position(ps)

    # Handle the "{"
    @assert kind(peek(ps)) == K"{"
    bump(ps)

    # Parse a list of statements.
    while peek(ps) ∉ KSet"} EndMarker"
        parse_declaration(ps)
    end

    # Handle the "}"
    consume(ps, K"}") && emit(ps, mark, K"block")
end


function parse_print_statement(ps::ParseStream)
    mark = position(ps)

    # Handle the "print".
    @assert kind(peek(ps)) == K"print"
    bump(ps)

    # Parse the expression.
    parse_expression(ps)
    consume(ps, K";") && emit(ps, mark, K"print_statement")
end

function parse_expression_statement(ps::ParseStream)
    mark = position(ps)
    parse_expression(ps)
    consume(ps, K";") && emit(ps, mark, K"expression_statement")
end

function parse_expression(ps::ParseStream)
    parse_assignment(ps)
    return nothing
end

function parse_assignment(ps::ParseStream)
    mark = position(ps)

    # Parse left side of assignment (or only side of non-assignment!)
    parse_equality(ps)

    # Check for a right side.
    if peek(ps) == K"="
        mark2 = bump(ps)

        # Parse the right side.
        parse_expression(ps)

        # Look back to left side of assignment to ensure it's an identifier.
        # TODO: Ensure this isn't too hacky compared to the JLox approach.
        left_side_tokens = [
            t for t in ps._tokens[mark.token_index+1:mark2.token_index] if !is_whitespace(kind(t))
        ]
        @assert kind(left_side_tokens[end]) == K"="
        if length(left_side_tokens) != 2 || kind(left_side_tokens[1]) != K"Identifier"
            emit(ps, mark, K"error"; error="Invalid assignment target.")
        else
            # Emit the assignment.
            emit(ps, mark, K"assignment")
        end
    end
end

function parse_infix_operator(ps::ParseStream, op_kset::Tuple{Vararg{Kind}}, left_right_parse_fn::Function)
    mark = position(ps)

    # Parse the left operand.
    left_right_parse_fn(ps)

    # Parse zero or more operators matching `op_kset``.
    # The entire expression built up becomes the left operand of the next operation parsed.
    while (k = peek(ps)) ∈ op_kset
        # Consume the operator.
        bump(ps)

        # Parse the right operatnd.
        left_right_parse_fn(ps)

        # Emit the operand covering from start of left through end of right.
        emit(ps, mark, K"infix_operation")
    end
    return nothing
end

parse_equality(ps::ParseStream) = parse_infix_operator(ps, KSet"!= ==", parse_comparison)
parse_comparison(ps::ParseStream) = parse_infix_operator(ps, KSet"> >= < <=", parse_term)
parse_term(ps::ParseStream) = parse_infix_operator(ps, KSet"+ -", parse_factor)
parse_factor(ps::ParseStream) = parse_infix_operator(ps, KSet"* /", parse_unary)

function parse_unary(ps::ParseStream)
    if peek(ps) ∈ KSet"! -"
        mark = position(ps)
        bump(ps)
        parse_unary(ps)
        emit(ps, mark, K"unary")
    else
        parse_primary(ps)
    end
end

function parse_primary(ps::ParseStream)
    is_error(peek(ps)) && bump(ps)  # Pass through errors.
    mark = position(ps)
    k = peek(ps)
    if k ∈ KSet"false true nil Number String Identifier"
        bump(ps)
        # We don't actually have to emit a range for a single token item.
    elseif k == K"("
        # Consume the left parenthesis.
        bump(ps)

        # Handle parenthesized expression.
        parse_expression(ps)

        # Parse right parenthesis.
        consume(ps, K")") && emit(ps, mark, K"grouping")
    else
        # We got all the way down looking for some kind of expression and found nothing.
        emit(ps, mark, K"error"; error="Expect expression.")
    end
end

#-------------------------------------------------------------------------------
# Main (public) API.

struct ParseError <: Exception
    source::String
    diagnostics::Vector{Diagnostic}
end

function ParseError(stream::ParseStream)
    ParseError(sourcetext(stream), stream._diagnostics)
end

function Base.showerror(io::IO, err::ParseError)
    println(io, "ParseError:")
    show_diagnostics(io, err.diagnostics, err.source)
end

Base.showerror(io::IO, err::ParseError, bt; backtrace=false) = showerror(io, err)
Base.display_error(io::IO, err::ParseError, bt) = showerror(io, err, bt)

# TODO: Re-evaluate simplifying the public API to not expose any streaming functionality.
# TODO: Re-evaluate simplifying to not skip whitespace.
# TODO: Re-evaluate simplifying to single function, rather than parseall, parsestmt, parseatom.
function parseall(::Type{T}, text::AbstractString, index::Int=1) where {T}
    stream = ParseStream(text, index)
    parse_toplevel(stream)
    validate_tokens(stream)
    if peek(stream) != K"EndMarker"
        emit_diagnostic(stream, "Unexpected text after parsing input")
    end
    tree = build_tree(T, stream)
    if !isempty(stream._diagnostics)
        throw(ParseError(stream))
    end
    return tree, last_byte(stream) + 1
end

end  # end module Parse
