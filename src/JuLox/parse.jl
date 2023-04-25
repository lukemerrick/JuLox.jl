# A sequence-based interface for parsing Lox tokens.
#
# Rather than directly constructing a sytax tree, this Lox parser
# produces syntax "events" that enable later lossless tree creation.
# (or lossy tree creation, if trivia tokens are filtered!)

module Parse
using Fractal.JuLox: JuLox, SyntaxKinds, Tokenize
using Fractal.JuLox.SyntaxKinds: @K_str, @KSet_str


#-------------------------------------------------------------------------------
# Building up the Parser struct. 

struct Position
    token_index::Int  # Index of last token in output.
    event_index::Int  # Index of last event in output.
end

"""
Events represent syntax that in a tree are stored as nodes.
"""
struct Event
    kind::SyntaxKinds.Kind
    first_token::Int
    last_token::Int
    error_message::String
end

SyntaxKinds.kind(event::Event) = event.kind
error_message(event::Event) = event.error_message

"""
Internal state used in parsing a stream of Tokens. Use via `parse(source::AbstractString)`.
"""
mutable struct Parser
    # Parsing builds on and consumes a Tokenizer.
    _tokenizer::Tokenize.Tokenizer
    # Internal position tracking for parsing.
    _finished_token_index::Int
    _lookahead_index::Int
    # The Parser builds up a sequence of tokens.
    # Some of these tokens may not yet be parsed but are a lookahead buffer.
    _tokens::Vector{Tokenize.Token}
    # Parser output as an ordered sequence of ranges, parent nodes after children.
    _events::Vector{Event}
    # Counter for number of peek()s we've done without making progress via a bump().
    # Used to avoid parsing freezing if we accidentally hit infinite recursion/looping.
    _peek_count::Int

    function Parser(source::AbstractString)
        new(Tokenize.Tokenizer(source), 0, 1, Vector{Tokenize.Token}(), Vector{Event}(), 0)
    end
end

tokens(parser::Parser) = parser._tokens[1:parser._finished_token_index]
events(parser::Parser) = parser._events


#-------------------------------------------------------------------------------
# Defining `peek()` and related functions.

# Buffer around 100 tokens.
function _buffer_lookahead_tokens(lexer::Tokenize.Tokenizer, tokens::Vector{Tokenize.Token})
    token_count = 0
    while true
        raw = Tokenize.next_token(lexer)
        k = SyntaxKinds.kind(raw)
        push!(tokens, raw)
        token_count += 1
        if (k == K"EndMarker") || (!SyntaxKinds.is_whitespace(k) && token_count > 100)
            break
        end
    end
end

# Find the index of the next nontrivia token.
function _lookahead_index(stream::Parser)
    i = stream._lookahead_index
    while true
        if i > length(stream._tokens)
            _buffer_lookahead_tokens(stream._tokenizer, stream._tokens)
        end
        !SyntaxKinds.is_whitespace(stream._tokens[i]) && return i
        i += 1
    end
end

@noinline function _parser_stuck_error(stream::Parser)
    # Optimization: emit unlikely errors in a separate function.
    error("The parser seems stuck at byte $(_next_byte(stream))")
end

"""
Look ahead in the stream, returning the token kind. Comments and whitespace are skipped
automatically.
"""
Base.peek(stream::Parser) = SyntaxKinds.kind(peek_token(stream))

"""
Like `peek`, but return the full token information rather than just the kind.
"""
function peek_token(stream::Parser)
    stream._peek_count += 1
    if stream._peek_count > 100_000
        _parser_stuck_error(stream)
    end
    i = _lookahead_index(stream)
    return @inbounds stream._tokens[i]
end

function current_token(stream::Parser)
    return stream._tokens[stream._finished_token_index]
end

# Return the index of the next byte of the input.
function last_byte(stream::Parser)
    JuLox.endbyte(current_token(stream))
end

_next_byte(stream::Parser) = _next_byte(stream) + 1


#-------------------------------------------------------------------------------
# Defining `bump()`.

# Bump up until the `n`th token.
function _bump_until_n(stream::Parser, n::Integer)
    if n < stream._lookahead_index
        return nothing
    end
    stream._finished_token_index = n
    stream._lookahead_index = n + 1
    # Defuse the time bomb.
    stream._peek_count = 0
    return nothing
end

"""
Advance the stream's token position so that the next nontrivia token is finalized.
"""
function bump(stream::Parser)
    _bump_until_n(stream, _lookahead_index(stream))
    return position(stream)
end

#-------------------------------------------------------------------------------
# Defining `emit()`.

# Get position of last item emitted into the output stream
function Base.position(stream::Parser)
    Position(stream._finished_token_index, lastindex(stream._events))
end

"""
    emit(stream, mark, kind, [error_message=""])

Emit a new Event into the output which covers tokens from `mark` to
the end of the most recent token which was `bump()`'ed. The starting `mark`
should be a previous return value of `position()`.
"""
function emit(stream::Parser, mark::Position, kind::SyntaxKinds.Kind, error_message::String="")
    first_token = mark.token_index + 1
    event = Event(kind, first_token, stream._finished_token_index, error_message)
    push!(stream._events, event)
    return position(stream)
end

#-------------------------------------------------------------------------------
# The actual logic for recursive descent parsing.

function consume(parser::Parser, expected::SyntaxKinds.Kind)
    if peek(parser) == expected
        bump(parser)
        return true
    else
        recover(parser, "Expected '$(convert(String, expected))' not found.")
        return false
    end
end

"""Recover from invalid synax by trying to find the start of the next statement."""
function recover(parser::Parser, error_msg::String)
    mark = position(parser)
    while (k = peek(parser)) != K"EndMarker"

        # If we hit a semicolon, we probably start a statement just after.
        if k == K";"
            bump(parser)
            break
        end

        # If we hit one of these keywords, we probably start a statement with it.
        if k ∈ KSet"class fun var for if while print return"
            break
        end

        # Keep bumping and dumping, otherwise!
        bump(parser)
    end
    emit(parser, mark, K"error", error_msg)
    return nothing
end

function parse_toplevel(parser::Parser)
    mark = position(parser)
    while peek(parser) != K"EndMarker"
        parse_declaration(parser)
    end
    emit(parser, mark, K"toplevel")
end

function parse_declaration(parser::Parser)
    if peek(parser) == K"var"
        parse_var_declaration(parser)
    else
        parse_statement(parser)
    end
end

function parse_var_declaration(parser::Parser)
    mark = position(parser)
    bump(parser)  # K"var" token
    if consume(parser, K"Identifier")

        # Handle initializer.
        if peek(parser) == K"="
            bump(parser)
            parse_expression(parser)
        end

        consume(parser, K";") && emit(parser, mark, K"var_decl_statement")
    end
end

function parse_statement(parser::Parser)
    k = peek(parser)
    if k == K"{"
        parse_block(parser)
    elseif k == K"print"
        parse_print_statement(parser)
    else
        parse_expression_statement(parser)
    end
end

function parse_block(parser::Parser)
    mark = position(parser)

    # Handle the "{"
    @assert SyntaxKinds.kind(peek(parser)) == K"{"
    bump(parser)

    # Parse a list of statements.
    while peek(parser) ∉ KSet"} EndMarker"
        parse_declaration(parser)
    end

    # Handle the "}"
    consume(parser, K"}") && emit(parser, mark, K"block")
end

function parse_print_statement(parser::Parser)
    mark = position(parser)

    # Handle the "print".
    @assert SyntaxKinds.kind(peek(parser)) == K"print"
    bump(parser)

    # Parse the expression.
    parse_expression(parser)
    consume(parser, K";") && emit(parser, mark, K"print_statement")
end

function parse_expression_statement(parser::Parser)
    mark = position(parser)
    parse_expression(parser)
    consume(parser, K";") && emit(parser, mark, K"expression_statement")
end

function parse_expression(parser::Parser)
    parse_assignment(parser)
    return nothing
end

function parse_assignment(parser::Parser)
    mark = position(parser)

    # Parse left side of assignment (or only side of non-assignment!)
    parse_equality(parser)

    # Check for a right side.
    if peek(parser) == K"="
        mark2 = bump(parser)

        # Parse the right side.
        parse_expression(parser)

        # Look back to left side of assignment to ensure it's an identifier.
        # TODO: Ensure this isn't too hacky compared to the JLox approach.
        left_side_tokens = [
            t for t in parser._tokens[mark.token_index+1:mark2.token_index] if !SyntaxKinds.is_whitespace(SyntaxKinds.kind(t))
        ]
        @assert SyntaxKinds.kind(left_side_tokens[end]) == K"="
        if length(left_side_tokens) != 2 || SyntaxKinds.kind(left_side_tokens[1]) != K"Identifier"
            emit(parser, mark, K"error", "Invalid assignment target.")
        else
            # Emit the assignment.
            emit(parser, mark, K"assignment")
        end
    end
end

function parse_infix_operator(parser::Parser, op_kset::Tuple{Vararg{SyntaxKinds.Kind}}, left_right_parse_fn::Function)
    mark = position(parser)

    # Parse the left operand.
    left_right_parse_fn(parser)

    # Parse zero or more operators matching `op_kset``.
    # The entire expression built up becomes the left operand of the next operation parsed.
    while (k = peek(parser)) ∈ op_kset
        # Consume the operator.
        bump(parser)

        # Parse the right operatnd.
        left_right_parse_fn(parser)

        # Emit the operand covering from start of left through end of right.
        emit(parser, mark, K"infix_operation")
    end
    return nothing
end

parse_equality(parser::Parser) = parse_infix_operator(parser, KSet"!= ==", parse_comparison)
parse_comparison(parser::Parser) = parse_infix_operator(parser, KSet"> >= < <=", parse_term)
parse_term(parser::Parser) = parse_infix_operator(parser, KSet"+ -", parse_factor)
parse_factor(parser::Parser) = parse_infix_operator(parser, KSet"* /", parse_unary)

function parse_unary(parser::Parser)
    if peek(parser) ∈ KSet"! -"
        mark = position(parser)
        bump(parser)
        parse_unary(parser)
        emit(parser, mark, K"unary")
    else
        parse_primary(parser)
    end
end

function parse_primary(parser::Parser)
    SyntaxKinds.is_error(peek(parser)) && bump(parser)  # Pass through errors.
    mark = position(parser)
    k = peek(parser)
    if k ∈ KSet"false true nil Number String Identifier"
        bump(parser)
        # We don't actually have to emit a range for a single token item.
    elseif k == K"("
        # Consume the left parenthesis.
        bump(parser)

        # Handle parenthesized expression.
        parse_expression(parser)

        # Parse right parenthesis.
        consume(parser, K")") && emit(parser, mark, K"grouping")
    else
        # We got all the way down looking for some kind of expression and found nothing.
        emit(parser, mark, K"error", "Expect expression.")
    end
end

#-------------------------------------------------------------------------------
# The (very small) public API.

struct ParseResult
    tokens::Vector{Tokenize.Token}
    events::Vector{Event}
end

function parse_lox(text::AbstractString)
    parser = Parser(text)
    parse_toplevel(parser)
    return ParseResult(tokens(parser), events(parser))
end

end  # end module Parse
