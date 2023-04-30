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
end

SyntaxKinds.kind(event::Event) = event.kind

"""
Internal state used in parsing a stream of Tokens. Use via `parse(source::AbstractString)`.
"""
mutable struct Parser
    # Parsing builds on and consumes a Tokenizer.
    _tokenizer::Tokenize.Tokenizer
    # The Parser builds up a sequence of parsed tokens.
    _parsed_tokens::Vector{Tokenize.Token}
    # Parser output as an ordered sequence of ranges, parent nodes after children.
    _events::Vector{Event}
    # Token lookahead buffer.
    _lookahead_index::Int
    _lookahead_tokens::Vector{Tokenize.Token}
    # Counter for number of peek()s we've done without making progress via a bump().
    # Used to avoid parsing freezing if we accidentally hit infinite recursion/looping.
    _peek_count::Int

    function Parser(source::AbstractString)
        new(
            Tokenize.Tokenizer(source),
            Vector{Tokenize.Token}(),
            Vector{Event}(),
            1,
            Vector{Tokenize.Token}(),
            0,
        )
    end
end

tokens(parser::Parser) = parser._parsed_tokens
events(parser::Parser) = parser._events

# Get position of last item emitted into the output stream
function Base.position(parser::Parser)
    Position(lastindex(parser._parsed_tokens), lastindex(parser._events))
end


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
function _next_nontrivia_index(parser::Parser)
    i = parser._lookahead_index
    while true
        if i > length(parser._lookahead_tokens)
            _buffer_lookahead_tokens(parser._tokenizer, parser._lookahead_tokens)
        end
        !SyntaxKinds.is_whitespace(parser._lookahead_tokens[i]) && return i
        i += 1
    end
end

@noinline function _parser_stuck_error(parser::Parser)
    # Optimization: emit unlikely errors in a separate function.
    error("The parser seems stuck at byte $(_next_byte(parser))")
end

"""
Look ahead in the token stream, returning the token kind. Comments and whitespace are
skipped automatically.
"""
Base.peek(parser::Parser) = SyntaxKinds.kind(peek_token(parser))

"""
Like `peek`, but return the full token information rather than just the kind.
"""
function peek_token(parser::Parser)
    parser._peek_count += 1
    if parser._peek_count > 100_000
        _parser_stuck_error(parser)
    end
    i = _next_nontrivia_index(parser)
    return @inbounds parser._lookahead_tokens[i]
end

function current_token(parser::Parser)
    return last(parser._parsed_tokens)
end

# Return the index of the next byte of the input.
function last_byte(parser::Parser)
    t = current_token(parser)
    # Handle edge-case of zero-length tokens.
    max(JuLox.startbyte(t), JuLox.endbyte(t))
end

_next_byte(parser::Parser) = _next_byte(parser) + 1


#-------------------------------------------------------------------------------
# Defining `bump()`.

# Bump up until the `n`th token.
function _bump_until_n(parser::Parser, n::Integer)
    if n < parser._lookahead_index
        return nothing
    end
    append!(parser._parsed_tokens, parser._lookahead_tokens[parser._lookahead_index:n])
    parser._lookahead_index = n + 1
    # Defuse the time bomb.
    parser._peek_count = 0
    return nothing
end

"""
Advance the token position so that the next nontrivia token is finalized.
"""
function bump(parser::Parser)
    _bump_until_n(parser, _next_nontrivia_index(parser))
    return position(parser)
end


"""
Bump into the outputs a zero-length error token created by the parsing process.

This is useful when a specific token is expected but missing, while error-kind events
are useful for other kinds of invalid syntax (like assinging values to a non-variable).
"""
function bump_error(parser::Parser, err_kind::SyntaxKinds.Kind)
    @assert SyntaxKinds.is_error(err_kind)
    err_token = Tokenize.Token(err_kind, last_byte(parser), "")
    push!(parser._parsed_tokens, err_token)
end

#-------------------------------------------------------------------------------
# Defining `emit()`.

"""
    emit(parser, mark, kind)

Emit a new Event into the output which covers tokens from `mark` to
the end of the most recent token which was `bump()`'ed. The starting `mark`
should be a previous return value of `position()`.
"""
function emit(parser::Parser, mark::Position, kind::SyntaxKinds.Kind)
    first_token_index = mark.token_index + 1
    last_token_index = lastindex(parser._parsed_tokens)
    event = Event(kind, first_token_index, last_token_index)
    push!(parser._events, event)
    return position(parser)
end

#-------------------------------------------------------------------------------
# The actual logic for recursive descent parsing.

function consume(parser::Parser, expected::SyntaxKinds.Kind, error_kind::SyntaxKinds.Kind)
    if peek(parser) == expected
        bump(parser)
        return true
    else
        bump_error(parser, error_kind)
        recover(parser)
        return false
    end
end

"""Recover from invalid synax by trying to find the start of the next statement."""
function recover(parser::Parser)
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
    # Label dumped tokens with a special recovery event.
    if position(parser).token_index > mark.token_index
        emit(parser, mark, K"UnparsedErrorRecovery")
    end
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
    if consume(parser, K"Identifier", K"ErrorInvalidAssigmentTarget")

        # Handle initializer.
        if peek(parser) == K"="
            bump(parser)
            parse_expression(parser)
        end

        # Handle final semicolon.
        consume(parser, K";", K"ErrorStatementMissingSemicolon")
    end
    emit(parser, mark, K"var_decl_statement")
end

function parse_statement(parser::Parser)
    k = peek(parser)
    if k == K"{"
        parse_block(parser)
    elseif k == K"print"
        parse_print_statement(parser)
    elseif k == K"if"
        parse_if_statement(parser)
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
    consume(parser, K"}", K"ErrorBlockMissingClosingBrace")
    emit(parser, mark, K"block")
end

function parse_print_statement(parser::Parser)
    mark = position(parser)

    # Handle the "print".
    @assert SyntaxKinds.kind(peek(parser)) == K"print"
    bump(parser)

    # Parse the expression.
    parse_expression(parser)
    consume(parser, K";", K"ErrorStatementMissingSemicolon")
    emit(parser, mark, K"print_statement")
end

function parse_expression_statement(parser::Parser)
    mark = position(parser)
    parse_expression(parser)
    consume(parser, K";", K"ErrorStatementMissingSemicolon")
    emit(parser, mark, K"expression_statement")
end

function parse_if_statement(parser::Parser)
    mark = position(parser)

    # Handle the "if".
    @assert SyntaxKinds.kind(peek(parser)) == K"if"
    bump(parser)

    # Parse the if conditional.
    consume(parser, K"(", K"ErrorIfMissingOpenParenthesis")
    parse_expression(parser)
    consume(parser, K")", K"ErrorIfMissingClosingParenthesis")

    # Parse then statement.
    parse_statement(parser)

    # Possibly parse else statement.
    if peek(parser) == K"else"
        bump(parser)
        parse_statement(parser)
    end

    # Consume the final semicolon.
    consume(parser, K";", K"ErrorStatementMissingSemicolon")

    # Emit the if statement event.
    emit(parser, mark, K"if_statement")
end

function parse_expression(parser::Parser)
    parse_assignment(parser)
    return nothing
end

function parse_assignment(parser::Parser)
    mark = position(parser)

    # Parse left side of assignment (or only side of non-assignment!)
    parse_or(parser)

    # Check for a right side.
    if peek(parser) == K"="
        mark2 = bump(parser)

        # Parse the right side.
        parse_expression(parser)

        # Look back to left side of assignment to ensure it's an identifier.
        # TODO: Ensure this isn't too hacky compared to the JLox approach.
        left_side_tokens = [
            t for t in parser._parsed_tokens[mark.token_index+1:mark2.token_index] if !SyntaxKinds.is_whitespace(SyntaxKinds.kind(t))
        ]
        @assert SyntaxKinds.kind(left_side_tokens[end]) == K"="
        if length(left_side_tokens) != 2 || SyntaxKinds.kind(left_side_tokens[1]) != K"Identifier"
            emit(parser, mark, K"ErrorInvalidAssigmentTarget")
        else
            # Emit the assignment.
            emit(parser, mark, K"assignment")
        end
    end
end

function parse_infix_syntax(parser::Parser, op_kset::Tuple{Vararg{SyntaxKinds.Kind}}, left_right_parse_fn::Function, syntax_type::SyntaxKinds.Kind)
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
        emit(parser, mark, syntax_type)
    end
    return nothing
end

function parse_infix_operator(parser::Parser, op_kset::Tuple{Vararg{SyntaxKinds.Kind}}, left_right_parse_fn::Function)
    return parse_infix_syntax(parser, op_kset, left_right_parse_fn, K"infix_operation")
end

function parse_logical_operator(parser::Parser, op_kset::Tuple{Vararg{SyntaxKinds.Kind}}, left_right_parse_fn::Function)
    return parse_infix_syntax(parser, op_kset, left_right_parse_fn, K"logical")
end

parse_or(parser::Parser) = parse_logical_operator(parser, KSet"or", parse_and)
parse_and(parser::Parser) = parse_logical_operator(parser, KSet"and", parse_equality)
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
    if k == K"Identifier"
        bump(parser)
        emit(parser, mark, K"variable")
    elseif k ∈ KSet"false true nil Number String"
        bump(parser)
        # We don't actually have to emit a range for a single token item.
    elseif k == K"("
        # Consume the left parenthesis.
        bump(parser)

        # Handle parenthesized expression.
        parse_expression(parser)

        # Parse right parenthesis.
        consume(parser, K")", K"ErrorGroupMissingClosingParenthesis")
        emit(parser, mark, K"grouping")
    else
        # We got all the way down looking for some kind of expression and found nothing.
        bump_error(parser, K"ErrorExpectedExpression")
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
