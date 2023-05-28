
#-------------------------------------------------------------------------------
# Buffering tokens to enable peeking and advancing.

# Since a Tokenize.Tokenizer only exposes an interable API, we need to wrap it if
# we want to be able to peek at future tokens without losing the current one.

# NOTE: We could also just pre-tokenize source code into a `Vector{Token}`, but
# theoretically for very long chunks of source code, this streaming approach could
# be more performant. Either way we'd want to track our current index and create the
# `peek` function for convenience. 

"""
Buffered wrapper of Tokenizer that allows peeking ahead.

Useful for `peek(stream::TokenStream)` returning a token kind without advancing.
"""
mutable struct TokenStream
    _tokenizer::Tokenize.Tokenizer
    _next_index::Int
    _tokens::Vector{Tokenize.Token}
    # Counter for number of peek()s we've done without making progress via a bump().
    # Used to avoid parsing freezing if we accidentally hit infinite recursion/looping.
    _peek_count::Int

    function TokenStream(source::AbstractString)
        new(Tokenize.Tokenizer(source), 1, Vector{Tokenize.Token}(), 0)
    end
end

next_index(stream::TokenStream) = stream._next_index

# Buffer around 100 tokens.
function _load_more_tokens(stream::TokenStream)
    token_count = 0
    while true
        raw = Tokenize.next_token(stream._tokenizer)
        k = SyntaxKinds.kind(raw)
        push!(stream._tokens, raw)
        token_count += 1
        if (k == K"EndMarker") || (!SyntaxKinds.is_whitespace(k) && token_count > 100)
            break
        end
    end
end

# Find the index of the next nontrivia token.
function _index_of_next_nontrivia(stream::TokenStream)
    i = stream._next_index
    while true
        if i > length(stream._tokens)
            _load_more_tokens(stream)
        end
        !SyntaxKinds.is_whitespace(stream._tokens[i]) && return i
        i += 1
    end
end

"""
Advance the TokenStream to index n, returning a view of the tokens we advanced through.
"""
function _advance_to_n(stream::TokenStream, n::Int)
    # If we're past n already, this is a no-op.
    n < stream._next_index && return nothing

    # Get a view of the tokens we're advancing through.
    result = view(stream._tokens, stream._next_index:n)

    # Advance to the next index.
    stream._next_index = n + 1

    # Defuse the time bomb.
    stream._peek_count = 0

    # Return our view.
    return result
end

"""
Advance the token stream, returning a view of all the tokens we've advanced through.
Comments and whitespace are skipped automatically.
"""
function advance(stream::TokenStream)
    return _advance_to_n(stream, _index_of_next_nontrivia(stream))
end


"""
Look ahead in the token stream, returning the token kind.
Comments and whitespace are skipped automatically.
"""
Base.peek(stream::TokenStream) = SyntaxKinds.kind(peek_token(stream))

"""
Like `peek`, but return the full token information rather than just the kind.
"""
function peek_token(stream::TokenStream)
    stream._peek_count += 1
    stream._peek_count > 100_000 && _stuck_peeking_error(stream)
    i = _index_of_next_nontrivia(stream)
    return @inbounds stream._tokens[i]
end

# Optimization: emit unlikely errors in a separate function.
@noinline function _stuck_peeking_error(stream::TokenStream)
    finished_text = join(String[Tokenize.text(t) for t in _finished_tokens(stream)])
    text_excerpt = finished_text[max(1, length(finished_text) - 30):end]
    error("We got stuck at byte $(_next_byte(stream)), just after `$(text_excerpt)`")
end

_finished_tokens(stream::TokenStream) = stream._tokens[1:stream._next_index]
_next_byte(stream::TokenStream) = last_finished_byte(stream) + 1

# Return the index of the last byte of the input that has been tokenized.
function last_finished_byte(stream::TokenStream)
    finished = _finished_tokens(stream)

    # Edge case: just starting.
    isempty(finished) && return 0

    t = last(finished)
    result = JuLox.endbyte(t)

    # Handle edge-case of zero-length tokens.
    if JuLox.startbyte(t) > result
        result = JuLox.startbyte(t)
    end

    return result
end


#-------------------------------------------------------------------------------
# The Parser struct itself, with its core functionality. 

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
    # Parsing builds on and consumes a TokenStream.
    _token_stream::TokenStream
    # The Parser builds up a sequence of parsed tokens.
    _parsed_tokens::Vector{Tokenize.Token}
    # Parser output as an ordered sequence of ranges, parent nodes after children.
    _events::Vector{Event}
    # Panic mode used to avoid cascaded errors.
    _supress_cascaded_errors::Bool

    function Parser(source::AbstractString)
        new(TokenStream(source), Vector{Tokenize.Token}(), Vector{Event}(), false)
    end
end

tokens(parser::Parser) = parser._parsed_tokens
events(parser::Parser) = parser._events
is_errors_suppressed(parser::Parser) = parser._supress_cascaded_errors
function suppress_errors!(parser::Parser)
    parser._supress_cascaded_errors = true
end
function unsuppress_errors!(parser::Parser)
    parser._supress_cascaded_errors = false
end

# Get position of last item emitted into the output stream
function Base.position(parser::Parser)
    Position(lastindex(parser._parsed_tokens), lastindex(parser._events))
end

# Pass through the token stream's peeking functionality.
Base.peek(parser::Parser) = peek(parser._token_stream)
peek_token(parser::Parser) = peek_token(parser._token_stream)

# Define the functionality for `bump()`.

"""
Advance the token position so that the next nontrivia token is finalized.
"""
function bump(parser::Parser)
    tokens = advance(parser._token_stream)
    append!(parser._parsed_tokens, tokens)
    return position(parser)
end

"""
Bump into the outputs a zero-length implied token created by the parsing process.

This is useful when certain syntax implies a null expression, like a variable assignment
or for loop without an initializer.
"""
function bump_implied(parser::Parser, kind::SyntaxKinds.Kind)
    token = Tokenize.Token(kind, last_finished_byte(parser._token_stream), "")
    push!(parser._parsed_tokens, token)
end

"""
Bump into the outputs a zero-length error token created by the parsing process.

This is useful when a specific token is expected but missing, while error-kind events
are useful for other kinds of invalid syntax (like assinging values to a non-variable).
"""
function bump_error(parser::Parser, err_kind::SyntaxKinds.Kind)
    @assert SyntaxKinds.is_error(err_kind)
    is_errors_suppressed(parser) && return nothing
    bump_implied(parser, err_kind)
end

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

function emit_error(parser::Parser, mark::Position, kind::SyntaxKinds.Kind)
    @assert SyntaxKinds.is_error(err_kind)
    is_errors_suppressed(parser) && return nothing
    emit(parser, mark, kind)
end
