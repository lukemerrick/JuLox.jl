# A streaming interface for parsing Lox tokens.
#
# Rather than directly constructing a single sytax tree, this Lox parser
# produces syntax "ranges" that enable various types of tree creation, including
# lossless source tree creation.


#-------------------------------------------------------------------------------
# Building up the ParseStream struct. 

struct Diagnostic
    _first_byte::Int
    _last_byte::Int
    _message::String
end

first_byte(d::Diagnostic) = d._first_byte
last_byte(d::Diagnostic) = d._last_byte
message(d::Diagnostic) = d._message
Base.range(d::Diagnostic) = first_byte(d):last_byte(d)

struct ParseStreamPosition
    token_index::Int  # Index of last token in output
    range_index::Int
end

"""
Range in the source text which will become a node in the tree. Can be either a
token (leaf node of the tree) or an interior node, depending on how the
start_mark compares to previous nodes.
"""
struct TaggedRange
    kind::Kind
    # The following field is used for one of two things:
    # - For leaf nodes it's an index in the tokens array
    # - For non-leaf nodes it points to the index of the first child
    first_token::Int
    last_token::Int
end

kind(r::TaggedRange) = r.kind

"""
    ParseStream(text::AbstractString,          index::Integer=1)
    ParseStream(text::IO;                                      )
    ParseStream(text::Vector{UInt8},           index::Integer=1)
    ParseStream(ptr::Ptr{UInt8}, len::Integer, index::Integer=1)

Construct a `ParseStream` from input which may come in various forms:
* An string (zero copy for `String` and `SubString`)
* An `IO` object (zero copy for `IOBuffer`). The `IO` object must be seekable.
* A buffer of bytes (zero copy). The caller is responsible for preserving
  buffers passed as `(ptr,len)`.

A byte `index` may be provided as the position to start parsing.

ParseStream provides an IO interface for the parser which provides lexing of
the source text input into tokens, manages insignificant whitespace tokens on
behalf of the parser, and stores output tokens and tree nodes in a pair of
output arrays.
"""
mutable struct ParseStream
    # `textbuf` is a buffer of UTF-8 encoded text of the source code. This is a
    # natural representation as we desire random access and zero-copy parsing
    # of UTF-8 text from various containers, and unsafe_wrap(Vector{UInt8},
    # ...) allows us to use a Vector here.
    #
    # We want `ParseStream` to be concrete so that all `parse_*` functions only
    # need to be compiled once. Thus `textbuf` must not be parameterized here.
    _text_buf::Vector{UInt8}
    # GC root for the object which owns the memory in `textbuf`. `nothing` if
    # the `textbuf` owner was unknown (eg, ptr,length was passed)
    _text_root::Any
    # Lexer, transforming the input bytes into a token stream
    _lexer::Tokenize.Tokenizer{IOBuffer}
    # Track position in tokens
    _finished_token_index::Int
    _lookahead_index::Int
    # Buffer of tokens
    _tokens::Vector{Token}
    # Parser output as an ordered sequence of ranges, parent nodes after children.
    _ranges::Vector{TaggedRange}
    # Parsing diagnostics (errors/warnings etc)
    _diagnostics::Vector{Diagnostic}
    # Counter for number of peek()s we've done without making progress via a bump()
    _peek_count::Int

    function ParseStream(text_buf::Vector{UInt8}, text_root, next_byte::Integer)
        io = IOBuffer(text_buf)
        seek(io, next_byte - 1)
        lexer = Tokenize.Tokenizer(io)
        new(
            text_buf,
            text_root,
            lexer,
            0,
            1,
            Vector{Token}(),
            Vector{TaggedRange}(),
            Vector{Diagnostic}(),
            0,
        )
    end
end

# Constructor for a directly-passed Vector{UInt8} buffer.
function ParseStream(text::Vector{UInt8}, index::Integer=1)
    return ParseStream(text, text, index)
end

# Constructor for buffers originating from strings.
function ParseStream(text::String, index::Integer=1)
    return ParseStream(unsafe_wrap(Vector{UInt8}, text), text, index)
end
function ParseStream(text::SubString, index::Integer=1)
    # See also IOBuffer(SubString("x"))
    return ParseStream(unsafe_wrap(Vector{UInt8}, pointer(text), sizeof(text)), text, index)
end
function ParseStream(text::AbstractString, index::Integer=1)
    return ParseStream(String(text), index)
end

#-------------------------------------------------------------------------------
# Diagnostic functionality.

function show_diagnostic(io::IO, diagnostic::Diagnostic, source::String)
    # Figure out the location of the issue.
    lines = split(source[1:first_byte(diagnostic)], '\n')
    linecol = "[line $(length(lines)), column $(length(lines[end]))]"

    # Print the error.
    println(io, "# Error @ $linecol - $(message(diagnostic))")

    return nothing
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, source::String)
    return show_diagnostic.(Ref(io), diagnostics, Ref(source))
end

function emit_diagnostic(stream::ParseStream, byterange::AbstractUnitRange, message)
    push!(stream._diagnostics, Diagnostic(first(byterange), last(byterange), message))
    return nothing
end

"""
Emit a diagnostic at the position of the next token.
"""
function emit_diagnostic(stream::ParseStream, message)
    t = peek_token(stream)
    emit_diagnostic(stream, startbyte(t):endbyte(t), message)
    return nothing
end

function show_diagnostics(io::IO, stream::ParseStream)
    show_diagnostics(io, stream._diagnostics, sourcetext(stream))
end


#-------------------------------------------------------------------------------
# Defining `peek()`.

# Buffer several tokens ahead
function _buffer_lookahead_tokens(lexer::Tokenize.Tokenizer, tokens)
    token_count = 0
    while true
        raw = Tokenize.next_token(lexer)
        k = kind(raw)
        push!(tokens, raw)
        token_count += 1
        if k == K"EndMarker"
            break
        end
        if !is_whitespace(k) && token_count > 100
            # Buffer tokens in batches for lookahead. Generally we want a
            # moderate-size buffer to make sure we hit the fast path of peek(),
            # but not too large to avoid (a) polluting the processor cache and
            # (b) doing unnecessary work when not parsing the whole input.
            break
        end
    end
end

# Return the index of the next byte of the input.
function _next_byte(stream::ParseStream)
    endbyte(stream._tokens[stream._finished_token_index]) + 1
end

last_byte(stream::ParseStream) = _next_byte(stream) - 1

# Find the index of the next nontrivia token.
@inline function _lookahead_index(stream::ParseStream, n::Integer)
    # Much of the time we'll be peeking ahead a single token and have one or
    # zero whitespace tokens before the next token. The following code is an
    # unrolled optimized version for that fast path.
    i = stream._lookahead_index
    @inbounds if n == 1 && i + 2 <= length(stream._tokens)
        if !is_whitespace(stream._tokens[i])
            return i
        end
        i += 1
        if !is_whitespace(stream._tokens[i])
            return i
        end
    end
    # Fall through to the general case.
    return __lookahead_index(stream, n)
end

@noinline function __lookahead_index(stream::ParseStream, n::Integer)
    i = stream._lookahead_index
    while true
        if i > length(stream._tokens)
            _buffer_lookahead_tokens(stream._lexer, stream._tokens)
        end
        if !is_whitespace(stream._tokens[i])
            if n == 1
                return i
            end
            n -= 1
        end
        i += 1
    end
end

@noinline function _parser_stuck_error(stream::ParseStream)
    # Optimization: emit unlikely errors in a separate function.
    error("The parser seems stuck at byte $(_next_byte(stream))")
end

"""
    peek(stream [, n=1])

Look ahead in the stream `n` tokens, returning the token kind. Comments and whitespace are
skipped automatically.
"""
function Base.peek(stream::ParseStream, n::Integer=1)
    kind(peek_token(stream, n))
end

"""
    peek_token(stream [, n=1])

Like `peek`, but return the full token information rather than just the kind.
"""
function peek_token(stream::ParseStream, n::Integer=1)
    stream._peek_count += 1
    if stream._peek_count > 100_000
        _parser_stuck_error(stream)
    end
    i = _lookahead_index(stream, n)
    return @inbounds stream._tokens[i]
end

function current_token(stream::ParseStream)
    return stream._tokens[stream._finished_token_index]
end


#-------------------------------------------------------------------------------
# Defining `bump()`.

# Bump up until the `n`th token
function _bump_until_n(stream::ParseStream, n::Integer)
    if n < stream._lookahead_index
        return
    end
    stream._finished_token_index = n
    stream._lookahead_index = n + 1
    # Defuse the time bomb
    stream._peek_count = 0
end

"""
    bump(stream; error)

Advance the stream's token position so that the next nontrivia token is finalized.

Keyword arguments:
* `error` - if set, emit an error for this token
"""
function bump(stream::ParseStream; error=nothing)
    emark = position(stream)
    _bump_until_n(stream, _lookahead_index(stream, 1))
    if !isnothing(error)
        emit(stream, emark, K"error", error=error)
    end
    return position(stream)
end

#-------------------------------------------------------------------------------
# Defining `emit()`.

# Get position of last item emitted into the output stream
function Base.position(stream::ParseStream)
    ParseStreamPosition(stream._finished_token_index, lastindex(stream._ranges))
end

"""
    emit(stream, mark, kind; error=nothing)

Emit a new text span into the output which covers source bytes from `mark` to
the end of the most recent token which was `bump()`'ed. The starting `mark`
should be a previous return value of `position()`.
"""
function emit(stream::ParseStream, mark::ParseStreamPosition, kind::Kind; error=nothing)
    first_token = mark.token_index + 1
    range = TaggedRange(kind, first_token, stream._finished_token_index)
    if !isnothing(error)
        # The first child must be a leaf, otherwise ranges would be improperly
        # nested.
        fbyte = startbyte(stream._tokens[first_token])
        lbyte = endbyte(stream._tokens[stream._finished_token_index])
        # If the `first_token` is the EndMarker, we need to adjust.
        fbyte = min(fbyte, length(stream._text_buf))
        emit_diagnostic(stream, fbyte:lbyte, error)
    end
    push!(stream._ranges, range)
    return position(stream)
end
