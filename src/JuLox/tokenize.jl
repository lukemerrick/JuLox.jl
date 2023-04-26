module Tokenize
using Fractal.JuLox: JuLox, SyntaxKinds
using Fractal.JuLox.SyntaxKinds: @K_str

# Create EOF sentinel value.
const EOF_CHAR = typemax(Char)

#-------------------------------------------------------------------------------
# Keyword matching.

const MAX_KW_LENGTH = 6

# This creates a hash for chars in [a-z] using 5 bit per char.
# Requires an additional input-length check somewhere, because
# this only works up to ~12 chars.
@inline function simple_hash(c::Char, h::UInt64)
    bytehash = (clamp(c - 'a' + 1, -1, 30) % UInt8) & 0x1f
    h << 5 + bytehash
end

function simple_hash(str)
    ind = 1
    h = UInt64(0)
    while ind <= length(str)
        h = simple_hash(str[ind], h)
        ind = nextind(str, ind)
    end
    h
end

const kw_hash = Dict(
    simple_hash(lowercase(string(kw))) => kw
    for kw in [
        K"and"
        K"class"
        K"else"
        K"false"
        K"fun"
        K"for"
        K"if"
        K"nil"
        K"or"
        K"print"
        K"return"
        K"super"
        K"this"
        K"true"
        K"var"
        K"while"
    ]
)

#-------------------------------------------------------------------------------
# Token.

struct Token
    _kind::SyntaxKinds.Kind
    _startbyte::Int # The byte where the token start in the buffer
    _text::String
end
Token() = Token(K"error", 0, "")

SyntaxKinds.kind(t::Token) = t._kind
JuLox.startbyte(t::Token) = t._startbyte
JuLox.endbyte(t::Token) = t._startbyte + length(t._text) - 1
text(t::Token) = t._text
JuLox.span(token::Token) = JuLox.endbyte(token) - JuLox.startbyte(token) + 1

function Base.show(io::IO, t::Token)
    print(io, rpad(string(JuLox.startbyte(t), "-", JuLox.endbyte(t)), 11, " "))
    print(io, rpad(SyntaxKinds.kind(t), 15, " "))
end

const EMPTY_TOKEN = Token()

#-------------------------------------------------------------------------------
# Tokenizer.

@enum StringState OUTSIDE=1 INSIDE_UNFINISHED=2 INSIDE_FINISHED=3

"""
`Tokenizer` reads from an input stream and emits a single token each time
`next_token` is called.
"""
mutable struct Tokenizer
    _io::IOBuffer
    _token_startpos::Int
    _position::Int
    _chars::Tuple{Char,Char,Char}
    _string_state::StringState
    _token_in_progress::Vector{Char}

    # Initialize Tokenizer from just IO by reading up to the first three characters.
    function Tokenizer(str::AbstractString)
        # We only need to interact with a string at an abstract buffer level.
        io = IOBuffer(str)

        # Fake a current character.
        c1 = ' '

        # Record the start position.
        startpos = position(io)
        currentpos = startpos + 1

        # Read up to two more characters beyond the faked first one.
        if eof(io)
            c2, c3 = EOF_CHAR, EOF_CHAR
        else
            c2 = read(io, Char)
            if eof(io)
                c3 = EOF_CHAR
            else
                c3 = read(io, Char)
            end
        end
        return new(io, startpos, currentpos, (c1, c2, c3), OUTSIDE, Char[])
    end
end

# Pretty printing.
function Base.show(io::IO, tokenizer::Tokenizer)
    print(io, typeof(tokenizer), " at position: ", position(tokenizer))
end

# Implement methods for some other IO/stream functions.
"""
    position(tokenizer::Tokenizer)

Returns the current position.
"""
Base.position(tokenizer::Tokenizer) = tokenizer._position

"""
    eof(tokenizer::Tokenizer)

Determine whether the end of the lexer's underlying buffer has been reached.
"""
Base.eof(tokenizer::Tokenizer) = eof(tokenizer._io)

Base.seek(tokenizer::Tokenizer, pos) = seek(tokenizer._io, pos)

#-------------------------------------------------------------------------------
# Iterator interface.

Base.IteratorSize(::Type{<:Tokenizer}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:Tokenizer}) = Base.HasEltype()
Base.eltype(::Type{<:Tokenizer}) = Token

function Base.iterate(tokenizer::Tokenizer, isdone::Any)
    isdone && return nothing
    t = next_token(tokenizer)
    isdone = SyntaxKinds.kind(t) == K"EndMarker"
    return t, isdone
end
Base.iterate(tokenizer::Tokenizer) = iterate(tokenizer, false)

#-------------------------------------------------------------------------------
# Lexing implementation.

@inline isalpha(c::Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
@inline isalphanumeric(c::Char) = isalpha(c) || isdigit(c)
@inline iswhitespace(c::Char) = Base.isvalid(c) && Base.isspace(c)

startpos(tokenizer::Tokenizer) = tokenizer._token_startpos
peekchar(tokenizer::Tokenizer) = tokenizer._chars[2]
peekchar2(tokenizer::Tokenizer) = tokenizer._chars[3]
string_state(tokenizer::Tokenizer) = tokenizer._string_state

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)
function readchar(tokenizer::Tokenizer)
    c = readchar(tokenizer._io)
    tokenizer._chars = (tokenizer._chars[2], tokenizer._chars[3], c)
    current_char = tokenizer._chars[1]
    push!(tokenizer._token_in_progress, current_char)
    tokenizer._position += 1
    return current_char
end

@inline function accept(tokenizer::Tokenizer, want_char::Char)
    ok = peekchar(tokenizer) == want_char
    ok && readchar(tokenizer)
    return ok
end

"""
    start_token!(tokenizer::Tokenizer)

Updates the Tokenizer's state such that the next `Token` will start at the current
position and the built-up work-in-progress text is cleared.
"""
function start_token!(tokenizer::Tokenizer)
    empty!(tokenizer._token_in_progress)
    tokenizer._token_startpos = position(tokenizer)
end

function emit(tokenizer::Tokenizer, kind::SyntaxKinds.Kind)
    startbyte = startpos(tokenizer)
    text = String(tokenizer._token_in_progress)
    return Token(kind, startbyte, text)
end

# Passthrough with @assert for convenience.
function emit_error(tokenizer::Tokenizer, err_kind::SyntaxKinds.Kind)
    @assert SyntaxKinds.is_error(err_kind)
    return emit(tokenizer, err_kind)
end

"""
    next_token(l::Lexer)

Returns the next `Token`.
"""
function next_token(tokenizer::Tokenizer)::Token
    # Advance starting position to the next token.
    start_token!(tokenizer)

    # If we are in a string, tokenize until the closing quote mark.
    string_state(tokenizer) == INSIDE_UNFINISHED && return tokenize_string(tokenizer)

    # If we are *not* in a string, tokenize normally.
    c = readchar(tokenizer)

    if c == EOF_CHAR
        return emit(tokenizer, K"EndMarker")
    elseif iswhitespace(c)
        return tokenize_whitespace(tokenizer, c)

    elseif c == '('
        return emit(tokenizer, K"(")
    elseif c == ')'
        return emit(tokenizer, K")")
    elseif c == '{'
        return emit(tokenizer, K"{")
    elseif c == '}'
        return emit(tokenizer, K"}")
    elseif c == ','
        return emit(tokenizer, K",")
    elseif c == '.'
        return emit(tokenizer, K".")
    elseif c == '-'
        return emit(tokenizer, K"-")
    elseif c == '+'
        return emit(tokenizer, K"+")
    elseif c == ';'
        return emit(tokenizer, K";")
    elseif c == '*'
        return emit(tokenizer, K"*")

    elseif c == '!'
        return tokenize_exclaim(tokenizer)
    elseif c == '='
        return tokenize_equal(tokenizer)
    elseif c == '<'
        return tokenize_less(tokenizer)
    elseif c == '>'
        return tokenize_greater(tokenizer)

    elseif c == '/'
        return tokenize_forwardslash(tokenizer)
    elseif c == '"'
        return tokenize_quote(tokenizer)
    elseif isdigit(c)
        return tokenize_digit(tokenizer)
    elseif isalpha(c)
        return tokenize_identifier(tokenizer, c)

    else
        emit_error(tokenizer, K"ErrorUnknownCharacter")
    end
end

# Lex string when an opening quote `"` has been tokenized already.
function tokenize_string(tokenizer::Tokenizer)
    @assert string_state(tokenizer) == INSIDE_UNFINISHED
    
    # Read through the string.
    pc = peekchar(tokenizer)
    while pc != '"' && pc != EOF_CHAR
        readchar(tokenizer)
        pc = peekchar(tokenizer)
    end

    # Check for unterminated string case.
    if pc == EOF_CHAR
        # Normally a closing quote terminates a string via a call to `tokenize_quote`, but if
        # there isn't a closing quote, we must terminate here in `tokenize_string` instead.
        tokenizer._string_state = OUTSIDE
        # TODO: Consider if emitting a token with start byte i and end byte i - 1 should be
        # avoided.
        return emit_error(tokenizer, K"ErrorUnterminatedString")
    else
        # Finish and emit.
        tokenizer._string_state = INSIDE_FINISHED
        return emit(tokenizer, K"String")
    end
end

# Lex whitespace, a whitespace char `c` has been consumed.
function tokenize_whitespace(tokenizer::Tokenizer, c::Char)
    hit_newline = c == '\n'
    pc = peekchar(tokenizer)
    # Stop on non whitespace or a second newline (we limit to one newline per token).
    while iswhitespace(pc) && !(hit_newline && pc == '\n')
        c = readchar(tokenizer)
        pc = peekchar(tokenizer)
        hit_newline = hit_newline || c == '\n'
    end
    return emit(tokenizer, hit_newline ? K"NewlineWs" : K"Whitespace")
end

# One-or-two character tokens.
tokenize_exclaim(tokenizer::Tokenizer) = accept(tokenizer, '=') ? emit(tokenizer, K"!=") : emit(tokenizer, K"!")
tokenize_equal(tokenizer::Tokenizer) = accept(tokenizer, '=') ? emit(tokenizer, K"==") : emit(tokenizer, K"=")
tokenize_less(tokenizer::Tokenizer) = accept(tokenizer, '=') ? emit(tokenizer, K"<=") : emit(tokenizer, K"<")
tokenize_greater(tokenizer::Tokenizer) = accept(tokenizer, '=') ? emit(tokenizer, K">=") : emit(tokenizer, K">")

# "//" comment or "/" forward slash.
function tokenize_forwardslash(tokenizer::Tokenizer)
    # Detect comment from two slashes.
    if accept(tokenizer, '/')
        # Read until end of line or file, then return a comment token.
        while true
            pc = peekchar(tokenizer)
            if pc == '\n' || pc == EOF_CHAR
                return emit(tokenizer, K"Comment")
            end
            readchar(tokenizer)
        end
    else
        return emit(tokenizer, K"/")
    end
end

function tokenize_quote(tokenizer::Tokenizer)
    if tokenizer._string_state == OUTSIDE
        tokenizer._string_state = INSIDE_UNFINISHED
    elseif tokenizer._string_state == INSIDE_FINISHED
        tokenizer._string_state = OUTSIDE
    else
        error("Should not `tokenize_quote` when string state is INSIDE_UNFINISHED")
    end 
    emit(tokenizer, K"\"")
end 

function tokenize_digit(tokenizer::Tokenizer)
    # Read all subsequent digits.
    while isdigit(peekchar(tokenizer))
        readchar(tokenizer)
    end

    # Handle the possibility of a fractional part.
    has_fraction = peekchar(tokenizer) == '.' && isdigit(peekchar2(tokenizer))
    if has_fraction
        readchar(tokenizer)
        while isdigit(peekchar(tokenizer))
            readchar(tokenizer)
        end
    end

    return emit(tokenizer, K"Number")
end


function tokenize_identifier(tokenizer::Tokenizer, c::Char)
    # Read and hash all alphanumeric characters. 
    n = 1
    hashed_identifier = simple_hash(c, UInt64(0))
    while isalphanumeric(peekchar(tokenizer))
        c = readchar(tokenizer)
        hashed_identifier = simple_hash(c, hashed_identifier)
        n += 1
    end

    # Use the hash to check if the identifier is a keyword.
    if n > MAX_KW_LENGTH
        k = K"Identifier"
    else
        k = get(kw_hash, hashed_identifier, K"Identifier")
    end
    
    return emit(tokenizer, k)
end

end  # end module
