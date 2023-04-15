"""Heavily adapted from JuliaSyntax.jl on 2023.04.05"""

module Tokenize
using Fractal.JuLox: Kind, @K_str
import Fractal.JuLox: kind, is_literal, is_error

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
# Tokens.

struct RawToken
    _kind::Kind
    # Offsets into a string or buffer
    _startbyte::Int # The byte where the token start in the buffer
    _endbyte::Int # The byte where the token ended in the buffer
end
RawToken() = RawToken(K"error", 0, 0)

kind(t::RawToken) = t._kind
startbyte(t::RawToken) = t._startbyte
endbyte(t::RawToken) = t._endbyte

function untokenize(t::RawToken, str::String)
    String(codeunits(str)[1 .+ (t.startbyte:t.endbyte)])
end

function Base.show(io::IO, t::RawToken)
    print(io, rpad(string(startbyte(t), "-", endbyte(t)), 11, " "))
    print(io, rpad(kind(t), 15, " "))
end

const EMPTY_TOKEN = RawToken()
Base.__throw_invalid_ascii

#-------------------------------------------------------------------------------
# Lexer struct.

"""
`Lexer` reads from an input stream and emits a single token each time
`next_token` is called.
"""
mutable struct Lexer{IO_t<:IO}
    _io::IO_t
    _token_startpos::Int
    _position::Int
    _chars::Tuple{Char,Char,Char}
    _in_string::Bool
end

# Initialize Lexer from just IO by reading up to the first three characters.
function Lexer(io::IO)
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
    return Lexer(io, startpos, currentpos, (c1, c2, c3), false)
end
Lexer(str::AbstractString) = Lexer(IOBuffer(str))

# Pretty printing.
function Base.show(io::IO, l::Lexer)
    print(io, typeof(l), " at position: ", position(l))
end

# Implement methods for some other IO/stream functions.
"""
    position(l::Lexer)

Returns the current position.
"""
Base.position(l::Lexer) = l._position

"""
    eof(l::Lexer)

Determine whether the end of the lexer's underlying buffer has been reached.
"""
Base.eof(l::Lexer) = eof(l._io)

Base.seek(l::Lexer, pos) = seek(l._io, pos)

#-------------------------------------------------------------------------------
# Lexer iterator interface.

Base.IteratorSize(::Type{<:Lexer}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:Lexer}) = Base.HasEltype()
Base.eltype(::Type{<:Lexer}) = RawToken

function Base.iterate(l::Lexer, isdone::Any)
    isdone && return nothing
    t = next_token(l)
    isdone = kind(t) == K"EndMarker"
    return t, isdone
end
Base.iterate(l::Lexer) = iterate(l, false)

#-------------------------------------------------------------------------------
# Lexing implementation.

@inline isalpha(c::Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
@inline isalphanumeric(c::Char) = isalpha(c) || isdigit(c)
@inline iswhitespace(c::Char) = Base.isvalid(c) && Base.isspace(c)

"""
    tokenize(x)

Returns an `Iterable` containing the tokenized input. Can be reverted by e.g.
`join(untokenize.(tokenize(x)))`.
"""
tokenize(x) = Lexer(x)

startpos(l::Lexer) = l._token_startpos
peekchar(l::Lexer) = l._chars[2]
peekchar2(l::Lexer) = l._chars[3]
instring(l::Lexer) = l._in_string

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)
function readchar(l::Lexer)
    c = readchar(l._io)
    l._chars = (l._chars[2], l._chars[3], c)
    l._position += 1
    return l._chars[1]
end

"""
    accept(l::Lexer, f::Union{Function, Char, Vector{Char}, String})

Consumes the next character `c` if either `f::Function(c)` returns true, `c == f`
for `c::Char` or `c in f` otherwise. Returns `true` if a character has been
consumed and `false` otherwise.
"""
@inline function accept(l::Lexer, f::Union{Function,Char,Vector{Char},String})
    c = peekchar(l)
    if isa(f, Function)
        ok = f(c)
    elseif isa(f, Char)
        ok = c == f
    else
        ok = c in f
    end
    ok && readchar(l)
    return ok
end

"""
    start_token!(l::Lexer)

Updates the lexer's state such that the next  `RawToken` will start at the current
position.
"""
function start_token!(l::Lexer)
    l._token_startpos = position(l)
end

emit(l::Lexer, kind::Kind) = RawToken(kind, startpos(l), position(l) - 1)

function emit_error(l::Lexer, err::Kind)
    @assert is_error(err)
    return emit(l, err)
end

"""
    next_token(l::Lexer)

Returns the next `RawToken`.
"""
function next_token(l::Lexer)
    # Advance starting position to the next token.
    start_token!(l)

    # If we are in a string, tokenize until the closing quote mark.
    instring(l) && peekchar(l) != '"' && return lex_string(l)

    # If we are *not* in a string, tokenize normally.
    c = readchar(l)

    if c == EOF_CHAR
        return emit(l, K"EndMarker")
    elseif iswhitespace(c)
        return lex_whitespace(l, c)

    elseif c == '('
        return emit(l, K"(")
    elseif c == ')'
        return emit(l, K")")
    elseif c == '{'
        return emit(l, K"{")
    elseif c == '}'
        return emit(l, K"}")
    elseif c == ','
        return emit(l, K",")
    elseif c == '.'
        return emit(l, K".")
    elseif c == '-'
        return emit(l, K"-")
    elseif c == '+'
        return emit(l, K"+")
    elseif c == ';'
        return emit(l, K";")
    elseif c == '*'
        return emit(l, K"*")

    elseif c == '!'
        return lex_exclaim(l)
    elseif c == '='
        return lex_equal(l)
    elseif c == '<'
        return lex_less(l)
    elseif c == '>'
        return lex_greater(l)

    elseif c == '/'
        return lex_forwardslash(l)
    elseif c == '"'
        return lex_quote(l)
    elseif isdigit(c)
        return lex_digit(l)
    elseif isalpha(c)
        return lex_identifier(l, c)

    else
        emit_error(l, K"ErrorUnknownCharacter")
    end
end

# Lex string, a quote `"` has been tokenized already.
function lex_string(l::Lexer)
    @assert instring(l)
    
    # Read through the string.
    pc = peekchar(l)
    while pc != '"' && pc != EOF_CHAR
        readchar(l)
        pc = peekchar(l)
    end

    # Check for unterminated string case.
    if pc == EOF_CHAR
        # Normally a closing quote terminates a string via a call to `lex_quote`, but if
        # there isn't a closing quote, we must terminate here in `lex_string` instead.
        l._in_string = false
        # TODO: Consider if emitting a token with start byte i and end byte i - 1 should be
        # avoided.
        return emit_error(l, K"ErrorUnterminatedString")
    else
        return emit(l, K"String")
    end
end

# Lex whitespace, a whitespace char `c` has been consumed.
function lex_whitespace(l::Lexer, c::Char)
    hit_newline = c == '\n'
    pc = peekchar(l)
    # Stop on non whitespace or a second newline (we limit to one newline per token).
    while iswhitespace(pc) && !(hit_newline && pc == '\n')
        c = readchar(l)
        pc = peekchar(l)
        hit_newline = hit_newline || c == '\n'
    end
    return emit(l, hit_newline ? K"NewlineWs" : K"Whitespace")
end

# One-or-two character tokens.
lex_exclaim(l::Lexer) = accept(l, '=') ? emit(l, K"!=") : emit(l, K"!")
lex_equal(l::Lexer) = accept(l, '=') ? emit(l, K"==") : emit(l, K"=")
lex_less(l::Lexer) = accept(l, '=') ? emit(l, K"<=") : emit(l, K"=")
lex_greater(l::Lexer) = accept(l, '=') ? emit(l, K">=") : emit(l, K"=")

# "//" comment or "/" forward slash.
function lex_forwardslash(l::Lexer)
    # Detect comment from two slashes.
    if accept(l, '/')
        # Read until end of line or file, then return a comment token.
        while true
            pc = peekchar(l)
            if pc == '\n' || pc == EOF_CHAR
                return emit(l, K"Comment")
            end
            readchar(l)
        end
    else
        return emit(l, K"/")
    end
end

function lex_quote(l::Lexer)
    l._in_string = !l._in_string
    emit(l, K"\"")
end 

function lex_digit(l::Lexer)
    # Read all subsequent digits.
    while isdigit(peekchar(l))
        readchar(l)
    end

    # Handle the possibility of a fractional part.
    has_fraction = peekchar(l) == '.' && isdigit(peekchar2(l))
    if has_fraction
        while isdigit(peekchar(l))
            readchar(l)
        end
    end

    return emit(l, K"Number")
end


function lex_identifier(l::Lexer, c::Char)
    # Read and hash all alphanumeric characters. 
    n = 1
    hashed_identifier = simple_hash(c, UInt64(0))
    while isalphanumeric(peekchar(l))
        c = readchar(l)
        hashed_identifier = simple_hash(c, hashed_identifier)
        n += 1
    end

    # Use the hash to check if the identifier is a keyword.
    if n > MAX_KW_LENGTH
        k = K"Identifier"
    else
        k = get(kw_hash, hashed_identifier, K"Identifier")
    end
    
    return emit(l, k)
end

end  # end module