"""Parsing -- from Tokens to ParseStream to AST. Adapted from JuliaSyntax.jl on 2023-04-09."""

module Parse
using Fractal.JuLox.Tokenize: Tokenize, RawToken, kind, startbyte, endbyte
using Fractal.JuLox: Kind, @K_str, @KSet_str, is_whitespace, is_error
import Fractal.JuLox: kind, _token_error_descriptions

#-------------------------------------------------------------------------------
# The ParseStream struct and related structs.

struct Diagnostic
    first_byte::Int
    last_byte::Int
    message::String
end

first_byte(d::Diagnostic) = d.first_byte
last_byte(d::Diagnostic) = d.last_byte
message(d::Diagnostic) = d.message
Base.range(d::Diagnostic) = first_byte(d):last_byte(d)

function show_diagnostic(io::IO, diagnostic::Diagnostic, source::String)
    # Figure out the location of the issue.
    lines = split(source[1:first_byte(diagnostic)], '\n')
    linecol = "[line $(length(lines)), column $(length(lines[end]))]"

    # Print the error.
    println(io, "# Error @ $linecol - $(message(diagnostic))")
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, source::String)
    return show_diagnostic.(Ref(io), diagnostics, Ref(source))
end

function emit_diagnostic(diagnostics::AbstractVector{Diagnostic}, byterange::AbstractUnitRange, message)
    push!(diagnostics, Diagnostic(first(byterange), last(byterange), message))
end

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
    text_buf::Vector{UInt8}
    # GC root for the object which owns the memory in `textbuf`. `nothing` if
    # the `textbuf` owner was unknown (eg, ptr,length was passed)
    text_root::Any
    # Lexer, transforming the input bytes into a token stream
    lexer::Tokenize.Lexer{IOBuffer}
    # Track position in tokens
    finished_token_index::Int
    lookahead_index::Int
    # Buffer of tokens
    tokens::Vector{RawToken}
    # Parser output as an ordered sequence of ranges, parent nodes after children.
    ranges::Vector{TaggedRange}
    # Parsing diagnostics (errors/warnings etc)
    diagnostics::Vector{Diagnostic}
    # Counter for number of peek()s we've done without making progress via a bump()
    peek_count::Int

    function ParseStream(text_buf::Vector{UInt8}, text_root, next_byte::Integer)
        io = IOBuffer(text_buf)
        seek(io, next_byte - 1)
        lexer = Tokenize.Lexer(io)
        new(
            text_buf,
            text_root,
            lexer,
            0,
            1,
            Vector{RawToken}(),
            Vector{TaggedRange}(),
            Vector{Diagnostic}(),
            0,
        )
    end
end

function ParseStream(text::Vector{UInt8}, index::Integer=1)
    ParseStream(text, text, index)
end

# Buffers originating from strings
function ParseStream(text::String, index::Integer=1)
    ParseStream(unsafe_wrap(Vector{UInt8}, text), text, index)
end
function ParseStream(text::SubString, index::Integer=1)
    # See also IOBuffer(SubString("x"))
    ParseStream(unsafe_wrap(Vector{UInt8}, pointer(text), sizeof(text)), text, index)
end
function ParseStream(text::AbstractString, index::Integer=1)
    ParseStream(String(text), index)
end

function Base.show(io::IO, mime::MIME"text/plain", stream::ParseStream)
    println(io, "ParseStream at position $(_next_byte(stream))")
end

function show_diagnostics(io::IO, stream::ParseStream)
    show_diagnostics(io, stream.diagnostics, sourcetext(stream))
end


#-------------------------------------------------------------------------------
# Stream input interface - the peek_* family of functions.

# Buffer several tokens ahead
function _buffer_lookahead_tokens(lexer, tokens)
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
    endbyte(stream.tokens[stream.finished_token_index]) + 1
end

last_byte(stream::ParseStream) = _next_byte(stream) - 1

# Find the index of the next nontrivia token.
@inline function _lookahead_index(stream::ParseStream, n::Integer)
    # Much of the time we'll be peeking ahead a single token and have one or
    # zero whitespace tokens before the next token. The following code is an
    # unrolled optimized version for that fast path.
    i = stream.lookahead_index
    @inbounds if n == 1 && i + 2 <= length(stream.tokens)
        if !is_whitespace(stream.tokens[i])
            return i
        end
        i += 1
        if !is_whitespace(stream.tokens[i])
            return i
        end
    end
    # Fall through to the general case.
    return __lookahead_index(stream, n)
end

@noinline function __lookahead_index(stream, n)
    i = stream.lookahead_index
    while true
        if i > length(stream.tokens)
            _buffer_lookahead_tokens(stream.lexer, stream.tokens)
        end
        if !is_whitespace(stream.tokens[i])
            if n == 1
                return i
            end
            n -= 1
        end
        i += 1
    end
end

@noinline function _parser_stuck_error(stream)
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
    stream.peek_count += 1
    if stream.peek_count > 100_000
        _parser_stuck_error(stream)
    end
    i = _lookahead_index(stream, n)
    return @inbounds stream.tokens[i]
end


#-------------------------------------------------------------------------------
# Stream output interface - the `bump_*` and `emit_*` family of functions
#
# Though note bump() really does both input and output

# Bump up until the `n`th token
function _bump_until_n(stream::ParseStream, n::Integer)
    if n < stream.lookahead_index
        return
    end
    stream.finished_token_index = n
    stream.lookahead_index = n + 1
    # Defuse the time bomb
    stream.peek_count = 0
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

"""
Bump comments and whitespace tokens preceding the next nontrivia token.
"""
function bump_trivia(stream::ParseStream; error=nothing)
    emark = position(stream)
    _bump_until_n(stream, _lookahead_index(stream, 1) - 1)
    if !isnothing(error)
        emit(stream, emark, K"error", error=error)
    end
    return position(stream)
end

# Get position of last item emitted into the output stream
function Base.position(stream::ParseStream)
    ParseStreamPosition(stream.finished_token_index, lastindex(stream.ranges))
end

"""
    emit(stream, mark, kind; error=nothing)

Emit a new text span into the output which covers source bytes from `mark` to
the end of the most recent token which was `bump()`'ed. The starting `mark`
should be a previous return value of `position()`.
"""
function emit(stream::ParseStream, mark::ParseStreamPosition, kind::Kind; error=nothing)
    first_token = mark.token_index + 1
    range = TaggedRange(kind, first_token, stream.finished_token_index)
    if !isnothing(error)
        # The first child must be a leaf, otherwise ranges would be improperly
        # nested.
        fbyte = startbyte(stream.tokens[first_token])
        lbyte = endbyte(stream.tokens[stream.finished_token_index])
        emit_diagnostic(stream, fbyte:lbyte, error)
    end
    push!(stream.ranges, range)
    return position(stream)
end

function emit_diagnostic(stream::ParseStream, byterange::AbstractUnitRange, message)
    emit_diagnostic(stream.diagnostics, byterange, message)
    return nothing
end

"""
Emit a diagnostic at the position of the next token
"""
function emit_diagnostic(stream::ParseStream, message)
    i = _lookahead_index(stream, 1)
    t = stream.tokens[i]
    fbyte = startbyte(t)
    lbyte = endbyte(t)
    emit_diagnostic(stream, fbyte:lbyte, message)
    return nothing
end

function emit_diagnostic(stream::ParseStream, mark::ParseStreamPosition, message; trim_whitespace=true)
    i = mark.token_index
    j = stream.finished_token_index
    if trim_whitespace
        while i < j && is_whitespace(stream.tokens[j])
            j -= 1
        end
        while i + 1 < j && is_whitespace(stream.tokens[i+1])
            i += 1
        end
    end
    fbyte = endbyte(stream.tokens[i]) + 1
    lbyte = endbyte(stream.tokens[j])
    emit_diagnostic(stream, fbyte:lbyte, message)
end

function emit_diagnostic(stream::ParseStream, mark::ParseStreamPosition, end_mark::ParseStreamPosition, message)
    fbyte = endbyte(stream.tokens[mark.token_index]) + 1
    lbyte = endbyte(stream.tokens[end_mark.token_index])
    emit_diagnostic(stream, fbyte:lbyte, message)
end


#-------------------------------------------------------------------------------
# ParseStream Post-processing

# Tree construction from the list of text ranges held by ParseStream
# API for extracting results from ParseStream


function token_span(token::RawToken)
    return endbyte(token) - startbyte(token) + 1
end

"""
    build_tree(::Type{NodeType}, stream::ParseStream;
               wrap_toplevel_as_kind=nothing)

Construct a tree with `NodeType` nodes from a ParseStream using depth-first
traversal. `NodeType` must have the constructors

    NodeType(k::Kind, span::Integer)
    NodeType(k::Kind, children::NodeType...)

A single node which covers the input is expected, but if the ParseStream has
multiple nodes at the top level, `wrap_toplevel_as_kind` may be used to wrap
them in a single node.

The tree here is constructed depth-first, but it would also be possible to use
a bottom-up tree builder interface similar to rust-analyzer. (In that case we'd
traverse the list of ranges backward rather than forward.)
"""
function build_tree(
    ::Type{NodeType}, stream::ParseStream;
    wrap_toplevel_as_kind::Union{Nothing,Kind}=nothing
) where {NodeType}
    stack = Vector{NamedTuple{(:first_token, :node),Tuple{Int,NodeType}}}()
    tokens = stream.tokens
    ranges = stream.ranges
    i = firstindex(tokens)
    j = firstindex(ranges)
    while true
        last_token = j <= lastindex(ranges) ? ranges[j].last_token : stream.finished_token_index
        # Process tokens to nodes for all tokens used by the next internal node.
        while i <= last_token
            t = tokens[i]
            node = NodeType(kind(t), token_span(t))
            push!(stack, (first_token=i, node=node))
            i += 1
        end
        if j > lastindex(ranges)
            break
        end
        # Process internal nodes which end at the current position.
        while j <= lastindex(ranges)
            r = ranges[j]
            if r.last_token != last_token
                break
            end
            # Collect children from the stack for this internal node.
            k = length(stack) + 1
            while k > 1 && r.first_token <= stack[k-1].first_token
                k -= 1
            end
            children = (stack[n].node for n = k:length(stack))
            node = NodeType(kind(r), children...)
            resize!(stack, k - 1)
            push!(stack, (first_token=r.first_token, node=node))
            j += 1
        end
    end
    if length(stack) == 1
        return only(stack).node
    elseif !isnothing(wrap_toplevel_as_kind)
        # Mostly for debugging
        children = (x.node for x in stack)
        return NodeType(wrap_toplevel_as_kind, children...)
    else
        error("Found multiple nodes at top level")
    end
end

"""
    sourcetext(stream::ParseStream; steal_textbuf=true)

Return the source text being parsed by this `ParseStream` as a UTF-8 encoded
string.
"""
function sourcetext(stream::ParseStream)
    root = stream.text_root
    if root isa String
        return root
    else
        return String(copy(stream.textbuf))
    end
end


function validate_tokens(stream::ParseStream)
    for t in stream.tokens
        k = kind(t)
        if is_error(k) && k != K"error"
            # Emit messages for non-generic token errors
            # TODO: Are these only tokenization errors? Should we handle this elsewhere?
            diagnostic_range = startbyte(t):endbyte(t)
            emit_diagnostic(stream, diagnostic_range, _token_error_descriptions[k])
        end
    end
    sort!(stream.diagnostics, by=first_byte)
    return nothing
end

struct GreenNode
    kind::Kind
    span::Int
    args::Union{Tuple{},Vector{GreenNode}}
end

function GreenNode(k::Kind, span::Integer)
    return GreenNode(k, span, ())
end

function GreenNode(k::Kind, args::GreenNode...)
    children = collect(GreenNode, args)
    span = isempty(children) ? 0 : sum(x.span for x in children)
    return GreenNode(k, span, children)
end

kind(node::GreenNode) = node.kind
span(node::GreenNode) = node.span
children(node::GreenNode) = node.args
haschildren(node::GreenNode) = !(node.args isa Tuple{})
function is_trivia(node::GreenNode)
    return is_whitespace(kind(node)) || kind(node) ∈ KSet"None EndMarker ( ) { }"
end

# Pretty printing
Base.summary(io::IO, node::GreenNode) = show(io, kind(node))
function _show_node(io, node, indent, pos, str)
    posstr = "$(lpad(pos, 6)):$(rpad(pos+span(node)-1, 6)) │"
    is_leaf = !haschildren(node)
    if is_leaf
        line = string(posstr, indent, summary(node))
    else
        line = string(posstr, indent, '[', summary(node), ']')
    end
    if !is_trivia(node) && is_leaf
        line = rpad(line, 40) * "✔"
    end
    if is_error(node)
        line = rpad(line, 41) * "✘"
    end
    if is_leaf && !isnothing(str)
        line = string(rpad(line, 43), ' ', repr(str[pos:prevind(str, pos + span(node))]))
    end
    line = line * "\n"
    if is_error(node)
        printstyled(io, line, color=:light_red)
    else
        print(io, line)
    end
    if !is_leaf
        new_indent = indent * "  "
        p = pos
        for x in children(node)
            _show_node(io, x, new_indent, p, str)
            p += x.span
        end
    end
end

function Base.show(io::IO, node::GreenNode)
    _show_node(io, node, "", 1, nothing)
end

function Base.show(io::IO, node::GreenNode, str::AbstractString)
    _show_node(io, node, "", 1, str)
end

struct SyntaxNode
    green_node::GreenNode
    args::Union{Tuple{},Vector{SyntaxNode}}
    value::Any
end

function SyntaxNode(source::String, raw::GreenNode, position::Int)
    if !haschildren(raw)
        # Leaf node.
        k = kind(raw)
        val_range = position:position + span(raw) - 1
        val_str = view(source, val_range)
        val = if k == K"Number"
            parse(Float64, val_str)
        elseif k == K"String"
            val_str
        elseif k == K"Identifier"
            Symbol(val_str)
        else
            # Operations, true/false/nil, etc. which function just on type.
            nothing
        end
        return SyntaxNode(raw, (), val)
    else
        # Inner node.
        cs = SyntaxNode[]
        for rawchild in children(raw)
            if !is_trivia(rawchild)
                # Recurse.
                push!(cs, SyntaxNode(source, rawchild, position))
            end
        end
        node = SyntaxNode(raw, cs, position)
        return node
    end
end

children(node::SyntaxNode) = node.args
haschildren(node::SyntaxNode) = !(children(node) isa Tuple{})
kind(node::SyntaxNode) = kind(node.green_node)

function build_tree(::Type{SyntaxNode}, stream::ParseStream;
    wrap_toplevel_as_kind::Union{Nothing,Kind}=nothing
)
    green_tree = build_tree(GreenNode, stream; wrap_toplevel_as_kind=wrap_toplevel_as_kind)
    # TODO: Consider cleaning this up to avoid a copy!
    source = String(stream.text_buf)
    start_pos = startbyte(first(stream.tokens))
    return SyntaxNode(source, green_tree, start_pos)
end

function _show_syntax_node(io, node::SyntaxNode, indent, pos)
    green_node = node.green_node
    val = node.value
    posstr = "$(lpad(pos, 6)):$(rpad(pos+span(green_node)-1, 6)) │"
    nodestr = haschildren(node) ? "[$(kind(node))]" :
        isnothing(val) ? kind(node) : repr(val)
    treestr = string(indent, nodestr)
    println(io, posstr, treestr)
    if haschildren(node)
        new_indent = indent*"  "
        p = pos
        for n in children(node)
            _show_syntax_node(io, n, new_indent, p)
            p += n.green_node.span
        end
    end
end


Base.show(io::IO, node::SyntaxNode) = _show_syntax_node(io, node, "", 1)


#-------------------------------------------------------------------------------
# Parser.

function parse_expression(ps::ParseStream)
    parse_equality(ps)
    return nothing
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
        parse_primary(ps)
        emit(ps, mark, K"unary")
    else
        parse_primary(ps)
    end
end

function parse_primary(ps::ParseStream)
    is_error(peek(ps)) && bump(ps)  # Pass through errors.
    mark = position(ps)
    k = peek(ps)
    if k ∈ KSet"false true nil Number String"
        bump(ps)
        # We don't actually have to emit a range for a single token item.
    elseif k == K"("
        # Consume the left parenthesis.
        bump(ps)

        # Handle parenthesized expression.
        parse_expression(ps)

        # Parse right parenthesis.
        # TODO: Error handling.
        mark = position(ps)
        if peek(ps) == K")"
            bump(ps)
        else
            b = last_byte(ps)
            emit_diagnostic(ps.diagnostics, b:b-1, "Expect ')' after expression.")
        end
    end
end

#-------------------------------------------------------------------------------
# Main (public) API.

struct ParseError <: Exception
    source::String
    diagnostics::Vector{Diagnostic}
end

function ParseError(stream::ParseStream)
    ParseError(sourcetext(stream), stream.diagnostics)
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
    parse_expression(stream)
    validate_tokens(stream)
    if peek(stream) != K"EndMarker"
        emit_diagnostic(stream, "Unexpected text after parsing input")
    end
    tree = build_tree(T, stream; wrap_toplevel_as_kind=K"toplevel")
    if !isempty(stream.diagnostics)
        throw(ParseError(stream))
    end
    return tree, last_byte(stream) + 1
end

end  # end module Parse
