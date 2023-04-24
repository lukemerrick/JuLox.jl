# Tree construction from the list of text ranges held by ParseStream.

#-------------------------------------------------------------------------------
# Lossless source tree, aka "green tree" in Roslyn parlance.

struct LosslessSourceNode
    kind::Kind
    span::Int
    args::Union{Tuple{},Vector{LosslessSourceNode}}
end

function LosslessSourceNode(k::Kind, span::Integer)
    return LosslessSourceNode(k, span, ())
end

function LosslessSourceNode(k::Kind, args::LosslessSourceNode...)
    children = collect(LosslessSourceNode, args)
    span = isempty(children) ? 0 : sum(x.span for x in children)
    return LosslessSourceNode(k, span, children)
end

JuLox.kind(node::LosslessSourceNode) = node.kind
JuLox.span(node::LosslessSourceNode) = node.span
children(node::LosslessSourceNode) = node.args
haschildren(node::LosslessSourceNode) = !(node.args isa Tuple{})


"""
    build_tree(::Type{LosslessSourceNode}, stream::ParseStream)

Construct a lossless source tree from a ParseStream using depth-first
traversal.

The tree here is constructed depth-first, but it would also be possible to use
a bottom-up tree builder interface similar to rust-analyzer. (In that case we'd
traverse the list of ranges backward rather than forward.)
"""
function build_tree(::Type{LosslessSourceNode}, stream::ParseStream)
    stack = Vector{NamedTuple{(:first_token, :node),Tuple{Int,LosslessSourceNode}}}()
    tokens = stream._tokens
    ranges = stream._ranges
    i = firstindex(tokens)
    j = firstindex(ranges)
    while true
        last_token = j <= lastindex(ranges) ? ranges[j].last_token : stream._finished_token_index
        # Process tokens to nodes for all tokens used by the next internal node.
        while i <= last_token
            t = tokens[i]
            node = LosslessSourceNode(kind(t), span(t))
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
            node = LosslessSourceNode(kind(r), children...)
            resize!(stack, k - 1)
            push!(stack, (first_token=r.first_token, node=node))
            j += 1
        end
    end
    if length(stack) == 1
        return only(stack).node
    else
        error("Found multiple nodes at top level")
    end
end

# Pretty printing
Base.summary(io::IO, node::LosslessSourceNode) = show(io, kind(node))
function _show_node(io, node, indent, pos, str)
    posstr = "$(lpad(pos, 6)):$(rpad(pos+span(node)-1, 6)) │"
    is_leaf = !haschildren(node)
    if is_leaf
        line = string(posstr, indent, summary(node))
    else
        line = string(posstr, indent, '[', summary(node), ']')
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

function Base.show(io::IO, node::LosslessSourceNode)
    _show_node(io, node, "", 1, nothing)
end

function Base.show(io::IO, node::LosslessSourceNode, str::AbstractString)
    _show_node(io, node, "", 1, str)
end

#-------------------------------------------------------------------------------
# Lossy abstract syntax tree (AST) layered on top of the lossless source tree.

struct SyntaxNode
    green_node::LosslessSourceNode
    position::Int
    args::Union{Tuple{},Vector{SyntaxNode}}
    value::Any
end

"""Specify the kinds of nodes to drop from the lossless source tree to create the AST."""
function is_ast_dropped_kind(node::LosslessSourceNode)
    return is_whitespace(kind(node)) || kind(node) ∈ KSet"None EndMarker ( ) { } print ; = var"
end

function SyntaxNode(source::String, raw::LosslessSourceNode, position::Int)
    if !haschildren(raw)
        # Leaf node.
        k = kind(raw)
        val_range = position:position+span(raw)-1
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
        return SyntaxNode(raw, position, (), val)
    else
        # Inner node.
        cs = SyntaxNode[]
        pos = position
        for rawchild in children(raw)
            if !is_ast_dropped_kind(rawchild)
                # Recurse.
                push!(cs, SyntaxNode(source, rawchild, pos))
            end
            pos += span(rawchild)
        end
        node = SyntaxNode(raw, position, cs, position)
        return node
    end
end

children(node::SyntaxNode) = node.args
haschildren(node::SyntaxNode) = !(children(node) isa Tuple{})
kind(node::SyntaxNode) = kind(node.green_node)

function build_tree(::Type{SyntaxNode}, stream::ParseStream)
    green_tree = build_tree(LosslessSourceNode, stream)
    # TODO: Consider cleaning this up to avoid a copy!
    source = String(stream._text_buf)
    start_pos = startbyte(first(stream._tokens))
    return SyntaxNode(source, green_tree, start_pos)
end

function _show_syntax_node(io, node::SyntaxNode, indent)
    green_node = node.green_node
    val = node.value
    pos = node.position
    posstr = "$(lpad(pos, 6)):$(rpad(pos+span(green_node)-1, 6)) │"
    nodestr = haschildren(node) ? "[$(kind(node))]" :
              isnothing(val) ? kind(node) : repr(val)
    treestr = string(indent, nodestr)
    println(io, posstr, treestr)
    if haschildren(node)
        new_indent = indent * "  "
        for n in children(node)
            _show_syntax_node(io, n, new_indent)
        end
    end
end


Base.show(io::IO, node::SyntaxNode) = _show_syntax_node(io, node, "")

#-------------------------------------------------------------------------------
# ParseStream Post-processing


"""
    sourcetext(stream::ParseStream; steal_textbuf=true)

Return the source text being parsed by this `ParseStream` as a UTF-8 encoded
string.
"""
function sourcetext(stream::ParseStream)
    root = stream._text_root
    if root isa String
        return root
    else
        return String(copy(stream.textbuf))
    end
end


function validate_tokens(stream::ParseStream)
    for t in stream._tokens
        k = kind(t)
        if is_error(k) && k != K"error"
            # Emit messages for non-generic token errors
            # TODO: Are these only tokenization errors? Should we handle this elsewhere?
            diagnostic_range = startbyte(t):endbyte(t)
            emit_diagnostic(stream, diagnostic_range, _token_error_descriptions[k])
        end
    end
    sort!(stream._diagnostics, by=first_byte)
    return nothing
end
