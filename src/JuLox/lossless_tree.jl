# Tree construction from the list of text ranges held by ParseStream.
# Builds an untyped lossless source tree, aka "green tree" in Roslyn parlance.

module BuildLosslessTree
using Fractal.JuLox: JuLox, SyntaxKinds, Parse, Tokenize

LosslessLeafNode = Tokenize.Token  # alias

struct LosslessInnerNode
    _kind::SyntaxKinds.Kind
    _children::Vector{Union{LosslessInnerNode,LosslessLeafNode}}
    _startbyte::Integer
    _endbyte::Integer

    function LosslessInnerNode(kind::SyntaxKinds.Kind, children::Vector{Union{LosslessInnerNode,LosslessLeafNode}})
        startbyte = typemax(Int)
        endbyte = typemin(Int)
        for c in children
            # NOTE: We cannot define JuLox.startbyte(node::LosslessInnerNode) before here :(
            if c isa LosslessInnerNode
                child_start = c._startbyte
                child_end = c._endbyte
            else
                child_start = JuLox.startbyte(c)
                child_end = JuLox.endbyte(c)
            end
            startbyte = min(startbyte, child_start)
            endbyte = max(endbyte, child_end)
        end
        @assert startbyte < typemax(Int) && endbyte > typemin(Int)
        return new(kind, children, startbyte, endbyte)
    end
end

JuLox.startbyte(node::LosslessInnerNode) = node._startbyte
JuLox.endbyte(node::LosslessInnerNode) = node._endbyte

LosslessNode = Union{LosslessInnerNode,LosslessLeafNode}

SyntaxKinds.kind(node::LosslessInnerNode) = node._kind
children(node::LosslessInnerNode) = node._children
haschildren(node::LosslessInnerNode) = !(children(node) isa Tuple{})
haschildren(node::LosslessLeafNode) = false

"""
    build_tree(parse_result::Parse.ParseResult)

Construct a lossless source tree from a ParseResult using depth-first
traversal.

The tree here is constructed depth-first, but it would also be possible to use
a bottom-up tree builder interface similar to rust-analyzer. (In that case we'd
traverse the list of ranges backward rather than forward.)
"""
function build_tree(parse_result::Parse.ParseResult)
    stack = Vector{NamedTuple{(:first_token, :node),Tuple{Int,Union{LosslessInnerNode,LosslessLeafNode}}}}()
    tokens = parse_result.tokens
    events = parse_result.events
    event_idx = firstindex(events)
    while event_idx <= lastindex(events)
        # Add all tokens used by the next internal node to the stack.
        stacked_event = events[event_idx]
        for token_idx in stacked_event.first_token:stacked_event.last_token
            push!(stack, (first_token=token_idx, node=tokens[token_idx]))
        end
        # Process internal nodes which end at the current position.
        for event in events[event_idx:end]
            if event.last_token != stacked_event.last_token
                break
            end
            # Collect children from the stack for this internal node.
            k = length(stack) + 1
            while k > 1 && event.first_token <= stack[k-1].first_token
                k -= 1
            end
            children = LosslessNode[stack[n].node for n = k:length(stack)]
            node = LosslessInnerNode(SyntaxKinds.kind(event), children)
            resize!(stack, k - 1)
            push!(stack, (first_token=event.first_token, node=node))
            event_idx += 1
        end
    end
    if length(stack) == 1
        return only(stack).node
    else
        error("Found multiple nodes at top level")
    end
end

# Pretty printing
Base.summary(io::IO, node::LosslessNode) = show(io, SyntaxKinds.kind(node))
function _show_node(io, node, indent)
    posstr = "$(lpad(JuLox.startbyte(node), 6)):$(rpad(JuLox.endbyte(node), 6)) │"
    is_leaf = node isa LosslessLeafNode
    if is_leaf
        line = string(posstr, indent, summary(node))
        line = string(rpad(line, 43), ' ', repr(Tokenize.text(node)))
    else
        line = string(posstr, indent, '[', summary(node), ']')
    end
    line = line * "\n"
    print(io, line)
    if !is_leaf
        new_indent = indent * "  "
        for x in children(node)
            _show_node(io, x, new_indent)
        end
    end
end

function Base.show(io::IO, node::LosslessInnerNode)
    _show_node(io, node, "")
end

end  # end module BuildLosslessTree
