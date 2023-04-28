# Tree construction from the list of text ranges held by ParseStream.
# Builds an untyped lossless source tree, aka "green tree" in Roslyn parlance.

module LosslessTrees
using Fractal.JuLox: JuLox, SyntaxKinds, Parse, Tokenize

LosslessLeafNode = Tokenize.Token  # alias

struct LosslessInnerNode
    _kind::SyntaxKinds.Kind
    _children::Vector{Union{LosslessInnerNode,LosslessLeafNode}}
    _startbyte::Integer
    _endbyte::Integer
end

JuLox.startbyte(node::LosslessInnerNode) = node._startbyte
JuLox.endbyte(node::LosslessInnerNode) = node._endbyte

LosslessNode = Union{LosslessInnerNode,LosslessLeafNode}

SyntaxKinds.kind(node::LosslessInnerNode) = node._kind
children(node::LosslessInnerNode) = node._children
haschildren(node::LosslessInnerNode) = !(children(node) isa Tuple{})
haschildren(node::LosslessLeafNode) = false

function event_start_end(event::Parse.Event, tokens::Vector{Tokenize.Token})
    start_token = event.first_token <= length(tokens) ? tokens[event.first_token] : last(tokens)
    end_token = event.last_token <= length(tokens) ? tokens[event.last_token] : last(tokens)
    return JuLox.startbyte(start_token), JuLox.endbyte(end_token)
end

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
    token_idx = firstindex(tokens)
    event_idx = firstindex(events)
    while event_idx <= lastindex(events)
        # Add all tokens used by the next internal node to the stack.
        last_stacked_token = events[event_idx].last_token
        for token_idx in token_idx:last_stacked_token
            push!(stack, (first_token=token_idx, node=tokens[token_idx]))
        end
        token_idx = last_stacked_token + 1
        # Process internal nodes which end at the current position.
        for event in events[event_idx:end]
            if event.last_token != last_stacked_token
                break
            end
            # Collect children from the stack for this internal node.
            k = length(stack) + 1
            while k > 1 && event.first_token <= stack[k-1].first_token
                k -= 1
            end
            children = LosslessNode[stack[n].node for n = k:length(stack)]
            startbyte, endbyte = event_start_end(event, tokens)
            node = LosslessInnerNode(SyntaxKinds.kind(event), children, startbyte, endbyte)
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
function Base.summary(io::IO, node::LosslessNode)
    k = SyntaxKinds.kind(node)
    res = convert(String, k)
    if SyntaxKinds.is_error(k)
        res = "$(res) - $(SyntaxKinds.error_description(k))"
    end
    print(io, res)
end
function _show_node(io, node, indent)
    posstr = "$(lpad(JuLox.startbyte(node), 6)):$(rpad(JuLox.endbyte(node), 6)) │"
    is_leaf = node isa LosslessLeafNode
    if is_leaf
        line = string(posstr, indent, summary(node))
        if JuLox.span(node) > 0
            line = string(rpad(line, 43), ' ', repr(Tokenize.text(node)))
        end
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
