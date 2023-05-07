module Resolver

using Fractal.JuLox: JuLox, LossyTrees, SyntaxValidation

#-------------------------------------------------------------------------------
# Building blocks.

# Type alias.
Scope = Dict{Symbol,Bool}

struct ResolverState
    scopes::Vector{Scope}
    locals::Dict{LossyTrees.LossyNode,Int}
    diagnostics::Vector{SyntaxValidation.Diagnostic}

    function ResolverState()
        return new(Scope[], Dict{LossyTrees.LossyNode,Int}(), SyntaxValidation.Diagnostic[])
    end
end

function begin_scope!(state::ResolverState)
    push!(state.scopes, Scope())
    return nothing
end

function end_scope!(state::ResolverState)
    pop!(state.scopes)
    return nothing
end

function declare!(state::ResolverState, symbol::Symbol)
    if !isempty(state.scopes)
        last(state.scopes)[symbol] = false
    end
    return nothing
end

function define!(state::ResolverState, symbol::Symbol)
    if !isempty(state.scopes)
        last(state.scopes)[symbol] = true
    end
    return nothing
end

#-------------------------------------------------------------------------------
# Resolving reading from and writing to variables.

function resolve(
    state::ResolverState,
    node::Union{LossyTrees.Variable,LossyTrees.Assign},
    depth::Int
)
    state.locals[node] = depth
end

function resolve_local(
    state::ResolverState,
    node::Union{LossyTrees.Variable,LossyTrees.Assign},
    symbol::Symbol
)
    for (i, scope) in enumerate(state.scopes[end:-1:1])
        if haskey(scope, symbol)
            depth = i - 1
            resolve(state, node, depth)
            return nothing
        end
    end
    return nothing
end

#-------------------------------------------------------------------------------
# Analysis triggering resolution.

function analyze(state::ResolverState, node::LossyTrees.Block)
    begin_scope!(state)
    analyze.(Ref(state), node.statements)
    end_scope!(state)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.VariableDeclaration)
    declare!(state, node.name.symbol)
    analyze(state, node.initializer)
    define!(state, node.name.symbol)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Variable)
    symbol = node.name.symbol
    # If the variable has been declared but not defined, we had better not be trying to
    # resolve it already!
    if (
        !isempty(state.scopes)
        && haskey(last(state.scopes), symbol)
        && !last(state.scopes)[symbol]
    )
        diagnostic = SyntaxValidation.Diagnostic(
            node.lossless_node,  "Can't read local variable in its own initializer"
        )
        push!(state.diagnostics, diagnostic)
    end

    # Resolve the local variable name.
    resolve_local(state, node, symbol)

    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Assign)
    # Analyze the expression assigned to the variable.
    analyze(state, node.value)

    # Resolve the local variable name.
    resolve_local(state, node, node.name.symbol)
end

function analyze(state::ResolverState, node::LossyTrees.FunctionDeclaration)
    # Function names are bound to the surrounding scope.
    declare!(state, node.name.symbol)
    define!(state, node.name.symbol)

    # Handle the function itself.
    analyze_function(state, node)

    return nothing
end

function analyze_function(state::ResolverState, node::LossyTrees.FunctionDeclaration)
    # Function declarations introduce a new scope.
    begin_scope!(state)

    # Deal with the parameters.
    for param in node.parameters
        declare!(state, param.symbol)
        define!(state, param.symbol)
    end

    # Deal with the body.
    analyze(state, node.body)

    # Pop scope.
    end_scope!(state)
end

#-------------------------------------------------------------------------------
# Boring analysis used just to recurse into subtrees.

function analyze(state::ResolverState, node::LossyTrees.Toplevel)
    # NOTE: We do not begin/end scope at the toplevel, because global scope is
    # special-cased as separate from local scope.
    analyze.(Ref(state), node.statements)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.ExpressionStatement)
    analyze(state, node.expression)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.ReturnStatement)
    analyze(state, node.return_value)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Print)
    analyze(state, node.expression)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.If)
    analyze(state, node.condition)
    analyze(state, node.then_statement)
    !isnothing(node.else_statement) && analyze(state, node.else_statement)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.While)
    analyze(state, node.condition)
    analyze(state, node.statement)
    return nothing
end

function analyze(state::ResolverState, node::Union{LossyTrees.Infix,LossyTrees.Logical})
    analyze(state, node.left)
    analyze(state, node.right)
    # NOTE: We don't have to analyze operators.
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Unary)
    analyze(state, node.right)
    # NOTE: We don't have to analyze operators.
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Call)
    analyze(state, node.callee)
    analyze.(Ref(state), node.arguments)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Grouping)
    analyze(state, node.expression)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Literal)
    return nothing
end

#-------------------------------------------------------------------------------
# Public API.

function resolve_scopes(node::LossyTrees.Toplevel)
    state = ResolverState()
    analyze(state, node)
    return state.locals, state.diagnostics
end

end  # module Resolver