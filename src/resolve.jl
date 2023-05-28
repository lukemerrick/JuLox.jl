module Resolver

# TODO: (Challenge item from the book): Extend the validation to report an error if
# a local variable is defined but not used.

using JuLox: JuLox, LossyTrees, SyntaxValidation

#-------------------------------------------------------------------------------
# Building blocks.

# Type alias.
Scope = Dict{Symbol,Bool}

_assert_valid_function_scope(current_function::Symbol) = @assert current_function ∈ (:none, :function, :method, :initializer)
_assert_valid_class_type(current_class_type::Symbol) = @assert current_class_type ∈ (:none, :class, :subclass)

struct ResolverState
    scopes::Vector{Scope}
    locals::Dict{LossyTrees.Expression,Int}
    diagnostics::Vector{SyntaxValidation.Diagnostic}
    current_function::Ref{Symbol}
    current_class::Ref{Symbol}

    function ResolverState()
        return new(Scope[], Dict{LossyTrees.Expression,Int}(), SyntaxValidation.Diagnostic[], Ref(:none), Ref(:none))
    end
end

current_function(state::ResolverState) = state.current_function[]
current_class(state::ResolverState) = state.current_class[]

function enter_scope(f::Function, state::ResolverState; only_if::Bool=true)
    only_if && push!(state.scopes, Scope())
    result = f(state)
    only_if && pop!(state.scopes)
    return result
end


function declare!(state::ResolverState, name::LossyTrees.Identifier)
    # Do nothing in the global scope.
    isempty(state.scopes) && return nothing

    current_scope = last(state.scopes)
    symbol = name.symbol

    # Add an error message if we're repeatedly declaring a variable in a local scope.
    if haskey(current_scope, symbol)
        diagnostic = SyntaxValidation.Diagnostic(
            name.lossless_node, "already a variable with this name in this scope"
        )
        push!(state.diagnostics, diagnostic)
    end

    # Track that this variable has been declared (but not defined yet) in the current scope.
    current_scope[symbol] = false

    return nothing
end

function define!(state::ResolverState, name::LossyTrees.Identifier)
    # Do nothing in the global scope.
    isempty(state.scopes) && return nothing

    # Track that definition has completed.
    current_scope = last(state.scopes)
    symbol = name.symbol
    @assert haskey(current_scope, symbol)
    current_scope[symbol] = true
    return nothing
end

function enter_function_state(f::Function, state::ResolverState, current_function::Symbol)
    _assert_valid_function_scope(current_function)
    original_function = state.current_function[]
    state.current_function[] = current_function
    result = f(state)
    state.current_function[] = original_function
    return result
end


function enter_class_state(f::Function, state::ResolverState, current_class::Symbol)
    _assert_valid_class_type(current_class)
    original_class = state.current_class[]
    state.current_class[] = current_class
    result = f(state)
    state.current_class[] = original_class
    return result
end


#-------------------------------------------------------------------------------
# Resolving reading from and writing to variables.

function resolve(
    state::ResolverState,
    node::Union{LossyTrees.Variable,LossyTrees.Assign,LossyTrees.This,LossyTrees.Super},
    depth::Int
)
    state.locals[node] = depth
end

function resolve_local(
    state::ResolverState,
    node::Union{LossyTrees.Variable,LossyTrees.Assign,LossyTrees.This,LossyTrees.Super},
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
    enter_scope(state) do state
        analyze.(Ref(state), node.statements)
    end
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.VariableDeclaration)
    declare!(state, node.name)
    analyze(state, node.initializer)
    define!(state, node.name)
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
            node.lossless_node, "can't read local variable in its own initializer"
        )
        push!(state.diagnostics, diagnostic)
    end

    # Resolve the local variable name.
    resolve_local(state, node, symbol)

    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.This)
    if current_class(state) == :none
        diagnostic = SyntaxValidation.Diagnostic(
            node.lossless_node, "can't use 'this' outside a class"
        )
        push!(state.diagnostics, diagnostic)
    end
    resolve_local(state, node, :this)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Super)
    if current_class(state) != :subclass
        if current_class(state) == :none
            message = "can't use 'super' outside a class"
        else
            @assert current_class(state) == :class
            message = "can't use 'super' in a class with no superclass"
        end
        diagnostic = SyntaxValidation.Diagnostic(node.lossless_node, message)
        push!(state.diagnostics, diagnostic)
    end
    resolve_local(state, node, :super)
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
    declare!(state, node.name)
    define!(state, node.name)

    # Handle the function itself.
    enter_function_state(state, :function) do state
        analyze_function(state, node)
    end

    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.ClassDeclaration)
    # Class names are bound to the surrounding scope.
    declare!(state, node.name)
    define!(state, node.name)

    # Analyze the superclass if it exists.
    if !isnothing(node.superclass)
        # Disallow inheriting from self.
        if node.superclass.name.symbol == node.name.symbol
            diagnostic = SyntaxValidation.Diagnostic(
                node.superclass.lossless_node, "a class can't inherit from itself"
            )
            push!(state.diagnostics, diagnostic)
        end
        analyze(state, node.superclass)
    end

    # Handle the `super` keyword by conditionally entering a new scope and defining `super` in that scope.
    is_subclass = !isnothing(node.superclass)
    enter_scope(state; only_if=is_subclass) do state
        if is_subclass
            last(state.scopes)[:super] = true
        end

        # Handle the `this` keyword by entering a new scope defining `this` in that scope.
        enter_scope(state) do state
            last(state.scopes)[:this] = true

            # Enter class state so that references to `this`, `super` etc. are allowed.
            enter_class_state(state, is_subclass ? :subclass : :class) do state

                # Analyze the method definitions.
                for method_definition in node.methods

                    # Set the function state to :method (for context-specific validity checks).
                    # Unless this is an initializer, then set to :initializer.
                    is_initializer = method_definition.name.symbol == :init
                    enter_function_state(state, is_initializer ? :initializer : :method) do state
                        analyze_function(state, method_definition)
                    end
                end
            end
        end
    end

    return nothing
end

function analyze_function(state::ResolverState, node::LossyTrees.FunctionDeclaration)
    # Function declarations introduce a new scope.
    enter_scope(state) do state

        # Deal with the parameters.
        for param in node.parameters
            declare!(state, param)
            define!(state, param)
        end

        # Deal with the body, explicitly *not* entering a new scope for this block.
        analyze.(Ref(state), node.body.statements)
    end
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
    if current_function(state) == :none
        diagnostic = SyntaxValidation.Diagnostic(
            node.lossless_node, "can't return from top-level code"
        )
        push!(state.diagnostics, diagnostic)
    end
    if !isnothing(node.return_value)
        if current_function(state) == :initializer
            diagnostic = SyntaxValidation.Diagnostic(
                node.lossless_node, "can't return a value from and initializer"
            )
            push!(state.diagnostics, diagnostic)
        end
        analyze(state, node.return_value)
    end
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

function analyze(state::ResolverState, node::LossyTrees.Get)
    analyze(state, node.object)
    return nothing
end

function analyze(state::ResolverState, node::LossyTrees.Set)
    analyze(state, node.object)
    analyze(state, node.value)
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