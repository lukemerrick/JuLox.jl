module Interpret
using Fractal.JuLox: LossyTrees

struct RuntimeError <: Exception
    msg::String
    position::Int
end

#-------------------------------------------------------------------------------
# Environments to hold state and manage scope.


"""
    Environment(parent_env::Nothing)
    Environment(parent_env::Environment)

An environment holds state in a scope-managed way. If `parent_env == nothing`,
we create a global environment, otherwise we create a local environment nested within
the specified parent.
"""
struct Environment
    global_env::Union{Nothing,Environment}
    parent_env::Union{Nothing,Environment}
    values::Dict{Symbol,Any}

    function Environment(parent_env::Union{Nothing,Environment})
        if isnothing(parent_env)
            # This env is the global env, set it's `global_env` to `nothing` as a poor-man's
            # self-reference.
            global_env = nothing
        else
            if isnothing(parent_env.global_env)
                # The parent env is the global env since its `global_env` is set to
                # `nothing` as a marker.
                global_env = parent_env
            else
                # The parent env is not the global env, so it has an explicit referene to
                # the global env.
                global_env = parent_env.global_env
            end
        end
        # The `values` dict is always created from scratch when creating an `Environment`.
        values = Dict{Symbol,Any}()
        return new(global_env, parent_env, values)
    end
end

global_env(env::Environment) = isnothing(env.global_env) ? env : env.global_env
parent_env(env::Environment) = env.parent_env
values(env::Environment) = env.values

"""Get the environment a given distance up relative to the specified environment.

The relationship `environment_at(env, 0) == env` is always true.
"""
function environment_at(env::Environment, distance::Union{Nothing,Int})
    if isnothing(distance)
        return global_env(env)
    else
        selected_env = env
        for _ in 1:distance
            selected_env = parent_env(selected_env)
        end
        return selected_env
    end
end

function define!(environment::Environment, name::Symbol, value::Any)
    values(environment)[name] = value
end

function assign!(environment::Environment, distance::Union{Nothing,Int}, name::Symbol, value::Any, code_pos::Int)
    target_env = environment_at(environment, distance)
    target_values = values(target_env)
    if !haskey(target_values, name)
        throw(RuntimeError("Cannot assign undefined variable '$(name)'", code_pos))
    end
    target_values[name] = value
end

function Base.get(environment::Environment, distance::Union{Nothing,Int}, name::Symbol, code_pos::Int)
    target_env = environment_at(environment, distance)
    target_values = values(target_env)
    if !haskey(target_values, name)
        throw(RuntimeError("Cannot get undefined variable '$(name)'", code_pos))
    end
    return target_values[name]
end

function Base.string(environment::Environment)
    parent = parent_env(environment)
    parent_str = isnothing(parent) ? "<no parent>" : string(parent)
    values_str = string(Dict(key => typeof(value) for (key, value) in pairs(values(environment))))
    return "Environment(values=$(values_str), parent=$(parent_str))"
end

Base.show(io::IO, environment::Environment) = show(io, string(environment))

#-------------------------------------------------------------------------------
# Parser state composing Environment and name resolution.

mutable struct InterpreterState
    environment::Environment
    local_scope_map::Dict{LossyTrees.Expression,Int}

    function InterpreterState()
        # Initialize an empty global environment.
        global_environment = Environment(nothing)

        # Define the native functions in global scope.
        for native_fn in NATIVE_FUNCTIONS
            define!(global_environment, Symbol(native_fn.name), native_fn)
        end

        # Initialize an empty variable resolution map.
        local_scope_map = Dict{LossyTrees.Expression,Int}()

        return new(global_environment, local_scope_map)
    end
end

function update_local_scope_map!(state::InterpreterState, new_scope_map::Dict{LossyTrees.Expression,Int})
    merge!(state.local_scope_map, new_scope_map)
    return nothing
end

function enter_environment(f::Function, state::InterpreterState, environment::Environment; only_if::Bool=true)
    if only_if
        original_env = state.environment
        state.environment = environment
    end
    result = f(state)
    if only_if
        state.environment = original_env
    end
    return result
end

function enter_environment(f::Function, state::InterpreterState; only_if::Bool=true)
    return enter_environment(f, state, Environment(state.environment); only_if=only_if)
end

#-------------------------------------------------------------------------------
# Interface / structure for callables.

struct ReturnAsException <: Exception
    value::Any
end

# TODO: Make this an explicit Union of all allowed types? That would be stricter.
abstract type Callable end

# Callables must implement `arity` and `_call` and `string`!

# TODO: Be strict about type of values allowed as arguments etc.
# LoxValue = Union{String,Float64,Symbol,String,Callable,LoxInstance}
LoxValue = Any

function call(state::InterpreterState, callee::Callable, args::Vector{LoxValue}, code_position::Int)
    # Validate that arguments match callee arity.
    if arity(callee) != length(args)
        message = (
            "Callable $(string(callee)) expected $(arity(callee)) arguments but got " *
            "$(length(args))."
        )
        throw(RuntimeError(message, code_position))
    end

    # Run the actual callee-specific logic.
    return _call(state, callee, args, code_position)
end


#-------------------------------------------------------------------------------
# Native functions. Well, since there's just one, maybe "native function" is more apt.

struct NativeFunction <: Callable
    name::String
    arity::Int
    implementation::Function
end

Base.string(fn::NativeFunction) = "<native fn $(fn.name)>"
arity(fn::NativeFunction) = fn.arity
_call(state::InterpreterState, callee::NativeFunction, args::Vector{LoxValue}, code_position::Int) = callee.implementation(args, code_position)

const NATIVE_FUNCTIONS = NativeFunction[
    NativeFunction("clock", 0, (args, code_pos) -> time())
]


# Additional native functions for LoxLox support, see: https://github.com/benhoyt/loxlox
function try_read_char(args, code_position::Int)
    try
        c = read(stdin, Char)
        number = Float64(c)
        return number
    catch e
        @assert isa(e, EOFError)
        return -1.0
    end
end

function try_parse_number_to_char(args, code_position::Int)
    number = only(args)
    try
        return string(Char(number))
    catch e
        @assert isa(e, InexactError)
        throw(RuntimeError("Cannot convert $(number) to character.", code_position))
    end
end

append!(
    NATIVE_FUNCTIONS,
    NativeFunction[
        NativeFunction("getc", 0, try_read_char),
        NativeFunction("chr", 1, try_parse_number_to_char),
        NativeFunction("exit", 1, (args, code_pos) -> exit(only(args))),
        NativeFunction("print_error", 1, (args, code_pos) -> println(stderr, only(args)))
    ]
)

#-------------------------------------------------------------------------------
# Lox (aka non-native) functions.

struct LoxFunction <: Callable
    declaration::LossyTrees.FunctionDeclaration
    closure::Environment
    is_initializer::Bool
end

get_closured_this(lox_fn::LoxFunction) = get(lox_fn.closure, 0, :this, position(lox_fn.declaration))

function _call(state::InterpreterState, callee::LoxFunction, args::Vector{LoxValue}, code_position::Int)
    # Function execution happens in a new environment, with the function's closure as the
    # parent environment.
    result = enter_environment(state, Environment(callee.closure)) do fn_state
        # Define all parameter-named variables with argument-defined values.
        for (identifier, arg) in zip(callee.declaration.parameters, args)
            define!(fn_state.environment, identifier.symbol, arg)
        end

        # Try-catch so that we can interpret return statements via exceptions.
        try
            evaluate(fn_state, callee.declaration.body)
        catch e
            !isa(e, ReturnAsException) && rethrow()
            result = e.value
            if callee.is_initializer
                @assert isnothing(result)
                return get_closured_this(callee)
            end
            return result
        end

        # If we're in an initizlier, return the instance.
        if callee.is_initializer
            return get_closured_this(callee)
        end
        return nothing
    end
    return result
end

Base.string(fn::LoxFunction) = "<fn $(string(fn.declaration.name))>"
Base.show(fn::LoxFunction) = string(fn)
arity(fn::LoxFunction) = length(fn.declaration.parameters)

#-------------------------------------------------------------------------------
# Lox classes.

struct LoxClass <: Callable
    name::Symbol
    superclass::Union{Nothing,LoxClass}
    methods::Dict{Symbol,LoxFunction}
end

function _call(state::InterpreterState, callee::LoxClass, args::Vector{LoxValue}, code_position::Int)
    instance = LoxInstance(callee, Dict{Symbol,LoxValue}())
    initializer = find_method(callee, :init)
    if !isnothing(initializer)
        bound_initializer = bind(instance, initializer)
        code_position = position(initializer.declaration)
        call(state, bound_initializer, args, code_position)
    end
    return instance
end

function find_method(class::LoxClass, name::Symbol)
    result = get(class.methods, name, nothing)
    if isnothing(result) && !isnothing(class.superclass)
        result = get(class.superclass.methods, name, nothing)
    end
    return result
end

Base.string(c::LoxClass) = "<class $(string(c.name))>"
Base.show(c::LoxClass) = string(c)

function arity(c::LoxClass)
    initializer = find_method(c, :init)
    return isnothing(initializer) ? 0 : arity(initializer)
end

struct LoxInstance
    class::LoxClass
    fields::Dict{Symbol,LoxValue}
end

function bind(instance::LoxInstance, method::LoxFunction)
    binding_env = Environment(method.closure)
    define!(binding_env, :this, instance)
    bound_function = LoxFunction(method.declaration, binding_env, method.is_initializer)
    return bound_function
end

function Base.get(instance::LoxInstance, name::Symbol, code_position::Int)
    # Check fields first.
    haskey(instance.fields, name) && return instance.fields[name]

    # Then check class methods.
    method = find_method(instance.class, name)
    !isnothing(method) && return bind(instance, method)

    # If we find nothing, that's an error.
    throw(RuntimeError("Undefined property '$(name)'.", code_position))
end
Base.string(instance::LoxInstance) = "<instance of class $(string(instance.class.name))>"

function set!(instance::LoxInstance, name::Symbol, value::Any)
    instance.fields[name] = value
    return nothing
end


#-------------------------------------------------------------------------------
# The interpreter logic.

function linecol(pos::Int, text::String)
    lines = split(text[1:pos], '\n')
    line_number = length(lines)
    column_number = length(lines[end])
    return line_number, column_number
end

function interpret(state::InterpreterState, node::LossyTrees.Toplevel, source::String)
    try
        evaluate(state, node)
        had_error = false
        return had_error
    catch e
        !isa(e, RuntimeError) && rethrow()
        line_number, column_number = linecol(e.position, source)
        linecol_str = "[line $(lpad(line_number, 4)), column $(lpad(column_number, 3))]"
        println("Error @ $linecol_str - $(e.msg)")
        had_error = true
        return had_error
    end
end

function stringify(value)
    isnothing(value) && return "nil"
    if isa(value, Float64)
        text = string(value)
        if endswith(text, ".0")
            text = text[1:end-2]
        end
        return text
    end
    return string(value)
end

function evaluate(state::InterpreterState, node::LossyTrees.Toplevel)
    for statement in node.statements
        evaluate(state, statement)
    end
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.Block)
    # Use a new environment for the block scope.
    enter_environment(state) do block_state
        # Execute the block statements.
        for statement in node.statements
            evaluate(block_state, statement)
        end
    end
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.VariableDeclaration)
    initial_value = evaluate(state, node.initializer)
    identifier = node.name
    define!(state.environment, identifier.symbol, initial_value)
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.FunctionDeclaration)
    identifier = node.name
    define!(state.environment, identifier.symbol, LoxFunction(node, state.environment, false))
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.ClassDeclaration)
    # Handle superclass.
    if !isnothing(node.superclass)
        superclass = evaluate(state, node.superclass)
        # Ensure the superclass is actually a class.
        if !isa(superclass, LoxClass)
            throw(RuntimeError("Superclass must be a class", position(node.superclass)))
        end
    else
        superclass = nothing
    end

    identifier = node.name
    define!(state.environment, identifier.symbol, nothing)

    # Whip up an extra closure environment for subclasses to support `super`.
    is_subclass = !isnothing(node.superclass)
    class = enter_environment(state; only_if=is_subclass) do state
        is_subclass && define!(state.environment, :super, node.superclass)
        class_name = node.name.symbol
        class_methods = Dict{Symbol,LoxFunction}(
            decl.name.symbol => LoxFunction(decl, state.environment, decl.name.symbol == :init)
            for decl in node.methods
        )
        class = LoxClass(class_name, superclass, class_methods)
        return class
    end
    assign!(state.environment, 0, identifier.symbol, class, position(identifier))
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.ExpressionStatement)
    evaluate(state, node.expression)
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.ReturnStatement)
    return_value = isnothing(node.return_value) ? nothing : evaluate(state, node.return_value)
    throw(ReturnAsException(return_value))
end

function evaluate(state::InterpreterState, node::LossyTrees.Print)
    println(stringify(evaluate(state, node.expression)))
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.If)
    if is_truthy(evaluate(state, node.condition))
        evaluate(state, node.then_statement)
    else
        !isnothing(node.else_statement) && evaluate(state, node.else_statement)
    end
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.While)
    while is_truthy(evaluate(state, node.condition))
        evaluate(state, node.statement)
    end
    return nothing
end

function evaluate(state::InterpreterState, node::LossyTrees.Assign)
    value = evaluate(state, node.value)
    identifier = node.name
    distance = get(state.local_scope_map, node, nothing)
    assign!(state.environment, distance, identifier.symbol, value, position(identifier))
    return nothing
end

function evaluate(state::InterpreterState, node::Union{LossyTrees.Variable,LossyTrees.This})
    identifier = node.name
    distance = get(state.local_scope_map, node, nothing)
    return get(state.environment, distance, identifier.symbol, position(identifier))
end

function evaluate(state::InterpreterState, node::LossyTrees.Super)
    identifier = node.name
    distance = get(state.local_scope_map, node, nothing)
    superclass = get(state.environment, distance, :super, position(identifier))
    object = get(state.environment, distance - 1, :this, position(node))
    method = find_method(superclass, node.method_name.symbol)
    if isnothing(method)
        throw(RuntimeError("Undefined property '$(node.method_name.symbol)'", position(node.method_name)))
    end
    return bind(object, method)
end

function evaluate(node::LossyTrees.Literal)
    return LossyTrees.value(node)
end
# Match the API.
evaluate(state::InterpreterState, node::LossyTrees.Literal) = evaluate(node)

function evaluate(state::InterpreterState, node::LossyTrees.Grouping)
    return evaluate(state, node.expression)
end

function evaluate(state::InterpreterState, node::LossyTrees.Unary)
    operand_value = evaluate(state, node.right)
    operator = node.operator
    if node.operator isa LossyTrees.OperatorMinus
        raise_on_non_number_in_operation(operator, operand_value)
        return -operand_value
    elseif operator isa LossyTrees.OperatorBang
        return !is_truthy(operand_value)
    end

    # Unreachable.
    error("Should not reach here")
end

function evaluate(state::InterpreterState, node::LossyTrees.Infix)
    left_value = evaluate(state, node.left)
    right_value = evaluate(state, node.right)
    operator = node.operator
    if node.operator isa LossyTrees.OperatorMinus
        raise_on_non_number_in_operation(operator, left_value, right_value)
        return left_value - right_value
    elseif node.operator isa LossyTrees.OperatorPlus
        if isa(left_value, Float64) && isa(right_value, Float64)
            return left_value + right_value
        elseif isa(left_value, String) && isa(right_value, String)
            return left_value * right_value
        else
            throw(RuntimeError("Operands must be two numbers or two strings.", operator.position))
        end
    elseif node.operator isa LossyTrees.OperatorDivide
        raise_on_non_number_in_operation(operator, left_value, right_value)
        return left_value / right_value
    elseif node.operator isa LossyTrees.OperatorMultiply
        raise_on_non_number_in_operation(operator, left_value, right_value)
        return left_value * right_value
    elseif node.operator isa LossyTrees.OperatorMore
        raise_on_non_number_in_operation(operator, left_value, right_value)
        return left_value > right_value
    elseif node.operator isa LossyTrees.OperatorMoreEqual
        raise_on_non_number_in_operation(operator, left_value, right_value)
        return left_value >= right_value
    elseif node.operator isa LossyTrees.OperatorLess
        raise_on_non_number_in_operation(operator, left_value, right_value)
        return left_value < right_value
    elseif node.operator isa LossyTrees.OperatorLessEqual
        raise_on_non_number_in_operation(operator, left_value, right_value)
        return left_value <= right_value
    elseif node.operator isa LossyTrees.OperatorEqual
        # NOTE: Lox and Julia share the same equality logic on Lox types
        # (including nill/nothing)!
        return left_value == right_value
    elseif node.operator isa LossyTrees.OperatorNotEqual
        return left_value != right_value
    end

    # Unreachable.
    error("Should not reach here")
end

function evaluate(state::InterpreterState, node::LossyTrees.Call)
    callee = evaluate(state, node.callee)
    if !isa(callee, Callable)
        throw(RuntimeError("Can only call functions and classess.", position(node.callee)))
    end
    arguments = LoxValue[evaluate(state, arg) for arg in node.arguments]
    return call(state, callee, arguments, position(node))
end


function evaluate(state::InterpreterState, node::LossyTrees.Get)
    object = evaluate(state, node.object)
    if !isa(object, LoxInstance)
        throw(RuntimeError("Only instances have properties.", position(node.object)))
    end
    return get(object, node.name.symbol, position(node.name))
end


function evaluate(state::InterpreterState, node::LossyTrees.Set)
    object = evaluate(state, node.object)
    if !isa(object, LoxInstance)
        throw(RuntimeError("Only instances have fields.", position(node.object)))
    end
    value = evaluate(state, node.value)
    set!(object, node.name.symbol, value)
    return nothing
end


function evaluate(state::InterpreterState, node::LossyTrees.Logical)
    left_value = evaluate(state, node.left)
    if (
        (node.operator isa LossyTrees.OperatorOr && is_truthy(left_value))
        ||
        (node.operator isa LossyTrees.OperatorAnd && !is_truthy(left_value))
    )
        return left_value
    end
    return evaluate(state, node.right)
end

function raise_on_non_number_in_operation(operator_node::LossyTrees.Operator, values::Any...)
    pos = position(operator_node)
    if !all(isa.(values, Float64))
        throw(RuntimeError("Oparation requires number operand(s).", pos))
    end
    return nothing
end


function is_truthy(value)
    return !(isnothing(value) || value === false)
end


end  # end module Interpret