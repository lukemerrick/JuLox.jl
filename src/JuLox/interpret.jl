module Interpret
using Fractal.JuLox: LossyTrees

struct RuntimeError <: Exception
    msg::String
    position::Int
end

#-------------------------------------------------------------------------------
# Environments to hold state and manage scope.

struct Environment
    enclosing::Union{Nothing,Environment}
    values::Dict{Symbol,Any}

    # The `values` dict is always created from scratch when creating an `Environment`.
    function Environment(enclosing::Union{Nothing,Environment})
        return new(enclosing, Dict{Symbol,Any}())
    end
end

Environment() = Environment(nothing)

function initialize_global_environment()
    env = Environment()
    define!(env, :clock, CLOCK_NATIVE_FN)
    return env
end

function define!(environment::Environment, name::Symbol, value::Any)
    environment.values[name] = value
end

function assign!(environment::Environment, name::Symbol, value::Any, code_position::Int)
    if name ∈ keys(environment.values)
        # Use local scope if possible.
        environment.values[name] = value
    elseif !isnothing(environment.enclosing)
        # Fall back to enclosing scope.
        assign!(environment.enclosing, name, value, code_position)
    else
        # Throw an error if we can't find the `name`-ed item.
        throw(RuntimeError("Undefined variable $(name)", code_position))
    end
end

function get(environment::Environment, name::Symbol, code_position::Int)
    # Check the local scope.
    haskey(environment.values, name) && return environment.values[name]

    # Fall back to enclosing scope.
    !isnothing(environment.enclosing) && return get(environment.enclosing, name, code_position)

    # Throw an error if we can't find the `name`-ed item.
    throw(RuntimeError("Undefined variable $(name)", code_position))
end

#-------------------------------------------------------------------------------
# Interface / structure for callables.

# TODO: Make this an explicit Union of all allowed types? That would be stricter.
abstract type Callable end

# Callables must implement `arity` and `_call` and `string`!

# Be strict about type of values allowed as arguments.
ArgType = Union{String,Float64,Symbol,String,Callable}

function call(environment::Environment, callee::Callable, args::Vector{ArgType}, code_position::Int)
    # Validate that arguments match callee arity.
    if arity(callee) != length(args)
        message = (
            "Callable $(string(callee)) expected $(arity(callee)) arguments but got " *
            "$(length(args))"
        )
        throw(RuntimeError(message, code_position))
    end

    # Run the actual callee-specific logic.
    return _call(environment, callee, args)
end


#-------------------------------------------------------------------------------
# Native functions. Well, since there's just one, maybe "native function" is more apt.

struct NativeFunction <: Callable
    name::String
    arity::Int
    implementation::Function
end

Base.string(fn::NativeFunction) = fn.name
arity(fn::NativeFunction) = fn.arity
_call(environment::Environment, callee::NativeFunction, args::Vector{ArgType}) = callee.implementation(args)

const CLOCK_NATIVE_FN = NativeFunction("clock", 0, args -> time())


#-------------------------------------------------------------------------------
# Lox functions.

struct LoxFunction <: Callable
    declaration::LossyTrees.FunctionDeclaration
end

function _call(environment::Environment, callee::LoxFunction, args::Vector{ArgType})
    function_env = Environment(environment)
    for (identifier, arg) in zip(callee.declaration.parameters, args)
        define!(function_env, identifier.symbol, arg)
    end
    evaluate(function_env, callee.declaration.body)
    return nothing
end

Base.string(fn::LoxFunction) = "<fn $(string(fn.declaration.name))>"
arity(fn::LoxFunction) = length(fn.declaration.parameters)

#-------------------------------------------------------------------------------
# The interpreter logic.

function linecol(pos::Int, text::String)
    lines = split(text[1:pos], '\n')
    line_number = length(lines)
    column_number = length(lines[end])
    return line_number, column_number
end

function interpret(environment::Environment, node::LossyTrees.Toplevel, source::String)
    try
        evaluate(environment, node)
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

function evaluate(environment::Environment, node::LossyTrees.Toplevel)
    for statement in node.statements
        evaluate(environment, statement)
    end
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.Block)
    block_environment = Environment(environment)
    for statement in node.statements
        evaluate(block_environment, statement)
    end
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.VariableDeclaration)
    initial_value = evaluate(environment, node.initializer)
    identifier = node.name
    define!(environment, identifier.symbol, initial_value)
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.FunctionDeclaration)
    identifier = node.name
    define!(environment, identifier.symbol, LoxFunction(node))
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.ExpressionStatement)
    evaluate(environment, node.expression)
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.Print)
    println(stringify(evaluate(environment, node.expression)))
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.If)
    if is_truthy(evaluate(environment, node.condition))
        evaluate(environment, node.then_statement)
    else
        !isnothing(node.else_statement) && evaluate(environment, node.else_statement)
    end
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.While)
    while is_truthy(evaluate(environment, node.condition))
        evaluate(environment, node.statement)
    end
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.Assign)
    value = evaluate(environment, node.value)
    identifier = node.name
    assign!(environment, identifier.symbol, value, position(node.name))
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.Variable)
    identifier = node.name
    return get(environment, identifier.symbol, position(node))
end

function evaluate(node::LossyTrees.Literal)
    return LossyTrees.value(node)
end
# Match the API.
evaluate(environment::Environment, node::LossyTrees.Literal) = evaluate(node)

function evaluate(environment::Environment, node::LossyTrees.Grouping)
    return evaluate(environment, node.expression)
end

function evaluate(environment::Environment, node::LossyTrees.Unary)
    operand_value = evaluate(environment, node.right)
    operator = node.operator
    if node.operator isa LossyTrees.OperatorMinus
        raise_on_non_number_in_operation(operator, operand_value)
        return -operand_value
    elseif operator isa LossyTrees.OperatorBang
        return !is_truthy(operand_value)
    end

    # Unreachable.
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.Infix)
    left_value = evaluate(environment, node.left)
    right_value = evaluate(environment, node.right)
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
    return nothing
end

function evaluate(environment::Environment, node::LossyTrees.Call)
    callee = evaluate(environment, node.callee)
    arguments = ArgType[evaluate(environment, arg) for arg in node.arguments]
    return call(environment, callee, arguments, position(node))
end


function evaluate(environment::Environment, node::LossyTrees.Logical)
    left_value = evaluate(environment, node.left)
    if (
        (node.operator isa LossyTrees.OperatorOr && is_truthy(left_value))
        ||
        (node.operator isa LossyTrees.OperatorAnd && !is_truthy(left_value))
    )
        return left_value
    end
    return evaluate(environment, node.right)
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