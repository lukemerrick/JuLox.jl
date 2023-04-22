module Interpret
using Fractal.JuLox: Parse, Kind, @K_str, @KSet_str, kind, is_literal, is_statement

struct RuntimeError <: Exception
    msg::String
    position::Int
end

struct Environment
    values::Dict{Symbol,Any}

    function Environment()
        values = Dict{Symbol,Any}()
        return new(values)
    end
end

struct Interpreter
    env::Environment

    function Interpreter()
        env = Environment()
        return new(env)
    end
end

function define!(env::Environment, name::Symbol, value::Any)
    env.values[name] = value
end

function get(env::Environment, name::Symbol, code_position::Int)
    haskey(env.values, name) && return env.values[name]
    throw(RuntimeError("Undefined variable $(name)", code_position))
end

function interpret(interpreter::Interpreter, node::Parse.SyntaxNode, source::String)
    try
        evaluate_statement(interpreter, node)
        had_error = false
        return had_error
    catch e
        !isa(e, RuntimeError) && rethrow()
        lines = split(source[1:e.position], '\n')
        linecol = "[line $(length(lines)), column $(length(lines[end]))]"
        println("Error @ $linecol - $(e.msg)")
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

function evaluate_statement(interpreter::Interpreter, node::Parse.SyntaxNode)
    @assert is_statement(node)
    if kind(node) == K"expression_statement"
        return evaluate_expression_statement(interpreter, node)
    elseif kind(node) == K"print_statement"
        return evaluate_print_statement(interpreter, node)
    elseif kind(node) == K"var_decl_statement"
        evaluate_var_statement(interpreter, node)
    end

    # Unreachable.
    return nothing
end

function evaluate_var_statement(interpreter::Interpreter, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) ∈ (1, 2)
    var = c[1]
    @assert kind(var) == K"Identifier"
    name = var.value
    value = length(c) == 2 ? evaluate_expression(interpreter, c[2]) : nothing
    define!(interpreter.env, name, value)
    return nothing
end

function evaluate_expression_statement(interpreter::Interpreter, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    evaluate_expression(interpreter, c[1])
    return nothing
end

function evaluate_print_statement(interpreter::Interpreter, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    println(stringify(evaluate_expression(interpreter, c[1])))
    return nothing
end

function evaluate_expression(interpreter::Interpreter, node::Parse.SyntaxNode)
    k = kind(node)
    k == K"Identifier" && return evaluate_variable(interpreter, node)
    is_literal(k) && return evaluate_literal(node)
    k == K"grouping" && return evaluate_grouping(interpreter, node)
    k == K"unary" && return evaluate_unary(interpreter, node)
    k == K"infix_operation" && return evaluate_infix_operation(interpreter, node)
    return nothing
end

function evaluate_variable(interpreter::Interpreter, node::Parse.SyntaxNode)
    return get(interpreter.env, node.value, node.position)
end

function evaluate_literal(node::Parse.SyntaxNode)
    return node.value
end

function evaluate_grouping(interpreter::Interpreter, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    return evaluate_expression(interpreter, c[1])
end

function evaluate_unary(interpreter::Interpreter, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 2
    operator, operand = c
    operator_kind = kind(operator)
    operand_value = evaluate_expression(interpreter, operand)
    if operator_kind == K"-"
        raise_on_non_number_in_operation(operator, operand_value)
        return -operand_value
    elseif operator_kind == K"!"
        return !is_truthy(operand_value)
    end

    # Unreachable.
    return nothing
end

function evaluate_infix_operation(interpreter::Interpreter, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 3
    left_node, operator_node, right_node = c
    operator_kind = kind(operator_node)
    left_value = evaluate_expression(interpreter, left_node)
    right_value = evaluate_expression(interpreter, right_node)
    if operator_kind == K"-"
        raise_on_non_number_in_operation(operator_node, left_value, right_value)
        return left_value - right_value
    elseif operator_kind == K"+"
        if isa(left_value, Float64) && isa(right_value, Float64)
            return left_value + right_value
        elseif isa(left_value, String) && isa(right_value, String)
            return left_value * right_value
        else
            throw(RuntimeError("Operands must be two numbers or two strings.", operator_node.position))
        end
    elseif operator_kind == K"/"
        raise_on_non_number_in_operation(operator_node, left_value, right_value)
        return left_value / right_value
    elseif operator_kind == K"*"
        raise_on_non_number_in_operation(operator_node, left_value, right_value)
        return left_value * right_value
    elseif operator_kind == K">"
        raise_on_non_number_in_operation(operator_node, left_value, right_value)
        return left_value > right_value
    elseif operator_kind == K">="
        raise_on_non_number_in_operation(operator_node, left_value, right_value)
        return left_value >= right_value
    elseif operator_kind == K"<"
        raise_on_non_number_in_operation(operator_node, left_value, right_value)
        return left_value < right_value
    elseif operator_kind == K"<="
        raise_on_non_number_in_operation(operator_node, left_value, right_value)
        return left_value <= right_value
    elseif operator_kind == K"=="
        # NOTE: Lox and Julia share the same equality logic on Lox types
        # (including nill/nothing)!
        return left_value == right_value
    elseif operator_kind == K"!="
        return left_value != right_value
    end

    # Unreachable.
    return nothing
end

function raise_on_non_number_in_operation(operator_node::Parse.SyntaxNode, values::Any...)
    !all(isa.(values, Float64)) && throw(RuntimeError("Oparation requires number operand(s).", operator_node.position))
    return nothing
end


function is_truthy(value)
    return !(isnothing(value) || value === false)
end


end  # end module Interpret