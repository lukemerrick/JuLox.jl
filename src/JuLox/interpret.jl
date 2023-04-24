module Interpret
using Fractal.JuLox: Parse, Kind, @K_str, @KSet_str, kind, is_literal, is_statement

struct RuntimeError <: Exception
    msg::String
    position::Int
end

struct Environment
    enclosing::Union{Nothing,Environment}
    values::Dict{Symbol,Any}

    # The `values` dict is always created from scratch when creating an `Environment`.
    function Environment(enclosing::Union{Nothing,Environment})
        return new(enclosing, Dict{Symbol,Any}())
    end
end

Environment() = Environment(nothing)

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

function interpret(environment::Environment, node::Parse.SyntaxNode, source::String)
    try
        evaluate_toplevel(environment, node)
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

function evaluate_toplevel(environment::Environment, node::Parse.SyntaxNode)
    @assert kind(node) == K"toplevel"
    for statement in Parse.children(node)
        evaluate_statement(environment, statement)
    end
    return nothing
end

function evaluate_block_statement(environment::Environment, node::Parse.SyntaxNode)
    @assert kind(node) == K"block"
    block_environment = Environment(environment)
    for statement in Parse.children(node)
        evaluate_statement(block_environment, statement)
    end
    return nothing
end

function evaluate_statement(environment::Environment, node::Parse.SyntaxNode)
    @assert is_statement(node)
    k = kind(node)
    if k == K"expression_statement"
        return evaluate_expression_statement(environment, node)
    elseif k == K"print_statement"
        return evaluate_print_statement(environment, node)
    elseif k == K"var_decl_statement"
        evaluate_var_statement(environment, node)
    elseif k == K"block"
        evaluate_block_statement(environment, node)
    end

    # Unreachable.
    return nothing
end

function evaluate_var_statement(environment::Environment, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) ∈ (1, 2)
    var = c[1]
    @assert kind(var) == K"Identifier"
    name = var.value
    value = length(c) == 2 ? evaluate_expression(environment, c[2]) : nothing
    define!(environment, name, value)
    return nothing
end

function evaluate_expression_statement(environment::Environment, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    evaluate_expression(environment, c[1])
    return nothing
end

function evaluate_print_statement(environment::Environment, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    println(stringify(evaluate_expression(environment, c[1])))
    return nothing
end

function evaluate_expression(environment::Environment, node::Parse.SyntaxNode)
    k = kind(node)
    k == K"Identifier" && return evaluate_variable(environment, node)
    is_literal(k) && return evaluate_literal(node)
    k == K"grouping" && return evaluate_grouping(environment, node)
    k == K"unary" && return evaluate_unary(environment, node)
    k == K"infix_operation" && return evaluate_infix_operation(environment, node)
    k == K"assignment" && return evaluate_assignment(environment, node)
    return nothing
end

function evaluate_assignment(environment::Environment, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 2
    l_node, r_node = c
    @assert kind(l_node) == K"Identifier"
    name = l_node.value
    value = evaluate_expression(environment, r_node)
    assign!(environment, name, value, l_node.position)
    return nothing
end

function evaluate_variable(environment::Environment, node::Parse.SyntaxNode)
    return get(environment, node.value, node.position)
end

function evaluate_literal(node::Parse.SyntaxNode)
    return node.value
end

function evaluate_grouping(environment::Environment, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    return evaluate_expression(environment, c[1])
end

function evaluate_unary(environment::Environment, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 2
    operator, operand = c
    operator_kind = kind(operator)
    operand_value = evaluate_expression(environment, operand)
    if operator_kind == K"-"
        raise_on_non_number_in_operation(operator, operand_value)
        return -operand_value
    elseif operator_kind == K"!"
        return !is_truthy(operand_value)
    end

    # Unreachable.
    return nothing
end

function evaluate_infix_operation(environment::Environment, node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 3
    left_node, operator_node, right_node = c
    operator_kind = kind(operator_node)
    left_value = evaluate_expression(environment, left_node)
    right_value = evaluate_expression(environment, right_node)
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