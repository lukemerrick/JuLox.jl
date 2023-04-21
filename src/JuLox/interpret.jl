module Interpret
using Fractal.JuLox: Parse, Kind, @K_str, @KSet_str, kind, is_literal, is_statement

struct RuntimeError <: Exception
    msg::String
    node::Parse.SyntaxNode
end

function interpret(node::Parse.SyntaxNode, source::String)
    try
        evaluate_statement(node)
        had_error = false
        return had_error
    catch e
        !isa(e, RuntimeError) && rethrow()
        lines = split(source[1:e.node.position], '\n')
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

function evaluate_statement(node::Parse.SyntaxNode)
    @assert is_statement(node)
    if kind(node) == K"expression_statement"
        return evaluate_expression_statement(node)
    elseif kind(node) == K"print_statement"
        return evaluate_print_statement(node)
    end

    # Unreachable.
    return nothing
end

function evaluate_expression_statement(node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    evaluate_expression(c[1])
    return nothing
end

function evaluate_print_statement(node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    println(stringify(evaluate_expression(c[1])))
    return nothing
end


function evaluate_expression(node::Parse.SyntaxNode)
    k = kind(node)
    is_literal(k) && return evaluate_literal(node)
    k == K"grouping" && return evaluate_grouping(node)
    k == K"unary" && return evaluate_unary(node)
    k == K"infix_operation" && return evaluate_infix_operation(node)
    return nothing
end

function evaluate_literal(node::Parse.SyntaxNode)
    return node.value
end

function evaluate_grouping(node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 1
    return evaluate_expression(c[1])
end

function evaluate_unary(node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 2
    operator, operand = c
    operator_kind = kind(operator)
    operand_value = evaluate_expression(operand)
    if operator_kind == K"-"
        raise_on_non_number_in_operation(node, operand_value)
        return -operand_value
    elseif operator_kind == K"!"
        return !is_truthy(operand_value)
    end

    # Unreachable.
    return nothing
end

function evaluate_infix_operation(node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 3
    left_node, operator_node, right_node = c
    operator_kind = kind(operator_node)
    left_value = evaluate_expression(left_node)
    right_value = evaluate_expression(right_node)
    if operator_kind == K"-"
        raise_on_non_number_in_operation(operator_node, left_value, right_value)
        return left_value - right_value
    elseif operator_kind == K"+"
        if isa(left_value, Float64) && isa(right_value, Float64)
            return left_value + right_value
        elseif isa(left_value, String) && isa(right_value, String)
            return left_value * right_value
        else
            throw(RuntimeError("Operands must be two numbers or two strings.", operator_node))
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
    !all(isa.(values, Float64)) && throw(RuntimeError("Oparation requires number operand(s).", operator_node))
    return nothing
end


function is_truthy(value)
    return !(isnothing(value) || value === false)
end


end  # end module Interpret