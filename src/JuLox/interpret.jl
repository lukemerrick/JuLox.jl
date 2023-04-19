module Interpret
using Fractal.JuLox: Parse, Kind, @K_str, @KSet_str, kind, is_literal

function evaluate(node::Parse.SyntaxNode)
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
    return evaluate(c[1])
end

function evaluate_unary(node::Parse.SyntaxNode)
    c = Parse.children(node)
    @assert length(c) == 2
    operator, operand = c
    operator_kind = kind(operator)
    operand_value = evaluate(operand)
    if operator_kind == K"-"
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
    left_value = evaluate(left_node)
    right_value = evaluate(right_node)
    if operator_kind == K"-"
        return left_value - right_value
    elseif operator_kind == K"+"
        if isa(left_value, Float64) && isa(right_value, Float64)
            return left_value + right_value
        elseif isa(left_value, String) && isa(right_value, String)
            return left_value * right_value
        end
    elseif operator_kind == K"/"
        return left_value / right_value
    elseif operator_kind == K"*"
        return left_value * right_value
    elseif operator_kind == K">"
        return left_value > right_value
    elseif operator_kind == K">="
        return left_value >= right_value
    elseif operator_kind == K"<"
        return left_value < right_value
    elseif operator_kind == K"<="
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


function is_truthy(value)
    return !(isnothing(value) || value === false)
end


end  # end module Interpret