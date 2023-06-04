module Transpile
using JuLox: LossyTrees
using JuLox.Interpret: linecol


#-------------------------------------------------------------------------------
# Define functions that run inside the evaluation module as a runtime.

const JULOX_RUNTIME = quote
    struct __RuntimeError__ <: Exception
        msg::String
        position::Int
    end

    function __raise_on_non_number_in_operation__(pos::Int, values::Any...)
        if !all(isa.(values, Float64))
            throw(__RuntimeError__("Oparation requires number operand(s).", pos))
        end
        return nothing
    end

    function __truthy__(value::Any)
        return !(isnothing(value) || value === false)
    end

    function __stringify__(value)
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
end

const JULOX_MANGLE = "__loxname_"

_mangle_identifier(s::Symbol) = Symbol(JULOX_MANGLE * string(s))

#-------------------------------------------------------------------------------
# Transpiler state and evaluation.

struct TranspilerState
    output_io::IO
    error_io::IO
    mod::Module
    local_scope_map::Dict{LossyTrees.AbstractExpression,Int}

    function TranspilerState(output_io::IO, error_io::IO)
        # Create a fresh module to encapsulate the JuLox code.
        mod = Module()

        # Initialize JuLox runtime.
        Base.eval(mod, JULOX_RUNTIME)

        # Initialize an empty variable resolution map.
        local_scope_map = Dict{LossyTrees.AbstractExpression,Int}()

        return new(output_io, error_io, mod, local_scope_map)
    end
end

TranspilerState() = TranspilerState(stdout, stderr)

local_scope_map(state::TranspilerState) = state.local_scope_map

function interpret_transpiled(state::TranspilerState, expr::Expr, source::String)
    had_error = false
    try
        Base.eval(state.mod, expr)
    catch e
        !isa(e, state.mod.__RuntimeError__) && rethrow()
        line_number, column_number = linecol(e.position, source)
        linecol_str = "[line $(lpad(line_number, 4)), column $(lpad(column_number, 3))]"
        println(state.error_io, "Error @ $linecol_str - $(e.msg)")
        # Print just line for Lox test compatibility.
        println(state.error_io, "[line $(line_number)]")
        had_error = true
        had_error = true
    end
    return had_error
end


#-------------------------------------------------------------------------------
# Transpiler logic.

function _blockify(state::TranspilerState, statements::Vector{<:LossyTrees.AbstractStatement})
    res = Expr(:block)
    res.args = [transpile(state, statement) for statement in statements]
    return res
end

function transpile(state::TranspilerState, node::LossyTrees.Toplevel)
    return _blockify(state, node.statements)
end

function transpile(state::TranspilerState, node::LossyTrees.Block)
    # In Julia, :block doesn't introduce nested hard scope, while :let does, so for a Lox block
    # we wrap a Julia :block with a Julia :let.
    # The extra Expr(:block) is equivalent to a newline after `let` in Julia source and is needed in this case.
    return Expr(:let, Expr(:block), _blockify(state, node.statements))
end

function transpile(state::TranspilerState, node::LossyTrees.ExpressionStatement)
    return transpile(state, node.expression)
end

function transpile(state::TranspilerState, node::LossyTrees.Print)
    out_io = state.output_io
    expr = transpile(state, node.expression)
    return :(println($out_io, __stringify__($expr)))
end

function transpile(state::TranspilerState, node::LossyTrees.LeafValue)
    result = LossyTrees.value(node)
    @assert typeof(result) ∉ Set([Symbol, LossyTrees.ThisValue, LossyTrees.SuperValue]) "Expect literal"
    return result
end

function transpile(state::TranspilerState, node::LossyTrees.Grouping)
    return transpile(state, node.expression)
end

function transpile(state::TranspilerState, node::LossyTrees.Unary)
    # Since the operand can be complicated, we transpile to a variable assingment for re-use in checks.
    operand = transpile(state, node.right)
    bind_operand = Expr(:(=), :__julox_operand__, operand)

    if node.operator isa LossyTrees.Operator{:-}
        # Special case on number literal.
        # TODO: Add check for invalid literal (nil, String) to semantic analysis.
        if isa(operand, Float64)
            return -operand
        end

        # General case of expression or variable.
        raise_on_nonnumber_operand = Expr(:call, :__raise_on_non_number_in_operation__, position(node.operator), :__julox_operand__)
        negate_operand = Expr(:call, :-, :__julox_operand__)
        return Expr(:block, bind_operand, raise_on_nonnumber_operand, negate_operand)

    elseif operator isa LossyTrees.Operator{:!}
        # Special case on literals.
        if isa(operand, Bool)
            return operand
        elseif isa(operand, String) || isa(operand, Float64)
            return true
        elseif isnothing(operand)
            return false
        end

        # General case of expression or variable.
        operand_is_truthy = Expr(:call, :__truthy__, __julox_operand__)
        operand_is_not_truthy = Expr(:call, :!, operand_is_truthy)
        return Expr(:block, bind_operand, operand_is_not_truthy)
    end
    error("unreachable")
end

_op_kind(::LossyTrees.Operator{T}) where {T} = T

# TODO: Special case literals like in Unary.
function transpile(state::TranspilerState, node::LossyTrees.Infix)
    # First we bind the results of transpiling the left and right to new variables.
    bind_left = Expr(:(=), :__julox_left__, transpile(state, node.left))
    bind_right = Expr(:(=), :__julox_right__, transpile(state, node.right))
    bind_left_right = Expr(:block, bind_left, bind_right)

    # Then we transpile what to do with those new variables.
    operator = node.operator
    pos = position(node.operator)
    if node.operator isa LossyTrees.Operator{:+}
        eval_infix_expr = :(
            if isa(__julox_left__, Float64) && isa(__julox_right__, Float64)
                __julox_left__ + __julox_right__
            elseif isa(__julox_left__, String) && isa(__julox_right__, String)
                __julox_left__ * __julox_right__
            else
                throw(__RuntimeError__("Operands must be two numbers or two strings.", $pos))
            end
        )
    elseif node.operator isa LossyTrees.Operator{:(==)}
        # NOTE: Lox and Julia share the same equality logic on Lox types on nil/nothing,
        # but in Julia (not Lox) true == 1, so we need to special case this.
        eval_infix_expr = :(xor(__julox_left__ isa Bool, __julox_right__ isa Bool) ? false : __julox_left__ == __julox_right__)
    elseif node.operator isa LossyTrees.Operator{:!=}
        # NOTE: Same special case as above in `==`.
        eval_infix_expr = :(xor(__julox_left__ isa Bool, __julox_right__ isa Bool) ? true : __julox_left__ != __julox_right__)
    else
        # All other ops follow the same pattern of checking runtime types and then passing through cleanly.
        op_kind = _op_kind(operator)
        @assert op_kind ∈ [:-, :/, :*, :>, :>=, :<, :<=]
        eval_infix_expr = Expr(
            :block,
            :(__raise_on_non_number_in_operation__($pos, __julox_left__, __julox_right__)),
            Expr(:call, op_kind, :__julox_left__, :__julox_right__)
        )
    end
    return Expr(:let, bind_left_right, eval_infix_expr)
end

function transpile(state::TranspilerState, node::LossyTrees.VariableDeclaration)
    var_name::Symbol = _mangle_identifier(LossyTrees.value(node.name))
    var_expr = transpile(state, node.initializer)
    return Expr(:(=), var_name, var_expr)
end

function transpile(state::TranspilerState, node::LossyTrees.Assign)
    # NOTE: Since static analysis checks for nonexistent symbols before the transpilation step,
    # `var_name` must already be defined in the current Julia scope.
    # If we don't want to trust the static analysis step, we can add a runtime function
    # akin to `__raise_on_non_number_in_operation__` that uses the `@isdefined` macro.

    # Grab the name and value for the assignment.
    var_name::Symbol = _mangle_identifier(LossyTrees.value(node.name))
    var_expr = transpile(state, node.value)

    # Prepare the resulting expression.
    result = Expr(:(=), var_name, var_expr)

    # If the variable is global, we need to transpile to `global x = "foo"` instead of `x = "foo"`.
    # See: https://docs.julialang.org/en/v1/manual/variables-and-scoping/#local-scope
    is_global_target = !haskey(state.local_scope_map, node)
    if is_global_target
        result = Expr(:global, result)
    end
    return result
end

function _raise_on_undefined(pos::Int, lox_identifier::Symbol, item_kind::String)
    julia_identifier = _mangle_identifier(lox_identifier)
    error_message = "Cannot get undefined $(item_kind) '$(lox_identifier)'"
    return :(!(@isdefined $julia_identifier) && throw(__RuntimeError__($error_message, $pos)))
end

function transpile(state::TranspilerState, node::LossyTrees.Variable)
    lox_identifier::Symbol = LossyTrees.value(node.name)
    raise_on_undefined_expr = _raise_on_undefined(position(node.name), lox_identifier, "variable")
    return Expr(:block, raise_on_undefined_expr, _mangle_identifier(lox_identifier))
end

function transpile(state::TranspilerState, node::LossyTrees.If)
    condition = Expr(:call, :__truthy__, transpile(state, node.condition))
    then_statement = transpile(state, node.then_statement)
    if isnothing(node.else_statement)
        return Expr(:if, condition, then_statement)
    else
        else_statement = transpile(state, node.else_statement)
        return Expr(:if, condition, then_statement, else_statement)
    end
    error("unreachable")
end

function transpile(state::TranspilerState, node::LossyTrees.While)
    condition = Expr(:call, :__truthy__, transpile(state, node.condition))
    if isnothing(node.statement)
        return Expr(:while, condition)
    else
        statement = transpile(state, node.statement)
        return Expr(:while, condition, statement)
    end
    error("unreachable")
end

function transpile(state::TranspilerState, node::LossyTrees.Logical)
    left = transpile(state, node.left)
    right = transpile(state, node.right)
    op = _op_kind(node.operator)
    if op == :or
        return :(__truthy__($left) ? $left : $right)
    elseif op == :and
        return :(!__truthy__($left) ? $left : $right)
    else
        @assert false "expect :and or :or"
    end
end


end  # module Transpile
