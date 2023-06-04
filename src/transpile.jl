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

    function TranspilerState(output_io::IO, error_io::IO)
        # Create a fresh module to encapsulate the JuLox code.
        mod = Module()

        # Initialize JuLox runtime.
        Base.eval(mod, JULOX_RUNTIME)

        return new(output_io, error_io, mod)
    end
end

TranspilerState() = TranspilerState(stdout, stderr)

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

function transpile(state::TranspilerState, node::Union{LossyTrees.Toplevel,LossyTrees.Block})
    return Expr(:block, (transpile(state, statement) for statement in node.statements)...)
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
    @assert typeof(result)  ∉ Set([Symbol, LossyTrees.ThisValue, LossyTrees.SuperValue])
    return result
end

function transpile(state::TranspilerState, node::LossyTrees.Grouping)
    return transpile(state, node.expression)
end

function transpile(state::TranspilerState, node::LossyTrees.Unary)
    operand = transpile(state, node.right)
    operator = node.operator
    if node.operator isa LossyTrees.Operator{:-}
        pos = position(operator)
        return quote
            __raise_on_non_number_in_operation__($pos, $operand)
            -$operand
        end
    elseif operator isa LossyTrees.Operator{:!}
        return :(!__truthy__($operand))
    end
    error("unreachable")
end

_op_kind(::LossyTrees.Operator{T}) where T = T

function transpile(state::TranspilerState, node::LossyTrees.Infix)
    left = transpile(state, node.left)
    right = transpile(state, node.right)
    operator = node.operator
    pos = position(node.operator)
    if node.operator isa LossyTrees.Operator{:+}
        return quote
            if isa($left, Float64) && isa($right, Float64)
                $left + $right
            elseif isa($left, String) && isa($right, String)
                $left * $right
            else
                throw(__RuntimeError__("Operands must be two numbers or two strings.", $pos))
            end
        end
    elseif node.operator isa LossyTrees.Operator{:(==)}
        # NOTE: Lox and Julia share the same equality logic on Lox types on nil/nothing,
        # but in Julia (not Lox) true == 1, so we need to special case this.
        return quote
            if xor($left isa Bool, $right isa Bool)
                false
            else
                $left == $right
            end
        end
    elseif node.operator isa LossyTrees.Operator{:!=}
        # NOTE: Lox and Julia share the same equality logic on Lox types on nil/nothing,
        # but in Julia (not Lox) true == 1, so we need to special case this.
        return quote
            if xor($left isa Bool, $right isa Bool)
                true
            else
                $left != $right
            end
        end
    else
        # All other ops follow the same pattern of checking runtime types and then passing through cleanly.
        op_kind = _op_kind(operator)
        @assert op_kind ∈ [:-, :/, :*, :>, :>=, :<, :<=]
        return Expr(
            :block,
            :(__raise_on_non_number_in_operation__($pos, $left, $right)),
            Expr(:call, op_kind, left, right)
        )
    end
end

function transpile(state::TranspilerState, node::LossyTrees.VariableDeclaration)
    var_name::Symbol = _mangle_identifier(LossyTrees.value(node.name))
    var_expr = transpile(state, node.initializer)
    return Expr(:(=), var_name, var_expr)
end

function transpile(state::TranspilerState, node::LossyTrees.Assign)
    var_name::Symbol = _mangle_identifier(LossyTrees.value(node.name))
    var_expr = transpile(state, node.value)
    # NOTE: Since static analysis checks for nonexistent symbols before the transpilation step,
    # `var_name` must already be defined in the current Julia scope.
    # If we don't want to trust the static analysis step, we can add a runtime function
    # akin to `__raise_on_non_number_in_operation__` that uses the `@isdefined` macro.
    return Expr(:(=), var_name, var_expr)
end

function transpile(state::TranspilerState, node::LossyTrees.Variable)
    var_name::Symbol = _mangle_identifier(LossyTrees.value(node.name))
    return var_name
end

end  # module Transpile
