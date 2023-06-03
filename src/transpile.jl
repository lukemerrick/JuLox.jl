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

function transpile(state::TranspilerState, node::LossyTrees.Toplevel)
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
        return :(__truthy__($operand))
    end
    error("unreachable")
end


end  # module Transpile
