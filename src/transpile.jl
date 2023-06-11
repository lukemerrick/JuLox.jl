module Transpile
using JuLox: LossyTrees
using JuLox.Interpret: linecol


#-------------------------------------------------------------------------------
# Define functions that run inside the evaluation module as a runtime.

const JULOX_RUNTIME = quote
    struct RuntimeError <: Exception
        msg::String
        position::Int
    end

    function raise_on_non_number_in_operation(pos::Int, values::Any...)
        if !all(isa.(values, Float64))
            throw(RuntimeError("Oparation requires number operand(s).", pos))
        end
        return nothing
    end

    function truthy(value::Any)
        return !(isnothing(value) || value === false)
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
end

const JULOX_MANGLE_PREFIX = "__mangled"


#-------------------------------------------------------------------------------
# Transpiler state and evaluation.

struct TranspilerState
    output_io::IO
    error_io::IO
    mod::Module
    global_declarations::Set{LossyTrees.Identifier}
    local_scope_map::Dict{LossyTrees.AbstractExpression,Tuple{LossyTrees.Identifier,Int}}
    identifier_mangle_map::Dict{LossyTrees.Identifier,Symbol}
    identifier_mangle_counter::Ref{Int}

    function TranspilerState(output_io::IO, error_io::IO)
        # Create a fresh module to encapsulate the JuLox code.
        mod = Module()

        # Initialize JuLox runtime.
        Base.eval(mod, JULOX_RUNTIME)

        # Initialize empty sets/maps.
        global_declarations = Set{LossyTrees.Identifier}()
        local_scope_map = Dict{LossyTrees.AbstractExpression,Tuple{LossyTrees.Identifier,Int}}()
        identifier_mangle_map = Dict{LossyTrees.AbstractExpression,Symbol}()

        return new(output_io, error_io, mod, global_declarations, local_scope_map, identifier_mangle_map, Ref(1))
    end
end

TranspilerState() = TranspilerState(stdout, stderr)

global_declarations(state::TranspilerState) = state.global_declarations
local_scope_map(state::TranspilerState) = state.local_scope_map

function _local_mangle(state::TranspilerState, name_node::LossyTrees.Identifier)
    mangle_counter = state.identifier_mangle_counter[]
    state.identifier_mangle_counter[] = mangle_counter + 1
    lox_name = LossyTrees.value(name_node)
    return Symbol("$(JULOX_MANGLE_PREFIX)_$(mangle_counter)_$(string(lox_name))")
end

function _global_mangle(name_node::LossyTrees.Identifier)
    lox_name = LossyTrees.value(name_node)
    return Symbol("$(JULOX_MANGLE_PREFIX)_$(string(lox_name))")
end

function _declare_mangled_identifier_name(state::TranspilerState, node::LossyTrees.AbstractDeclaration)
    if node.name ∈ global_declarations(state)
        # In global scope, we eschew the counter to intentionally allow collisions.
        mangled_name = _global_mangle(node.name)
    else
        mangled_name = _local_mangle(state, node.name)
        # Cache local mangles for lookup via the resolution map.
        state.identifier_mangle_map[node.name] = mangled_name
    end

    # Return the name.
    return mangled_name
end

function _get_mangled_identifier_name(state::TranspilerState, node::LossyTrees.AbstractAccess)
    local_resolution = get(local_scope_map(state), node, nothing)
    if !isnothing(local_resolution)
        # If this access resolves to a locally declared entity, we use resolution to find the exact one.
        resolved_declaration_identifier, _ = local_resolution
        mangled_name = state.identifier_mangle_map[resolved_declaration_identifier]
    else
        # If we don't resolve locally, we want to mangle global style to match the global declaration(s).
        mangled_name = _global_mangle(node.name)
    end
    return mangled_name
end


function interpret_transpiled(state::TranspilerState, expr::Expr, source::String)
    had_error = false
    try
        Base.eval(state.mod, expr)
    catch e
        !isa(e, state.mod.RuntimeError) && rethrow()
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
    return :(println($out_io, stringify($expr)))
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
    bind_operand = Expr(:(=), :operand, operand)

    if node.operator isa LossyTrees.Operator{:-}
        # Special case on number literal.
        # TODO: Add check for invalid literal (nil, String) to semantic analysis.
        if isa(operand, Float64)
            return -operand
        end

        # General case of expression or variable.
        raise_on_nonnumber_operand = Expr(:call, :raise_on_non_number_in_operation, position(node.operator), :operand)
        negate_operand = Expr(:call, :-, :operand)
        return Expr(:block, bind_operand, raise_on_nonnumber_operand, negate_operand)

    elseif node.operator isa LossyTrees.Operator{:!}
        # Special case on literals.
        if isa(operand, Bool)
            return operand
        elseif isa(operand, String) || isa(operand, Float64)
            return true
        elseif isnothing(operand)
            return false
        end

        # General case of expression or variable.
        operand_is_truthy = Expr(:call, :truthy, operand)
        operand_is_not_truthy = Expr(:call, :!, operand_is_truthy)
        return Expr(:block, bind_operand, operand_is_not_truthy)
    end
    error("unreachable")
end

_op_kind(::LossyTrees.Operator{T}) where {T} = T

# TODO: Special case literals like in Unary.
function transpile(state::TranspilerState, node::LossyTrees.Infix)
    # First we bind the results of transpiling the left and right to new variables.
    bind_left = Expr(:(=), :left, transpile(state, node.left))
    bind_right = Expr(:(=), :right, transpile(state, node.right))
    bind_left_right = Expr(:block, bind_left, bind_right)

    # Then we transpile what to do with those new variables.
    operator = node.operator
    pos = position(node.operator)
    if node.operator isa LossyTrees.Operator{:+}
        eval_infix_expr = :(
            if isa(left, Float64) && isa(right, Float64)
                left + right
            elseif isa(left, String) && isa(right, String)
                left * right
            else
                throw(RuntimeError("Operands must be two numbers or two strings.", $pos))
            end
        )
    elseif node.operator isa LossyTrees.Operator{:(==)}
        # NOTE: Lox and Julia share the same equality logic on Lox types on nil/nothing,
        # but in Julia (not Lox) true == 1, so we need to special case this.
        eval_infix_expr = :(xor(left isa Bool, right isa Bool) ? false : left == right)
    elseif node.operator isa LossyTrees.Operator{:!=}
        # NOTE: Same special case as above in `==`.
        eval_infix_expr = :(xor(left isa Bool, right isa Bool) ? true : left != right)
    else
        # All other ops follow the same pattern of checking runtime types and then passing through cleanly.
        op_kind = _op_kind(operator)
        @assert op_kind ∈ [:-, :/, :*, :>, :>=, :<, :<=]
        eval_infix_expr = Expr(
            :block,
            :(raise_on_non_number_in_operation($pos, left, right)),
            Expr(:call, op_kind, :left, :right)
        )
    end
    return Expr(:let, bind_left_right, eval_infix_expr)
end

function transpile(state::TranspilerState, node::LossyTrees.VariableDeclaration)
    var_name::Symbol = _declare_mangled_identifier_name(state, node)
    var_expr = transpile(state, node.initializer)
    return Expr(:(=), var_name, var_expr)
end

function transpile(state::TranspilerState, node::LossyTrees.Assign)
    # Grab the name and value for the assignment.
    var_name::Symbol = _get_mangled_identifier_name(state, node)
    var_expr = transpile(state, node.value)

    # Prepare the resulting expression.
    result = Expr(:block, _raise_on_undefined(state, node), Expr(:(=), var_name, var_expr))
    return result
end

function transpile(state::TranspilerState, node::LossyTrees.Variable)
    return Expr(:block, _raise_on_undefined(state, node), _get_mangled_identifier_name(state, node))
end

function _raise_on_undefined(state::TranspilerState, node::LossyTrees.AbstractAccess)
    if node isa LossyTrees.Variable
        action = "get"
    elseif node isa LossyTrees.Assign
        action = "set"
    else
        error("Should only hit undefined on variables or assigment")
    end
    lox_name::Symbol = LossyTrees.value(node.name)
    julia_name::Symbol = _get_mangled_identifier_name(state, node)
    pos = position(node.name)
    error_message = "Cannot $(action) undefined variable '$(lox_name)'"
    return :(!(@isdefined $julia_name) && throw(RuntimeError($error_message, $pos)))
end

# function _try_get_identifier(pos::Int, lox_identifier::Symbol, item_kind::String)
#     julia_identifier = _mangle_identifier(lox_identifier)
#     error_message = "Cannot get undefined $(item_kind) '$(lox_identifier)'"
#     return quote
#         try
#             $julia_identifier
#         catch e
#             !isa(e, UndefVarError) && rethrow()
#             throw(RuntimeError($error_message, $pos))
#         end
#     end
# end



function transpile(state::TranspilerState, node::LossyTrees.If)
    condition = Expr(:call, :truthy, transpile(state, node.condition))
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
    condition = Expr(:call, :truthy, transpile(state, node.condition))
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
        return :(truthy($left) ? $left : $right)
    elseif op == :and
        return :(!truthy($left) ? $left : $right)
    else
        @assert false "expect :and or :or"
    end
end


end  # module Transpile
