# Provides a typed API for syntax trees.

module LossyTrees

using JuLox: JuLox, SyntaxKinds, Parse, LosslessTrees, Tokenize
using JuLox.SyntaxKinds: @K_str, @KSet_str

# Define an abstract type hierarchy.
abstract type AbstractLossyNode end
abstract type AbstractLogicOperator <: AbstractLossyNode end
abstract type AbstractExpression <: AbstractLossyNode end
abstract type AbstractStatement <: AbstractLossyNode end

"""Specify the kinds of nodes to drop from the lossless source tree to create the AST."""
function is_kept_kind(node::LosslessTrees.LosslessLeafNode)
    k = SyntaxKinds.kind(node)
    return (
        !SyntaxKinds.is_whitespace(k)
        && !SyntaxKinds.is_syntax_keyword(k)
        && k ∉ KSet"( ) { } ; = , ."
    )
end
is_kept_kind(node::LosslessTrees.LosslessInnerNode) = true

#-------------------------------------------------------------------------------
# Leaf values (literals and identifiers).

# Define a few marker types to differnetiate Super and This from Identifiers etc.
struct ThisValue end
struct SuperValue end

struct LeafValue{T <: Union{String,Float64,Nothing,Bool,Symbol,ThisValue,SuperValue}} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessLeafNode
    value::T

    # A convenience constructor is our primary conversion mechanism.
    function LeafValue(lossless_node::LosslessTrees.LosslessLeafNode)
        k = SyntaxKinds.kind(lossless_node)
        text = Tokenize.text(lossless_node)
        if k == K"String"
            return new{String}(lossless_node, text)
        elseif k == K"Number"
            return new{Float64}(lossless_node, parse(Float64, text))
        elseif k ∈ KSet"nil omitted_var_initializer"
            return new{Nothing}(lossless_node, nothing)
        elseif k ∈ KSet"false true omitted_for_condition"
            return new{Bool}(lossless_node, parse(Bool, text))
        elseif k == K"Identifier"
            return new{Symbol}(lossless_node, Symbol(text))
        elseif k == K"this"
            return new{ThisValue}(lossless_node, ThisValue())
        elseif k == K"super"
            return new{SuperValue}(lossless_node, SuperValue())
        end
        error("Syntax kind $(k) is not a leaf value")
    end
end


# Aliases for better readability.
# Sadly we cannot instantiate using these, though, since we added a non-generic custom constructor.
StringLiteral = LeafValue{String}
NumberLiteral = LeafValue{Float64}
BoolLiteral = LeafValue{Bool}
NilLiteral = LeafValue{Nothing}
Identifier = LeafValue{Symbol}
This = LeafValue{ThisValue}
Super = LeafValue{SuperValue}

value(l::LeafValue) = l.value
value(::This) = :this
value(::Super) = :super

#-------------------------------------------------------------------------------
# Operators.

struct Operator{O} <: AbstractLossyNode
    lossless_node::LosslessTrees.LosslessLeafNode

    function Operator(lossless_node::LosslessTrees.LosslessLeafNode)
        k = SyntaxKinds.kind(lossless_node)
        @assert k ∈ KSet"* / + - ! < <= > >= == != or and"
        return new{Symbol(k)}(lossless_node)
    end
end

#-------------------------------------------------------------------------------
# Inner node expressions.

struct ThisExpression <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    name::This
end

struct SuperExpression <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Super
    method_name::Identifier
end

struct Variable <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
end

struct Assign{V<:AbstractExpression} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
    value::V
end

struct Unary{R<:AbstractExpression} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    operator::Operator
    right::R
end

struct Infix{L<:AbstractExpression,R<:AbstractExpression} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    operator::Operator
    left::L
    right::R
end

struct Logical{L<:AbstractExpression,R<:AbstractExpression} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    operator::Operator
    left::L
    right::R
end

struct Grouping{E<:AbstractExpression} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    expression::E
end

struct Call{C<:AbstractExpression} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    callee::C
    arguments::Vector{<:AbstractExpression}
end

struct Get{O<:AbstractExpression} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    object::O
    name::Identifier
end

struct Set{O<:AbstractExpression,V<:AbstractExpression} <: AbstractExpression
    lossless_node::LosslessTrees.LosslessInnerNode
    object::O
    name::Identifier
    value::V
end

function to_expression(lossless_node::LosslessTrees.LosslessNode)
    k = SyntaxKinds.kind(lossless_node)
    @assert SyntaxKinds.is_expression(k) "$k should be an expression kind"

    # Case 1: Leaf node (literal expression or implied missing expression).
    if SyntaxKinds.is_literal(k) || k ∈ KSet"omitted_var_initializer omitted_for_condition"
        return LeafValue(lossless_node)
    elseif k ∈ KSet"omitted_for_incrementer omitted_return_value"
        return nothing
    end

    # Case 2: Inner node (more complicated expression with children).
    children = filter(is_kept_kind, LosslessTrees.children(lossless_node))
    if k == K"unary"
        @assert length(children) == 2
        operator, right = children
        return Unary(lossless_node, Operator(operator), to_expression(right))
    elseif k == K"infix_operation"
        @assert length(children) == 3
        left, operator, right = children
        return Infix(lossless_node, Operator(operator), to_expression(left), to_expression(right))
    elseif k == K"logical"
        @assert length(children) == 3
        left, operator, right = children
        return Logical(lossless_node, Operator(operator), to_expression(left), to_expression(right))
    elseif k == K"grouping"
        return Grouping(lossless_node, to_expression(only(children)))
    elseif k == K"variable"
        return Variable(lossless_node, LeafValue(only(children)))
    elseif k == K"this_expression"
        @assert length(children) == 1
        @assert SyntaxKinds.kind(children[1]) == K"this"
        return ThisExpression(lossless_node, LeafValue(only(children)))
    elseif k == K"super_expression"
        @assert length(children) == 2
        name, method_name = children
        @assert SyntaxKinds.kind(name) == K"super"
        @assert SyntaxKinds.kind(method_name) == K"Identifier"
        return SuperExpression(lossless_node, LeafValue(name), LeafValue(method_name))
    elseif k == K"assignment"
        @assert length(children) == 2
        name, value = children
        @assert SyntaxKinds.kind(name) == K"variable"
        # NOTE: We unwrap the `Variable` to the child `Identifier` here, slightly flattening
        # the syntax tree for easier interpreting.
        return Assign(lossless_node, to_expression(name).name, to_expression(value))
    elseif k == K"call"
        callee, args... = to_expression.(children)
        return Call(lossless_node, callee, args)
    elseif k == K"accessor"
        @assert length(children) == 2
        object, name = children
        return Get(lossless_node, to_expression(object), LeafValue(name))
    elseif k == K"set"
        @assert length(children) == 2
        accessor, value = children
        @assert SyntaxKinds.kind(accessor) == K"accessor"
        # NOTE: We form and then unwrap the `Get` to the child `Identifier` here, slightly
        # flattening the syntax tree.
        accessor_lossless::Get = to_expression(accessor)
        return Set(lossless_node, accessor_lossless.object, accessor_lossless.name, to_expression(value))
    elseif k == K"inheritance"
        @assert length(children) == 2
        lt, superclass = children
        @assert SyntaxKinds.kind(lt) == K"<"
        @assert SyntaxKinds.kind(superclass) == K"variable"
        # Flatten we flatten out inheritance to just a variable.
        return to_expression(superclass)
    end

    # Unreachable.
    return nothing
end

#-------------------------------------------------------------------------------
# Statements.

struct Block <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    statements::Vector{<:AbstractStatement}
end

struct ExpressionStatement{E<:AbstractExpression} <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    expression::E
end

struct Print{E<:AbstractExpression} <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    expression::E
end

struct VariableDeclaration{E<:AbstractExpression} <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
    initializer::E
end

struct FunctionDeclaration <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
    parameters::Vector{Identifier}
    body::Block
end

struct ClassDeclaration <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
    superclass::Union{Nothing,Variable}
    methods::Vector{FunctionDeclaration}
end

struct If{C<:AbstractExpression,T<:AbstractStatement,E<:Union{Nothing,AbstractStatement}} <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    condition::C
    then_statement::T
    else_statement::E
end

struct While{C<:AbstractExpression,S<:Union{Nothing,AbstractStatement}} <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    condition::C
    statement::S
end

struct ReturnStatement{E<:Union{Nothing,AbstractExpression}} <: AbstractStatement
    lossless_node::LosslessTrees.LosslessInnerNode
    return_value::E
end

function to_statement(lossless_node::LosslessTrees.LosslessNode)
    k = SyntaxKinds.kind(lossless_node)
    @assert SyntaxKinds.is_statement(k)

    # Case 1: Leaf node omitted statements.
    k ∈ KSet"omitted_else omitted_for_initializer" && return nothing

    # Case 2: Inner node "real" statements.
    children = filter(is_kept_kind, LosslessTrees.children(lossless_node))
    if k == K"block"
        return Block(lossless_node, AbstractStatement[to_statement(c) for c in children])
    elseif k == K"expression_statement"
        return ExpressionStatement(lossless_node, to_expression(only(children)))
    elseif k == K"return_statement"
        return ReturnStatement(lossless_node, to_expression(only(children)))
    elseif k == K"print_statement"
        return Print(lossless_node, to_expression(only(children)))
    elseif k == K"var_decl_statement"
        @assert length(children) == 2
        name, initializer = children
        name, initializer = LeafValue(name), to_expression(initializer)
        return VariableDeclaration(lossless_node, name, initializer)
    elseif k ∈ KSet"fun_decl_statement method_decl_statement"
        name = children[1]
        parameters = children[2:end-1]
        body = children[end]
        @assert SyntaxKinds.kind(body) == K"block" "Expected block but got $(SyntaxKinds.kind(body))"
        name = LeafValue(name)
        parameters = LeafValue.(parameters)
        body = to_statement(body)
        return FunctionDeclaration(lossless_node, name, parameters, body)
    elseif k == K"class_decl_statement"
        name = children[1]
        if length(children) >= 2 && SyntaxKinds.kind(children[2]) == K"inheritance"
            superclass = to_expression(children[2])
            methods = children[3:end]
        else
            superclass = nothing
            methods = children[2:end]
        end
        @assert all([SyntaxKinds.kind(m) == K"method_decl_statement" for m in methods]) "$(methods)"
        name = LeafValue(name)
        methods = AbstractStatement[to_statement(m) for m in methods]
        return ClassDeclaration(lossless_node, name, superclass, methods)
    elseif k == K"if_statement"
        @assert length(children) == 3
        condition, then_statement, else_statement = children
        condition = to_expression(condition)
        then_statement = to_statement(then_statement)
        else_statement = to_statement(else_statement)
        return If(lossless_node, condition, then_statement, else_statement)
    elseif k == K"while_statement"
        @assert length(children) == 2
        condition, statement = children
        return While(lossless_node, to_expression(condition), to_statement(statement))
    elseif k == K"for_statement"
        return desugar_for_to_while(lossless_node, children)
    end

    # Unreachable.
    return nothing
end


function desugar_for_to_while(
    lossless_node::LosslessTrees.LosslessInnerNode,
    children::Vector{<:Union{LosslessTrees.LosslessInnerNode,LosslessTrees.LosslessLeafNode}}
)
    # TODO: Figure out how to point the new desugared nodes to appropriate lossless nodes.
    @assert SyntaxKinds.kind(lossless_node) == K"for_statement"
    @assert length(children) == 4

    # Unpack the children and convert them to lossless node objects.
    initializer, condition, incrementer, body = children
    initializer = to_statement(initializer)
    condition = to_expression(condition)
    incrementer = to_expression(incrementer)
    body = to_statement(body)

    # Combine the incrementer and looped body statement into a block.
    if !isnothing(incrementer)
        # Promote the expression to an expression statement.
        incrementer_statement = ExpressionStatement(incrementer.lossless_node, incrementer)
        body = Block(lossless_node, AbstractStatement[body, incrementer_statement])
    end

    # Create a while loop with the condition and body.
    for_loop = While(lossless_node, condition, body)

    # Combine the initializer and while loop into a block.
    if !isnothing(initializer)
        for_loop = Block(lossless_node, AbstractStatement[initializer, for_loop])
    end

    return for_loop
end

#-------------------------------------------------------------------------------
# Toplevel and building from a lossless tree.

struct Toplevel <: AbstractLossyNode
    lossless_node::LosslessTrees.LosslessInnerNode
    statements::Vector{<:AbstractStatement}
end

function to_lossy(toplevel_node::LosslessTrees.LosslessInnerNode)
    @assert SyntaxKinds.kind(toplevel_node) == K"toplevel"
    children = filter(is_kept_kind, LosslessTrees.children(toplevel_node))
    return Toplevel(toplevel_node, AbstractStatement[to_statement(c) for c in children])
end

#-------------------------------------------------------------------------------
# Shared functionality and display.

lossless_node(node::AbstractLossyNode) = node.lossless_node
Base.position(node::AbstractLossyNode) = JuLox.startbyte(lossless_node(node))
SyntaxKinds.kind(node::AbstractLossyNode) = SyntaxKinds.kind(lossless_node(node))
function range(node::AbstractLossyNode)
    # TODO: Drop whitespace and other trivia when determining position.
    lnode = lossless_node(node)
    startbyte = JuLox.startbyte(lnode)
    endbyte = JuLox.endbyte(lnode)
    return startbyte, endbyte
end
function children(node::T) where {T<:AbstractLossyNode}
    res = []
    child_properties = [sym for sym in propertynames(node) if sym != :lossless_node]
    for child_sym in child_properties
        children = getproperty(node, child_sym)
        children = children isa AbstractVector ? children : [children]
        for child in children
            if child isa AbstractLossyNode
                push!(res, child)
            end
        end
    end
    return res
end

function _value_str(node::AbstractLossyNode)
    node isa NilLiteral && return "nil"
    node isa LeafValue && return repr(value(node))
    return SyntaxKinds.kind(lossless_node(node))
end

function _show_lossy_node(io, node::AbstractLossyNode, indent)
    val = _value_str(node)
    startbyte, endbyte = range(node)
    posstr = "$(lpad(startbyte, 6)):$(rpad(endbyte, 6)) │"
    is_leaf = node isa Union{LeafValue,Operator}
    nodestr = is_leaf ? val : "[$(val)]"
    treestr = string(indent, nodestr)
    println(io, posstr, treestr)
    if !is_leaf
        new_indent = indent * "  "
        for child in children(node)
            _show_lossy_node(io, child, new_indent)
        end
    end
end

Base.show(io::IO, node::AbstractLossyNode) = _show_lossy_node(io, node, "")

end  # module LossyTrees