# Provides a typed API for syntax trees.

module LossyTrees

using Fractal.JuLox: JuLox, SyntaxKinds, Parse, LosslessTrees, Tokenize
using Fractal.JuLox.SyntaxKinds: @K_str, @KSet_str

abstract type LossyNode end
abstract type Expression <: LossyNode end
abstract type Statement <: LossyNode end

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
# Literals (leaf node expressions).

abstract type Literal <: Expression end

struct StringLiteral <: Literal
    lossless_node::LosslessTrees.LosslessLeafNode
    value::String
end

struct NumberLiteral <: Literal
    lossless_node::LosslessTrees.LosslessLeafNode
    value::Float64
end

struct BoolLiteral <: Literal
    lossless_node::LosslessTrees.LosslessLeafNode
    value::Bool
end

struct NilLiteral <: Literal
    lossless_node::LosslessTrees.LosslessLeafNode
    value::Nothing

    NilLiteral(lossless_node::LosslessTrees.LosslessLeafNode) = new(lossless_node, nothing)
end

function to_literal(lossless_node::LosslessTrees.LosslessLeafNode)
    k = SyntaxKinds.kind(lossless_node)
    text = Tokenize.text(lossless_node)
    k == K"String" && return StringLiteral(lossless_node, text)
    k == K"Number" && return NumberLiteral(lossless_node, parse(Float64, text))
    k == K"nil" && return NilLiteral(lossless_node)
    k ∈ KSet"true false" && return BoolLiteral(lossless_node, parse(Bool, text))
    error("Must only call `to_literal` on a literal kind node")
end

value(l::Literal) = l.value

#-------------------------------------------------------------------------------
# Operators.

abstract type Operator <: LossyNode end
abstract type LogicOperator <: LossyNode end


struct OperatorMultiply <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorDivide <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorPlus <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorMinus <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorBang <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorLess <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorLessEqual <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorMore <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorMoreEqual <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorEqual <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorNotEqual <: Operator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorOr <: LogicOperator
    lossless_node::LosslessTrees.LosslessLeafNode
end

struct OperatorAnd <: LogicOperator
    lossless_node::LosslessTrees.LosslessLeafNode
end

function to_operator(lossless_node::LosslessTrees.LosslessLeafNode)
    k = SyntaxKinds.kind(lossless_node)
    k == K"*" && return OperatorMultiply(lossless_node)
    k == K"/" && return OperatorDivide(lossless_node)
    k == K"+" && return OperatorPlus(lossless_node)
    k == K"-" && return OperatorMinus(lossless_node)
    k == K"!" && return OperatorBang(lossless_node)
    k == K"<" && return OperatorLess(lossless_node)
    k == K"<=" && return OperatorLessEqual(lossless_node)
    k == K">" && return OperatorMore(lossless_node)
    k == K">=" && return OperatorMoreEqual(lossless_node)
    k == K"==" && return OperatorEqual(lossless_node)
    k == K"!=" && return OperatorNotEqual(lossless_node)
    k == K"or" && return OperatorOr(lossless_node)
    k == K"and" && return OperatorAnd(lossless_node)
    error("Must only call `to_operator` on an operator kind node")
end

#-------------------------------------------------------------------------------
# Identifier.
# TODO: Treat this like a literal?

struct Identifier <: Expression
    lossless_node::LosslessTrees.LosslessLeafNode
    symbol::Symbol
end

function to_identifier(lossless_node::LosslessTrees.LosslessLeafNode)
    k = SyntaxKinds.kind(lossless_node)
    text = Tokenize.text(lossless_node)
    @assert k == K"Identifier" "Expected identifier but got $(k)"
    return Identifier(lossless_node, Symbol(text))
end

#-------------------------------------------------------------------------------
# Inner node expressions.

abstract type Callable <: Expression end

struct Variable <: Expression
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
end

struct Assign{V<:Expression} <: Expression
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
    value::V
end

struct Unary{O<:Operator,R<:Expression} <: Expression
    lossless_node::LosslessTrees.LosslessInnerNode
    operator::O
    right::R
end

struct Infix{O<:Operator,L<:Expression,R<:Expression} <: Expression
    lossless_node::LosslessTrees.LosslessInnerNode
    operator::O
    left::L
    right::R
end

struct Logical{O<:LogicOperator,L<:Expression,R<:Expression} <: Expression
    lossless_node::LosslessTrees.LosslessInnerNode
    operator::O
    left::L
    right::R
end

struct Grouping{E<:Expression} <: Expression
    lossless_node::LosslessTrees.LosslessInnerNode
    expression::E
end

struct Call{C<:Expression} <: Expression
    lossless_node::LosslessTrees.LosslessInnerNode
    callee::C
    arguments::Vector{<:Expression}
end

function to_expression(lossless_node::LosslessTrees.LosslessNode)
    k = SyntaxKinds.kind(lossless_node)
    @assert SyntaxKinds.is_expression(k) "$k should be an expression kind"

    # Case 1: Leaf node (literal expression or implied missing expression).
    if SyntaxKinds.is_literal(k)
        return to_literal(lossless_node)
    elseif k ∈ KSet"omitted_var_initializer omitted_return_value"
        return NilLiteral(lossless_node)
    elseif k == K"omitted_for_condition"
        return BoolLiteral(lossless_node, true)
    elseif k == K"omitted_for_incrementer"
        return nothing
    end

    # Case 2: Inner node (more complicated expression with children).
    children = filter(is_kept_kind, LosslessTrees.children(lossless_node))
    if k == K"unary"
        @assert length(children) == 2
        operator, right = children
        return Unary(lossless_node, to_operator(operator), to_expression(right))
    elseif k == K"infix_operation"
        @assert length(children) == 3
        left, operator, right = children
        return Infix(lossless_node, to_operator(operator), to_expression(left), to_expression(right))
    elseif k == K"logical"
        @assert length(children) == 3
        left, operator, right = children
        return Logical(lossless_node, to_operator(operator), to_expression(left), to_expression(right))
    elseif k == K"grouping"
        @assert length(children) == 1
        return Grouping(lossless_node, to_expression(children[1]))
    elseif k == K"variable"
        @assert length(children) == 1
        return Variable(lossless_node, to_identifier(children[1]))
    elseif k == K"assignment"
        @assert length(children) == 2
        name, value = children
        @assert SyntaxKinds.kind(name) == K"variable"
        # NOTE: We unwrap the `Variable` to the child `Identifier` here, slightly flattening
        # the syntax tree.
        return Assign(lossless_node, to_expression(name).name, to_expression(value))
    elseif k == K"call"
        callee, args... = to_expression.(children)
        return Call(lossless_node, callee, args)
    end

    # Unreachable.
    return nothing
end

#-------------------------------------------------------------------------------
# Statements.

struct Block <: Statement
    lossless_node::LosslessTrees.LosslessInnerNode
    statements::Vector{<:Statement}
end

struct ExpressionStatement{E<:Expression} <: Statement
    lossless_node::LosslessTrees.LosslessInnerNode
    expression::E
end

struct Print{E<:Expression} <: Statement
    lossless_node::LosslessTrees.LosslessInnerNode
    expression::E
end

struct VariableDeclaration{E<:Expression} <: Statement
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
    initializer::E
end

struct FunctionDeclaration <: Statement
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
    parameters::Vector{Identifier}
    body::Block
end

struct ClassDeclaration <: Statement
    lossless_node::LosslessTrees.LosslessInnerNode
    name::Identifier
    methods::Vector{FunctionDeclaration}
end

struct If{C<:Expression,T<:Statement,E<:Union{Nothing,Statement}} <: Statement
    lossless_node::LosslessTrees.LosslessInnerNode
    condition::C
    then_statement::T
    else_statement::E
end

struct While{C<:Expression,S<:Union{Nothing,Statement}} <: Statement
    lossless_node::LosslessTrees.LosslessInnerNode
    condition::C
    statement::S
end

struct ReturnStatement{E<:Expression} <: Statement
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
        return Block(lossless_node, to_statement.(children))
    elseif k == K"expression_statement"
        @assert length(children) == 1
        return ExpressionStatement(lossless_node, to_expression(children[1]))
    elseif k == K"return_statement"
        @assert length(children) == 1
        return ReturnStatement(lossless_node, to_expression(children[1]))
    elseif k == K"print_statement"
        @assert length(children) == 1
        return Print(lossless_node, to_expression(children[1]))
    elseif k == K"var_decl_statement"
        @assert length(children) == 2
        name, initializer = children
        name, initializer = to_identifier(name), to_expression(initializer)
        return VariableDeclaration(lossless_node, name, initializer)
    elseif k ∈ KSet"fun_decl_statement method_decl_statement"
        name = children[1]
        parameters = children[2:end-1]
        body = children[end]
        @assert SyntaxKinds.kind(body) == K"block" "Expected block but got $(SyntaxKinds.kind(body))"
        name = to_identifier(name)
        parameters = to_identifier.(parameters)
        body = to_statement(body)
        return FunctionDeclaration(lossless_node, name, parameters, body)
    elseif k == K"class_decl_statement"
        name = children[1]
        methods = children[2:end]
        @assert all([SyntaxKinds.kind(m) == K"method_decl_statement" for m in methods])
        name = to_identifier(name)
        methods = to_statement.(methods)
        return ClassDeclaration(lossless_node, name, methods)
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
        body = Block(lossless_node, Statement[body, incrementer_statement])
    end

    # Create a while loop with the condition and body.
    for_loop = While(lossless_node, condition, body)

    # Combine the initializer and while loop into a block.
    if !isnothing(initializer)
        for_loop = Block(lossless_node, Statement[initializer, for_loop])
    end

    return for_loop
end

#-------------------------------------------------------------------------------
# Toplevel and building from a lossless tree.

struct Toplevel <: LossyNode
    lossless_node::LosslessTrees.LosslessInnerNode
    statements::Vector{<:Statement}
end

function to_lossy(toplevel_node::LosslessTrees.LosslessInnerNode)
    @assert SyntaxKinds.kind(toplevel_node) == K"toplevel"
    children = filter(is_kept_kind, LosslessTrees.children(toplevel_node))
    return Toplevel(toplevel_node, to_statement.(children))
end

#-------------------------------------------------------------------------------
# Shared functionality and display.

lossless_node(node::T) where {T<:LossyNode} = node.lossless_node
Base.position(node::T) where {T<:LossyNode} = JuLox.startbyte(lossless_node(node))
SyntaxKinds.kind(node::T) where {T<:LossyNode} = SyntaxKinds.kind(lossless_node(node))
function range(node::T) where {T<:LossyNode}
    # TODO: Drop whitespace and other trivia when determining position.
    lnode = lossless_node(node)
    startbyte = JuLox.startbyte(lnode)
    endbyte = JuLox.endbyte(lnode)
    return startbyte, endbyte
end
function children(node::T) where {T<:LossyNode}
    res = []
    child_properties = [sym for sym in propertynames(node) if sym != :lossless_node]
    for child_sym in child_properties
        children = getproperty(node, child_sym)
        children = children isa AbstractVector ? children : [children]
        for child in children
            if child isa LossyNode
                push!(res, child)
            end
        end
    end
    return res
end

function _value_str(node::LossyNode)
    node isa NilLiteral && return "nil"
    node isa Literal && return repr(node.value)
    node isa Identifier && return node.symbol
    return SyntaxKinds.kind(lossless_node(node))
end

function _show_lossy_node(io, node::LossyNode, indent)
    val = _value_str(node)
    startbyte, endbyte = range(node)
    posstr = "$(lpad(startbyte, 6)):$(rpad(endbyte, 6)) │"
    is_leaf = node isa Union{Literal,Operator,Identifier}
    nodestr = is_leaf ? val : "[$(val)]"
    treestr = string(indent, nodestr)
    println(io, posstr, treestr)
    if !is_leaf
        _show_lossy_node.(Ref(io), children(node), Ref(indent * "  "))
    end
end

Base.show(io::IO, node::LossyNode) = _show_lossy_node(io, node, "")

end  # module LossyTrees