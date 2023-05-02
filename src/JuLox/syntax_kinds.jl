"""Adapted from JuliaSyntax.jl on 2023.04.05"""

module SyntaxKinds

# Definition of Kind type - mapping from token string identifiers to
# enumeration values as used in @K_str
const _kind_names =
[
    "\""  # TODO: Reorder (maybe create delimeters section?)
    "Identifier"

    "BEGIN_TRIVIA"
        "None"         # Placeholder; never emitted by lexer
        "EndMarker"    # EOF
        "Comment"
        "Whitespace"
        "NewlineWs"    # newline-containing whitespace
    "END_TRIVIA"

    "BEGIN_ERRORS"
        # Tokenization errors
        "ErrorUnknownCharacter"
        "ErrorUnterminatedString"
        # Parsing errors
        "ErrorStatementMissingSemicolon"
        "ErrorBlockMissingClosingBrace"
        "ErrorGroupMissingClosingParenthesis"
        "ErrorInvalidAssigmentTarget"
        "ErrorExpectedExpression"
        "ErrorTextAfterParsing"
        "ErrorIfMissingOpenParenthesis"
        "ErrorIfMissingClosingParenthesis"
        "ErrorWhileMissingOpenParenthesis"
        "ErrorWhileMissingClosingParenthesis"
        "ErrorForMissingOpenParenthesis"
        "ErrorForMissingClosingParenthesis"
        "ErrorForHeaderMissingSemicolon"
        "ErrorCallArgsMissingClosingParenthesis"
        "ErrorExceedMaxArguments"
        "ErrorExceedMaxParameters"
        "ErrorInvalidMethodOrFunctionIdentifier"
        "ErrorDeclarationMissingOpenParenthesis"
        "ErrorDeclarationMissingOpenBraces"
        "ErrorInvalidParameterIdentifier"
        "ErrorParametersMissingClosingParenthesis"
        # Generic error
        "error"
    "END_ERRORS"

    "UnparsedErrorRecovery"

    "BEGIN_SINGLE_CHARACTER_TOKENS"
        "("
        ")"
        "{"
        "}"
        ","
        "."
        ";"
        "BEGIN_OPERATIONS"
            "+"
            "-"
            "/"
            "*"
        "END_OPERATIONS"
    "END_SINGLE_CHARACTER_TOKENS"

    "BEGIN_ONE_OR_TWO_CHARACTER_TOKENS"
        "!"
        "!="
        "="
        "=="
        ">"
        ">="
        "<"
        "<="
    "END_ONE_OR_TWO_CHARACTER_TOKENS"

    "BEGIN_LITERAL"
        "String"
        "Number"
    "END_LITERAL"

    "BEGIN_KEYWORDS"
        "and"
        "class"
        "else"
        "false"
        "fun"
        "for"
        "if"
        "nil"
        "or"
        "print"
        "return"
        "super"
        "this"
        "true"
        "var"
        "while"
    "END_KEYWORDS"

    "BEGIN_SYNTAX_KINDS"
        "BEGIN_EXPRESSIONS"
            "call"
            "assignment"
            "grouping"
            "infix_operation"
            "logical"
            "unary"
            "variable"
            "omitted_var_initializer"
            "omitted_for_condition"
            "omitted_for_incrementer"
            "omitted_return_value"
        "END_EXPRESSIONS"
        "BEGIN_STATEMENTS"
            "block"
            "print_statement"
            "expression_statement"
            "var_decl_statement"
            "fun_decl_statement"
            "if_statement"
            "while_statement"
            "for_statement"
            "return_statement"
            "omitted_else"
            "omitted_for_initializer"
        "END_STATEMENTS"
        "toplevel"
    "END_SYNTAX_KINDS"

    "TOMBSTONE" # Empty placeholder for kind to be filled later
]

"""
    K"name"
    Kind(namestr)

`Kind` is a type tag for specifying the type of tokens and interior nodes of
a syntax tree. Abstractly, this tag is used to define our own *sum types* for
syntax tree nodes. We do this explicitly outside the Julia type system because
(a) Julia doesn't have sum types and (b) we want concrete data structures which
are unityped from the Julia compiler's point of view, for efficiency.

Naming rules:
* Kinds which correspond to exactly one textural form are represented with that
  text. This includes keywords like K"for" and operators like K"*".
* Kinds which represent many textural forms have UpperCamelCase names. This
  includes kinds like K"Identifier" and K"Comment".
* Kinds which exist merely as delimiters are all uppercase
"""
primitive type Kind 16 end

# The implementation of Kind here is basically similar to @enum. However we use
# the K_str macro to self-name these kinds with their literal representation,
# rather than needing to invent a new name for each.

let kind_int_type = :UInt16, max_kind_int = length(_kind_names)-1

    @eval begin
        function Kind(x::Integer)
            if x < 0 || x > $max_kind_int
                throw(ArgumentError("Kind out of range: $x"))
            end
            return Base.bitcast(Kind, convert($kind_int_type, x))
        end

        Base.convert(::Type{String}, k::Kind) = _kind_names[1 + Base.bitcast($kind_int_type, k)]

        let kindstr_to_int = Dict(s=>i-1 for (i,s) in enumerate(_kind_names))
            function Base.convert(::Type{Kind}, s::AbstractString)
                i = get(kindstr_to_int, s) do
                    error("unknown Kind name $(repr(s))")
                end
                Kind(i)
            end
        end

        Base.string(x::Kind) = convert(String, x)
        Base.print(io::IO, x::Kind) = print(io, convert(String, x))

        Base.typemin(::Type{Kind}) = Kind(0)
        Base.typemax(::Type{Kind}) = Kind($max_kind_int)

        Base.:<(x::Kind, y::Kind) = reinterpret($kind_int_type, x) < reinterpret($kind_int_type, y)

        Base.instances(::Type{Kind}) = (Kind(i) for i in reinterpret($kind_int_type, typemin(Kind)):reinterpret($kind_int_type, typemax(Kind)))
    end
end

function Base.show(io::IO, k::Kind)
    print(io, "K\"$(convert(String, k))\"")
end

#-------------------------------------------------------------------------------

"""
    K"s"

The kind of a token or AST internal node with string "s".

For example
* K")" is the kind of the right parenthesis token
* K"block" is the kind of a block of code (eg, statements within a begin-end).
"""
macro K_str(s)
    convert(Kind, s)
end

"""
A set of kinds which can be used with the `in` operator.  For example

    k in KSet"+ - *"
"""
macro KSet_str(str)
    kinds = [convert(Kind, s) for s in split(str)]

    quote
        ($(kinds...),)
    end
end

"""
    kind(x)

Return the `Kind` of `x`.
"""
kind(k::Kind) = k

#-------------------------------------------------------------------------------
const _nonunique_kind_names = Set([
    K"Comment"
    K"Whitespace"
    K"NewlineWs"

    K"ErrorUnknownCharacter"
    K"ErrorUnterminatedString"

    K"Identifier"
    K"Number"
    K"String"
])

"""
Return the string representation of a token kind, or `nothing` if the kind
represents a class of tokens like K"Identifier".

When `unique=true` only return a string when the kind uniquely defines the
corresponding input token, otherwise return `nothing`.  When `unique=false`,
return the name of the kind.

TODO: Replace `untokenize()` with `Base.string()`?
"""
function untokenize(k::Kind; unique=true)
    if unique && k in _nonunique_kind_names
        return nothing
    else
        return convert(String, k)
    end
end

# Error kind => description
_token_error_descriptions = Dict{Kind, String}(
    K"ErrorUnknownCharacter" => "unknown character",
    K"ErrorUnterminatedString" => "unterminated string",
    K"error" => "unknown error token",
    K"ErrorStatementMissingSemicolon" => "expect `;` to end the statement",
    K"ErrorBlockMissingClosingBrace" => "expect closing `}`",
    K"ErrorGroupMissingClosingParenthesis" => "expect closing `)`",
    K"ErrorInvalidAssigmentTarget" => "invalid assignment target",
    K"ErrorExpectedExpression" => "expect expression",
    K"ErrorTextAfterParsing" => "unexpected text after parsing",
    K"ErrorIfMissingOpenParenthesis" => "expect '(' after if.",
    K"ErrorIfMissingClosingParenthesis" => "expect ')' after if condition.",
    K"ErrorWhileMissingOpenParenthesis" => "expect '(' after while.",
    K"ErrorWhileMissingClosingParenthesis" => "expect ')' after while condition.",
    K"ErrorForMissingOpenParenthesis" => "expect '(' after for.",
    K"ErrorForMissingClosingParenthesis" => "expect ')' after for clauses.",
    K"ErrorForHeaderMissingSemicolon" => "expect ';' after for loop condition",
    K"ErrorCallArgsMissingClosingParenthesis" => "expect ')' after call arguments",
    K"ErrorExceedMaxArguments" => "exceeded maximum argument count of 255",
    K"ErrorExceedMaxParameters" => "exceeded maximum parameter count of 255",
    K"ErrorInvalidMethodOrFunctionIdentifier" => "expect function/method name",
    K"ErrorDeclarationMissingOpenParenthesis" => "expect '(' after function/method name",
    K"ErrorDeclarationMissingOpenBraces" => "expect '{' before function/method body",
    K"ErrorInvalidParameterIdentifier" => "expect parameter name",
    K"ErrorParametersMissingClosingParenthesis" => "expect ')' after parameters.",
)

function error_description(error_kind::Kind)
    @assert is_error(error_kind)
    return _token_error_descriptions[error_kind]
end

#-------------------------------------------------------------------------------
# Predicates
is_error(k::Kind) = K"BEGIN_ERRORS" < k < K"END_ERRORS"
is_keyword(k::Kind) = K"BEGIN_KEYWORDS" < k < K"END_KEYWORDS"
is_literal(k::Kind) = K"BEGIN_LITERAL" < k < K"END_LITERAL" || k ∈ KSet"true false nil"
is_operation(k::Kind) = K"BEGIN_OPERATIONS" < k < K"END_OPERATIONS"
is_expression(k::Kind) = K"BEGIN_EXPRESSIONS" < k < K"END_EXPRESSIONS" || is_literal(k)
is_statement(k::Kind) = K"BEGIN_STATEMENTS" < k < K"END_STATEMENTS"


is_error(k) = is_error(kind(k))
is_keyword(k) = is_keyword(kind(k))
is_literal(k) = is_literal(kind(k))
is_operation(k) = is_operation(kind(k))
is_expression(k) = is_expression(kind(k))
is_statement(k) = is_statement(kind(k))


"""
Return true if `x` has whitespace, comment, or quote mark kind.
"""
function is_whitespace(x)
    k = kind(x)
    return k == K"Whitespace" || k == K"NewlineWs" || k == K"Comment" || k == K"\""
end

end  # module SyntaxKinds
