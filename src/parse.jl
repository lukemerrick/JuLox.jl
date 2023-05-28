# A sequence-based interface for parsing Lox tokens.
#
# Rather than directly constructing a sytax tree, this Lox parser
# produces syntax "events" that enable later lossless tree creation.
# (or lossy tree creation, if trivia tokens are filtered!)

module Parse
using JuLox: JuLox, SyntaxKinds, Tokenize
using JuLox.SyntaxKinds: @K_str, @KSet_str

# Since this file is long by itself, let's put the `Parser` and its core functionality in another file.
include("parser_interface.jl")

#-------------------------------------------------------------------------------
# The actual logic for recursive descent parsing.

function consume(parser::Parser, expected::SyntaxKinds.Kind, error_kind::SyntaxKinds.Kind)
    if peek(parser) == expected
        bump(parser)
        return true
    else
        bump_error(parser, error_kind)
        recover(parser)
        return false
    end
end

"""Recover from invalid synax by trying to find the start of the next statement."""
function recover(parser::Parser)
    suppress_errors!(parser)
    mark = position(parser)
    # We have to bump at least once to avoid getting stuck.
    peek(parser) != K"EndMarker" && bump(parser)
    while (k = peek(parser)) != K"EndMarker"

        # If we hit a semicolon, we probably start a statement just after.
        if k == K";"
            bump(parser)
            break
        end

        # If we hit one of these keywords, we probably start a statement with it.
        if k ∈ KSet"class fun var for if while print return"
            break
        end

        # Keep bumping and dumping, otherwise!
        bump(parser)
    end
    # Label dumped tokens with a special recovery event.
    if position(parser).token_index > mark.token_index
        emit(parser, mark, K"UnparsedErrorRecovery")
    end
    return nothing
end

function parse_toplevel(parser::Parser)
    mark = position(parser)
    while peek(parser) != K"EndMarker"
        # Turn off error panic mode between each statement.
        unsuppress_errors!(parser)
        parse_declaration(parser)
    end
    emit(parser, mark, K"toplevel")
end

function parse_declaration(parser::Parser)
    k = peek(parser)
    if k == K"var"
        parse_var_declaration(parser)
    elseif k == K"fun"
        parse_function_declaration(parser, K"fun_decl_statement")
    elseif k == K"class"
        parse_class_declaration(parser)
    else
        parse_statement(parser)
    end
end

function parse_var_declaration(parser::Parser)
    mark = position(parser)
    bump(parser)  # K"var" token
    if consume(parser, K"Identifier", K"ErrorInvalidAssigmentTarget")

        # Handle initializer.
        if peek(parser) == K"="
            bump(parser)
            parse_expression(parser)
        else
            # Bump a special zero-width token as a placeholder if there is no initializer.
            bump_implied(parser, K"omitted_var_initializer")
        end

        # Handle final semicolon.
        consume(parser, K";", K"ErrorStatementMissingSemicolon")
    end
    emit(parser, mark, K"var_decl_statement")
end

function parse_function_declaration(parser::Parser, kind::SyntaxKinds.Kind)
    @assert kind ∈ KSet"fun_decl_statement method_decl_statement"
    mark = position(parser)
    kind == K"fun_decl_statement" && consume(parser, K"fun", K"ErrorExpectedFun")

    # Parse the identifier.
    if consume(parser, K"Identifier", K"ErrorInvalidIdentifier")

        # Consume the open parenthesis.
        consume(parser, K"(", K"ErrorDeclarationMissingOpenParenthesis")

        # Consume the argument list.
        n_args = 0
        if peek(parser) != K")"
            while true  # Do-while loop.
                consume(parser, K"Identifier", K"ErrorInvalidIdentifier")
                n_args += 1
                peek(parser) != K"," && break
                bump(parser)  # Take the comma.
            end
        end

        # Consume the closing parenthesis.
        consume(parser, K")", K"ErrorParametersMissingClosingParenthesis")

        # Emit an error event if we exceeded the maximum parameter count.
        n_args >= 255 && emit(parser, mark, K"ErrorExceedMaxArguments")

        # Handle definition block.
        if peek(parser) != K"{"
            bump_error(parser, K"ErrorDeclarationMissingOpenBraces")
            recover(parser)
        else
            parse_block(parser)
        end
    end
    emit(parser, mark, kind)
end

function parse_class_declaration(parser::Parser)
    mark = position(parser)
    bump(parser)  # K"class" token

    # Parse the identifier.
    if consume(parser, K"Identifier", K"ErrorInvalidIdentifier")
        # Parse subclassing by matching on a '<'.
        if peek(parser) == K"<"
            inheritance_mark = position(parser)
            bump(parser)  # Bump the '<'.
            superclass_name_mark = position(parser)
            consume(parser, K"Identifier", K"ErrorMissingSuperclassName")
            emit(parser, superclass_name_mark, K"variable")
            emit(parser, inheritance_mark, K"inheritance")
        end

        # Parse Parse the body
        if consume(parser, K"{", K"ErrorDeclarationMissingOpenBraces")
            while peek(parser) ∉ KSet"} EndMarker"
                parse_function_declaration(parser, K"method_decl_statement")
            end
            consume(parser, K"}", K"ErrorDeclarationMissingClosingBraces")
        end
    end
    emit(parser, mark, K"class_decl_statement")
end

function parse_statement(parser::Parser)
    k = peek(parser)
    if k == K"{"
        parse_block(parser)
    elseif k == K"print"
        parse_print_statement(parser)
    elseif k == K"if"
        parse_if_statement(parser)
    elseif k == K"while"
        parse_while_statement(parser)
    elseif k == K"for"
        parse_for_statement(parser)
    elseif k == K"return"
        parse_return_statement(parser)
    else
        parse_expression_statement(parser)
    end
end

function parse_block(parser::Parser)
    mark = position(parser)

    # Handle the "{"
    @assert SyntaxKinds.kind(peek(parser)) == K"{"
    bump(parser)

    # Parse a list of statements.
    while peek(parser) ∉ KSet"} EndMarker"
        parse_declaration(parser)
    end

    # Handle the "}"
    consume(parser, K"}", K"ErrorBlockMissingClosingBrace")
    emit(parser, mark, K"block")
end

function parse_print_statement(parser::Parser)
    mark = position(parser)

    # Handle the "print".
    @assert SyntaxKinds.kind(peek(parser)) == K"print"
    bump(parser)

    # Parse the expression.
    parse_expression(parser)
    consume(parser, K";", K"ErrorStatementMissingSemicolon")
    emit(parser, mark, K"print_statement")
end

function parse_expression_statement(parser::Parser)
    mark = position(parser)
    parse_expression(parser)
    consume(parser, K";", K"ErrorStatementMissingSemicolon")
    emit(parser, mark, K"expression_statement")
end

function parse_if_statement(parser::Parser)
    mark = position(parser)

    # Handle the "if".
    @assert SyntaxKinds.kind(peek(parser)) == K"if"
    bump(parser)

    # Parse the if conditional.
    consume(parser, K"(", K"ErrorIfMissingOpenParenthesis")
    parse_expression(parser)
    consume(parser, K")", K"ErrorIfMissingClosingParenthesis")

    # Parse then statement.
    parse_statement(parser)

    # Possibly parse else statement.
    if peek(parser) == K"else"
        bump(parser)
        parse_statement(parser)
    else
        # Bump a special zero-width token as a placeholder if there is no else.
        bump_implied(parser, K"omitted_else")
    end

    # Emit the if statement event.
    emit(parser, mark, K"if_statement")
end

function parse_while_statement(parser::Parser)
    mark = position(parser)

    # Handle the "while".
    @assert SyntaxKinds.kind(peek(parser)) == K"while"
    bump(parser)

    # Parse the continuation conditional.
    consume(parser, K"(", K"ErrorWhileMissingOpenParenthesis")
    parse_expression(parser)
    consume(parser, K")", K"ErrorWhileMissingClosingParenthesis")

    # Parse then statement.
    parse_statement(parser)

    # Emit the while statement event.
    emit(parser, mark, K"while_statement")
end

function parse_for_statement(parser::Parser)
    mark = position(parser)

    # Handle the "for".
    @assert SyntaxKinds.kind(peek(parser)) == K"for"
    bump(parser)

    # Start parsing the header parenthetical.
    consume(parser, K"(", K"ErrorForMissingOpenParenthesis")

    # Item 1 of 3: Initializer.
    k = peek(parser)
    if k == K";"
        # No initializer to parse. Bump the semicolon.
        bump_implied(parser, K"omitted_for_initializer")
        bump(parser)
    elseif k == K"var"
        # Var declaration initializer.
        parse_var_declaration(parser)
    else
        # Expression initializer.
        parse_expression_statement(parser)
    end

    # Item 2 of 3: Condition expression.
    if peek(parser) != K";"
        parse_expression(parser)
    else
        bump_implied(parser, K"omitted_for_condition")
    end
    # Consume semicolon.
    consume(parser, K";", K"ErrorForHeaderMissingSemicolon")

    # Item 3 of 3: Incrementer.
    if peek(parser) != K")"
        parse_expression(parser)
    else
        bump_implied(parser, K"omitted_for_incrementer")
    end

    # End parsing the header parenthetical.
    consume(parser, K")", K"ErrorForMissingClosingParenthesis")

    # Parse then statement.
    parse_statement(parser)

    # Emit the while statement event.
    emit(parser, mark, K"for_statement")
end

function parse_return_statement(parser::Parser)
    mark = position(parser)

    # Consume "return".
    bump(parser)

    # Check for a return value.
    if peek(parser) != K";"
        parse_expression(parser)
    else
        bump_implied(parser, K"omitted_return_value")
    end

    # Consume semicolon.
    consume(parser, K";", K"ErrorStatementMissingSemicolon")

    # Emite the return statement event.
    emit(parser, mark, K"return_statement")
end

function parse_expression(parser::Parser)
    parse_assignment(parser)
    return nothing
end

function parse_assignment(parser::Parser)
    mark = position(parser)

    # Parse left side of assignment (or only side of non-assignment!)
    parse_or(parser)

    # Check for a right side.
    if peek(parser) == K"="
        left_last_token_index = position(parser).token_index

        # Bump the equals sign.
        bump(parser)

        # Parse the right side expression.
        parse_expression(parser)

        # Look back to left side of assignment to ensure it's an identifier or instance property.
        left_side_tokens = [
            t for t in parser._parsed_tokens[mark.token_index+1:left_last_token_index]
            if !SyntaxKinds.is_whitespace(SyntaxKinds.kind(t))
        ]
        if length(left_side_tokens) == 1 && SyntaxKinds.kind(only(left_side_tokens)) == K"Identifier"
            # This is an assigment. Emit an event for it.
            emit(parser, mark, K"assignment")
        elseif (
            length(left_side_tokens) > 2
            && SyntaxKinds.kind(left_side_tokens[end-1]) == K"."
            && SyntaxKinds.kind(left_side_tokens[end]) == K"Identifier"
        )
            # This is a set operation on an instance property. Emit an event for it.
            emit(parser, mark, K"set")
        else
            emit(parser, mark, K"ErrorInvalidAssigmentTarget")
        end
    end
end

function parse_infix_syntax(parser::Parser, op_kset::Tuple{Vararg{SyntaxKinds.Kind}}, left_right_parse_fn::Function, syntax_type::SyntaxKinds.Kind)
    mark = position(parser)

    # Parse the left operand.
    left_right_parse_fn(parser)

    # Parse zero or more operators matching `op_kset``.
    # The entire expression built up becomes the left operand of the next operation parsed.
    while (k = peek(parser)) ∈ op_kset
        # Consume the operator.
        bump(parser)

        # Parse the right operatnd.
        left_right_parse_fn(parser)

        # Emit the operand covering from start of left through end of right.
        emit(parser, mark, syntax_type)
    end
    return nothing
end

function parse_infix_operator(parser::Parser, op_kset::Tuple{Vararg{SyntaxKinds.Kind}}, left_right_parse_fn::Function)
    return parse_infix_syntax(parser, op_kset, left_right_parse_fn, K"infix_operation")
end

function parse_logical_operator(parser::Parser, op_kset::Tuple{Vararg{SyntaxKinds.Kind}}, left_right_parse_fn::Function)
    return parse_infix_syntax(parser, op_kset, left_right_parse_fn, K"logical")
end

parse_or(parser::Parser) = parse_logical_operator(parser, KSet"or", parse_and)
parse_and(parser::Parser) = parse_logical_operator(parser, KSet"and", parse_equality)
parse_equality(parser::Parser) = parse_infix_operator(parser, KSet"!= ==", parse_comparison)
parse_comparison(parser::Parser) = parse_infix_operator(parser, KSet"> >= < <=", parse_term)
parse_term(parser::Parser) = parse_infix_operator(parser, KSet"+ -", parse_factor)
parse_factor(parser::Parser) = parse_infix_operator(parser, KSet"* /", parse_unary)

function parse_unary(parser::Parser)
    if peek(parser) ∈ KSet"! -"
        mark = position(parser)
        bump(parser)
        parse_unary(parser)
        emit(parser, mark, K"unary")
    else
        parse_call(parser)
    end
end

function parse_call(parser::Parser)
    # Track the start of the leftmost call.
    mark = position(parser)

    # Start with the primary, which could evaluate to a function.
    parse_primary(parser)

    # Parse zero or more parenthetical expressions representing gets or calls.
    while true
        k = peek(parser)
        if k == K"("  # Case 1: Call.
            # Consume the open parenthesis.
            bump(parser)

            # Consume the argument list.
            n_args = 0
            if peek(parser) != K")"
                while true  # Do-while loop.
                    parse_expression(parser)
                    n_args += 1
                    peek(parser) != K"," && break
                    bump(parser)  # Take the comma.
                end
            end

            # Consume the closing parenthesis.
            consume(parser, K")", K"ErrorCallArgsMissingClosingParenthesis")

            # Emit a call event covering from start of left through end of right.
            emit(parser, mark, K"call")

            # Emit an error event if we exceeded the maximum argument count.
            n_args >= 255 && emit(parser, mark, K"ErrorExceedMaxArguments")
        elseif k == K"."  # Case 2: Get or possibly set. We call this "accessor".
            # Consume the dot.
            bump(parser)

            # Consume the property identifier.
            consume(parser, K"Identifier", K"ErrorMissingPropertyName")

            # Emit a get event covering from start of left through end of right.
            emit(parser, mark, K"accessor")
        else
            break
        end
    end
end

function parse_primary(parser::Parser)
    SyntaxKinds.is_error(peek(parser)) && bump(parser)  # Pass through errors.
    mark = position(parser)
    k = peek(parser)
    if k == K"this"
        bump(parser)
        emit(parser, mark, K"this_expression")
    elseif k == K"super"
        bump(parser)
        consume(parser, K".", K"ErrorSuperMissingDot")
        consume(parser, K"Identifier", K"ErrorSuperMissingMethod")
        emit(parser, mark, K"super_expression")
    elseif k == K"Identifier"
        bump(parser)
        emit(parser, mark, K"variable")
    elseif k ∈ KSet"false true nil Number String"
        bump(parser)
        # We don't actually have to emit a range for a single token item.
    elseif k == K"("
        # Consume the left parenthesis.
        bump(parser)

        # Handle parenthesized expression.
        parse_expression(parser)

        # Parse right parenthesis.
        consume(parser, K")", K"ErrorGroupMissingClosingParenthesis")
        emit(parser, mark, K"grouping")
    else
        # We got all the way down looking for some kind of expression and found nothing.
        bump_error(parser, K"ErrorExpectedExpression")
        recover(parser)
    end
end

#-------------------------------------------------------------------------------
# The (very small) public API.

struct ParseResult
    tokens::Vector{Tokenize.Token}
    events::Vector{Event}
end

function parse_lox(text::AbstractString)
    parser = Parser(text)
    parse_toplevel(parser)
    return ParseResult(tokens(parser), events(parser))
end

end  # end module Parse
