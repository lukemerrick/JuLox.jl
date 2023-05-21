module Entrypoint
using Fractal.JuLox: JuLox, SyntaxKinds, Tokenize, Parse, LosslessTrees, SyntaxValidation, LossyTrees, Resolver, Interpret
using Fractal.JuLox.SyntaxKinds: @K_str


"""Read the next line from `stdin`, or return `nothing` if we hit EOF."""
function _read_line_or_eof()::Union{String,Nothing}
    # NOTE: Possibly not the most performant way to do this, but it seems plenty fast.
    input = Char[]
    while true
        eof(stdin) && return nothing
        next_char = read(stdin, Char)
        if next_char == '\n'
            return String(input)
        end
        push!(input, next_char)
    end
end

function print_tokens(io::IO, tokens::Vector{Tokenize.Token})
    println(io, "Tokens")
    for t in tokens
        posstr = "$(lpad(JuLox.startbyte(t), 6)):$(rpad(JuLox.endbyte(t), 6)) │ " 
        line = rpad(string(posstr, string(SyntaxKinds.kind(t))), 43)
        line = line * ' ' * repr(Tokenize.text(t))
        println(io, line)
    end
end

function print_analysis_results(locals)
    println("Variable Resolution")
    println("Name           Position      Up")
    println("-------------------------------")
    for (key, value) in sort(collect(pairs(locals)); by=(p -> position(p[1])))
        println(
            rpad(":$(key.name.symbol)", 15)
            * lpad("$(position(key))", 8)
            * lpad(string(value), 8)
        )
    end
    println()
end

print_tokens(tokens::Vector{Tokenize.Token}) = print_tokens(stdout, tokens)

function run(interpreter_state::Interpret.InterpreterState, source::String, verbose::Bool)
    result = Parse.parse_lox(source)
    tree = LosslessTrees.build_tree(result)

    if !isempty(result.tokens)
        if verbose
            # Print tokens.
            print_tokens(result.tokens)
            println()

            # Print events.
            println("Events")
            for event in result.events
                println(event)
            end

            println()

            # Print lossless tree.
            println("Lossless Syntax Tree")
            println(tree)
        end

        # Validate the syntax.
        diagnostics = SyntaxValidation.validate_syntax(tree)
        if !isempty(diagnostics)
            SyntaxValidation.show_diagnostics(stdout, diagnostics, source)
            return 1
        end

        # Craft the lossy tree.
        lossy_tree = LossyTrees.to_lossy(tree)

        # Print the lossy tree.
        if verbose
            println("Lossy Syntax Tree")
            println(lossy_tree)
        end

        # Resolve variables (resolving the semantics).
        locals, diagnostics = Resolver.resolve_scopes(lossy_tree)
        if !isempty(diagnostics)
            SyntaxValidation.show_diagnostics(stdout, diagnostics, source)
            return 1
        end
        
        # Print analysis results.
        if verbose
            print_analysis_results(locals)
        end
        
        # Interpret.
        if verbose
            println("Interpreter")
            println("-----------")
        end
        Interpret.update_local_scope_map!(interpreter_state, locals)
        had_error = Interpret.interpret(interpreter_state, lossy_tree, source)
        exit_code = had_error ? 70 : 0
        return exit_code
    end

    return 0
end


"""Create a super lightweight REPL experience."""
function run_prompt(verbose::Bool)::Integer
    # NOTE: Different from Julia's source code, which can be found here:
    # https://github.com/JuliaLang/julia/blob/826674cf7d21ff5940ecc4dd6c06103cccbed392/stdlib/REPL/src/REPL.jl#L395
    # One reason for differences: Julia handles multi-line expressions, while Lox doesn't!

    exit_code = 0

    # Allow handling ^C interrupts.
    Base.exit_on_sigint(false)

    # Print welcome message.
    print("Hello! Welcome to ")
    printstyled("JuLox!"; bold=true, color=:green)
    print(
        "\n" *
        "To exit the interactive session, type CTRL-D." *
        "\n\n"
    )

    # Initialize the interpreter state.
    interpreter_state = Interpret.initialize_interpreter()

    # Loop until CTRL-D (EOF) signal.
    while true
        Base.reseteof(stdin)
        printstyled("JuLox> "; bold=true, color=:green)
        line = ""
        try
            input = _read_line_or_eof()
            is_eof = isnothing(input)
            if is_eof
                print("\nGoodbye!\n")
                exit_code = 0
                return exit_code
            else
                line = input
            end
        catch e
            if isa(e, InterruptException)
                println()
                println()
                continue
            else
                rethrow()
            end
        end
        if line != "\n"
            run(interpreter_state, line, verbose)
        end
        println()  # Add an extra newline after the result.
    end
    return exit_code
end

function run_file(filepath::String, verbose::Bool)::Integer
    return run(Interpret.initialize_interpreter(), read(filepath, String), verbose)
end
end  # module
