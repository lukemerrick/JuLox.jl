module Entrypoint
using Fractal.JuLox: JuLox, SyntaxKinds, Tokenize, Parse, LosslessTrees, SyntaxValidation, LossyTrees, Interpret
using Fractal.JuLox.SyntaxKinds: @K_str

# TODO: Restrict typing of `result`.
struct RunResult
    result::Any
    run_error::Exception
end

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

function run(environment::Interpret.Environment, source::String)
    result = Parse.parse_lox(source)
    tree = LosslessTrees.build_tree(result)

    if !isempty(result.tokens)
        # Print tokens.
        println("Tokens")
        println("Location   Kind                     Text                ")
        println("--------------------------------------------------------")
        for t in result.tokens
            print(rpad(string(JuLox.startbyte(t), "-", JuLox.endbyte(t)), 11, " "))
            print(rpad(SyntaxKinds.kind(t), 35, " "))
            print(rpad("$(repr(Tokenize.text(t)))", 20, " "))
            println()
        end

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

        # Validate the syntax.
        diagnostics = SyntaxValidation.validate_syntax(tree)
        if !isempty(diagnostics)
            SyntaxValidation.show_diagnostics(stdout, diagnostics, source)
            return 1
        end

        # Craft the lossy tree.
        lossy_tree = LossyTrees.to_lossy(tree)
        println("Lossy Syntax Tree")
        println(lossy_tree)

        # Interpret.
        println("Interpreter")
        println("-----------")
        had_error = Interpret.interpret(environment, lossy_tree, source)
        exit_code = had_error ? 70 : 0
        return exit_code
    end

    return 0
end


"""Create a super lightweight REPL experience."""
function run_prompt()::Integer
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

    # # Initialize interpreter global environment.
    environment = Interpret.Environment()

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
            try
                run(environment, line)
            catch e
                # !isa(e, Parse.ParseError) && rethrow()
                # showerror(stdout, e)
                rethrow()
            end
        end
        println()  # Add an extra newline after the result.
    end
    return exit_code
end

function run_file(filepath::String)::Integer
    environment = Interpret.Environment()
    exit_code = run(environment, read(filepath, String))
    return exit_code
end
end  # module
