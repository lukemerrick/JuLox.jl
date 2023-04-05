module Entrypoint
using Fractal.JuLox.Tokenizer: tokenize
using Fractal.JuLox: SyntaxError

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
        push!(input, next_char)
        if next_char == '\n'
            return String(input)
        end
    end
end

# Pretty error printing when something in the users code is wrong.
function report_error(error::SyntaxError)
    error_name = error |> typeof |> string |> x -> split(x, '.')[end]
    println(
        "[line $(error.line) position $(error.position)] " * 
        "$(error_name): $(error.message)"
    )
end

function run(line::String)
    result = nothing
    error = nothing
    exit_code = 0
    try
        # For now, the result is the tokens.
        result = tokenize(line)
    catch e
        !isa(e, Union{SyntaxError}) && rethrow()
        error = e
    end
    if !isnothing(error)
        report_error(error)
        exit_code = 65
    elseif !isnothing(result)
        # For now, just print the tokens.
        collect(map(println, result))
    else
        println()
    end
    return exit_code
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
        line != "\n" && run(line)
        println()  # Add an extra newline after the result.
    end
    return exit_code
end

function run_file(filepath::String)::Integer
    exit_code = read(filepath, String) |> run
    return exit_code
end
end  # module
