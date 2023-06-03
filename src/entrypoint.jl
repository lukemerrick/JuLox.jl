module Entrypoint
using JuLox: JuLox, SyntaxKinds, Tokenize, Parse, LosslessTrees, SyntaxValidation, LossyTrees, Resolver, Interpret, Transpile
using JuLox.SyntaxKinds: @K_str

using AbstractTrees: print_tree
using ArgParse: @add_arg_table, ArgParseSettings, parse_args
using Profile: Profile, @profile
using ProfileSVG: ProfileSVG


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

function print_analysis_results(io::IO, locals)
    println(io, "Variable Resolution")
    println(io, "Name           Position      Up")
    println(io, "-------------------------------")
    for (key, value) in sort(collect(pairs(locals)); by=(p -> position(p[1])))
        println(
            io,
            rpad(":$(LossyTrees.value(key.name))", 15)
            * lpad("$(position(key))", 8)
            * lpad(string(value), 8)
        )
    end
    println(io)
end

print_tokens(tokens::Vector{Tokenize.Token}) = print_tokens(stdout, tokens)

function _prepare_to_run(output_io::IO, error_io::IO, source::String, verbose::Bool)
    # Edge case: empty string.
    isempty(source) && return nothing

    # Parse.
    result = Parse.parse_lox(source)

    if !isempty(result.tokens)
        if verbose
            # Print tokens.
            print_tokens(output_io, result.tokens)
            println(output_io)

            # Print events.
            println(output_io, "Events")
            for event in result.events
                println(output_io, event)
            end

            println(output_io)
        end

        # Construct lossless syntax tree.
        tree = LosslessTrees.build_tree(result)

        if verbose
            # Print lossless tree.
            println(output_io, "Lossless Syntax Tree")
            println(output_io, tree)
        end

        # Validate the syntax.
        diagnostics = SyntaxValidation.validate_syntax(tree)
        if !isempty(diagnostics)
            SyntaxValidation.show_diagnostics(error_io, diagnostics, source)
            return 65
        end

        # Craft the lossy tree.
        lossy_tree = LossyTrees.to_lossy(tree)

        # Print the lossy tree.
        if verbose
            println(output_io, "Lossy Syntax Tree")
            println(output_io, lossy_tree)
        end

        # Resolve variables (resolving the semantics).
        locals, diagnostics = Resolver.resolve_scopes(lossy_tree)
        if !isempty(diagnostics)
            SyntaxValidation.show_diagnostics(error_io, diagnostics, source)
            return 65
        end

        # Print analysis results.
        if verbose
            print_analysis_results(output_io, locals)
        end
        return lossy_tree, locals
    end
    return nothing
end

function run(output_io::IO, error_io::IO, interpreter_state::Interpret.InterpreterState, source::String, verbose::Bool)
    prep_result = _prepare_to_run(output_io, error_io, source, verbose)
    isnothing(prep_result) && return 0
    lossy_tree, locals = prep_result
    if verbose
        println(output_io, "Interpreter")
        println(output_io, "-----------")
    end
    Interpret.update_local_scope_map!(interpreter_state, locals)
    had_error = Interpret.interpret(interpreter_state, lossy_tree, source)
    exit_code = had_error ? 70 : 0
    return exit_code
end

function run_transpiled(output_io::IO, error_io::IO, state::Transpile.TranspilerState, source::String, verbose::Bool)
    prep_result = _prepare_to_run(output_io, error_io, source, verbose)
    isnothing(prep_result) && return 0
    lossy_tree, locals = prep_result
    native_expr = Transpile.transpile(state, lossy_tree)
    if verbose
        println(output_io, "Transpiled Code")
        print_tree(output_io, native_expr)
        println(output_io)
        if verbose
            println(output_io, "Interpreter")
            println(output_io, "-----------")
        end
    end
    # TODO: Figure out if we need to apply the scope map somehow to the interpreter.
    had_error = Transpile.interpret_transpiled(state, native_expr, source)
    exit_code = had_error ? 70 : 0
    return exit_code
end


"""Create a super lightweight REPL experience."""
function run_prompt(verbose::Bool, transpile::Bool)::Integer
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
    if transpile
        state = Transpile.TranspilerState()
        run_fn = run_transpiled
    else
        state = Interpret.InterpreterState()
        run_fn = run
    end

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
            run_fn(stdout, stderr, state, line, verbose)
        end
        println()  # Add an extra newline after the result.
    end
    return exit_code
end

function run_file(output_io::IO, error_io::IO, filepath::String, verbose::Bool, transpile::Bool)::Integer
    source = read(filepath, String)
    if transpile
        exit_code = run_transpiled(output_io, error_io, Transpile.TranspilerState(), source, verbose)
    else
        exit_code = run(output_io, error_io, Interpret.InterpreterState(), source, verbose)
    end
    return exit_code
end

run_file(filepath::String, verbose::Bool, transpile::Bool)::Integer = run_file(stdout, stderr, filepath, verbose, transpile)

function parse_command_line(args)
    settings = ArgParseSettings()
    @add_arg_table settings begin
        "--verbose"
            help = "display tokenization, parsing, and analysis in addition to interpreting the code"
            action = :store_true
        "--transpile"
            help = "transpile Lox to Julia for higher performance"
            action = :store_true
        "--profile-internals"
            help = "use the Profile Julia package to profile JuLox as it runs"
            action = :store_true
        "filepath"
            help = "path of .lox file to run (REPL launched if not given)"
    end
    parsed_args = parse_args(args, settings)
    return parsed_args
end


function cli(args, do_exit)
    args = parse_command_line(args)
    verbose = args["verbose"]
    profile = args["profile-internals"]
    transpile = args["transpile"]
    filepath = args["filepath"]
    if isnothing(filepath)
        profile && error("Profiling REPL is not supported, please pass a filename")
        exit_code = run_prompt(verbose, transpile)
        exit(exit_code)
    else
        if profile
            Profile.clear()
            exit_code = @profile run_file(filepath, verbose, transpile)
            ProfileSVG.save("julox_profile.svg"; maxdepth=200)
        else
            exit_code = run_file(filepath, verbose, transpile)
        end
        if do_exit
            exit(exit_code)
        else
            return exit_code
        end
    end
end

# Useful to make this an option but not a requirement for precompiling purposes.
cli() = cli(ARGS, true)


end  # module
