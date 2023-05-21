using Fractal.JuLox.Entrypoint: run_file, run_prompt
using ArgParse: @add_arg_table, ArgParseSettings, parse_args

function parse_command_line()
    settings = ArgParseSettings()
    @add_arg_table settings begin
        "--verbose"
            help = "display tokenization, parsing, and analysis in addition to interpreting the code"
            action = :store_true
        "filepath"
            help = "path of .lox file to run (REPL launched if not given)"
    end
    parsed_args = parse_args(settings)
    return parsed_args
end

function main()
    args = parse_command_line()
    verbose = args["verbose"]
    filepath = args["filepath"]
    if !isnothing(filepath)
        exit_code = run_file(filepath, verbose)
        exit(exit_code)
    else
        exit_code = run_prompt(verbose)
        exit(exit_code)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
