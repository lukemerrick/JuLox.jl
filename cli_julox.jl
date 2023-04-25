using Fractal.JuLox.Entrypoint: run_file, run_prompt

function main()
    if length(ARGS) > 1
        println("Maximum one argument (a script file path).")
        exit(64)
    elseif length(ARGS) == 1
        exit_code = run_file(ARGS[1])
        exit(exit_code)
    else
        exit_code = run_prompt()
        exit(exit_code)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
