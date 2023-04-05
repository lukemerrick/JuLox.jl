using Fractal: JuLox

function main()
    if length(ARGS) > 1
        println("Maximum one argument (a script file path).")
        exit(64)
    elseif length(ARGS) == 1
        exit_code = JuLox.Entrypoint.run_file(ARGS[1])
        exit(exit_code)
    else
        exit_code = JuLox.Entrypoint.run_prompt()
        exit(exit_code)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
