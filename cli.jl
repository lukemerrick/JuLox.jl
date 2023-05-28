using JuLox.Entrypoint: cli

if abspath(PROGRAM_FILE) == @__FILE__
    cli()
end
