module JuLox

using PrecompileTools: @setup_workload, @compile_workload

# Some functions that have methods defined in various places.
function span end
function startbyte end
function endbyte end

include("./syntax_kinds.jl")
include("./tokenize.jl")
include("./parse.jl")
include("./lossless_trees.jl")
include("./syntax_validation.jl")
include("./lossy_trees.jl")
include("./resolve.jl")
include("./interpret.jl")
include("./transpile.jl")
include("./entrypoint.jl")

# @setup_workload begin
#     example_dir_path = abspath(joinpath(dirname(@__FILE__),  "..", "lox_examples"))
#     # Read the LoxLox interpreter file (a really big Lox program).
#     # We cut off the part that runs a file (since that requires stdin).
#     loxlox_lines = readlines(joinpath(example_dir_path, "lox.lox"))
#     loxlox_contents = join(loxlox_lines[1:1920], '\n')
#     println("Precompiling, hang on for a minute or three...")

#     @compile_workload begin
#         println("Running a small program from a file...")
#         Entrypoint.cli([joinpath(example_dir_path, "complex_code.lox")], false)
#         println("Running a really big program (the LoxLox interpreter)...")
#         Entrypoint.run(devnull, devnull, Interpret.InterpreterState(), loxlox_contents, false, false)
#     end
#     println("Programs finished, building the cache probably (?)")
# end


end # module
