module JuLox

# Some functions that have methods defined in various places.
function span end
function startbyte end
function endbyte end

include("./syntax_kinds.jl")
include("./tokenize.jl")
# include("./parse/parse.jl")
# include("./interpret.jl")
include("./parse.jl")
include("./lossless_trees.jl")
include("./syntax_validation.jl")
include("./lossy_trees.jl")
include("./entrypoint.jl")

end # module
