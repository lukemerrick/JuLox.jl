module JuLox

# Some functions that have methods defined in various places.
function span end
function kind end
function startbyte end
function endbyte end

include("./kinds.jl")
include("./tokenize.jl")
# include("./parse/parse.jl")
# include("./interpret.jl")
include("./parse.jl")
include("./lossless_tree.jl")
include("./entrypoint.jl")

end # module
