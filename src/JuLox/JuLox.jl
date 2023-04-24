module JuLox

# Some functions that have methods defined in various places.
function span end
function kind end

include("./kinds.jl")
include("./tokenize.jl")
include("./parse/parse.jl")
include("./interpret.jl")
include("./entrypoint.jl")

end # module
