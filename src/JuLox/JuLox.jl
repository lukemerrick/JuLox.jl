module JuLox

struct SyntaxError <: Exception
    line::Int
    position::Int
    message::String
end

include("./kinds.jl")
include("./tokenize.jl")
include("./entrypoint.jl")

end # module
