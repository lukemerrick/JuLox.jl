module Transpile
using JuLox: LossyTrees

struct TranspilerState
    mod::Module

    function TranspilerState()
        return new(Module())
    end
end

function transpile(state::TranspilerState, node::LossyTrees.Toplevel, source::String)
    return Expr(:call, :println, "hello world!")
end

function interpret_transpiled(state::TranspilerState, expr::Expr)
    had_error = false
    try
        Base.eval(state.mod, expr)
    catch e
        @info e
        had_error = true
    end
    return had_error
end

end  # module Transpile
