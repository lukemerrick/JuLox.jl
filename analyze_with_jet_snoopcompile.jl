# Requires JET and SnoopCompile, neither of which is a dependency for JuLox.
using SnoopCompile: @snoopi_deep, report_callees, inference_triggers
using JET: @report_call
using JuLox: JuLox

const FILE_NAME = "lox_examples/class_methods.lox"

# Load some Lox code.
code = read(FILE_NAME, String)

# Check for issues in tokenization and parsing.
println("Checking parsing")
println(report_callees(inference_triggers(@snoopi_deep JuLox.Parse.parse_lox(code))))
# println(@report_call JuLox.Parse.parse_lox(code))
println("Parsing")
@time parse_result = JuLox.Parse.parse_lox(code)

# Check for issues in the lossless tree.
println("Checking lossless tree")
println(report_callees(inference_triggers(@snoopi_deep JuLox.LosslessTrees.build_tree(parse_result))))
# println(@report_call JuLox.LosslessTrees.build_tree(parse_result))
println("Building lossless tree")
@time lossless_tree = JuLox.LosslessTrees.build_tree(parse_result)

# Check for issues in syntax validation.
println("Checking syntax validation")
println(report_callees(inference_triggers(@snoopi_deep JuLox.SyntaxValidation.validate_syntax(lossless_tree))))
# println(@report_call JuLox.SyntaxValidation.validate_syntax(lossless_tree))
println("Validating syntax")
@time JuLox.SyntaxValidation.validate_syntax(lossless_tree)

# Check for issues in syntax validation.
println("Checking lossy tree")
println(report_callees(inference_triggers(@snoopi_deep JuLox.LossyTrees.to_lossy(lossless_tree))))
# println(@report_call JuLox.LossyTrees.to_lossy(lossless_tree))
println("Building lossy tree")
@time lossy_tree = JuLox.LossyTrees.to_lossy(lossless_tree)

# Check for issues in resolution.
println("Checking resolution")
println(report_callees(inference_triggers(@snoopi_deep JuLox.Resolver.resolve_scopes(lossy_tree))))
# println(@report_call JuLox.Resolver.resolve_scopes(lossy_tree))
println("Running resolution")
@time locals, diagnostics = JuLox.Resolver.resolve_scopes(lossy_tree)

# Check for issues in interpreting.
println("Checking interpreting")
println(report_callees(inference_triggers(
    @snoopi_deep begin
        state = JuLox.Interpret.InterpreterState(devnull,devnull)
        JuLox.Interpret.update_local_scope_map!(state, locals)
        JuLox.Interpret.interpret(state, lossy_tree, code)
    end
)))
println("Running interpreter")
@time begin
    state = JuLox.Interpret.InterpreterState(devnull,devnull)
    JuLox.Interpret.update_local_scope_map!(state, locals)
    JuLox.Interpret.interpret(state, lossy_tree, code)
end
