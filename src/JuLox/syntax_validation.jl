module SyntaxValidation

using Fractal.JuLox: JuLox, LosslessTrees, SyntaxKinds

#-------------------------------------------------------------------------------
# Diagnostic structure.

struct Diagnostic
    _first_byte::Int
    _last_byte::Int
    _message::String
end

function Diagnostic(node::LosslessTrees.LosslessNode, message::String)
    return Diagnostic(JuLox.startbyte(node), JuLox.endbyte(node), message)
end

first_byte(d::Diagnostic) = d._first_byte
last_byte(d::Diagnostic) = d._last_byte
message(d::Diagnostic) = d._message
Base.range(d::Diagnostic) = first_byte(d):last_byte(d)


function show_diagnostic(io::IO, diagnostic::Diagnostic, source::String)
    # Figure out the location of the issue.
    issue_start = first_byte(diagnostic)
    if issue_start > length(source)
        @warn "Diagnostic start byte is $issue_start while source is only length $(length(source))"
        issue_start = length(source)
    end
    lines = split(source[1:issue_start], '\n')
    line_number = length(lines)
    column_number = length(lines[end])
    linecol = "[line $(lpad(line_number, 4)), column $(lpad(column_number, 3))]"

    # Print the error.
    println(io, "# Error @ $linecol - $(message(diagnostic))")

    return nothing
end

function show_diagnostics(io::IO, diagnostics::AbstractVector{Diagnostic}, source::String)
    return show_diagnostic.(Ref(io), diagnostics, Ref(source))
end


#-------------------------------------------------------------------------------
# Validating the lossless tree.

# Recursive implementation. Modifying the `diagnostics` vector inplace is more efficient
# than combining child results.
function _validate_syntax(node::LosslessTrees.LosslessNode, diagnostics::Vector{Diagnostic})
    # For now, just check if the node is an error.
    # TODO: Consider skipping error cascades, e.g. from unterminated strings.
    k = SyntaxKinds.kind(node)
    if SyntaxKinds.is_error(k)
        diagnostic = Diagnostic(node, SyntaxKinds.error_description(k))
        push!(diagnostics, diagnostic)
    end

    # Recurse downward.
    if LosslessTrees.haschildren(node)
        _validate_syntax.(LosslessTrees.children(node), Ref(diagnostics))
    end
    return nothing
end

function validate_syntax(root_node::LosslessTrees.LosslessNode)
    diagnostics = Diagnostic[]
    _validate_syntax(root_node, diagnostics)
    return diagnostics
end


end  # module SyntaxValidation
