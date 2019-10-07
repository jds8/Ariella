module DefinitionTableModule

include("./abstractexpressions.jl");
include("./variables.jl");

using .AbstractExpressionsModule;
using .VariableModule;

export Variable_Binding, Definition_Table;

# The string var.name is mapped to this struct via a Definition_Table
# The variable var is bound to the expression exp for the first time in
# the Definition_Table at index, index.
struct Variable_Binding{T <: Variable}
    index::Int64
    var::T
    exp::Expression
end

# Definition_Table is for storing variable name->Value pairs
struct Definition_Table
    maps::Array{Dict{String,Variable_Binding},1}
    Definition_Table() = new([Dict{String,Variable_Binding}()]);
    Definition_Table(maps::Array{Dict{String,Variable_Binding},1}) = new(maps);
end

# Creates a Definition_Table using the first index Dicts from table
function Definition_Table(table::Definition_Table, index::Int64)
    return index > 0 ? Definition_Table(table.maps[1:index]) : Definition_Table();
end

# Accesses var.name in the indexth index of the table
function get_binding(table::Definition_Table, var_name::String, index::Int64 = length(table.maps))
    return table.maps[index][var_name];
end

# Gets the keys of the last dictionary in dt.maps
keys(dt::Definition_Table) = keys(dt.maps[end]);

# Determines if the name is in the table
∈(name::String, dt::Definition_Table) = name in keys(dt);

# Add a variable to the Definition_Table
function add(table::Definition_Table, var::Variable, exp::Expression)
    dt = isempty(table.maps) ? Dict{String,Variable_Binding}() : deepcopy(table.maps[end]);
    dt[get_name(var)] = Variable_Binding(length(table.maps), var, exp);
    push!(table.maps, dt);
end

end
