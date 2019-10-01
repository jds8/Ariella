module VariableModule

include("./types.jl");

using .TypesModule

export Variable, Expression_Variable, Function_Variable

abstract type Variable end;

struct Expression_Variable <: Variable
    name::String
    type::Variable_Type
    Expression_Variable(name::String, type::Variable_Type) = new(name, type);
    Expression_Variable(name::String) = new(name, Dynamic_Type());
end

# Returns an Expression_Variable corresponding to the input Expression_Variable
get_type(ev::Expression_Variable) = ev.type;

struct Function_Variable <: Variable
    name::String
    args::Array{Expression_Variable,1}
    return_type::Variable_Type
end

end
