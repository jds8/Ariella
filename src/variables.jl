module VariableModule

include("./types.jl");

using .TypesModule

export Variable, Expression_Variable, Function_Variable;
export get_name, get_type, get_args, get_return_type;

abstract type Variable end;

struct Expression_Variable <: Variable
    name::String
    type::Variable_Type
    Expression_Variable(name::String, type::Variable_Type) = new(name, type);
    Expression_Variable(name::String) = new(name, Dynamic_Type());
end

# Create a copy of the input Expression_Variable with a new name, name.
Variable(name::String, var::Expression_Variable) = Expression_Variable(name, var.type);

# Returns Expression_Variable fields
get_name(ev::Expression_Variable) = ev.name;
get_type(ev::Expression_Variable) = ev.type;

struct Function_Variable <: Variable
    name::String
    args::Array{Expression_Variable,1}
    return_type::Variable_Type
end

# Create a copy of the input Function_Variable with a new name, name.
Variable(name::String, var::Function_Variable) = Function_Variable(name, var.args, var.return_type);

# Returns Function_Variable fields
get_name(fv::Function_Variable) = fv.name;
get_args(fv::Function_Variable) = fv.args;
get_return_type(fv::Function_Variable) = fv.return_type;
# Returns a Function_Type corresponding to the input Function_Variable
get_type(fv::Function_Variable) = Function_Type(fv.args.→get_type, fv.return_type);

end
