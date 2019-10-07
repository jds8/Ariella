module ValueModule

include("./abstractexpressions.jl")
include("./types.jl")
include("./variables.jl")
include("./definitiontable.jl")

using .AbstractExpressionsModule;
using .TypesModule;
using .VariableModule;
using .DefinitionTableModule;

export Bool_Value, Number_Value, Function_Value, Null_Value;

# Define Values
struct Number_Value{T <: Real} <: Value
    value::T
    type::Numeric_Type
    Number_Value(val::T where T <: Real) = new{T}(val, get_type(val));
    Number_Value(val::T where T <: Real, type::Numeric_Type) = type == get_type(val) ? new{T}(val,type) : throw("Type mismatch");
end
struct Bool_Value <: Value
    value::Bool
    type::Bool_Type
    Bool_Value(val::Bool) = new(val, Bool_Type());
    Bool_Value(val::Bool, type::Bool_Value) = new(val, type);
end
abstract type Callable <: Value end;
struct Function_Value <: Callable
    dt::Definition_Table
    args::Array{Expression_Variable,1}
    body::Expression
    return_type::Variable_Type
end
struct Null_Value <: Value end;

# Getters for Values
get_underlying(num::Number_Value{T} where {T <: Real}) = num.value;
get_underlying(bool::Bool_Value) = bool.value;
get_underlying(fn::Function_Value) = string("Function", to_string(fn.type));
get_type(num::Number_Value{T} where {T <: Real}) = num.type;
get_type(bool::Bool_Value) = bool.type;

get_dt(fn::Function_Value) = fn.dt;
get_args(fn::Function_Value) = fn.args;
get_body(fn::Function_Value) = fn.body;
get_return_type(fn::Function_Value) = fn.return_type;
get_type(fn::Function_Value) = fn.type;

# Returns whether the Value is callable
is_callable(val::Callable) = true;
is_callable(val::Value) = false;

#

end
