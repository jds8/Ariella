module ValueModule

include("./abstractexpressions.jl")
include("./types.jl")

using .AbstractExpressionsModule;
using .TypesModule;

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
struct Function_Value <: Value
    args::Array{Expression_Variable,1}
    body::Expression
    type::Function_Type
end
struct Null_Value <: Value end;

# Getters for Values
get_underlying(num::Number_Value{T} where {T <: Real})::T = num.value;
get_underlying(bool::Bool_Value)::Bool = bool.value;
get_underlying(fn::Function_Value)::String = string("Function", to_string(fn.type));
get_type(num::Number_Value{T} where {T <: Real})::Numeric_Type = num.type;
get_type(bool::Bool_Value)::Bool_Type = bool.type;
get_type(fn::Function_Value)::Function_Type = fn.type;

end
