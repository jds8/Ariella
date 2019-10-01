module OperatorModule

include("./values.jl")

using .ValueModule;

export Operator

export eqeq, gt, ge, lt, le, add, subtract, multiply, divide, not, and, or, cons;

# Operator enum
@enum Operator_Class binary=1 unary=2 null=3

# Defines an Operator
struct Operator
    value::String
    precedence::Int64
    op_class::Operator_Class
    operation::Function
end

Operator(val::String) = Operator(val, -1, null::Operator_Class, ()->throw("Null operator"));

is_valid(o::Operator) = o.precedence >= 1;

# Define Operators
eqeq = Operator("==", 5, binary, ==);
gt = Operator(">", 5, binary, >);
ge = Operator(">=", 5, binary, >=);
lt = Operator("<", 5, binary, <);
le = Operator("<=", 5, binary, <=);
plus = Operator("+", 10, binary, +);
minus = Operator("-", 10, binary, -);
times = Operator("*", 20, binary, *);
divide = Operator("/", 20, binary, /);
not = Operator("!", 25, unary, x::Bool_Value->!x.value);
and = Operator("&&", 25, binary, (x::Bool_Value,y::Bool_Value)->x.value&&y.value);
or = Operator("||", 25, binary, (x::Bool_Value,y::Bool_Value)->x.value||y.value);
# h::t is the cons operator which prepends elements h onto t in a new list
cons = Operator("::", 30, binary, (h,t)->(h_copy=deepcopy(h);push!(hh, t...);));

end
