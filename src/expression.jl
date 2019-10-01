module ExpressionModule

include("./abstractexpressions.jl");
include("./operators.jl");
include("./variables.jl");

using .AbstractExpressionsModule
using .OperatorModule
using .VariableModule

export Null_Expression, If_Statement, Unary_Expression, Binary_Expression
export Call, Let_Binding, Abstract_Cons_Expression, Cons_Expression, Empty_Cons_Expression
export Pattern_Match

struct Null_Expression <: Expression end;

struct If_Statement <: Expression
    condition::Expression
    then_exp::Expression
    else_exp::Expression
end

struct Unary_Expression <: Expression
    operator::Operator
    exp::Expression
end

struct Binary_Expression <: Expression
    left::Expression
    operator::Operator
    right::Expression
end

struct Call <: Expression
    exp::Expression
    arg_list::Array{Expression, 1}
end

struct Let_Binding <: Expression
    variable::Variable
    exp::Expression
    rest::Expression
    Let_Binding(var::Variable, exp::Expression, rest::Expression) = new(var, exp, rest);
    Let_Binding(var::Variable, exp::Expression) = new(var, exp, Null_Expression());
end

abstract type Abstract_Cons_Expression <: Matchable_Expression end;

struct Cons_Expression <: Abstract_Cons_Expression
    head::String
    tail::Abstract_Cons_Expression
end

struct Empty_Cons_Expression <: Abstract_Cons_Expression end;

struct Pattern_Match <: Expression
    exp::Expression_Variable
    clauses::Array{Pair{Matchable_Expression, Expression}, 1}
end

end
