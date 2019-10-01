module AbstractExpressionsModule

export Expression, Value, Matchable_Expression

abstract type Expression end;
abstract type Value <: Expression end;
abstract type Matchable_Expression <: Expression end;

end
