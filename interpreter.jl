module Interpreter

using Main.LexerModule;

import Main.LexerModule.op
import Main.LexerModule.kw
import Main.LexerModule.punc
import Main.LexerModule.var
import Main.LexerModule.number
import Main.LexerModule.eof
import Main.LexerModule.assign
import Main.LexerModule.binary

using Main.ParserModule;

export interpret, evaluate;

function get_type(val::T) where {T <: Real}
    if isa(val, Int64)
        return Int_Type();
    elseif isa(val, Float64);
        return Float_Type();
    end
    throw(string(val, " is a number of an unsupported primitive type."));
end

abstract type Value end;

struct Number_Value{T <: Real} <: Value
    value::T
    type::Concrete_Type
    Number_Value(val::T) = new(val, get_type(val));
end

struct Function_Value <: Value
    value::Function
    type::Function_Type
end

# Define operations on Concrete_Types
##################################################
import Base.+, Base.-, Base.*, Base./
+(x::Float_Type, y::Concrete_Type) = Float_Type();
-(x::Float_Type, y::Concrete_Type) = Float_Type();
*(x::Float_Type, y::Concrete_Type) = Float_Type();
/(x::Float_Type, y::Concrete_Type) = Float_Type();
+(x::Concrete_Type, y::Float_Type) = Float_Type();
-(x::Concrete_Type, y::Float_Type) = Float_Type();
*(x::Concrete_Type, y::Float_Type) = Float_Type();
/(x::Concrete_Type, y::Float_Type) = Float_Type();
+(x::Int_Type, y::Int_Type) = Int_Type();
-(x::Int_Type, y::Int_Type) = Int_Type();
*(x::Int_Type, y::Int_Type) = Int_Type();
/(x::Int_Type, y::Int_Type) = Float_Type();
# Note that dividing Ints results in a Float
##################################################

# Override operators for values
function +(v1::Number_Value{T}, v2::Number_Value{U}) where {T,U <: Real}
    return Number_Value(v1.value + v2.value, v1.type + v2.type);
end
function -(v1::Number_Value{T}, v2::Number_Value{U}) where {T,U <: Real}
    return Number_Value(v1.value - v2.value, v1.type - v2.type);
end
function *(v1::Number_Value{T}, v2::Number_Value{U}) where {T,U <: Real}
    return Number_Value(v1.value * v2.value, v1.type * v2.type);
end
function /(v1::Number_Value{T}, v2::Number_Value{U}) where {T,U <: Real}
    return Number_Value(v1.value / v2.value, v1.type / v2.type);
end

# Determines if we can operate on these two function variables
function co_operable(f1::Function_Value, f2::Function_Value)
    return f1.type == f2.type
end

# Override operators for functions
function +(v1::Function_Value, v2::Function_Value)
    if co_operable(v1, v2)
        return Function_Value(v1.value + v2.value, v1.type);
    end
    throw(string("Cannot add functions of different types: ", v1, " ", v2));
end
function -(v1::Function_Value, v2::Function_Value)
    if co_operable(v1, v2)
        return Function_Value(v1.value - v2.value, v1.type);
    end
    throw(string("Cannot subtract functions of different types: ", v1, " ", v2));
end
function *(v1::Function_Value, v2::Function_Value)
    if co_operable(v1, v2)
        return Function_Value(v1.value * v2.value, v1.type);
    end
    throw(string("Cannot multiply functions of different types: ", v1, " ", v2));
end
function /(v1::Function_Value, v2::Function_Value)
    if co_operable(v1, v2)
        return Function_Value(v1.value / v2.value, v1.type);
    end
    throw(string("Cannot divide functions of different types: ", v1, " ", v2));
end

# Definition_Table is for storing variable name->Value pairs
struct Definition_Table
    map::Dict{String,Value}
end

# Gets a value from the Definition_Table given a token/variable name
function get(table::Definition_Table, t::Token)
    if t.value in table.keys
        return table.map[t.value];
    end
    throw(string("Unknown free variable: ", t.value));
end

function evaluate(t::Token, table::Definition_Table)
    if t.class == number::Class
        if occursin(".", t.value)
            return Number_Value(parse(Float64, t.value));
        else
            return Number_Value(parse(Int64, t.value));
        end
    elseif t.class == var::Class
        return get(table, t);
    end
    throw(string("Cannot evaluate token ", t.value, " of class ", t.class));
end

function evaluate(binary_exp::Binary_Expression, table::Definition_Table)
    left_val = evaluate(binary_exp.left);
    right_val = evaluate(binary_exp.right);
    return binary_exp.operator.operation(left_val, right_val);
end

function add!(table::Definition_Table, var::Expression_Variable, exp::Expression)
    value = evaluate(exp, table);
    if var.type == Dynamic_Type() || var.type == value.type
        table[var.name] = value;
    else
        throw(string("Type mismatch: ", var.value, " is not of type ", value.type));
    end
end

function add!(table::Definition_Table, var::Function_Variable, exp::Expression)
    value = evaluate(exp, table);
    if var.return_type == Dynamic_Type() || var.return_type == value.type
        table[var.name] = value;
    else
        throw(string("Type mismatch: ", var.value, " is not of type ", value.type));
    end
end

→(lst, field) = getfield(lst, field);

function evaluate(call::Call, table::Definition_Table)
    value = evaluate(call.exp, table);
    fn_type = value.type;
    if length(fn_type.arg_types) == length(call.arg_list)
        args = Value[];
        for (i,arg) in enumerate(call.arg_list)
            arg_value = evaluate(arg, table);
            fn_arg_type = fn_type.arg_types[i];
            # If the function argument type does not match the input type, throw
            if arg_value.type != fn_arg_type && fn_arg_type == Dynamic_Type()
                throw(string("Type mismatch: ", arg_value, " is not a ", fn_arg_type));
            else
                push!(args, arg_value);
            end
        end
        # Create an expression of a call and all the arguments
        call_value = Expr(:call, :(value.value), (args.→:value)...);
        return Value(call_value, value.type.return_type);
    else
        throw(string("Function requires ", length(fn_type.arg_types), " arguments.");
    end
end

function evaluate(let_binding::Let_Binding, table::Definition_Table)
    add!(table, let_binding.variable, let_binding.exp);
    evaluate(let_binding.rest, table);
end

function interpret(prog::Array{Expression, 1})
    table = Definition_Table();
    for exp in prog
        evaluate(exp, table);
    end
end

end
