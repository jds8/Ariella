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

# Gets Ariella type given a Julia value
function get_type(val::T) where {T <: Real}
    if isa(val, Int64)
        return Int_Type();
    elseif isa(val, Float64);
        return Float_Type();
    end
    throw(string(val, " is a number of an unsupported primitive type."));
end

# Converts an Ariella type to a Julia type
function convert_concrete_type(atype::Concrete_Type)
    if isa(atype, Int_Type)
        return Int64;
    elseif isa(atype, Float_Type)
        return Float64;
    elseif isa(atype, Function_Type)
        return Function;
    end
    throw("Unknown Ariella primitive type");
end

# Converts an Ariella type to a Julia type string
function convert_type_str(atype::Variable_Type)
    if isa(atype, Concrete_Type)
        return string(convert_concrete_type(atype));
    elseif isa(atype, Dynamic_Type)
        return "";
    else
        throw("Unknown Ariella type");
    end
end

abstract type Value end;

struct Number_Value{T <: Real} <: Value
    value::T
    type::Primitive_Type
    Number_Value(val::T) = new(val, get_type(val));
end

struct Function_Value <: Value
    value::Function
    type::Function_Type
end

# Define operations on Primitive_Types
##################################################
import Base.+, Base.-, Base.*, Base./
+(x::Float_Type, y::Primitive_Type) = Float_Type();
-(x::Float_Type, y::Primitive_Type) = Float_Type();
*(x::Float_Type, y::Primitive_Type) = Float_Type();
/(x::Float_Type, y::Primitive_Type) = Float_Type();
+(x::Primitive_Type, y::Float_Type) = Float_Type();
-(x::Primitive_Type, y::Float_Type) = Float_Type();
*(x::Primitive_Type, y::Float_Type) = Float_Type();
/(x::Primitive_Type, y::Float_Type) = Float_Type();
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

# Creates a function out of two functions using an operator string
# Outputs a Julia function
function create_fn(v1::Function_Value, op::Function, v2::Function_Value)::Function
    if co_operable(v1, v2)
        arg_list = String[];
        for (i, arg) in enumerate(v1.type.arg_types)
            arg_name = repeat("x", i);
            arg_type = convert_type_str(arg);
            # Add type annotation if type is not dynamic
            arg_type = isempty(arg_type) ? arg_type : string("::", arg_type);
            arg_with_type = string(arg_name, arg_type);
            push!(arg_list, arg_with_type);
        end
        # args_str is the arguments of the anonymous function
        args_str = string("(", join(arg_list, ","), ")");
        # out_str is a string representing the definition of the function
        out_str = string(v1.value, args_str, op, v2.value, args_str);
        # parse the concatenation of these strings and evaluate to get a function
        return eval(Meta.parse(string(args_str, "->", out_str)));
    end
    throw("Input functions are of different types.")
end

# Override operators for functions
function +(v1::Function_Value, v2::Function_Value)
    return Function_Value(create_fn(v1, +, v2), v1.type);
end
function -(v1::Function_Value, v2::Function_Value)
    return Function_Value(create_fn(v1, -, v2), v1.type);
end
function *(v1::Function_Value, v2::Function_Value)
    return Function_Value(create_fn(v1, *, v2), v1.type);
end
function /(v1::Function_Value, v2::Function_Value)
    return Function_Value(create_fn(v1, /, v2), v1.type);
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
        call_value = eval(Expr(:call, :(value.value), (args.→:value)...));
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
