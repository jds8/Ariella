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
export Bool_Value, Number_Value;

# Gets Ariella type given a Julia value
get_type(val::Int64) = Int_Type();
get_type(val::Float64) = Float_Type();
get_type(val::Bool) = Bool_Type();

# Converts an Ariella type to a Julia type string
convert_type_to_str(atype::Int_Type) = string(Int64);
convert_type_to_str(atype::Float_Type) = string(Float64);
convert_type_to_str(atype::Bool_Type) = string(Bool);
convert_type_to_str(atype::Function_Type) = string(Function);
convert_type_to_str(atype::Dynamic_Type) = "";

abstract type Value end;
struct Number_Value{T <: Real} <: Value
    value::T
    type::Numeric_Type
    Number_Value(val::T) = new(val, get_type(val));
    Number_Value(val::T, type::Numeric_Type) = type == get_type(val) ? new(val,type) : error("Type mismatch");
end
struct Bool_Value <: Value
    value::Bool
    type::Bool_Type
    Bool_Value(val::Bool) = new(val, Bool_Type());
    Bool_Value(val::Bool, type::Bool_Value) = new(val, type);
end
struct Function_Value <: Value
    value::Function
    type::Function_Type
end
struct Null_Value <: Value end;

# Getters for Values
get_value(num::Number_Value{T} where {T <: Real})::T = num.value;
get_value(bool::Bool_Value)::Bool = bool.value;
get_value(fn::Function_Value)::Function = fn.value;
get_type(num::Number_Value{T})::Numeric_Type = num.type;
get_type(bool::Bool_Value)::Bool_Type = bool.type;
get_type(fn::Function_Value)::Function_Type = fn.type;

# Define operations on Primitive_Types
##################################################
import Base.+, Base.-, Base.*, Base./
+(x::Float_Type, y::Numeric_Type) = Float_Type();
-(x::Float_Type, y::Numeric_Type) = Float_Type();
*(x::Float_Type, y::Numeric_Type) = Float_Type();
/(x::Float_Type, y::Numeric_Type) = Float_Type();
+(x::Numeric_Type, y::Float_Type) = Float_Type();
-(x::Numeric_Type, y::Float_Type) = Float_Type();
*(x::Numeric_Type, y::Float_Type) = Float_Type();
/(x::Numeric_Type, y::Float_Type) = Float_Type();
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
# Only works when the return type is numeric (for now)
function co_operable(f1::Function_Value, f2::Function_Value)
    return f1.type == f2.type && isa(f1.type.return_type, Numeric_Type)
end

# Gets the arguments of f as a string of the form "(x, y::Type,...)"
function get_rest_of_args(f::Function_Type, index::Int64 = 1)
    arg_list = String[];
    for (i, arg) in enumerate(f.arg_types[index:end])
        arg_name = repeat("x", i);
        arg_type = convert_type_to_str(arg);
        # Add type annotation if type is not dynamic
        arg_type = isempty(arg_type) ? arg_type : string("::", arg_type);
        arg_with_type = string(arg_name, arg_type);
        push!(arg_list, arg_with_type);
    end
    # Return the arguments of an anonymous function
    return string("(", join(arg_list, ","), ")");
end

# Creates a Julia Function from an arguments string and a return string
function generate_fun_from_strs(args_str::String, out_str::String)::Function
    # parses the concatenation of these strings and evaluates to get a function
    return eval(Meta.parse(string(args_str, "->", out_str)));
end

# Creates a function out of two functions using an operator string
# Outputs a Julia function
function create_fn(v1::Function_Value, op::Function, v2::Function_Value)::Function
    if co_operable(v1, v2)
        args_str = get_rest_of_args(v1.type);
        # out_str is a string representing the definition of the function
        out_str = string(v1.value, args_str, op, v2.value, args_str);
        return generate_fun_from_strs(args_str, out_str);
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

# Returns a Function_Value of the same type as v1 that simply returns v2
# Throws if v2 is a different type from the return type of v1
function num_to_fun(num::Number_Value{T}, fun::Function_Value)::Function where {T <: Real}
    if num.type == fun.type.return_type
        args_str = get_rest_of_args(fun.type);
        new_fun = generate_fun_from_strs(args_str, string(num.value));
        return Function_Value(new_fun, fun.type);
    end
    throw(string("Cannot add ", num, " to output of ", fun));
end

# Override operators for functions and numbers
function +(v1::Function_Value, v2::Number_Value{T} where {T <: Real})
    f2 = num_to_fun(v2, v1);
    return Function_Value(create_fn(v1, +, f2), v1.type);
end
function -(v1::Function_Value, v2::Number_Value{T} where {T <: Real})
    f2 = num_to_fun(v2, v1);
    return Function_Value(create_fn(v1, -, f2), v1.type);
end
function *(v1::Function_Value, v2::Number_Value{T} where {T <: Real})
    f2 = num_to_fun(v2, v1);
    return Function_Value(create_fn(v1, *, f2), v1.type);
end
function /(v1::Function_Value, v2::Number_Value{T} where {T <: Real})
    f2 = num_to_fun(v2, v1);
    return Function_Value(create_fn(v1, /, f2), v1.type);
end
+(v1::Number_Value{T}, v2::Function_Value) where {T <: Real} = +(v2, v1);
-(v1::Number_Value{T}, v2::Function_Value) where {T <: Real} = -(v2, v1);
*(v1::Number_Value{T}, v2::Function_Value) where {T <: Real} = *(v2, v1);
/(v1::Number_Value{T}, v2::Function_Value) where {T <: Real} = /(v2, v1);

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
        return Number_Value(Meta.parse(t.value));
    elseif t.class == boolean::Class
        return Bool_Value(Meta.parse(t.value));
    elseif t.class == var::Class
        return get(table, t);
    end
    throw(string("Cannot evaluate token ", t.value, " of class ", t.class));
end

# Evaluate a null expression
evaluate(null::Null_Expression, table::Definition_Table) = Null_Value();

# Evaluate a unary expression
function evaluate(unary_exp::Unary_Expression, table::Definition_Table)
    return unary_exp.operator.operation(unary_exp.exp);
end

# Evaluate a binary expression
function evaluate(binary_exp::Binary_Expression, table::Definition_Table)
    left_val = evaluate(binary_exp.left, table);
    right_val = evaluate(binary_exp.right, table);
    return binary_exp.operator.operation(left_val, right_val);
end

# Add an expression variable to the Definition_Table
function add!(table::Definition_Table, var::Expression_Variable, exp::Expression)
    value = evaluate(exp, table);
    if var.type == Dynamic_Type() || var.type == value.type
        table[var.name] = value;
    else
        throw(string("Type mismatch: ", var.value, " is not of type ", value.type));
    end
end

# Add a function variable to the Definition_Table
function add!(table::Definition_Table, var::Function_Variable, exp::Expression)
    value = evaluate(exp, table);
    if var.return_type == Dynamic_Type() || var.return_type == value.type
        table[var.name] = value;
    else
        throw(string("Type mismatch: ", var.value, " is not of type ", value.type));
    end
end

# An infix operator that applies fun on obj
→(obj, fun) = fun(obj);

# Returns a particular Value
make_value(num_val::T, type::Numeric_Type) = Number_Value(num_val);
make_value(bool_val::Bool, type::Bool_Type) = Bool_Value(bool_val);
make_value(fun_val::Function, type::Function_Type) = Function_Value(fun_val, type);

# Returns a string representing the first length(args) arguments of
# the function being supplied by args and the rest supplied as x,xx,xxx...
# @param fn The function in question
# @param args A list of supplied arguments
# @param other_args A string of the form (x,xx,..) which are the unsupplied arguments
# @return A string of the form f(a,b,x,xx,..) where a and b are the supplied
# arguments and x,xx,... are the unsupplied ones
function get_partial_application_str(fn::Function, args::Array{Value,1}, other_args::String)
    # Ignore the initial "(" in other_args
    unsupplied_args = other_args[2:end];
    output_call = string(fn, "(");
    output_call = string(output_call, join(args.→get_value, ","), ",");
    output_call = string(output_call, unsupplied_args);
    return output_call;
end

# Evaluates a Call expression
function evaluate(call::Call, table::Definition_Table)
    value = evaluate(call.exp, table);
    fn_value = get_value(value);
    fn_type = get_type(value);
    # The number of arguments supplied can be fewer than the number requied
    # The result in this case would be a curried function
    num_missing_args = length(fn_type.arg_types) - length(call.arg_list);
    if num_missing_args >= 0
        args = Value[];
        for (i,arg) in enumerate(call.arg_list)
            arg_value = evaluate(arg, table);
            fn_arg_type = fn_type.arg_types[i];
            # If the function argument type does not match the input type, throw
            if get_type(arg_value) != fn_arg_type && fn_arg_type != Dynamic_Type()
                throw(string("Type mismatch: ", arg_value, " is not a ", fn_arg_type));
            else
                push!(args, arg_value);
            end
        end
        # Create an expression of a call and all the arguments
        args_str = get_rest_of_args(fn_type, length(args));
        # If all arguments have been supplied, then call the function
        if args_str == "()"
            call_value = eval(Expr(:call, fn_value, (args.→get_value)...));
        # Otherwise, curry it
        else
            out_str = get_partial_application_str(fn_value, args, args_str);
            call_value = generate_fun_from_strs(args_str, out_str);
        end
        return make_value(call_value, fn_type.return_type);
    else
        throw(string(length(fn_type.arg_types), " arguments expected, but ", length(cal.arg_list), " provided."));
    end
end

# Evaluates a let-binding
function evaluate(let_binding::Let_Binding, table::Definition_Table)
    value = evaluate(let_binding.exp);
    add!(table, let_binding.variable, value);
    evaluate(let_binding.rest, table);
end

as_bool(num::Number_Value{T} where {T <: Real})::Bool = get_value(num) != 0;
as_bool(bool::Bool_Value)::Bool = get_value(bool);

# Evaluates an if-statement
function evaluate(if_statement::If_Statement, table::Definition_Table)
    bool_val = evaluate(if_statement.condition, table);
    if as_bool(bool_val)
        return evaluate(then_exp, table);
    else
        return evaluate(else_exp, table);
    end
end

# Interprets a program
function interpret(prog::Array{Expression, 1})
    table = Definition_Table();
    for exp in prog
        evaluate(exp, table);
    end
end

end
