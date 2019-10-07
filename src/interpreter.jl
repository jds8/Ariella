module InterpreterModule

include("./abstractexpressions.jl");
include("./expression.jl");
include("./variables.jl");
include("./lexer.jl");
include("./values.jl");
include("./types.jl");
include("./operators.jl");
include("./definitiontable.jl");

using .AbstractExpressionsModule;
using .ExpressionModule;
using .VariableModule;
using .LexerModule;
using .ValueModule;
using .TypesModule;
using .OperatorModule;
using .DefinitionTableModule;

import .LexerModule.op
import .LexerModule.kw
import .LexerModule.punc
import .LexerModule.var
import .LexerModule.number
import .LexerModule.eof
import .LexerModule.assign
import .LexerModule.binary

export interpret, evaluate;

# Gets the value in the defintion table for this token
function get_value(table::Definition_Table, t::Token)
    if t.value ∈ table
        binding = get_binding(table, t.value);
        return get_value(table, binding);
    end
    throw(string("Unknown free variable: ", t.value));
end

# Retrieves an Expression_Variable from the definition table
function get_value(dt::Definition_Table, exp::Expression, var::Expression_Variable)
    return evaluate(exp, dt);
end

# Retrieves a Function_Variable
function get_value(dt::Definition_Table, exp::Expression, var::Function_Variable)
    return Function_Value(dt, get_args(var), exp, get_return_type(var));
end

# Retrieves a variable from the definition table
function get_value(table::Definition_Table, binding::Variable_Binding{T} where T <: Variable)
    dt = Definition_Table(table, binding.index);
    exp = binding.exp;
    get_value(dt, exp, binding.var);
end

# A table containing transformations from one token to another
struct Transformation_Table
    map::Dict{String,Expression}
    Transformation_Table(map::Dict{String,Expression}) = new(map);
    Transformation_Table() = new(Dict{String,Expression}());
end

# Determines if the Expression is in the table
∈(s::String, transforms::Transformation_Table) = s in transforms.map.keys;

# Adds String to Transformation_Table
function add(transforms::Transformation_Table, var::String, exp::Expression)
    transforms.map[var] = exp;
end

# Accesses var_name in transforms
function get(var_name::String, transforms::Transformation_Table)
    return transforms.map[var_name];
end

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

# Evaluates a Value
evaluate(value::Value, table::Definition_Table) = value;
# Transform a Value
transform(v::Value, transforms::Transformation_Table) = v;

# Evaluates a Token
function evaluate(t::Token, table::Definition_Table)
    if t.class == number::Class
        return Number_Value(Meta.parse(t.value));
    elseif t.class == boolean::Class
        return Bool_Value(Meta.parse(t.value));
    elseif t.class == var::Class
        return get_value(table, t);
    end
    throw(string("Cannot evaluate token ", t.value, " of class ", t.class));
end

# Transforms a variable Token t into another Token via the transforms table
function transform(t::Token, transforms::Transformation_Table)
    # If the Token is not a variable or not in transforms, then return it unmodified
    if t.class == var::Class && t.value ∈ transforms
        return get(t.value, transforms);
    else
        return t;
    end
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

# Override operators for values
function +(v1::ValueModule.Number_Value{T}, v2::ValueModule.Number_Value{U}, dt::Definition_Table) where {T,U <: Real}
    return Number_Value(v1.value + v2.value, v1.type + v2.type);
end
function -(v1::ValueModule.Number_Value{T}, v2::ValueModule.Number_Value{U}, dt::Definition_Table) where {T,U <: Real}
    return Number_Value(v1.value - v2.value, v1.type - v2.type);
end
function *(v1::ValueModule.Number_Value{T}, v2::ValueModule.Number_Value{U}, dt::Definition_Table) where {T,U <: Real}
    return Number_Value(v1.value * v2.value, v1.type * v2.type);
end
function /(v1::ValueModule.Number_Value{T}, v2::ValueModule.Number_Value{U}, dt::Definition_Table) where {T,U <: Real}
    return Number_Value(v1.value / v2.value, v1.type / v2.type);
end

# Determines if we can operate on these two function variables
# Only works when the return type is numeric (for now)
function is_co_operable(f1::ValueModule.Function_Value, f2::ValueModule.Function_Value)
    return f1.type == f2.type && isa(f1.type.return_type, Numeric_Type)
end

# Returns a new Function_Value
function transform(f::ValueModule.Function_Value, args::Array{Expression_Variable,1}, transforms::Transformation_Table)
    transformed_body = transform(transforms, f.body);
    return Function_Value(f.dt, args, transformed_body, f.type);
end

# Replaces all local function variables with new local variables such that
# all functions passed in have the same local variable names
function transform(dt::Definition_Table, vs::ValueModule.Function_Value...)
    v1 = vs[1];
    for i in 2:length(vs)
        if !is_co_operable(v1, vs[i])
            throw("Input functions have different types so are not co-operable.");
        end
    end
    key_str = string(keys(dt)...);
    fs = Function_Value[];
    args = Expression_Variable[];
    for j in 1:length(vs)
        vj = vs[j];
        transforms_j = Transformation_Table();
        for (i, arg) in enumerate(vj.args)
            var = string(key_str, i);
            add(transforms_j, arg.name, Token(var::Class, var));
            # Create Expression_Variables out of these new variable names
            if j == 1
                push!(args, Expression_Variable(var, arg.type));
            end
        end
        fj = transform(vj, args, transforms_j);
        push!(fs, fj);
    end
    return (fs...,);
end

# Creates a Function_Value from two FVs and an operation
# The arguments of the resultant function are a new set of variables in dt
function function_operation_helper(v1::ValueModule.Function_Value, o::OperatorModule.Operator, v2::ValueModule.Function_Value, dt::Definition_Table)
    (f1, f2) = transform(dt, v1, v2);
    return Function_Value(deepcopy(dt), f1.args, Binary_Expression(f1, o, f2), f1.return_type);
end

# Override operators for functions
function +(v1::ValueModule.Function_Value, v2::ValueModule.Function_Value, dt::Definition_Table)
    return function_operation_helper(v1, Operator.add, v2, dt);
end
function -(v1::ValueModule.Function_Value, v2::ValueModule.Function_Value, dt::Definition_Table)
    return function_operation_helper(v1, Operator.subtract, v2, dt);
end
function *(v1::ValueModule.Function_Value, v2::ValueModule.Function_Value, dt::Definition_Table)
    return function_operation_helper(v1, Operator.multiply, v2, dt);
end
function /(v1::ValueModule.Function_Value, v2::ValueModule.Function_Value, dt::Definition_Table)
    return function_operation_helper(v1, Operator.divide, v2, dt);
end

# Returns a Function_Value of the same type as v1 that simply returns v2
# Throws if v2 is a different type from the return type of v1
function num_to_fun(num::ValueModule.Number_Value{T}, fun::ValueModule.Function_Value) where {T <: Real}
    if num.type == fun.return_type
        return Function_Value(fun.dt, fun.args, num, fun.return_type);
    end
    throw(string("Cannot add ", num, " to output of ", fun));
end

# Override operators for functions and numbers
function +(v1::ValueModule.Function_Value, v2::ValueModule.Number_Value{T} where {T <: Real}, dt::Definition_Table)
    f2 = num_to_fun(v2, v1);
    return Function_Value(v1.dt, v1.args, Binary_Expression(v1, Operator.add, f2), v1.return_type);
end
function -(v1::ValueModule.Function_Value, v2::ValueModule.Number_Value{T} where {T <: Real}, dt::Definition_Table)
    f2 = num_to_fun(v2, v1);
    return Function_Value(v1.dt, v1.args, Binary_Expression(v1, Operator.subtract, f2), v1.return_type);
end
function *(v1::ValueModule.Function_Value, v2::ValueModule.Number_Value{T} where {T <: Real}, dt::Definition_Table)
    f2 = num_to_fun(v2, v1);
    return Function_Value(v1.dt, v1.args, Binary_Expression(v1, Operator.multiply, f2), v1.return_type);
end
function /(v1::ValueModule.Function_Value, v2::ValueModule.Number_Value{T} where {T <: Real}, dt::Definition_Table)
    f2 = num_to_fun(v2, v1);
    return Function_Value(v1.dt, v1.args, Binary_Expression(v1, Operator.divide, f2), v1.return_type);
end
+(v1::ValueModule.Number_Value{T}, v2::ValueModule.Function_Value, dt::Definition_Table) where {T <: Real} = +(v2, v1);
-(v1::ValueModule.Number_Value{T}, v2::ValueModule.Function_Value, dt::Definition_Table) where {T <: Real} = -(v2, v1);
*(v1::ValueModule.Number_Value{T}, v2::ValueModule.Function_Value, dt::Definition_Table) where {T <: Real} = *(v2, v1);
/(v1::ValueModule.Number_Value{T}, v2::ValueModule.Function_Value, dt::Definition_Table) where {T <: Real} = /(v2, v1);

# Evaluate a null expression
evaluate(null::ExpressionModule.Null_Expression, table::Definition_Table) = Null_Value();
# Transform a Null_Expression
transform(n::ExpressionModule.Null_Expression, transforms::Transformation_Table) = n;

# Evaluate a unary expression
function evaluate(unary_exp::ExpressionModule.Unary_Expression, table::Definition_Table)
    return unary_exp.operator.operation(unary_exp.exp);
end

# Evaluate a binary expression
function evaluate(binary_exp::ExpressionModule.Binary_Expression, table::Definition_Table)
    left_val = evaluate(binary_exp.left, table);
    right_val = evaluate(binary_exp.right, table);
    return binary_exp.operator.operation(left_val, right_val, table);
end

# Transforms a Unary_Expression
function transform(u::ExpressionModule.Unary_Expression, transforms::Transformation_Table)
    exp = transform(u.exp, transforms);
    return Unary_Expression(u.operator, exp);
end

# TRansforms a Binary_Expression
function transform(b::ExpressionModule.Binary_Expression, transforms::Transformation_Table)
    left_exp = transform(b.left, transforms);
    right_exp = transform(b.right, transforms);
    return Binary_Expression(left_exp, b.operator, right_exp);
end

# Evaluates a Call expression
function evaluate(call::ExpressionModule.Call, table::Definition_Table)
    fn_val = evaluate(call.exp, table);
    if !is_callable(fn_val)
        throw("Cannot call a value of type: ", get_type(fn_val));
    end
    # The number of arguments supplied can be fewer than the number requied
    # The result in this case would be a partially applied function
    fn_args = get_args(fn_val);
    arg_types = get_arg_types(val_type);
    num_missing_args = length(arg_types) - length(call.arg_list);
    if num_missing_args >= 0
        transforms = Transformation_Table();
        for (i,arg_exp) in enumerate(call.arg_list)
            # Get the argument and the type of the called function
            fn_arg = fn_args[i];
            fn_arg_name = get_name(fn_arg);
            fn_arg_type = get_type(fn_arg);
            # If the function argument type does not match the input type, throw
            if fn_arg_type != Dynamic_Type() && get_type(arg_exp) != fn_arg_type
                throw(string("Type mismatch: ", arg_value, " is not a ", fn_arg_type));
            else
                add(transforms, fn_arg_name, arg_exp);
            end
        end
        body = get_body(fn_val);
        new_body = transform(body, transforms);
        # If all arguments have been supplied, then call the function
        if num_missing_args == 0
            # Evaluate body of function
            return evaluate(new_body, table);
        else
            # Otherwise, return a Function_Value
            dt = get_dt(fn_val);
            arg_vars = get_args(fn_val);
            return_type = get_return_type(fn_val);
            return Function_Value(dt, arg_vars[end-num_missing_args+1:end], new_body, return_type);
        end
    else
        throw(string(length(arg_types), " arguments expected, but ", length(call.arg_list), " provided."));
    end
end

# Transforms a Call Expression
function transform(c::ExpressionModule.Call, transforms::Transformation_Table)
    exp = transform(c.exp, transforms);
    args = c.args .|> x->transform(x, transforms);
    return Call(exp, args);
end

# Evaluates a let-binding
function evaluate(let_binding::ExpressionModule.Let_Binding, table::Definition_Table)
    add(table, let_binding.variable, let_binding.exp);
    evaluate(let_binding.rest, table);
end

# Trasforms a Let_Binding
function transform(lb::ExpressionModule.Let_Binding, transforms::Transformation_Table)
    var = transform(lb.variable, transforms);
    exp = transform(lb.exp, transforms);
    rest = transform(lb.rest, transforms);
    return Let_Binding(var, exp, rest);
end

# Transforms a Variable
function transform(v::Variable, transforms::Transformation_Table)
    v_name = get_name(v);
    if v_name ∈ transforms
        return Variable(get(v_name, transforms), v);
    else
        return v;
    end
end

as_bool(num::Number_Value{T} where {T <: Real})::Bool = get_underlying(num) != 0;
as_bool(bool::Bool_Value)::Bool = get_underlying(bool);

# Evaluates an if-statement
function evaluate(if_statement::If_Statement, table::Definition_Table)
    bool_val = evaluate(if_statement.condition, table);
    if as_bool(bool_val)
        return evaluate(if_statement.then_exp, table);
    else
        return evaluate(if_statement.else_exp, table);
    end
end

# Transforms an If_Statement
function transform(i_s::If_Statement, transforms::Transformation_Table)
    condition = transform(i_s.condition, transforms);
    then_exp = transform(i_s.then_exp, transforms);
    else_exp = transform(i_s.else_exp, transforms);
    return If_Statement(condition, then_exp, else_exp);
end

# Interprets a program
function interpret(prog::Array{Expression, 1})
    table = Definition_Table();
    out_values = [];
    for exp in prog
        push!(out_values, evaluate(exp, table));
    end
    return out_values;
end

end
