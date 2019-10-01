module InterpreterModule

include("./abstractexpressions.jl");
include("./expression.jl");
include("./variables.jl");
include("./lexer.jl");
include("./values.jl");
include("./types.jl");
include("./operators.jl");

using .AbstractExpressionsModule;
using .ExpressionModule;
using .VariableModule;
using .LexerModule;
using .ValueModule;
using .TypesModule;
using .OperatorModule;

import .LexerModule.op
import .LexerModule.kw
import .LexerModule.punc
import .LexerModule.var
import .LexerModule.number
import .LexerModule.eof
import .LexerModule.assign
import .LexerModule.binary

export interpret, evaluate;

# The string var.name is mapped to this struct via a Definition_Table
# The variable var is bound to the expression exp for the first time in
# the Definition_Table at index, index.
struct Variable_Binding{T <: Variable}
    index::Int64
    var::T
    exp::Expression
end

# Definition_Table is for storing variable name->Value pairs
struct Definition_Table
    maps::Array{Dict{String,Variable_Binding},1}
end

# Creates a Definition_Table using the first index Dicts from table
Definition_Table(table::Definition_Table, index::Int64) = Definition_Table(table.maps[1:index]);

# Gets the keys of the last dictionary in dt.maps
keys(dt::Definition_Table) = keys(dt.maps[end]);

# Determines if the name is in the table
∈(name::String, dt::Definition_Table) = name in keys(dt);

# A table containing transformations from one token to another
struct Transformation_Table
    map::Dict{Token,Expression}
    Transformation_Table(map::Dict{Token,Expression}) = new(map);
    Transformation_Table() = new(Dict{Token,Expression}());
end

# Determines if the Expression is in the table
#∈(t::Token, transforms::Transformation_Table) = t in transforms.map.keys;

# Transforms exp into another expression via the transforms table
#transform(transforms::Transformation_Table, exp::Expression) = exp ∈ transforms ? transforms.map[exp] : exp;

# Add a variable to the Definition_Table
function add!(table::Definition_Table, var::Variable, exp::Expression)
    dt = deepcopy(table.maps[end]);
    dt[var.name] = Variable_Binding(length(table.maps)+1, var, exp);
    push!(table.maps, dt);
end

# Retrieves a variable from the definition table
function get_value(table::Definition_Table, binding::Variable_Binding{Expression_Variable})
    index = binding.index;
    exp = binding.exp;
    dt = Definition_Table(table, index);
    return evaluate(exp, dt);
end

# Retrieves a function variable
function get_value(table::Definition_Table, binding::Variable_Binding{Function_Variable})
    return Function_Value(binding.var.args, binding.exp, get_type(binding.var));
end

# Accesses var.name in the indexth index of the table
function get(table::Definition_Table, var_name::String, index::Int64 = length(table.maps))
    return table.maps[index][var_name];
end

# Gets the value in the defintion table for this token
function get_value(table::Definition_Table, t::Token)
    if t.value ∈ table
        binding = get(table, t.value);
        return get_value(table, binding);
    end
    throw(string("Unknown free variable: ", var.name));
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
function +(v1::ValueModule.Number_Value{T}, v2::ValueModule.Number_Value{U}) where {T,U <: Real}
    return Number_Value(v1.value + v2.value, v1.type + v2.type);
end
function -(v1::ValueModule.Number_Value{T}, v2::ValueModule.Number_Value{U}) where {T,U <: Real}
    return Number_Value(v1.value - v2.value, v1.type - v2.type);
end
function *(v1::ValueModule.Number_Value{T}, v2::ValueModule.Number_Value{U}) where {T,U <: Real}
    return Number_Value(v1.value * v2.value, v1.type * v2.type);
end
function /(v1::ValueModule.Number_Value{T}, v2::ValueModule.Number_Value{U}) where {T,U <: Real}
    return Number_Value(v1.value / v2.value, v1.type / v2.type);
end

# Determines if we can operate on these two function variables
# Only works when the return type is numeric (for now)
function is_co_operable(f1::ValueModule.Function_Value, f2::ValueModule.Function_Value)
    return f1.type == f2.type && isa(f1.type.return_type, Numeric_Type)
end

# Transforms the ith local variable of the input function to be var
# Returns a new Function_Value
function transform(dt::Definition_Table, f::ValueModule.Function_Value,
                   args::Array{Expression_Variable,1}, transforms::Transformation_Table)
    transformed_body = transform(dt, f.body, transforms);
    return Function_Value(args, transformed_body, f.type);
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
            add(transforms_j, arg.name, var);
            # Create Expression_Variables out of these new variable names
            if j == 1
                push!(args, Expression_Variable(var, arg.type));
            end
        end
        fj = transform(dt, vj, args, transforms_j);
        push!(fs, fj);
    end
    return (fs...,);
end

# Creates a Function_Value from two FVs and an operation
# The arguments of the resultant function are a new set of variables in dt
function function_operation_helper(v1::ValueModule.Function_Value, o::OperatorModule.Operator, v2::ValueModule.Function_Value, dt::Definition_Table)
    (f1, f2) = transform(dt, v1, v2);
    return Function_Value(f1.args, Binary_Expression(f1, o, f2), f1.type);
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
function num_to_fun(num::Number_Value{T}, fun::Function_Value) where {T <: Real}
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
    return Function_Value(Binary_Expression(v1.exp, Operator("+"), v2), v1.type);
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

# An infix operator that applies fun on obj
→(obj, fun) = fun(obj);

# Returns a particular Value
make_value(num_val::T, type::Numeric_Type) = Number_Value(num_val);
make_value(bool_val::Bool, type::Bool_Type) = Bool_Value(bool_val);
make_value(fun_val::Expression, type::Function_Type) = Function_Value(fun_val, type);

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
    output_call = string(output_call, join(args.get_underlying, ","), ",");
    output_call = string(output_call, unsupplied_args);
    return output_call;
end

# Evaluates a Call expression
function evaluate(call::Call, table::Definition_Table)
    value = evaluate(call.exp, table);
    fn_value = get_underlying(value);
    fn_type = get_type(value);
    # The number of arguments supplied can be fewer than the number requied
    # The result in this case would be a partially applied function
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
            call_value = eval(Expr(:call, fn_value, (args.get_underlying)...));
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
    add!(table, let_binding.variable, let_binding.exp);
    evaluate(let_binding.rest, table);
end

as_bool(num::Number_Value{T} where {T <: Real})::Bool = get_underlying(num) != 0;
as_bool(bool::Bool_Value)::Bool = get_underlying(bool);

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
    out_values = [];
    for exp in prog
        push!(out_values, evaluate(exp, table));
    end
    return out_values;
end

end
