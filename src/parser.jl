module ParserModule

import Base.parse;

export Parser, parse, next, parse_type_annotation;
export Variable_Type, Dynamic_Type, Concrete_Type, Primitive_Type, Numeric_Type;
export Int_Type, Float_Type, Function_Type;
export Null_Expression, If_Statement, Binary_Expression, Call, Let_Binding;
export Abstract_Cons_Expression, Cons_Expression, Empty_Cons_Expression;
export Pattern_Match;

using Main.LexerModule

import Main.LexerModule.op
import Main.LexerModule.kw
import Main.LexerModule.punc
import Main.LexerModule.var
import Main.LexerModule.number
import Main.LexerModule.eof
import Main.LexerModule.assign
import Main.LexerModule.binary

mutable struct Parser
    tokens::Array{Token,1}
    operatorMap::Dict{String, Operator}
    current::Int64
end

function Parser(tokens::Array{Token,1}, operators::Array{Operator,1}, types::Array{String,1})
    operatorMap = Dict([op.value=>op for op in operators]);
    return Parser(tokens, operatorMap, 0);
end

function get_operator(p::Parser, tok::Token)
    if haskey(p.operatorMap, tok.value)
        return p.operatorMap[tok.value];
    else
        return Operator(tok.value);
    end
end

function is_type(p::Parser, type::String)
    return hasskey(p.baseTypeMap, type);
end

struct Null_Expression <: Expression end;

struct If_Statement <: Expression
    condition::Expression
    then_exp::Expression
    else_exp::Expression
    If_Statement(cond::Expression, then_exp::Expression) = new(cond, then_exp, Null_Expression())
    If_Statement(cond::Expression, then_exp::Expression, else_exp::Expression) = new(cond, then_exp, else_exp)
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

# Define types for variables
abstract type Variable_Type end;
struct Dynamic_Type <: Variable_Type end;

# Define concrete types of variables
abstract type Concrete_Type <: Variable_Type end;
abstract type Primitive_Type <: Concrete_Type end;
abstract type Numeric_Type <: Primitive_Type end;
struct Float_Type <: Numeric_Type end;
struct Int_Type <: Numeric_Type end;
struct Bool_Type <: Primitive_Type end;

# Define function types for functions
struct Function_Type <: Concrete_Type
    arg_types::Array{Variable_Type,1};
    return_type::Variable_Type;
end

# Define equality on Function_Types
import Base.==
function ==(x::Function_Type, y::Function_Type)
    return x.arg_types == y.arg_types && x.return_type == y.return_type
end

# Gets a Variable_Type object from an input string
function get_type(type_str::String)
    if type_str == "int"
        return Int_Type();
    elseif type_str == "float"
        return Float_Type();
    elseif type_str == "vector"
        throw("Vector not yet implemented");
    else
        throw(string("Invalid type: ", type_str));
    end
end

abstract type Variable end;

struct Expression_Variable <: Variable
    name::String
    type::Variable_Type
    Expression_Variable(name::String, type::Variable_Type) = new(name, type);
    Expression_Variable(name::String) = new(name, Dynamic_Type());
end

struct Function_Variable <: Variable
    name::String
    args::Array{Expression_Variable,1}
    return_type::Variable_Type
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

# Returns whether the parser is at the end of the file
function is_at_end(p::Parser)
    return p.current + 1 > length(p.tokens);
end

# Reads the next token of the Parser
# Throws if there are no more tokens to read
function next(p::Parser)
    p.current += 1;
    if p.current <= length(p.tokens)
        return p.tokens[p.current];
    else
        throw("Trying to read past EOF");
    end
end

# Skips a symbol if it's next; otherwise throws
function skip_sym(p::Parser, symbol::String)
    nextToken = next(p);
    if nextToken.value != symbol
        throw(string("Next token is `", nextToken.value, "` not `", symbol, "`"));
    end
end

# Parses a binary expression
function maybe_binary(p::Parser, left::Expression, left_precedence::Int64)
    op = next_op(p);
    if is_valid(op) && op.precedence > left_precedence
        skip_sym(p, op.value);
        right = maybe_binary(p, parse_atom(p), op.precedence);
        bin_exp = Binary_Expression(left, op, right);
        return maybe_binary(p, bin_exp, left_precedence);
    else
        return left;
    end
end

# parses a call to a variable
function parse_call(p::Parser, exp::Expression)
    skip_sym(p, "(");
    arg_list = Array{Expression, 1}();
    read_more = false;
    while !is_next(p, ")")
        arg = parse_expression(p);
        push!(arg_list, arg);
        read_more = is_next(p, ",");
        if read_more
            skip_sym(p, ",");
        end
    end
    if read_more
        throw("Argument not supplied for call");
    end
    skip_sym(p, ")");
    return Call(exp, arg_list);
end

function maybe_call(p::Parser, expr_fun::Function)
    exp = expr_fun();
    return is_next(p, "(") ? parse_call(p, exp) : exp;
end

# Parses an expression
function parse_expression(p::Parser)
    return maybe_call(p, function()
        return maybe_binary(p, parse_atom(p), 0);
    end);
end

# Parses a type annotation on a variable
function parse_type_annotation_helper(p::Parser, types::Array{Variable_Type,1})
    if is_next(p, "(")
        skip_sym(p, "(");
        returnTypes = parse_type_annotation_helper(p, types);
        skip_sym(p, ")");
        return returnTypes;
    else
        type_tok = next(p);
        type = get_type(type_tok.value);
        push!(types, type);
        if is_next(p, "->")
            skip_sym(p, "->");
            return parse_type_annotation_helper(p, types);
        else
            return types;
        end
    end
end

function parse_type_annotation(p::Parser)
    typeList = parse_type_annotation_helper(p, Variable_Type[]);
    # If there is only one type in the list, then it's an Expression_Type
    if length(typeList) == 1
        return typeList[1];
    # otherwise, there are more than one in the list, so it's a Function_Type
    else
        return Function_Type(typeList[1:end-1], typeList[end]);
    end
end

# Parses a function binding within a let binding
function parse_function_binding(p::Parser, varName::String)
    # skip "(" symbol
    skip_sym(p, "(");
    # collect function arguments
    args = Expression_Variable[];
    read_more = false;
    while !is_next(p, ")")
        # get variable name
        varToken = next(p);
        # assume it is dynamically typed
        argType = Dynamic_Type();
        # if it has a type annotation, then parse it
        if is_next(p, ":")
            skip_sym(p, ":");
            argType = parse_type_annotation(p);
        end
        # create an Expression_Variable and add it to args list
        arg = Expression_Variable(varToken.value, argType);
        push!(args, arg);
        # read more arguments if available
        read_more = is_next(p, ",");
        if read_more
            skip_sym(p, ",");
        end
    end
    if read_more
        throw("Argument not supplied for call");
    end
    # read closing paren
    skip_sym(p, ")");
    # assume return type is dynamic
    returnType = Dynamic_Type();
    # if there is another type annotation, then it is the return type
    if is_next(p, ":")
        skip_sym(p, ":");
        returnType = parse_type_annotation(p);
    end
    return Function_Variable(varName, args, returnType);
end

# Parses a variable binding within a let binding
function parse_expr_binding(p::Parser, varName::String)
    type = Dynamic_Type();
    if is_next(p, ":")
        skip_sym(p, ":");
        type = parse_type_annotation(p);
    end
    return Expression_Variable(varName, type);
end

function parse_rhs(p::Parser, variable::Variable)
    if is_next(p, "=")
        skip_sym(p, "=");
        return parse_expression(p);
    else
        throw("Invalid syntax for 'let binding' a variable");
    end
end

function parse_variable_binding(p::Parser, varName::String)
    # if the next token is a parenthesis, then the let binding defines a fn
    # otherwise, it defines a variable declaration
    isFunctionBind = is_next(p, "(");
    return isFunctionBind ? parse_function_binding(p, varName) : parse_expr_binding(p, varName);
end

# Parses a let binding variable
function parse_let_binding(p::Parser)
    # the variable name must come after `let`
    skip_sym(p, "let");
    var_token = next(p);
    if var_token.class != var::Class
        throw(string("Invalid syntax: `let ", var_token.value, "`"));
    else
        var_binding = parse_variable_binding(p, var_token.value);
        expr = parse_rhs(p, var_binding);
        # the next token can be the keyword `in` to continue the current program
        if is_next(p, "in")
            skip_sym(p, "in");
            rest_of_program = parse_expression(p);
            return Let_Binding(var_binding, expr, rest_of_program);
        else
            return Let_Binding(var_binding, expr);
        end
    end
end

# Parses an atoms
function parse_atom(p::Parser)
    return maybe_call(p, function()
        if is_next(p, "(")
            skip_sym(p, "(");
            exp = parse_expression(p);
            skip_sym(p, ")");
            return exp;
        elseif is_next(p, "let") return parse_let_binding(p);
        elseif is_next(p, "if") return parse_if(p);
        #elseif is_next(p, "true") || is_next(p, "false")) return parse_bool(p);
        elseif is_next(p, "match") return parse_match(p);
        else
            tok = next(p);
            if tok.class == var::Class || tok.class == number::Class || tok.value == "[]"
                return tok;
            end
            throw(string("Unknown atom: ", tok.value));
        end
    end);
end

# Parses and if statement
function parse_if(p::Parser)
    skip_sym(p, "if");
    cond = parse_expression(p);
    skip_sym(p, "then");
    then_exp = parse_expression(p);
    if is_kw(p, "else")
        skip_sym(p, "else");
        else_exp = parse_expression(p);
        return If_Statement(cond, then_exp, else_exp)
    else
        return If_Statement(cond, then_exp);
    end
end

# Determines if the input token is matchable
function is_matchable(tok::Token)
    if tok.class == var::Class || tok.class == number::Class
        return true;
    elseif tok.value == "[]"# || tok.value == "("
        return true;
    else
        return false;
    end
end

# Parses a cons expression
function parse_cons_exp(p::Parser)
    if is_next(p, "::")
        skip_sym(p, "::");
        tok = next(p);
        tail_cons = parse_cons_exp(p);
        return Cons_Expression(tok.value, tail_cons);
    else
        return Empty_Cons_Expression();
    end
end

# Parses a matchable expression
function parse_matchable_exp(p::Parser)
    tok = next(p);
    if is_matchable(tok);
        if is_next(p, "::")
            tail_cons = parse_cons_exp(p);
            return Cons_Expression(tok.value, tail_cons);
        #elseif tok.value == "("
        #    return parse_pair(p, tok)
        else
            return tok;
        end
    end
    throw(string("Cannot match on tokens that are neither variables nor numbers: ", tok.value));
end

# Parses a pattern match
function parse_match(p::Parser)
    skip_sym(p, "match");
    tok = next(p);
    # Can only match on variables and numbers
    if tok.class != var::Class && tok.class != number::Class
        throw(string(tok.value, " is not matchable."));
    end
    skip_sym(p, "with");
    # Look for pipes for clauses
    clauses = Array{Pair{Matchable_Expression, Expression}, 1}();
    while is_next(p, "|")
        skip_sym(p, "|");
        matchable_exp = parse_matchable_exp(p);
        skip_sym(p, "->");
        return_exp = parse_atom(p);
        push!(clauses, Pair(matchable_exp, return_exp));
    end
    return Pattern_Match(Expression_Variable(tok.value), clauses);
end

# Peeks at the next tokens
function peek_next(p::Parser)
    if p.current+1 <= length(p.tokens)
        return p.tokens[p.current+1]
    else
        return Token(eof::Class, "EOF");
    end
end

# Returns false if there is no more to read or if the next token is not kw
function is_next(p::Parser, tokenValue::String)
    return p.current+1 <= length(p.tokens) && p.tokens[p.current+1].value == tokenValue
end

# Gets the next operator or a null operator if an operator is not the next token
function next_op(p::Parser)
    tok = peek_next(p);
    return get_operator(p, tok);
end

function parse(tokens::Array{Token,1}, operators::Array{Operator,1}, types::Array{String,1})
    p = Parser(tokens, operators, types);
    prog = Expression[];
    while (!is_at_end(p))
        push!(prog, parse_expression(p));
    end
    return prog;
end

end
