include("./lexer.jl");
include("./parser.jl");
include("./interpreter.jl");
include("./operators.jl");
include("./values.jl");
include("./types.jl");

using .LexerModule
using .ParserModule
using .InterpreterModule
using .OperatorModule
using .ValueModule
using .TypesModule

import .LexerModule.op
import .LexerModule.kw
import .LexerModule.punc
import .LexerModule.assign
import .LexerModule.binary

# Create Tokenizer
t = Tokenizer();

# Create operators
ops = Operator[];
push!(ops, eqeq);
push!(ops, gt);
push!(ops, ge);
push!(ops, lt);
push!(ops, le);
push!(ops, add);
push!(ops, subtract);
push!(ops, multiply);
push!(ops, divide);
push!(ops, not);
push!(ops, and);
push!(ops, or);
push!(ops, cons);

# Add operator tokens
addToken!(t, ops);

# Add keyword tokens
addToken!(t, "match", kw);
addToken!(t, "with", kw);
addToken!(t, "if", kw);
addToken!(t, "then", kw);
addToken!(t, "else", kw);
addToken!(t, "let", kw);
addToken!(t, "vector", kw);
addToken!(t, "int", kw);
addToken!(t, "float", kw);
addToken!(t, "bool", kw);
addToken!(t, "in", kw);
addToken!(t, "true", kw);
addToken!(t, "false", kw);

# Add punctuation token
addToken!(t, "(", punc);
addToken!(t, ")", punc);
addToken!(t, "{", punc);
addToken!(t, "}", punc);
addToken!(t, ":", punc);
addToken!(t, ",", punc);
addToken!(t, "|", punc);
addToken!(t, "[", punc);
addToken!(t, "]", punc);
addToken!(t, "[]", punc);
addToken!(t, "->", punc);

# Tokenize the file
tokens = lex(t, "code.txt");

# Get types
types = String["int", "float"];

# Parse the tokens given the operators
prog = ParserModule.parse(tokens, ops, types);

#Interpreter.interpret(prog);
Interpreter.evaluate(prog[1].rest.exp)
