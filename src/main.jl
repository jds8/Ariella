include("./lexer.jl");
include("./parser.jl");
include("./interpreter.jl");

using Main.LexerModule
using Main.ParserModule
using Main.Interpreter

import Main.LexerModule.op
import Main.LexerModule.kw
import Main.LexerModule.punc
import Main.LexerModule.assign
import Main.LexerModule.binary

# Create Tokenizer
t = Tokenizer();

# Create operators
ops = Operator[];
push!(ops, Operator("+", 10, binary, +));
push!(ops, Operator("-", 10, binary, -));
push!(ops, Operator("*", 20, binary, *));
push!(ops, Operator("/", 20, binary, /));
push!(ops, Operator("<", 10, binary, <));
push!(ops, Operator("<=", 5, binary, <=));
push!(ops, Operator("==", 5, binary, ==));
push!(ops, Operator(">", 5, binary, >));
push!(ops, Operator(">=", 5, binary, >=));
# h::t is the cons operator which prepends elements h onto t in a new list
push!(ops, Operator("::", 30, binary, (h,t)->(h_copy=deepcopy(h);push!(hh, t...);)));

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
