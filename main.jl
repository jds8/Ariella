include("./lexer.jl");
include("./parser.jl");

using Main.LexerModule
using Main.ParserModule

import Main.LexerModule.op
import Main.LexerModule.kw
import Main.LexerModule.punc
import Main.LexerModule.assign
import Main.LexerModule.binary

# Create Tokenizer
t = Tokenizer();

# Add operator tokens
addToken!(t, "=", op);
addToken!(t, "+", op);
addToken!(t, "-", op);
addToken!(t, "*", op);
addToken!(t, "/", op);
addToken!(t, "::", op);

# Create operators
ops = Operator[];
push!(ops, Operator("=", 1, assign));
push!(ops, Operator("+", 10, binary));
push!(ops, Operator("-", 10, binary));
push!(ops, Operator("*", 20, binary));
push!(ops, Operator("/", 20, binary));
push!(ops, Operator("::", 30, binary));

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
prog = ParserModule.parse(tokens, ops, types)
