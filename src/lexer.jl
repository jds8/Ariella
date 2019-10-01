module LexerModule

include("./operators.jl")
include("./values.jl")
include("./abstractexpressions.jl")

using DelimitedFiles
using .OperatorModule
using .ValueModule
using .AbstractExpressionsModule

import .ValueModule.Expression;

export Class, Token, Tokenizer, Operator_Class, Operator, Expression, Matchable_Expression;
export getOperatorStrings, addToken!, lex, is_valid;

@enum Class op=1 var=2 number=3 boolean=4 kw=5 punc=6 eof=7

struct Token <: Matchable_Expression
    class::Class
    value::String
end

# Tokenizer that keeps track of tokens, operators, and punctuation
mutable struct Tokenizer
    tokens::Dict{String,Class}
    opAndPunc::Dict{String,Class}
    Tokenizer() = new(Dict{String, Class}(), Dict{String,Class}())
end

# Gets all of the Operators from the Tokenizer
function getOperatorStrings(t::Tokenizer)
    return [k for (k,v) in t.opAndPunc if v == op::Class];
end

# Add token to tokenizer
function addToken!(t::Tokenizer, tokenVal::String, tokenClass::Class)
    t.tokens[tokenVal] = tokenClass;
    if tokenClass == op::Class || tokenClass == punc::Class
        t.opAndPunc[tokenVal] = tokenClass;
    end
end

# Add operators to tokenizer
function addToken!(t::Tokenizer, ops::Array{Operator,1})
    for operator in ops
        addToken!(t, operator.value, op::Class);
    end
end

# Read file
read(file::String) = readdlm(file, String);

# returns whether the token is a number
# precondition, length(token) >= startIndex
function isNumber(token::String)
    # match an integer or a float
    if occursin(r"^(?=.)([+-]?([0-9]*)(\.([0-9]+))?)$", token)
        return true;
    # if it did not match an integer or a float but it does contain a number, throw
    elseif occursin(r"\d", token)
        throw(string("Invalid syntax: ", token));
    # otherwise, it is not a number
    else
        return false;
    end
end

# Determines if the input is a boolean
function isBoolean(token::String)
    return token == "true" || token == "false";
end

# Returns the longest key in dict that occurs in str starting at
# getNumberStartingAtIndex. Also returns the index where key is
# Returns the empty string along with startIndex if no key occurs in str along
function findKeyFromDictInStr(str::String, dict::Dict{String,Class}, startIndex::Int64)
    currentIndex = startIndex
    while currentIndex <= length(str)
        # longestKey is the longest key that mateches a substring of str
        longestKey = ""
        for key in keys(dict)
            key_len = length(key)
            endIndex = currentIndex+key_len-1
            # if we match a key, then add that key as a token and look
            # at the next part of the input string
            if length(str[currentIndex:end]) >= key_len &&
               str[currentIndex:endIndex] == key &&
               key_len > length(longestKey)
                longestKey = key
            end
        end
        if !isempty(longestKey)
            return (longestKey, currentIndex)
        end
        currentIndex += 1
    end
    return ("", startIndex)
end

function getTokens(t::Tokenizer, str::String)
    output = Array{Token, 1}()
    # Find the first occurrence of an operator or punctuation
    (key, index) = findKeyFromDictInStr(str, t.opAndPunc, 1)
    # If no token operator or punctuation was found, then add str as a number
    # or variable
    if isempty(key)
        if isNumber(str)
            push!(output, Token(number::Class, str))
        elseif isBoolean(str)
            push!(output, Token(boolean::Class, str))
        elseif !isempty(str)
            push!(output, Token(var::Class, str))
        end
    # Otherwise look at the substring up to index
    else
        previousSubStr = str[1:index-1]
        # If the previous substring is not empty, then check if it's a keyword
        if !isempty(previousSubStr)
            # If the previous substring is a token, then add it
            if haskey(t.tokens, previousSubStr)
                push!(output, Token(t.tokens[previousSubStr], previousSubStr));
            # Otherwise, check if it's a number
            elseif isNumber(previousSubStr)
                push!(output, Token(number::Class, previousSubStr));
            # Check if it's a boolean
            elseif isBoolean(previousSubStr)
                push!(output, Token(boolean::Class, previousSubStr));
            # If not, add it as a variable
            else
                push!(output, Token(var::Class, previousSubStr));
            end
        end
        # Add key as a punctuation token
        push!(output, Token(t.tokens[key], key))
        # Call this function again on the rest of the string
        startOfRest = index+length(key)
        restOfStr = str[startOfRest:end]
        push!(output, getTokens(t, restOfStr)...)
    end
    return output
end

# Tokenize a string without whitespaces
function tokenize(t::Tokenizer, str::String)
    output = Array{Token, 1}()
    # If the input string is a known token, push
    if haskey(t.tokens, str)
        push!(output, Token(t.tokens[str], str))
    else
        tokens = getTokens(t, str)
        push!(output, tokens...)
    end
    return output
end

# Tokenize a list of strings
function tokenize(t::Tokenizer, lines::Array{String, 2})
    (row, col) = size(lines)
    output = Array{Token, 1}()
    sizehint!(output, row*col)
    for r in 1:row
        for c in 1:col
            str = lines[r, c]
            # empty strings and comments
            if isempty(str) || str[1] == '#'
                break
            end
            tokens = tokenize(t, str)
            push!(output, tokens...)
        end
    end
    return output
end

function lex(t::Tokenizer, file::String)
    lines = read(file)
    return tokenize(t, lines)
end

end
