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

function evaluate(t::Token)
    if t.class == number::Class
        if occursin(".", t.value)
            return parse(Float64, t.value);
        else
            return parse(Int64, t.value);
        end
    end
    throw(string("Cannot evaluate token of class ", t.class));
end

function evaluate(binary_exp::Binary_Expression)
    left_val = evaluate(binary_exp.left);
    right_val = evaluate(binary_exp.right);
    return binary_exp.operator.operation(left_val, right_val);
end

function interpret(prog::Array{Expression, 1})
    for exp in prog
        evaluate(exp);
    end
end

end
