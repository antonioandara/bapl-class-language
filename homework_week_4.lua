local lpeg = require "lpeg"
local pt = require "pt"

--------------------------------------------------------------------------------
function I (msg)
    return lpeg.P(function () print(msg); return true end)
end
--------------------------------------------------------------------------------

local function nodeNum (num)
    return {tag = "number", val = tonumber(num)}
end

local function nodeVar (var)
    return {tag = "variable", var = var}
end

local function nodeAssgn (id, exp)
    return {tag = "assignment", id = id, exp = exp}
end

local function nodeSeq (st1, st2)
    if st2 == nil then
        return st1
    else
        return {tag = "sequence", st1 = st1, st2 = st2}
    end
end

local function nodeRet (exp)
    return {tag = "return", exp = exp}
end

local function nodePrint (exp)
    return {tag = "print", exp = exp}
end

local alpha = lpeg.R("AZ","az")
local digit = lpeg.R("09")
local alphanum = alpha + digit
local idChar = lpeg.S("_@")

-- comments
local comment = "#" * (lpeg.P(1) - "\n")^0
local blockComment = "#{" * (lpeg.P(1) - "#}")^0


local maxmatch = 0
local maxline = 1
-- match empty spaces
local space = lpeg.V"space"

-- match an integer in base 10
local int = lpeg.R("09")^1

-- match a hex number
local hex = ("0" * lpeg.S("xX") * (lpeg.R("09") + lpeg.R("af", "AF"))^1)

-- match a float point number
local float = ((lpeg.R("09")^1)^-1 * "." * lpeg.R("09")^1)

-- match a scientific notation number
local sci = float * lpeg.S("eE") * lpeg.P("-")^-1 * lpeg.R("09")^1

-- match any of the previously defined numbers
local numeral = space * ( (float + sci + hex + int)) / nodeNum * space

local function T(t)
    return t * space
end

local reserved = {"return", "if"}
local excluded = lpeg.P(false)

for i=1, #reserved do
    excluded = excluded + reserved[i]
end
excluded = excluded * -alphanum

local function Rw(t)
    assert(excluded:match(t))
    return t * -alphanum * space
end

local function checkReserved(subject)
    local firstWord = subject:match("^%S+")
    for _,v in pairs(reserved) do
        if v == firstWord then
            return nil
        end
    end
    return true
end

-- operator definitions
local ID = lpeg.C(alpha * (alphanum + idChar)^0) * lpeg.P(checkReserved) * space
local var = ID / nodeVar
local prin = "@" * space
local opNeg = lpeg.C(lpeg.P("-")) * space -- unary operator
local opA = lpeg.C(lpeg.S("+-")) * space  -- additive operator
local opM = lpeg.C(lpeg.S("*/%")) * space -- multiplicative operator
local opExp = lpeg.C(lpeg.S("^")) * space -- exponent operator
local opC = lpeg.C(lpeg.P("<=") + ">=" + "<" +">" + "==" + "!=") * space


local function foldBin(lst)
    local tree = lst[1]
    for i=2, #lst, 2 do
        tree = {tag = "binop", e1=tree, op = lst[i], e2=lst[i+1]}
    end
    return tree
end

local function foldUnary(lst)
    local tree = {tag = "unary", op = lst[1], e2=lst[2]}
    return tree
end

local neg = lpeg.V"neg"
local prim = lpeg.V"prim"
local factor = lpeg.V"factor"
local term = lpeg.V"term"
local expression = lpeg.V"expression"
local statement = lpeg.V"statement"
local statements = lpeg.V"statements"
local block = lpeg.V"block"

local grammar = lpeg.P{"program",
program = space * statements * -1,
statements = ((T"{" * space * T"}") + statement)
           * (T";"^1 * statements)^-1 / nodeSeq,
block = T"{" * statements * T";"^-1 * T"}",
statement = block
          + ID * T"=" * expression / nodeAssgn
          + Rw"return" * expression / nodeRet
          + prin * expression / nodePrint,
prim = (numeral 
     + (T"(" * expression * T")")
     + var),
neg = lpeg.Ct(opNeg * (prim))/ foldUnary + prim,
factor = lpeg.Ct(neg * (opExp * neg)^0) / foldBin,
term = lpeg.Ct(factor * (opM * factor)^0) / foldBin,
expression = lpeg.Ct(term * ((opC + opA) * term)^0) / foldBin,
space = (lpeg.S(" \n\t")
        + blockComment
        + comment)^0
            * lpeg.P(function (s, p)
                    maxmatch =  p;
                    if string.sub(s,p-1,p-1)== '\n' then
                        maxline = maxline + 1
                    end
                    return true
                    end)
}

local function syntaxError(input, max)
    io.stderr:write("syntax error on line: ", maxline, "\n")
    io.stderr:write(string.sub(input, max - 10, max - 1),
        "|", string.sub(input, max, max + 11), "\n")
    end

local function parse (input)
    local result = grammar:match(input)
    if not result then
        syntaxError(input, maxmatch)
        os.exit(1)
    end
    -- print("maxmatch: " .. maxmatch)
    return result
end
--------------------------------------------------------------------------------
local Compiler = {code = {}, vars ={}, nvars =0}

function Compiler:addCode( op)
    local code = self.code
    code[#code + 1] = op
end


local ops = {["+"]="add",  ["-"]="sub",
             ["*"]="mul",  ["/"]="div",
             ["%"]="rem",  ["^"]="pow",
             ["<"]="lt",   [">"]="gt",
             [">="]="gte", ["<="]="lte",
             ["=="]="eq",  ["!="]="neq"
            }

function Compiler:var2num (id)
    local num = self.vars[id]
    if not num then
        num = self.nvars + 1
        self.nvars = num
        self.vars[id] = num
    end
    return num
end

function Compiler:codeExp(ast)
    if ast.tag == "number" then
        self:addCode("push")
        self:addCode(ast.val)
    elseif ast.tag == "variable" then
        self:addCode("load")
        self:addCode(self:var2num(ast.var))
    elseif ast.tag == "binop" then
        self:codeExp(ast.e1)
        self:codeExp(ast.e2)
        self:addCode(ops[ast.op])
    elseif ast.tag == "unary" then
        self:codeExp(ast.e2)
        self:addCode("neg")
    else error("invalid tree")
    end
end

function Compiler:codeStat(ast) 
    if ast.tag == "assignment" then
        self:codeExp(ast.exp)
        self:addCode("store")
        self:addCode(self:var2num(ast.id))
    elseif ast.tag == "sequence" then
        self:codeStat(ast.st1)
        self:codeStat(ast.st2)
    elseif ast.tag == "return" then
        self:codeExp(ast.exp)
        self:addCode("ret")
    elseif ast.tag == "print" then
        self:codeExp(ast.exp)
        self:addCode("print")
    else error("invalid tree")
    end
end

local function compile(ast)
    Compiler:codeStat(ast)
    Compiler:addCode("push")
    Compiler:addCode("0")
    Compiler:addCode("ret")
    return Compiler.code
end

--------------------------------------------------------------------------------
local function run (code, mem, stack)
    local pc = 1
    local top = 0
    while true do
    --[[
    io.write("-->")
    for i = 1, top do io.write(stack[i], " ") end
    io.write("\n", code[pc], "\n")
    --]]
        if code[pc] == "ret" then
            return
        elseif code[pc] == "push" then
            pc = pc + 1
            top = top + 1
            stack[top] = code[pc]
        elseif code[pc] == "neg" then
            stack[top] = -stack[top]
        elseif code[pc] == "add" then
            stack[top - 1] = stack[top-1] + stack[top]
            top = top - 1
        elseif code[pc] == "sub" then
            stack[top - 1] = stack[top-1] - stack[top]
            top = top - 1
        elseif code[pc] == "mul" then
            stack[top - 1] = stack[top-1] * stack[top]
            top = top - 1
        elseif code[pc] == "div" then
            stack[top - 1] = stack[top-1] / stack[top]
            top = top - 1
        elseif code[pc] == "rem" then
            stack[top - 1] = stack[top-1] % stack[top]
            top = top - 1
        elseif code[pc] == "pow" then
            stack[top - 1] = stack[top-1] ^ stack[top]
            top = top - 1
        elseif code[pc] == "gt" then
            if stack[top-1] > stack[top] then
                stack[top - 1] = 1
            else
                stack[top - 1] = 0
            end
            top = top - 1
        elseif code[pc] == "lt" then
            if stack[top-1] < stack[top] then
                stack[top - 1] = 1
            else
                stack[top - 1] = 0
            end
            top = top - 1
        elseif code[pc] == "gte" then
            if stack[top-1] >= stack[top] then
                stack[top - 1] = 1
            else
                stack[top - 1] = 0
            end
            top = top - 1
        elseif code[pc] == "lte" then
            if stack[top-1] <= stack[top] then
                stack[top - 1] = 1
            else
                stack[top - 1] = 0
            end
            top = top - 1
        elseif code[pc] == "eq" then
            if stack[top-1] == stack[top] then
                stack[top - 1] = 1
            else
                stack[top - 1] = 0
            end
            top = top - 1
        elseif code[pc] == "neq" then
            if stack[top-1] ~= stack[top] then
                stack[top - 1] = 1
            else
                stack[top - 1] = 0
            end
            top = top - 1
        elseif code[pc] == "load" then
            pc = pc + 1
            local id = code[pc]
            -- checking undeclared variables
            if mem[id] then
                top = top + 1
                stack[top] = mem[id]
            else
                error("variable not defined")
            end
        elseif code[pc] == "store" then
            pc = pc + 1
            local id = code[pc]
            mem[id] = stack[top]
            top = top - 1
        elseif code[pc] == "print" then
            print(stack[top])
        else error("unknown instrucion")
        end
        pc = pc + 1
    end
end


--------------------------------------------------------------------------------
local input = io.read("a") -- read input
local ast = parse(input) -- parse input into an ast
print(pt.pt(ast))
local code = compile(ast) -- compile ast into code
print(pt.pt(code))
local stack = {}
local mem = {k0 = 0, k1 = 1, k10 = 10}
run(code, mem, stack) -- execute code
print(stack[1])
-- print(mem.result)
