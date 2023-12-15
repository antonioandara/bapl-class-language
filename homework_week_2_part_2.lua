local lpeg = require "lpeg"
local pt = require "pt"

--------------------------------------------------------------------------------

local function node (num)
    return {tag = "number", val = tonumber(num)}
end

-- match empty spaces
local space = lpeg.S(" \n\t")^0

-- match an integer in base 10
local int = lpeg.R("09")^1

-- match a hex number
local hex = ("0" * lpeg.S("xX") * (lpeg.R("09") + lpeg.R("af", "AF"))^1)

-- match a float point number
local float = ((lpeg.R("09")^1)^-1 * "." * lpeg.R("09")^1)

-- match a scientific notation number
local sci = float * lpeg.S("eE") * lpeg.P("-")^-1 * lpeg.R("09")^1

-- match any of the previously defined numbers
-- adding negative unary operator
local numeral = (lpeg.P("-")^-1 * (float + sci + hex + int)) / node * space

-- operator definitions
local opA = lpeg.C(lpeg.S("+-")) * space
local opM = lpeg.C(lpeg.S("*/%")) * space
local opE = lpeg.C(lpeg.S("^")) * space
local opC = lpeg.C(lpeg.P("<=") + ">=" + "<" +">" + "==" + "!=") * space


local function foldBin(lst)
    local tree = lst[1]
    for i=2, #lst, 2 do
        tree = {tag = "binop", e1=tree, op = lst[i], e2=lst[i+1]}
    end
    return tree
end

local pow = lpeg.Ct(numeral * (opE * numeral)^0) / foldBin
local term = lpeg.Ct(pow * (opM * pow)^0) / foldBin
local exp = lpeg.Ct(term * ((opC + opA) * term)^0) / foldBin

local function parse (input)
    return exp:match(input)
end
--------------------------------------------------------------------------------

local function addCode(state, op)
    local code = state.code
    code[#code + 1] = op
end


local ops = {["+"]="add",  ["-"]="sub",
             ["*"]="mul",  ["/"]="div",
             ["%"]="rem",  ["^"]="pow",
             ["<"]="lt",   [">"]="gt",
             [">="]="gte", ["<="]="lte",
             ["=="]="eq",  ["!="]="neq"}

local function codeExp(state, ast)
    if ast.tag == "number" then
        addCode(state, "push")
        addCode(state, ast.val)
    elseif ast.tag == "binop" then
        codeExp(state, ast.e1)
        codeExp(state, ast.e2)
        addCode(state, ops[ast.op])
    else error("invalid tree")
    end
end

local function compile(ast)
    local state = {code = {}}
    codeExp(state, ast)
    return state.code
end
--------------------------------------------------------------------------------

local function run (code, stack)
    local pc = 1
    local top = 0
    while pc <= #code do
        if code[pc] == "push" then
            pc = pc + 1
            top = top + 1
            stack[top] = code[pc]
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
        else error("unknown instrucion")
        end
        pc = pc + 1
    end
end

local input = io.read("a")
local ast = parse(input)
print(pt.pt(ast))
local code = compile(ast)
print(pt.pt(code))
local stack = {}
run(code, stack)
print(stack[1])
