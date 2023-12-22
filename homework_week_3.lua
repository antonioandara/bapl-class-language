local lpeg = require "lpeg"
local pt = require "pt"

--------------------------------------------------------------------------------

local function nodeNum (num)
    return {tag = "number", val = tonumber(num)}
end

local function nodeVar (var)
    return {tag = "variable", var = var}
end

local function nodeAssgn (id, exp)
    return {tag = "assgn", id = id, exp = exp}
end

local function nodeSeq (st1, st2)
    if st2 == nil then
        return st1
    else
        return {tag = "seq", st1 = st1, st2 = st2}
    end
end

local function nodeRet (exp)
    return {tag = "ret", exp = exp}
end

local function nodePrint (exp)
    return {tag = "print", exp = exp}
end

local alpha = lpeg.R("AZ", "az")
local underscore = lpeg.S("_")
local digit =  lpeg.R("09")
local alphanum = alpha + digit + underscore

-- match empty spaces
local space = lpeg.S(" \n\t")^0

-- match an integer in base 10
local numeral = lpeg.R("09")^1 / nodeNum * space

local Assgn = "=" * space
local SC = ";" * space

--adding underscore
local ID = lpeg.C((underscore + alpha) * alphanum^0) * space
local var = ID / nodeVar

local ret = "return" * space
local printt = "@" * space
local OP = "(" * space
local CP = ")" * space
local OB = "{" * space
local CB = "}" * space

local opA = lpeg.C(lpeg.S("+-")) * space
local opM = lpeg.C(lpeg.S("*/%")) * space
local opC = lpeg.C(lpeg.P("<=") + ">=" + "<" +">" + "==" + "!=") * space

local function foldBin(lst)
    local tree = lst[1]
    for i=2, #lst, 2 do
        tree = {tag = "binop", e1=tree, op = lst[i], e2=lst[i+1]}
    end
    return tree
end

local factor = lpeg.V"factor"
local term = lpeg.V"term"
local exp = lpeg.V"exp"
local stat = lpeg.V"stat"
local stats = lpeg.V"stats"
local block = lpeg.V"block"


local grammar = lpeg.P{"stats",
-- adding empty statements
stats = ((OB * space * CB) + stat) * (SC^1 * stats)^-1 / nodeSeq,
block = OB * stats * SC^-1 * CB,
stat = block
     + ID * Assgn * exp / nodeAssgn
     + ret * exp / nodeRet
     + printt * exp / nodePrint,
factor = numeral + OP * exp * CP + var,
term = lpeg.Ct(factor * (opM * factor)^0) / foldBin,
exp = lpeg.Ct(term * ((opA + opC) * term)^0) / foldBin,
}

grammar = space * grammar * -1

local function parse (input)
    return grammar:match(input)
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

local function var2num (state, id)
    local num = state.vars[id]
    if not num then
        num = state.nvars + 1
        state.nvars = num
        state.vars[id] = num
    end
    return num
end

local function codeExp(state, ast)
    if ast.tag == "number" then
        addCode(state, "push")
        addCode(state, ast.val)
    elseif ast.tag == "variable" then
        addCode(state, "load")
        addCode(state, var2num(state, ast.var))
    elseif ast.tag == "binop" then
        codeExp(state, ast.e1)
        codeExp(state, ast.e2)
        addCode(state, ops[ast.op])
    else error("invalid tree")
    end
end

local function codeStat(state, ast)
    if ast.tag == "assgn" then
        codeExp(state, ast.exp)
        addCode(state, "store")
        addCode(state, var2num(state, ast.id))
    elseif ast.tag == "seq" then
        codeStat(state, ast.st1)
        codeStat(state, ast.st2)
    elseif ast.tag == "ret" then
        codeExp(state, ast.exp)
        addCode(state, "ret")
    elseif ast.tag == "print" then
        codeExp(state, ast.exp)
        print(ast.exp)
    else error("invalid tree")
    end
end

local function compile(ast)
    local state = {code = {}, vars = {}, nvars = 0}
    codeStat(state, ast)
    addCode(state, "push")
    addCode(state, "0")
    addCode(state, "ret")
    return state.code
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
        else error("unknown instruction")
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
local mem = {}
run(code, mem, stack)
print(stack[1])
