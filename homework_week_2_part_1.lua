local lpeg = require "lpeg"
local pt = require "pt"

--------------------------------------------------------------------------------
--[[
What arithmetic expression would generate that previous program?
push 4
push 2
push 24
push 21
sub
mult
add    

Answer: 4 + 2 * (24 - 21)
--]]
--------------------------------------------------------------------------------

local function node (num)
    return {tag = "number", val = tonumber(num)}
end

local space = lpeg.S(" \n\t")^0

-- match an integer in base 10
local int = lpeg.R("09")^1 / node * space

-- match a hex number
local hex = ("0" * lpeg.S("xX") * (lpeg.R("09") + lpeg.R("af", "AF"))^1) / node * space

-- match any of the previously defined numbers
local numeral = hex + int

-- Change the program, so it executes multiplication and division, instead of addition and subtraction.
local opM = lpeg.C(lpeg.S("*/")) * space

local function foldBin(lst)
    local tree = lst[1]
    for i=2, #lst, 2 do
        tree = {tag = "binop", e1=tree, op = lst[i], e2=lst[i+1]}
    end
    return tree
end


local exp = lpeg.Ct(numeral * (opM * numeral)^0) / foldBin

local function parse (input)
    return exp:match(input)
end

--------------------------------------------------------------------------------
local function addCode(state, op)
    local code = state.code
    code[#code + 1] = op
end


local ops = {["*"]="mul", ["/"]="div"}
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
        -- Add code to the interpreter loop (function run) so that it prints
        -- a trace of the instructions it executes.
        print(code[pc])
        if code[pc] == "push" then
            pc = pc + 1
            top = top + 1
            stack[top] = code[pc]
        elseif code[pc] == "mul" then
            stack[top - 1] = stack[top-1] * stack[top]
            top = top - 1
        elseif code[pc] == "div" then
            stack[top - 1] = stack[top-1] / stack[top]
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
run(code, stack)
print(stack[1])
