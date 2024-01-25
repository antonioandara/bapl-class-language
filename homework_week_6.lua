local lpeg = require "lpeg"
local pt = require "pt"

-- usually dynamic languages like Lua or python will allow self referencing data structures by default
-- but programming languages like C won't allow recursive arrays because they are typed and of fixed size

-- probabbly the best way to improve garbage collection of my language would be to make it somewhat compatible or visible
-- to the Lua garbage colector, I don't know much about garbage colletion myself so I'd be inclined in leveraging Lua's
-- mechanism for garbage collection in any way that I can.

--------------------------------------------------------------------------------
function I (msg)
    return lpeg.P(function () print(msg); return true end)
end

-- function node using tables
local function node(tag, ...)
    local labels = table.pack(...)
    return function (...)
        local params = table.pack(...)
        local nodeTable = {tag = tag}
        for i, label in ipairs(labels) do
            nodeTable[label] = params[i]
        end
        return nodeTable
    end
end
--------------------------------------------------------------------------------


local function nodeSeq (st1, st2)
    if st2 == nil then
        return st1
    else
        return {tag = "sequence", st1 = st1, st2 = st2}
    end
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
local numeral =(float + sci + hex + int) / tonumber / node("number", "val") * space

local function T(t)
    return t * space
end

local reserved = {"return", "if", "elseif", "else", "while", "and", "or", "new"}
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
local var = ID / node("variable", "var")
local prin = "@" * space
local opLogic = lpeg.C(lpeg.P("and") + lpeg.P("or")) * space
local opNeg = lpeg.C(lpeg.S("-!")) * space -- unary operator
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

local function foldIndex(lst)
    local tree = lst[1]
    for i=2, #lst do
        tree = { tag = "indexed", array = tree, index = lst[i] }
    end
    return tree
end

local function foldArray(lst)
	local tree = {tag = "new", indexes = lst}
	return tree
end

local LHS = lpeg.V"LHS"
local neg = lpeg.V"neg"
local comp = lpeg.V"comp"
local prim = lpeg.V"prim"
local factor = lpeg.V"factor"
local term = lpeg.V"term"
local expression = lpeg.V"expression"
local statement = lpeg.V"statement"
local statements = lpeg.V"statements"
local block = lpeg.V"block"
local elif = lpeg.V"elif"

local grammar = lpeg.P{"program",
    program = space * statements * -1,
    statements = (T"{" * space * T"}" + statement) * T";"^1 * statements^-1 / nodeSeq,
    block = T"{" * statements * T"}",
    elif = (Rw"if" + Rw"elseif") * expression * block * elif^-1 * (Rw"else" * block)^-1
            / node("if1", "cond", "th", "el"),
    statement = block
              + LHS * T"=" * expression / node("assignment", "LHS", "exp")
              + Rw"while" * expression * block /node("while1", "cond", "body")
              + elif
              + Rw"return" * expression / node("return", "exp")
              + prin * expression / node("print", "exp"),
  LHS = lpeg.Ct(var * (T"[" * expression * T"]")^0) / foldIndex,
  prim = lpeg.Ct(Rw"new" * (T"[" * expression * T"]")^1) / foldArray
       + numeral
       + (T"(" * expression * T")")
       + LHS,
    comp = lpeg.Ct((prim * (opC + opLogic) * prim)) / foldBin + prim,
    factor = lpeg.Ct(comp * (opExp * comp)^0) / foldBin,
    neg = lpeg.Ct(opNeg * factor) / foldUnary + factor,
    term = lpeg.Ct(neg * (opM * neg)^0) / foldBin,
    expression = lpeg.Ct(term * (opA * term)^0) / foldBin,
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

local unaryops = {["!"]="not",  ["-"]="neg"}


function Compiler:var2num (id)
    local num = self.vars[id]
    if not num then
        num = self.nvars + 1
        self.nvars = num
        self.vars[id] = num
    end
    return num
end

function Compiler:currentPosition()
    return #self.code
end

function Compiler:codeJmp(op)
    self:addCode(op)
    self:addCode(0)
    return self:currentPosition()
end

function Compiler:fixJmp2here(jmp, currentPos)
    local jump = self:currentPosition()
    self.code[jmp] = currentPos - jmp
end

function Compiler:codeExp(ast)
    if ast.tag == "number" then
        self:addCode("push")
        self:addCode(ast.val)
    elseif ast.tag == "variable" then
        self:addCode("load")
        self:addCode(self:var2num(ast.var))
    elseif ast.tag == "indexed" then
        self:codeExp(ast.array)
        self:codeExp(ast.index)
        self:addCode("getArray")
    elseif ast.tag == "new" then
        local array = self:codeExp(ast.indexes[1])
        self:addCode("newArray")
        for i = 2, #ast.indexes do
            print("newArray: " .. pt.pt(ast.indexes[i]))
			self:codeExp(ast.indexes[i])
            self:addCode("newMultiArray")
		end

    elseif ast.tag == "binop" then
        if ast.op == "and" then
            self:codeExp(ast.e1)
            local jmp = self:codeJmp("jmpZP")
            self:codeExp(ast.e2)
            local currentPos = self:currentPosition()
            self:fixJmp2here(jmp, currentPos)
        elseif ast.op == "or" then
            self:codeExp(ast.e1)
            local jmp = self:codeJmp("jmpNZP")
            self:codeExp(ast.e2)
            local currentPos = self:currentPosition()
            self:fixJmp2here(jmp, currentPos)
        else
            self:codeExp(ast.e1)
            self:codeExp(ast.e2)
            self:addCode(ops[ast.op])
        end

    elseif ast.tag == "unary" then
        self:codeExp(ast.e2)
        self:addCode(unaryops[ast.op])
    else error("invalid tree")
    end
end

function Compiler:codeAssgn(ast)
local lhs = ast.LHS
    if lhs.tag == "variable" then
        self:codeExp(ast.exp)
        self:addCode("store")
        self:addCode(self:var2num(lhs.var)) -- change ast.id to ast.lhs
    elseif lhs.tag == "indexed" then
        self:codeExp(lhs.array)
        self:codeExp(lhs.index)
        self:codeExp(ast.exp)
        self:addCode("setArray")
    else error("unknown tag")
    end
end

function Compiler:codeStat(ast) 
    if ast.tag == "assignment" then
        self:codeAssgn(ast)
    elseif ast.tag == "sequence" then
        self:codeStat(ast.st1)
        self:codeStat(ast.st2)
    elseif ast.tag == "return" then
        self:codeExp(ast.exp)
        self:addCode("ret")
    elseif ast.tag == "print" then
        self:codeExp(ast.exp)
        self:addCode("print")
    elseif ast.tag == "while1" then
        local ilabel = self:currentPosition()
        self:codeExp(ast.cond)
        local jmp = self:codeJmp("jmpR")
        self:codeStat(ast.body)
        local jmp2 = self:codeJmp("jmp")
        self:fixJmp2here(jmp2, ilabel)
        self:fixJmp2here(jmp, jmp2)
    elseif ast.tag == "if1" then
        self:codeExp(ast.cond)
        local jmp = self:codeJmp("jmpR")
        self:codeStat(ast.th)
        local currentPos = self:currentPosition()
        if ast.el == nil then
            self:fixJmp2here(jmp, currentPos)
        else
            local jmp2 = self:codeJmp("jmp")
            self:fixJmp2here(jmp, jmp2)
            self:codeStat(ast.el)
            currentPos = self:currentPosition()
            self:fixJmp2here(jmp2, currentPos)
        end

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
    if type(code[pc]) ~= "table" then
        for i = 1, top do
            if type(stack[i]) ~= "table" then
                io.write(stack[i], " ")
            end
        end
        io.write("\n", code[pc], "\n")
    end
    --]]
        if code[pc] == "ret" then
            return
        elseif code[pc] == "push" then
            pc = pc + 1
            top = top + 1
            stack[top] = code[pc]
        elseif code[pc] == "neg" then
            stack[top] = -stack[top]
        elseif code[pc] == "not" then
            if stack[top] == 0 then
                stack[top] = 1
            else
                stack[top] = 0
            end
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
        elseif code[pc] == "newArray" then
            local size = stack[top]
            stack[top] = { size = size }
        elseif code[pc] == "newMultiArray" then
            local size = stack[top]
            local array = stack[top - 1]

            local function addNestedLevel(t, newSize)
                local function isTableEmpty(t)
                    return type(t) == "table" and next(t) == nil
                end
                local function createNewLevel(size)
                    local newLevel = {size = size}
                    for i = 0, size - 1 do
                        newLevel[i] = {}
                    end
                    return newLevel
                end
                local function recursiveAdd(t)
                    if type(t) ~= "table" or t.size == nil then
                        return
                    end 
                    for i = 0, t.size - 1 do
                        if type(t[i]) == "table" and not isTableEmpty(t[i]) then
                            recursiveAdd(t[i])
                        else
                            t[i] = createNewLevel(newSize)
                        end
                    end
                end
                recursiveAdd(t)
            end

            addNestedLevel(array, size)
            top = top - 1
        elseif code[pc] == "getArray" then
            local array = stack[top - 1]
            local index = stack[top]
            if index >= 0 and index < array.size then
                stack[top - 1] = array[index]
                top = top - 1
            else
                error("index out of range")
            end

        elseif code[pc] == "setArray" then
            local array = stack[top - 2]
            local index = stack[top - 1]
            if index >= 0 and index < array.size then
                local value = stack[top]
                array[index] = value
                top = top - 3
            else
                error("index out of range")
            end

        elseif code[pc] == "print" then
            if type(stack[top]) == "number" then
               io.write(stack[top], "\n")
            elseif type(stack[top]) == "table" then
                local array = stack[top]
                io.write("[\n")
                for i = 0, array.size - 1 do
                    if array[i] ~= nil then
                        io.write(" ", i, " = ", array[i], "\n")
                    else
                        io.write(" ", i, " = ", "\n")
                    end
                end
                io.write("]\n")
            else
                io.write("Unsupported type: ", type(stack[top]), "\n")
            end
        elseif code[pc] == "jmp" then
            pc = pc + 1
            pc = code[pc] + pc
        elseif code[pc] == "jmpR" then
            pc = pc + 1
            if stack[top] == 0 or stack[top] == nil then
                pc = code[pc] + pc
            end
            top = top - 1
        elseif code[pc] == "jmpZP" then
            pc = pc + 1
            if stack[top] == 0 or stack[top] == nil then
                pc = code[pc] + pc
            else
                top = top - 1
            end
        elseif code[pc] == "jmpNZP" then
            pc = pc + 1
            if stack[top] == 0 or stack[top] == nil then
                top = top - 1
            else
                pc = code[pc] + pc
            end

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
