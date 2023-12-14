local lpeg = require "lpeg"

-- match a single digit
local digit = lpeg.R("09")
print(digit:match(1))

-- match multiple digits
local number = digit^1
print(number:match(123456789))

-- match any number of whitespaces
local spaces = lpeg.S(" \n\t")^0
print(spaces:match("     "))

-- match a number surrounded by spaces
local clean_number = number * spaces
print(clean_number:match("123   "))


-- match plus sign
local plus = lpeg.P("+") * spaces
print(plus:match("+"))


-- match a list of number followed by a plus operator
-- base case is the sum of two numbers a + b
-- for any other case we match repeat the first pattern "a +"
-- and end with b
local summation = spaces * lpeg.P(clean_number * plus)^1 * clean_number
print(summation:match("   123   +2+ 345 +   88 "))

-- capturing numerals and positions of plus operator
local positions = spaces * lpeg.P(lpeg.C(clean_number) * lpeg.Cp() * plus)^1 * lpeg.C(clean_number)
print(positions:match("12+13+35"))


-- matching the whole subject
local clean_sum = spaces * lpeg.P(lpeg.C(clean_number) * lpeg.Cp() * plus)^1 * lpeg.C(clean_number) * lpeg.P(-1)
print(clean_sum:match("12 + 13 + 88 "))
print(clean_sum:match("12 + 13 + 88  a"))


-- adding an optional sign to numerals
local sign = lpeg.S("+-")^-1
local signed = sign * clean_number
print(signed:match("+8"))
print(signed:match("-5"))

local signed_sum = spaces * lpeg.P(lpeg.C(signed) * lpeg.Cp() * plus * spaces)^1 * lpeg.C(signed) * lpeg.P(-1)
print(signed_sum:match("-1 + 3  - -8 + 2"))

-- adding operators
local operator_additive = lpeg.C(lpeg.S("+-")) * spaces
local operator_multiplicative = lpeg.C(lpeg.S("*/%")) * spaces
local operator_power = lpeg.C(lpeg.P("^")) * spaces
local numeral = lpeg.C(digit^1) * spaces

function fold (lst)
	local acc = lst[1]
	for i=2, #lst, 2 do
		if lst[i] == "+" then
			acc = acc + lst[i+1]
		elseif lst[i] == "-" then
			acc = acc - lst[i+1]
		elseif lst[i] == "*" then
			acc = acc * lst[i+1]
		elseif lst[i] == "/" then
			acc = acc / lst[i+1]
		elseif lst[i] == "%" then
			acc = acc % lst[i+1]
		elseif lst[i] == "^" then
			acc = acc ^ lst[i+1]
		end
	end
		return acc
end

local term = spaces * lpeg.Ct((numeral * operator_multiplicative)^0 * numeral) / fold
local power = spaces * lpeg.Ct((numeral * operator_power)^0 * term) / fold
local sum = spaces * lpeg.Ct((power * operator_additive)^0 * term) / fold * -1

local subject = " 13 + 12 - 25   +    8   "

print(subject)
print(sum:match(subject))

local subject = " 4 + 2*6 "
print(subject)
print(sum:match(subject))

local subject = " 4 + 10 %  3 "
print(subject)
print(sum:match(subject))

local subject = " 8 + 5*2 - 10 + 3^2^3 -8  "
print(subject)
print(sum:match(subject))

