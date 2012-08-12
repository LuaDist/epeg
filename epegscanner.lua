

epeg = dofile("./epeg.lua")

grammar = epeg.read_grammar("epeg.epeg")

if not grammar then
	error("Couldn't read grammar.")
end

epeg.dump_grammar(grammar, "epeg.grammar", "r1.lua")

local file = io.open("epeg.epeg", "r")
local text = file:read("*a")

local tree = epeg.parse_text(grammar, text)

if not tree then
	error ("Couldn't parse input.")
end

epeg.dump_grammar(tree, "epeg.grammar", "r2.lua")
