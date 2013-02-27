--[[
	Copyright 2008 Victor Camps

    This file is part of epeg.

    Epeg is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Epeg is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
]]


--------------------------------------------------------------------------------
--                                                                            --
--  Plumbing                                                                  --
--                                                                            --
--------------------------------------------------------------------------------


epeg = {}
epeg_proxy = {}

-- If we don't use a function to check against invalid function names,
-- lua will just stop without any notice which is hard to track.
function epeg_proxy.__index(table, key)
	if epeg[key] == nil then error("Called unknown function "..key..".\n") end
	return epeg[key]
end



--------------------------------------------------------------------------------
--                                                                            --
--  Mainfunctions                                                             --
--                                                                            --
--------------------------------------------------------------------------------


-- Reads a grammar from file filename and returns it.
-- If unsuccessful, it returns false and prints errors to stdout.
-- It is possible that it prints other informations to stdout.
function epeg.read_grammar(filename)

	local file, input, error
	file, error = io.open(filename, 'r')

	if file == nil then
		io.write("Error: could not open ", filename, ".\nReason: ", error)
		return false
	end

	input = file:read('*a')
	file:close()

	if input == nil then
		io.write("Error: Could not read out of file or file is empty.")
		return false
	end

	local result

	-- Get ast from epeg.
	result = epeg.parse_text(epeg.grammar, input)

	if not result then return false end

	-- Refine ast to grammar.
	local r = result[1].name.string
	local grammar = { [0] = r }
	local ok = grammar

	for i,v in ipairs(result) do
		if grammar[v.name.string] then
			io.write("Error: Rule ", v.name.string, " redeclared at ", v.row, ":", v.column, ".\n")
			if not type(grammar[v.name.string]) == "bool" then
				io.write("Error: Original declaration at ", grammar[v.name.string].row, ":", grammar[v.name.string].column, ".\n")
			else
				grammar[v.name.string] = false
			end

			ok = false
		else
			grammar[v.name.string] = v
		end
	end

	if ok and not epeg.check(grammar) then ok = false end

	return ok
end


-- Parses a given text using the grammar and returns the specified ast
-- for the grammar.
function epeg.parse_text(grammar, text)
	local topnode = {
		["position"] = 1,
		["column"] = 0,
		["row"] = 1,
		["string"] = ""
	}

	local parser = {
		["position"] = 1,
		["column"] = 0,
		["row"] = 1,
		["to_parse"] = text,
		["grammar"] = grammar,
		["grabs"] = 0,
		["anchor"] = {},
		["topnode"] = topnode,
	}

	setmetatable(parser, epeg_proxy)

	parsing = coroutine.create(epeg.run_rule)
	if not coroutine.resume(parsing, parser, grammar[grammar[0]], topnode) then
		return false
	end

	return topnode
end


-- Dumps the given grammar as a lua table to file 'filename'. If filename is
-- nil, stdout is used.
-- if name is nil, it writes return { ... instead of tablename = {
-- returns true if success, else error string.
-- works only correct on grammars.
--
-- Note: is a specialized function to dump noncyclic tables.
function epeg.dump_grammar(grammar, name, filename)
	local file, err

	if not filename then
		file = io.stdout
	else
		file, err = io.open(filename, "w")
		if not file then return err end
	end

	if type(name) ~= "string" then
		file:write("return {\n")
	else
		file:write(name, " = {\n")
	end

	epeg.dump_table(grammar, file, 1)
	file:write("}\n")

	return true
end



--------------------------------------------------------------------------------
--                                                                            --
--  Parsing                                                                   --
--                                                                            --
--------------------------------------------------------------------------------


-- Returns the string which matches with the regex. The search start at the
-- actual position in the input stream. if walk is NOT set, the position will
-- be updated after a successfull match
-- If plain is set to true, the regex is a normal string.
-- returns false if the regex was not found.
function epeg:read_next(regex, walk, plain)
	local start, length, char, row, column, position, to_parse

	position = self.position
	row = self.row
	column = self.column
	to_parse = self.to_parse

	start, stop = string.find(to_parse, regex, position, plain)

	if start == nil then
		return false
	end

	if start ~= position then
		io.write("Note: Regex ", regex,
				 " does not match against the actual position. (position=",
				 position,", row=", row, ", column=", column,
				 ")\n")
	end

	-- update row/column/position
	if walk ~= false then
		for i = position, stop do
			char = string.sub(to_parse, position, position)

			if char == '\n' then
				row = row + 1
				column = 0
			-- tabs count as 4 whitespaces
			elseif char == '\t' then
				column = column + 4
			else
				column = column + 1
			end

			position = position + 1
		end

		self.position = position
		self.row = row
		self.column = column
	end

	return string.sub(to_parse, start, stop)
end


-- Workhorse for transforming text to an ast with help of a grammar.
-- Both used by read_grammar and parse_text.
function epeg.run_rule(parser, rule, node, parent)
	local position = parser.position
	local row = parser.row
	local column = parser.column
	local string_old_length = #(node.string)
	local string_old = node.string

	local newnode = {
		["position"] = position,
		["column"] = column,
		["row"] = row,
		["string"] = "",
		["parent"] = node
	}

	local result = false

	if rule.type.string == "Rule" then
		result = epeg.run_rule(parser, rule.rule, node)
		return result

	elseif rule.type.string == "Choice" then
		-- Only one list out of the selection has to match successfull.

		for rulename, subrule in ipairs(rule) do
			if epeg.run_rule(parser, subrule, node) then
				return true
			end
		end

		-- Failed. Resetting the parser gets handled at the end of the function.

	elseif rule.type.string == "List" then
		-- Every pattern in the list needs to succeed. If not, we have to
		-- reset the tree and parser to the state before we went through
		-- the list.
		-- To reset the tree, we use parser.anchor to store a reference to all
		-- newly created child nodes. If the rule fails, we can use this list to
		-- remove the newly added childs from our actual node.
		-- The parser gets reset at the end of the function.

		local oldanchor = parser.anchor
		local anchor = {}
		parser.anchor = anchor

		local worked = true

		for rulename, subrule in ipairs(rule) do
			if not epeg.run_rule(parser, subrule, node) then
				worked = false
				break
			end
		end

		parser.anchor = oldanchor

		if worked then return true end

		-- the rule failed and we have to remove every created child node from
		-- our node to clean the mess up.
		for i,v in ipairs(anchor) do
			if v ~= -1 then
				node[v] = nil
			end
		end

		anchor = nil

	elseif rule.type.string == "RuleName" then
		return epeg.run_rule(parser, parser.grammar[rule.name.string], node)

	elseif rule.type.string == "AnonymousFixedNode" then
		newnode.string = rule.grabs.string
		table.insert(node, newnode)
		table.insert(parser.anchor, #node)
		newnode.pindex = #node
		return true

	elseif rule.type.string == "AnonymousGrabbingNode" then
		table.insert(node, newnode)
		newnode.pindex = #node
		table.insert(parser.anchor, #node)

		if not epeg.run_rule(parser, rule.grabs, newnode) then
			table.remove(node)
			parser.anchor[#parser.anchor] = -1
			return false
		end

		newnode.string = epeg.unescape_string(newnode.string)

		return true

	elseif rule.type.string == "FixedNode" then

		if node[rule.name.string] then
			epeg.print_table(node)
			epeg.print_table(rule)
			parser:error("Tried to add second node with same name ("..rule.name.string..") to parent node.")
		end

		node[rule.name.string] = newnode
		newnode.pindex = rule.name.string
		newnode.string = rule.grabs.string
		table.insert(parser.anchor, rule.name.string)
		return true

	elseif rule.type.string == "GrabbingNode" then
		if node[rule.name.string] then
			epeg.print_table(node)
			epeg.print_table(rule)
			parser:error("Tried to add second node with same name ("..rule.name.string..") to parent node.")
		end

		node[rule.name.string] = newnode
		newnode.pindex = rule.name.string
		table.insert(parser.anchor, rule.name.string)

		if not epeg.run_rule(parser, rule.grabs, newnode) then
			node[rule.name.string] = nil
			parser.anchor[#parser.anchor] = -1
			return false
		end

		newnode.string = epeg.unescape_string(newnode.string)

		return true

	elseif rule.type.string == "CollectInput" then

		parser.grabs = parser.grabs + 1
		result = epeg.run_rule(parser, rule.grabs, node)
		parser.grabs = parser.grabs - 1

		return result

	elseif rule.type.string == "FunctionCall" then

		result = _G[rule["function"]["string"]](parser, node)

		if parent and parent.type.string == "Pattern" and parent.prefix then
			if parent.prefix.string == "!" then return not result
			else return result end
		end

		return true
	elseif rule.type.string == "Pattern" then
		local repetition = ""
		local prefix = ""

		if rule.errorhandling and rule.errorhandling.type.string == "Warning" then
			io.write("Warning (row="..parser.row..", column="..parser.column.."): "..rule.errorhandling.message.string)
		end

		if rule.prefix then prefix = rule.prefix.string end

		if rule.repetition then repetition = rule.repetition.string end

		-- First check for simple rules (string, dot and class), because we can
		-- use regular expressions to match them.
		if rule.pattern.type.string == "String" then
			if not rule.pattern.is_escaped then
				rule.pattern.value.string = string.gsub(rule.pattern.value.string, "[%$%^%(%)%%%.%[%]%*%+%-%?]", "%%%0")
				rule.pattern.is_escaped = true
			end

			result = parser:read_next("^"..rule.pattern.value.string..repetition)
			if parser.grabs ~= 0 and result then node.string = node.string..result end

		elseif rule.pattern.type.string == "Dot" then
			result = parser:read_next("^".."."..repetition)
			if parser.grabs ~= 0 and result then node.string = node.string..result end

		elseif rule.pattern.type.string == "Class" then
			result = parser:read_next("^".."["..rule.pattern.value.string.."]"..repetition)
			if parser.grabs ~= 0 and result then node.string = node.string..result end

		else
			-- Otherwise we handle the complex rules:

			if repetition == "*" then
				while epeg.run_rule(parser, rule.pattern, node, rule) do end
				result = true
			elseif repetition == "?" then
				epeg.run_rule(parser, rule.pattern, node, rule)
				result = true
			elseif repetition == "+" then
				result = epeg.run_rule(parser, rule.pattern, node, rule)

				while epeg.run_rule(parser, rule.pattern, node, rule) do end
			else
				result = epeg.run_rule(parser, rule.pattern, node, rule)
			end
		end

		if rule.prefix and rule.prefix.string == "!" then result = not result end

		if result and not rule.prefix then return true end

		if not result and rule.errorhandling and rule.errorhandling.type.string == "Error" then
			parser:error(rule.errorhandling.message.string)
		end

	else
		parser:error("Epeg construct "..tostring(rule.type.string).." not implemented or unknown.")
	end

	-- Reset parser to position before we processed the rule.
	-- (used by !, & and if the rule failed)

	-- If the rule failed, we have to reset the grabbed input, too.
	if parser.grabs ~= 0 and #(node.string) ~= string_old_length and not result then
		node.string = string_old
	end

	parser.row = row
	parser.column = column
	parser.position = position

	return result
end


-- Finishes the coroutine with an errormessage and returns false.
function epeg:error(text)
	io.write("Error (row=", tostring(self.row),
	                     ", column=", tostring(self.column),
				         "): ", tostring(text), ".\n")
	yield(false)
end


--------------------------------------------------------------------------------
--                                                                            --
--  Validation of Grammar                                                     --
--                                                                            --
--------------------------------------------------------------------------------


-- Checks grammar against infinite left recursion.
-- returns true if no error, else false.
-- Prints its status to stdout.
function epeg.check(grammar)
	local ok = true
	local top = grammar[0]

	io.write("Checking...\n")

	-- check against possible infinite left recursion. Algorithm taken from
	-- peg/leg by Ian Piumarta.
	-- See: http://piumarta.com/software/peg/ for more information.
	for i,v in pairs(grammar) do
		if i ~= 0 then
			epeg.consumesInput(v, grammar)
		end
	end

	for i,v in pairs(grammar) do
		if i ~= 0 then
			if v.is_marked_lr then ok = false end
		end
	end


	if ok then
		io.write("... Success\n")
	end

	return ok
end


function epeg.consumesInput(node, grammar)
	if node == nil then return false end

	if node.type.string == "Rule" then
		local result = false

		if node.reached ~= nil then
			if node.is_marked_lr == nil then
				io.write("Possible endless left recursion in ", node.name.string, ".\n")
				node.is_marked_lr = true
			end
		else
			node.reached = true
			result = epeg.consumesInput(node.rule, grammar)
			node.reached = nil
		end

		return result
	elseif node.type.string == "Choice" then
		for i,v in ipairs(node) do
			if not epeg.consumesInput(v, grammar) then return false end
		end

		return true
	elseif node.type.string == "List" then
		for i,v in ipairs(node) do
			if epeg.consumesInput(v, grammar) then return true end
		end

		return false
	elseif node.type.string == "RuleName" then
		return epeg.consumesInput(grammar[node.name.string], grammar)
	elseif node.type.string == "AnonymousFixedNode" then
		return false
	elseif node.type.string == "AnonymousGrabbingNode" then
		return epeg.consumesInput(node.grabs, grammar)
	elseif node.type.string == "FixedNode" then
		return false
	elseif node.type.string == "GrabbingNode" then
		return epeg.consumesInput(node.grabs, grammar)
	elseif node.type.string == "CollectInput" then
		return epeg.consumesInput(node.grabs, grammar)
	elseif node.type.string == "Error" then
		return false
	elseif node.type.string == "Warning" then
		return false
	elseif node.type.string == "Resume" then
		return false
	elseif node.type.string == "FunctionCall" then
		return false
	elseif node.type.string == "EmbeddedCode" then
		return false
	elseif node.type.string == "String" then
		if node.value.string == "" then return false end
		return true
	elseif node.type.string == "Class" then
		if node.value.string == "" then return false end
		return true
	elseif node.type.string == "Dot" then
		return true
	elseif node.type.string == "Pattern" then
		-- The pattern does not need to consume input if it uses ? or *.
		if node.repetition and
		   (node.repetition.string == "*" or
		    node.repetition.string == "?") then
			return false
		end
		return epeg.consumesInput(node.pattern, grammar)
	else
		io.write("Error: Unknown epeg construct: ", tostring(node.type.string), ".\n")
		return false
	end
end


--------------------------------------------------------------------------------
--                                                                            --
--  Dumping                                                                   --
--                                                                            --
--------------------------------------------------------------------------------


-- Dumps a table and its child tables into the specified file.
-- Does not work with cyclic tables (endless recursion).
function epeg.dump_table(table, file, depth)
	local tabs = string.rep("\t", depth)

	for i,v in pairs(table) do
		if i ~= "position" and i ~= "column" and i ~= "row" and
		   i ~= "pindex" and i ~= "parent" then
			if type(i) == "string" then
				file:write(tabs, "[\"", i, "\"] = ")
			else
				file:write(tabs, "[", tostring(i), "] = ")
			end

			if type(v) == "table" then
				file:write("{\n")
					epeg.dump_table(v, file, depth + 1)
				file:write(tabs, "},\n")
			else
				if type(v) == "string" then
					file:write("\"", epeg.escape_string(v), "\",\n")
				else
					file:write(tostring(v), ",\n")
				end
			end
		end
	end
end


--------------------------------------------------------------------------------
--                                                                            --
--  Debugging                                                                 --
--                                                                            --
--------------------------------------------------------------------------------


-- Prints a noncyclic table to stdout. Cyclic tables cause
-- infinite rekursion (DONT USE ON GRAMMARS OR ASTS!).
function epeg.print_table(tab, depth)
	if not (type(tab) == "table") then print ("failure"); return end
	if depth == nil then depth = 0 end
	local tabs = string.rep("\t", depth)

	for i,v in pairs(tab) do
		if type(v) ~= "table" then
			io.write(tabs, tostring(i), ": ", tostring(v), "\n")
		end
	end
	for i,v in pairs(tab) do
		if type(v) == "table" then
			io.write(tabs, tostring(i), ": (table)\n")
			epeg.print_table(v, depth + 1)
		end
	end
end


-- Prints a grammar translated back as epeg to stdout.
-- warning: old
function epeg.grammar2epeg(grammar)
	for i,v in pairs(grammar) do
		io.write("\n", i, " <- ")
		epeg.translate_node(v.rule)
		io.write('\n')
	end
end


-- warning: old
function epeg.translate_node(node)
	if node.type.string == "Choice" then
		for i,v in ipairs(node) do
			epeg.translate_node(v)
			if node.list[i + 1] ~= nil then io.write('/ ') end
		end
	elseif node.type.string == "List" then
		for i,v in ipairs(node) do
			epeg.translate_node(v)
		end
	elseif node.type.string == "RuleName" then
		io.write(node.name.string, " ")
	elseif node.type.string == "AnonymousFixedNode" then
		io.write("<:: ")
		epeg.translate_node(node.grabs)
		io.write(">")
	elseif node.type.string == "AnonymousGrabbingNode" then
		io.write("<: ")
		epeg.translate_node(node.grabs)
		io.write(">")
	elseif node.type.string == "FixedNode" then
		io.write("<", node.name.string, ":: ")
		epeg.translate_node(node.grabs)
		io.write(">")
	elseif node.type.string == "GrabbingNode" then
		io.write("<", node.name.string, ": ")
		epeg.translate_node(node.grabs)
		io.write(">")
	elseif node.type.string == "CollectInput" then
		io.write("< ")
		epeg.translate_node(node.grabs)
		io.write(" >")
	elseif node.type.string == "Error" then
		io.write(" error (")
		epeg.translate_node(node.message)
		io.write(") ")
		if node.recover then
			io.write("recover (")
			epeg.translate_node(node.recover)
			io.write(") ")
		end
	elseif node.type.string == "Warning" then
		io.write("warning (")
		epeg.translate_node(node.message)
		io.write(") ")
	elseif node.type.string == "Resume" then
		io.write("resume ")
	elseif node.type.string == "FunctionCall" then
		io.write("{: ", node.name.string, " } ")
	elseif node.type.string == "EmbeddedCode" then
		io.write("{", node.code, "} ")
	elseif node.type.string == "String" then
		io.write("\"", node.value.string, "\" ")
	elseif node.type.string == "Class" then
		io.write("[", node.value.string, "] ")
	elseif node.type.string == "Dot" then
		io.write(".")
	elseif node.type.string == "Pattern" then
		if node.prefix then io.write(node.prefix.string) end
		if node.pattern.type.string == "Choice" then
			io.write("(")
			epeg.translate_node(node.pattern)
			io.write(")")
		else
			epeg.translate_node(node.pattern)
		end

		if node.repetition then
			io.write(node.repetition.string)
		end

		if node.errorhandling then
			epeg.translate_node(node.errorhandling)
		end
	else
		io.write("\n***ERROR***:", node.type.string, " \n")
	end
end



--------------------------------------------------------------------------------
--                                                                            --
--  Escaping/Unescaping                                                       --
--                                                                            --
--------------------------------------------------------------------------------

epeg.esc_table = {
	["\a"] = '\\a',
	["\b"] = '\\b',
	["\f"] = '\\f',
	["\n"] = "\\n",
	["\r"] = "\\r",
	["\t"] = '\\t',
	["\v"] = '\\v',
	["\""] = '\\\"',
	["\'"] = '\\\'',
	["\["] = '\\\[',
	["\]"] = '\\\]',
	["\\"] = '\\\\',
}


epeg.unesc_table = {
	["\\a"] = '\a',
	["\\b"] = '\b',
	["\\f"] = '\f',
	["\\n"] = '\n',
	["\\r"] = '\r',
	["\\t"] = '\t',
	["\\v"] = '\v',
	['\\"'] = '\"',
	["\\'"] = "\'",
	["\\["] = '\[',
	["\\]"] = '\]',
	["\\\\"] = '\\',
}


function epeg.unescape_string(str)
	local result = {}
	local pos = 1
	local oldpos = 1
	local echar, tesc

	result = string.gsub(str, "\\.", epeg.unesc_table)
	return result
end


function epeg.escape_string(str)
	local result
	result = string.gsub(str, "[]\\\a\b\f\n\r\t\v\"\'[]", epeg.esc_table)
	return result
end



--------------------------------------------------------------------------------
--                                                                            --
--  Grammar                                                                   --
--                                                                            --
--------------------------------------------------------------------------------


epeg.grammar = {
	[0] = "Grammar",
	["Pattern"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Pattern",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "Pattern",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				[2] = {
					["string"] = "",
					["pattern"] = {
						[1] = {
							[1] = {
								["string"] = "",
								["pattern"] = {
									[1] = {
										["string"] = "",
										[2] = {
											["string"] = "",
											["name"] = {
												["string"] = "prefix",
											},
											["grabs"] = {
												["string"] = "&",
											},
											["type"] = {
												["string"] = "FixedNode",
											},
										},
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["name"] = {
													["string"] = "and",
												},
												["type"] = {
													["string"] = "RuleName",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									[2] = {
										["string"] = "",
										[2] = {
											["string"] = "",
											["name"] = {
												["string"] = "prefix",
											},
											["grabs"] = {
												["string"] = "!",
											},
											["type"] = {
												["string"] = "FixedNode",
											},
										},
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["name"] = {
													["string"] = "not",
												},
												["type"] = {
													["string"] = "RuleName",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									["type"] = {
										["string"] = "Choice",
									},
									["string"] = "",
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							[2] = {
								["string"] = "",
								["name"] = {
									["string"] = "pattern",
								},
								["grabs"] = {
									["string"] = "",
									[1] = {
										["string"] = "",
										["pattern"] = {
											[1] = {
												["string"] = "",
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "FunctionCall",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[2] = {
												["string"] = "",
												[2] = {
													["string"] = "",
													["name"] = {
														["string"] = "type",
													},
													["grabs"] = {
														["string"] = "Dot",
													},
													["type"] = {
														["string"] = "FixedNode",
													},
												},
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "dot",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[3] = {
												["string"] = "",
												[2] = {
													["string"] = "",
													["name"] = {
														["string"] = "type",
													},
													["grabs"] = {
														["string"] = "RuleName",
													},
													["type"] = {
														["string"] = "FixedNode",
													},
												},
												[1] = {
													["string"] = "",
													["name"] = {
														["string"] = "name",
													},
													["grabs"] = {
														["string"] = "",
														[1] = {
															["string"] = "",
															["pattern"] = {
																["string"] = "",
																["name"] = {
																	["string"] = "Identifier",
																},
																["type"] = {
																	["string"] = "RuleName",
																},
															},
															["type"] = {
																["string"] = "Pattern",
															},
														},
														["type"] = {
															["string"] = "List",
														},
													},
													["type"] = {
														["string"] = "GrabbingNode",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[4] = {
												["string"] = "",
												[2] = {
													["string"] = "",
													["name"] = {
														["string"] = "type",
													},
													["grabs"] = {
														["string"] = "Class",
													},
													["type"] = {
														["string"] = "FixedNode",
													},
												},
												[1] = {
													["string"] = "",
													["name"] = {
														["string"] = "value",
													},
													["grabs"] = {
														["string"] = "",
														[1] = {
															["string"] = "",
															["pattern"] = {
																["string"] = "",
																["name"] = {
																	["string"] = "Class",
																},
																["type"] = {
																	["string"] = "RuleName",
																},
															},
															["type"] = {
																["string"] = "Pattern",
															},
														},
														["type"] = {
															["string"] = "List",
														},
													},
													["type"] = {
														["string"] = "GrabbingNode",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[5] = {
												["string"] = "",
												[2] = {
													["string"] = "",
													["name"] = {
														["string"] = "type",
													},
													["grabs"] = {
														["string"] = "String",
													},
													["type"] = {
														["string"] = "FixedNode",
													},
												},
												[1] = {
													["string"] = "",
													["name"] = {
														["string"] = "value",
													},
													["grabs"] = {
														["string"] = "",
														[1] = {
															["string"] = "",
															["pattern"] = {
																["string"] = "",
																["name"] = {
																	["string"] = "String",
																},
																["type"] = {
																	["string"] = "RuleName",
																},
															},
															["type"] = {
																["string"] = "Pattern",
															},
														},
														["type"] = {
															["string"] = "List",
														},
													},
													["type"] = {
														["string"] = "GrabbingNode",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[6] = {
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "open",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												[2] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "Choice",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["errorhandling"] = {
														["message"] = {
															["string"] = "\'.../...\' Expected after \')\'.",
														},
														["string"] = "",
														["type"] = {
															["string"] = "error",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												[3] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "close",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["errorhandling"] = {
														["message"] = {
															["string"] = "\'(\' expected after \'.../...\'.",
														},
														["string"] = "",
														["type"] = {
															["string"] = "error",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
												["string"] = "",
											},
											["string"] = "",
											["type"] = {
												["string"] = "Choice",
											},
										},
										["errorhandling"] = {
											["message"] = {
												["string"] = " \'{ ... }\', \'.\', \'\[...\]\', \"...\" or rulename expected after &/!.",
											},
											["string"] = "",
											["type"] = {
												["string"] = "error",
											},
										},
										["type"] = {
											["string"] = "Pattern",
										},
									},
									["type"] = {
										["string"] = "List",
									},
								},
								["type"] = {
									["string"] = "GrabbingNode",
								},
							},
							[3] = {
								["repetition"] = {
									["string"] = "?",
								},
								["string"] = "",
								["pattern"] = {
									[1] = {
										["string"] = "",
										[2] = {
											["string"] = "",
											["name"] = {
												["string"] = "repetition",
											},
											["grabs"] = {
												["string"] = "?",
											},
											["type"] = {
												["string"] = "FixedNode",
											},
										},
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["name"] = {
													["string"] = "query",
												},
												["type"] = {
													["string"] = "RuleName",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									["string"] = "",
									["type"] = {
										["string"] = "Choice",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
							["string"] = "",
						},
						[2] = {
							["string"] = "",
							[2] = {
								["repetition"] = {
									["string"] = "?",
								},
								["string"] = "",
								["pattern"] = {
									[1] = {
										["string"] = "",
										[2] = {
											["string"] = "",
											["name"] = {
												["string"] = "repetition",
											},
											["grabs"] = {
												["string"] = "?",
											},
											["type"] = {
												["string"] = "FixedNode",
											},
										},
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["name"] = {
													["string"] = "query",
												},
												["type"] = {
													["string"] = "RuleName",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									[2] = {
										["string"] = "",
										[2] = {
											["string"] = "",
											["name"] = {
												["string"] = "repetition",
											},
											["grabs"] = {
												["string"] = "*",
											},
											["type"] = {
												["string"] = "FixedNode",
											},
										},
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["name"] = {
													["string"] = "star",
												},
												["type"] = {
													["string"] = "RuleName",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									["type"] = {
										["string"] = "Choice",
									},
									[3] = {
										["string"] = "",
										[2] = {
											["string"] = "",
											["name"] = {
												["string"] = "repetition",
											},
											["grabs"] = {
												["string"] = "+",
											},
											["type"] = {
												["string"] = "FixedNode",
											},
										},
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["name"] = {
													["string"] = "plus",
												},
												["type"] = {
													["string"] = "RuleName",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									["string"] = "",
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							[1] = {
								["string"] = "",
								["name"] = {
									["string"] = "pattern",
								},
								["grabs"] = {
									["string"] = "",
									[1] = {
										["string"] = "",
										["pattern"] = {
											[1] = {
												["string"] = "",
												[2] = {
													["string"] = "",
													["name"] = {
														["string"] = "type",
													},
													["grabs"] = {
														["string"] = "Dot",
													},
													["type"] = {
														["string"] = "FixedNode",
													},
												},
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "dot",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[2] = {
												["string"] = "",
												[2] = {
													["string"] = "",
													["name"] = {
														["string"] = "type",
													},
													["grabs"] = {
														["string"] = "RuleName",
													},
													["type"] = {
														["string"] = "FixedNode",
													},
												},
												[1] = {
													["string"] = "",
													["name"] = {
														["string"] = "name",
													},
													["grabs"] = {
														["string"] = "",
														[1] = {
															["string"] = "",
															["pattern"] = {
																["string"] = "",
																["name"] = {
																	["string"] = "IdentifierWithoutLeftArrow",
																},
																["type"] = {
																	["string"] = "RuleName",
																},
															},
															["type"] = {
																["string"] = "Pattern",
															},
														},
														["type"] = {
															["string"] = "List",
														},
													},
													["type"] = {
														["string"] = "GrabbingNode",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[5] = {
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "open",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												[2] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "Choice",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["errorhandling"] = {
														["message"] = {
															["string"] = "\'.../...\' Expected after \')\'.",
														},
														["string"] = "",
														["type"] = {
															["string"] = "error",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												[3] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "close",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["errorhandling"] = {
														["message"] = {
															["string"] = "\'(\' expected after \'.../...\'.",
														},
														["string"] = "",
														["type"] = {
															["string"] = "error",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
												["string"] = "",
											},
											["type"] = {
												["string"] = "Choice",
											},
											[3] = {
												["string"] = "",
												[2] = {
													["string"] = "",
													["name"] = {
														["string"] = "type",
													},
													["grabs"] = {
														["string"] = "Class",
													},
													["type"] = {
														["string"] = "FixedNode",
													},
												},
												[1] = {
													["string"] = "",
													["name"] = {
														["string"] = "value",
													},
													["grabs"] = {
														["string"] = "",
														[1] = {
															["string"] = "",
															["pattern"] = {
																["string"] = "",
																["name"] = {
																	["string"] = "Class",
																},
																["type"] = {
																	["string"] = "RuleName",
																},
															},
															["type"] = {
																["string"] = "Pattern",
															},
														},
														["type"] = {
															["string"] = "List",
														},
													},
													["type"] = {
														["string"] = "GrabbingNode",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[4] = {
												["string"] = "",
												[2] = {
													["string"] = "",
													["name"] = {
														["string"] = "type",
													},
													["grabs"] = {
														["string"] = "String",
													},
													["type"] = {
														["string"] = "FixedNode",
													},
												},
												[1] = {
													["string"] = "",
													["name"] = {
														["string"] = "value",
													},
													["grabs"] = {
														["string"] = "",
														[1] = {
															["string"] = "",
															["pattern"] = {
																["string"] = "",
																["name"] = {
																	["string"] = "String",
																},
																["type"] = {
																	["string"] = "RuleName",
																},
															},
															["type"] = {
																["string"] = "Pattern",
															},
														},
														["type"] = {
															["string"] = "List",
														},
													},
													["type"] = {
														["string"] = "GrabbingNode",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											["string"] = "",
										},
										["type"] = {
											["string"] = "Pattern",
										},
									},
									["type"] = {
										["string"] = "List",
									},
								},
								["type"] = {
									["string"] = "GrabbingNode",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						["type"] = {
							["string"] = "Choice",
						},
						["string"] = "",
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[3] = {
					["repetition"] = {
						["string"] = "?",
					},
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "ErrorHandling",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["close"] = {
		["string"] = "",
		["name"] = {
			["string"] = "close",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = ")",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["AnonymousFixedNode"] = {
		["string"] = "",
		["name"] = {
			["string"] = "AnonymousFixedNode",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "AnonymousFixedNode",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "colon2",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[3] = {
					["string"] = "",
					["name"] = {
						["string"] = "grabs",
					},
					["grabs"] = {
						["string"] = "",
						[1] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "String",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["errorhandling"] = {
								["message"] = {
									["string"] = "Expected string after \'(::\'.",
								},
								["string"] = "",
								["type"] = {
									["string"] = "error",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["leftarrow"] = {
		["string"] = "",
		["name"] = {
			["string"] = "leftarrow",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "<-",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["colon"] = {
		["string"] = "",
		["name"] = {
			["string"] = "colon",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = ":",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Identifier"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Identifier",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["pattern"] = {
						[1] = {
							["string"] = "",
							[2] = {
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["name"] = {
										["string"] = "ForcedSeparation",
									},
									["type"] = {
										["string"] = "RuleName",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							[1] = {
								["string"] = "",
								["pattern"] = {
									[1] = {
										["string"] = "",
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["type"] = {
													["string"] = "String",
												},
												["value"] = {
													["string"] = "error",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									[2] = {
										["string"] = "",
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["type"] = {
													["string"] = "String",
												},
												["value"] = {
													["string"] = "warning",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									["type"] = {
										["string"] = "Choice",
									},
									[3] = {
										["string"] = "",
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["type"] = {
													["string"] = "String",
												},
												["value"] = {
													["string"] = "recover",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									[4] = {
										["string"] = "",
										[1] = {
											["string"] = "",
											["pattern"] = {
												["string"] = "",
												["type"] = {
													["string"] = "String",
												},
												["value"] = {
													["string"] = "resume",
												},
											},
											["type"] = {
												["string"] = "Pattern",
											},
										},
										["type"] = {
											["string"] = "List",
										},
									},
									["string"] = "",
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						["string"] = "",
						["type"] = {
							["string"] = "Choice",
						},
					},
					["string"] = "",
					["prefix"] = {
						["string"] = "!",
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[2] = {
					["string"] = "",
					["grabs"] = {
						["string"] = "",
						[2] = {
							["repetition"] = {
								["string"] = "*",
							},
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["type"] = {
									["string"] = "Class",
								},
								["value"] = {
									["string"] = "a-zA-Z_0-9",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						[1] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["type"] = {
									["string"] = "Class",
								},
								["value"] = {
									["string"] = "a-zA-Z_",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "CollectInput",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["slash"] = {
		["string"] = "",
		["name"] = {
			["string"] = "slash",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "/",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["begin"] = {
		["string"] = "",
		["name"] = {
			["string"] = "begin",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "<",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Error"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Error",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "close",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["errorhandling"] = {
						["message"] = {
							["string"] = "\')\' expected after errormessage",
						},
						["string"] = "",
						["type"] = {
							["string"] = "error",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "errorhandling",
					},
					["grabs"] = {
						[1] = {
							["string"] = "",
							["name"] = {
								["string"] = "type",
							},
							["grabs"] = {
								["string"] = "error",
							},
							["type"] = {
								["string"] = "FixedNode",
							},
						},
						[2] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["type"] = {
									["string"] = "String",
								},
								["value"] = {
									["string"] = "error",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						[3] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "ForcedSeparation",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						[4] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "open",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["errorhandling"] = {
								["message"] = {
									["string"] = "\'(\' expected after \'error\'.",
								},
								["string"] = "",
								["type"] = {
									["string"] = "error",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
						[5] = {
							["string"] = "",
							["name"] = {
								["string"] = "message",
							},
							["grabs"] = {
								["string"] = "",
								[1] = {
									["string"] = "",
									["pattern"] = {
										["string"] = "",
										["name"] = {
											["string"] = "String",
										},
										["type"] = {
											["string"] = "RuleName",
										},
									},
									["errorhandling"] = {
										["message"] = {
											["string"] = "Expected \'errormessage\'.",
										},
										["string"] = "",
										["type"] = {
											["string"] = "error",
										},
									},
									["type"] = {
										["string"] = "Pattern",
									},
								},
								["type"] = {
									["string"] = "List",
								},
							},
							["type"] = {
								["string"] = "GrabbingNode",
							},
						},
						["string"] = "",
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["AnonymousGrabbingNode"] = {
		["string"] = "",
		["name"] = {
			["string"] = "AnonymousGrabbingNode",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "AnonymousGrabbingNode",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "colon",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[3] = {
					["string"] = "",
					["name"] = {
						["string"] = "grabs",
					},
					["grabs"] = {
						["string"] = "",
						[1] = {
							["repetition"] = {
								["string"] = "?",
							},
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "List",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["String"] = {
		["string"] = "",
		["name"] = {
			["string"] = "String",
		},
		["rule"] = {
			["string"] = "",
			[2] = {
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "\"",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[2] = {
					["string"] = "",
					["grabs"] = {
						["string"] = "",
						[1] = {
							["repetition"] = {
								["string"] = "*",
							},
							["string"] = "",
							["pattern"] = {
								[1] = {
									["string"] = "",
									[2] = {
										["string"] = "",
										["pattern"] = {
											[1] = {
												["string"] = "",
												[1] = {
													["repetition"] = {
														["string"] = "?",
													},
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["type"] = {
															["string"] = "String",
														},
														["value"] = {
															["string"] = "\\\"",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[2] = {
												["string"] = "",
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["type"] = {
															["string"] = "Dot",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											["type"] = {
												["string"] = "Choice",
											},
											["string"] = "",
										},
										["type"] = {
											["string"] = "Pattern",
										},
									},
									[1] = {
										["pattern"] = {
											["string"] = "",
											["value"] = {
												["string"] = "\"",
											},
											["type"] = {
												["string"] = "String",
											},
										},
										["string"] = "",
										["prefix"] = {
											["string"] = "!",
										},
										["type"] = {
											["string"] = "Pattern",
										},
									},
									["type"] = {
										["string"] = "List",
									},
								},
								["string"] = "",
								["type"] = {
									["string"] = "Choice",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "CollectInput",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "\"",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[4] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			[1] = {
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "\'",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[2] = {
					["string"] = "",
					["grabs"] = {
						["string"] = "",
						[1] = {
							["repetition"] = {
								["string"] = "*",
							},
							["string"] = "",
							["pattern"] = {
								[1] = {
									["string"] = "",
									[2] = {
										["string"] = "",
										["pattern"] = {
											[1] = {
												["string"] = "",
												[1] = {
													["repetition"] = {
														["string"] = "?",
													},
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["type"] = {
															["string"] = "String",
														},
														["value"] = {
															["string"] = "\\\'",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[2] = {
												["string"] = "",
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["type"] = {
															["string"] = "Dot",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											["type"] = {
												["string"] = "Choice",
											},
											["string"] = "",
										},
										["type"] = {
											["string"] = "Pattern",
										},
									},
									[1] = {
										["pattern"] = {
											["string"] = "",
											["value"] = {
												["string"] = "\'",
											},
											["type"] = {
												["string"] = "String",
											},
										},
										["string"] = "",
										["prefix"] = {
											["string"] = "!",
										},
										["type"] = {
											["string"] = "Pattern",
										},
									},
									["type"] = {
										["string"] = "List",
									},
								},
								["string"] = "",
								["type"] = {
									["string"] = "Choice",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "CollectInput",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "\'",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[4] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Comment"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Comment",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["repetition"] = {
						["string"] = "*",
					},
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "Class",
						},
						["value"] = {
							["string"] = "^\r\n",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "#",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Class"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Class",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "\[",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[2] = {
					["string"] = "",
					["grabs"] = {
						[1] = {
							["repetition"] = {
								["string"] = "?",
							},
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["type"] = {
									["string"] = "String",
								},
								["value"] = {
									["string"] = "^",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						[2] = {
							["repetition"] = {
								["string"] = "?",
							},
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["type"] = {
									["string"] = "String",
								},
								["value"] = {
									["string"] = "\]",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						[3] = {
							["repetition"] = {
								["string"] = "*",
							},
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["type"] = {
									["string"] = "Class",
								},
								["value"] = {
									["string"] = "^\]",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
						["string"] = "",
					},
					["type"] = {
						["string"] = "CollectInput",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "\]",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[4] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["FixedNode"] = {
		["string"] = "",
		["name"] = {
			["string"] = "FixedNode",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "FixedNode",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				[2] = {
					["grabs"] = {
						["string"] = "",
						[1] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "Identifier",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["string"] = "",
					["name"] = {
						["string"] = "name",
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "colon2",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[4] = {
					["string"] = "",
					["name"] = {
						["string"] = "grabs",
					},
					["grabs"] = {
						["string"] = "",
						[1] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "String",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["errorhandling"] = {
								["message"] = {
									["string"] = "Expected string after \'(Node::\'.",
								},
								["string"] = "",
								["type"] = {
									["string"] = "error",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["ForcedSeparation"] = {
		["string"] = "",
		["name"] = {
			["string"] = "ForcedSeparation",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["pattern"] = {
						["string"] = "",
						["value"] = {
							["string"] = "^%w",
						},
						["type"] = {
							["string"] = "Class",
						},
					},
					["string"] = "",
					["prefix"] = {
						["string"] = "&",
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["ErrorHandling"] = {
		["string"] = "",
		["name"] = {
			["string"] = "ErrorHandling",
		},
		["rule"] = {
			["string"] = "",
			[2] = {
				["string"] = "",
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Error",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			[1] = {
				["string"] = "",
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Warning",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["query"] = {
		["string"] = "",
		["name"] = {
			["string"] = "query",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "?",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["EndOfFile"] = {
		["string"] = "",
		["name"] = {
			["string"] = "EndOfFile",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[1] = {
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "Dot",
						},
					},
					["string"] = "",
					["prefix"] = {
						["string"] = "!",
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["star"] = {
		["string"] = "",
		["name"] = {
			["string"] = "star",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "*",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Rule"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Rule",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["grabs"] = {
						[1] = {
							["string"] = "",
							["name"] = {
								["string"] = "type",
							},
							["grabs"] = {
								["string"] = "Rule",
							},
							["type"] = {
								["string"] = "FixedNode",
							},
						},
						[2] = {
							["string"] = "",
							["name"] = {
								["string"] = "name",
							},
							["grabs"] = {
								["string"] = "",
								[1] = {
									["string"] = "",
									["pattern"] = {
										["string"] = "",
										["name"] = {
											["string"] = "Identifier",
										},
										["type"] = {
											["string"] = "RuleName",
										},
									},
									["type"] = {
										["string"] = "Pattern",
									},
								},
								["type"] = {
									["string"] = "List",
								},
							},
							["type"] = {
								["string"] = "GrabbingNode",
							},
						},
						[3] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "leftarrow",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["errorhandling"] = {
								["message"] = {
									["string"] = "\'<-\' expected after name of rule.",
								},
								["string"] = "",
								["type"] = {
									["string"] = "error",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						[4] = {
							["string"] = "",
							["name"] = {
								["string"] = "rule",
							},
							["grabs"] = {
								["string"] = "",
								[1] = {
									["string"] = "",
									["pattern"] = {
										["string"] = "",
										["name"] = {
											["string"] = "Choice",
										},
										["type"] = {
											["string"] = "RuleName",
										},
									},
									["errorhandling"] = {
										["message"] = {
											["string"] = "\'Pattern / ... / ...\' expected after \'<-\'.",
										},
										["string"] = "",
										["type"] = {
											["string"] = "error",
										},
									},
									["type"] = {
										["string"] = "Pattern",
									},
								},
								["type"] = {
									["string"] = "List",
								},
							},
							["type"] = {
								["string"] = "GrabbingNode",
							},
						},
						["type"] = {
							["string"] = "List",
						},
						["string"] = "",
					},
					["type"] = {
						["string"] = "AnonymousGrabbingNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Spacing"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Spacing",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[1] = {
					["repetition"] = {
						["string"] = "*",
					},
					["string"] = "",
					["pattern"] = {
						[1] = {
							["string"] = "",
							[1] = {
								["repetition"] = {
									["string"] = "+",
								},
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["type"] = {
										["string"] = "Class",
									},
									["value"] = {
										["string"] = "%s",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						[2] = {
							["string"] = "",
							[1] = {
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["name"] = {
										["string"] = "Comment",
									},
									["type"] = {
										["string"] = "RuleName",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						["type"] = {
							["string"] = "Choice",
						},
						["string"] = "",
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["dot"] = {
		["string"] = "",
		["name"] = {
			["string"] = "dot",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = ".",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["not"] = {
		["string"] = "",
		["name"] = {
			["string"] = "not",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "!",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["colon2"] = {
		["string"] = "",
		["name"] = {
			["string"] = "colon2",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "::",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["end"] = {
		["string"] = "",
		["name"] = {
			["string"] = "end",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = ">",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["FunctionCall"] = {
		["string"] = "",
		["name"] = {
			["string"] = "FunctionCall",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "FunctionCall",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "{:",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[4] = {
					["string"] = "",
					["name"] = {
						["string"] = "function",
					},
					["grabs"] = {
						["string"] = "",
						[1] = {
							["string"] = "",
							["pattern"] = {
								[1] = {
									["string"] = "",
									[1] = {
										["string"] = "",
										["grabs"] = {
											["string"] = "",
											[2] = {
												["repetition"] = {
													["string"] = "*",
												},
												["string"] = "",
												["pattern"] = {
													["string"] = "",
													["type"] = {
														["string"] = "Class",
													},
													["value"] = {
														["string"] = "a-zA-Z_.:0-9",
													},
												},
												["type"] = {
													["string"] = "Pattern",
												},
											},
											[1] = {
												["string"] = "",
												["pattern"] = {
													["string"] = "",
													["type"] = {
														["string"] = "Class",
													},
													["value"] = {
														["string"] = "a-zA-Z_",
													},
												},
												["type"] = {
													["string"] = "Pattern",
												},
											},
											["type"] = {
												["string"] = "List",
											},
										},
										["type"] = {
											["string"] = "CollectInput",
										},
									},
									["type"] = {
										["string"] = "List",
									},
								},
								["string"] = "",
								["type"] = {
									["string"] = "Choice",
								},
							},
							["errorhandling"] = {
								["message"] = {
									["string"] = "Name of function expected after \'{:\'.",
								},
								["string"] = "",
								["type"] = {
									["string"] = "error",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				[5] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[6] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "}",
						},
					},
					["errorhandling"] = {
						["message"] = {
							["string"] = "Expected \'}\' after name of function.",
						},
						["string"] = "",
						["type"] = {
							["string"] = "error",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[7] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["string"] = "",
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["List"] = {
		["string"] = "",
		["name"] = {
			["string"] = "List",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["repetition"] = {
						["string"] = "+",
					},
					["string"] = "",
					["pattern"] = {
						[1] = {
							["string"] = "",
							[1] = {
								["string"] = "",
								["grabs"] = {
									["string"] = "",
									[1] = {
										["string"] = "",
										["pattern"] = {
											[1] = {
												["string"] = "",
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "Collecting",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											[2] = {
												["string"] = "",
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "Pattern",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											["type"] = {
												["string"] = "Choice",
											},
											[3] = {
												["string"] = "",
												[1] = {
													["string"] = "",
													["pattern"] = {
														["string"] = "",
														["name"] = {
															["string"] = "FunctionCall",
														},
														["type"] = {
															["string"] = "RuleName",
														},
													},
													["type"] = {
														["string"] = "Pattern",
													},
												},
												["type"] = {
													["string"] = "List",
												},
											},
											["string"] = "",
										},
										["type"] = {
											["string"] = "Pattern",
										},
									},
									["type"] = {
										["string"] = "List",
									},
								},
								["type"] = {
									["string"] = "AnonymousGrabbingNode",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						["string"] = "",
						["type"] = {
							["string"] = "Choice",
						},
					},
					["errorhandling"] = {
						["message"] = {
							["string"] = "Expected Pattern, \'resume\', < ... > or { ... }.",
						},
						["string"] = "",
						["type"] = {
							["string"] = "error",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "List",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["open"] = {
		["string"] = "",
		["name"] = {
			["string"] = "open",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "(",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["CollectInput"] = {
		["string"] = "",
		["name"] = {
			["string"] = "CollectInput",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["name"] = {
						["string"] = "grabs",
					},
					["grabs"] = {
						["string"] = "",
						[1] = {
							["repetition"] = {
								["string"] = "?",
							},
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "List",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "CollectInput",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Choice"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Choice",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "Choice",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				[2] = {
					["string"] = "",
					["grabs"] = {
						["string"] = "",
						[1] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "List",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "AnonymousGrabbingNode",
					},
				},
				[3] = {
					["repetition"] = {
						["string"] = "*",
					},
					["string"] = "",
					["pattern"] = {
						[1] = {
							["string"] = "",
							[2] = {
								["string"] = "",
								["grabs"] = {
									["string"] = "",
									[1] = {
										["string"] = "",
										["pattern"] = {
											["string"] = "",
											["name"] = {
												["string"] = "List",
											},
											["type"] = {
												["string"] = "RuleName",
											},
										},
										["type"] = {
											["string"] = "Pattern",
										},
									},
									["type"] = {
										["string"] = "List",
									},
								},
								["type"] = {
									["string"] = "AnonymousGrabbingNode",
								},
							},
							[1] = {
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["name"] = {
										["string"] = "slash",
									},
									["type"] = {
										["string"] = "RuleName",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						["string"] = "",
						["type"] = {
							["string"] = "Choice",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Grammar"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Grammar",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[2] = {
					["repetition"] = {
						["string"] = "+",
					},
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Rule",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["errorhandling"] = {
						["message"] = {
							["string"] = "Expected at least one rule in file.",
						},
						["string"] = "",
						["type"] = {
							["string"] = "error",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "EndOfFile",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["plus"] = {
		["string"] = "",
		["name"] = {
			["string"] = "plus",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "+",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Collecting"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Collecting",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "begin",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[2] = {
					["string"] = "",
					["pattern"] = {
						[1] = {
							["string"] = "",
							[1] = {
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["name"] = {
										["string"] = "AnonymousFixedNode",
									},
									["type"] = {
										["string"] = "RuleName",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						[2] = {
							["string"] = "",
							[1] = {
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["name"] = {
										["string"] = "AnonymousGrabbingNode",
									},
									["type"] = {
										["string"] = "RuleName",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						[5] = {
							["string"] = "",
							[1] = {
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["name"] = {
										["string"] = "CollectInput",
									},
									["type"] = {
										["string"] = "RuleName",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						["type"] = {
							["string"] = "Choice",
						},
						[3] = {
							["string"] = "",
							[1] = {
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["name"] = {
										["string"] = "FixedNode",
									},
									["type"] = {
										["string"] = "RuleName",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						[4] = {
							["string"] = "",
							[1] = {
								["string"] = "",
								["pattern"] = {
									["string"] = "",
									["name"] = {
										["string"] = "GrabbingNode",
									},
									["type"] = {
										["string"] = "RuleName",
									},
								},
								["type"] = {
									["string"] = "Pattern",
								},
							},
							["type"] = {
								["string"] = "List",
							},
						},
						["string"] = "",
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "end",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["errorhandling"] = {
						["message"] = {
							["string"] = "Expected \'>\' after \'<...\'.",
						},
						["string"] = "",
						["type"] = {
							["string"] = "error",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["Warning"] = {
		["string"] = "",
		["name"] = {
			["string"] = "Warning",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "close",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["errorhandling"] = {
						["message"] = {
							["string"] = "\')\' expected after warningmessage",
						},
						["string"] = "",
						["type"] = {
							["string"] = "error",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "errorhandling",
					},
					["grabs"] = {
						[1] = {
							["string"] = "",
							["name"] = {
								["string"] = "type",
							},
							["grabs"] = {
								["string"] = "warning",
							},
							["type"] = {
								["string"] = "FixedNode",
							},
						},
						[2] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["type"] = {
									["string"] = "String",
								},
								["value"] = {
									["string"] = "warning",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						[3] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "ForcedSeparation",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						[4] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "open",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["errorhandling"] = {
								["message"] = {
									["string"] = "\'(\' expected after \'warning\'.",
								},
								["string"] = "",
								["type"] = {
									["string"] = "error",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
						[5] = {
							["string"] = "",
							["name"] = {
								["string"] = "message",
							},
							["grabs"] = {
								["string"] = "",
								[1] = {
									["string"] = "",
									["pattern"] = {
										["string"] = "",
										["name"] = {
											["string"] = "String",
										},
										["type"] = {
											["string"] = "RuleName",
										},
									},
									["errorhandling"] = {
										["message"] = {
											["string"] = "Expected \"warningmessage\".",
										},
										["string"] = "",
										["type"] = {
											["string"] = "error",
										},
									},
									["type"] = {
										["string"] = "Pattern",
									},
								},
								["type"] = {
									["string"] = "List",
								},
							},
							["type"] = {
								["string"] = "GrabbingNode",
							},
						},
						["string"] = "",
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["GrabbingNode"] = {
		["string"] = "",
		["name"] = {
			["string"] = "GrabbingNode",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				[1] = {
					["string"] = "",
					["name"] = {
						["string"] = "type",
					},
					["grabs"] = {
						["string"] = "GrabbingNode",
					},
					["type"] = {
						["string"] = "FixedNode",
					},
				},
				[2] = {
					["string"] = "",
					["name"] = {
						["string"] = "name",
					},
					["grabs"] = {
						["string"] = "",
						[1] = {
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "Identifier",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				[3] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "colon",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[4] = {
					["string"] = "",
					["name"] = {
						["string"] = "grabs",
					},
					["grabs"] = {
						["string"] = "",
						[1] = {
							["repetition"] = {
								["string"] = "?",
							},
							["string"] = "",
							["pattern"] = {
								["string"] = "",
								["name"] = {
									["string"] = "List",
								},
								["type"] = {
									["string"] = "RuleName",
								},
							},
							["type"] = {
								["string"] = "Pattern",
							},
						},
						["type"] = {
							["string"] = "List",
						},
					},
					["type"] = {
						["string"] = "GrabbingNode",
					},
				},
				["type"] = {
					["string"] = "List",
				},
				["string"] = "",
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["and"] = {
		["string"] = "",
		["name"] = {
			["string"] = "and",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Spacing",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["type"] = {
							["string"] = "String",
						},
						["value"] = {
							["string"] = "&",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
	["IdentifierWithoutLeftArrow"] = {
		["string"] = "",
		["name"] = {
			["string"] = "IdentifierWithoutLeftArrow",
		},
		["rule"] = {
			["string"] = "",
			[1] = {
				["string"] = "",
				[2] = {
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "leftarrow",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["string"] = "",
					["prefix"] = {
						["string"] = "!",
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				[1] = {
					["string"] = "",
					["pattern"] = {
						["string"] = "",
						["name"] = {
							["string"] = "Identifier",
						},
						["type"] = {
							["string"] = "RuleName",
						},
					},
					["type"] = {
						["string"] = "Pattern",
					},
				},
				["type"] = {
					["string"] = "List",
				},
			},
			["type"] = {
				["string"] = "Choice",
			},
		},
		["type"] = {
			["string"] = "Rule",
		},
	},
}


return epeg
