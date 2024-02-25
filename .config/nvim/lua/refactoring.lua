local functions                               = require('functions')

local M                                       = {}
local Utils                                   = {}
local Data                                    = {}

Data.possible_indent_symbols                  = {
	' ',
	'	',
}

Data.extraction_parent_nodes                  = {
	lua = {
		'if_statement',
		'arguments',
		'table_constructor',
	},
	python = {
		'return_statement',
		'if_statement',
		'call',
		'elif_clause',
		'comparison_operator'
	}
}

---@type table<string, function>
Data.variable_declaration_constructors        = {}

Data.variable_declaration_constructors.lua    = function(name, expression, indent)
	return indent .. 'local ' .. name .. ' = ' .. expression
end

Data.variable_declaration_constructors.python = function(name, expression, indent)
	return indent .. name .. ' = ' .. expression
end


---@param str string String with indent that should be extracted
Utils.extract_indent = function(str)
	local indent = ''

	for i = 1, #Data.possible_indent_symbols do
		local symbol = Data.possible_indent_symbols[i]
		indent = indent .. string.rep(symbol, Utils.count_leading_chars(str, symbol))
	end
	return indent
end

---@param str string
---@param char string
Utils.count_leading_chars = function(str, char)
	local count = 0
	for i = 1, #str do
		local c = str:sub(i, i)
		if c == char then
			count = count + 1
		else
			break
		end
	end
	return count
end

---@param node TSNode|nil Start node for extending
---@param range table<integer, integer, integer, integer> Range for extending
---@return TSNode|nil
Utils.extend_node_to_range = function(node, range)
	if not node then
		vim.notify('Failed to find expression to extract', vim.log.levels.ERROR)
		return node
	end

	local node_range = { node:range() }

	while not functions.tables_eq(node_range, range) do
		node = node:parent()
		if not node then
			vim.notify('Failed to find expression to extract', vim.log.levels.ERROR)
			return nil
		end
		node_range = { node:range() }
	end
	return node
end

---@param node TSNode
---@return TSNode|nil
Utils.find_parent_of_extraction = function(node)
	local ft = vim.bo.filetype
	local should_stop = false
	local parent = node:parent()
	local types2stop = Data.extraction_parent_nodes[ft]

	while parent ~= nil do
		for i = 1, #types2stop do
			if parent:type() == types2stop[i] then
				should_stop = true
				break
			end
		end

		if should_stop then
			break
		end

		parent = parent:parent()
	end

	return parent
end

M.setup = function()
	vim.keymap.set('v', '<leader>re', M.extract_variable, { desc = "[R]efactor [E]xtract" })
end

function M.extract_variable()
	local range = functions.get_visual_selection_range()
	local ft = vim.bo.filetype

	local node = Utils.extend_node_to_range(vim.treesitter.get_node(), range)
	if not node then return end

	local parent = Utils.find_parent_of_extraction(node)

	if not parent then
		vim.notify('Failed to found parent node', vim.log.levels.ERROR)
		return
	end

	-- Expression that will be extracted
	local to_extrcat_expression = vim.treesitter.get_node_text(node, 0)
	local to_extract_range = { node:range() }

	local declaration_linenr = parent:range()

	local insert_lines = vim.api.nvim_buf_get_lines(0, declaration_linenr, declaration_linenr + 1, true)
	local indent = Utils.extract_indent(insert_lines[1])

	vim.ui.input({ prompt = "Variable name: " }, function(name)
		local lines = { Data.variable_declaration_constructors[ft](name, to_extrcat_expression, indent) }
		vim.api.nvim_buf_set_text(0, to_extract_range[1], to_extract_range[2], to_extract_range[3], to_extract_range[4],
			{ name })
		vim.api.nvim_buf_set_lines(0, declaration_linenr, declaration_linenr, false, lines)
	end)

	local esc = vim.api.nvim_replace_termcodes('<esc>', true, false, true)
	vim.api.nvim_feedkeys(esc, 'x', false)
end

return M
