local M = {}

---@param path string
function M.split_path(path)
	local separator = package.config:sub(1, 1)
	local last_separator = path:find(separator .. "[^" .. separator .. "]*$")
	if last_separator then
		return path:sub(1, last_separator - 1), path:sub(last_separator + 1)
	else
		return "", path
	end
end

---@param path string
function M.create_dirs(path)
	local dir, _ = M.split_path(path)
	local current_path = vim.fn.getcwd() .. '/' .. dir

	local stat = vim.loop.fs_stat(current_path)
	if stat ~= nil then
		return
	end

	local input = vim.fn.inputlist({
		"Create new directories: " .. current_path,
		"1. Yes",
		"2. No",
	})

	if input == 1 then
		os.execute("mkdir -p " .. current_path)
	end
end

function M.query_path()
	vim.ui.input({ prompt = "Path: ", completion = "file" }, function(path)
		if not path then
			return
		end

		M.create_dirs(path)
		vim.cmd({ cmd = 'e', args = { path } })
		vim.cmd('w')
	end)
end

M.get_cursor = function()
	local cursor = vim.api.nvim_win_get_cursor(0)
	return { row = cursor[1], col = cursor[2] }
end

M.get_visual_selection_range = function()
	local vpos = vim.fn.getpos("v")
	local begin_pos = { row = vpos[2] - 1, col = vpos[3] - 1 }
	local end_pos = M.get_cursor()
	end_pos.row = end_pos.row - 1
	end_pos.col = end_pos.col + 1

	if begin_pos.col > end_pos.col then
		begin_pos, end_pos = end_pos, begin_pos
		begin_pos.col = begin_pos.col - 1
		end_pos.col = end_pos.col + 1
	end

	return {
		begin_pos.row,
		begin_pos.col,
		end_pos.row,
		end_pos.col
	}
end

---@param lhs table
---@param rhs table
M.tables_eq = function(lhs, rhs)
	if type(lhs) ~= "table" or type(rhs) ~= "table" then
		return false
	end

	if #lhs ~= #rhs then
		return false
	end

	for key, value in pairs(lhs) do
		if type(value) == "table" then
			if not M.tables_eq(value, rhs[key]) then
				return false
			end
		elseif value ~= rhs[key] then
			return false
		end
	end

	return true
end

return M
