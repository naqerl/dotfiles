local M = {}

M.setup = function()
	local set = vim.keymap.set
	set('n', '<leader>.', require('functions').query_path, { desc = 'Create file and directories if needed' })
	set('n', '<leader>Rc', function()
		vim.cmd "so ~/.config/nvim/lua/base.lua"
		vim.cmd "so ~/.config/nvim/lua/mappings.lua"
		vim.cmd "so ~/.config/nvim/lua/refactoring.lua"
		vim.notify('Custom modules reimported')
	end, { desc = '[R]estart [C]onfig' })
end

return M
