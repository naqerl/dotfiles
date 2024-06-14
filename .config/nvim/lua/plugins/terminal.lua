local M = {
	'rebelot/terminal.nvim'
}

M.lazy = true

M.keys = {
	{ "<leader>ts", function() require('terminal.mappings').operator_send() end,                             { expr = true } },
	{
		mode = { "n", "x" },
		"<leader>to",
		function()
			require('terminal.mappings').toggle()
		end
	},
	{ "<leader>tO", function() require('terminal.mappings').toggle({ open_cmd = "enew" }) end },
	{ "<leader>tr", function() require('terminal.mappings').run() end },
	{ "<leader>tR", function() require('terminal.mappings').run(nil, { layout = { open_cmd = "enew" } }) end },
	{ "<leader>tk", function() require('terminal.mappings').kill() end },
	{ "<leader>t]", function() require('terminal.mappings').cycle_next() end },
	{ "<leader>t[", function() require('terminal.mappings').cycle_prev() end },
	{ "<leader>tl", function() require('terminal.mappings').move({ open_cmd = "belowright vnew" }) end },
	{ "<leader>tL", function() require('terminal.mappings').move({ open_cmd = "botright vnew" }) end },
	{ "<leader>th", function() require('terminal.mappings').move({ open_cmd = "belowright new" }) end },
	{ "<leader>tH", function() require('terminal.mappings').move({ open_cmd = "botright new" }) end },
	{ "<leader>tf", function() require('terminal.mappings').move({ open_cmd = "float" }) end },
}

function M.config()
	require('terminal').setup({
		layout = { open_cmd = "botright new" },
		cmd = { "zsh" },
		autoclose = false,
	})
end

return M
