local M = {
	"coffebar/neovim-project",
}

M.dependencies = {
	{ "nvim-lua/plenary.nvim" },
	{ "nvim-telescope/telescope.nvim", tag = "0.1.4" },
	{ "Shatur/neovim-session-manager" },
}

M.lazy = false
M.priority = 100

M.opts = {
	projects = { -- define project roots
		"~/code/work/pixelplex/*",
		"~/code/work/haas/*",
		"~/code/work/mirvr/",
		"~/code/personal/*",
		"~/dotfiles/",
	},
}
M.init = function()
	-- enable saving the state of plugins in the session
	vim.opt.sessionoptions:append("globals") -- save global variables that start with an uppercase letter and contain at least one lowercase letter.
end

return {}
