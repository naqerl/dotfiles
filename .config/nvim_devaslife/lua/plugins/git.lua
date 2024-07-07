return {
	{
		"dinhhuy258/git.nvim",
		event = "BufReadPre",
		opts = {
			keymaps = {
				-- Open blame window
				blame = "<Leader>gb",
				-- Open file/folder in git repository
				browse = "<Leader>go",
			},
		},
	},

	{
		"tpope/vim-fugitive",
		event = "BufReadPre",
	},
	{
		"tpope/vim-rhubarb",
		event = "BufReadPre",
	},
	{
		-- Adds git releated signs to the gutter, as well as utilities for managing changes
		"lewis6991/gitsigns.nvim",
		event = "BufReadPre",
		keys = {
			{
				"]g",
				function()
					require("gitsigns").next_hunk()
				end,
				desc = "Next git hunk",
			},
			{
				"[g",
				function()
					require("gitsigns").prev_hunk()
				end,
				desc = "Previous git hunk",
			},
			{
				"<leader>hp",
				function()
					require("gitsigns").preview_hunk()
				end,
				desc = "[H]unk [P]review",
			},
			{ "<leader>og", ":vertical Git<CR><C-w>o", desc = "[O]pen [G]it" },
		},
		config = function()
			require("gitsigns").setup()
		end,
	},
	{
		"idanarye/vim-merginal",
		cmd = { "Merginal" },
	},
}
