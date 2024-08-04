local M = {
	'nvim-telescope/telescope.nvim',
}

M.lazy = true

M.dependencies = {
	'nvim-lua/plenary.nvim',
	'ThePrimeagen/harpoon',
	'nvim-tree/nvim-web-devicons',
}

M.keys = {
	{ '<leader>ma', function() require('harpoon.mark').add_file() end },
	{ '<leader>ml', function() require('harpoon.ui').toggle_quick_menu() end },
	{ '<leader>h',  function() require('harpoon.ui').nav_prev() end },
	{ '<leader>l',  function() require('harpoon.ui').nav_next() end },
	{ '<leader>j',  ':cn<CR>' },
	{ '<leader>k',  ':cp<CR>' },

	{ '<leader>?',  require('telescope.builtin').oldfiles,                   desc = 'Find recently opened files' },
	{ '<leader>/',  require('telescope.builtin').current_buffer_fuzzy_find,  desc = 'Fuzzily search in current buffer' },
	{ '<leader>gf', require('telescope.builtin').git_files,                  desc = 'Search git files' },
	{ '<leader>gs', require('telescope.builtin').git_status,                 desc = 'Search git status' },
	{ '<leader>f',  require('telescope.builtin').find_files,                 desc = 'Find files' },
	{
		mode = 'v',
		'<leader>f',
		'"zy:Telescope find_files default_text=<C-R>z<CR>',
		desc = 'Find file with selected name'
	},
	{
		mode = 'v',
		'<leader>;',
		'"zy:Telescope live_grep default_text=<C-R>z<CR>',
		desc = 'Find selected name in files'
	},
	{ '<leader>;',  require('telescope.builtin').live_grep, desc = 'Search by grep' },
	{ '<leader>sh', require('telescope.builtin').help_tags, desc = 'Search help' },
	{ '<leader>sb', require('telescope.builtin').buffers,   desc = 'Find existing buffers' },
	{ '<leader>bl', require('telescope.builtin').buffers,   desc = 'Find existing buffers' },
	{ "<leader>'",  require('telescope.builtin').resume,    desc = 'Open last telescope picker' },
	{
		'<leader>gw',
		require('telescope').extensions.git_worktree.git_worktrees,
		desc = 'Git worktree switch'
	},
	{
		'<leader>gW',
		require('telescope').extensions.git_worktree.create_git_worktree,
		desc = 'Git worktree create'
	},

	{
		"<leader>sf",
		function() require('telescope.builtin').lsp_document_symbols({ default_text = ':function: ' }) end,
		desc = '[S]earch [F]unctions'
	},
	{
		"<leader>sm",
		function() require('telescope.builtin').lsp_document_symbols({ default_text = ':method: ' }) end,
		desc = '[S]earch [F]unctions'
	},

	{
		"<leader>sc",
		function() require('telescope.builtin').lsp_document_symbols({ default_text = ':class: ' }) end,
		desc = '[S]search [C]lasses'
	}
}

function M.config()
	local telescope = require('telescope')
	local config = {
		defaults = {
			prompt_prefix = '❯ ',
			selection_caret = '  ',
			mappings = {
				i = {
					['<C-u>'] = false,
					['<C-d>'] = false,
				},
			},
		},
		pickers = {
			find_files = {
				wrap_results = true,
				path_display = function(_opts, path)
					local tail = require("telescope.utils").path_tail(path)
					return string.format("%s (%s)", tail, path)
				end,
				-- sorting_strategy = "ascending",
				previewer = false,
				layout_config = {
					width = 0.5,
					height = 0.5,
					prompt_position = "top",
				},
			},
			live_grep = {
				theme = "ivy",
			},
			help_tags = {
				theme = "ivy",
			},
			diagnostics = {
				theme = "ivy",
			},
			buffers = {
				previewer = false,
				theme = "dropdown",
			},
			current_buffer_fuzzy_find = {
				previewer = false,
				theme = "dropdown",
			},
			git_status = {
				theme = "ivy",
			},
			git_branches = {
				theme = "ivy",
			},
			lsp_references = {
				theme = "ivy",
				previewer = false,
			},
		},
	}

	telescope.setup(config)

	-- pcall(telescope.load_extension, 'fzf')
	pcall(telescope.load_extension, 'harpoon')
	pcall(telescope.load_extension, 'git_worktree')
	pcall(telescope.load_extension, 'dap')
	pcall(telescope.load_extension, 'notify')
end

return M
