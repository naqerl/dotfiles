local M = {
	'nvim-telescope/telescope.nvim',
}

M.dependencies = {
	'nvim-lua/plenary.nvim',
	'ThePrimeagen/harpoon',
	'nvim-tree/nvim-web-devicons',
	'nvim-telescope/telescope-project.nvim',
}

M.keys = {
	{ '<leader>ma', function() require('harpoon.mark').add_file() end },
	{ '<leader>ml', function() require('harpoon.ui').toggle_quick_menu() end },
	{ '<leader>h',  function() require('harpoon.ui').nav_prev() end },
	{ '<leader>l',  function() require('harpoon.ui').nav_next() end },
	{ '<leader>j',  ':cn<CR>' },
	{ '<leader>k',  ':cp<CR>' },

	{ '<leader>p',  ':lua require"telescope".extensions.project.project{}<CR>',              silent = true },

	{ '<leader>?',  function() require('telescope.builtin').oldfiles() end,                  desc = 'Find recently opened files' },
	{ '<leader>/',  function() require('telescope.builtin').current_buffer_fuzzy_find() end, desc = 'Fuzzily search in current buffer' },
	{ '<leader>gf', function() require('telescope.builtin').git_files() end,                 desc = 'Search git files' },
	{ '<leader>gs', function() require('telescope.builtin').git_status() end,                desc = 'Search git status' },
	{ '<leader>f',  function() require('telescope.builtin').find_files() end,                desc = 'Find files' },
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
	{ '<leader>;',  function() require('telescope.builtin').live_grep() end,   desc = 'Search by grep' },
	{ '<leader>sh', function() require('telescope.builtin').help_tags() end,   desc = 'Search help' },
	{ '<leader>sb', function() require('telescope.builtin').buffers() end,     desc = 'Find existing buffers' },
	{ "<leader>'",  function() require('telescope.builtin').resume() end,      desc = 'Open last telescope picker' },
	{ "<leader>sd", function() require('telescope.builtin').diagnostics() end, desc = 'Search diagnostics' },
	{
		'<leader>gw',
		function() require('telescope').extensions.git_worktree.git_worktrees() end,
		desc = 'Git worktree switch'
	},
	{
		'<leader>gW',
		function() require('telescope').extensions.git_worktree.create_git_worktree() end,
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
	local project_actions = require("telescope._extensions.project.actions")

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
				find_command = {
					'fd',
					'--type',
					'f',
					'--follow',
				},
				wrap_results = true,
				path_display = function(_opts, path)
					local tail = require("telescope.utils").path_tail(path)
					return string.format("%s (%s)", tail, path)
				end,
				sorting_strategy = "ascending",
				previewer = false,
				layout_config = {
					width = 0.5,
					height = 0.5,
					prompt_position = "top",
				},
			},
			live_grep = {
				theme = "ivy",
				additional_args = { "--hidden" }
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
		extensions = {
			project = {
				base_dirs = {
					{ path = '~/code/',     max_depth = 5 },
					{ path = '~/dotfiles/', max_depth = 1 },
				},
				theme = "dropdown",
				order_by = "asc",
				search_by = "title",
				on_project_selected = function(prompt_bufnr)
					project_actions.change_working_directory(prompt_bufnr, false)
					-- require("harpoon.ui").nav_file(1)
					require('telescope.builtin').find_files()
				end
			}
		}
	}

	telescope.setup(config)

	-- pcall(telescope.load_extension, 'fzf')
	pcall(telescope.load_extension, 'harpoon')
	pcall(telescope.load_extension, 'git_worktree')
	pcall(telescope.load_extension, 'dap')
	pcall(telescope.load_extension, 'notify')
	pcall(telescope.load_extension, 'projects')
end

return M
