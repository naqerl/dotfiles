vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.o.breakindent = true
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.signcolumn = 'yes'
vim.o.timeoutlen = 300
vim.o.splitright = true
vim.o.splitbelow = true
vim.o.inccommand = 'split'
vim.o.confirm = true
vim.o.laststatus = 3
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.signcolumn = "auto"
vim.o.showmode = false
vim.o.cmdheight = 1
vim.o.guicursor = "i:block"
vim.o.colorcolumn = "80"
vim.o.wrap = false
vim.o.expandtab = true
vim.o.autoread = true

vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')
vim.keymap.set('n', ']c', '<cmd>cnext<cr>', { desc = "quick fix next" })
vim.keymap.set('n', '[c', '<cmd>cprev<cr>', { desc = "quick fix prev" })
vim.keymap.set('n', '<leader>oc', '<cmd>copen<cr>', { desc = "quick fix open" })
vim.keymap.set('n', 'gV', '`[v`]', { desc = "Reselect pasted" })
vim.keymap.set("v", "<M-p>", '"_dp', { desc = "Paster without yank" })
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Replace word under cursor" })
vim.keymap.set("v", "<leader>y", '"+y', { desc = "Copy to system clipboard" })
vim.keymap.set("v", "<leader>p", '"_dP', { desc = "Paste without saving" })
vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll down & center" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll up & center" })

-- Make wrapped liens navigation human friendly
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

vim.api.nvim_create_autocmd('TextYankPost', {
	desc = 'Highlight when yanking (copying) text',
	group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
	callback = function()
		vim.hl.on_yank()
	end,
})

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
	local out = vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
	if vim.v.shell_error ~= 0 then
		error('Error cloning lazy.nvim:\n' .. out)
	end
end

vim.opt.rtp:prepend(lazypath)
require('lazy').setup({
	{
		'lewis6991/gitsigns.nvim',
		event = 'VimEnter',
		config = function()
			require('gitsigns').setup({
				auto_attach = true,
				signs = {
					add = { text = '+' },
					change = { text = '~' },
					delete = { text = '_' },
					topdelete = { text = '‾' },
					changedelete = { text = '~' },
				},
			})
			local gitsigns = require('gitsigns')
			vim.keymap.set('n', "]h", function()
				gitsigns.nav_hunk('next')
			end)
			vim.keymap.set('n', "[h", function()
				gitsigns.nav_hunk('prev')
			end)
			vim.keymap.set('n', '<leader>hS', gitsigns.stage_buffer)
			vim.keymap.set('n', '<leader>hR', gitsigns.reset_buffer)
			vim.keymap.set('n', '<leader>hP', gitsigns.preview_hunk)
		end,
	},
	{ 'folke/todo-comments.nvim', event = 'VimEnter', dependencies = { 'nvim-lua/plenary.nvim' }, opts = { signs = false } },
	{
		'echasnovski/mini.nvim',
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects"
		},
		config = function()
			require('mini.surround').setup()
			require('mini.pairs').setup()
			require('mini.trailspace').setup()

			local spec_treesitter = require('mini.ai').gen_spec.treesitter
			require('mini.ai').setup {
				n_lines = 500,
				custom_textobjects = {
					F = spec_treesitter({ a = '@function.outer', i = '@function.inner' }),
					L = spec_treesitter({
						a = { '@conditional.outer', '@loop.outer' },
						i = { '@conditional.inner', '@loop.inner' },
					})
				}
			}

			local statusline = require 'mini.statusline'
			statusline.setup { use_icons = vim.g.have_nerd_font }
			statusline.section_location = function()
				return '%2l:%-2v'
			end
		end,
	},
	{
		'nvim-treesitter/nvim-treesitter',
		build = ':TSUpdate',
		config = function()
			require("nvim-treesitter.configs").setup({
				auto_install = true,
				highlight = {
					enable = true,
				},
				incremental_selection = {
					enable = true,
					keymaps = {
						init_selection = "<CR>",
						node_incremental = "<CR>",
						scope_incremental = "<Tab>",
						node_decremental = "<S-Tab>",
					},
				},
				indent = { enable = true },
			})
		end,
	},
	{
		'pechorin/any-jump.vim',
		config = function()
			vim.g.any_jump_grouping_enabled = 1
			vim.g.any_jump_window_width_ratio  = 1
			vim.g.any_jump_window_height_ratio = 1
			vim.g.any_jump_window_top_offset   = 0
			vim.g.any_jump_list_numbers = 0
			vim.g.any_jump_max_search_results = 100
		end,
		keys = {
			{ "<leader>j", "<cmd>AnyJump<cr>" },
		}
	},
	{ "powerman/vim-plugin-ruscmd" },
	{ "tpope/vim-rsi" },
	{ 
		"tpope/vim-fugitive",
		config = function()
			-- Create custom :G command that opens fugitive in full window
			vim.api.nvim_create_user_command('G', function(opts)
				vim.cmd('Git ' .. (opts.args or ''))
				vim.schedule(function()
					vim.cmd('only')
				end)
			end, { nargs = '*', complete = 'file' })
		end
	},
	{ "tpope/vim-rhubarb" },
	{
		'nmac427/guess-indent.nvim',
		config = function()
			require('guess-indent').setup({})
		end,
	},
	{
		'mikesmithgh/kitty-scrollback.nvim',
		lazy = true,
		cmd = { 'KittyScrollbackGenerateKittens', 'KittyScrollbackCheckHealth', 'KittyScrollbackGenerateCommandLineEditing' },
		event = { 'User KittyScrollbackLaunch' },
		config = function()
			require('kitty-scrollback').setup()
		end,
	},
	{
		'stevearc/oil.nvim',
		lazy = false,
		config = function()
			require("oil").setup({
				watch_for_changes = true,
				view_options = {
					show_hidden = true,
				},
				columns = {
					"permissions",
					"size",
					"mtime",
				},
				win_options = {
					signcolumn = "no",
					cursorcolumn = false,
				},
			})
			vim.keymap.set("n", "-", "<CMD>Oil<CR>")
		end
	},
	{
		"iamcco/markdown-preview.nvim",
		cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
		build = "cd app && pnpm install",
		init = function()
			vim.g.mkdp_filetypes = { "markdown" }
		end,
		ft = { "markdown" },
	},
	{
		"junegunn/fzf.vim",
		config = function()
			vim.keymap.set("n", "<leader>f", "<cmd>GitFiles<cr>")
			vim.keymap.set("n", "<leader>r", "<cmd>RG<cr>")
			vim.keymap.set("v", "<leader>r", '"vy<cmd>exec "RG" getreg("v")<cr>')
			vim.keymap.set("n", "<leader>m", "<cmd>Marks<cr>")
			vim.keymap.set("n", "<leader>b", "<cmd>Buffers<cr>")
			vim.keymap.set("n", "<leader>/", "<cmd>Lines<cr>")
			vim.fn.setenv("FZF_DEFAULT_OPTS", "--color=bg:#000000,gutter:-1,border:#000000 --bind ctrl-a:select-all --history=/tmp/.fzf_history")
			vim.g.fzf_vim = {
				preview_window = { 'hidden,right,70%', 'ctrl-/' }
			}
			vim.cmd [[
				function! s:build_quickfix_list(lines)
				  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
				  copen
				  cc
				endfunction
				let g:fzf_action = {
					\ 'ctrl-q': function('s:build_quickfix_list'), }
			]]
			vim.g.fzf_layout = {
				window = {
					width = 1,
					height = 0.37,
					yoffset = 0,
					border = "bottom",
				}
			}
		end,
	},
	{
		'RRethy/base16-nvim',
		lazy = false
	},
	{
		'maxmx03/solarized.nvim',
		lazy = false,
		enabled = false,
		priority = 1000,
		opts = {
			variant = "winter",
		},
		config = function(_, opts)
			vim.o.termguicolors = true
			vim.o.background = 'dark'
			require('solarized').setup(opts)
			vim.cmd.colorscheme 'solarized'
			vim.api.nvim_set_hl(0, 'SpellBad', { underline = true })
		end,
	},
	{
		"maxbrunsfeld/vim-yankstack",
		config = function()
			vim.keymap.set("n", "<m-y>", "<Plug>yankstack_substitute_older_paste")
			vim.keymap.set("n", "<m-s-y>", "<Plug>yankstack_substitute_newer_paste")
		end
	},
	{
		"jakewvincent/mkdnflow.nvim",
		config = function()
			require("mkdnflow").setup()
		end
	},
})

if vim.g.neovide then
	vim.o.guifont = "0xProto Nerd Font:h16"
	vim.g.neovide_cursor_animate_command_line = false
	vim.g.neovide_position_animation_length = 0
	vim.g.neovide_cursor_smooth_blink = true
	vim.g.neovide_hide_mouse_when_typing = true
	vim.g.neovide_cursor_trail_size = 0
	vim.g.neovide_cursor_animation_length = 0.05
	vim.g.neovide_cursor_antialiasing = true
	vim.g.neovide_padding_top = 0
	vim.g.neovide_padding_bottom = 0
	vim.g.neovide_padding_right = 0
	vim.g.neovide_padding_left = 0
end

-- Custom plugins
require("plugins/makex").setup()
require("plugins/marpoon").setup()
