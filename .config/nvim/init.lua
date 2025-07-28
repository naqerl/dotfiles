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
vim.o.spell = true
vim.o.expandtab = true

-- Update buffer after file change
vim.o.autoread = true
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, {
	command = "if mode() != 'c' | checktime | endif",
	pattern = "*",
})

-- Terminal mode
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Turn of highlight search
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Window navigation
vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- Quick fix list
vim.keymap.set('n', '<M-]>', '<cmd>cnext<cr>')
vim.keymap.set('n', '<M-[>', '<cmd>cprev<cr>')
vim.keymap.set('n', '<leader>oc', '<cmd>copen<cr>')

-- Reselect pasted
vim.keymap.set('n', 'gV', '`[v`]')

-- Paster without yank
vim.keymap.set("v", "<M-p>", '"_dp')

-- Replace word under cursor
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

-- Copy to system clipboard
vim.keymap.set("v", "<leader>y", '"+y')

-- Paste without saving
vim.keymap.set("v", "<leader>p", '"_dP')

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

---@type vim.Option
local rtp = vim.opt.rtp
rtp:prepend(lazypath)
require('lazy').setup({
	{ -- Adds git related signs to the gutter, as well as utilities for managing changes
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
			vim.keymap.set('n', "<F7>", function()
				gitsigns.nav_hunk('next')
			end)
			vim.keymap.set('n', "<F6>", function()
				gitsigns.nav_hunk('prev')
			end)
			vim.keymap.set('n', '<leader>hS', gitsigns.stage_buffer)
			vim.keymap.set('n', '<leader>hR', gitsigns.reset_buffer)
			vim.keymap.set('n', '<leader>hP', gitsigns.preview_hunk)
		end,
	},
	{ 'folke/todo-comments.nvim', event = 'VimEnter', dependencies = { 'nvim-lua/plenary.nvim' }, opts = { signs = false } },
	{ -- Collection of various small independent plugins/modules
		'echasnovski/mini.nvim',
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects"
		},
		config = function()
			require('mini.surround').setup()
			require('mini.pairs').setup()
			require('mini.indentscope').setup()
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
	{ -- Highlight, edit, and navigate code
		'nvim-treesitter/nvim-treesitter',
		build = ':TSUpdate',
		-- [[ Configure Treesitter ]] See `:help nvim-treesitter`
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = { 'bash', 'c', 'diff', 'html', 'lua', 'luadoc', 'markdown', 'markdown_inline', 'query', 'vim', 'vimdoc', 'go' },
				-- Autoinstall languages that are not installed
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
			vim.g.any_jump_window_width_ratio = 0.8
		end,
		keys = {
			{ "<leader>j", "<cmd>AnyJump<cr>" },
		}
	},
	{ "powerman/vim-plugin-ruscmd" },
	{ "tpope/vim-rsi" },
	{
		"tpope/vim-fugitive",
		dependencies = {
			"tpope/vim-rhubarb",
		}
	},
	{
		'nmac427/guess-indent.nvim',
		config = function()
			require('guess-indent').setup({})
		end,
	},
	{
		'mikesmithgh/kitty-scrollback.nvim',
		enabled = true,
		lazy = true,
		cmd = { 'KittyScrollbackGenerateKittens', 'KittyScrollbackCheckHealth', 'KittyScrollbackGenerateCommandLineEditing' },
		event = { 'User KittyScrollbackLaunch' },
		-- version = '*', -- latest stable version, may have breaking changes if major version changed
		-- version = '^6.0.0', -- pin major version, include fixes and features that do not have breaking changes
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
		"smithbm2316/centerpad.nvim"
	},
	{
		"junegunn/fzf.vim",
		config = function()
			vim.keymap.set("n", "<leader>f", "<cmd>Files<cr>")
			vim.keymap.set("n", "<leader>r", "<cmd>Rg<cr>")
			vim.keymap.set("n", "<leader>R", "<cmd>RG<cr>")
			vim.keymap.set("n", "<leader>m", "<cmd>Marks<cr>")
			vim.fn.setenv("FZF_DEFAULT_OPTS", "--color=bg:#000000,gutter:-1,border:#000000")
			vim.g.fzf_vim = {
				preview_window = { 'hidden,right,70%', 'ctrl-/' }
			}
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

-- :make autocompletion
vim.cmd([[
	function! MakeCompletion(A,L,P) abort
	let l:targets = systemlist('make -qp | awk -F'':'' ''/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}'' | grep -v Makefile | sort -u')
	return filter(l:targets, 'v:val =~ "^' . a:A . '"')
	endfunction
	command! -nargs=* -complete=customlist,MakeCompletion Make !make <args>
]])

-- Custom plugins
require("plugins/makex")
require("plugins/marpoon").setup()
