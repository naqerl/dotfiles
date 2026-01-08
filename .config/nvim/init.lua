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
vim.keymap.set("v", "<M-p>", '"_dp', { desc = "Paste without yank" })
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Replace word under cursor" })
vim.keymap.set("v", "<leader>y", '"+y', { desc = "Copy to system clipboard" })
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
		"folke/snacks.nvim",
		config = function()

			snacks = require("snacks")
			snacks.setup({
				picker = {
					icons = {
						files = {
							enabled = false,
						},
					},
					matcher = {
						frecency = true,
					},
					layouts = {
						ivy = {
							layout = {
								box = "vertical",
								backdrop = false,
								row = 1,
								width = 0,
								height = 0.99,
								border = "top",
								title = " {title} {live} {flags}",
								title_pos = "right",
								{ win = "input", height = 1, border = "none" },
								{
									box = "horizontal",
									{ win = "list", border = "none" },
								},
							},
						},
					}
				}
			})
			vim.keymap.set('n', '<leader>f', function()
				snacks.picker.files({
					layout = "ivy",
					hidden = true,
				})
			end)
			vim.keymap.set('n', '<leader>b', function()
				snacks.picker.buffers({
					layout = "ivy"
				})
			end)
			vim.keymap.set('n', '<leader>g', function()
				snacks.picker.grep({
					layout = "ivy"
				})
			end)
			vim.keymap.set('n', '<leader>r', function()
				snacks.picker.resume({
					layout = "ivy",
					hidden = true,
				})
			end)
			vim.keymap.set('v', '<leader>g', function()
				snacks.picker.grep_word({
					layout = "ivy",
					hidden = true,
				})
			end)
			vim.keymap.set('n', '<leader>l', function()
				snacks.picker.lines({
					layout = "ivy",
				})
			end)
		end
	},
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
		"ThePrimeagen/harpoon",
		branch = "harpoon2",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			local harpoon = require("harpoon")
			harpoon:setup()

			vim.keymap.set("n", "<leader>a", function() harpoon:list():add() end, { desc = "Harpoon add file" })
			vim.keymap.set("n", "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end, { desc = "Harpoon menu" })

			vim.keymap.set("n", "<F1>", function() harpoon:list():select(1) end, { desc = "Harpoon file 1" })
			vim.keymap.set("n", "<F2>", function() harpoon:list():select(2) end, { desc = "Harpoon file 2" })
			vim.keymap.set("n", "<F3>", function() harpoon:list():select(3) end, { desc = "Harpoon file 3" })
			vim.keymap.set("n", "<F4>", function() harpoon:list():select(4) end, { desc = "Harpoon file 4" })
			vim.keymap.set("n", "<F5>", function() harpoon:list():select(5) end, { desc = "Harpoon file 5" })

			-- Toggle previous & next buffers stored within Harpoon list
			vim.keymap.set("n", "<C-S-P>", function() harpoon:list():prev() end, { desc = "Harpoon prev" })
			vim.keymap.set("n", "<C-S-N>", function() harpoon:list():next() end, { desc = "Harpoon next" })
		end,
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
		'maxmx03/solarized.nvim',
		lazy = false,
		priority = 1000,
		---@type solarized.config
		opts = {},
		config = function(_, opts)
			vim.o.termguicolors = true
			vim.o.background = 'dark'
			require('solarized').setup(opts)
			vim.cmd.colorscheme 'solarized'
		end,
	},
	{
		'roman/golden-ratio',
	},
	{
		'jbyuki/venn.nvim',
		config = function()
			-- venn.nvim: enable or disable keymappings
			function _G.Toggle_venn()
				local venn_enabled = vim.inspect(vim.b.venn_enabled)
				if venn_enabled == "nil" then
					vim.b.venn_enabled = true
					vim.cmd[[setlocal ve=all]]
					-- draw a line on HJKL keystokes
					vim.api.nvim_buf_set_keymap(0, "n", "J", "<C-v>j:VBox<CR>", {noremap = true})
					vim.api.nvim_buf_set_keymap(0, "n", "K", "<C-v>k:VBox<CR>", {noremap = true})
					vim.api.nvim_buf_set_keymap(0, "n", "L", "<C-v>l:VBox<CR>", {noremap = true})
					vim.api.nvim_buf_set_keymap(0, "n", "H", "<C-v>h:VBox<CR>", {noremap = true})
					-- draw a box by pressing "f" with visual selection
					vim.api.nvim_buf_set_keymap(0, "v", "f", ":VBox<CR>", {noremap = true})
				else
					vim.cmd[[setlocal ve=]]
					vim.api.nvim_buf_del_keymap(0, "n", "J")
					vim.api.nvim_buf_del_keymap(0, "n", "K")
					vim.api.nvim_buf_del_keymap(0, "n", "L")
					vim.api.nvim_buf_del_keymap(0, "n", "H")
					vim.api.nvim_buf_del_keymap(0, "v", "f")
					vim.b.venn_enabled = nil
				end
			end
			-- toggle keymappings for venn using <leader>v
			vim.api.nvim_set_keymap('n', '<leader>v', ":lua Toggle_venn()<CR>", { noremap = true})
		end
	},
})

