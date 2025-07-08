vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.o.relativenumber = true
vim.o.breakindent = true
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.signcolumn = 'yes'
vim.o.timeoutlen = 300
vim.o.splitright = true
vim.o.splitbelow = true
vim.o.inccommand = 'split'
vim.o.scrolloff = 7
vim.o.confirm = true
vim.o.laststatus = 3
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.signcolumn = "auto"
vim.o.showmode = false
vim.o.cmdheight = 0
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
    opts = {
			auto_attach = true,
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
    },
		keys = {
			{ "<M-)>", "<cmd>lua require('gitsigns').nav_hunk('next')<cr>"},
			{ "<M-(>", "<cmd>lua require('gitsigns').nav_hunk('prev')<cr>"},
		}
  },
  { -- Fuzzy Finder (files, lsp, etc)
    'nvim-telescope/telescope.nvim',
    event = 'VimEnter',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { -- If encountering errors, see telescope-fzf-native README for installation instructions
        'nvim-telescope/telescope-fzf-native.nvim',

        -- `build` is used to run some command when the plugin is installed/updated.
        -- This is only run then, not every time Neovim starts up.
        build = 'make',

        -- `cond` is a condition used to determine whether this plugin should be
        -- installed and loaded.
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
      { 'nvim-telescope/telescope-ui-select.nvim' },
    },
    config = function()
			require('telescope').setup {
				extensions = {
					['ui-select'] = {
						require('telescope.themes').get_dropdown(),
					},
				},
				defaults = require("telescope.themes").get_ivy({
					layout_config = { height = vim.o.lines },
					borderchars = {
						{ ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
						prompt = {" ", " ", " ", " ", ' ', ' ', " ", " "},
						results = {" ", " ", " ", " ", " ", " ", " ", " "},
						preview = { ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '},
					},
					history = {
						path = '~/.local/share/nvim/databases/telescope_history.sqlite3',
						limit = 100,
					},
				}),
				pickers = {
					find_files = {
						previewer = false,
					},
					current_buffer_fuzzy_find = {
						previewer = false,
					},
				},
			}
      pcall(require('telescope').load_extension, 'fzf')
      pcall(require('telescope').load_extension, 'ui-select')

      local builtin = require 'telescope.builtin'
      vim.keymap.set('n', '<leader>f', builtin.find_files)
      vim.keymap.set('n', '<leader>g', builtin.live_grep)
      vim.keymap.set('n', '<leader>b', builtin.buffers)
      vim.keymap.set('n', '<leader>sh', builtin.help_tags)
      vim.keymap.set('n', '<leader>sk', builtin.keymaps)
      vim.keymap.set('n', '<leader>ss', builtin.builtin)
      vim.keymap.set('n', '<leader>sw', builtin.grep_string)
      vim.keymap.set('n', '<leader>sd', builtin.diagnostics)
      vim.keymap.set('n', '<leader>sr', builtin.resume)
      vim.keymap.set('n', '<leader>s.', builtin.oldfiles)
      vim.keymap.set('n', '<leader>r', builtin.registers)

      -- Slightly advanced example of overriding default behavior and theme
      vim.keymap.set('n', '<leader>/', builtin.current_buffer_fuzzy_find)

      -- It's also possible to pass additional configuration options.
      --  See `:help telescope.builtin.live_grep()` for information about particular keys
      vim.keymap.set('n', '<leader>s/', function()
        builtin.live_grep {
          grep_open_files = true,
          prompt_title = 'Live Grep in Open Files',
        }
      end, { desc = '[S]earch [/] in Open Files' })

      -- Shortcut for searching your Neovim configuration files
      vim.keymap.set('n', '<leader>C', function()
        builtin.find_files { cwd = vim.fn.stdpath 'config' }
      end, { desc = '[S]earch [N]eovim files' })
    end,
  },
  { 'folke/todo-comments.nvim', event = 'VimEnter', dependencies = { 'nvim-lua/plenary.nvim' }, opts = { signs = false } },
  { -- Collection of various small independent plugins/modules
    'echasnovski/mini.nvim',
    config = function()
      -- Better Around/Inside textobjects
      --
      -- Examples:
      --  - va)  - [V]isually select [A]round [)]paren
      --  - yinq - [Y]ank [I]nside [N]ext [Q]uote
      --  - ci'  - [C]hange [I]nside [']quote
      require('mini.ai').setup { n_lines = 500 }

      -- Add/delete/replace surroundings (brackets, quotes, etc.)
      --
      -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
      -- - sd'   - [S]urround [D]elete [']quotes
      -- - sr)'  - [S]urround [R]eplace [)] [']
      require('mini.surround').setup()

			-- Simple and easy statusline.
			local statusline = require 'mini.statusline'
			statusline.setup({
				use_icons = vim.g.have_nerd_font,
				content = {
					active = function()
						local mode, mode_hl = MiniStatusline.section_mode({ trunc_width = 120 })
						local git           = MiniStatusline.section_git({ trunc_width = 40 })
						local diff          = MiniStatusline.section_diff({ trunc_width = 75 })
						local diagnostics   = MiniStatusline.section_diagnostics({ trunc_width = 75 })
						local lsp           = MiniStatusline.section_lsp({ trunc_width = 75 })
						local filename      = MiniStatusline.section_filename({ trunc_width = 140 })
						local fileinfo      = MiniStatusline.section_fileinfo({ trunc_width = 120 })
						local location      = '%2l:%-2v'
						local search        = MiniStatusline.section_searchcount({ trunc_width = 75 })

						return MiniStatusline.combine_groups({
							{ hl = mode_hl,                  strings = { mode } },
							{ hl = 'MiniStatuslineDevinfo',  strings = { git, diff, diagnostics, lsp } },
							'%<', -- Mark general truncate point
							{ hl = 'MiniStatuslineFilename', strings = { filename } },
							'%=', -- End left alignment
							{ hl = 'MiniStatuslineFileinfo', strings = { fileinfo } },
							{ hl = mode_hl,                  strings = { search, location } },
						})
					end,
				},
			})

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
		'stevearc/oil.nvim',
		opts = {},
		lazy = false,
		keys = {
			{ "<C-e>", "<cmd>Oil<cr>"},
		},
	},
	{
		"ThePrimeagen/harpoon",
		lazy = false,
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		config = true,
		keys = {
			{ "<leader>a", "<cmd>lua require('harpoon.mark').add_file()<cr>"},
			{ "<leader>hn", "<cmd>lua require('harpoon.ui').nav_next()<cr>"},
			{ "<leader>hp", "<cmd>lua require('harpoon.ui').nav_prev()<cr>"},
			{ "<F1>", "<cmd>lua require('harpoon.ui').nav_file(1)<cr>"},
			{ "<F2>", "<cmd>lua require('harpoon.ui').nav_file(2)<cr>"},
			{ "<F3>", "<cmd>lua require('harpoon.ui').nav_file(3)<cr>"},
			{ "<F4>", "<cmd>lua require('harpoon.ui').nav_file(4)<cr>"},
			{ "<F5>", "<cmd>lua require('harpoon.ui').nav_file(5)<cr>"},
			{ "<F6>", "<cmd>lua require('harpoon.ui').toggle_quick_menu()<cr>"},
		},
	},
	{
		"rebelot/kanagawa.nvim",
		lazy = false,
		priority = 1000,
		config = function()
			vim.cmd.colorscheme "kanagawa-dragon"
		end
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
	{
		'windwp/nvim-autopairs',
		event = "InsertEnter",
		config = true
	},
	{ "powerman/vim-plugin-ruscmd" },
	{
    "fredrikaverpil/godoc.nvim",
    version = "*",
    dependencies = {
        { "nvim-telescope/telescope.nvim" }, -- optional
        {
            "nvim-treesitter/nvim-treesitter",
            opts = {
              ensure_installed = { "go" },
            },
        },
    },
    build = "go install github.com/lotusirous/gostdsym/stdsym@latest", -- optional
    cmd = { "GoDoc" }, -- optional
    opts = {
			picker = {
				type = "telescope"
			}
		}, -- see further down below for configuration
	},
	{ "tpope/vim-rsi" },
	{ "tpope/vim-fugitive" },
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		config = function()
			require('noice').setup({
				cmdline = {
					enbled = true,
				},
				messages = {
					enabled = false,
				}
			})

			-- Change cmdline highlighting
			local hl = vim.api.nvim_get_hl(0, {name = 'StatusLine'})
			vim.api.nvim_set_hl(0, 'NoiceCmdline', { bg = hl.bg, fg = hl.fg })
			vim.api.nvim_set_hl(0, 'NoiceCmdlineIcon', { bg = hl.bg })
			vim.api.nvim_set_hl(0, 'NoiceCmdlinePrompt', { bg = hl.bg, fg = hl.fg })
			vim.api.nvim_set_hl(0, 'NoiceCmdlinePopUp', { bg = hl.bg, fg = hl.fg })
			vim.api.nvim_set_hl(0, 'NoiceCmdlinePopupBorder', { bg = hl.bg, fg = hl.bg })
		end,
		dependencies = {
			"MunifTanjim/nui.nvim",
		}
	}
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
