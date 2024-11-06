local M = {}

M.setup = function()
  vim.g.mapleader = ' '
  vim.g.maplocalleader = ' '

  -- Set highlight on search
  vim.o.hlsearch = false

  -- Save undo history
  vim.o.undofile = true

  -- Make line numbers default
  vim.wo.number = true

  -- Enable mouse mode
  vim.o.mouse = 'a'

  -- Enable break indent
  vim.o.breakindent = true

  -- Case insensitive searching UNLESS \C or capital in search
  vim.o.ignorecase = true
  vim.o.smartcase = true

  -- Keep signcolumn on by default
  vim.wo.signcolumn = 'yes'

  -- Decrease update time
  vim.o.updatetime = 250
  vim.o.timeout = true
  vim.o.timeoutlen = 300

  -- Set completeopt to have a better completion experience
  -- vim.o.completeopt = 'menuone,noselect'

  vim.o.termguicolors = true

  -- vim.opt.winbar = '%=%m %f'
  vim.opt.wrap = false -- No wrap lines
  vim.wo.relativenumber = true

  vim.opt.scrolloff = 7

  -- Keymaps for better default experience
  vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

  -- Remap for dealing with word wrap
  vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
  vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

  -- Buffers navigation
  vim.keymap.set('n', '<leader>bk', ':bp|bd #<CR>', { silent = true })
  vim.keymap.set('n', '<leader>bak', ':%bd<CR>', { silent = true })
  vim.keymap.set("n", "gn", ":bn<CR>", { silent = true })
  vim.keymap.set("n", "gp", ":bp<CR>", { silent = true })

  -- Paste without yank
  vim.keymap.set('x', '<leader>R', '"_dP', { silent = true })

  -- Move selectd lines
  vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv", { silent = true })
  vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv", { silent = true })

  -- Keep cursor in a middle while scrolling
  vim.keymap.set('n', '<C-u>', '<C-u>zz', { silent = true })
  vim.keymap.set('n', '<C-d>', '<C-d>zz', { silent = true })

  -- Keep cursor in a middle while seaching
  vim.keymap.set('n', 'n', 'nzzzv', { silent = true })
  vim.keymap.set('n', 'N', 'Nzzzv', { silent = true })

  vim.keymap.set("v", "<leader>S", '"zy:%s/<C-r>z/<C-r>z/gI<Left><Left><Left>',
    { desc = "Replace visual selection" })

  -- Better horisontal navigation
  vim.keymap.set('n', 'gh', '0', { silent = true })
  vim.keymap.set('n', 'gl', '$', { silent = true })

  -- Higlight yanked text
  local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
  vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
      vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*',
  })

  -- Yank to system clipboard
  -- vim.keymap.set({ 'n', 'v' }, '<leader>y', '"+y', { silent = true })
  -- vim.keymap.set({ 'n', 'v' }, '<leader>Y', '"+Y', { silent = true })
  -- vim.keymap.set({ 'n', 'v' }, '<leader>p', '"+p', { silent = true })
  -- vim.keymap.set({ 'n', 'v' }, '<leader>P', '"+P', { silent = true })

  -- region:    Toggle wrap
  vim.opt.wrap = false
  vim.opt.linebreak = false

  function Toggle_wrap()
    vim.cmd [[ set wrap! ]]
    vim.cmd [[ set linebreak! ]]
  end

  vim.keymap.set('n', '<leader>w', Toggle_wrap, { desc = 'Toggle [W]rap', silent = true })
  -- endregion: Toggle wrap

  vim.opt.splitright = true;
  vim.opt.guicursor = "";
  vim.opt.conceallevel = 2;
  -- vim.opt.pumblend = 15
  -- vim.opt.winblend = 5

  vim.opt.fillchars = { eob = " " }

  vim.api.nvim_set_option("clipboard", "unnamedplus")

  vim.cmd [[
    autocmd FileType NeogitCommitMessage setlocal spell
  ]]
end

return M
