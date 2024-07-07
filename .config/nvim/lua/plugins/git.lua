return {
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",  -- required
      "sindrets/diffview.nvim", -- optional - Diff integration

      -- Only one of these is needed, not both.
      "nvim-telescope/telescope.nvim", -- optional
      "ibhagwan/fzf-lua",              -- optional
    },
    keys = {
      { '<leader>og', function() require('neogit').open() end, desc = '[O]pen [G]it' },
    },
    config = true
  },
  {
    'tpope/vim-rhubarb',
    event = 'BufReadPre',
  },
  {
    -- Adds git releated signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    event = 'BufReadPre',
    keys = {
      { ']g',         function() require('gitsigns').next_hunk() end,    desc = 'Next git hunk' },
      { '[g',         function() require('gitsigns').prev_hunk() end,    desc = 'Previous git hunk' },
      { '<leader>hp', function() require('gitsigns').preview_hunk() end, desc = '[H]unk [P]review' },
    },
    config = function()
      require('gitsigns').setup()
    end
  },
  {
    'idanarye/vim-merginal',
    cmd = { 'Merginal' }
  },
  {
    'ThePrimeagen/git-worktree.nvim',
    lazy = true,
    config = function()
      require("git-worktree").setup({
        change_directory_command = "cd",  -- default: "cd",
        update_on_change = true,          -- default: true,
        update_on_change_command = "e .", -- default: "e .",
        clearjumps_on_change = true,      -- default: true,
        autopush = false,                 -- default: false,
      })
    end
  },
}
