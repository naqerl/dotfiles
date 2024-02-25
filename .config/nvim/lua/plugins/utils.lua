return {
  {
    'windwp/nvim-autopairs',
    lazy = true,
    event = { 'InsertEnter' },
    config = function()
      require('nvim-autopairs').setup()
    end,
  },
  {
    'aserowy/tmux.nvim',
    config = function()
      require('tmux').setup()
    end
  },
  {
    'numToStr/Comment.nvim',
    lazy = true,
    keys = {
      {
        'gcc',
        '<Plug>(comment_toggle_linewise)',
        desc = 'Comment toggle linewise'
      },
      {
        mode = 'v',
        'gc',
        '<Plug>(comment_toggle_linewise_visual)',
        desc = 'Comment toggle linewise (visula)'
      },
    },
    opts = {
      -- add any options here
    },
  },
  {
    'lambdalisue/suda.vim',
    lazy = true,
    cmd = { "SudaWrite", "SudaRead" }
  },
  {
    'tpope/vim-sleuth',
    event = 'BufReadPre'
  },
  {
    'elkowar/yuck.vim',
    ft = 'yuck',
  },
  {
    'nvim-treesitter/playground',
    lazy = true,
    cmd = { "TSPlaygroundToggle" }
  },
}
