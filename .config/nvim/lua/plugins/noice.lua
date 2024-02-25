return {
  -- Highly experimental plugin that completely replaces the UI for messages, cmdline and the popupmenu.
  "folke/noice.nvim",
  event = "VeryLazy",
  opts = {
    lsp = { progress = { enabled = false } },
    presets = {
      bottom_search = true,           -- use a classic bottom cmdline for search
      command_palette = false,        -- position the cmdline and popupmenu together
      long_message_to_split = true,   -- long messages will be sent to a split
      inc_rename = false,             -- enables an input dialog for inc-rename.nvim
      lsp_doc_border = false,         -- add a border to hover docs and signature help
    },
    routes = {
      {
        filter = {
          event = 'msg_show',
          any = {
            { find = '%d+L, %d+B' },
            { find = '; after #%d+' },
            { find = '; before #%d+' },
            { find = '%d fewer lines' },
            { find = '%d more lines' },
          },
        },
        opts = { skip = true },
      }
    },
  },
  dependencies = {
    -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
    "MunifTanjim/nui.nvim",
    -- OPTIONAL:
    --   `nvim-notify` is only needed, if you want to use the notification view.
    --   If not available, we use `mini` as the fallback
    {
      'rcarriga/nvim-notify',
      opts = {
        top_down = false,
        stages = "slide",
        render = "wrapped-compact",
        background_colour = "#000000",
        enabled = false,
      },
    },
  }
}
