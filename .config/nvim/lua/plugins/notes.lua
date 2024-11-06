-- local neorg = {
--   "nvim-neorg/neorg",
--   build = ":Neorg sync-parsers",
--   dependencies = { "nvim-lua/plenary.nvim", "laher/neorg-exec", "folke/zen-mode.nvim", },
--   lazy = true,
--   cmd = { "Neorg" },
--   ft = { "norg" },
--   config = function()
--     require("neorg").setup {
--       load = {
--         ["core.esupports.indent"] = {},
--         ["core.defaults"] = {},
--         ["core.presenter"] = {
--           config = {
--             zen_mode = "zen-mode",
--           },
--         },
--         ["core.concealer"] = {
--           config = {
--             folds = false,
--           },
--         },
--         ["core.summary"] = {},
--         ["core.completion"] = {
--           config = {
--             engine = "nvim-cmp",
--           },
--         },
--         ["core.dirman"] = {
--           config = {
--             workspaces = {
--               notes = "~/notes",
--             },
--           },
--         },
--         ["external.exec"] = {}
--       },
--     }
--   end,
-- }
--
local obsidian = {
  "epwalsh/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  cmd = "ObsidianSearch",
  -- ft = "markdown",
  -- event = {
  --   "BufReadPre " .. vim.fn.expand("~" .. "/notes/**.md"),
  --   "BufNewFile " .. vim.fn.expand("~" .. "/notes/**.md"),
  -- },
  dependencies = {
    -- Required.
    "nvim-lua/plenary.nvim",
  },
  config = function()
    require('obsidian').setup({
      log_level = 0,
      mappings = {},
      daily_notes = {},
      workspaces = {
        {
          name = "personal",
          path = "~/notes/personal",
        },
        {
          name = "haas",
          path = "~/notes/projects/haas",
        },
        {
          name = "wiki",
          path = "~/notes/wiki"
        },
        {
          name = "capture_serbia",
          path = "~/notes/projects/capture_serbia"
        }
      },
      templates = {
        folder = "~/notes/templates/",
        date_format = "%d-%m-%Y",
        time_format = "%HH:%M",
      },
      completion = {
        -- Set to false to disable completion.
        nvim_cmp = true,

        -- Trigger completion at 2 chars.
        min_chars = 2,

        -- 2. Whether to add the note path during completion.
        -- E.g. "[[Foo" completes to "[[notes/foo|Foo]]" assuming "notes/foo.md" is the path of the note.
        -- Mutually exclusive with 'prepend_note_id' and 'use_path_only'.
        prepend_note_path = false,
        -- 3. Whether to only use paths during completion.
        -- E.g. "[[Foo" completes to "[[notes/foo]]" assuming "notes/foo.md" is the path of the note.
        -- Mutually exclusive with 'prepend_note_id' and 'prepend_note_path'.
        use_path_only = false,
      }
    })
  end,
}

obsidian.mappings = {
  -- Overrides the 'gf' mapping to work on markdown/wiki links within your vault.
  ["gf"] = {
    action = function()
      return require("obsidian").util.gf_passthrough()
    end,
    opts = { noremap = true, expr = true, buffer = true },
  },
  -- Toggle check-boxes.
  ["<leader>tc"] = {
    action = function()
      return require("obsidian").util.toggle_checkbox()
    end,
    -- opts = { buffer = true },
  },
}

return obsidian
