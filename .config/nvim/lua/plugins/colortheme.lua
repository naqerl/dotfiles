-- return {
--   "catppuccin/nvim",
--   name = "catppuccin",
--   priority = 1000,
--   config = function()
--     require("catppuccin").setup({
--       flavour = "frappe",            -- latte, frappe, macchiato, mocha
--       transparent_background = true, -- disables setting the background color.
--       show_end_of_buffer = false,    -- shows the '~' characters after the end of buffers
--       no_italic = false,             -- Force no italic
--       no_bold = false,               -- Force no bold
--       no_underline = false,          -- Force no underline
--       term_colors = false,
--       styles = {                     -- Handles the styles of general hi groups (see `:h highlight-args`):
--         comments = { "italic" },     -- Change the style of comments
--         conditionals = { "italic" },
--         loops = {},
--         functions = {},
--         keywords = {},
--         strings = {},
--         variables = {},
--         numbers = {},
--         booleans = {},
--         properties = {},
--         types = {},
--         operators = {},
--       },
--       color_overrides = {
--         oceanic_next = {
--           red = '#EC5F67',
--           yellow = '#FAC863',
--           blue = '#6699CC',
--         }
--       },
--       custom_highlights = function(colors)
--         return {
--           CursorLine = { bg = colors.none },
--           CursorLineNr = { bg = colors.none, fg = colors.blue, bold = true },
--
--           Normal = { bg = colors.none },
--
--           Pmenu = { bg = colors.base, fg = colors.blue },
--
--           TelescopeBorder = { bg = colors.crust, fg = colors.crust },
--           TelescopePromptBorder = { bg = colors.base, fg = colors.base },
--           TelescopePromptNormal = { bg = colors.base },
--           TelescopePromptPrefix = { bg = colors.base, fg = colors.red },
--           TelescopeNormal = { bg = colors.crust },
--           TelescopePreviewTitle = { bg = colors.base, fg = colors.base },
--           TelescopePromptTitle = { bg = colors.base, fg = colors.base },
--           TelescopeResultsTitle = { bg = colors.crust, fg = colors.crust },
--           TelescopeSelection = { bg = colors.base, fg = colors.white },
--           TelescopeResultsDiffChange = { fg = colors.yellow },
--           TelescopeResultsDiffDelete = { fg = colors.red },
--
--           NoiceCmdline = { bg = colors.crust },
--           NoiceCmdlinePopup = { bg = colors.crust },
--           NoiceCmdlinePopupBorder = { bg = colors.crust, fg = colors.crust },
--
--           NotifyBackground = { bg = colors.crust },
--           NotifyINFOBody = { bg = colors.crust },
--           NotifyINFOTitle = { bg = colors.crust },
--           NotifyINFOBorder = { bg = colors.crust, fg = colors.crust },
--
--           NotifyWARNBody = { bg = colors.crust },
--           NotifyWARNTitle = { bg = colors.crust },
--           NotifyWARNBorder = { bg = colors.crust, fg = colors.crust },
--
--           NotifyERRORBody = { bg = colors.crust },
--           NotifyERRORTitle = { bg = colors.crust },
--           NotifyERRORBorder = { bg = colors.crust, fg = colors.crust },
--
--           NotifyDEBUGBody = { bg = colors.crust },
--           NotifyDEBUGTitle = { bg = colors.crust },
--           NotifyDEBUGBorder = { bg = colors.crust, fg = colors.crust },
--
--           NotifyTRACEBody = { bg = colors.crust },
--           NotifyTRACETitle = { bg = colors.crust },
--           NotifyTRACEBorder = { bg = colors.crust, fg = colors.crust },
--
--           WhichKeyBorder = { bg = colors.crust, fg = colors.crust },
--           WhichKeyFloat = { bg = colors.crust },
--
--           FloatBorder = { bg = colors.crust, fg = colors.crust },
--           NormalFloat = { bg = colors.crust },
--         }
--       end,
--       integrations = {
--         cmp = true,
--         gitsigns = true,
--         treesitter = true,
--         notify = false,
--         harpoon = true,
--         telescope = {
--           enabled = false,
--           style = "nvchad",
--         },
--         fidget = true,
--         noice = false,
--       },
--     })
--
--     -- setup must be called before loading
--     vim.cmd.colorscheme "catppuccin"
--     vim.opt.cursorline = true
--   end
-- }
local M = {
  "miikanissi/modus-themes.nvim"
}

M.priority = 1000

M.opts = {
  -- Theme comes in two styles `modus_operandi` and `modus_vivendi`
  -- `auto` will automatically set style based on background set with vim.o.background
  style = "auto",
  variant = "default",  -- Theme comes in four variants `default`, `tinted`, `deuteranopia`, and `tritanopia`
  transparent = true,   -- Transparent background (as supported by the terminal)
  dim_inactive = false, -- "non-current" windows are dimmed
  styles = {
    -- Style to be applied to different syntax groups
    -- Value is any valid attr-list value for `:help nvim_set_hl`
    comments = { italic = true },
    keywords = { italic = true },
    functions = {},
    variables = {},
  },

  --- You can override specific color groups to use other groups or a hex color
  --- Function will be called with a ColorScheme table
  --- Refer to `extras/lua/modus_operandi.lua` or `extras/lua/modus_vivendi.lua` for the ColorScheme table
  ---@param colors ColorScheme
  on_colors = function(colors) end,

  --- You can override specific highlights to use other groups or a hex color
  --- Function will be called with a Highlights and ColorScheme table
  --- Refer to `extras/lua/modus_operandi.lua` or `extras/lua/modus_vivendi.lua` for the Highlights and ColorScheme table
  ---@param highlights Highlights
  ---@param colors ColorScheme
  on_highlights = function(highlights, colors)
    highlights.CursorLine = { bg = colors.none }
    highlights.CursorLineNr = { bg = colors.none, fg = colors.blue, bold = true }

    highlights.Normal = { bg = colors.none }

    highlights.Pmenu = { bg = colors.bg_dim, fg = colors.blue }

    highlights.TelescopeBorder = { bg = colors.bg_dim, fg = colors.bg_dim }
    highlights.TelescopePromptBorder = { bg = colors.base, fg = colors.base }
    highlights.TelescopePromptNormal = { bg = colors.base }
    highlights.TelescopePromptPrefix = { bg = colors.base, fg = colors.red }
    highlights.TelescopeNormal = { bg = colors.bg_dim }
    highlights.TelescopePreviewTitle = { bg = colors.base, fg = colors.base }
    highlights.TelescopePromptTitle = { bg = colors.base, fg = colors.base }
    highlights.TelescopeResultsTitle = { bg = colors.bg_dim, fg = colors.bg_dim }
    highlights.TelescopeSelection = { bg = colors.base, fg = colors.white }
    highlights.TelescopeResultsDiffChange = { fg = colors.yellow }
    highlights.TelescopeResultsDiffDelete = { fg = colors.red }

    highlights.NoiceCmdline = { bg = colors.bg_dim }
    highlights.NoiceCmdlinePopup = { bg = colors.bg_dim }
    highlights.NoiceCmdlinePopupBorder = { bg = colors.bg_dim, fg = colors.bg_dim }

    highlights.NotifyBackground = { bg = colors.bg_dim }
    highlights.NotifyINFOBody = { bg = colors.bg_dim }
    highlights.NotifyINFOTitle = { bg = colors.bg_dim }
    highlights.NotifyINFOBorder = { bg = colors.bg_dim, fg = colors.bg_dim }

    highlights.NotifyWARNBody = { bg = colors.bg_dim }
    highlights.NotifyWARNTitle = { bg = colors.bg_dim }
    highlights.NotifyWARNBorder = { bg = colors.bg_dim, fg = colors.bg_dim }

    highlights.NotifyERRORBody = { bg = colors.bg_dim }
    highlights.NotifyERRORTitle = { bg = colors.bg_dim }
    highlights.NotifyERRORBorder = { bg = colors.bg_dim, fg = colors.bg_dim }

    highlights.NotifyDEBUGBody = { bg = colors.bg_dim }
    highlights.NotifyDEBUGTitle = { bg = colors.bg_dim }
    highlights.NotifyDEBUGBorder = { bg = colors.bg_dim, fg = colors.bg_dim }

    highlights.NotifyTRACEBody = { bg = colors.bg_dim }
    highlights.NotifyTRACETitle = { bg = colors.bg_dim }
    highlights.NotifyTRACEBorder = { bg = colors.bg_dim, fg = colors.bg_dim }

    highlights.WhichKeyBorder = { bg = colors.bg_dim, fg = colors.bg_dim }
    highlights.WhichKeyFloat = { bg = colors.bg_dim }

    highlights.FloatBorder = { bg = colors.bg_dim, fg = colors.bg_dim }
    highlights.NormalFloat = { bg = colors.bg_dim }
  end,
}

return M
