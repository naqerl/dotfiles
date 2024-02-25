return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  config = function()
    require("catppuccin").setup({
      flavour = "frappe",            -- latte, frappe, macchiato, mocha
      transparent_background = true, -- disables setting the background color.
      show_end_of_buffer = false,    -- shows the '~' characters after the end of buffers
      no_italic = false,             -- Force no italic
      no_bold = false,               -- Force no bold
      no_underline = false,          -- Force no underline
      term_colors = false,
      styles = {                     -- Handles the styles of general hi groups (see `:h highlight-args`):
        comments = { "italic" },     -- Change the style of comments
        conditionals = { "italic" },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
      },
      color_overrides = {
        oceanic_next = {
          red = '#EC5F67',
          yellow = '#FAC863',
          blue = '#6699CC',
        }
      },
      custom_highlights = function(colors)
        return {
          CursorLine = { bg = colors.none },
          CursorLineNr = { bg = colors.none, fg = colors.blue, bold = true },

          Normal = { bg = colors.none },

          Pmenu = { bg = colors.base, fg = colors.blue },

          TelescopeBorder = { bg = colors.crust, fg = colors.crust },
          TelescopePromptBorder = { bg = colors.base, fg = colors.base },
          TelescopePromptNormal = { bg = colors.base },
          TelescopePromptPrefix = { bg = colors.base, fg = colors.red },
          TelescopeNormal = { bg = colors.crust },
          TelescopePreviewTitle = { bg = colors.base, fg = colors.base },
          TelescopePromptTitle = { bg = colors.base, fg = colors.base },
          TelescopeResultsTitle = { bg = colors.crust, fg = colors.crust },
          TelescopeSelection = { bg = colors.base, fg = colors.white },
          TelescopeResultsDiffChange = { fg = colors.yellow },
          TelescopeResultsDiffDelete = { fg = colors.red },

          NoiceCmdline = { bg = colors.crust },
          NoiceCmdlinePopup = { bg = colors.crust },
          NoiceCmdlinePopupBorder = { bg = colors.crust, fg = colors.crust },

          NotifyBackground = { bg = colors.crust },
          NotifyINFOBody = { bg = colors.crust },
          NotifyINFOTitle = { bg = colors.crust },
          NotifyINFOBorder = { bg = colors.crust, fg = colors.crust },

          NotifyWARNBody = { bg = colors.crust },
          NotifyWARNTitle = { bg = colors.crust },
          NotifyWARNBorder = { bg = colors.crust, fg = colors.crust },

          NotifyERRORBody = { bg = colors.crust },
          NotifyERRORTitle = { bg = colors.crust },
          NotifyERRORBorder = { bg = colors.crust, fg = colors.crust },

          NotifyDEBUGBody = { bg = colors.crust },
          NotifyDEBUGTitle = { bg = colors.crust },
          NotifyDEBUGBorder = { bg = colors.crust, fg = colors.crust },

          NotifyTRACEBody = { bg = colors.crust },
          NotifyTRACETitle = { bg = colors.crust },
          NotifyTRACEBorder = { bg = colors.crust, fg = colors.crust },

          WhichKeyBorder = { bg = colors.crust, fg = colors.crust },
          WhichKeyFloat = { bg = colors.crust },

          FloatBorder = { bg = colors.crust, fg = colors.crust },
          NormalFloat = { bg = colors.crust },
        }
      end,
      integrations = {
        cmp = true,
        gitsigns = true,
        treesitter = true,
        notify = false,
        harpoon = true,
        telescope = {
          enabled = false,
          style = "nvchad",
        },
        fidget = true,
        noice = false,
      },
    })

    -- setup must be called before loading
    vim.cmd.colorscheme "catppuccin"
    vim.opt.cursorline = true
  end
}
