local Catppuccin = { "catppuccin/nvim" }
Catppuccin.priority = 1000
Catppuccin.config = function()
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

local Modus = {
  "miikanissi/modus-themes.nvim"
}
Modus.priority = 1000
Modus.opts = {
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
    functions = { bold = true },
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
    highlights.LspInlayHint = { bg = nil, fg = colors.fg_dim }
    highlights.GitSignsAdd = { bg = nil, fg = colors.green }
    highlights.GitSignsAddLn = { bg = nil, fg = colors.green }
    highlights.GitSignsAddNr = { bg = nil, fg = colors.green }
    highlights.GitSignsChange = { bg = nil, fg = colors.yellow }
    highlights.GitSignsChangeLn = { bg = nil, fg = colors.yellow }
    highlights.GitSignsChangeNr = { bg = nil, fg = colors.yellow }
    highlights.GitSignsDelete = { bg = nil, fg = colors.red }
    highlights.GitSignsDeleteLn = { bg = nil, fg = colors.red }
    highlights.GitSignsDeleteNr = { bg = nil, fg = colors.red }
  end,
}

local Gruvbox = {
  'luisiacc/gruvbox-baby'
}
Gruvbox.priority = 1000;
Gruvbox.config = function()
  vim.g.gruvbox_baby_function_style = "bold"
  vim.g.gruvbox_baby_keyword_style = "italic"
  vim.g.gruvbox_baby_telescope_theme = 1
  vim.g.gruvbox_baby_transparent_mode = 1
end

local Kanagawa = {
  "rebelot/kanagawa.nvim"
}
Kanagawa.priority = 1000;
Kanagawa.config = function()
  require('kanagawa').setup({
    compile = false,  -- enable compiling the colorscheme
    undercurl = true, -- enable undercurls
    commentStyle = { italic = true },
    functionStyle = {},
    keywordStyle = { italic = true },
    statementStyle = { bold = true },
    typeStyle = {},
    transparent = true,    -- do not set background color
    dimInactive = false,   -- dim inactive window `:h hl-NormalNC`
    terminalColors = true, -- define vim.g.terminal_color_{0,17}
    colors = {             -- add/modify theme and palette colors
      palette = {},
      theme = { wave = {}, lotus = {}, dragon = {}, all = {} },
    },
    overrides = function(colors) -- add/modify highlights
      return {
        CursorLine = { bg = colors.palette.none },
        CursorLineNr = { bg = colors.palette.none, fg = colors.blue, bold = true },
        TelescopeBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        TelescopePromptBorder = { bg = colors.palette.base, fg = colors.base },
        TelescopePromptNormal = { bg = colors.palette.base },
        TelescopePromptPrefix = { bg = colors.palette.base, fg = colors.red },
        TelescopeNormal = { bg = colors.theme.ui.bg_m3 },
        TelescopePreviewTitle = { bg = colors.palette.base, fg = colors.base },
        TelescopePromptTitle = { bg = colors.palette.base, fg = colors.base },
        TelescopeResultsTitle = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        TelescopeSelection = { bg = colors.palette.base, fg = colors.white },
        TelescopeResultsDiffChange = { fg = colors.palette.yellow },
        TelescopeResultsDiffDelete = { fg = colors.palette.red },
        NoiceCmdline = { bg = colors.theme.ui.bg_m3 },
        NoiceCmdlineIcon = { bg = colors.theme.ui.bg_m3 },
        NoiceCmdlinePopup = { bg = colors.theme.ui.bg_m3 },
        NoiceCmdlinePopupBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        NotifyBackground = { bg = colors.theme.ui.bg_m3 },
        NotifyINFOBody = { bg = colors.theme.ui.bg_m3 },
        NotifyINFOTitle = { bg = colors.theme.ui.bg_m3 },
        NotifyINFOBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        NotifyWARNBody = { bg = colors.theme.ui.bg_m3 },
        NotifyWARNTitle = { bg = colors.theme.ui.bg_m3 },
        NotifyWARNBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        NotifyERRORBody = { bg = colors.theme.ui.bg_m3 },
        NotifyERRORTitle = { bg = colors.theme.ui.bg_m3 },
        NotifyERRORBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        NotifyDEBUGBody = { bg = colors.theme.ui.bg_m3 },
        NotifyDEBUGTitle = { bg = colors.theme.ui.bg_m3 },
        NotifyDEBUGBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        NotifyTRACEBody = { bg = colors.theme.ui.bg_m3 },
        NotifyTRACETitle = { bg = colors.theme.ui.bg_m3 },
        NotifyTRACEBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        WhichKeyBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        WhichKeyFloat = { bg = colors.theme.ui.bg_m3 },
        FloatBorder = { bg = colors.theme.ui.bg_m3, fg = colors.theme.ui.bg_m3 },
        LspInlayHint = { bg = nil, fg = colors.palette.fg_dim },
        GitSignsAdd = { bg = nil, fg = colors.palette.green },
        GitSignsAddLn = { bg = nil, fg = colors.palette.green },
        GitSignsAddNr = { bg = nil, fg = colors.palette.green },
        GitSignsChange = { bg = nil, fg = colors.palette.yellow },
        GitSignsChangeLn = { bg = nil, fg = colors.palette.yellow },
        GitSignsChangeNr = { bg = nil, fg = colors.palette.yellow },
        GitSignsDelete = { bg = nil, fg = colors.palette.red },
        GitSignsDeleteLn = { bg = nil, fg = colors.palette.red },
        GitSignsDeleteNr = { bg = nil, fg = colors.palette.red },
      }
    end,
  })
  vim.cmd [[colorscheme kanagawa-dragon]]
end

return Kanagawa
