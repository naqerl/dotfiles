local M = {
  'nvim-lualine/lualine.nvim'
}

M.event = { 'VimEnter' }

M.dependencies = {
  'arkav/lualine-lsp-progress',
  'ThePrimeagen/harpoon',
}

M.config = function()
  require('lualine').setup({
    disabled_filetypes = { 'alpha' },
    sections = {
      lualine_a = {
        { 'mode', fmt = function(str) return ' ' end }
      },
      lualine_b = {
        'branch',
        {
          'diff',
          colored = true,
          symbols = {
            added = ' ',
            modified = '󰏬 ',
            removed = ' ',
          },
        },
      },
      lualine_c = {
        {
          'diagnostics',
          sources = { 'nvim_lsp' },
          sections = {
            'info',
            'error',
            'warn',
            'hint',
          },
          diagnostic_color = {
            error = { fg = 'DiagnosticError' },
            warn = { fg = 'DiagnosticWarn' },
            info = { fg = 'DiaganosticInfo' },
            hint = { fg = 'DiagnosticHint' },
          },
          colored = true,
          update_in_insert = false,
          always_visible = false,
          symbols = {
            error = ' ',
            warn = ' ',
            hint = ' ',
            info = ' ',
          },
          separator = { left = '', right = '' },
        },
        {
          function()
            return require('nvim-treesitter').statusline()
          end
        },
        {
          'lsp_progress',
          display_components = { 'lsp_client_name', 'spinner' },
          timer = { progress_enddelay = 50, spinner = 100, lsp_client_name_enddelay = 1000 },
          separators = {
            lsp_client_name = { pre = '', post = '' },
          },
          spinner_symbols = { '', '', '', '', '', '', '', '', '', '', '', '', '', '' },
        },
      },
      lualine_x = {
        function()
          return require('harpoon.mark').status()
        end,
      },
      lualine_y = {
        'encoding',
        'filesize',
      },
      lualine_z = {
        'location',
        'progress',
        {
          function()
            for _, buf in ipairs(vim.api.nvim_list_bufs()) do
              if vim.api.nvim_buf_get_option(buf, 'modified') then
                return 'Unsaved buffers' -- any message or icon
              end
            end
            return ''
          end,
        },
      },
    },
    options = {
      icons_enabled = true,
      component_separators = '',
      section_separators = '',
      globalstatus = true,
      theme = "auto",
    },
  })
end

return M
