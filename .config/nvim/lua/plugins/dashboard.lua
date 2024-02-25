return {
  'goolord/alpha-nvim',
  event = 'VimEnter',
  config = function()
    local opts = { noremap = true, silent = true }

    vim.keymap.set('n', '<leader>a', ':Alpha<CR>', opts)

    local status_ok, alpha = pcall(require, 'alpha')
    if not status_ok then
      return
    end


    local dashboard = require('alpha.themes.dashboard')

    dashboard.section.header.val = require('globals').logos[2]

    dashboard.section.buttons.val = {
      -- dashboard.button('f', '  Find file', ':Telescope find_files <CR>'),
      -- dashboard.button('e', '  New file', ':ene <CR>'),
      -- dashboard.button('r', '  Recently used files', ':Telescope oldfiles <CR>'),
      -- dashboard.button('gr', '  Find text', ':Telescope live_grep <CR>'),
      -- dashboard.button('n', '  Open file tree', ':NvimTreeOpen <CR>'),
      -- dashboard.button('q', '  Quit Neovim', ':qa<CR>'),
    }

    local v = vim.version()
    local version = '  ' .. v.major .. '.' .. v.minor .. '.' .. v.patch
    local datetime = os.date('  %d-%m-%Y')

    local stats = require('lazy').stats()
    local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
    local load_time = '󱐌 Loaded ' .. stats.count .. ' plugins in ' .. ms .. 'ms'

    vim.api.nvim_create_autocmd('User', {
      pattern = 'LazyVimStarted',
      callback = function()
        dashboard.section.footer.val = {
          '',
          '',
          '',
          '',
          '',
          '┌' ..
          string.rep('─', version:len()) ..
          '┬' .. string.rep('─', datetime:len()) .. '┬' .. string.rep('─', load_time:len() - 1) .. '┐',
          '│ ' .. version .. ' | ' .. datetime .. ' | ' .. load_time .. ' │',
          '└' ..
          string.rep('─', version:len()) ..
          '┴' .. string.rep('─', datetime:len()) .. '┴' .. string.rep('─', load_time:len() - 1) .. '┘',
        }
        pcall(vim.cmd.AlphaRedraw)
      end,
    })

    dashboard.section.footer.val = {}

    dashboard.section.footer.opts.hl = 'Type'
    dashboard.section.header.opts.hl = 'Include'
    dashboard.section.buttons.opts.hl = 'Keyword'

    dashboard.opts.opts.noautocmd = true
    alpha.setup(dashboard.opts)
  end
}
