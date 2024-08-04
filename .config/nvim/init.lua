require('base').setup()
require('neovide')
require('mappings').setup()
require('refactoring').setup()

require('plugin_manager').setup()

vim.cmd("colorscheme modus")
