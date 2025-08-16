vim.keymap.set('n', '<leader>d', '<cmd>GoDoc<CR>')
vim.keymap.set(
	"n",
	"<leader>e",
	"oif err != nil {<CR>}<Esc>Oreturn err<Esc>"
)

vim.o.tabstop = 8
vim.o.shiftwidth = 8
vim.o.expandtab = false
vim.o.spell = true

vim.cmd [[set errorformat^=vet:\ %f:%l:%c:\ %m]] -- go vet (important to prepend, otherwise 'vet' will be a part of the file path)
