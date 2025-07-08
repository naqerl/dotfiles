vim.keymap.set('n', '<leader>d', '<cmd>GoDoc<CR>')
vim.keymap.set(
	"n",
	"<leader>e",
	"oif err != nil {<CR>}<Esc>Oreturn err<Esc>"
)

vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = false

vim.cmd [[set errorformat^=vet:\ %f:%l:%c:\ %m]] -- go vet (important to prepend, otherwise 'vet' will be a part of the file path)
