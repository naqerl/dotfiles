-- NOTE: This is where your plugins related to LSP can be installed.
--  The configuration is done below. Search for lspconfig to find it below.

local configured_file_types =
{ 'rust', 'python', 'lua', 'typescript', 'javascript', 'bash' }

return {
	{
		'simrat39/symbols-outline.nvim',
		keys = {
			{ '<leader>ol', ':SymbolsOutline<CR>' },
		},
		config = function()
			require('symbols-outline').setup({
				show_numbers = true,
				show_relative_numbers = true,
				autofold_depth = 1,
			})
		end,
	},
	{
		'neovim/nvim-lspconfig',
		ft = configured_file_types,
		dependencies = {
			-- Automatically install LSPs to stdpath for neovim
			{
				'williamboman/mason.nvim',
				ft = configured_file_types,
				config = true
			},
			{
				'williamboman/mason-lspconfig.nvim',
				ft = configured_file_types,
				config = function()
					-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
					-- local capabilities = vim.lsp.protocol.make_client_capabilities()
					-- Set up lspconfig.
					-- local capabilities = require('cmp_nvim_lsp').default_capabilities()

					-- Ensure the servers above are installed
					local mason_lspconfig = require('mason-lspconfig')

					local servers = {
						clangd = {},
						pyright = {},
						bashls = {},
						tsserver = {},
						lua_ls = {
							Lua = {
								diagnostics = {
									globals = { 'vim' }
								},
								-- runtime = {
								-- 	-- Tell the language server which version of Lua you're using
								-- 	-- (most likely LuaJIT in the case of Neovim)
								-- 	version = 'LuaJIT'
								-- },
								-- Make the server aware of Neovim runtime files
								workspace = {
									-- checkThirdParty = false,
									library = {
										vim.env.VIMRUNTIME
										-- "${3rd}/luv/library"
										-- "${3rd}/busted/library",
									}
									-- or pull in all of 'runtimepath'. NOTE: this is a lot slower
									-- library = vim.api.nvim_get_runtime_file("", true)
								}
							}
						},
						rust_analyzer = {},
					}
					mason_lspconfig.setup({
						ensure_installed = vim.tbl_keys(servers),
					})

					-- Diagnostic keymaps
					local on_attach = function(client, bufnr)
						-- if client.server_capabilities.inlayHintProvider then
						-- vim.lsp.inlay_hint.enable(bufnr, true)
						-- end

						if vim.bo.filetype == 'python' then
							local pyproject_path = vim.fn.findfile('pyproject.toml',
								vim.fn.getcwd() .. '/**')
							if pyproject_path ~= '' then
								require('venv-selector').retrieve_from_cache()
							end
						end

						local nmap = function(keys, func, desc)
							if desc then
								desc = 'LSP: ' .. desc
							end

							vim.keymap.set('n', keys, func,
								{ buffer = bufnr, desc = desc, silent = true })
						end

						nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
						nmap('<leader>ca', ':CodeActionMenu<CR>', '[C]ode [A]ction')

						nmap('[d', vim.diagnostic.goto_prev, 'Go to previous diagnostic message')
						nmap(']d', vim.diagnostic.goto_next, 'Go to next diagnostic message')

						nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
						nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
						nmap('gr', require('telescope.builtin').lsp_references,
							'[G]oto [R]eferences')
						nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
						nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
						nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols,
							'[D]ocument [S]ymbols')
						nmap('<leader>ws',
							require('telescope.builtin').lsp_dynamic_workspace_symbols,
							'[W]orkspace [S]ymbols')

						nmap('K', vim.lsp.buf.hover, 'Hover Documentation')

						-- Change diagnostic icons
						vim.fn.sign_define('DiagnosticSignError',
							{
								text = '',
								texthl = 'DiagnosticSignError',
								linehl = '',
								numhl =
								''
							})
						vim.fn.sign_define('DiagnosticSignWarn',
							{
								text = '',
								texthl = 'DiagnosticSignWarn',
								linehl = '',
								numhl =
								''
							})
						vim.fn.sign_define('DiagnosticSignInfo',
							{
								text = '',
								texthl = 'DiagnosticSignInfo',
								linehl = '',
								numhl =
								''
							})
						vim.fn.sign_define('DiagnosticSignHint',
							{
								text = '',
								texthl = 'DiagnosticSignHint',
								linehl = '',
								numhl =
								''
							})
					end

					mason_lspconfig.setup_handlers {
						function(server_name)
							require('lspconfig')[server_name].setup {
								-- capabilities = capabilities,
								on_attach = on_attach,
								settings = servers[server_name],
							}
						end,
					}
				end,
			},
			-- Additional lua configuration, makes nvim stuff amazing!
			{
				'folke/neodev.nvim',
				enable = false,
				lazy = true
			},
			{
				'linux-cultist/venv-selector.nvim',
				lazy = true,
				ft = { 'python' },
				dependencies = { 'nvim-telescope/telescope.nvim', 'mfussenegger/nvim-dap-python' },
				opts = {
					name = '.venv',
				},
				keys = {
					{ '<leader>vs', '<cmd>VenvSelect<cr>' },
					{ '<leader>vc', '<cmd>VenvSelectCached<cr>' },
				},
			},
			{
				'weilbith/nvim-code-action-menu',
				enable = false,
				cmd = 'CodeActionMenu',
				config = function()
					require('neodev').setup()
					vim.g.code_action_menu_show_details = false
					vim.g.code_action_menu_show_diff = false
				end
			},
			{
				"stevearc/conform.nvim",
				event = { "BufReadPre", "BufNewFile" },
				config = function()
					require('conform').setup({
						formatters_by_ft = {
							python = { "isort", "black" },

						},
						format_on_save = {
							lsp_fallback = true,
							async = false,
							timeout_ms = 1000,
						},
					})
				end
			},
			{
				"ThePrimeagen/refactoring.nvim",
				ft = configured_file_types,
				dependencies = {
					"nvim-lua/plenary.nvim",
					"nvim-treesitter/nvim-treesitter",
				},
				config = function()
					-- pcall(require("telescope").load_extension, "refactoring")
					-- vim.keymap.set(
					-- 	{ "n", "x" },
					-- 	"<leader>rr",
					-- 	function() require('telescope').extensions.refactoring.refactors() end
					-- )
					-- vim.keymap.set("x", "<leader>re",
					-- 	function() require('refactoring').refactor('Extract Function') end)
					-- vim.keymap.set("x", "<leader>rf",
					-- 	function() require('refactoring').refactor('Extract Function To File') end)
					-- vim.keymap.set("x", "<leader>rv",
					-- 	function() require('refactoring').refactor('Extract Variable') end)
					-- vim.keymap.set("n", "<leader>rI",
					-- 	function() require('refactoring').refactor('Inline Function') end)
					-- vim.keymap.set({ "n", "x" }, "<leader>ri",
					-- 	function() require('refactoring').refactor('Inline Variable') end)
					-- Inline var supports both normal and visual mode
					-- You can also use below = true here to to change the position of the printf
					-- statement (or set two remaps for either one). This remap must be made in normal mode.
					-- vim.keymap.set(
					-- 	"n",
					-- 	"<leader>rp",
					-- 	function() require('refactoring').debug.printf({ below = false }) end
					-- )

					-- Print var

					-- vim.keymap.set({ "x", "n" }, "<leader>rv",
					-- 	function() require('refactoring').debug.print_var() end)
					-- Supports both visual and normal mode

					-- vim.keymap.set("n", "<leader>rc",
					-- 	function() require('refactoring').debug.cleanup({}) end)
					-- Supports only normal mode
					-- require("refactoring").setup()
				end,
			}
		},
	}
}
