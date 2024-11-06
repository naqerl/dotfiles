local configured_file_types =
{ 'rust', 'python', 'lua', 'typescript', 'javascript', 'javascriptreact', 'bash', 'html' }


local on_attach = function(client, bufnr)
	require('nvim-navic').attach(client, bufnr)
	if vim.lsp.inlay_hint then
		vim.lsp.inlay_hint.enable(true, { 0 })
	end

	local nmap = function(keys, func, desc)
		if desc then
			desc = 'LSP: ' .. desc
		end

		vim.keymap.set('n', keys, func,
			{ buffer = bufnr, desc = desc, silent = true })
	end

	nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')

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

local M = {
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
		'williamboman/mason.nvim',
		ft = configured_file_types,
		config = function()
			require('mason').setup()
		end
	},
	{ -- Autocompletion
		'hrsh7th/nvim-cmp',
		event = 'InsertEnter',
		dependencies = {
			-- Snippet Engine & its associated nvim-cmp source
			{
				'L3MON4D3/LuaSnip',
				build = (function()
					-- Build Step is needed for regex support in snippets.
					-- This step is not supported in many windows environments.
					-- Remove the below condition to re-enable on windows.
					if vim.fn.has 'win32' == 1 or vim.fn.executable 'make' == 0 then
						return
					end
					return 'make install_jsregexp'
				end)(),
				config = function()
					require("luasnip.loaders.from_snipmate").lazy_load()
				end,
				dependencies = {
					-- `friendly-snippets` contains a variety of premade snippets.
					--    See the README about individual language/framework/plugin snippets:
					--    https://github.com/rafamadriz/friendly-snippets
					-- {
					-- 	'rafamadriz/friendly-snippets',
					-- 	config = function()
					-- 		require('luasnip.loaders.from_vscode').lazy_load()
					-- 	end,
					-- },
				},
			},
			'saadparwaiz1/cmp_luasnip',

			-- Adds other completion capabilities.
			--  nvim-cmp does not ship with all sources by default. They are split
			--  into multiple repos for maintenance purposes.
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-path',
		},
		config = function()
			-- See `:help cmp`
			local cmp = require 'cmp'
			local luasnip = require 'luasnip'

			cmp.setup {
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},
				completion = { completeopt = 'menu,menuone,noinsert' },

				-- For an understanding of why these mappings were
				-- chosen, you will need to read `:help ins-completion`
				--
				-- No, but seriously. Please read `:help ins-completion`, it is really good!
				mapping = cmp.mapping.preset.insert {
					-- Select the [n]ext item
					['<C-n>'] = cmp.mapping.select_next_item(),
					-- Select the [p]revious item
					['<C-p>'] = cmp.mapping.select_prev_item(),

					-- Scroll the documentation window [b]ack / [f]orward
					['<C-b>'] = cmp.mapping.scroll_docs(-4),
					['<C-f>'] = cmp.mapping.scroll_docs(4),

					-- Accept ([y]es) the completion.
					--  This will auto-import if your LSP supports it.
					--  This will expand snippets if the LSP sent a snippet.
					['<C-y>'] = cmp.mapping.confirm { select = true },

					-- If you prefer more traditional completion keymaps,
					-- you can uncomment the following lines
					--['<CR>'] = cmp.mapping.confirm { select = true },
					--['<Tab>'] = cmp.mapping.select_next_item(),
					--['<S-Tab>'] = cmp.mapping.select_prev_item(),

					-- Manually trigger a completion from nvim-cmp.
					--  Generally you don't need this, because nvim-cmp will display
					--  completions whenever it has completion options available.
					['<C-Space>'] = cmp.mapping.complete {},

					-- Think of <c-l> as moving to the right of your snippet expansion.
					--  So if you have a snippet that's like:
					--  function $name($args)
					--    $body
					--  end
					--
					-- <c-l> will move you to the right of each of the expansion locations.
					-- <c-h> is similar, except moving you backwards.
					['<C-l>'] = cmp.mapping(function()
						if luasnip.expand_or_locally_jumpable() then
							luasnip.expand_or_jump()
						end
					end, { 'i', 's' }),
					['<C-h>'] = cmp.mapping(function()
						if luasnip.locally_jumpable(-1) then
							luasnip.jump(-1)
						end
					end, { 'i', 's' }),

					-- For more advanced Luasnip keymaps (e.g. selecting choice nodes, expansion) see:
					--    https://github.com/L3MON4D3/LuaSnip?tab=readme-ov-file#keymaps
				},
				sources = {
					{
						name = 'lazydev',
						-- set group index to 0 to skip loading LuaLS completions as lazydev recommends it
						group_index = 0,
					},
					{ name = 'nvim_lsp' },
					{ name = 'luasnip' },
					{ name = 'path' },
				},
			}
		end,
	},
	{
		'neovim/nvim-lspconfig',
		dependencies = {
			'hrsh7th/cmp-nvim-lsp',
		},
		config = function()
			local lspconfig = require('lspconfig')
			-- local capabilities = require('cmp_nvim_lsp').default_capabilities()
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

			local servers = {
				cssls = {},
				tailwindcss = {
					on_attach = on_attach,
					capabilities = capabilities,
					root_dir = function(...)
						return require("lspconfig.util").root_pattern(".git")(...)
					end,
				},
				ts_ls = {
					on_attach = on_attach,
					capabilities = capabilities,
					root_dir = function(...)
						return require("lspconfig.util").root_pattern(".git")(...)
					end,
					single_file_support = false,
					settings = {
						typescript = {
							inlayHints = {
								includeInlayParameterNameHints = "literal",
								includeInlayParameterNameHintsWhenArgumentMatchesName = false,
								includeInlayFunctionParameterTypeHints = true,
								includeInlayVariableTypeHints = false,
								includeInlayPropertyDeclarationTypeHints = true,
								includeInlayFunctionLikeReturnTypeHints = true,
								includeInlayEnumMemberValueHints = true,
							},
						},
						javascript = {
							inlayHints = {
								includeInlayParameterNameHints = "all",
								includeInlayParameterNameHintsWhenArgumentMatchesName = false,
								includeInlayFunctionParameterTypeHints = true,
								includeInlayVariableTypeHints = true,
								includeInlayPropertyDeclarationTypeHints = true,
								includeInlayFunctionLikeReturnTypeHints = true,
								includeInlayEnumMemberValueHints = true,
							},
						},
					},
				},
				html = {
					on_attach = on_attach,
					capabilities = capabilities,
				},
				yamlls = {
					on_attach = on_attach,
					capabilities = capabilities,
					settings = {
						yaml = {
							keyOrdering = false,
						},
					},
				},
				rust_analyzer = {
					on_attach = on_attach,
					capabilities = capabilities,
					settings = {
						rust_analyzer = {
							useLibraryCodeForTypes = true,
							autoSearchPaths = true,
							autoImportCompletions = false,
							reportMissingImports = true,
							followImportForHints = true,

							cargo = {
								allFeatures = true,
							},
							checkOnSave = {
								command = "cargo clippy",
							},
						},
					}
				},
				pyright = {
					on_attach = on_attach,
					capabilities = capabilities,
					before_init = function(_, config)
						local match = vim.fn.glob(require('lspconfig.util').path.join(config.root_dir, 'poetry.lock'))
						if match ~= '' then
							local venv = vim.fn.trim(vim.fn.system('poetry env info -p'))
							config.settings.python.pythonPath = require('lspconfig.util').path.join(venv, 'bin', 'python')
							print("Python venv path:", venv)
						end
					end
				},
				lua_ls = {
					on_attach = on_attach,
					capabilities = capabilities,
					-- enabled = false,
					single_file_support = true,
					settings = {
						Lua = {
							workspace = {
								checkThirdParty = false,
							},
							completion = {
								workspaceWord = true,
								callSnippet = "Both",
							},
							misc = {
								parameters = {
									-- "--log-level=trace",
								},
							},
							hint = {
								enable = true,
								setType = false,
								paramType = true,
								paramName = "Disable",
								semicolon = "Disable",
								arrayIndex = "Disable",
							},
							doc = {
								privateName = { "^_" },
							},
							type = {
								castNumberToInteger = true,
							},
							diagnostics = {
								disable = { "incomplete-signature-doc", "trailing-space" },
								-- enable = false,
								groupSeverity = {
									strong = "Warning",
									strict = "Warning",
								},
								groupFileStatus = {
									["ambiguity"] = "Opened",
									["await"] = "Opened",
									["codestyle"] = "None",
									["duplicate"] = "Opened",
									["global"] = "Opened",
									["luadoc"] = "Opened",
									["redefined"] = "Opened",
									["strict"] = "Opened",
									["strong"] = "Opened",
									["type-check"] = "Opened",
									["unbalanced"] = "Opened",
									["unused"] = "Opened",
								},
								unusedLocalExclude = { "_*" },
							},
							format = {
								enable = true,
								defaultConfig = {
									indent_style = "space",
									indent_size = "2",
									continuation_indent_size = "2",
								},
							},
						},
					},
				},
			}


			for server_name, server_config in pairs(servers) do
				lspconfig[server_name].setup(server_config)
			end
		end,
	},
	{
		"stevearc/conform.nvim",
		event = { "BufReadPre", "BufNewFile" },
		config = function()
			require('conform').setup({
				formatters_by_ft = {
					python = { "isort", "black" },
					html = { "htmlbeautifier" },
					rust = { "rustfmt" },
				},
				format_after_save = {
					lsp_fallback = true,
				},
			})
		end
	},
	{
		'b0o/incline.nvim',
		dependencies = {
			"SmiteshP/nvim-navic",
		},
		config = function()
			local helpers = require 'incline.helpers'
			local navic = require 'nvim-navic'
			local devicons = require 'nvim-web-devicons'
			require('incline').setup {
				window = {
					padding = 0,
					margin = { horizontal = 0, vertical = 0 },
				},
				render = function(props)
					local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ':t')
					if filename == '' then
						filename = '[No Name]'
					end
					local ft_icon, ft_color = devicons.get_icon_color(filename)
					local modified = vim.bo[props.buf].modified
					local res = {
						ft_icon and { ' ', ft_icon, '  ', guibg = ft_color, guifg = helpers.contrast_color(ft_color) } or '',
						' ',
						{ filename, gui = modified and 'bold,italic' or 'bold' },
					}
					for _, item in ipairs(navic.get_data(props.buf) or {}) do
						table.insert(res, {
							{ ' > ',     group = 'NavicSeparator' },
							{ item.icon, group = 'NavicIcons' .. item.type },
							{ item.name, group = 'NavicText' },
						})
					end
					table.insert(res, ' ')
					return res
				end,
			}
		end,
		-- Optional: Lazy load Incline
		event = 'VeryLazy',
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		keys = {
			{ "]t", function() require("todo-comments").jump_next() end, desc = "Next todo comment" },
			{ "[t", function() require("todo-comments").jump_prev() end, desc = "Previous todo comment" },
		},
		opts = {
			-- your configuration comes here
			-- or leave it empty to use the default settings
			-- refer to the configuration section below
		}
	},
	{
		"aznhe21/actions-preview.nvim",
		config = function()
			vim.keymap.set({ "v", "n" }, "<leader>c", require("actions-preview").code_actions)
		end,
	}
}

return M
