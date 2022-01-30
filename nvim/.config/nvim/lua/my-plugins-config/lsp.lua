local cmp = require'cmp'

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

cmp.setup({
	snippet = {
		expand = function(args)
			-- For `vsnip` user.
			-- vim.fn["vsnip#anonymous"](args.body)

			-- For `luasnip` user.
			require("luasnip").lsp_expand(args.body)

			-- For `ultisnips` user.
			-- vim.fn["UltiSnips#Anon"](args.body)
		end,
	},
	mapping = {
		["<C-u>"] = cmp.mapping.scroll_docs(-4),
		["<C-d>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
	},
  --formatting = {
      --format = function(entry, vim_item)
          --vim_item.kind = lspkind.presets.default[vim_item.kind]
          --local menu = source_mapping[entry.source.name]
          --if entry.source.name == 'cmp_tabnine' then
              --if entry.completion_item.data ~= nil and entry.completion_item.data.detail ~= nil then
                  --menu = entry.completion_item.data.detail .. ' ' .. menu
              --end
              --vim_item.kind = ''
          --end
          --vim_item.menu = menu
          --return vim_item
      --end
  --},
	sources = {
    { name = "nvim_lsp" },

    -- For luasnip user.
    { name = "luasnip" },

    { name = "buffer" },
	},
})

local function config(_config)
	return vim.tbl_deep_extend("force", {
		capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities()),
	}, _config or {})
end

require('lspconfig').tsserver.setup(config())

require('lspconfig').ccls.setup(config())

require('lspconfig').jedi_language_server.setup(config())
