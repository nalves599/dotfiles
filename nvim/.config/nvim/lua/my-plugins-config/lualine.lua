local lualine = require('lualine')

local config = {
  options = {
		theme = 'auto',
		section_separators = {left='', right=''},
		component_separators = {left='', right=''},
		icons_enabled = true
	},
	sections = {
		lualine_b = { 'diff' },
		lualine_c = {
			{'diagnostics', {
				sources = {nvim_lsp, ale},
				symbols = {error = ':', warn =':', info = ':', hint = ':'}}},
			{'filename', file_status = true, path = 1}
		},
		lualine_x = { 'encoding', {'filetype', colored = false} },
	},
	inactive_sections = {
		lualine_c = {
			{'filename', file_status = true, path = 1}
		},
		lualine_x = { 'encoding', {'filetype', colored = false} },
	},
	tabline = {
		lualine_a = { 'hostname' },
		lualine_b = { 'branch' },
		lualine_z = { {'tabs', tabs_color = { inactive = "TermCursor", active = "ColorColumn" } } }
	},
	extensions = { fzf, fugitive },
}


lualine.setup(config)

