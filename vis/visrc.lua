-- load standard vis module, providing parts of the Lua API
require('vis')
require('plugins/filetype')
require('plugins/textobject-lexer')

vis.events.subscribe(vis.events.INIT, function()
	-- Your global configuration options
	vis:command('set theme parchment')

	-- Use Workman bindings if needed
	vis:command('langmap neyojkhlNEYOJKHL jkhlyneoJKHLYNEO')
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- Your per window configuration options e.g.
	-- vis:command('set number')
	vis:command('set number')
	vis:command('set autoindent')
	vis:command('set cursorline')
end)
