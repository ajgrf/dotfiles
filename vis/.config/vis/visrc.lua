-- load standard vis module, providing parts of the Lua API
require('vis')
require('plugins/filetype')
require('plugins/textobject-lexer')
require('os')

vis.events.subscribe(vis.events.INIT, function()
	-- Your global configuration options
	vis:command('set tabwidth 4')
	vis:command('set theme plan9')

	-- Use Colemak bindings if needed
	if os.getenv('COLEMAK') == '1' then
		vis:command('langmap neikuljNEIKULJ jklniueJKLNIUE')
	end
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- Your per window configuration options e.g.
	-- vis:command('set number')
	vis:command('set number')
end)
