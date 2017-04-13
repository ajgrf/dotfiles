-- Eight-color scheme
local lexers = vis.lexers
local normal = 'fore:black,back:white'

lexers.STYLE_DEFAULT = normal
lexers.STYLE_NOTHING = ''
lexers.STYLE_CLASS = normal
lexers.STYLE_COMMENT = 'fore:blue'
lexers.STYLE_CONSTANT = normal
lexers.STYLE_DEFINITION = 'fore:blue,bold' --
lexers.STYLE_ERROR = 'fore:red,italics' --
lexers.STYLE_FUNCTION = normal
lexers.STYLE_KEYWORD = normal
lexers.STYLE_LABEL = normal
lexers.STYLE_NUMBER = normal
lexers.STYLE_OPERATOR = normal
lexers.STYLE_REGEX = 'fore:green'
lexers.STYLE_STRING = 'fore:green'
lexers.STYLE_PREPROCESSOR = normal
lexers.STYLE_TAG = normal
lexers.STYLE_TYPE = normal
lexers.STYLE_VARIABLE = normal
lexers.STYLE_WHITESPACE = ''
lexers.STYLE_EMBEDDED = 'fore:magenta'
lexers.STYLE_IDENTIFIER = normal

lexers.STYLE_LINENUMBER = 'fore:black,back:cyan'
lexers.STYLE_CURSOR = 'reverse'
lexers.STYLE_CURSOR_PRIMARY = lexers.STYLE_CURSOR..',fore:magenta'
lexers.STYLE_CURSOR_LINE = 'underlined'
lexers.STYLE_COLOR_COLUMN = 'reverse,fore:red'
lexers.STYLE_SELECTION = 'back:yellow'

lexers.STYLE_STATUS = 'fore:black,back:cyan'
lexers.STYLE_STATUS_FOCUSED = 'fore:white,back:blue,bold'
lexers.STYLE_SEPARATOR = lexers.STYLE_DEFAULT
lexers.STYLE_INFO = 'fore:default,back:default,bold'
lexers.STYLE_EOF = ''
