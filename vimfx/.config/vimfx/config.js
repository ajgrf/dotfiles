// Needed to access environment variables
const { require } = Components.utils.import("resource://gre/modules/commonjs/toolkit/require.js", {});
var system = require("sdk/system");

let options = {
    "hints.chars": "12345678 90",
    "prev_patterns": "prev  previous  ‹  «  ◀  ←  <<  <  back  newer  上一[頁页]",
    "next_patterns": "next  ›  »  ▶  →  >>  >  more  older  下一[頁页]",
    "mode.normal.scroll_half_page_down": "<c-d>",
    "mode.normal.scroll_half_page_up": "<c-u>",
    "mode.normal.tab_select_previous": "K    gT",
    "mode.normal.tab_select_next": "J    gt",
    "mode.normal.tab_move_backward": "gK",
    "mode.normal.tab_move_forward": "gJ",
    "mode.normal.tab_close": "d",
    "mode.normal.tab_restore": "u",
    "mode.normal.tab_restore_list": "U",
    "mode.normal.tab_close_to_end": "gd$",
    "mode.normal.tab_close_other": "gda",
    "mode.normal.follow_previous": "[[",
    "mode.normal.follow_next": "]]"
}

let colemak_bindings = {
    "mode.normal.go_up_path": "gl",
    "mode.normal.go_to_root": "gL",
    "mode.normal.history_forward": "I",
    "mode.normal.scroll_right": "i",
    "mode.normal.scroll_down": "n",
    "mode.normal.scroll_up": "e",
    "mode.normal.tab_select_previous": "E    gT",
    "mode.normal.tab_select_next": "N    gt",
    "mode.normal.tab_select_most_recent": "gi",
    "mode.normal.tab_select_oldest_unvisited": "gI",
    "mode.normal.tab_move_backward": "gE",
    "mode.normal.tab_move_forward": "gN",
    "mode.normal.tab_restore": "l",
    "mode.normal.tab_restore_list": "L",
    "mode.normal.follow_in_focused_tab": "jt",
    "mode.normal.follow_in_window": "jw",
    "mode.normal.follow_in_private_window": "jp",
    "mode.normal.follow_focus": "jf",
    "mode.normal.open_context_menu": "jc",
    "mode.normal.click_browser_element": "jb",
    "mode.normal.focus_text_input": "gu",
    "mode.normal.find_next": "k",
    "mode.normal.find_previous": "K",
    "mode.normal.enter_mode_ignore": "u",
    "mode.normal.quote": "U",
    "mode.caret.move_right": "i",
    "mode.caret.move_down": "n",
    "mode.caret.move_up": "e"
}

// Apply options and, if needed, Colemak bindings
if (system.env.COLEMAK) {
	for (option in options) { vimfx.set(option, options[option]); }
	for (option in colemak_bindings) {
		vimfx.set(option, colemak_bindings[option]);
	}
} else {
	// Reset Colemak bindings
	for (option in colemak_bindings) {
		vimfx.set(option, vimfx.getDefault(option));
	}
	for (option in options) { vimfx.set(option, options[option]); }
}
