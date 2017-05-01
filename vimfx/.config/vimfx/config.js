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

for (option in options) {
    vimfx.set(option, options[option]);
}
