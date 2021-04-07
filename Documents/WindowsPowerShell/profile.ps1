# Use Emacs-style keybindings
Set-PSReadLineOption -EditMode Emacs

function dot {
    git --git-dir="$env:USERPROFILE/.dot" --work-tree="$env:USERPROFILE" @args
}
