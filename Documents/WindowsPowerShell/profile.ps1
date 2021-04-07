function dot {
    git --git-dir="$env:USERPROFILE/.dot" --work-tree="$env:USERPROFILE" @args
}
