export APL_LIB_ROOT="${XDG_DATA_HOME:-$HOME/.local/share}/gnu-apl"

# Create GNU APL library paths
for d in workspaces wslib1 wslib2 wslib6 wslib7 wslib8 wslib9; do
	mkdir -p "${XDG_DATA_HOME:-$HOME/.local/share}/gnu-apl/$d"
done
