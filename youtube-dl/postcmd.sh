#!/bin/sh
set -e

input_file="$1"
output_file="${1%.*}.mkv"
info_json_file="${1%.*}.info.json"

# remux bad container formats to matroska
if echo "$input_file" | egrep -q "\.(3g[p2]|avi|divx|f[l4]v|mp4|mov|wmv)$"; then
    echo "[postcmd] Remuxing to '$output_file'"
    ffmpeg -hide_banner -loglevel error -i "$input_file" -c:v copy -c:a copy \
           -c:s copy "$output_file"
    echo "[postcmd] Deleting original file '$input_file'"
    rm "$input_file"
fi

# download is complete, no need to resume later
echo "[postcmd] Deleting JSON metadata file"
rm "$info_json_file"
