#!/usr/bin/env nix-shell
#! nix-shell -p gource -p ffmpeg -i bash

gource --path . --seconds-per-day 0.15 -1280x720 --file-idle-time 0 --auto-skip-seconds 0.75 --multi-sampling --stop-at-end --highlight-users --hide filenames,mouse,progress --max-files 0 --background-colour 000000 --disable-bloom --font-size 24 --output-ppm-stream - --output-framerate 30 -o - | ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -preset ultrafast -pix_fmt yuv420p -crf 1 -threads 0 -bf 0 output.mp4
