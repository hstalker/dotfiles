#!/usr/bin/env sh
set -eu

if ! command -v swaylock >/dev/null 2>&1; then
   echo "Unable to find swaylock! Please install it on your system."
   exit 1
fi

images=;
swaylock_args=;

if command -v grim >/dev/null 2>&1; then
  # Grab a screenshot for each output display
  for output in $(swaymsg -t get_outputs | jq -r '.[] .name'); do
    image=$(mktemp --suffix=.png)
    images="${images} ${image}"
    swaylock_args="${swaylock_args} -i ${output}:${image}"
    grim -o "${output}" "${image}"
    if command -v mogrify >/dev/null 2>&1; then
      # Pixelate the screenshot
      mogrify -scale 10% -scale 1000% "${image}"
    elif command -v convert >/dev/null 2>&1; then
      # Use imagemagick to blur the screenshot
      convert "${image}" -filter Gaussian -blur 0x8 \
              "${image}"
    fi
  done
fi

# Initiate lock using these compiled arguments
IFS=$' \t\n' swaylock ${swaylock_args} -s center -f

# Clean up the screenshots we took for locking
IFS=$' \t\n' rm ${images}

exit 0
