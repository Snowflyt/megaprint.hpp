#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT=$(pwd)
# Normalize PROJECT_ROOT (Windows paths -> Unix style)
PROJECT_ROOT=${PROJECT_ROOT//\\//}

input=$(cat)

# Detect color support
supports_color() {
  if [[ -t 1 ]] && [[ -n "${TERM:-}" ]] && [[ "$TERM" != "dumb" ]] && [[ -z "${NO_COLOR:-}" ]]; then
    return 0
  else
    return 1
  fi
}

if supports_color; then
  RED=$'\033[31m'
  YELLOW=$'\033[33m'
  GREEN=$'\033[32m'
  RESET=$'\033[0m'
else
  RED=''
  YELLOW=''
  GREEN=''
  RESET=''
fi

# Calculate filename column width
fname_w=$(echo "$input" | awk -v root="$PROJECT_ROOT/" '
NR>1 && $1 != "TOTAL" && $1 != "Filename" && $1 !~ /^-+/ {
  file=$1
  gsub("\\\\", "/", file)   # Normalize slashes
  if (file ~ /^[A-Za-z]:/) {
    drive = tolower(substr(file, 1, 1))
    file = "/" drive substr(file, 3)
  }
  root_norm = root
  gsub("\\\\", "/", root_norm)
  if (root_norm ~ /^[A-Za-z]:/) {
    drive = tolower(substr(root_norm, 1, 1))
    root_norm = "/" drive substr(root_norm, 3)
  }
  sub(root_norm, "", file)
  if (length(file) > max) max = length(file)
}
END {
  width = (max > length("File") ? max : length("File"))
  print width + 1
}')

# Calculate numeric column widths
calc_width() {
  col=$1
  header=$2
  echo "$input" | awk -v c="$col" -v hlen="${#header}" '
  BEGIN {max=hlen}
  $1 != "Filename" && $1 !~ /^-+/ {
    val=$c
    gsub("%","",val)
    if (length(val) > max) max=length(val)
  }
  END {print max}'
}

region_w=$(calc_width 4 "% Region")
branch_w=$(calc_width 13 "% Branch")
funcs_w=$(calc_width 7 "% Funcs")
lines_w=$(calc_width 10 "% Lines")

# Horizontal line
total_width=$((fname_w + region_w + branch_w + funcs_w + lines_w + 13))
print_line() { printf '%*s\n' "$total_width" '' | tr ' ' '-'; }

# Colorizer
colorize() {
  val=$(echo "$1" | tr -d '%')
  pct=$(printf "%.2f" "$val")
  width=$2
  color=$RESET

  if awk "BEGIN {exit !($pct >= 80)}"; then
    color=$GREEN
  elif awk "BEGIN {exit !($pct >= 50)}"; then
    color=$YELLOW
  else
    color=$RED
  fi

  formatted=$(printf "%*s%%" "$width" "$pct")
  printf "%s%s%s" "$color" "$formatted" "$RESET"
}

# Header
print_line
printf "%-*s | %*s | %*s | %*s | %*s\n" \
  "$fname_w" "File" \
  "$region_w" "% Region" \
  "$branch_w" "% Branch" \
  "$funcs_w" "% Funcs" \
  "$lines_w" "% Lines"
print_line

# First print TOTAL as "All files"
total_line=$(echo "$input" | awk '$1 == "TOTAL"')
if [[ -n "$total_line" ]]; then
  read -r _ _ _ region _ _ funcs _ _ lines _ _ branches _ <<< "$total_line"
  printf "%-*s |" "$fname_w" "All files"
  colorize "$region" "$region_w"; printf " |"
  colorize "$branches" "$branch_w"; printf " |"
  colorize "$funcs" "$funcs_w"; printf " |"
  colorize "$lines" "$lines_w"; printf "\n"
fi

# Print all other files
echo "$input" | awk '$1 != "Filename" && $1 != "TOTAL" && $1 !~ /^-+/' | \
while read -r file _ _ region _ _ funcs _ _ lines _ _ branches _; do
  norm_file=${file//\\//}
  if [[ $norm_file =~ ^[A-Za-z]: ]]; then
    drive=$(echo "${norm_file:0:1}" | tr '[:upper:]' '[:lower:]')
    norm_file="/$drive${norm_file:2}"
  fi

  root_norm=${PROJECT_ROOT//\\//}
  if [[ $root_norm =~ ^[A-Za-z]: ]]; then
    drive=$(echo "${root_norm:0:1}" | tr '[:upper:]' '[:lower:]')
    root_norm="/$drive${root_norm:2}"
  fi

  rel_file="${norm_file#$root_norm/}"
  printf "%-*s |" "$fname_w" " $rel_file"
  colorize "$region" "$region_w"; printf " |"
  colorize "$branches" "$branch_w"; printf " |"
  colorize "$funcs" "$funcs_w"; printf " |"
  colorize "$lines" "$lines_w"; printf "\n"
done

print_line
