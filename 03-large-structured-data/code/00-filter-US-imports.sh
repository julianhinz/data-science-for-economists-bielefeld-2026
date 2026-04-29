#!/usr/bin/env bash

###
# 02 - Filter Colombian imports from the USA
# 2026-03-03
#
# This script:
# - reads all CSV files in temp/Impo_2018/
# - detects whether each file uses comma or semicolon as separator
# - keeps the header row
# - keeps only rows where column 4, PAISPRO, equals 249
# - saves the filtered files in temp/imports_usa/
###

# Stop the script if any command fails.
set -e

# If a wildcard matches no files, expand it to nothing.
# This avoids trying to process the literal string temp/Impo_2018/*.csv.
shopt -s nullglob

# Move to the main lesson folder.
# The script is inside code/, so this moves one level up.
cd "$(dirname "$0")/.."

# Create the output folder if it does not already exist.
mkdir -p temp/imports_usa

# Store the list of CSV files in an array.
csv_files=(temp/Impo_2018/*.csv)

# Check that we actually found CSV files.
if [ ${#csv_files[@]} -eq 0 ]; then
  echo "Error: no CSV files found in temp/Impo_2018/"
  echo "Please run the download/extraction script first."
  exit 1
fi

# Loop over all CSV files in temp/Impo_2018/.
for f in "${csv_files[@]}"; do

  echo "Processing $f"

  # Read the first line of the file, i.e. the header.
  header=$(head -n 1 "$f")

  # Detect the separator used in this file.
  # Some files use semicolon ; and others use comma ,.
  if [[ "$header" == *";"* ]]; then
    sep=";"
  else
    sep=","
  fi

  echo "  Detected separator: $sep"

  # Filter the file.
  awk -F"$sep" '
    NR == 1 {
      print
      next
    }

    {
      paispro = $4
      gsub(/"/, "", paispro)

      if (paispro == "249") {
        print
      }
    }
  ' "$f" > "temp/imports_usa/$(basename "$f")"

done

echo "Filtering complete."