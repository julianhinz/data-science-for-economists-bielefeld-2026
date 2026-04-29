#!/usr/bin/env bash

###
# 00 - Download the data for firm-level analysis
# 2026-03-03
#
# This script:
# - downloads data from the CDN, if not already downloaded
# - unzips the data, if not already extracted
# - removes file formats that we do not use in class
#
# How to run:
#
# From the main lesson folder:
#
#   bash code/00-download.sh
#
# Or, from inside the code/ folder:
#
#   bash 00-download.sh
#
# Or, on Linux/macOS/Git Bash:
#
#   chmod +x code/00-download.sh
#   ./code/00-download.sh
#
# The direct execution command works because the first line tells
# the system to run this script with bash.
###

# Stop the script if any command fails.
set -e

# If a wildcard matches no files, expand it to nothing.
# This avoids trying to unzip a literal "*.zip" when no zip files exist.
shopt -s nullglob

# Move to the main lesson folder.
# The script is inside code/, so dirname "$0" gives code/.
# Adding /.. moves one level up to 03-large-structured-data/.
cd "$(dirname "$0")/.."

# Create input/ and temp/ folders if they do not already exist.
# -p means: do not complain if the folders already exist.
mkdir -p input temp

# Download the main zip file only if it does not already exist.
if [ ! -f input/Impo_2018.zip ]; then
  echo "Downloading data from CDN..."

  # Use wget if available; otherwise use curl.
  # command -v checks whether a command exists.
  # >/dev/null 2>&1 hides the output of that check.
  if command -v wget >/dev/null 2>&1; then

    # wget downloads a file from the internet.
    # -O specifies the output file name.
    wget -O input/Impo_2018.zip https://cdn.jhi.nz/data/Impo_2018.zip

  elif command -v curl >/dev/null 2>&1; then

    # curl also downloads files from the internet.
    # -L follows redirects.
    # -o specifies the output file name.
    curl -L -o input/Impo_2018.zip https://cdn.jhi.nz/data/Impo_2018.zip

  else

    echo "Error: neither wget nor curl is installed."
    echo "Please install one of them and run the script again."
    exit 1

  fi

else

  echo "input/Impo_2018.zip already exists; skipping download."

fi

# Extract the main zip file.
# unzip extracts files from a .zip archive.
# -q means quiet mode: print less output.
# -n means never overwrite existing files.
# -d specifies the destination folder.
echo "Extracting main archive if needed..."
unzip -q -n input/Impo_2018.zip -d temp/

# Extract the monthly zip files inside temp/Impo_2018/.
echo "Extracting monthly archives if needed..."

for zipfile in temp/Impo_2018/*.zip; do

  # "$zipfile" is quoted to protect paths that may contain spaces.
  unzip -q -n "$zipfile" -d temp/Impo_2018/

done

# Remove files that are not needed for the class.
# -f means: do not give an error if no matching files exist.
echo "Removing unused file formats..."
rm -f temp/Impo_2018/*.zip
rm -f temp/Impo_2018/*.sav
rm -f temp/Impo_2018/*.dta
rm -f temp/Impo_2018/*.txt

echo "Data download and extraction complete."