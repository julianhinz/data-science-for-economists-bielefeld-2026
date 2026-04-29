#!/bin/bash
###
# 00 - Filter Colombian Imports from USA
# 260226
###

# create imports_usa folder
mkdir -p temp/imports_usa

# loop over all csv files
for f in temp/Impo_2018/*csv; do
    echo "Processing $f"
    # Extract the header and filter rows where column 4 equals "249"
    awk -F';' 'NR==1 || $4 == "249"' "$f" >> temp/imports_usa/$(basename "$f")
done
