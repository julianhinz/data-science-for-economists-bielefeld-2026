#!/bin/bash

###
# 00 - Download the data for firm-level analysis
# 260303
# - downloads data from CDN
# - unzips the data
###

# create input and temp folders if they don't exist
mkdir -p input
mkdir -p temp

# download the data
echo "Downloading data from CDN..."
wget https://cdn.jhi.nz/data/Impo_2018.zip -O input/Impo_2018.zip

# unzip the data
echo "Unzipping data..."
unzip input/Impo_2018.zip -d temp

# unzip subfolders
echo "Unzipping subfolders..."
for i in temp/Impo_2018/*.zip; do
    unzip "$i" -d temp/Impo_2018/
done

# remove the .sav and .dta files
echo "Removing .sav and .dta files..."
rm temp/Impo_2018/*.zip
rm temp/Impo_2018/*.sav
rm temp/Impo_2018/*.dta
rm temp/Impo_2018/*.txt

echo "Data download and extraction complete."
