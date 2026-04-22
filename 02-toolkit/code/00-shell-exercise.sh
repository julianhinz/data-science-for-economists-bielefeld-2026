#!/usr/bin/env bash
set -euo pipefail

###
# 00 - Shell Exercise: Tiny Data Pipeline
# 260226
###

# This script creates a small CSV file and demonstrates common shell tools.
# Run from the module root:
#   bash code/00-shell-exercise.sh

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

mkdir -p temp output

cat > temp/exports.csv <<'CSV'
country,sector,value
DEU,cars,120
DEU,chemicals,80
FRA,cars,90
FRA,food,60
USA,tech,200
USA,cars,110
CHN,tech,180
CHN,textiles,70
BRA,food,55
BRA,minerals,95
CSV

echo "Created temp/exports.csv"

echo "\n1) Count rows (excluding header)"
tail -n +2 temp/exports.csv | wc -l

echo "\n2) List unique countries"
cut -d, -f1 temp/exports.csv | tail -n +2 | sort | uniq

echo "\n3) Filter rows with value > 100"
awk -F, 'NR==1 || $3 > 100' temp/exports.csv

echo "\n4) Total value by country"
awk -F, 'NR>1 {sum[$1]+=$3} END {for (c in sum) print c, sum[c]}' \
  temp/exports.csv | sort -k2,2nr

echo "\n5) Save totals to output/country_totals.txt"
awk -F, 'NR>1 {sum[$1]+=$3} END {for (c in sum) print c, sum[c]}' \
  temp/exports.csv | sort -k2,2nr > output/country_totals.txt

echo "Saved output/country_totals.txt"
