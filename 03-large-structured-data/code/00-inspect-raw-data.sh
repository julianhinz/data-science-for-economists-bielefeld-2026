#!/usr/bin/env bash

###
# 01 - Inspect the Colombian import data
# 2026-03-03
#
# This script introduces basic shell commands for inspecting
# the raw import data before filtering it.
#
# It shows how to:
# - inspect folders and files
# - look at the first rows of a CSV file
# - count rows and files
# - inspect columns using awk
# - discover that not all CSV files use the same separator
# - understand the filtering condition used later
#
# How to run:
#
# From the main lesson folder:
#
#   bash code/01-inspect-raw-data.sh
#
# Or, from inside the code/ folder:
#
#   bash 01-inspect-raw-data.sh
#
# Or, on Linux/macOS/Git Bash:
#
#   chmod +x code/01-inspect-raw-data.sh
#   ./code/01-inspect-raw-data.sh
###

# https://www.dian.gov.co/atencionciudadano/formulariosinstructivos/Formularios/2009/Paises_2009.pdf?utm_source=chatgpt.com
# Stop the script if any command fails.
set -e

# If a wildcard matches no files, expand it to nothing.
# This avoids trying to process the literal string temp/Impo_2018/*.csv.
shopt -s nullglob

# Move to the main lesson folder.
# The script is inside code/, so dirname "$0" gives code/.
# Adding /.. moves one level up to 03-large-structured-data/.
cd "$(dirname "$0")/.."

# Store the list of CSV files in an array.
csv_files=(temp/Impo_2018/*.csv)

# Check that the data exist.
if [ ${#csv_files[@]} -eq 0 ]; then
  echo "Error: no CSV files found in temp/Impo_2018/"
  echo "Please run the download/extraction script first."
  exit 1
fi

# We use two example files below.
# Agosto.csv uses semicolons.
# Enero_2018.csv uses commas.
august_file="temp/Impo_2018/Agosto.csv"
january_file="temp/Impo_2018/Enero_2018.csv"

if [ ! -f "$august_file" ]; then
  echo "Error: cannot find $august_file"
  exit 1
fi

if [ ! -f "$january_file" ]; then
  echo "Error: cannot find $january_file"
  exit 1
fi

echo "========================================"
echo "1. Where are we?"
echo "========================================"

# Print the current working directory.
pwd

echo
echo "========================================"
echo "2. What files and folders do we have?"
echo "========================================"

# List files and folders in the current directory.
ls

echo
echo "========================================"
echo "3. What is inside the extracted data folder?"
echo "========================================"

# List files inside the extracted import data folder.
ls temp/Impo_2018

echo
echo "========================================"
echo "4. Show file sizes"
echo "========================================"

# -l gives a detailed list.
# -h prints file sizes in a human-readable format.
ls -lh temp/Impo_2018

echo
echo "========================================"
echo "5. Count how many CSV files we have"
echo "========================================"

# ${#csv_files[@]} gives the number of elements in the csv_files array.
echo "${#csv_files[@]}"

echo
echo "========================================"
echo "6. Look at the first rows of one CSV file"
echo "========================================"

# head prints the first 10 lines by default.
head "$august_file"

echo
echo "========================================"
echo "7. Look only at the header"
echo "========================================"

# -n 1 means: print only the first line.
head -n 1 "$august_file"

echo
echo "========================================"
echo "8. Count the number of rows in one file"
echo "========================================"

# wc -l counts lines.
wc -l "$august_file"

echo
echo "========================================"
echo "9. Check the file type"
echo "========================================"

# file gives information about the file format.
file "$august_file"

echo
echo "========================================"
echo "10. Print the first 5 rows with awk"
echo "========================================"

# awk processes text files row by row and column by column.
# -F';' tells awk that columns are separated by semicolons.
# NR is the row number.
# NR <= 5 means: print the first 5 rows.
awk -F';' 'NR <= 5 { print }' "$august_file"

echo
echo "========================================"
echo "11. Print selected columns"
echo "========================================"

# $1, $2, $3, etc. refer to columns.
# This prints columns 1 to 5 for the first 10 rows.
awk -F';' 'NR <= 10 { print $1, $2, $3, $4, $5 }' "$august_file"

echo
echo "========================================"
echo "12. Print column numbers and column names"
echo "========================================"

# NR == 1 means: only use the first row, the header.
# NF is the number of fields/columns.
# The loop prints each column number and its name.
awk -F';' 'NR == 1 { for (i = 1; i <= NF; i++) print i, $i }' "$august_file"

echo
echo "========================================"
echo "13. Print the name of column 4"
echo "========================================"

# This helps us verify what column 4 contains.
awk -F';' 'NR == 1 { print $4 }' "$august_file"

echo
echo "========================================"
echo "14. Show a few values from column 4"
echo "========================================"

# NR > 1 skips the header.
# This prints values from column 4 and shows only the first 10.
awk -F';' 'NR > 1 { print $4 }' "$august_file" | head

echo
echo "========================================"
echo "15. Count how many rows have column 4 equal to 249"
echo "========================================"

# This counts rows where column 4 is equal to 249.
# In the next script, we use this logic to filter imports from the USA.
awk -F';' '$4 == "249" { count++ } END { print count + 0 }' "$august_file"

echo
echo "========================================"
echo "16. Show the header and a few rows where column 4 is 249"
echo "========================================"

# NR == 1 keeps the header.
# || means "or".
# $4 == "249" keeps rows where column 4 is equal to 249.
awk -F';' 'NR == 1 || $4 == "249"' "$august_file" | head

echo
echo "========================================"
echo "17. Count how often each value of column 4 appears"
echo "========================================"

# count[$4]++ counts occurrences of each value in column 4.
# The output is sorted to make it easier to inspect.
awk -F';' 'NR > 1 { count[$4]++ } END { for (x in count) print count[x], x }' temp/Impo_2018/Agosto.csv | sort -nr | head

echo
echo "========================================"
echo "18. Count rows in all monthly CSV files"
echo "========================================"

# This gives the number of rows in each CSV file.
wc -l temp/Impo_2018/*.csv

echo
echo "========================================"
echo "19. First attempt: count USA rows assuming all files use semicolons"
echo "========================================"

# This repeats the count for every CSV file.
# At this stage, we assume that all files use semicolons.
# The output will look suspicious: some months may show zero USA rows.
for f in "${csv_files[@]}"; do
  echo "$f"
  awk -F';' '$4 == "249" { count++ } END { print count + 0 }' "$f"
done

echo
echo "========================================"
echo "20. Diagnose the suspicious zeros"
echo "========================================"

# The zeros are suspicious. Let us compare the headers of two files.
# Agosto.csv uses semicolons.
# Enero_2018.csv uses commas.
echo "Header of $august_file:"
head -n 1 "$august_file"

echo
echo "Header of $january_file:"
head -n 1 "$january_file"

echo
echo "Notice:"
echo "- In Agosto.csv, columns are separated by semicolons: ;"
echo "- In Enero_2018.csv, columns are separated by commas: ,"

echo
echo "========================================"
echo "21. What happens if we use the wrong separator?"
echo "========================================"

# Here we incorrectly use semicolon as the separator for January.
# Because January is comma-separated, awk treats the whole row as one column.
echo "Using the wrong separator ; for January:"
awk -F';' 'NR == 1 { print "Number of columns detected:", NF }' "$january_file"

# Here we use the correct separator, comma.
echo "Using the correct separator , for January:"
awk -F',' 'NR == 1 { print "Number of columns detected:", NF }' "$january_file"

echo
echo "========================================"
echo "22. Print selected columns from January using the correct separator"
echo "========================================"

# For January, we need -F',' instead of -F';'.
awk -F',' 'NR <= 10 { print $1, $2, $3, $4, $5 }' "$january_file"

echo
echo "========================================"
echo "23. Count USA rows in January using the correct separator"
echo "========================================"

# Some January values are stored with quotation marks, for example "249".
# We remove quotation marks before comparing.
awk -F',' '
  NR > 1 {
    paispro = $4
    gsub(/"/, "", paispro)

    if (paispro == "249") {
      count++
    }
  }

  END {
    print count + 0
  }
' "$january_file"

echo
echo "========================================"
echo "24. Detect the separator used by each file"
echo "========================================"

# We can detect the separator from the header.
# If the header contains a semicolon, we use semicolon.
# Otherwise, we use comma.
for f in "${csv_files[@]}"; do

  header=$(head -n 1 "$f")

  if [[ "$header" == *";"* ]]; then
    sep=";"
  else
    sep=","
  fi

  echo "$f uses separator: $sep"

done

echo
echo "========================================"
echo "25. Count USA rows using the correct separator for each file"
echo "========================================"

# Now we repeat the count, but we first detect the separator file by file.
# This is the safer logic used in the filtering script.
for f in "${csv_files[@]}"; do

  header=$(head -n 1 "$f")

  if [[ "$header" == *";"* ]]; then
    sep=";"
  else
    sep=","
  fi

  echo "$f"

  awk -F"$sep" '
    NR > 1 {
      paispro = $4
      gsub(/"/, "", paispro)

      if (paispro == "249") {
        count++
      }
    }

    END {
      print count + 0
    }
  ' "$f"

done

echo
echo "========================================"
echo "26. Preview the filtering logic used later"
echo "========================================"

# This is the core logic used in the filtering script:
# keep the header OR keep rows where column 4 equals 249.
#
# We demonstrate it on Agosto.csv, which uses semicolons.
awk -F';' '
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
' "$august_file" | head

echo
echo "========================================"
echo "27. Preview the same filtering logic on January"
echo "========================================"

# We demonstrate the same logic on Enero_2018.csv, which uses commas.
awk -F',' '
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
' "$january_file" | head

echo
echo "Data inspection complete."