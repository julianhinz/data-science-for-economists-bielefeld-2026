# Shell Cheatsheet (Bash)

## Navigation
- `pwd` — print current directory
- `ls -lah` — list files (long, all, human sizes)
- `cd path/` — change directory
- `cd ..` — up one level

## Files and Directories
- `mkdir -p data/raw` — create nested directories
- `touch notes.txt` — create empty file
- `cp src.txt dst.txt` — copy file
- `mv old.txt new.txt` — move/rename
- `rm file.txt` — remove file
- `rm -r folder/` — remove folder (careful)

## Viewing Files
- `cat file.txt` — print entire file
- `head -n 5 file.txt` — first 5 lines
- `tail -n 5 file.txt` — last 5 lines
- `less file.txt` — paged view (q to quit)

## Search and Filter
- `grep "pattern" file.txt` — find matching lines
- `rg "pattern" .` — ripgrep across files
- `awk -F, 'NR>1 {print $1}' file.csv` — print column 1 (CSV)
- `cut -d, -f2 file.csv` — extract column 2 (CSV)

## Sorting and Counts
- `sort file.txt` — sort lines
- `sort -k2,2nr file.csv` — sort CSV by column 2 numeric desc
- `uniq -c` — count duplicates (use after sort)
- `wc -l file.txt` — count lines

## Pipes and Redirects
- `cmd1 | cmd2` — pipe output to another command
- `cmd > out.txt` — write output
- `cmd >> out.txt` — append output

## Variables and Loops
- `name="Kiel"; echo "$name"`
- `for f in *.csv; do echo "$f"; done`

## Safety Tips
- Use `rm -i` for interactive deletes
- Check `pwd` before running destructive commands
- Prefer `mkdir -p` over `mkdir` to avoid errors
