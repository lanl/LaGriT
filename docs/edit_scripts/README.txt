Notes for scripts in this directory.

Find unlinked .md files and either link or remove
Note some files are referenced as .md and some are referenced as .html

python find_unlinked_files.py
awk '{if ($1 == 0 && $2 == "Total" ) print $0}' find_unlinked_files.out.txt > unlinked.txt



Old scripts orginally written to keep track of file status.
Author Nathan Knapp for conversion from html to markdown pages

python get_site_map.py
writes docs/site_list.md and docs/site_map.md

Currently this does not give information about unused files.
It does not give file and path
Formatting is minimal

It looks like pages/new_md
are duplicates Nathan was using but were not used for final pages.
Check these and remove





