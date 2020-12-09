Notes for scripts in this directory.
These are very brute force, just walk through all directories and do something.
Output is written to stdout unless the python script is edited to write to a file name.
The root directory is defined in the python script.
All good enough to do some work on the files to fix and reduce files.


Find duplicate .md files and remove old copies
This was done Aug 10 2020 and most were removed.
Most old copies were located in /pages/docs and dir names html.
Most good copies are located in subdirs under /pages/docs/commands or demos

python find_duplicate_files.py
writes find_duplicate_files.out.txt

Find unlinked .md files and either link or remove
Note some files are referenced as .md and some are referenced as .html

python find_unlinked_files.py
writes find_unlinked_files.out.txt
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





