# for every file .md search all files for use of that .md
# If none found, report unlinked file

import os, sys



# routine walk through all the directories, search each file
# report .md files not found in any .md file
def list_in_dr(top_dir):
 	
    count = 0
    for root, drs, fles in os.walk(top_dir):
        for fle in fles:
            if '.md' in fle:

                infile_name = os.path.join(root, fle)	
                with open(infile_name, 'r') as infile:
                    if searchstring in infile.read():
                        print 'found %s in file %s' % (searchstring, infile_name)
                        count = count + 1
                    infile.close()
    print 'Total files with %s : %d ' % (searchstring, count)

# main

# keyword is filename
searchstring = "FSET.md" 
top_dir = '/project/eesdev/tam/clone/LaGriT/docs/pages/'
list_in_dr(top_dir)


