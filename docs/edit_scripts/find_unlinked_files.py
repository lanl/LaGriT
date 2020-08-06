# Search for unlinked .md files in all files
# USAGE: python find_unlinked_files.py
# for every file .md search all files for use of that .md
# If none found, report unlinked file

# MUST SET top_dir
# MUST SET file to open and write results

import os, sys



# routine walk through all the directories, search each file
# report .md files not found in any .md file
def find_word_walk(top_dir):
 	
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

    print '%d Total files with %s' % (count, searchstring)

# main -----------------------------------------------

searchstring = "notset" 
top_dir = '/project/eesdev/tam/clone/LaGriT/docs/'

sys.stdout=open("find_unlinked_files.out.txt","w")


# for each .md file find a link in another .md file
for root, drs, fles in os.walk(top_dir):
        for fle in fles:

#            if '.pdf' in fle:
             if '.md' in fle:

                searchstring = fle 
                find_word_walk(top_dir)


sys.stdout.close()
