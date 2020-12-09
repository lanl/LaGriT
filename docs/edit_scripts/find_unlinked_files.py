# Search for unlinked .md files in all files
# USAGE: python find_unlinked_files.py
# for every file .md search all files for use of that .md or .html
# If none found, report unlinked file

# TODO:
# This will not catch duplicate file.md if file.md is used somewhere 
# This is just checking .md in .md files, consider other file types

# MUST SET top_dir
# MUST SET file to open and write results

import os, sys



# routine walk through all the directories, search each file
# report .md files not found in any .md file

def find_word_walk(top_dir,searchstring):
 	
    count = 0

    for root, drs, fles in os.walk(top_dir):
        for fle in fles:

#           searchstring is file to look for
#           infile_name is the full path of searchstring
#           froot is the root name of file to look for (will find .md and .html)
#           infile is the file to look into

            if '.md' in fle:

                infile_name = os.path.join(root, fle)	
                with open(infile_name, 'r') as infile:

                    fstring = os.path.splitext(fle)
                    froot = fstring[0]
                    fhtml = froot+".html"

                    if searchstring in infile.read():
                        print 'found %s in file %s' % (searchstring, infile_name)
                        count = count + 1
                    if fhtml in infile.read():
                        print 'found %s in file %s' % (fhtml, infile_name)
                        count = count + 1
                    infile.close()

    print '%d found files with %s: ' % (count, searchstring)
    return count

# main -----------------------------------------------

searchstring = "notset" 
top_dir = '/project/eesdev/tam/clone/LaGriT/docs/'

# write to file instead of stdout
sys.stdout=open("find_unlinked_files.out.txt","w")


# for each .md file find a link in another .md file
for root, drs, fles in os.walk(top_dir):
        for fle in fles:

#            if '.pdf' in fle:
             if '.md' in fle:

                searchstring = fle 
                print 'Search files for  %s: %s' % (searchstring, root)

                itotal = find_word_walk(top_dir,searchstring)

                print '%d Total files with %s: %s' % (itotal, searchstring, root)

sys.stdout.close()
