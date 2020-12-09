# Search for unlinked .md files in all files
# USAGE: python find_duplicate_files.py
# for every file .md search all files for duplicates 

# This is just checking .md files, consider other file types

# MUST SET top_dir
# MUST SET file to open and write results

import os, sys, re



# routine walk through all the directories, search each file

def find_word_walk(top_dir,searchstring):
 	
    count = 0

    for root, drs, fles in os.walk(top_dir):
        for fle in fles:

#           searchstring is file name to look for
#           infile_name is the full path of searchstring
#           froot is the root name of file to look for (will find .md and .html)
#           fle is the file to check

            if '.md' in fle:

               infile_name = os.path.join(root, fle)	

               fstring = os.path.splitext(fle)
               froot = fstring[0]
               fhtml = froot+".html"

               result = re.match(searchstring,fle)
#              print 'check name %s current file: %s' % (searchstring, fle)

               if result :
                   count = count + 1
                   if count > 1 :
                       print 'match %s with file %s' % (searchstring, infile_name)


#    print '%d duplicate files for %s: ' % (count, searchstring)
    return count

# main -----------------------------------------------

searchstring = "notset" 
top_dir = '/project/eesdev/tam/clone/LaGriT/docs/'

# write to file instead of stdout
sys.stdout=open("find_duplicate_files.out.txt","w")


# for each .md file find a link in another .md file
for root, drs, fles in os.walk(top_dir):
        for fle in fles:

#            if '.pdf' in fle:
             if '.md' in fle:

                searchstring = fle 
#                print 'Search files for  %s: %s' % (searchstring, root)

                itotal = find_word_walk(top_dir,searchstring)

                if itotal > 1:
                    print '%d duplicates with %s: %s' % (itotal, searchstring, root)

sys.stdout.close()
