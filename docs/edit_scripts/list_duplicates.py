import os, sys

# get names of all markdown files


md_dir = '/Users/nknapp/Desktop/LaGriT/docs/pages/'
md_file_list = []

for root, drs, fles in os.walk(md_dir):
    for fle in fles:
        if '.md' in fle:
            md_file_list.append(os.path.join(root,fle))


home_key = '/Users/nknapp/Desktop/LaGrit/docs/index.md'

md_file_list.insert(0, home_key)

short_md_file_list = [i.split('LaGriT/docs')[-1] for i in md_file_list]

print len(short_md_file_list)
print len(list(set(short_md_file_list)))
exit()

for fle in short_md_file_list:
    counter = 0
    lst = []    
    for long_fle in md_file_list:
        if fle in long_fle:
            counter += 1
            lst.append(long_fle)
    if counter > 1:
        print 'DUPLICATES of ', fle
        print '---------------------'
        for item in lst:
            print item


        
