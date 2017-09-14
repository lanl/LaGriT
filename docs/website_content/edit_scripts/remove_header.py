# Removes old lanl.gov header code from markdown file

import os, sys
import shutil

dr = './pages_bad'

def ig_f(dir, files):
    return [f for f in files if os.path.isfile(os.path.join(dir, f))]
    

def rewrite_file(name, strings_to_remove):
    old_fle = open(name, 'r')
    new_fle_start = '/home/nknapp/deknapp.github.io/pages/'  
    new_file_name = new_fle_start + name[12:]
    print new_file_name
    sys.stdout.flush()
    new_file = open(new_file_name, 'w')
    for line in old_fle:
        if not any(string in line for string in strings_to_remove):
            new_file.write(line)

strings_to_remove = ['![](images/lagrit1.jpg){width="420" height="120"}', '![](images/lagrit2.jpg){width="180" height="120"}]',
                     '[]{#content}']

shutil.copytree('/home/nknapp/deknapp.github.io/pages_bad/', '/home/nknapp/deknapp.github.io/pages/', ignore=ig_f)
for root, drs, fles in os.walk(dr):
    for fle in fles:
        if fle[-3:] == '.md':
            rewrite_file(os.path.join(root, fle), strings_to_remove)
