# Removes old lanl.gov header code from markdown file

import os, sys

dr = './pages'

for root, drs, fles in os.walk(dr):
    for fle in fles:
        if 'temp2' in fle:
            os.system('rm ' + os.path.join(root, fle))
