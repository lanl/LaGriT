
import os, sys


dr = '/home/nknapp/deknapp.github.io/pages/'

for root, drs, fles in os.walk(dr):
	for fle in fles:
            if 'html' in fle:
                old_fle_name = os.path.join(root, fle)
                new_fle_name = old_fle_name.replace('html', 'md')
                cmd = 'mv ' + old_fle_name + ' ' + new_fle_name
                print cmd
                os.system(cmd)

for root, drs, fles in os.walk(dr):
    for dr in drs:
        try:
            os.rmdir(dr)
        except:
            pass
        
