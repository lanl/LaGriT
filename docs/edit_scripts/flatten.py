import os, sys

# get names of all markdown files

dr = '/Users/nknapp/Desktop/LaGriT/docs//home/nknapp/deknapp.github.io/pages/docs/demos/'

for root, drs, fles in os.walk(dr):
    for fle in fles:
        os.system('mv ' + os.path.join(root, fle) + ' ' + dr)

for root, drs, fles in os.walk(dr):
    for dr in drs:
        try:
            os.rmdir(dr)
        except:
            pass
