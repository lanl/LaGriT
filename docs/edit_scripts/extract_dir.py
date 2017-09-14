import os, sys

dr = '/Users/nknapp/Desktop/LaGriT/docs/assets/themes/'
dest = '/Users/nknapp/Desktop/LaGriT/docs/_layouts/'

for root, drs, fles in os.walk(dr):
	for fle in fles:
			os.system('cp ' + os.path.join(root, fle) + ' ' + dest)
