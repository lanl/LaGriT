
# Loop through all source files and find images that are missing minimum formatting 

import os, sys

def filter_file(infile_name):
	with open(infile_name, 'r') as infile:
		for line in infile.readlines():
				if 'assets' in line and 'docs' in line:
					print infile_name
					break
				if 'img' in line and not ('<' in line and '>' in line): 
					print infile_name
					break
	
def list_in_dr(dr):
	print 'broken images in these files'
	for root, drs, fles in os.walk(dr):
		for fle in fles:
			if '.md' in fle:
				filter_file(os.path.join(root, fle))

dr = '/project/eesdev/tam/clone/LaGriT/docs/pages/'

list_in_dr(dr)


