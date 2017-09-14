
import os, sys

def filter_file(infile_name):
	with open(infile_name, 'r') as infile:
		for line in infile.readlines():
			if 'assets' in line and 'LaGriT' not in line:
				print line
				break
	
def list_in_dr(dr):
 	
	print 'broken images in these files'
	for root, drs, fles in os.walk(dr):
		for fle in fles:
			if '.md' in fle:
				filter_file(os.path.join(root, fle))

#dr = '/Users/NKnapp/Desktop/deknapp.github.io/'
dr = '/home/nknapp/LaGriT/docs/pages/'
list_in_dr(dr)


