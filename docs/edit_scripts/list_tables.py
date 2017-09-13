
import os, sys

def filter_file(infile_name):
	with open(infile_name, 'r') as infile:
		for line in infile.readlines():
			if '|' in line and '---' in line: 
				print infile_name
				print '--------'			
				break
	
def list_in_dr(dr):
 	
	for root, drs, fles in os.walk(dr):
		for fle in fles:
			if '.md' in fle:
				filter_file(os.path.join(root, fle))

#dr = '/Users/NKnapp/Desktop/deknapp.github.io/'
dr = '/home/nknapp/deknapp.github.io/pages/'
list_in_dr(dr)


