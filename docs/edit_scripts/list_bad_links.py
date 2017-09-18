
import os, sys

def filter_file(infile_name):
	with open(infile_name, 'r') as infile:
		string = ''
		for line in infile.readlines():
			if '['  in line and '.' in line:
				string += line
			if string != '':
					print infile_name
					print '--------'
					print string            
def list_in_dr(dr):
    
    for root, drs, fles in os.walk(dr):
        for fle in fles:
            if '.md' in fle:
                filter_file(os.path.join(root, fle))

dr = '/Users/nknapp/Desktop/LaGriT/docs/pages/'
list_in_dr(dr)
