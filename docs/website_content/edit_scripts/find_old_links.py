
import os, sys

def filter_file(infile_name):
	with open(infile_name, 'r') as infile:
		for line in infile.readlines():
			if 'deknapp' in line:
				print infile_name
				print line
                                print '-------------'
                                break
	
def list_in_dr(dr):
 	
	print 'broken images in these files'
	for root, drs, fles in os.walk(dr):
		for fle in fles:
			if '.md' in fle:
				filter_file(os.path.join(root, fle))

dr = '/home/nknapp/LaGriT/docs/'
list_in_dr(dr)


