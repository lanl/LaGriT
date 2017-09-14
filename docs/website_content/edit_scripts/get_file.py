import os, sys

dr = '/Users/NKnapp/Desktop/deknapp.github.io/'
#dest = '/Users/NKnapp/Desktop/deknapp.github.io/pages/docs/demos'

dest = dr + sys.argv[2]
keyword = sys.argv[1] 

for root, drs, fles in os.walk(dr):
	for fle in fles:
		if keyword in fle and 'html' not in fle and 'png' not in fle:
			print 'copying ', fle, ' to ', dest
			os.system('cp ' + os.path.join(root, fle) + ' ' + dest)
			exit()		
