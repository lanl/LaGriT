import os, sys, re

def filter_file(infile_name):
    outfile_name = infile_name[:-3] + '_temp.md'
    with open(infile_name, 'r') as infile, open(outfile_name, 'w') as outfile:
	data = infile.read()
        #for no in no_list:
        data = data.replace('smd', 'md')
        outfile.write(data)

dr = '/home/nknapp/deknapp.github.io/pages/'
#no_list = ['[]{#content', '![](http://www.lanl.gov/images/tr', "\"495\"", "\"420\"", "\"1\"", "![](http://www.lanl.gov/images/xtr", "\"160\""]


for root, drs, fles in os.walk(dr):
    for fle in fles:
        if ".md" in fle and "temp" not in fle:
            filter_file(os.path.join(root, fle))

for root, drs, fles in os.walk(dr):
    for fle in fles:
        if ".md" in fle and "temp" not in fle:
            os.remove(os.path.join(root, fle))

for root, drs, fles in os.walk(dr):
    for fle in fles:
        if "_temp.md" in fle:
            name = os.path.join(root, fle)
            os.system('mv ' + name + ' ' + name.split('_temp')[0] + '.md')

