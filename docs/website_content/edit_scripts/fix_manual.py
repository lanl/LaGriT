import os, sys, re

def strip_whitespace(infile_name):
    outfile_name = infile_name[:-3] + '_temp.md'
    with open(infile_name, 'r') as infile, open(outfile_name, 'w') as outfile:
	data = infile.read()	
	data = ' '.join(data.split())	
	outfile.write(data)
    os.system('mv ' + outfile_name + ' ' + infile_name)

def filter_file(infile_name, no, yes):
    outfile_name = infile_name[:-3] + '_temp.md'
    with open(infile_name, 'r') as infile, open(outfile_name, 'w') as outfile:
	data = infile.read()
        data = data.replace(no, yes)
        outfile.write(data)
    os.system('mv ' + outfile_name + ' ' + infile_name)

dr = '/Users/NKnapp/Desktop/deknapp.github.io/pages/'
#no_list = ['[]{#content', '![](http://www.lanl.gov/images/tr', "\"495\"", "\"420\"", "\"1\"", "![](http://www.lanl.gov/images/xtr", "\"160\""]


os.system('cp ./pages/' + sys.argv[1] + '.md' + ' ./pages/' + sys.argv[1] + '_test.md')
infile_name  = dr + sys.argv[1] + '_test.md'
strip_whitespace(infile_name)
filter_file(infile_name,'\n', '  ')
filter_file(infile_name, '---', 'XXX')
before_list = ['###', '=============', '-', 'Keywords', 'description', 'title']
for x in before_list:
	filter_file(infile_name, x, ' \n ' + x + ' ')
filter_file(infile_name, 'XXX', '\n --- \n')

#for root, drs, fles in os.walk(dr):
#    for fle in fles:
#        if ".md" in fle and "temp" not in fle:
#            filter_file(os.path.join(root, fle), "\n", "")
#
#for root, drs, fles in os.walk(dr):
#    for fle in fles:
#        if ".md" in fle and "temp" not in fle:
#            os.remove(os.path.join(root, fle))
#
#for root, drs, fles in os.walk(dr):
#    for fle in fles:
#        if "_temp.md" in fle:
#            name = os.path.join(root, fle)
#            os.system('mv ' + name + ' ' + name.split('_temp')[0] + '.md')

