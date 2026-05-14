import os, sys

dr = '/Users/NKnapp/Desktop/deknapp.github.io/pylagrit/original/_sources/'
output_dr = '/Users/NKnapp/Desktop/deknapp.github.io/pylagrit/on_website/'
for fle in os.listdir(dr):
	full_name = os.path.join(dr, fle)
	cmd = 'pandoc ' + full_name + ' -f rst -t markdown -s -o ' + output_dr + fle[:-4] + '.md'
	os.system(cmd)


