import os, sys

# get names of all markdown files

md_dir = '/home/nknapp/deknapp.github.io/'
md_file_list = []

for root, drs, fles in os.walk(md_dir):
    for fle in fles:
        if '.md' in fle:
            md_file_list.append(fle)

# check that names are linked somewhere in the text of all .md files

data = 'start of all file lines '
for root, drs, fles in os.walk(md_dir):
    for fle in fles:
        if '.md' in fle:
            data += open(os.path.join(root, fle), 'r').read()

no_links = []
for fle in md_file_list:
    if fle not in data:
        print fle, ' not linked to anywhere !'
        no_links.append(fle)


for root, drs, fles in os.walk(md_dir):
    for fle in fles:
        if '.md' in fle:
            for bad_fle in no_links:
                if bad_fle in fle:
                    os.system('mv ' + os.path.join(root, fle) + ' /home/nknapp/deknapp.github.io/not_on_website/another_' + fle)
                    break
