import os, sys

# get names of all markdown files

def get_long_name(fle, md_file_list):
	for i in md_file_list:
		if fle in i:
			return i


def recursive_print(link_dict, key, indent, out, parent_list):
	if key in link_dict and key not in parent_list:
		parent_list.append(key)
		indent += "    "
		for link in link_dict[key]:
			out.write(indent + link)
			print indent + link
			recursive_print(link_dict, link, indent, out, parent_list)
	
md_dir = '/Users/nknapp/Desktop/LaGriT/docs/pages/'
md_file_list = []

for root, drs, fles in os.walk(md_dir):
    for fle in fles:
        if '.md' in fle:
            md_file_list.append(os.path.join(root,fle))

home_key = '/Users/nknapp/Desktop/LaGrit/docs/index.md'

md_file_list.insert(0, home_key)

short_md_file_list = [i.split('/')[-1] for i in md_file_list]

link_dict = {}
for fle in md_file_list:
	try:
		data = open(fle, 'r').read()
	except:
		print fle, ' does not exist'
		exit()
	link_list = []
	for short in short_md_file_list:
		if short in data:
			link_list.append(get_long_name(short, md_file_list))
	if len(link_list) > 0:
		link_dict[fle] = link_list

output_file = '/Users/nknapp/Desktop/LaGriT/docs/website_map.txt'
out = open(output_file, 'w')

parent_list = []
recursive_print(link_dict, home_key, '', out, parent_list)


		
