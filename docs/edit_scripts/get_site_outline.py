import os, sys

# get names of all markdown files

def html_link(md_name, spaces):
	string = '<a href="https://lanl.github.io/LaGriT/'
	location = md_name.split('LaGriT/docs/')[-1][:-3]
	string += location + '" > ' + spaces + location + ' </a>'
	return string

def get_long_name(fle, md_file_list):
	for i in md_file_list:
		if fle in i:
			return i


def recursive_print(link_dict, key, indent, out_md, out_html, parent_list, counter):
	if key in link_dict and key not in parent_list:
		parent_list.append(key)
		indent += "    "
		counter += 1
		spaces = ''
		for i in 3*range(counter):
			spaces += '&emsp; '	
		header = "h" + str(counter)
		if counter > 6:
			return 
		for link in list(set(link_dict[key])):
			md_string = indent + './' + link.split('LaGrit/docs/')[-1] + '\n' 
			out_md.write(md_string)
			html_string = indent + "<" + header + ">" + html_link(link, spaces) + " </" + header + ">  \n" 
			print html_string
			out_html.write(html_string)
			recursive_print(link_dict, link, indent, out_md, out_html, parent_list, counter)
	
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

md_output_file = '/Users/nknapp/Desktop/LaGriT/docs/md_website_map.txt'
html_output_file = '/Users/nknapp/Desktop/LaGriT/docs/html_website_map.html'
out_md = open(md_output_file, 'w')
out_html = open(html_output_file, 'w')
parent_list = []
counter = 0
recursive_print(link_dict, home_key, '', out_md, out_html, parent_list, counter)


		
