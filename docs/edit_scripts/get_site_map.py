import os, sys


def get_file(link, md_file_list):
	for fle in md_file_list:
		if link in fle:
			return fle

def print_links(root_fle, depth, max_depth, out_fle, md_file_list, already_linked):
	if depth < max_depth:
		link_list = []
		indent = ''
		infile = open(root_fle, 'r')
		data = infile.read()
		for fle in [i.split('/')[-1] for i in md_file_list]:
			if fle in data:
				link_list.append(fle)
		for i in range(depth-1):
			for j in range(10):
				indent += '&nbsp '

		link_list = list(set(link_list) - set(already_linked))
		for link in link_list:
			link_fle = get_file(link, md_file_list)
			rel_link = link_fle.split('LaGriT/docs')[-1]
			out_fle.write(indent + '[' + link[:-3] + ']' + '(' + rel_link + ')' + ' \n')
			already_linked.append(link)
			print_links(link_fle, depth+1, max_depth, out_fle, md_file_list, already_linked)

dr = '/Users/nknapp/Desktop/LaGriT/docs/pages'
md_file_list = []

for root, drs, fles in os.walk(dr):
	for fle in fles:
		if '.md' in fle:
			md_file_list.append(os.path.join(root, fle))

max_depth = 4
out_fle_name = '/Users/nknapp/Desktop/site_map.md'
start_page = '/Users/nknapp/Desktop/LaGriT/docs/index.md'
out_fle = open(out_fle_name, 'w')

already_linked = []
print_links(start_page, 0, max_depth, out_fle, md_file_list, already_linked)





