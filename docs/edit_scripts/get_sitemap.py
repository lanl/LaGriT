import os, sys

def get_md_links(dr):
	lst = []
	for fle in os.listdir(dr):
		if os.path.isfile(os.path.join(dr, fle)):
			if '.md' in fle:
				infile = open(os.path.join(dr, fle), 'r')
				data = infile.read()
				
def write_link(location, fle):
	string = '<a href="https://lanl.github.io/LaGriT'
	string += location + '"> ' 
	number_of_tabs = location.count("/")
	for i in range(number_of_tabs):
		string += "     "  
	string +=  location + " </a>"
	string += " <br> \n"
	fle.write(string)
	print string

def print_links(dr):
	for fle in os.listdir(dr):
		if os.path.isfile(os.path.join(dr, fle)):
			infile = open(os.path.join(dr, fle), 'r')
			data = infile.read()
					

#'"<p style="margin-left: 40px">This text is indented.</p>'"

# get names of all markdown files

md_dir = '/Users/nknapp/Desktop/LaGriT/docs/pages/'
md_file_list = []

for root, drs, fles in os.walk(md_dir):
   for fle in fles:
        if '.md' in fle:
            md_file_list.append(os.path.join(root, fle))

site_map_file_name = '/Users/nknapp/Desktop/LaGriT/docs/pages/lagrit_site_map.html'
map_fle = open(site_map_file_name, 'w')

for fle in md_file_list:
	location = fle.split('LaGriT/docs')[-1][:-3]
	write_link(location, map_fle)

print len(md_file_list), ' TOTAL PAGES'
	





