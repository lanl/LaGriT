import os, sys

# get the full path of a markdown file
def get_file(link, md_file_list):
    for fle in md_file_list:
        if link in fle:
            return fle

# This function, called recursively, prints out the site map, with formatting based on tags in the file
def print_links(root_fle, depth, max_depth, out_fle, md_file_list, already_linked):
    if depth < max_depth:
        link_list = []i
        indent = ''
        infile = open(root_fle, 'r')
        data = infile.read()
        for fle in [i.split('/')[-1] for i in md_file_list]:
            if fle in data:
                link_list.append(fle)
        for i in range(depth):
            for j in range(10):
                indent += '&nbsp; '

        link_list = list(set(link_list) - set(already_linked))
        for link in sorted(link_list):
            link_fle = get_file(link, md_file_list)
            link_fle_in = open(link_fle, 'r').read()
            tags = '~~'
            if 'tags' and 'ok' in link_fle_in:
                tags = '**'
            elif 'tags' and 'review' in link_fle_in:
                tags = '*'
            rel_link = link_fle.split('LaGriT/docs/')[-1][:-3]
            out_fle.write(indent + '[' + tags + link[:-3] + tags + ']' + '(' + rel_link + ')' + '\n' + '\n')
            already_linked.append(link)
            if 'release' not in link_fle:
                print_links(link_fle, depth+1, max_depth, out_fle, md_file_list, already_linked)

# CHANGE THIS LINE TO LOCATION OF REPO:
repo_location = '/Users/nknapp/Desktop/LaGriT/docs/'

dr = repo_location + 'pages'
md_file_list = []

# Get a list of all the Markdown files in the repository
for root, drs, fles in os.walk(dr):
    for fle in fles:
        if '.md' in fle:
            md_file_list.append(os.path.join(root, fle))

max_depth = 4
out_fle_name = repo_location + 'site_map.md'
start_page = repo_location + 'index.md'
out_fle = open(out_fle_name, 'w')
out_fle.write('[**Home**](index.md)' + '\n' +  '\n')
already_linked = []

# print out the website map using a recursive function
print_links(start_page, 0, max_depth, out_fle, md_file_list, already_linked)

# list all files in the website; format based on tags in file
out_fle_name = repo_location + 'site_list.md'
out = open(out_fle_name, 'w')
for fle in sorted(md_file_list):
    link_fle_in = open(fle, 'r').read()
    tags = '~~'
    if 'tags' and 'ok' in link_fle_in:
        tags = '**'
    elif 'tags' and 'review' in link_fle_in:
        tags = '*'
    rel_link = fle.split('LaGriT/docs/')[-1][:-3]
    out.write('[' + tags + fle.split('/')[-1][:-3] + tags + ']' + '(' + rel_link + ')' + '\n' + '\n') 
