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
        for i in range(depth):
            for j in range(10):
                indent += '&nbsp; '

        link_list = list(set(link_list) - set(already_linked))
        for link in sorted(link_list):
            link_fle = get_file(link, md_file_list)
            rel_link = 'LaGriT/' + link_fle.split('LaGriT/docs/')[-1][:-3]
            out_fle.write(indent + '[' + link[:-3] + ']' + '(' + rel_link + ')' + '\\' + '\n')
            already_linked.append(link)
            if 'release' not in link_fle:
                print_links(link_fle, depth+1, max_depth, out_fle, md_file_list, already_linked)

dr = '/Users/nknapp/Desktop/LaGriT/docs/pages'
md_file_list = []

for root, drs, fles in os.walk(dr):
    for fle in fles:
        if '.md' in fle:
            md_file_list.append(os.path.join(root, fle))

max_depth = 4
out_fle_name = '/Users/nknapp/Desktop/LaGrit/docs/md_site_map.md'
start_page = '/Users/nknapp/Desktop/LaGriT/docs/index.md'
out_fle = open(out_fle_name, 'w')
out_fle.write('[Home](index.md)' + '\\' +  '\n')
already_linked = []

print_links(start_page, 0, max_depth, out_fle, md_file_list, already_linked)

convert = 'pandoc ' + out_fle_name + ' -f markdown -t html -s -o /Users/nknapp/Desktop/LaGriT/docs/site_map.html'
os.system(convert)


out_fle_name = '/Users/nknapp/Desktop/LaGrit/docs/md_site_list.md'
out = open(out_fle_name, 'w')
for fle in sorted(md_file_list):
    rel_link = fle.split('LaGriT/docs/')[-1][:-3]
    out.write('[' + fle.split('/')[-1] + '](' + rel_link + ')' + '\\' + '\n') 

convert = 'pandoc ' + out_fle_name + ' -f markdown -t html -s -o /Users/nknapp/Desktop/LaGriT/docs/site_list.html'
os.system(convert)