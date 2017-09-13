import os, sys

def filter_file(infile_name, character):
    outfile_name = infile_name[:-3] + '_temp.md'
    with open(infile_name, 'r') as infile, open(outfile_name, 'w') as outfile:
	data = infile.read()
	data = data.replace(character, "")
	outfile.write(data)

infile_name = '/home/nknapp/deknapp.github.io/pages/manual.md'
filter_file(infile_name, "|")
