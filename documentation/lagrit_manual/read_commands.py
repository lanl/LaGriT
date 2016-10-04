#!/usr/bin/python

__author__ = 'lucia'

import os


def read_file_to_list(filename):
    """Opens files and returns the list of the lines
    :rtype : list
    :param filename: the name of the file to open
    :return:
    """
    f = open(filename, 'r')
    return f.read().splitlines()


def get_commands():
    """
    Returns list of commands
    :rtype : list
    """
    # file name command_list.txt
    return read_file_to_list("command_list.txt")


def get_pages():
    """Returns list of html pages
    :rtype : list
    """
    # file name page_list.txt
    return read_file_to_list("page_list.txt")


def get_dict(key, value):
    """Returns the dictionary with command:page.html
    :rtype : dict
    :param key: commands
    :param value: html page name
    :return: dictionary of the commands and names of the pages
    """
    return {k.lower(): v for (k, v) in zip(key, value)}


if __name__ == '__main__':
    # get commands and html pages that explain the commands.
    commands = get_commands()
    html = get_pages()
    # create dictionary command:page.html
    command_pages = get_dict(commands, html)
    # run a script to read a html page from the web and create a markdown file
    # sudo apt-get install html2markdown (on ubuntu)
    # command: html2markdown page.html >> page.txt
    # command pages are at http://lagrit.lanl.gov/docs/commands/
    # add the command name as the title to the file.
    for command in command_pages:
        page_path = "http://lagrit.lanl.gov/docs/commands/"
        page = page_path + command_pages[command].strip()
        command_file = "commands/" + command.strip(" ") + ".txt"
        cf = open(command_file, 'w')
        content = [".. _" + command + ":", ""]
        content = (l + "\n" for l in content)
        for l in content:
            cf.write(l)
        cf.close()
        cmd = "html2markdown " + page + " >> " + command_file
        os.system(cmd)

