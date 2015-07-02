#! /n/local_linux/epd/bin/python2.7
#
# /usr/local/bin/python -> python3.2  
# NOTE: this uses python before vers 3
#        newer versions use print as function not statement
#
# for vers 3 lanl machines /usr/bin/env python
# for sgi /usr/lanl/bin/python
#
#------------------------------------------------------------------------------
#  Name: suite.py
#  Last Modified: Jun 2015 by Mikita Yankouski yanki@lanl.gov 
#
#  Function: Controls methods from other scripts and 
#            allows the user to choose the depth of tests performed.
#
#  Sample command: ./suite.py -h
#------------------------------------------------------------------------------

import os, sys, argparse
from check_test import *
from run_test import *


class readable_dir(argparse.Action):
	def __call__(self,parser, namespace, values, option_string=None):
		try:
			prospective_dir = values
			if not os.path.isdir(prospective_dir):
					msg = "\nInvalid Directory: {0} is not a valid path\n".format(prospective_dir)
					raise argparse.ArgumentTypeError()
			if os.access(prospective_dir, os.R_OK):
					setattr(namespace,self.dest,prospective_dir)
			else:
					msg = "\nInvalid Directory: {0} is not a readable dir\n".format(prospective_dir)
					raise argparse.ArgumentTypeError()
		except argparse.ArgumentTypeError:
			print >>sys.stderr, msg
			print "/".join(os.listdir(os.curdir))
			sys.exit(2)


##############################################################################
# MAIN begin
#
# call lagrit with driver
# starting programs from the command line 
# os.system(cmd) or fo1 = os.popen(cmd) fo1.close()
#
#------------------------------------------------------------------------------


def main(argv=None):

	if argv is None:
			 argv = sys.argv

	parser = argparse.ArgumentParser(description="Perform LaGrit Tests")
	commands = parser.add_mutually_exclusive_group()
	commands.add_argument("-f", "--full", help = "Runs the entire test suite", action = "store_true")
	funcs = parser.add_argument_group()
	funcs.add_argument("-cl", "--clean", help = "Clean directories of previous test results", action = "store_true")
	funcs.add_argument("-t", "--test", help = "Runs tests on directories", action = "store_true")
	funcs.add_argument("-c", "--check", help = "Checks tests by their output files", action = "store_true")
	commands.add_argument_group(funcs)
	parser.add_argument("directory", help = "Name of directory to check", action = readable_dir, 
											default = os.curdir, nargs = "?")
	args = parser.parse_args()
	if args.full == True:
		Clean()
		RunTest()
		Check(target=args.directory)
	else:
		if args.clean == True:
			Clean()
		if args.test == True:
			RunTest()
		if args.check == True:
			Check(target=args.directory)
	sys.exit(2)
			
# end Main 
#------------------------------------------------------------------------------


def Clean():
	
	dirs = os.listdir(os.curdir)
	
	for directory in dirs:
		try:
			if os.path.isdir(directory):
				if directory == "result_files":
					print("Skipping directory: %s\n" % directory)
				else:
					print("Directory: %s" % directory)
					for the_file in os.listdir(directory):
						# print("\t%s" % the_file)
						file_path = os.path.join(directory, the_file)
						try:
							if os.path.isfile(file_path) and (the_file.startswith("out") or the_file.endswith("gmvF") or the_file.endswith("x3dgen")):
								os.remove(file_path)
						except Exception, e:
							print e
					print("\tClean.\n")
		except Exception, e:
			print e
	print("Done. All output files removed from directories.\n")


if __name__ == "__main__":
	sys.exit(main())
