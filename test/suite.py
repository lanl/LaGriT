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
sys.path.append('./level01')
from control01 import *
sys.path.append('./level02')
from control02 import *


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

	# xlagrit = "Y:/yanki/lagrit/src/mylagrit"
	xlagrit = "Y:/yanki/lagrit-mingw/lagrit_pnnl_win64/lagrit_pnnl_win64"
	# xlagrit = "/scratch/sft/yanki/lagrit/src/mylagrit"

	if argv is None:
			 argv = sys.argv

	parser = argparse.ArgumentParser(description = "Perform LaGrit Tests", prog = "./suite.py", )
	parser.add_argument("-f", "--full", help = "Runs the entire test suite, cancels other options", action = "store_true")
	parser.add_argument("-l", "--level", help = "Designate level of testing", action = "store", type = int, nargs = 1, default = 0)
	parser.add_argument("-cl", "--clean", help = "Clean directories of previous test results; cleans default output file unless testfile is specified", action = "store_true")
	parser.add_argument("-t", "--test", help = "Runs tests on directories; option for out file tag - stdout_[testfile].txt", action = "store_true")
	parser.add_argument("testfile", help = "Name <tag> for the test's out file stdout_<tag>.txt; default - <tag> = os name", 
											nargs = "?", default = sys.platform)
	parser.add_argument("-c", "--check", help = "Checks output files of tests; option for specific directory name [checkdir]", action = "store_true")
	parser.add_argument("checkdir", help = "Target dir for check function; default - recurse through current dir", action = readable_dir, 
											default = os.curdir, nargs = "?")
	parser.add_argument("-exe", "--executable", help = "Path to executable for testing", action = "store", type = str, default = xlagrit)
	args = parser.parse_args()

	if not (args.full or args.clean or args.test or args.check):
		args = parser.parse_args("--help".split())
		sys.exit(2)
	
	if args.full == True and  1 in args.level:
		os.chdir('level01')
		OneFull(args)
	elif args.full == True and 2 in args.level:
		os.chdir('level02')
		TwoFull(args)
	elif args.full == True and 0 in args.level:
		os.chdir('level01')
		OneFull(args)
		os.chdir('level02')
		TwoFull(args)
	else:
		if 1 in args.level:
			os.chdir('level01')
			ExecSuiteOne(args)
		elif 2 in args.level:
			os.chdir('level02')
			ExecSuiteTwo(args)
		else:
			args = parser.parse_args("--help".split())
	sys.exit(2)
			
# end Main 
#------------------------------------------------------------------------------


if __name__ == "__main__":
	sys.exit(main())
