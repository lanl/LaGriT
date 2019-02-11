import os, sys
from check_test import *
from run_test import *


__all__ = ['ExecSuiteTwo', 'TwoFull', 'CheckTwo', 'RunTestTwo']


def ExecSuiteTwo(args):
	if args.clean == True:
		Clean(tag = args.testfile)
	if args.test == True:
		RunTest(tag = args.testfile, executable = args.executable)
	if args.check == True:
		Check(target = args.checkdir)
	if not (args.clean or args.test or args.check):
		print("\nTest error (level02):")
		print("   [-l, --level] must be run with additional flags to specify exact behavior")
		print("   Please run again with [-cl, --clean], [-t, --test], and/or [-c, --check]\n")

			
def TwoFull(args):
	Clean(tag = args.testfile)
	RunTest(tag = args.testfile, executable = args.executable, flags = args.flags)
	Check(target = args.checkdir)


def CheckTwo(args):
	Check(target = args.checkdir)


def RunTestTwo(args):
	RunTest(tag = args.testfile, executable = args.executable, flags = args.flags)


def Clean(**args):

	dirs = os.listdir(os.curdir)
	tag = args['tag']
	
	for directory in dirs:
		try:
			if os.path.isdir(directory):
				if directory == "test_results":
					print("Skipping directory: %s\n" % directory)
				else:
					print("Directory: %s" % directory)
					for the_file in os.listdir(directory):
						file_path = os.path.join(directory, the_file)
						try:
							if os.path.isfile(file_path) and (the_file.startswith("out") or the_file.endswith("gmvF") 
								or the_file.startswith("tmp_") or the_file.endswith("x3dgen")):
								os.remove(file_path)
						except Exception as e:
							print(e)
					print("\tClean.")
			elif os.path.isfile(directory) and not directory.endswith(".old.txt"):
				if directory.startswith("stdout_" + tag):
					os.rename(directory, directory[:-4] + ".old" + directory[-4:])
				elif directory.startswith("diffout_" + tag):
					os.rename(directory, directory[:-4] + ".old" + directory[-4:])
		except Exception as e:
			print(e)
	print("Done. All output files removed from directories.\n")

