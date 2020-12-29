#------------------------------------------------------------------------------
#  Name: suite.py
#  Last Modified: Jun 2015 by Mikita Yankouski yanki@lanl.gov 
#
#  Function: Controls methods from other scripts and 
#            allows the user to choose the depth of tests performed.
#
#  Sample command: ./suite.py -h
#------------------------------------------------------------------------------

import os
import sys
import argparse

sys.path.append('.')
import lg_test_lib as lg_test

try:
    from pathlib import Path
    def fix_path(cwd, exe_path):
        if Path(exe_path).is_absolute():
            return str(Path(exe_path))

        return str(Path(cwd).joinpath(exe_path))

except ModuleNotFoundError:
    def fix_path(cwd, exe_path):
        if os.path.isabs(exe_path):
            return exe_path

        return os.path.join(cwd, exe_path)

##############################################################################
# MAIN begin
#
# call lagrit with driver
# starting programs from the command line 
# os.system(cmd) or fo1 = os.popen(cmd) fo1.close()
#
#------------------------------------------------------------------------------

def main(argv=None):

    test_dir_root = 'level0'
    xlagrit = os.path.join(os.getcwd(),'..','src','lagrit')
    lg_test.dtop = os.getcwd()
    lg_test.all_levels = [d for d in os.listdir('.') if 'level' in d]

    if argv is None:
        argv = sys.argv

    parser = argparse.ArgumentParser(description = "Perform LaGrit Tests", prog = "./suite.py", )
    parser.add_argument("-f", "--full", help = "Runs the entire test suite, cancels other options", action = "store_true", default = False)
    parser.add_argument("-l", "--level", help = "Designate level of testing", action = "store", type = int, nargs = 1, default = 0)
    parser.add_argument("-s", "--single", help = "Specify by name a single test to run", action = "store", type = str, default = '')
    parser.add_argument("-cl", "--clean", help = "Clean directories of previous test results; cleans default output file unless testfile is specified", action = "store_true")
    parser.add_argument("-t", "--test", help = "Runs tests on directories; option for out file tag - stdout_[testfile].txt", action = "store_true")
    parser.add_argument("testfile", help = "Name <tag> for the test's out file stdout_<tag>.txt; default - <tag> = os name", nargs = "?", default = sys.platform)
    parser.add_argument("-c", "--check", help = "Checks output files of tests; option for specific directory name [checkdir]", action = "store_true")
    parser.add_argument("checkdir", help = "Target dir for check function; default - recurse through current dir", action = lg_test.readable_dir, default = os.curdir, nargs = "?")
    parser.add_argument("-exe", "--executable", help = "Path to executable for testing", action = "store", type = str, default = xlagrit)
    parser.add_argument("-fl", "--flags", help = "Command line flags to pass to LaGriT on run", action = "store", type = str, default = "-log logx3dgen -out outx3dgen")
    parser.add_argument("-hf", "--hard_fail", help = "Quits and returns non-zero exit code on failed test", action = "store", type = int, nargs = 1, default = 0)
    args = parser.parse_args()

    # If no valid options, raise help screen
    if not (args.level or args.full or args.clean or args.test or args.check or args.single):
        args = parser.parse_args("--help".split())
        sys.exit(2)

    # Pass arguments to module-level variables
    lg_test.testfile = args.testfile
    lg_test.lagrit_exe = fix_path(os.getcwd(), args.executable)
    lg_test.flags = args.flags
    lg_test.checkdir = args.checkdir

    if args.full:

        if args.level:
            # Run full on defined level
            lg_test.TestDir(test_dir_root+str(args.level[0]))
        else:
            # Run full on all levels
            for level in lg_test.all_levels:
                print('Running full tests on %s' % level)
                lg_test.TestDir(level)

    elif args.single:
        # Run a single test directory
        lg_test.TestSingle(args.single)

    else:

        if args.level:
            testable_levels = [test_dir_root+str(args.level[0])]
        else:
            testable_levels = lg_test.all_levels

        # Run only user defined clean/check/test on levels
        for level in testable_levels:

            params = ['clean'*args.clean,
                      'test'*args.test,
                      'check'*args.check]

            params = ', '.join([x for x in params if x != ''])
            print('Running %s on %s' % (params,level))
            lg_test.TestDir(level,
                            clean=args.clean,
                            test=args.test,
                            check=args.checkdir)

            
# end Main 
#------------------------------------------------------------------------------

if __name__ == "__main__":
    sys.exit(main())
