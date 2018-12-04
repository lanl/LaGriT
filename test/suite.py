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
from control01 import Clean as CleanOne
#from check_test import Check as CheckOne
#from run_test import RunTest as RunTestOne

sys.path.append('./level02')
from control02 import *
from control02 import Clean as CleanTwo
#from check_test import Check as CheckTwo
#from run_test import RunTest as RunTestTwo


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
            
            print(sys.stderr, msg)
            print("/".join(os.listdir(os.curdir)))
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

    xlagrit = "/n/swdev/mesh_tools/lagrit/install-Ubuntu-16.04-x86_64-gcc5.4.0/bin/lagrit"

    dtop = os.getcwd() 

    if argv is None:
        argv = sys.argv

    parser = argparse.ArgumentParser(description = "Perform LaGrit Tests", prog = "./suite.py", )
    parser.add_argument("-f", "--full", help = "Runs the entire test suite, cancels other options", action = "store_true", default = False)
    parser.add_argument("-l", "--level", help = "Designate level of testing", action = "store", type = int, nargs = 1, default = 0)
    parser.add_argument("-cl", "--clean", help = "Clean directories of previous test results; cleans default output file unless testfile is specified", action = "store_true")
    parser.add_argument("-t", "--test", help = "Runs tests on directories; option for out file tag - stdout_[testfile].txt", action = "store_true")
    parser.add_argument("testfile", help = "Name <tag> for the test's out file stdout_<tag>.txt; default - <tag> = os name", 
                                            nargs = "?", default = sys.platform)
    parser.add_argument("-c", "--check", help = "Checks output files of tests; option for specific directory name [checkdir]", action = "store_true")
    parser.add_argument("checkdir", help = "Target dir for check function; default - recurse through current dir", action = readable_dir, 
                                            default = os.curdir, nargs = "?")
    parser.add_argument("-exe", "--executable", help = "Path to executable for testing", action = "store", type = str, default = xlagrit)
    parser.add_argument("-hf", "--hard_fail", help = "Quits and returns non-zero exit code on failed test", action = "store", type = int, nargs = 1, default = 0)
    args = parser.parse_args()

    if not (args.level or args.full or args.clean or args.test or args.check):
        args = parser.parse_args("--help".split())
        sys.exit(2)
    

    if args.full:
        if args.level:
            if args.level[0] == 1:
                os.chdir(dtop)
                os.chdir('level01')
                OneFull(args)

            elif args.level[0] == 2:
                os.chdir(dtop)
                os.chdir('level02')
                TwoFull(args)
        
        else:
            print("Running full tests on level01")
            os.chdir(dtop)                                                                                                                                                                  
            os.chdir('level01')                                                                                                                                                             
            OneFull(args)                                                                                                                                                                   
                                                                                                                                                                                                
            print("Running full tests on level02")
            os.chdir(dtop)                                                                                                                                                                  
            os.chdir('level02')                                                                                                                                                             
            TwoFull(args)  

    else:
        if args.level:
            if args.level[0] == 1:
                os.chdir(dtop)
                os.chdir('level01')
                ExecSuiteOne(args)

            elif args.level[0] == 2:
                os.chdir(dtop)
                os.chdir('level02')
                ExecSuiteTwo(args)

        else:
            if args.clean:
                print("Cleaning level01")
                os.chdir(dtop)
                os.chdir('level01')
                CleanOne(tag=args.testfile)

                print("Cleaning level02") 
                os.chdir(dtop) 
                os.chdir('level02')
                CleanTwo(tag=args.testfile) 

            if args.check:
                print("Isolated checking not implemented yet")
                #print("Checking level01")
                #os.chdir(dtop)
                #os.chdir('level01')
                #CheckOne(args)

                #print("Checking level02")
                #os.chdir(dtop)
                #os.chdir('level02')
                #CheckTwo(args)

            if args.test:
                print("Testing level01")
                os.chdir(dtop)
                os.chdir('level01')
                RunTestOne(args)
                            
                print("Testing level02")
                os.chdir(dtop)
                os.chdir('level02')
                RunTestTwo(args)

            
# end Main 
#------------------------------------------------------------------------------


if __name__ == "__main__":
    sys.exit(main())
