import os
import argparse
import lg_test_lib.clean_test as cln
import lg_test_lib.run_test as run
import lg_test_lib.check_test as chk

# Set module variables
dtop = None
lagrit_exe = None
all_levels = []
checkdir = None
testfile = None
flags = None

class readable_dir(argparse.Action):
    '''
    Throws an exception if a directory is non-existant.
    Otherwise, passes silently.
    '''
    def __call__(self,parser,namespace,values,option_string=None):
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

def TestSingle(testcase:str,clean:bool=True,test:bool=True,check:bool=True):
    '''
    Runs a single test case.
    '''
    os.chdir(os.path.join(dtop,testcase))
    print(os.getcwd())

    if clean:
        cln.CleanSingleDir(os.getcwd())

    os.chdir('..')

    if test:
        run.RunTest(tag=testfile,
                    executable=lagrit_exe,
                    flags=flags,
                    test_dir=testcase.split('/')[-1])

    if check:
        chk.Check(target=checkdir,
                  test_dir=testcase.split('/')[-1])
    
    os.chdir(os.path.join(dtop,testcase))


def TestDir(testing_dir:str,clean:bool=True,test:bool=True,check:bool=True):
    '''
    Runs a test suite on a given directory.

    Args:
       dir (str): directory to test (i.e., 'level01')

    Kwargs:
       clean (bool): cleans the directory before test
       test (bool): runs a test
       check (bool): verifies test accuracy
    '''

    os.chdir(os.path.join(dtop,testing_dir))

    if clean:
        cln.Clean(tag=testfile)

    if test:
        run.RunTest(tag=testfile,executable=lagrit_exe,flags=flags)

    if check:
        chk.Check(target=checkdir)
