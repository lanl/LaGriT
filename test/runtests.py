import os
import sys
import argparse
import unittest

# Get the directory of the current file
TEST_ROOT = os.path.dirname(os.path.abspath(__file__))
# Add the test root directory to the path
sys.path.append(TEST_ROOT)

# Import the test module as test_lg
import testlagrit as test_lg

# Define a placeholder test class
class LGTest(unittest.TestCase):
    pass

# Function to generate a unittest function for test case
def generate_unittest(lg_exe, test_dir):
    def test(self):
        message = "\n\nTest Failed at: " + test_dir +"\n\nPotential causes:\n\n-Difference between"+test_dir+"/outx3dgen and "+test_dir+"/reference/outx3dgen\n-Failure to install Exodus when running Level 2 tests"
        self.assertEqual(test_lg.run_test(lg_exe, test_dir), True, message)
    return test

# Main function to run tests and handle arguments
def main():
    # Information to clarify the help screen and set up command line parser
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,   
        description=('''
        LaGriT test suite
        ----------------------------

        Level 1 - Default Test Mode
        Level 2 - Requires Exodus'''),
        epilog=('''
        '''))


    # Define Arguments for command line
    parser.add_argument(
        "-l",
        "--levels",
        help="Designate level(s) of testing. Example: --levels 1 2",
        action="store",
        type=int,
        nargs='*',
        default=[1]
    )

    parser.add_argument(
        "-exe",
        "--executable",
        help="Full path to LaGriT executable for testing. Example: ~/LaGriT/build/lagrit",
        action="store",
        type=str,
        default=None
    )

    # Parse command line arguments
    args = parser.parse_args()
    
    # Ensure Level 3 is not automatically run
    if 3 in args.levels:
        print("Level 3 is for Developers only and should be run manually.")
        sys.exit(1)
    
    # Log the parsed arguments for debugging
    test_lg.log.debug("Called with args: " + str(args))

    # Get the LaGriT executable path from command-line arguments or find it
    lg_exe = args.executable
    if lg_exe is None:
        lg_exe = test_lg.find_lagrit(TEST_ROOT)

    # Collect test directories based on specified levels
    test_dirs = test_lg.collect_test_dirs(TEST_ROOT, args.levels)

    # Output to let user know current testing levels
    print("\nRunning tests in level(s): "+str(args.levels)[1:-1] )
    
    # Output to let user know that Exodus is not being tested
    if 2 not in args.levels:
        print("\n ** Exodus is not being tested. To test Exodus, please run Level 2. **")

    # Dynamically generate unittest functions for each test directory
    for test_dir in test_dirs:
        # Generate a unique test name based on the test directory
        test_name = 'test_%s' % os.path.basename(test_dir)
        # Create a unittest function for the test directory
        test = generate_unittest(lg_exe, test_dir)
        # Add the generated test function to the LGTest class
        setattr(LGTest, test_name, test)

    # Run the unittests with the specified command-line arguments
    unittest.main(argv=sys.argv[:1])

# Entry point for the script
if __name__ == '__main__':
    main()