import os
import sys
import argparse
import unittest

TEST_ROOT = os.path.dirname(os.path.abspath(__file__))
sys.path.append(TEST_ROOT)

import testlagrit as test_lg

class LGTest(unittest.TestCase):
    # https://stackoverflow.com/a/32939/5150303
    pass

def generate_unittest(lg_exe, test_dir):
    def test(self):
        self.assertEqual(test_lg.run_test(lg_exe, test_dir), True)
    return test

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="LaGriT test suite"
    )

    parser.add_argument(
        "-l",
        "--levels",
        help="designate level(s) of testing. Example: --levels 1 2",
        action="store",
        type=int,
        nargs='*',
        default=[1],
    )

    parser.add_argument(
        "-exe",
        "--executable",
        help="path to LaGriT executable for testing",
        action="store",
        type=str,
        default=None,
    )

    args = parser.parse_args()
    test_lg.log.debug("Called with args: " + str(args))

    lg_exe = args.executable
    if lg_exe is None:
        lg_exe = test_lg.find_lagrit(TEST_ROOT)
    
    test_dirs = test_lg.collect_test_dirs(TEST_ROOT, args.levels)

    for test_dir in test_dirs:
        test_name = 'test_%s' % os.path.basename(test_dir)
        test = generate_unittest(lg_exe, test_dir)
        setattr(LGTest, test_name, test)

    unittest.main()
