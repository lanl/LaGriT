#!/usr/bin/env python3

import os
import sys
import argparse
import unittest

TEST_ROOT = os.path.dirname(os.path.abspath(__file__))
sys.path.append(TEST_ROOT)
import testlagrit as test_lg

def parse_args():
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

    parsed_args, unparsed_args = parser.parse_known_args()
    test_lg.log.debug("Called with args: " + str(parsed_args))

    # Done to avoid conflicts with unittest's args
    unparsed_args = [sys.argv[0], *unparsed_args]

    return parsed_args, unparsed_args

class LGTest(unittest.TestCase):
    # https://stackoverflow.com/a/32939/5150303
    pass

def generate_unittest(lg_exe, test_dir):
    def test(self):
        self.assertEqual(test_lg.run_test(lg_exe, test_dir), True)
    return test


if __name__ == '__main__':
    lagrit_args, unittest_args = parse_args()

    lg_exe = lagrit_args.executable
    lg_levels = lagrit_args.levels

    if lg_exe is None:
        lg_exe = test_lg.find_lagrit(TEST_ROOT)
    
    if lg_levels is None:
        lg_levels = [1,2]
    
    test_dirs = test_lg.collect_test_dirs(TEST_ROOT, lg_levels)

    for test_dir in test_dirs:
        test_name = 'test_%s' % os.path.basename(test_dir)
        test = generate_unittest(lg_exe, test_dir)
        setattr(LGTest, test_name, test)

    unittest.main(argv=[sys.argv[0]])
