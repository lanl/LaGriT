import os
import sys
import unittest

TEST_ROOT = os.path.abspath(__file__)


class DynamicClassBase(unittest.TestCase):
    longMessage = True


def make_test_function(description, a, b):
    def test(self):
        self.assertEqual(a, b, description)

    return test


def collect_tests(test_levels):
    """
    Returns all sub-folders within `test_levels`, where
    `test_levels` is a list containing one or more of "level01",
    "level02", etc.
    """
    test_dirs = []

    for level in test_levels:
        current_dir = os.path.join(TEST_ROOT, level)
        for item in os.listdir(current_dir):
            item = os.path.join(current_dir, item)
            if os.path.isdir(item):
                test_dirs.append(item)

    return test_dirs


if __name__ == "__main__":
    levels = ["level01", "level02"]
    test_dirs = collect_tests(levels)

    for test in test_dirs:
        run_test(test)

    testsmap = {"foo": [1, 1], "bar": [1, 2], "baz": [5, 5]}

    for (name, params) in testsmap.items():
        test_func = make_test_function(name, params[0], params[1])
        klassname = "Test_{0}".format(name)
        globals()[klassname] = type(
            klassname, (DynamicClassBase,), {"test_gen_{0}".format(name): test_func}
        )

    unittest.main()
