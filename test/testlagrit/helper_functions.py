import os
from typing import List


def collect_test_dirs(test_root: str, test_levels: List[int]) -> List[str]:
    """Returns a list of test directories.

    Args:
        test_root (str): Should be `{LAGRIT_ROOT}/test/`
        test_levels (List): A list containing one or more of [1, 2, 3]

    Returns:
        List[str]: A list of absolute paths to valid test directories.
    """
    all_test_dirs = []

    # Get absolute path of 'level01', 'level02', etc.
    # test_levels = [os.path.join(test_root, level) for level in test_levels]

    for level in test_levels:
        if isinstance(level, int):
            level = "level%02d" % level

        level_abspath = os.path.join(test_root, level)
        assert os.path.isdir(level_abspath), "Could not find level: %s" % level_abspath

        # Grabs and appends all subfolders in a 'level0X/' directory
        # Requires each subfolder to have a 'reference/' directory
        for test_dir in sorted(os.listdir(level_abspath)):

            if test_dir == "test_results":
                continue

            test_dir = os.path.join(level_abspath, test_dir)
            reference_dir = os.path.join(test_dir, "reference/")

            if os.path.isdir(test_dir) and os.path.isdir(reference_dir):
                all_test_dirs.append(test_dir)

    return all_test_dirs
