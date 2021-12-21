import os
import subprocess
from typing import Any
from collections.abc import Iterable
from .logger import log

# $ lagrit < infile > outfile


def run_lagrit(lagrit_exe: str, working_directory: str, infile: str = "input.lgi", flags: str = None):
    """Calls the LaGriT executable.

    Args:
        lagrit_exe (str): Path to the LaGriT executable binary.
        flags (Any[str, Iterable], optional): Optional flags to pass to LaGriT. Defaults to None.

    """
    # Verify the path to LaGriT exists and is a file
    if not os.path.isfile(lagrit_exe):
        raise FileNotFoundError(
            "Invalid path - LaGriT does not exist at %s" % lagrit_exe
        )

    # Check test directory exists
    if not os.path.isdir(working_directory):
        raise FileNotFoundError("Invalid path - no directory exists: %s" % working_directory)

    # Check the infile exists in the desired working directory
    if not os.path.isabs(infile):
        if not os.path.isfile(os.path.join(working_directory, infile)):
            raise FileNotFoundError("Invalid path - %s does not exist" % (os.path.join(working_directory, infile)))

    # Make sure flags are in a list format
    if isinstance(flags, str):
        flags = flags.split(' ')
    if not isinstance(flags, Iterable):
        flags = []

    stdin = None
    with open(os.path.join(working_directory, infile), 'r') as f:
        stdin = f.read()

    log.info("Running test case at %s" % working_directory)
    log.info(" ".join([lagrit_exe, *flags]))

    # Finally, call LaGriT
    stdout, stderr = subprocess.Popen(
        [lagrit_exe, *flags],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        cwd=working_directory,
    ).communicate(stdin.encode('utf-8'))

    #log.info("stdout = " + stdout.decode())
    #log.info("stderr = " + stderr.decode())

    return (stdout.decode(), stderr.decode())
