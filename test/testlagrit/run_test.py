import os
from .run_lagrit import run_lagrit
from .diff import diff
from .logger import log

def run_test(lagrit_exe: str, test_dir: str, logfile: str = "logx3dgen", outfile: str = "outx3dgen"):
    log.info("Running %s" % test_dir)
    flags="-log %s -out %s" % (logfile, outfile)
    stdout, stderr = run_lagrit(lagrit_exe, test_dir, flags=flags)

    test_output = os.path.join(test_dir, outfile)
    reference_output = os.path.join(test_dir, 'reference', 'outx3dgen')

    return diff(test_output, reference_output)