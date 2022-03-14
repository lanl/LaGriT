import os
import sys
from distutils.spawn import find_executable
from .logger import log

def find_lagrit(start_dir: str) -> str:
    """Attempts to find LaGriT.

    Args:
        start_dir (str): 'Hint' directory to search from.

    Returns:
        str: Path to LaGriT.
    """

    search_dirs = []
    exe_candidates = []

    if start_dir.lower().replace('/','').replace('\\','').endswith('test'):
        for p in ["..", "../src", "../build", "../install", "../build/src"]:
            search_dirs.append(os.path.join(start_dir, p))
    
    for p in search_dirs:
        for exe_name in ["lagrit", "lagrit.exe"]:
            candidate = find_executable(exe_name, path=p)
            if candidate is not None:
                exe_candidates.append(candidate)
    
    for exe_name in ["lagrit", "lagrit.exe"]:
        candidate = find_executable(exe_name, path=p)

        if candidate is not None:
            exe_candidates.append(candidate)
    
    log.debug("Found LaGriT at: %s" % str(exe_candidates))

    if len(exe_candidates):
        log.info("Choosing LaGriT at %s" % exe_candidates[0])
        return exe_candidates[0]
    
    raise FileNotFoundError("Could not find LaGriT!")
