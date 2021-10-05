import os
import sys
import subprocess
import difflib
import pytest
from tempfile import TemporaryDirectory
from typing import List

class LaGriT():
    def __init__(self, exe: str, infile: str, log_file: str = "lagrit.log", out_file: str = "lagrit.out"):
        self.exe = exe
        self.infile = infile
        self.log_file = log_file
        self.out_file = out_file

        self.args = ["-outfile", self.out_file, "-log", self.log_file]

        self._stdout = None
        self._lg_out = None
        self._lg_log = None

    
    def run(self) -> None:
        cmd = [self.exe, *self.args]

        with TemporaryDirectory() as tmp_dir:
            with open(self.infile, 'r') as stdin:
                self._stdout = subprocess.check_call(
                    cmd,
                    cwd=tmp_dir,
                    stdin=stdin,
                    stderr=subprocess.STDOUT,
                )

            with open(os.path.join(tmp_dir, self.out_file), "r") as f:
                self._lg_out = f.readlines()

            with open(os.path.join(tmp_dir, self.log_file), "r") as f:
                self._lg_log = f.readlines()

    def check_diff(self) -> List[bool, List[str]]:
        diff = difflib.unified_diff(
            self._lg_out,
            self.infile.readlines(),
            fromfile='hosts0',
            tofile='hosts1',
        )

        for line in diff:
            sys.stdout.write(line)