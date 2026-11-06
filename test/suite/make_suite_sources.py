#!/usr/bin/env python3
"""make_suite_sources.py COVERAGE_BUILD_DIR [SUITE ...]

Writes suite_sources.txt: for every suite in run_all.sh (or the suites
named), the source files under src/ that its run executes at least one
line of. select_changed.py reads it for run_all.sh CHANGED=...

It needs a LaGriT built with coverage counting, with gcc and gfortran
(gcov must match the compiler), for example:

  T=$(dirname $(command -v gfortran))
  cmake -S ../.. -B ../../build_cov -D CMAKE_BUILD_TYPE=Release \\
        -D CMAKE_Fortran_COMPILER=$T/gfortran -D CMAKE_C_COMPILER=$T/gcc \\
        -D CMAKE_CXX_COMPILER=$T/g++ -D CMAKE_Fortran_FLAGS=--coverage \\
        -D CMAKE_C_FLAGS=--coverage -D CMAKE_CXX_FLAGS=--coverage
  cmake --build ../../build_cov --parallel 4
  python3 make_suite_sources.py ../../build_cov

Each suite is run on its own (its counters cleared first), so this takes
about twice as long as run_all.sh with BIG=1 (test_connect_timing is run
at its quick size). Suites named on the command line are measured again;
the others keep their lines in the file. Redo it now and then, and after
adding a suite: a source file the map has never seen makes CHANGED run
everything. Python 3, standard library only.
"""
import datetime
import glob
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
TOP = os.path.dirname(os.path.dirname(HERE))


def suites_of_run_all():
    s = open(os.path.join(HERE, "run_all.sh")).read()
    blk = s.split('SUITES="', 1)[1].split('"', 1)[0]
    out = []
    for l in blk.split():
        w = l.split(":")
        out.append((w[0], w[1]))
    return out


def executed(build):
    """the src/ files with at least one line run, from the .gcda files"""
    files = set()
    by_dir = {}
    for g in glob.glob(os.path.join(build, "**", "*.gcda"), recursive=True):
        by_dir.setdefault(os.path.dirname(g), []).append(g)
    for d, gs in by_dir.items():
        for i in range(0, len(gs), 200):
            r = subprocess.run(["gcov", "-n", "-o", d] + gs[i:i + 200], cwd=build,
                               capture_output=True, text=True)
            cur = None
            for l in r.stdout.split("\n"):
                m = re.match(r"File '(.*)'", l)
                if m:
                    cur = os.path.normpath(os.path.join(build, m.group(1)))
                    continue
                m = re.match(r"Lines executed:([\d.]+)% of (\d+)", l)
                if m and cur:
                    if float(m.group(1)) > 0:
                        rel = os.path.relpath(cur, TOP)
                        if rel.startswith("src/"):
                            files.add(rel)
                    cur = None
    return files


def main():
    build = os.path.abspath(sys.argv[1])
    lagrit = os.path.join(build, "lagrit")
    if not os.access(lagrit, os.X_OK):
        sys.exit("no %s: build LaGriT with coverage first (see the top of this file)" % lagrit)
    want = sys.argv[2:]
    mp = os.path.join(HERE, "suite_sources.txt")
    old = {}
    if os.path.isfile(mp):
        every = set()
        for l in open(mp):
            if l.startswith("#") or ":" not in l:
                continue
            n, f = l.split(":", 1)
            if n.strip() == "every suite":
                every = set(f.split())
            else:
                old[n.strip()] = set(f.split())
        for n in old:
            old[n] |= every
    new = {}
    for dir_, runner in suites_of_run_all():
        if want and dir_ not in want:
            if dir_ in old:
                new[dir_] = old[dir_]
            continue
        for g in glob.glob(os.path.join(build, "**", "*.gcda"), recursive=True):
            os.remove(g)
        t0 = datetime.datetime.now()
        r = subprocess.run(["sh", "./" + runner, lagrit], cwd=os.path.join(HERE, dir_),
                           capture_output=True, text=True)
        new[dir_] = executed(build)
        print("%-32s exit %d, %4d s, %4d source files" % (
            dir_, r.returncode, (datetime.datetime.now() - t0).seconds, len(new[dir_])), flush=True)
    full = [v for v in new.values() if v]      # a suite that runs no lagrit (test_predicates) has none
    every = set.intersection(*full) if full else set()
    head = git_head()
    with open(mp, "w") as f:
        f.write("# suite_sources.txt: the src/ files each suite's run executes (at least\n"
                "# one line), for run_all.sh CHANGED=... (select_changed.py). Made by\n"
                "# make_suite_sources.py with a coverage build on %s, at commit %s.\n"
                "# 'every suite' lists the files all suites run; each suite line adds its own.\n"
                % (datetime.date.today().isoformat(), head))
        f.write("every suite: %s\n" % " ".join(sorted(every)))
        for n in new:
            f.write(("%s: %s" % (n, " ".join(sorted(new[n] - every)))).rstrip() + "\n")
    print("wrote %s: %d files run by every suite" % (mp, len(every)))


def git_head():
    try:
        return subprocess.run(["git", "-C", TOP, "rev-parse", "--short", "HEAD"],
                              capture_output=True, text=True).stdout.strip()
    except OSError:
        return "?"


if __name__ == "__main__":
    main()
