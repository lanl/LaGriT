#!/usr/bin/env python3
"""select_changed.py SUITE_DIR BASE

For run_all.sh CHANGED=...: prints the suites (one name per line) that
what changed since BASE can affect, or ALL. BASE is a git ref; 1 means
the merge base with origin/master (or master). Changed = every file that
differs between BASE and the working tree, committed or not, plus new
untracked files. Writes the reasons to SUITE_DIR/.run_all_why.txt.

How a changed file selects suites:
  src/X (.f .c .cpp ...)    the suites whose runs execute X, from
                            suite_sources.txt (made by a coverage build,
                            make_suite_sources.py), and the suites that
                            compile X themselves (their files name src/X)
  src/X.h (an include)      as every source file that includes it
  test/suite/test_A/...     test_A, and every suite whose files name
                            test_A/<that file> (a shared checker)
  test/level01/...          test_lagrit_upstream
  docs/, *.md, .github/, PyLaGriT/, examples/, notes   nothing
  anything else (CMake files, src files the map has never seen,
  run_all.sh, lift_summary.py, this file, suite_sources.txt itself)
                            ALL: when in doubt, run everything
Python 3, standard library only.
"""
import os
import re
import subprocess
import sys


def git(top, *args):
    return subprocess.run(["git", "-C", top] + list(args), check=True,
                          capture_output=True, text=True).stdout


def main():
    suite_dir = os.path.abspath(sys.argv[1])
    base = sys.argv[2] if len(sys.argv) > 2 else "1"
    top = git(suite_dir, "rev-parse", "--show-toplevel").strip()
    why = []
    if base == "1":
        for ref in ("origin/master", "master"):
            try:
                base = git(top, "merge-base", "HEAD", ref).strip()
                why.append("CHANGED: compared with the merge base of HEAD and %s (%s)" % (ref, base[:8]))
                break
            except subprocess.CalledProcessError:
                continue
        else:
            sys.exit("select_changed.py: no origin/master or master to compare with")
    else:
        git(top, "rev-parse", "--verify", base + "^{commit}")
        why.append("CHANGED: compared with %s" % base)
    changed = set(git(top, "diff", "--name-only", base).split())
    changed |= set(git(top, "ls-files", "--others", "--exclude-standard").split())

    # the map
    mp = os.path.join(suite_dir, "suite_sources.txt")
    every, uses, suites = set(), {}, []
    for l in open(mp):
        if l.startswith("#") or ":" not in l:
            continue
        name, files = l.split(":", 1)
        name = name.strip()
        if name == "every suite":
            every = set(files.split())
        else:
            suites.append(name)
            uses[name] = set(files.split())
    for s in suites:
        uses[s] |= every
    known = set().union(*uses.values()) if uses else set()
    # suites in run_all.sh that the map does not have yet: they always run
    ra = open(os.path.join(suite_dir, "run_all.sh")).read().split('SUITES="', 1)[1].split('"', 1)[0]
    unmapped = sorted(l.split(":")[0] for l in ra.split() if l.split(":")[0] not in uses)

    def own_build(f):
        """suites that compile f themselves (test_predicates builds its
        drivers from src/lg_predicates*.c): their files name it"""
        b = os.path.basename(f)
        out = set()
        for s in suites:
            d = os.path.join(suite_dir, s)
            if not os.path.isdir(d):
                continue
            for fn in os.listdir(d):
                p = os.path.join(d, fn)
                if fn.endswith((".sh", ".py", ".c", ".f")) and os.path.isfile(p):
                    if "src/" + b in open(p, errors="replace").read():
                        out.add(s)
                        break
        return out

    sel, all_ = set(), None

    def pick(f, names, how):
        names = sorted(names)
        sel.update(names)
        why.append("%s: %s" % (f, how if not names else "%s -> %s" % (how, " ".join(
            n[5:] if n.startswith("test_") else n for n in names))))

    for f in sorted(changed):
        if f.startswith("src/"):
            if f.endswith((".h", ".inc", ".hpp")):
                inc = os.path.basename(f)
                users = set()
                for k in known:
                    p = os.path.join(top, k)
                    try:
                        if re.search(r'include\s*[\'"<]%s[\'">]' % re.escape(inc), open(p, errors="replace").read()):
                            users.add(k)
                    except OSError:
                        pass
                if not users and f not in known:
                    all_ = "%s: an include no mapped source file uses" % f
                    continue
                names = {s for s in suites if (users | {f}) & uses[s]}
                pick(f, names, "included by %d mapped files" % len(users))
            elif f in known or own_build(f):
                pick(f, {s for s in suites if f in uses[s]} | own_build(f), "executed by")
            elif os.path.isfile(os.path.join(top, f)):
                all_ = "%s: not in suite_sources.txt (new, or the map is old)" % f
            else:
                why.append("%s: deleted, and no suite ran it" % f)
        elif f.startswith("test/suite/test_"):
            a = f.split("/")[2]
            rel = "/".join(f.split("/")[2:])
            names = {a} if os.path.isdir(os.path.join(suite_dir, a)) else set()
            for s in suites:
                d = os.path.join(suite_dir, s)
                if s == a or not os.path.isdir(d):
                    continue
                for fn in os.listdir(d):
                    p = os.path.join(d, fn)
                    if os.path.isfile(p) and fn.endswith((".sh", ".py", ".lgi", ".mlgi", ".cases")):
                        if rel in open(p, errors="replace").read():
                            names.add(s)
                            break
            pick(f, names, "the suite's own file")
        elif f.startswith("test/level01/") or f.startswith("test/level02/"):
            pick(f, {"test_lagrit_upstream"}, "a LaGriT test")
        elif (f.startswith(("docs/", ".github/", "PyLaGriT/", "examples/", "notes/"))
              or f.endswith((".md", ".txt", ".html", ".png", ".pdf"))
              and not f.startswith("test/suite/") and not f.endswith("CMakeLists.txt")
              or f in ("test/suite/.gitignore",)):
            why.append("%s: affects no suite" % f)
        else:
            all_ = "%s: not sorted to suites, so everything runs" % f
    if unmapped and changed:
        sel.update(unmapped)
        why.append("not in suite_sources.txt yet, so always run: %s" % " ".join(unmapped))
    if all_:
        why.append("ALL, because " + all_)
    elif not sel:
        why.append("nothing to run: no change any suite can see")
    open(os.path.join(suite_dir, ".run_all_why.txt"), "w").write("\n".join(why) + "\n")
    if all_:
        print("ALL")
    else:
        for s in sorted(sel):
            print(s)
        if not sel:
            print("NONE")


if __name__ == "__main__":
    main()
