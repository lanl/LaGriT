#!/usr/bin/env python3
"""compare_outputs.py: a test's output files against the stored truth.

Used by check_upstream.py (test_lagrit_upstream). Python 3, standard
library only.

LaGriT's test folders keep, in reference/, not only the log
(outx3dgen) but the files a correct run writes; LaGriT's runtests.py
never compares them (user, Oct 17 2026: "stores the output and compares
a new run with the old stored away truth"). This compares each of them
with the file the test wrote.

The truth of a file: truth/<test>/<file> in this suite if there is one
(used where LaGriT's reference copy is out of date for LaGriT itself;
see truth/00README), else <test>/reference/<file>.

Files not compared: the log (outx3dgen, logx3dgen), copies of the
test's input files, pictures (.jpg .png .gif), Mac metadata (._*), and
reference files the deck does not write any more (listed as "not
written": the deck's dump lines are commented out; not a failure).

How two files are compared
  identical bytes                         same
  AVS mesh (.inp)                         same mesh: the same counts;
      vertices in the same order within 1e-6 of the mesh size; the
      same cells (type, material, vertices) in any order ("cells in
      another order" is noted); attribute values equal within a
      relative 1e-6
  other text                              word by word: numbers equal
      within a relative 1e-6 (or an absolute 1e-12 of the largest
      number), other words exactly; lines holding a date and time
      (e.g. the header of a .stor file) are skipped
  anything else (binary)                  must be identical

compare(test, src_dir, work_dir, truth_dir) -> (results, problems):
  results: [(file, verdict, detail)], verdict "same", "same*" (same,
  with a note), "DIFFERENT", "MISSING" or "not written";
  problems: number of DIFFERENT files.
"""
import os
import re

SKIP_NAMES = {"outx3dgen", "logx3dgen", "input.lgi"}
SKIP_EXT = (".jpg", ".jpeg", ".png", ".gif")
REL = 1e-6
DATE = re.compile(r"(Mon|Tue|Wed|Thu|Fri|Sat|Sun)\s+(Jan|Feb|Mar|Apr|May|Jun|"
                  r"Jul|Aug|Sep|Oct|Nov|Dec)\s+\d+\s+\d+:\d+:\d+\s+\d{4}")
NUM = re.compile(r"^[-+]?(\d+\.?\d*|\.\d+)([eEdD][-+]?\d+)?$")


def tofloat(s):
    return float(s.replace("D", "E").replace("d", "e"))


def read_text(path):
    try:
        return open(path, "rb").read().decode("utf-8")
    except UnicodeDecodeError:
        return None


def close(a, b, scale):
    return abs(a - b) <= max(REL * max(abs(a), abs(b)), 1e-12 * scale)


def compare_text(ta, tb):
    """(same?, detail)"""
    la = [l for l in ta.split("\n") if not DATE.search(l)]
    lb = [l for l in tb.split("\n") if not DATE.search(l)]
    wa = [w for l in la for w in l.split()]
    wb = [w for l in lb for w in l.split()]
    if len(wa) != len(wb):
        return False, "%d words, truth %d" % (len(wa), len(wb))
    nums = [abs(tofloat(w)) for w in wa if NUM.match(w)]
    scale = max(nums) if nums else 1.0
    worst, first = 0.0, None
    for k, (x, y) in enumerate(zip(wa, wb)):
        if x == y:
            continue
        if NUM.match(x) and NUM.match(y):
            fx, fy = tofloat(x), tofloat(y)
            if close(fx, fy, scale):
                continue
            d = abs(fx - fy) / max(abs(fx), abs(fy), 1e-300)
            worst = max(worst, d)
            if first is None:
                first = "word %d: %s, truth %s" % (k + 1, x, y)
        else:
            return False, "word %d: %r, truth %r" % (k + 1, x, y)
    if first:
        return False, "%s (largest relative difference %.2g)" % (first, worst)
    return True, ""


def parse_avs(text):
    """header, vertices, cells, vertex-attribute words, cell-attribute
    rows (one list of words per cell, in cell order)"""
    L = [l for l in text.split("\n")]
    head = L[0].split()
    nn, nc = int(head[0]), int(head[1])
    nnd = int(head[2]) if len(head) > 2 else 0
    ncd = int(head[3]) if len(head) > 3 else 0
    V = [tuple(tofloat(x) for x in l.split()[1:4]) for l in L[1:1 + nn]]
    C = [l.split()[1:] for l in L[1 + nn:1 + nn + nc]]
    k = 1 + nn + nc
    node = []
    if nnd > 0:
        na = int(L[k].split()[0])
        node = [w for l in L[k:k + 1 + na + nn] for w in l.split()]
        k += 1 + na + nn
    cellrows, chead = [], []
    if ncd > 0:
        na = int(L[k].split()[0])
        chead = [w for l in L[k:k + 1 + na] for w in l.split()]
        cellrows = [l.split()[1:] for l in L[k + 1 + na:k + 1 + na + nc]]
    return head, V, C, node, (chead, cellrows)


def compare_avs(ta, tb):
    try:
        ha, Va, Ca, Na, (Ha, Ra) = parse_avs(ta)
        hb, Vb, Cb, Nb, (Hb, Rb) = parse_avs(tb)
    except (ValueError, IndexError):
        return compare_text(ta, tb)
    if ha[:5] != hb[:5]:
        return False, "header %s, truth %s" % (" ".join(ha[:5]), " ".join(hb[:5]))
    span = max([max(p[k] for p in Vb) - min(p[k] for p in Vb)
                for k in range(3)] or [1.0]) or 1.0
    for i, (p, q) in enumerate(zip(Va, Vb)):
        if any(abs(p[k] - q[k]) > 1e-6 * span for k in range(3)):
            return False, "vertex %d at %s, truth %s" % (
                i + 1, " ".join("%g" % x for x in p), " ".join("%g" % x for x in q))
    note = ""
    order = list(range(len(Ca)))            # truth cell of each cell
    if Ca != Cb:
        if sorted(map(tuple, Ca)) != sorted(map(tuple, Cb)):
            return False, "cells differ (first: %s, truth %s)" % next(
                (" ".join(a), " ".join(b)) for a, b in zip(Ca, Cb) if a != b)
        note = "cells in another order"
        where = {}
        for j, c in enumerate(Cb):
            where.setdefault(tuple(c), []).append(j)
        order = [where[tuple(c)].pop(0) for c in Ca]
    same, d = compare_text(" ".join(Na), " ".join(Nb))
    if not same:
        return False, "vertex attributes: " + d
    if Ra or Rb:
        Rb2 = [Rb[j] for j in order] if len(Rb) == len(Ca) else Rb
        same, d = compare_text(" ".join(Ha + [w for r in Ra for w in r]),
                               " ".join(Hb + [w for r in Rb2 for w in r]))
        if not same:
            return False, "cell attributes: " + d
    return True, note


def compare(test, src_dir, work_dir, truth_dir):
    ref = os.path.join(src_dir, "reference")
    results, problems = [], 0
    if not os.path.isdir(ref):
        return results, problems
    tdir = os.path.join(truth_dir, test)
    names = sorted(set(os.listdir(ref)) |
                   (set(os.listdir(tdir)) if os.path.isdir(tdir) else set()))
    for name in names:
        if (name in SKIP_NAMES or name.startswith(("._", "stdout", "diffout"))
                or name.lower().endswith(SKIP_EXT) or name == "00README"
                or os.path.exists(os.path.join(src_dir, name))):
            continue
        truth = os.path.join(tdir, name)
        src = "truth/"
        if not os.path.isfile(truth):
            truth, src = os.path.join(ref, name), "reference/"
        out = os.path.join(work_dir, name)
        if not os.path.isfile(out):
            results.append((name, "not written", "in %s, not written by "
                            "this deck" % src))
            continue
        a, b = open(out, "rb").read(), open(truth, "rb").read()
        if a == b:
            results.append((name, "same", "identical to " + src))
            continue
        ta, tb = read_text(out), read_text(truth)
        if ta is None or tb is None:
            results.append((name, "DIFFERENT", "binary, not identical to " + src))
            problems += 1
            continue
        if name.endswith(".inp"):
            same, d = compare_avs(ta, tb)
        else:
            same, d = compare_text(ta, tb)
        if same:
            results.append((name, "same*" if d else "same",
                            ("%s; " % d if d else "") + "equal to " + src +
                            " within the tolerances"))
        else:
            results.append((name, "DIFFERENT", d + " (" + src + ")"))
            problems += 1
    return results, problems
