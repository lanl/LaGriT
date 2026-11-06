#!/usr/bin/env python3
"""check_avs.py DECK WORKDIR [--golden-dir DIR]

Judges one test_avs_io deck from its "# check:" lines, using LaGriT's
output (WORKDIR/<deck>.out), the files the deck wrote, and the input
files make_inputs.py wrote. Python 3, standard library only.

CHECKS (all must hold)
  finished             LaGriT printed "LaGriT successfully completed"
  noerror              no "ERROR" and no "Error from" in LaGriT's output
  printable            LaGriT's output is printable ASCII
  output REGEX         some output line matches REGEX (any case)
  nooutput REGEX       no output line matches REGEX (any case)
  model OUT REF [parts=vca] [atts=A,B]
                       OUT (written by LaGriT) holds the mesh in REF:
                       the same vertices (exactly), cells (type,
                       material, vertices in AVS order) and the
                       attributes REF has (same type; integers exactly,
                       reals within 1e-11 relative: dump / avs writes 12
                       significant digits). OUT may hold more attributes
                       (LaGriT adds imt1, itp1, icr1, isn1). parts limits
                       the comparison to v(ertices), c(ells) and
                       a(ttributes); atts to the attributes named.
                       Vertex and cell numbers are positions: a file
                       numbered from 0 holds the same mesh.
  model_or_error OUT REF [...]
                       the model check holds, or LaGriT printed an ERROR
                       line (a refusal with a message is accepted)
  golden OUT           OUT is byte-identical to golden/OUT (the output of
                       LaGriT before any change to read / dump avs)
  same A B             A and B are byte-identical
  cells OUT N          OUT's header gives N cells
  value OUT vertex I x|y|z TEXT
                       that coordinate of vertex I is written as TEXT
  line OUT N TEXT      line N of OUT is TEXT (spaces compared as runs)

A deck with a "# known:" line documents a problem not yet fixed: it
gives KNOWN when a check fails, FIXED (exit 1: drop the known line)
when every check holds.

Prints one "check ... ok|FAIL ..." line per check, then "result
PASS|FAIL|KNOWN|FIXED" and, if not PASS, "why ...".
"""
import os
import re
import sys

REL = 1e-11


# ------------------------------------------------------------ AVS reader
# Written for this test, from the AVS UCD format (not from LaGriT's code).

def read_avs(path):
    lines = [l for l in open(path, errors="replace").read().split("\n")
             if l.strip() and not l.lstrip().startswith("#")]
    h = lines[0].split()
    nv, nc, nva, nca = (int(h[0]), int(h[1]), int(h[2]), int(h[3]))
    k = 1
    V = []
    for l in lines[k:k + nv]:
        w = l.split()
        V.append(tuple(float(x) for x in w[1:4]))
    k += nv
    raw = []
    for l in lines[k:k + nc]:
        w = l.split()
        raw.append((int(w[1]), w[2], [int(x) for x in w[3:]]))
    k += nc
    base = 1
    if nv:
        base = int(lines[1].split()[0])
    C = [(m, {"pri": "prism"}.get(t, t), [i - base for i in idx])
         for (m, t, idx) in raw]
    atts = {"v": {}, "c": {}}

    def section(store, n, k):
        w = lines[k].split()
        na = int(w[0])
        ranks = [int(x) for x in w[1:1 + na]]
        k += 1
        names = []
        for i in range(na):
            nm, _, typ = lines[k + i].partition(",")
            names.append((nm.strip(), typ.strip().split()[0] if typ.strip() else ""))
        k += na
        rows = lines[k:k + n] if n else lines[k:]
        for j, (nm, typ) in enumerate(names):
            store[nm] = (typ, [], ranks[j])
        for r in rows:
            vals = r.split()[1:]
            p = 0
            for nm, typ in names:
                rank = store[nm][2]
                store[nm][1].append(vals[p] if rank == 1 else vals[p:p + rank])
                p += rank
        return k + len(rows)

    if nva:
        k = section(atts["v"], nv, k)
    elif nca and k < len(lines) and lines[k].split() == ["0"]:
        k += 1
    if nca:
        k = section(atts["c"], nc, k)
    return {"nv": nv, "nc": nc, "V": V, "C": C, "atts": atts}


def num_equal(a, b, typ):
    if typ == "integer":
        try:
            return int(float(a)) == int(float(b)) and float(a) == float(b)
        except ValueError:
            return False
    try:
        x, y = float(a), float(b)
    except (TypeError, ValueError):
        return False
    return abs(x - y) <= REL * max(1e-300, abs(x), abs(y))


def compare(out, ref, parts="vca", only=None):
    """None if OUT holds REF's mesh, else the first difference"""
    if "v" in parts:
        if len(out["V"]) != len(ref["V"]):
            return "%d vertices, expected %d" % (len(out["V"]), len(ref["V"]))
        for i, (p, q) in enumerate(zip(out["V"], ref["V"])):
            if p != q:
                return "vertex %d at %r, expected %r" % (i + 1, p, q)
    if "c" in parts:
        if len(out["C"]) != len(ref["C"]):
            return "%d cells, expected %d" % (len(out["C"]), len(ref["C"]))
        for i, (p, q) in enumerate(zip(out["C"], ref["C"])):
            if p != q:
                return "cell %d is %r, expected %r" % (i + 1, p, q)
    if "a" in parts:
        for where, what in (("v", "vertex"), ("c", "cell")):
            for nm, (typ, vals, rank) in ref["atts"][where].items():
                if only and nm not in only:
                    continue
                if nm not in out["atts"][where]:
                    return "no %s attribute %s" % (what, nm)
                otyp, ovals, orank = out["atts"][where][nm]
                if otyp != typ:
                    return "%s attribute %s is %s, expected %s" % (what, nm, otyp, typ)
                if len(ovals) != len(vals):
                    return "%s attribute %s has %d values, expected %d" % (
                        what, nm, len(ovals), len(vals))
                for i, (a, b) in enumerate(zip(ovals, vals)):
                    if isinstance(b, list) or isinstance(a, list):
                        if not (isinstance(a, list) and isinstance(b, list) and len(a) == len(b)
                                and all(num_equal(x, y, typ) for x, y in zip(a, b))):
                            return "%s attribute %s, %s %d: %s, expected %s" % (
                                what, nm, what, i + 1, a, b)
                    elif not num_equal(a, b, typ):
                        return "%s attribute %s, %s %d: %s, expected %s" % (
                            what, nm, what, i + 1, a, b)
    return None


# ------------------------------------------------------------ checks

def main():
    deck = sys.argv[1]
    work = sys.argv[2]
    golden = os.path.join(os.path.dirname(os.path.abspath(__file__)), "golden")
    name = os.path.basename(deck)[:-4]
    text = open(deck).read()
    checks = [l[len("# check:"):].strip() for l in text.split("\n")
              if l.startswith("# check:")]
    known = [l[len("# known:"):].strip() for l in text.split("\n")
             if l.startswith("# known:")]
    out_path = os.path.join(work, name + ".out")
    try:
        output = open(out_path, "rb").read()
    except OSError:
        output = b""
    otext = output.decode("latin-1")
    # LaGriT echoes the deck, comments too: judge only its own lines
    olines = [l for l in otext.split("\n") if not l.startswith("#")]
    has_error = any(re.search(r"ERROR|Error from", l) for l in olines)
    fails = []

    def W(p):
        return os.path.join(work, p)

    def model_check(args, allow_error):
        out, ref = args[0], args[1]
        parts, only = "vca", None
        for a in args[2:]:
            if a.startswith("parts="):
                parts = a[6:]
            elif a.startswith("atts="):
                only = a[5:].split(",")
        try:
            why = compare(read_avs(W(out)), read_avs(W(ref)), parts, only)
        except Exception as e:  # unreadable or missing
            why = "cannot read %s: %s" % (out, e.__class__.__name__)
        if why and allow_error and has_error:
            return None
        return why

    for c in checks:
        w = c.split()
        kind, args = w[0], w[1:]
        why = None
        if kind == "finished":
            if "LaGriT successfully completed" not in otext:
                why = "LaGriT did not finish (%s)" % (
                    next((l.strip() for l in olines if re.search(
                        r"PANIC|runtime error|Segmentation|signal", l)), "no end message"))
        elif kind == "noerror":
            bad = [l.strip() for l in olines if re.search(r"ERROR|Error from", l)]
            if bad:
                why = bad[0]
        elif kind == "printable":
            if re.search(rb"[^\x09\x0a\x0d\x20-\x7e]", output):
                why = "non-printable characters in LaGriT's output"
        elif kind in ("output", "nooutput"):
            rx = c.split(None, 1)[1]
            hit = [l.strip() for l in olines if re.search(rx, l, re.I)]
            if kind == "output" and not hit:
                why = "no line matches %s" % rx
            if kind == "nooutput" and hit:
                why = "line: %s" % hit[0]
        elif kind in ("model", "model_or_error"):
            why = model_check(args, kind == "model_or_error")
        elif kind == "golden":
            g = os.path.join(golden, args[0])
            try:
                a = open(W(args[0]), "rb").read()
            except OSError:
                a = None
            if a is None:
                why = "%s not written" % args[0]
            elif not os.path.exists(g):
                why = "no golden/%s" % args[0]
            elif a != open(g, "rb").read():
                al, gl = a.split(b"\n"), open(g, "rb").read().split(b"\n")
                n = next((i for i, (x, y) in enumerate(zip(al, gl)) if x != y),
                         min(len(al), len(gl)))
                why = "differs from golden/%s at line %d" % (args[0], n + 1)
        elif kind == "same":
            try:
                if open(W(args[0]), "rb").read() != open(W(args[1]), "rb").read():
                    why = "%s and %s differ" % (args[0], args[1])
            except OSError as e:
                why = "missing: %s" % e.filename
        elif kind == "cells":
            try:
                n = int(open(W(args[0])).readline().split()[1])
                if n != int(args[1]):
                    why = "%d cells, expected %s" % (n, args[1])
            except (OSError, IndexError, ValueError):
                why = "cannot read %s" % args[0]
        elif kind == "value":
            out, i, comp, want = args[0], int(args[2]), "xyz".index(args[3]), args[4]
            try:
                L = open(W(out)).read().split("\n")
                got = L[i].split()[1 + comp]
                if got != want:
                    why = "vertex %d %s written as %s, expected %s" % (i, args[3], got, want)
            except (OSError, IndexError):
                why = "cannot read %s" % out
        elif kind == "line":
            out, n, want = args[0], int(args[1]), " ".join(args[2:])
            try:
                got = " ".join(open(W(out)).read().split("\n")[n - 1].split())
                if got != want:
                    why = "line %d is '%s', expected '%s'" % (n, got, want)
            except (OSError, IndexError):
                why = "cannot read %s" % out
        else:
            why = "unknown check %s" % kind
        print("check %-60s %s" % (c[:60], "ok" if why is None else "FAIL " + why))
        if why:
            fails.append("%s: %s" % (kind, why))
    if known:
        res = "KNOWN" if fails else "FIXED"
    else:
        res = "FAIL" if fails else "PASS"
    print("result", res)
    if res == "KNOWN":
        print("known", known[0])
    if fails:
        print("why", fails[0])
    if res == "FIXED":
        print("why every check holds: remove the '# known:' line")
    return 1 if res in ("FAIL", "FIXED") else 0


if __name__ == "__main__":
    sys.exit(main())
