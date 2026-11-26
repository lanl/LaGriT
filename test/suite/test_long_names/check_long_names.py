#!/usr/bin/env python3
"""check_long_names.py WORKDIR LOGDIR

Judges test_long_names (Python 3, standard library only) from each
deck's output (WORKDIR/<deck>.out) and the files it wrote, against
WORKDIR/expect.json, which make_long_names.py writes with the decks.

A deck PASSES when
  - LaGriT finished ("LaGriT successfully completed"), with no signal,
    no Fortran runtime error and no MMVERIFY message;
  - every mesh object in "mo" exists and none in "no_mo" does (from
    the cmo / status / brief at the end of the deck);
  - every file in "files" was written (not empty), under its full
    name, and none in "no_files" was (the names cut to 32 characters,
    as LaGriT V3.4.0 and earlier wrote them);
  - every text in "has" appears in the output, and each text in
    "count" appears exactly that many times;
  - for each pair in "order", the second text comes after the first
    (a WARNING about a command's tokens comes after the command).
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
import os
import re
import sys

E = html.escape


def about():
    """{deck: what it holds}, from the list in make_long_names.py"""
    out, cur = {}, None
    src = open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "make_long_names.py")).read()
    for l in src.split('"""')[1].split("\n"):
        m = re.match(r"^  (ln\d\d_\w+)\s+(.*)$", l)
        if m:
            cur = m.group(1)
            out[cur] = m.group(2).strip()
        elif cur and l.startswith("      ") and l.strip():
            out[cur] += " " + l.strip()
        else:
            cur = None
    return out


def short(f):
    """a long file name as the page shows it"""
    return f if len(f) <= 40 else "%s...%s (%d characters)" % (f[:14], f[-12:], len(f))


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    rows = []
    ok_all = True
    for deck in sorted(exp):
        e = exp[deck]
        p = os.path.join(work, deck + ".out")
        text = open(p, errors="replace").read() if os.path.isfile(p) else ""
        flat = text.replace(" \n", "\n")
        fails = []
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        for bad in ("Program received signal", "Fortran runtime error", "MMVERIFY"):
            if bad in text:
                fails.append('"%s" in the output' % bad)
        names = set(re.findall(r"Mesh Object name:\s*(\S+)", text))
        for m in e["mo"]:
            if m not in names:
                fails.append("mesh object %s missing" % m)
        for m in e["no_mo"]:
            if m in names:
                fails.append("mesh object %s made" % m)
        written = []
        for f in e["files"]:
            q = os.path.join(work, f)
            if not os.path.isfile(q) or os.path.getsize(q) == 0:
                fails.append("file %s not written" % short(f))
            else:
                written.append(f)
        for f in e["no_files"]:
            if os.path.isfile(os.path.join(work, f)):
                fails.append("file %s written (a cut name)" % short(f))
        for t in e["has"]:
            if t not in text:
                fails.append('no "%s"' % t)
        for t, n in e["count"].items():
            got = text.count(t)
            if got != n:
                fails.append('"%s" %d times, expected %d' % (t, got, n))
        for a, b in e.get("order", []):
            ia, ib = flat.find(a), flat.find(b)
            if ia < 0 or ib < 0 or ib < ia:
                fails.append('"%s" not after "%s"' % (b, a))
        msgs = []
        lines = text.splitlines()
        for i, l in enumerate(lines):
            if l.startswith("WARNING: token"):
                msgs.append(l.rstrip())
                if i + 1 < len(lines):
                    msgs.append(lines[i + 1].rstrip())
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, msgs, e, written))
        print("  %-4s  %s%s" % (res, deck, ("  " + "; ".join(fails[:3])) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (r[0], r[1]) for r in rows)
    ABOUT = about()
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>long file names</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1100px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:left;vertical-align:top}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}pre{font-size:11px;background:#f6f8fa;padding:6px;margin:2px 0;white-space:pre-wrap}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">Long file names: up to 132 characters</h1>
<p>A word token is kept with 32 characters. A command that takes a file name (read, dump in every format,
dump / control_volume, infile, cmo / readatt, pset and eltset write, ...) gets it in full, up to 132 characters;
LaGriT V3.4.0 and earlier cut it to 32, without a message. A long word that was used cut (a mesh object name, a
define's value, a file name over 132 characters) gives a WARNING after the command, saying how much was used.</p>
<p><b>What it does:</b> the decks are written by make_long_names.py (they hold names of exact lengths). The checker
looks for every file under its full name and for none under the name cut to 32, and reads LaGriT's output for the
WARNING lines and the mesh objects at the end. No mesh is drawn: these tests are about names.</p>
<table><tr><th>deck</th><th>result</th><th>files written (full names)</th><th>LaGriT's WARNING lines</th></tr>"""]
    for d, r, fails, msgs, e, written in rows:
        o.append('<tr><td>%s<details><summary>What it does</summary>%s</details></td><td class="%s">%s</td><td>%s</td><td><pre>%s</pre>%s</td></tr>'
                 % (E(d), E(ABOUT.get(d, "")), r, r,
                    "<br>".join(E(short(f)) for f in written) or "-",
                    E("\n".join(msgs) or "(none)"),
                    "".join('<div class="FAIL">%s</div>' % E(x) for x in fails)))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(r[1] == "PASS" for r in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
