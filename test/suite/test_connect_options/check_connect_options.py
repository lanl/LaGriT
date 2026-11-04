#!/usr/bin/env python3
"""check_connect_options.py WORKDIR LOGDIR

Judges test_connect_options (Python 3, standard library only) from each
deck's output (WORKDIR/<deck>.out) against WORKDIR/expect.json, which
make_connect_options.py writes with the decks.

A deck PASSES when
  - LaGriT finished ("LaGriT successfully completed"), with no signal
    and no MMVERIFY message;
  - the node and element counts of mesh object mo (from the
    cmo / status / brief at the end) meet "nodes" and "elements":
    ("eq", n) exactly n, ("gt", n) more than n, ("same", deck) the
    same as that deck's;
  - every text in "has" appears in the output.
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
import os
import re
import sys

E = html.escape


def counts(text):
    n = re.findall(r"number of nodes\s*=\s*(\d+)", text)
    e = re.findall(r"number of elements\s*=\s*(\d+)", text)
    return (int(n[-1]) if n else None, int(e[-1]) if e else None)


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    got = {}
    texts = {}
    for deck in exp:
        p = os.path.join(work, deck + ".out")
        texts[deck] = open(p, errors="replace").read() if os.path.isfile(p) else ""
        got[deck] = counts(texts[deck])
    rows = []
    ok_all = True
    for deck in sorted(exp):
        e, text = exp[deck], texts[deck]
        fails = []
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        for bad in ("Program received signal", "MMVERIFY"):
            if bad in text:
                fails.append('"%s" in the output' % bad)
        for k, i in (("nodes", 0), ("elements", 1)):
            if k not in e:
                continue
            op, v = e[k]
            have = got[deck][i]
            want = got[v][i] if op == "same" else v
            if have is None:
                fails.append("no %s count" % k)
            elif op in ("eq", "same") and have != want:
                fails.append("%s %d, expected %d%s" % (k, have, want, " (as %s)" % v if op == "same" else ""))
            elif op == "gt" and not have > want:
                fails.append("%s %d, expected more than %d" % (k, have, want))
        for t in e.get("has", []):
            if t not in text:
                fails.append('no "%s"' % t)
        msgs = [l.rstrip() for l in text.splitlines() if "WARNING: connect" in l or l.startswith("   (") or
                l.startswith("   tetrahedron needs") or l.startswith("   triangle needs")]
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, msgs, e, got[deck]))
        print("  %-4s  %s%s" % (res, deck, ("  " + "; ".join(fails[:3])) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (r[0], r[1]) for r in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>connect options</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1150px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:left;vertical-align:top}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}code,pre{font-size:11px;background:#f6f8fa}pre{padding:6px;margin:2px 0;white-space:pre-wrap}
details summary{cursor:pointer;color:#0969da;font-size:12px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">connect options: noadd, check_interface, old_order, point range, enclosing tet</h1>
<p>The keywords <b>noadd</b>, <b>check_interface</b> and <b>old_order</b> may be anywhere in the command, in 2D and 3D
(<b>check_interface</b> is 3D only: a WARNING in 2D). When <b>delaunay</b> is left out of token 2, a point range or a pset
there is still read as the point range. The coordinates of the enclosing tetrahedron (12 numbers) or triangle (9) are
used with a WARNING that they are deprecated; fewer numbers give a WARNING and are ignored. Mesh A: 3D, two materials
split by a tilted plane, 216 points. Mesh B: 3D, 125 points, points 1 to 60 chosen. Mesh C: 2D, two materials, 49
points. The column V3.3.4 says what LaGriT V3.3.4 did with the same command.</p>
<table><tr><th>deck</th><th>result</th><th>command</th><th>nodes / elements</th><th>V3.3.4</th><th>LaGriT's warnings</th></tr>"""]
    for d, r, fails, msgs, e, (nn, ne) in rows:
        o.append('<tr><td>%s<details><summary>What this tests</summary>%s</details></td><td class="%s">%s</td>'
                 '<td><code>%s</code></td><td>%s / %s</td><td>%s</td><td><pre>%s</pre>%s</td></tr>'
                 % (E(d), E(e["about"]), r, r, E(e["cmd"]), nn, ne, E(e["old"]),
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
