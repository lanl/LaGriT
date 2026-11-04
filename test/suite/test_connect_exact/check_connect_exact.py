#!/usr/bin/env python3
"""check_connect_exact.py WORKDIR LOGDIR

Judges test_connect_exact (Python 3, standard library only). For each
deck it reads the input points (WORKDIR/pts_<set>.inp, 17 digits, the
coordinates LaGriT read) and the mesh LaGriT wrote (out_<deck>.inp),
and computes with exact rational arithmetic where floating point
cannot decide:

  verts      vertices in the mesh (points added by connect: verts - n)
  missing    input points in no tet (dudded)
  nonDel     interior faces that are not locally Delaunay: the vertex
             across the face is strictly inside the other tet's
             circumsphere
  inverted   tets whose orientation is opposite to the majority, or
             exactly flat
  flat_fp    tets whose volume is zero or negative in floating point
             but positive exactly (flat to round-off)

A connect / exact deck PASSES when nothing was added, nothing is
missing, nonDel = 0, inverted = 0 and flat_fp = 0 (the sphere: flat_fp
is reported, a known property of its exact Delaunay tetrahedralization,
not a failure). A plain connect deck PASSES when LaGriT finished and
inverted = 0; its other numbers are shown as the baseline the exact
tests are compared with.
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
import os
import sys
from fractions import Fraction as Fr

E = html.escape


def read_avs(fn):
    with open(fn) as f:
        h = f.readline().split()
        n, ne = int(h[0]), int(h[1])
        X = [tuple(float(v) for v in f.readline().split()[1:4]) for _ in range(n)]
        T = [tuple(int(v) - 1 for v in f.readline().split()[3:7]) for _ in range(ne)]
    return X, T


def det3(m):
    return (m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1]) - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
            + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]))


def orient(a, b, c, d):
    """det(b - a, c - a, d - a): positive for a positively oriented tet"""
    return det3([[b[i] - a[i] for i in range(3)], [c[i] - a[i] for i in range(3)], [d[i] - a[i] for i in range(3)]])


def insphere(a, b, c, d, e):
    """> 0 when e is strictly inside the sphere through a, b, c, d (any order)"""
    rows = []
    for p in (a, b, c, d):
        w = [p[i] - e[i] for i in range(3)]
        rows.append(w + [w[0] * w[0] + w[1] * w[1] + w[2] * w[2]])
    s = 0
    for j in range(4):
        s += (-1) ** j * rows[0][j] * det3([[rows[r][k] for k in range(4) if k != j] for r in range(1, 4)])
    o = orient(a, b, c, d)
    return 0 if o == 0 else (-s if o > 0 else s)


def check(X0, X, T):
    X = list(X0) + list(X[len(X0):])
    XF = {}

    def xf(i):
        if i not in XF:
            XF[i] = tuple(Fr(v) for v in X[i])
        return XF[i]

    vol = [orient(*(X[i] for i in t)) for t in T]
    scale = max(abs(v) for v in vol) if vol else 1.0
    pos = sum(v > 0 for v in vol)
    sgn = 1 if pos >= len(vol) - pos else -1
    inverted = flat_fp = 0
    for t, v in zip(T, vol):
        if v * sgn > 1e-10 * scale:
            continue
        o = orient(*(xf(i) for i in t)) * sgn
        if o <= 0:
            inverted += 1
        elif v * sgn <= 0:
            flat_fp += 1
    faces = {}
    for k, t in enumerate(T):
        for j in range(4):
            faces.setdefault(frozenset(t[:j] + t[j + 1:]), []).append((k, t[j]))
    nondel = 0
    for f, L in faces.items():
        if len(L) != 2:
            continue
        (k, _), (_, z) = L
        P = [X[i] for i in T[k]] + [X[z]]
        r = insphere(*P)
        h = max(abs(P[i][j] - P[4][j]) for i in range(4) for j in range(3))
        if r < -1e-8 * h ** 5:
            continue
        if insphere(*(xf(i) for i in list(T[k]) + [z])) > 0:
            nondel += 1
    used = set(i for t in T for i in t)
    missing = sum(1 for i in range(len(X0)) if i not in used)
    return dict(tets=len(T), verts=len(X), missing=missing, nonDel=nondel, inverted=inverted, flat_fp=flat_fp)


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    rows, ok_all = [], True
    for deck in sorted(exp):
        e = exp[deck]
        fails, r = [], None
        text = open(os.path.join(work, deck + ".out"), errors="replace").read() \
            if os.path.isfile(os.path.join(work, deck + ".out")) else ""
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        mesh = os.path.join(work, "out_%s.inp" % deck)
        if os.path.isfile(mesh):
            X0, _ = read_avs(os.path.join(work, "pts_%s.inp" % e["set"]))
            X, T = read_avs(mesh)
            r = check(X0, X, T) if T else dict(tets=0, verts=len(X), missing=len(X0), nonDel=0, inverted=0, flat_fp=0)
            r["added"] = r["verts"] - e["n"]
            if r["inverted"]:
                fails.append("%d tets inverted or flat (exactly)" % r["inverted"])
            if e["mode"] == "exact":
                for k, what in (("added", "points added"), ("missing", "points missing"),
                                ("nonDel", "faces not Delaunay")):
                    if r[k]:
                        fails.append("%d %s" % (r[k], what))
                if r["flat_fp"] and not e["flat_known"]:
                    fails.append("%d tets flat to round-off" % r["flat_fp"])
                if r["tets"] == 0:
                    fails.append("no tets")
        else:
            fails.append("no mesh written")
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            if r:
                f.write("".join("%s %s\n" % kv for kv in sorted(r.items())))
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, e, r))
        print("  %-4s  %-24s %s%s" % (res, deck, " ".join("%s=%s" % (k, r[k]) for k in
              ("tets", "added", "missing", "nonDel", "inverted", "flat_fp")) if r else "",
              ("  " + "; ".join(fails)) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (d, s) for d, s, _, _, _ in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>connect exact</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1150px;color:#1f2328;background:#fff}
p{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:right;vertical-align:top}td:first-child,th:first-child,td.l{text-align:left}th{background:#f6f8fa}
.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}.bad{color:#cf222e}
details summary{cursor:pointer;color:#0969da;font-size:12px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">3D connect: tolerance tests and connect / exact</h1>
<p>Each point set is connected with plain <b>connect</b> (tolerance in-sphere tests, plus points added in small tets)
and with <b>connect / exact</b> (exact in-sphere tests, no points added). Every mesh is checked against the input
coordinates with exact rational arithmetic. <i>added</i>: points connect added; <i>missing</i>: input points in no tet
(dudded); <i>nonDel</i>: interior faces that are not Delaunay; <i>inverted</i>: tets inverted or flat, exactly;
<i>flat fp</i>: tets valid exactly but of zero or negative volume in floating point. connect / exact must add and
miss nothing and be exactly Delaunay; plain connect must give valid tets, and its other numbers are the baseline.</p>
<table><tr><th>deck</th><th>result</th><th>points</th><th>tets</th><th>added</th><th>missing</th><th>nonDel</th>
<th>inverted</th><th>flat fp</th><th class="l">notes</th></tr>"""]
    for d, s, fails, e, r in rows:
        cells = "".join('<td%s>%s</td>' % (' class="bad"' if r and r[k] and (e["mode"] == "exact" or k == "inverted")
                        and not (k == "flat_fp" and e["flat_known"]) else "", r[k] if r else "-")
                        for k in ("tets", "added", "missing", "nonDel", "inverted", "flat_fp"))
        o.append('<tr><td>%s<details><summary>What this tests</summary>%s</details></td><td class="%s">%s</td>'
                 '<td>%d</td>%s<td class="l">%s</td></tr>'
                 % (E(d), E(e["about"]), s, s, e["n"], cells,
                    "".join('<div class="FAIL">%s</div>' % E(x) for x in fails) or
                    ("known: flat to round-off" if e["flat_known"] else "")))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(s == "PASS" for _, s, _, _, _ in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
