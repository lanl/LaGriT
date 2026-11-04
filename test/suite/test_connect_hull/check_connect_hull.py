#!/usr/bin/env python3
"""check_connect_hull.py WORKDIR LOGDIR

Judges test_connect_hull (Python 3, standard library only). For each
mesh, against the input coordinates (exact rationals where floating
point cannot decide):
  valid     every tet positively oriented (exactly), no face in more
            than two tets
  missing   the share of the convex hull's volume (computed here with
            an incremental hull) not covered by the tets: 0 when the
            mesh fills the hull; about 1e-7 when only pockets flatter
            than 1e-6 of their size are left open
  open      boundary faces with a point outside (pockets left)
  nonDel    (connect / exact) interior faces not locally Delaunay
A deck PASSES when the mesh is valid and, with the fill (plain connect
and exact), missing <= 1e-6; with exact also nonDel = 0. nohull decks are
the baseline (valid only).
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
import math
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
    return det3([[b[i] - a[i] for i in range(3)], [c[i] - a[i] for i in range(3)], [d[i] - a[i] for i in range(3)]])


def insphere(a, b, c, d, e):
    rows = []
    for p in (a, b, c, d):
        w = [p[i] - e[i] for i in range(3)]
        rows.append(w + [w[0] * w[0] + w[1] * w[1] + w[2] * w[2]])
    s = 0
    for j in range(4):
        s += (-1) ** j * rows[0][j] * det3([[rows[r][k] for k in range(4) if k != j] for r in range(1, 4)])
    o = orient(a, b, c, d)
    return 0 if o == 0 else (-s if o > 0 else s)



def hull_volume(P):
    """volume of the convex hull of the points P (incremental hull; the
    visibility tests exact, in rationals, when floating point is close)"""
    PF = {}

    def pf(i):
        if i not in PF:
            PF[i] = tuple(Fr(v) for v in P[i])
        return PF[i]

    def side(f, q):
        a, b, c = f
        o = orient(P[a], P[b], P[c], P[q])
        h = max(abs(P[x][k] - P[q][k]) for x in f for k in range(3))
        if abs(o) > 1e-9 * h ** 3:
            return 1 if o > 0 else -1
        o = orient(pf(a), pf(b), pf(c), pf(q))
        return (o > 0) - (o < 0)

    n = len(P)
    i0 = 0
    i1 = max(range(n), key=lambda i: math.dist(P[i], P[i0]))
    best, i2 = -1.0, None
    for i in range(n):
        ab = [P[i1][k] - P[i0][k] for k in range(3)]
        ac = [P[i][k] - P[i0][k] for k in range(3)]
        cr = [ab[1] * ac[2] - ab[2] * ac[1], ab[2] * ac[0] - ab[0] * ac[2], ab[0] * ac[1] - ab[1] * ac[0]]
        v = sum(x * x for x in cr)
        if v > best:
            best, i2 = v, i
    best, i3 = -1.0, None
    for i in range(n):
        v = abs(orient(P[i0], P[i1], P[i2], P[i]))
        if v > best:
            best, i3 = v, i
    faces = [(i0, i1, i2), (i0, i1, i3), (i0, i2, i3), (i1, i2, i3)]
    cen = [sum(P[i][k] for i in (i0, i1, i2, i3)) / 4 for k in range(3)]
    fixed = []
    for f in faces:
        a, b, c = f
        if orient(P[a], P[b], P[c], cen) > 0:   # make the centre inside: orient < 0
            f = (a, c, b)
        fixed.append(f)
    faces = set(fixed)
    for q in range(n):
        if q in (i0, i1, i2, i3):
            continue
        vis = [f for f in faces if side(f, q) > 0]
        if not vis:
            continue
        edges = {}
        for f in vis:
            for e in ((f[0], f[1]), (f[1], f[2]), (f[2], f[0])):
                edges[e] = edges.get(e, 0) + 1
        for f in vis:
            faces.discard(f)
        for (a, b), _ in list(edges.items()):
            if (b, a) not in edges:
                faces.add((a, b, q))
    vol = 0.0
    for a, b, c in faces:
        vol += -orient(P[a], P[b], P[c], cen) / 6.0
    return abs(vol)

def check(X, T, exact):
    XF = {}

    def xf(i):
        if i not in XF:
            XF[i] = tuple(Fr(v) for v in X[i])
        return XF[i]

    vol = [orient(*(X[i] for i in t)) for t in T]
    scale = max(abs(v) for v in vol)
    pos = sum(v > 0 for v in vol)
    sgn0 = 1 if pos >= len(vol) - pos else -1      # LaGriT's orientation
    bad = 0
    for t, v in zip(T, vol):
        if v * sgn0 > 1e-9 * scale:
            continue
        if orient(*(xf(i) for i in t)) * sgn0 <= 0:
            bad += 1
    faces = {}
    for k, t in enumerate(T):
        for j in range(4):
            faces.setdefault(frozenset(t[:j] + t[j + 1:]), []).append((k, t[j]))
    over = sum(1 for L in faces.values() if len(L) > 2)
    # gap: points outside boundary faces
    gap, nopen = 0.0, 0
    used = sorted(set(i for t in T for i in t))
    diag = math.dist([min(X[i][k] for i in used) for k in range(3)],
                     [max(X[i][k] for i in used) for k in range(3)])
    for f, L in faces.items():
        if len(L) != 1:
            continue
        k, w = L[0]
        a, b, c = [X[i] for i in f]
        nrm = [(b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]),
               (b[2] - a[2]) * (c[0] - a[0]) - (b[0] - a[0]) * (c[2] - a[2]),
               (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])]
        nl = math.sqrt(sum(x * x for x in nrm))
        if nl == 0:
            continue
        sgn = -1.0 if sum(nrm[i] * (X[w][i] - a[i]) for i in range(3)) > 0 else 1.0
        L2 = max(math.dist(a, b), math.dist(b, c), math.dist(c, a))
        worst = 0.0
        for q in used:
            d = sgn * sum(nrm[i] * (X[q][i] - a[i]) for i in range(3)) / nl
            if d > worst:
                worst = d
        if worst > 1e-12 * L2:
            nopen += 1
            gap = max(gap, worst / diag)
    nondel = 0
    if exact:
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
    hv = hull_volume([X[i] for i in used])
    mv = sum(abs(v) for v in vol) / 6.0
    return dict(tets=len(T), invalid=bad + over, gap=gap, open=nopen, nonDel=nondel,
                missing=(hv - mv) / hv)


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    rows, ok_all = [], True
    for deck in sorted(exp):
        e = exp[deck]
        fails, r, added = [], None, None
        outp = os.path.join(work, deck + ".out")
        text = open(outp, errors="replace").read() if os.path.isfile(outp) else ""
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        for l in text.splitlines():
            if "tets to reach the convex hull" in l:
                added = int(l.split()[2])
        mesh = os.path.join(work, "out_%s.inp" % deck)
        if os.path.isfile(mesh):
            X0, _ = read_avs(os.path.join(work, "pts_%s.inp" % e["set"]))
            X, T = read_avs(mesh)
            X = list(X0) + list(X[len(X0):])
            r = check(X, T, e["mode"] == "exact")
            if r["invalid"]:
                fails.append("%d tets inverted or overlapping" % r["invalid"])
            if e["mode"] != "nohull" and r["missing"] > 1e-6:
                fails.append("%.2g of the hull volume missing" % r["missing"])
            if e["mode"] == "exact" and r["nonDel"]:
                fails.append("%d faces not Delaunay" % r["nonDel"])
            if e["mode"] == "nohull" and added is not None:
                fails.append("nohull still filled")
        else:
            fails.append("no mesh written")
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            if r:
                f.write("".join("%s %s\n" % kv for kv in sorted(r.items())))
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, e, r, added))
        print("  %-4s  %-22s %s%s" % (res, deck, ("tets=%d added=%s missing=%.2g open=%d nonDel=%d" %
              (r["tets"], added, r["missing"], r["open"], r["nonDel"])) if r else "", ("  " + "; ".join(fails)) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (d, s) for d, s, _, _, _, _ in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>connect hull</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1150px;color:#1f2328;background:#fff}
p{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:right;vertical-align:top}td:first-child,th:first-child,td.l{text-align:left}th{background:#f6f8fa}
.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}
details summary{cursor:pointer;color:#0969da;font-size:12px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">3D connect: fill to the convex hull</h1>
<p>After the enclosing tetrahedron is removed, connect adds the Delaunay tets between the mesh and the convex hull
(src/lg_connect3d_hull.c). <i>added</i>: tets added; <i>missing</i>: the share of the convex hull's volume not covered (pockets flatter than 1e-6 of their size are left open by design);
<i>open</i>: boundary faces with a point outside; <i>nonDel</i>: faces not Delaunay (exact rationals, connect / exact).
<b>nohull</b> keeps the mesh as the enclosing tet left it: the baseline.</p>
<table><tr><th>deck</th><th>result</th><th>points</th><th>tets</th><th>added</th><th>missing</th><th>open</th><th>nonDel</th>
<th class="l">notes</th></tr>"""]
    for d, s, fails, e, r, added in rows:
        o.append('<tr><td>%s<details><summary>What this tests</summary>%s</details></td><td class="%s">%s</td>'
                 '<td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class="l">%s</td></tr>'
                 % (E(d), E(e["about"]), s, s, e["n"], r["tets"] if r else "-", "-" if added is None else added,
                    "%.2g" % r["missing"] if r else "-", r["open"] if r else "-",
                    r["nonDel"] if r and e["mode"] == "exact" else "", "".join('<div class="FAIL">%s</div>' % E(x) for x in fails)))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(s == "PASS" for _, s, _, _, _, _ in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
