#!/usr/bin/env python3
"""check_predicates.py WORKDIR LOGDIR

test_predicates (Python 3, standard library only): the exact geometric
predicates of 2D and 3D connect (src/lg_predicates2d.c,
src/lg_predicates3d.c, src/lg_exact_int.h) against exact rational
arithmetic (Python fractions) on adversarial inputs: lattices moved by
multiples of tiny powers of two, lattices with spacing 1/n, exact ties,
UTM-sized coordinates with tiny offsets, wild scales, points on a
circle or sphere, random points. Fixed seeds. Every sign must agree.
WORKDIR holds the compiled drivers (pred2d, pred3d).
Writes LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html, math, os, random, struct, subprocess, sys
from fractions import Fraction as F

E = html.escape
KINDS3 = ["lattice + k 2^-m", "lattice 1/n", "cube corners (ties)", "UTM + tiny", "wild scales",
          "on a sphere", "random"]
KINDS2 = ["lattice + k 2^-m", "lattice 1/n", "integer ties", "UTM + tiny", "wild scales", "on a circle"]


def sgn(x):
    return (x > 0) - (x < 0)


def det3(m):
    return (m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1]) - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
            + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]))


def gen3(rng, kind):
    if kind == 0:
        j = 2.0 ** -rng.randint(20, 52)
        return [rng.randint(0, 3) + rng.randint(-2, 2) * j for _ in range(15)]
    if kind == 1:
        n = rng.randint(3, 100)
        return [rng.randint(0, 4) / n for _ in range(15)]
    if kind == 2:
        return [float(rng.randint(0, 1)) for _ in range(15)]
    if kind == 3:
        P = []
        for _ in range(5):
            P += [400000 + 10 * rng.randint(0, 2) + rng.uniform(-1e-9, 1e-9), 3800000 + 10 * rng.randint(0, 2),
                  1000 + 10 * rng.randint(0, 2) + rng.choice([0, 2 ** -30])]
        return P
    if kind == 4:
        return [rng.choice([1, -1]) * rng.random() * 10 ** rng.randint(-30, 30) for _ in range(15)]
    if kind == 5:
        P = []
        for _ in range(5):
            z = rng.uniform(-1, 1); t = rng.uniform(0, 6.283); r = math.sqrt(1 - z * z)
            P += [r * math.cos(t), r * math.sin(t), z]
        return P
    return [rng.random() for _ in range(15)]


def gen2(rng, kind):
    if kind == 0:
        j = 2.0 ** -rng.randint(20, 52)
        return [rng.randint(0, 3) + rng.randint(-2, 2) * j for _ in range(8)]
    if kind == 1:
        n = rng.randint(3, 100)
        return [rng.randint(0, 4) / n for _ in range(8)]
    if kind == 2:
        return [float(rng.randint(0, 2)) for _ in range(8)]
    if kind == 3:
        P = []
        for _ in range(4):
            P += [400000 + 10 * rng.randint(0, 2) + rng.choice([0, 2 ** -30]),
                  3800000 + 10 * rng.randint(0, 2) + rng.uniform(-1e-9, 1e-9)]
        return P
    if kind == 4:
        return [rng.choice([1, -1]) * rng.random() * 10 ** rng.randint(-30, 30) for _ in range(8)]
    P = []
    for _ in range(4):
        t = rng.uniform(0, 6.283); P += [math.cos(t), math.sin(t)]
    return P


def want3(P):
    a, b, c, d, e = [tuple(F(x) for x in P[3 * i:3 * i + 3]) for i in range(5)]
    oe = sgn(det3([[a[i] - d[i] for i in range(3)], [b[i] - d[i] for i in range(3)], [c[i] - d[i] for i in range(3)]]))
    rows = []
    for p in (a, b, c, d):
        w = [p[i] - e[i] for i in range(3)]
        rows.append(w + [w[0] ** 2 + w[1] ** 2 + w[2] ** 2])
    s = 0
    for j in range(4):
        s += (-1) ** j * rows[0][j] * det3([[rows[r][k] for k in range(4) if k != j] for r in range(1, 4)])
    return (oe * sgn(s) if oe else 0), oe


def want2(P):
    ax, ay, bx, by, cx, cy, px, py = [F(x) for x in P]
    oe = sgn((ax - cx) * (by - cy) - (ay - cy) * (bx - cx))
    adx, ady, bdx, bdy, cdx, cdy = ax - px, ay - py, bx - px, by - py, cx - px, cy - py
    D = ((adx * adx + ady * ady) * (bdx * cdy - cdx * bdy) + (bdx * bdx + bdy * bdy) * (cdx * ady - adx * cdy)
         + (cdx * cdx + cdy * cdy) * (adx * bdy - bdx * ady))
    return oe * sgn(D), oe


def run(exe, gen, want, kinds, n, nd, seed):
    rng = random.Random(seed)
    cases = [(k, gen(rng, k)) for _ in range(n) for k in [rng.randrange(len(kinds))]]
    data = b"".join(struct.pack("%dd" % nd, *P) for _, P in cases)
    out = subprocess.run([exe], input=data, capture_output=True).stdout.decode().split()
    stats = {k: [0, 0, 0, 0] for k in range(len(kinds))}    # tests, wrong, zero, near
    for i, (k, P) in enumerate(cases):
        s, o = int(out[2 * i]), int(out[2 * i + 1])
        ws, wo = want(P)
        st = stats[k]
        st[0] += 1
        st[1] += (s != ws) + (o != wo) > 0
        st[2] += ws == 0
    return stats


def main(work, logdir):
    r3 = run(os.path.join(work, "pred3d"), gen3, want3, KINDS3, 20000, 15, 101)
    r2 = run(os.path.join(work, "pred2d"), gen2, want2, KINDS2, 40000, 8, 202)
    ok = all(v[1] == 0 for v in list(r3.values()) + list(r2.values()))
    rows = [("3D in-sphere / orient", KINDS3[k], v) for k, v in r3.items()] + \
           [("2D in-circle / orient", KINDS2[k], v) for k, v in r2.items()]
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        for t, k, v in rows:
            f.write("%s | %s | tests %d wrong %d ties %d\n" % (t, k, v[0], v[1], v[2]))
    for t, k, v in rows:
        print("  %-4s  %-22s %-22s tests %6d  wrong %d  exact ties %d" % ("PASS" if v[1] == 0 else "FAIL", t, k,
                                                                         v[0], v[1], v[2]))
    o = ["""<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>exact predicates</title><style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1000px;
color:#1f2328;background:#fff}p{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:13px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 8px;text-align:right}td.l,th.l{text-align:left}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">Exact predicates of connect against rational arithmetic</h1>
<p>src/lg_predicates2d.c and src/lg_predicates3d.c (with src/lg_exact_int.h) decide each orientation, in-circle and
in-sphere test in double precision with an error bound, then in double-double, then exactly in integers. Every answer
here is compared with the same test in exact rational arithmetic. <i>exact ties</i>: tests whose true answer is 0.
Before the integer stage (Nov 2026) 341 of the 2D tests and 147 of the 3D tests here were wrong (most with coordinates
of wildly different scales, a few on jittered lattices and at UTM offsets): near-ties below about 1e-28 of the
determinant counted as ties.</p><table><tr><th class="l">test</th><th class="l">inputs</th><th>tests</th>
<th>wrong</th><th>exact ties</th><th>result</th></tr>"""]
    for t, k, v in rows:
        r = "PASS" if v[1] == 0 else "FAIL"
        o.append('<tr><td class="l">%s</td><td class="l">%s</td><td>%d</td><td>%d</td><td>%d</td><td class="%s">%s</td></tr>'
                 % (E(t), E(k), v[0], v[1], v[2], r, r))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    print("\n%s" % ("all agree" if ok else "MISMATCHES"))
    return 0 if ok else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
