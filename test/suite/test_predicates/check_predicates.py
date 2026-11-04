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

# ---- small SVG helpers (standard library only; copied into each checker) ----
def svg_bars(title, labels, series, colors, width=560, height=220, logy=False, unit=""):
    """grouped bar chart: labels on x, series = [(name, [values])]"""
    import math
    E2 = lambda s: str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    L, R, T, B = 48, 12, 28, 58
    W, H = width - L - R, height - T - B
    vals = [v for _, vs in series for v in vs if v is not None]
    vmax = max([v for v in vals] + [1e-30])
    if logy:
        pos = [v for v in vals if v and v > 0]
        lo = 10 ** math.floor(math.log10(min(pos))) if pos else 1.0
        hi = 10 ** math.ceil(math.log10(vmax)) if vmax > 0 else 10.0
        if hi <= lo: hi = lo * 10
        def Y(v):
            if not v or v <= 0: return T + H
            return T + H - H * (math.log10(v) - math.log10(lo)) / (math.log10(hi) - math.log10(lo))
        ticks = [10 ** k for k in range(int(round(math.log10(lo))), int(round(math.log10(hi))) + 1)]
    else:
        hi = vmax * 1.1 if vmax > 0 else 1.0
        def Y(v): return T + H - H * (v or 0) / hi
        step = 10 ** math.floor(math.log10(hi)) if hi > 0 else 1
        while hi / step > 6: step *= 2
        ticks = [k * step for k in range(0, int(hi / step) + 1)]
    o = ['<svg viewBox="0 0 %d %d" width="%d" style="max-width:100%%;height:auto;font:11px sans-serif">' % (width, height, width),
         '<text x="%d" y="16" font-weight="600">%s</text>' % (L, E2(title))]
    for t in ticks:
        y = Y(t)
        o.append('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#d0d7de"/>' % (L, L + W, y, y))
        o.append('<text x="%d" y="%.1f" text-anchor="end" fill="#57606a">%s</text>' % (L - 4, y + 4, ("%g" % t) + unit))
    n, k = len(labels), len(series)
    gw = W / max(n, 1)
    bw = gw * 0.8 / max(k, 1)
    for i, lab in enumerate(labels):
        for j, (name, vs) in enumerate(series):
            v = vs[i]
            x = L + i * gw + gw * 0.1 + j * bw
            y = Y(v)
            o.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s"><title>%s: %s</title></rect>'
                     % (x, y, bw - 1, T + H - y, colors[j], E2(name), E2(v)))
        o.append('<text x="%.1f" y="%d" text-anchor="end" transform="rotate(-30 %.1f %d)">%s</text>'
                 % (L + i * gw + gw / 2, T + H + 14, L + i * gw + gw / 2, T + H + 14, E2(lab)))
    for j, (name, _) in enumerate(series):
        o.append('<rect x="%d" y="%d" width="10" height="10" fill="%s"/><text x="%d" y="%d">%s</text>'
                 % (L + W - 150, 6 + 13 * j, colors[j], L + W - 136, 15 + 13 * j, E2(name)))
    o.append("</svg>")
    return "\n".join(o)


def svg_plot2d(title, segs=(), pts=(), circles=(), width=260, height=260, pad=0.08, note=""):
    """segs: [(x1,y1,x2,y2,color,widthpx)], pts: [(x,y,color,rpx,label)],
    circles: [(cx,cy,r,color)]; equal axes, fitted to everything"""
    E2 = lambda s: str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    xs = [s[0] for s in segs] + [s[2] for s in segs] + [p[0] for p in pts] + \
         [c[0] - c[2] for c in circles] + [c[0] + c[2] for c in circles]
    ys = [s[1] for s in segs] + [s[3] for s in segs] + [p[1] for p in pts] + \
         [c[1] - c[2] for c in circles] + [c[1] + c[2] for c in circles]
    if not xs:
        xs, ys = [0, 1], [0, 1]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    span = max(x1 - x0, y1 - y0) or 1.0
    cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
    T = 22
    s = (min(width, height - T) * (1 - 2 * pad)) / span
    def P(x, y):
        return width / 2 + (x - cx) * s, T + (height - T) / 2 - (y - cy) * s
    o = ['<svg viewBox="0 0 %d %d" width="%d" style="max-width:100%%;height:auto;font:10px sans-serif;background:#fff">'
         % (width, height, width), '<text x="4" y="14" font-weight="600">%s</text>' % E2(title)]
    for (x, y, r, col) in circles:
        X, Y = P(x, y)
        o.append('<circle cx="%.1f" cy="%.1f" r="%.1f" fill="none" stroke="%s" stroke-dasharray="3 2"/>' % (X, Y, r * s, col))
    for (xa, ya, xb, yb, col, w) in segs:
        A, B = P(xa, ya), P(xb, yb)
        o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="%s"/>' % (A[0], A[1], B[0], B[1], col, w))
    for p in pts:
        X, Y = P(p[0], p[1])
        o.append('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s"/>' % (X, Y, p[3], p[2]))
        if len(p) > 4 and p[4]:
            o.append('<text x="%.1f" y="%.1f">%s</text>' % (X + 4, Y - 3, E2(p[4])))
    if note:
        o.append('<text x="4" y="%d" fill="#57606a">%s</text>' % (height - 4, E2(note)))
    o.append("</svg>")
    return "\n".join(o)

KINDS3 = ["lattice + k 2^-m", "lattice 1/n", "cube corners (ties)", "UTM + tiny", "wild scales",
          "on a sphere", "random"]
KINDS2 = ["lattice + k 2^-m", "lattice 1/n", "integer ties", "UTM + tiny", "wild scales", "on a circle"]
ABOUT3 = [
 "Points on an integer lattice moved by -2..2 times a tiny power of two (2^-20 to 2^-52): the five points are "
 "almost on one sphere, and the in-sphere determinant can be 1e-36 of its size.",
 "Points on a lattice with spacing 1/n (n = 3 to 100): coordinates that are not exact in binary, many exact ties.",
 "Cube corners (0 or 1): every test is an exact tie or an exact orientation zero.",
 "Points 10 m apart at UTM-sized coordinates (400000, 3800000), moved by 1e-9 or 2^-30: large coordinates, "
 "tiny differences.",
 "Coordinates from 1e-30 to 1e30 in one test: differences that are not exact in double, the hardest case for "
 "double-double.",
 "Points on the unit sphere: near-ties from rounding.",
 "Random points: ordinary cases, decided in double precision."]
ABOUT2 = [ABOUT3[0].replace("sphere", "circle").replace("in-sphere", "in-circle").replace("1e-36", "1e-24"),
          ABOUT3[1], "Integer points 0..2: many exact ties and collinear points.",
          ABOUT3[3], ABOUT3[4], "Points on the unit circle: near-ties from rounding."]
# wrong answers of the predicates before the exact integer stage, on these
# same inputs (measured Nov 2026 with the double-double version)
OLD3 = [4, 0, 0, 0, 143, 0, 0]
OLD2 = [10, 2, 0, 4, 325, 0]


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
    sample = {}
    for i, (k, P) in enumerate(cases):
        s, o = int(out[2 * i]), int(out[2 * i + 1])
        ws, wo = want(P)
        st = stats[k]
        st[0] += 1
        st[1] += (s != ws) + (o != wo) > 0
        st[2] += ws == 0
        # the picture: a genuine tie (on the circle or sphere, not a flat
        # triangle or tet) if the kind has one
        good = ws == 0 and wo != 0
        if k not in sample or (good and not sample[k][2]):
            sample[k] = (P, ws, good)
    return stats, sample


def main(work, logdir):
    r3, s3 = run(os.path.join(work, "pred3d"), gen3, want3, KINDS3, 20000, 15, 101)
    r2, s2 = run(os.path.join(work, "pred2d"), gen2, want2, KINDS2, 40000, 8, 202)
    ok = all(v[1] == 0 for v in list(r3.values()) + list(r2.values()))
    rows = [("3D in-sphere / orient", KINDS3[k], v) for k, v in r3.items()] + \
           [("2D in-circle / orient", KINDS2[k], v) for k, v in r2.items()]
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        for t, k, v in rows:
            f.write("%s | %s | tests %d wrong %d ties %d\n" % (t, k, v[0], v[1], v[2]))
    for t, k, v in rows:
        print("  %-4s  %-22s %-22s tests %6d  wrong %d  exact ties %d" % ("PASS" if v[1] == 0 else "FAIL", t, k,
                                                                         v[0], v[1], v[2]))
    def circ2(P):
        ax, ay, bx, by, cx, cy = P[:6]
        d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
        if d == 0:
            return None
        ux = ((ax * ax + ay * ay) * (by - cy) + (bx * bx + by * by) * (cy - ay) + (cx * cx + cy * cy) * (ay - by)) / d
        uy = ((ax * ax + ay * ay) * (cx - bx) + (bx * bx + by * by) * (ax - cx) + (cx * cx + cy * cy) * (bx - ax)) / d
        return ux, uy, math.hypot(ax - ux, ay - uy)

    def pic(P, ws, dim, title):
        col = {1: "#cf222e", 0: "#1a7f37", -1: "#0969da"}[ws]
        word = {1: "inside", 0: "exactly on", -1: "outside"}[ws]
        if dim == 2:
            q = [(P[2 * i], P[2 * i + 1]) for i in range(4)]
            c = circ2(P)
        else:
            # 3D: an orthographic view along (1, 1, 1), in which the sphere's
            # outline is a circle of its own radius about its centre
            Q = [P[3 * i:3 * i + 3] for i in range(5)]
            def proj(v):
                return ((v[0] - v[1]) / math.sqrt(2), (v[0] + v[1] - 2 * v[2]) / math.sqrt(6))
            q = [proj(v) for v in Q]
            c = None
            A = [[Q[i][k] - Q[0][k] for k in range(3)] for i in (1, 2, 3)]
            rhs = [sum(x * x for x in A[i]) / 2 for i in range(3)]
            def det(m):
                return (m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1]) - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
                        + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]))
            D = det(A)
            if D != 0:
                cc = []
                for k in range(3):
                    M = [row[:] for row in A]
                    for i in range(3):
                        M[i][k] = rhs[i]
                    cc.append(det(M) / D)
                cen = [Q[0][k] + cc[k] for k in range(3)]
                c = proj(cen) + (math.sqrt(sum(x * x for x in cc)),)
        m = len(q) - 1
        if dim == 2:
            segs = [(q[i][0], q[i][1], q[(i + 1) % 3][0], q[(i + 1) % 3][1], "#57606a", 1) for i in range(3)]
        else:
            segs = [(q[i][0], q[i][1], q[j][0], q[j][1], "#57606a", 1) for i in range(4) for j in range(i + 1, 4)]
        pts = [(q[i][0], q[i][1], "#1f2328", 3, "abcd"[i]) for i in range(m)] + [(q[m][0], q[m][1], col, 4, "p")]
        circles = [(c[0], c[1], c[2], "#8250df")] if c and c[2] < 1e6 * (1 + max(abs(v) for v in P)) else []
        return svg_plot2d(title, segs, pts, circles, 180, 180,
                          note="p is %s the %s" % (word, "circle" if dim == 2 else "sphere"))

    o = ["""<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>exact predicates</title><style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1100px;
color:#1f2328;background:#fff}p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:13px}td,th{border-bottom:1px solid #d0d7de;
padding:4px 8px;text-align:right;vertical-align:top}td.l,th.l{text-align:left}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}details summary{cursor:pointer;color:#0969da;font-size:12px}.pics{display:flex;flex-wrap:wrap;gap:8px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">Exact predicates of connect against rational arithmetic</h1>
<p><b>What these tests do.</b> Connect builds a Delaunay mesh from two geometric questions: is a point left or right of a
line or plane (<i>orientation</i>), and is it inside, on or outside the circle or sphere through three or four others
(<i>in-circle</i>, <i>in-sphere</i>). Answered with ordinary floating point, these questions go wrong when the answer is
close to "on": the mesh then has edges that are not Delaunay, or tets that are inverted. src/lg_predicates2d.c and
src/lg_predicates3d.c answer them in stages: double precision with a bound on its rounding error, then double-double
(about 32 digits), then exact integer arithmetic (src/lg_exact_int.h). Here two small C programs feed them 40,000 2D and
20,000 3D test cases built to be hard, and every answer is compared with the same question in exact rational arithmetic
(Python fractions). <i>exact ties</i>: cases whose true answer is "on".</p>
<p>The chart shows the wrong answers before the exact integer stage (Nov 2026, the same inputs) and now. The pictures show
one case of each kind: a, b, c (and d in 3D, seen from above), the circle through them, and the test point p coloured by
the answer (red inside, green exactly on, blue outside). For the near-ties the difference is far too small to see: that is
the point of the test.</p>"""]
    o.append(svg_bars("Wrong answers per kind of input (out of about 2,800 (3D) or 6,600 (2D) each)",
                      ["3D " + k for k in KINDS3] + ["2D " + k for k in KINDS2],
                      [("before the exact integer stage", OLD3 + OLD2),
                       ("now", [r3[k][1] for k in range(len(KINDS3))] + [r2[k][1] for k in range(len(KINDS2))])],
                      ["#cf222e", "#1a7f37"], width=900, height=300))
    o.append("<p>Now: %d wrong in every kind.</p>" % sum(v[1] for v in list(r3.values()) + list(r2.values())))
    o.append("""<table><tr><th class="l">test</th><th class="l">inputs</th><th>tests</th><th>wrong</th><th>wrong before</th>
<th>exact ties</th><th>result</th><th class="l">one case</th></tr>""")
    for dim, R, S_, K, A_, OLD in ((3, r3, s3, KINDS3, ABOUT3, OLD3), (2, r2, s2, KINDS2, ABOUT2, OLD2)):
        for k, v in R.items():
            r = "PASS" if v[1] == 0 else "FAIL"
            t = "3D in-sphere / orient" if dim == 3 else "2D in-circle / orient"
            P, ws, _ = S_[k]
            o.append('<tr><td class="l">%s</td><td class="l">%s<details><summary>What it does</summary>%s</details></td>'
                     '<td>%d</td><td>%d</td><td>%d</td><td>%d</td><td class="%s">%s</td><td class="l">%s</td></tr>'
                     % (E(t), E(K[k]), E(A_[k]), v[0], v[1], OLD[k], v[2], r, r, pic(P, ws, dim, K[k])))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    print("\n%s" % ("all agree" if ok else "MISMATCHES"))
    return 0 if ok else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
