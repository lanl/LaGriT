#!/usr/bin/env python3
"""check_ic.py DECK WORKDIR

Judges one test_interpolate_compute deck from its "# check:" line:
    # check: KIND | OUTPUT.inp | ATTR[,ATTR] | ARG... [| skip_itp21]
against values computed here, independently of LaGriT, from the input
files written by make_inputs.py. Python 3, standard library only.

KIND
  voronoi   each ATTR at each sink vertex = the source attribute (ARG,
            on src_points.inp) of the nearest source vertex (brute
            force over all 300). Ties are reported, not judged.
  linear    each ATTR at each sink point = flin(x, y, z) =
            1 + 2x - 3y + 0.5z (continuous interpolation in a tet
            reproduces a linear field)
  map       cell ATTR of each sink cell = cval of the source cell (ARG
            = the source file) that contains the sink cell's centroid,
            found here geometrically (tri, tet: barycentric; quad, hex:
            the axis-aligned box; prism: its triangle in x-y and its
            z range). A centroid within 1e-9 of two cells is a tie, not
            judged.
  distance  ATTR = distance to the nearest of the 300 source points
  plane     |ATTR| = distance to the nearest VERTEX of plane_tri.inp
            (as COMPUTE.md defines it: "the minimum distance from any
            node in mo_source"), and the sign is the same for every
            sink vertex above the plane z = ARG and the opposite below
skip_itp21  vertices with itp1 = 21 (dudded) are not judged: interpolate
            skips them by design.
HARD: LaGriT finished; the output exists; every judged value within
TOL (1e-9, relative to max(1, |expected|)). Prints key value lines and
"result PASS|FAIL". A deck with a "# known:" line (a documented
failure) gives KNOWN when it fails as documented, FIXED (exit 1: update
the deck) when it passes.
"""
import math
import os
import sys

TOL = 1e-9


def read_avs(path):
    L = open(path).read().split("\n")
    h = L[0].split()
    nn, nc, nnd, ncd = int(h[0]), int(h[1]), int(h[2]), int(h[3])
    V = [tuple(float(x) for x in l.split()[1:4]) for l in L[1:1 + nn]]
    C = [[int(x) - 1 for x in l.split()[3:]] for l in L[1 + nn:1 + nn + nc]]
    k = 1 + nn + nc
    vatt, catt = {}, {}
    for count, store in ((nnd, vatt), (ncd, catt)):
        if not count:
            continue
        na = int(L[k].split()[0])
        names = [L[k + 1 + i].split(",")[0].strip() for i in range(na)]
        n = nn if store is vatt else nc
        rows = [l.split()[1:] for l in L[k + 1 + na:k + 1 + na + n]]
        for j, name in enumerate(names):
            store[name] = [float(r[j]) for r in rows]
        k += 1 + na + n
    return V, C, vatt, catt


def contains(ctype, P, p, eps=1e-9):
    """is p inside (or within eps of) the cell with vertices P"""
    def bary2(a, b, c, q):
        det = (b[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (b[1] - a[1])
        l1 = ((b[0] - q[0]) * (c[1] - q[1]) - (c[0] - q[0]) * (b[1] - q[1])) / det
        l2 = ((c[0] - q[0]) * (a[1] - q[1]) - (a[0] - q[0]) * (c[1] - q[1])) / det
        return min(l1, l2, 1 - l1 - l2) >= -eps
    def det3(u, v, w):
        return (u[0] * (v[1] * w[2] - v[2] * w[1]) - u[1] * (v[0] * w[2] - v[2] * w[0])
                + u[2] * (v[0] * w[1] - v[1] * w[0]))
    if ctype in ("quad", "hex"):
        return all(min(q[m] for q in P) - eps <= p[m] <= max(q[m] for q in P) + eps
                   for m in range(3 if ctype == "hex" else 2))
    if ctype == "tri":
        return bary2(P[0], P[1], P[2], p)
    if ctype in ("pri", "prism"):
        zs = [q[2] for q in P]
        return min(zs) - eps <= p[2] <= max(zs) + eps and bary2(P[0], P[1], P[2], p)
    if ctype == "tet":
        a = P[0]
        sub = lambda u, v: [u[m] - v[m] for m in range(3)]
        D = det3(sub(P[1], a), sub(P[2], a), sub(P[3], a))
        ls = []
        for k in range(4):
            Q = list(P)
            Q[k] = p
            ls.append(det3(sub(Q[1], Q[0]), sub(Q[2], Q[0]), sub(Q[3], Q[0])) / D)
        return min(ls) >= -eps
    raise ValueError(ctype)


def cell_types(path):
    L = open(path).read().split("\n")
    nn, nc = int(L[0].split()[0]), int(L[0].split()[1])
    return [l.split()[2] for l in L[1 + nn:1 + nn + nc]]


def flin(x, y, z):
    return 1.0 + 2.0 * x - 3.0 * y + 0.5 * z


def main(argv):
    deck, work = argv[1], argv[2]
    spec = None
    for l in open(deck):
        if l.startswith("# check:"):
            spec = [s.strip() for s in l.split(":", 1)[1].split("|")]
    if not spec:
        print("result FAIL\nwhy no '# check:' line")
        return 1
    kind, out, attrs = spec[0], spec[1], spec[2].split(",")
    args = [s for s in spec[3:] if s != "skip_itp21"]
    skip = "skip_itp21" in spec
    log = os.path.join(work, os.path.basename(deck)[:-4] + ".out")
    finished = os.path.isfile(log) and "LaGriT successfully completed" in open(log, errors="replace").read()
    path = os.path.join(work, out)
    if not os.path.isfile(path):
        print("finished %s\nresult FAIL\nwhy %s not written" % (finished, out))
        return 1
    V, C, va, ca = read_avs(path)
    judged, wrong, worst, ties, skipped, first = 0, 0, 0.0, 0, 0, None

    def judge(where, got, exp):
        nonlocal judged, wrong, worst, first
        judged += 1
        err = abs(got - exp) / max(1.0, abs(exp))
        worst = max(worst, err)
        if err > TOL:
            wrong += 1
            if first is None:
                first = "%s: got %.10g, expected %.10g" % (where, got, exp)

    itp = va.get("itp1", [0] * len(V))
    if kind in ("voronoi", "distance"):
        S, _, sa, _ = read_avs(os.path.join(work, "src_points.inp"))
        for i, p in enumerate(V):
            if skip and itp[i] == 21:
                skipped += 1
                continue
            d = sorted((math.dist(p, s), j) for j, s in enumerate(S))
            if d[1][0] - d[0][0] <= 1e-12 * max(1.0, d[0][0]):
                ties += 1
                continue
            for a in attrs:
                if kind == "voronoi":
                    judge("%s vertex %d" % (a, i + 1), va[a][i], sa[args[0]][d[0][1]])
                else:
                    judge("%s vertex %d" % (a, i + 1), va[a][i], d[0][0])
    elif kind == "linear":
        for i, p in enumerate(V):
            if skip and itp[i] == 21:
                skipped += 1
                continue
            for a in attrs:
                judge("%s point %d" % (a, i + 1), va[a][i], flin(*p))
    elif kind == "boundary":
        # expect = 1: on the source within rounding, the linear value;
        # expect = 0: outside, the flag value args[0]
        flag = float(args[0])
        for i, p in enumerate(V):
            for a in attrs:
                exp = flin(*p) if va["expect"][i] > 0.5 else flag
                judge("%s point %d (%s)" % (a, i + 1, "on" if va["expect"][i] > 0.5 else "outside"),
                      va[a][i], exp)
    elif kind == "equal":
        # two attributes made two ways must agree (args: none)
        a, b = attrs[0], attrs[1]
        for i, p in enumerate(V):
            judge("%s vs %s point %d" % (a, b, i + 1), va[a][i], va[b][i])
    elif kind == "map":
        SV, SC, _, sca = read_avs(os.path.join(work, args[0]))
        stype = cell_types(os.path.join(work, args[0]))
        for c, ids in enumerate(C):
            cen = [sum(V[q][m] for q in ids) / len(ids) for m in range(3)]
            hit = [s for s, sids in enumerate(SC)
                   if contains(stype[s], [SV[q] for q in sids], cen)]
            if len(hit) != 1:
                ties += 1
                continue
            for a in attrs:
                judge("%s cell %d" % (a, c + 1), ca[a][c], sca["cval"][hit[0]])
    elif kind == "plane":
        z0 = float(args[0])
        P, _, _, _ = read_avs(os.path.join(work, "plane_tri.inp"))
        signs = {}
        for i, p in enumerate(V):
            for a in attrs:
                v = va[a][i]
                judge("|%s| vertex %d" % (a, i + 1), abs(v), min(math.dist(p, q) for q in P))
                if abs(p[2] - z0) > 1e-9:
                    side = "above" if p[2] > z0 else "below"
                    signs.setdefault(side, set()).add(v > 0)
        mixed = [s for s, v in signs.items() if len(v) > 1]
        same = len(signs) == 2 and signs["above"] == signs["below"]
        if mixed or same:
            wrong += 1
            first = first or ("sign not consistent: above %s, below %s"
                              % (sorted(signs.get("above", [])), sorted(signs.get("below", []))))
        print("sign_above %s" % ("+" if True in signs.get("above", ()) else "-"))
    else:
        print("result FAIL\nwhy unknown check kind %s" % kind)
        return 1
    ok = finished and judged > 0 and wrong == 0
    print("finished %s" % finished)
    print("judged %d" % judged)
    print("wrong %d" % wrong)
    print("worst_rel_error %.3g" % worst)
    print("ties_not_judged %d" % ties)
    print("dudded_not_judged %d" % skipped)
    if first:
        print("first_wrong %s" % first)
    known = None
    for l in open(deck):
        if l.startswith("# known:"):
            known = l.split(":", 1)[1].strip()
    if known:
        print("known %s" % known)
        print("result %s" % ("FIXED" if ok else "KNOWN"))
        if ok:
            print("why passes now: remove its '# known' lines")
            return 1
        return 0
    print("result %s" % ("PASS" if ok else "FAIL"))
    if not ok:
        why = []
        if not finished:
            why.append("LaGriT did not finish")
        if wrong:
            why.append("%d wrong values (%s)" % (wrong, first))
        if not judged:
            why.append("nothing judged")
        print("why %s" % "; ".join(why))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
