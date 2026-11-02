#!/usr/bin/env python3
"""check_improve.py WORKDIR LOGDIR

Judges the test_improve_tets decks (Python 3, standard library only) from
the meshes each deck writes before (<tag>_before.inp) and after
(<tag>_after.inp) improve_tets, and LaGriT's output (<deck>.out).

Always (except it06):
  finished     LaGriT printed "LaGriT successfully completed"
  ran          improve_tets printed its report
  progress     it printed its progress (the pass table and "improve_tets:
               finished"), or none of it for a deck with progress / off
  report       its resolution report (vertex move / h median and largest;
               h after / h before at 1%, median, 99%; h = mean length of
               the edges at a vertex) agrees with the same quantities
               computed here from the meshes (0.002)
  valid        every tet has positive volume (AVS order: det < 0)
  volume       total volume unchanged (relative 1e-9)
  count        the vertex count is unchanged
  box          every boundary face lies on a face of the unit box, and
               the counts of vertices on box faces / edges / corners are
               unchanged
The deck's "# check:" line adds:
  nopoor            no tet with a dihedral angle <= 10 or >= 170 degrees
  resolution        the local spacing (mean length of the edges at a
                    vertex) compared with the input mesh at the same
                    place: median within 0.9 - 1.1
  gradient_x_0.5    the mean edge length in each band of distance from the
                    plane x = 0.5 (10 bands of 0.05) within 5% of the input
  boundary_fixed    no vertex on the box moved
  pset_z_below_0.5  vertices at z >= 0.5 did not move, and the tets with
                    a vertex there are unchanged
  materials         the volume of each itetclr value is unchanged
  messages          (it06) each refused command printed its message and
                    the mesh is unchanged
Measured and reported: smallest / largest dihedral angle and smallest /
mean min sine before and after, LaGriT's improve_tets report.
Writes LOGDIR/<deck>.check and LOGDIR/summary.html.
"""
import html
import math
import os
import sys

EXPECTED_MESSAGES = [
    "improve_tets: cell          1 is not a tet",
    "improve_tets: unknown keyword bogus",
    "improve_tets: need 0 < target <= 0.95",
    "improve_tets: token    3 should be a keyword followed by a value",
    "improve_tets: bad value high for target",
    "improve_tets: bad value maybe for smooth",
]


def sub(a, b):
    return [a[i] - b[i] for i in range(3)]


def cross(a, b):
    return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def norm(a):
    return math.sqrt(dot(a, a))


def read_avs(path):
    L = open(path).read().split("\n")
    nn, ne = (int(x) for x in L[0].split()[:2])
    V = [tuple(float(x) for x in L[1 + i].split()[1:4]) for i in range(nn)]
    C, col, typ = [], [], []
    for i in range(ne):
        w = L[1 + nn + i].split()
        col.append(int(w[1]))
        typ.append(w[2])
        C.append([int(x) - 1 for x in w[3:]])
    return V, C, col, typ


def vol6(P):
    """6 x volume, positive for a valid tet in LaGriT's AVS order"""
    return -dot(sub(P[1], P[0]), cross(sub(P[2], P[0]), sub(P[3], P[0])))


def angles(P):
    """smallest and largest dihedral angle (degrees), min sine"""
    F = [(1, 2, 3), (0, 2, 3), (0, 1, 3), (0, 1, 2)]
    n = []
    for i, f in enumerate(F):
        m = cross(sub(P[f[1]], P[f[0]]), sub(P[f[2]], P[f[0]]))
        if dot(m, sub(P[i], P[f[0]])) > 0:
            m = [-x for x in m]
        L = norm(m)
        n.append([x / L for x in m] if L else [0, 0, 0])
    a = []
    for f1, f2 in ((2, 3), (1, 3), (1, 2), (0, 3), (0, 2), (0, 1)):
        c = max(-1.0, min(1.0, -dot(n[f1], n[f2])))
        a.append(math.degrees(math.acos(c)))
    return min(a), max(a), min(math.sin(math.radians(min(a))), math.sin(math.radians(max(a))))


def on_box(p):
    return sum(1 for c in p if abs(c) < 1e-9 or abs(c - 1) < 1e-9)


def local_h(V, C):
    s = [0.0] * len(V)
    n = [0] * len(V)
    seen = set()
    for c in C:
        for a in range(4):
            for b in range(a + 1, 4):
                e = (min(c[a], c[b]), max(c[a], c[b]))
                if e in seen:
                    continue
                seen.add(e)
                L = math.dist(V[e[0]], V[e[1]])
                for k in e:
                    s[k] += L
                    n[k] += 1
    return [s[i] / n[i] if n[i] else 0.0 for i in range(len(V))]


def nearest_index(V0):
    cell = 0.05
    grid = {}
    for i, p in enumerate(V0):
        grid.setdefault(tuple(int(c / cell) for c in p), []).append(i)

    def near(p):
        g = [int(c / cell) for c in p]
        best = (1e30, -1)
        for r in range(1, 8):
            for dx in range(-r, r + 1):
                for dy in range(-r, r + 1):
                    for dz in range(-r, r + 1):
                        for i in grid.get((g[0] + dx, g[1] + dy, g[2] + dz), ()):
                            d = math.dist(p, V0[i])
                            if d < best[0]:
                                best = (d, i)
            if best[1] >= 0 and best[0] < (r - 1) * cell:
                break
        return best[1]
    return near


def stats(V, C):
    mn, mx, ms = 180.0, 0.0, []
    poor = 0
    for c in C:
        a, b, s = angles([V[k] for k in c])
        mn, mx = min(mn, a), max(mx, b)
        ms.append(s)
        poor += a <= 10 or b >= 170
    return {"min": mn, "max": mx, "poor": poor, "minsine": min(ms), "meansine": sum(ms) / len(ms)}


def check(deck, work):
    name = deck[:-4]
    tag = name.split("_")[0]
    text = open(os.path.join(work, name + ".out"), errors="replace").read()
    checks = []
    for l in open(deck):
        if l.startswith("# check:"):
            checks = l.split(":", 1)[1].split()
    bad, rows = [], []
    if "LaGriT successfully completed" not in text:
        bad.append("LaGriT did not finish")
        return bad, rows, {}
    V0, C0, col0, _ = read_avs(os.path.join(work, tag + "_before.inp"))
    V1, C1, col1, _ = read_avs(os.path.join(work, tag + "_after.inp"))
    if "messages" in checks:
        for m in EXPECTED_MESSAGES:
            ok = m in text
            rows.append(("message", m, ok))
            if not ok:
                bad.append("missing message: " + m)
        same = V0 == V1 and C0 == C1
        rows.append(("mesh unchanged", "after the refused commands", same))
        if not same:
            bad.append("the mesh changed")
        return bad, rows, {}
    ran = "improve_tets: tet quality" in text
    rows.append(("ran", "improve_tets printed its report", ran))
    if not ran:
        bad.append("improve_tets did not run")
    quiet = any(l.startswith("improve_tets") and "progress / off" in l for l in open(deck))
    shown = "  pass  target   worst" in text and "improve_tets: finished," in text
    ok = (not shown and "improve_tets: finished," not in text) if quiet else shown
    rows.append(("progress", "none (progress / off)" if quiet else "pass table and finish line", ok))
    if not ok:
        bad.append("progress lines wrong")
    neg = sum(1 for c in C1 if vol6([V1[k] for k in c]) <= 0)
    rows.append(("valid", "%d tets, %d not positive" % (len(C1), neg), neg == 0))
    if neg:
        bad.append("%d tets with zero or negative volume" % neg)
    v0 = sum(vol6([V0[k] for k in c]) for c in C0) / 6
    v1 = sum(vol6([V1[k] for k in c]) for c in C1) / 6
    ok = abs(v1 - v0) <= 1e-9 * abs(v0)
    rows.append(("volume", "%.12g -> %.12g" % (v0, v1), ok))
    if not ok:
        bad.append("volume changed")
    ok = len(V1) == len(V0)
    rows.append(("count", "%d -> %d vertices" % (len(V0), len(V1)), ok))
    if not ok:
        bad.append("vertex count changed")
    faces = {}
    for c in C1:
        for k in range(4):
            f = tuple(sorted(c[:k] + c[k + 1:]))
            faces[f] = faces.get(f, 0) + 1
    off = sum(1 for f, n in faces.items() if n == 1 and not any(
        all(abs(V1[q][i] - val) < 1e-9 for q in f) for i in range(3) for val in (0, 1)))
    cnt0 = [sum(1 for p in V0 if on_box(p) == k) for k in (1, 2, 3)]
    cnt1 = [sum(1 for p in V1 if on_box(p) == k) for k in (1, 2, 3)]
    ok = off == 0 and cnt0 == cnt1
    rows.append(("box", "boundary faces off the box %d; face/edge/corner vertices %s -> %s" % (off, cnt0, cnt1), ok))
    if not ok:
        bad.append("box not kept")
    s0, s1 = stats(V0, C0), stats(V1, C1)
    rows.append(("angles", "min %.2f -> %.2f, max %.2f -> %.2f; poor %d -> %d" % (
        s0["min"], s1["min"], s0["max"], s1["max"], s0["poor"], s1["poor"]), True))
    rows.append(("min sine", "worst %.3f -> %.3f, mean %.3f -> %.3f" % (
        s0["minsine"], s1["minsine"], s0["meansine"], s1["meansine"]), True))
    if "nopoor" in checks:
        ok = s1["poor"] == 0
        rows.append(("nopoor", "%d tets at or beyond 10 / 170 degrees" % s1["poor"], ok))
        if not ok:
            bad.append("%d poor tets left" % s1["poor"])
    if "resolution" in checks:
        h0, h1 = local_h(V0, C0), local_h(V1, C1)
        near = nearest_index(V0)
        rat = sorted(h1[i] / h0[near(p)] for i, p in enumerate(V1))
        med = rat[len(rat) // 2]
        ok = 0.9 <= med <= 1.1
        rows.append(("resolution", "local spacing after / before: 1%% %.2f, median %.2f, 99%% %.2f" % (
            rat[len(rat) // 100], med, rat[(99 * len(rat)) // 100]), ok))
        if not ok:
            bad.append("local spacing median %.2f" % med)
    if "gradient_x_0.5" in checks:
        def profile(V, C):
            bins = {}
            for c in C:
                for a in range(4):
                    for b in range(a + 1, 4):
                        p, q = V[c[a]], V[c[b]]
                        k = min(9, int(abs((p[0] + q[0]) / 2 - 0.5) / 0.05))
                        bins.setdefault(k, []).append(math.dist(p, q))
            return [sum(bins[k]) / len(bins[k]) for k in sorted(bins)]
        g0, g1 = profile(V0, C0), profile(V1, C1)
        worst = max(abs(b / a - 1) for a, b in zip(g0, g1))
        ok = len(g0) == len(g1) and worst <= 0.05
        rows.append(("gradient", "mean edge length by distance from x = 0.5: before %s; after %s; largest change %.1f%%" % (
            " ".join("%.3f" % x for x in g0), " ".join("%.3f" % x for x in g1), 100 * worst), ok))
        if not ok:
            bad.append("the gradient changed by %.1f%%" % (100 * worst))
    if "boundary_fixed" in checks:
        moved = sum(1 for p, q in zip(V0, V1) if on_box(p) and p != q)
        ok = moved == 0
        rows.append(("boundary_fixed", "%d boundary vertices moved" % moved, ok))
        if not ok:
            bad.append("boundary vertices moved")
    if "pset_z_below_0.5" in checks:
        moved = sum(1 for p, q in zip(V0, V1) if p[2] >= 0.5 and p != q)
        out0 = sorted(tuple(sorted(c)) for c in C0 if any(V0[k][2] >= 0.5 for k in c))
        have = set(tuple(sorted(c)) for c in C1)
        lost = sum(1 for c in out0 if c not in have)
        ok = moved == 0 and lost == 0
        rows.append(("pset", "%d vertices outside the pset moved; %d of %d tets with such a vertex changed" % (
            moved, lost, len(out0)), ok))
        if not ok:
            bad.append("outside the pset changed")
    if "materials" in checks:
        def per(V, C, col):
            d = {}
            for c, k in zip(C, col):
                d[k] = d.get(k, 0.0) + vol6([V[q] for q in c]) / 6
            return d
        m0, m1 = per(V0, C0, col0), per(V1, C1, col1)
        ok = set(m0) == set(m1) and all(abs(m0[k] - m1[k]) <= 1e-9 for k in m0)
        rows.append(("materials", "; ".join("itetclr %d: %.10f -> %.10f" % (k, m0[k], m1.get(k, 0))
                                             for k in sorted(m0)), ok))
        if not ok:
            bad.append("a material volume changed")
    rep = [l.strip() for l in text.splitlines() if l.startswith("  ") and (
        "worst" in l or "dihedral angles" in l or "tets below" in l or "vertex moves" in l or
        "vertices free" in l or l.strip().startswith("tets ") or "resolution (h" in l or
        "vertex move / h" in l or "h after / h before" in l)]
    # the command's resolution report against the same quantities computed here
    # (h = mean length of the edges at a vertex, each edge counted once per tet)
    def mean_edge(V, C):
        sm, n = [0.0] * len(V), [0] * len(V)
        for c in C:
            for a, b in ((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)):
                L = math.dist(V[c[a]], V[c[b]])
                for k in (c[a], c[b]):
                    sm[k] += L
                    n[k] += 1
        return [sm[i] / n[i] if n[i] else 0.0 for i in range(len(V))]
    rep_nums = {}
    for l in rep:
        w = l.replace("%", " ").split()
        if l.startswith("vertex move / h"):
            rep_nums["move_med"], rep_nums["move_max"] = float(w[5]), float(w[7])
        if l.startswith("h after / h before"):
            rep_nums["h_p01"], rep_nums["h_med"], rep_nums["h_p99"] = float(w[6]), float(w[8]), float(w[10])
    if len(rep_nums) == 5 and len(V0) == len(V1):
        h0, h1 = mean_edge(V0, C0), mean_edge(V1, C1)
        mv = sorted(math.dist(V0[i], V1[i]) / h0[i] for i in range(len(V0)) if h0[i] > 0 and h1[i] > 0)
        rt = sorted(h1[i] / h0[i] for i in range(len(V0)) if h0[i] > 0 and h1[i] > 0)
        n = len(rt)
        mine = {"move_med": mv[n // 2], "move_max": mv[-1], "h_p01": rt[n // 100], "h_med": rt[n // 2],
                "h_p99": rt[min(n - 1, (99 * n) // 100)]}
        diff = max(abs(mine[k] - rep_nums[k]) for k in mine)
        ok = diff <= 0.002
        rows.append(("report", "resolution report (move median / largest; h after/before 1%% / median / 99%%): "
                     "LaGriT %s, recomputed %s" % (
                         " ".join("%.3f" % rep_nums[k] for k in ("move_med", "move_max", "h_p01", "h_med", "h_p99")),
                         " ".join("%.3f" % mine[k] for k in ("move_med", "move_max", "h_p01", "h_med", "h_p99"))), ok))
        if not ok:
            bad.append("resolution report differs from the meshes by %.3f" % diff)
    elif "messages" not in checks:
        rows.append(("report", "no resolution report found", False))
        bad.append("no resolution report")
    return bad, rows, {"report": rep, "before": s0, "after": s1}


def main(work, logdir):
    decks = sorted(f for f in os.listdir(".") if f.startswith("it") and f.endswith(".lgi"))
    page, nfail = [], 0
    for d in decks:
        try:
            bad, rows, extra = check(d, work)
        except (OSError, ValueError, IndexError) as e:
            bad, rows, extra = ["output not readable (%s)" % e], [], {}
        res = "FAIL" if bad else "PASS"
        nfail += bool(bad)
        with open(os.path.join(logdir, d[:-4] + ".check"), "w") as f:
            f.write("result %s\n" % res)
            for b in bad:
                f.write("why %s\n" % b)
            for r in rows:
                f.write("row %s | %s | %s\n" % (r[0], r[1], "ok" if r[2] else "WRONG"))
            for l in extra.get("report", []):
                f.write("lagrit %s\n" % l)
        print("%-24s %s%s" % (d[:-4], res, "" if not bad else "  (" + "; ".join(bad) + ")"))
        desc = " ".join(l[2:].strip() for l in open(d) if l.startswith("# ") and not l.startswith("# check:"))
        page.append((d[:-4], res, desc.split(".lgi", 1)[-1].strip(), bad, rows, extra, open(d).read()))
    write_page(os.path.join(logdir, "summary.html"), page)
    return 1 if nfail else 0


def write_page(path, page):
    e = html.escape
    h = ["<!doctype html><html><head><meta charset='utf-8'><title>test_improve_tets</title>",
         "<style>body{font-family:sans-serif;margin:16px;max-width:1100px}table{border-collapse:collapse;"
         "margin:8px 0}td,th{border:1px solid #bbb;padding:3px 7px;font-size:13px;text-align:left}"
         ".PASS{color:#060;font-weight:bold}.FAIL{color:#b00;font-weight:bold}pre{background:#f4f4f4;"
         "padding:8px;font-size:12px}</style></head><body><h1>test_improve_tets</h1>",
         "<p>The command <code>improve_tets</code> on 3D Poisson disk meshes: quality, validity, the box, "
         "the resolution, fixed boundary, pset, materials, refused commands. Run: <code>sh "
         "run_improve_tests.sh [path-to-lagrit]</code>.</p><table><tr><th>deck</th><th>result</th>"
         "<th>what</th></tr>"]
    for d, res, desc, bad, rows, extra, deck in page:
        h.append("<tr><td><a href='#%s'>%s</a></td><td class='%s'>%s</td><td>%s</td></tr>" % (d, d, res, res, e(desc)))
    h.append("</table>")
    for d, res, desc, bad, rows, extra, deck in page:
        h.append("<h2 id='%s'>%s <span class='%s'>%s</span></h2><p>%s</p>" % (d, d, res, res, e(desc)))
        if bad:
            h.append("<ul>%s</ul>" % "".join("<li>%s</li>" % e(b) for b in bad))
        h.append("<table>%s</table>" % "".join("<tr><td>%s</td><td>%s</td><td class='%s'>%s</td></tr>" % (
            e(r[0]), e(r[1]), "PASS" if r[2] else "FAIL", "ok" if r[2] else "WRONG") for r in rows))
        if extra.get("report"):
            h.append("<pre>%s</pre>" % e("\n".join(extra["report"])))
        h.append("<details><summary>%s.lgi</summary><pre>%s</pre></details>" % (d, e(deck)))
    h.append("</body></html>")
    open(path, "w").write("\n".join(h))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2]))
