#!/usr/bin/env python3
"""check_angles.py WORKDIR LOGDIR

Checks LaGriT's element angle and quality attributes (Python 3,
standard library only) against values computed here independently:

  cmo/addatt ... ang_mind ang_minr ang_maxd ang_maxr   dihedral angles
                 (between faces of 3D elements, between edges of 2D
                 elements), degrees / radians
  cmo/addatt ... ang_mind_solid ang_minr_solid ang_maxd_solid
                 ang_maxr_solid                         solid angles at
                 the vertices of a tet, in steradians (r) and in
                 steradians x 180/pi (d; LaGriT's "degrees": a full
                 sphere is 720, a flat vertex 360, a regular tet 31.59)
  quality / aspect / y       aratio   tet 3 r_in / R_circ, tri
                             2 r_in / R_circ, quad and hex shortest /
                             longest diagonal (1 is best); 0 for other
                             element types
  quality / edge_ratio / y   eratio   shortest / longest edge
  quality / edge_min / y     edgemin  shortest edge
  quality / edge_max / y     edgemax  longest edge
  cmo/addatt ... volume      vol

Reference: dihedral angle from the two other vertices projected onto
the plane normal to the edge; solid angle by Van Oosterom and Strackee
(1983); circumradius from the circumcentre formula; inradius 3 V / area.

Every deck (ea*.lgi) dumps WORKDIR/<name>_out.inp. A deck PASSES when
LaGriT finished and every check below holds:
  ea_tets   every attribute within 1e-6 (relative, or absolute for
            values below 1) of the reference, on all 213 tets
  ea_thin   ang_mind < 1 degree, ang_maxd > 179, 0 <= s_mind <= s_maxd
            < 1 (no angle lost to rounding)
  ea_solid  hex, prism, pyramid: known dihedral angles, edge ratio,
            edges, aspect (hex 1, others 0); solid angles: LaGriT's
            special value for non-tets (99999 rad, -99999 rad)
  ea_2d     triangle and quad: known angles, edge ratio, edges, aspect
  all       degrees = radians x 180 / pi for every pair; min <= max;
            eratio = edgemin / edgemax
Writes LOGDIR/<name>.check and LOGDIR/summary.html.
"""
import html
import math
import os
import sys

TOL = 1e-6
DEG = 180.0 / math.pi
EDGES = [(0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)]
TRI_FACES = [(1, 2, 3), (0, 2, 3), (0, 1, 3), (0, 1, 2)]


def sub(a, b):
    return [a[i] - b[i] for i in range(3)]


def cross(a, b):
    return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def norm(a):
    return math.sqrt(dot(a, a))


def angle(a, b):
    c = dot(a, b) / (norm(a) * norm(b))
    return math.acos(max(-1.0, min(1.0, c)))


def read_avs(path):
    """vertices, element vertex lists (0-based), element types and the
    element attributes {name: [values]} of an AVS file LaGriT wrote"""
    L = open(path).read().split("\n")
    nn, ne, nva, nea, _ = (int(x) for x in L[0].split())
    V = [tuple(float(x) for x in L[1 + i].split()[1:4]) for i in range(nn)]
    C, typ = [], []
    for i in range(ne):
        w = L[1 + nn + i].split()
        typ.append(w[2])
        C.append([int(x) - 1 for x in w[3:]])
    k = 1 + nn + ne
    if nva:
        k += 1 + int(L[k].split()[0]) + nn
    A = {}
    if nea:
        nh = int(L[k].split()[0])
        names = [L[k + 1 + j].split(",")[0].strip() for j in range(nh)]
        A = {n: [] for n in names}
        for i in range(ne):
            for n, x in zip(names, L[k + 1 + nh + i].split()[1:]):
                A[n].append(float(x))
    return V, C, typ, A


def tet_reference(P):
    dih = []
    for a, b in EDGES:
        o = [k for k in range(4) if k not in (a, b)]
        e = sub(P[b], P[a])
        ee = dot(e, e)

        def perp(w):
            return sub(w, [e[i] * dot(w, e) / ee for i in range(3)])
        dih.append(angle(perp(sub(P[o[0]], P[a])), perp(sub(P[o[1]], P[a]))))
    sol = []
    for k in range(4):
        a, b, c = [sub(P[q], P[k]) for q in range(4) if q != k]
        la, lb, lc = norm(a), norm(b), norm(c)
        num = abs(dot(a, cross(b, c)))
        den = la * lb * lc + dot(a, b) * lc + dot(a, c) * lb + dot(b, c) * la
        sol.append(2.0 * math.atan2(num, den))
    el = [norm(sub(P[a], P[b])) for a, b in EDGES]
    a, b, c = sub(P[1], P[0]), sub(P[2], P[0]), sub(P[3], P[0])
    six_v = dot(a, cross(b, c))
    area = sum(norm(cross(sub(P[f[1]], P[f[0]]), sub(P[f[2]], P[f[0]]))) / 2 for f in TRI_FACES)
    if six_v != 0:
        bc, ca, ab = cross(b, c), cross(c, a), cross(a, b)
        ctr = [(dot(a, a) * bc[i] + dot(b, b) * ca[i] + dot(c, c) * ab[i]) / six_v / 2
               for i in range(3)]
        asp = 3.0 * (abs(six_v) / 2.0 / area) / norm(ctr)
    else:
        asp = 0.0
    return {"ang_minr": min(dih), "ang_maxr": max(dih),
            "ang_mind": min(dih) * DEG, "ang_maxd": max(dih) * DEG,
            "s_minr": min(sol), "s_maxr": max(sol),
            "s_mind": min(sol) * DEG, "s_maxd": max(sol) * DEG,
            "aratio": asp, "eratio": min(el) / max(el), "edgemin": min(el),
            "edgemax": max(el), "vol": -six_v / 6.0}


def close(x, y):
    return abs(x - y) <= TOL * max(1.0, abs(x))


def consistency(A, i):
    """degrees = radians x 180/pi, min <= max, eratio = edgemin / edgemax"""
    bad = []
    for d, r in (("ang_mind", "ang_minr"), ("ang_maxd", "ang_maxr"),
                 ("s_mind", "s_minr"), ("s_maxd", "s_maxr")):
        if not close(A[r][i] * DEG, A[d][i]):
            bad.append("%s %.10g is not %s %.10g x 180/pi" % (d, A[d][i], r, A[r][i]))
    if A["ang_minr"][i] > A["ang_maxr"][i]:
        bad.append("ang_minr %.6g > ang_maxr %.6g" % (A["ang_minr"][i], A["ang_maxr"][i]))
    if A["edgemax"][i] > 0 and not close(A["edgemin"][i] / A["edgemax"][i], A["eratio"][i]):
        bad.append("eratio %.10g is not edgemin / edgemax" % A["eratio"][i])
    return bad


NAMES = ["ang_mind", "ang_minr", "ang_maxd", "ang_maxr", "s_mind", "s_minr",
         "s_maxd", "s_maxr", "aratio", "eratio", "edgemin", "edgemax"]

# known values of the deck ea_solid and ea_2d (in AVS order of the file)
R2, R3, R5 = math.sqrt(2), math.sqrt(3), math.sqrt(5)
KNOWN = {
    "ea_solid": [
        ("hex", {"ang_mind": 90, "ang_maxd": 90, "aratio": 1, "eratio": 1,
                 "edgemin": 1, "edgemax": 1}),
        ("prism", {"ang_mind": 45, "ang_maxd": 90, "aratio": 0, "eratio": 1 / R2,
                   "edgemin": 1, "edgemax": R2}),
        ("pyramid", {"ang_mind": 45, "ang_maxd": 120, "aratio": 0, "eratio": R3 / 2,
                     "edgemin": R3 / 2, "edgemax": 1})],
    "ea_2d": [
        ("triangle", {"ang_mind": math.degrees(math.atan(0.5)), "ang_maxd": 90,
                      "aratio": 2 * (2 / (3 + R5)) / (R5 / 2), "eratio": 1 / R5,
                      "edgemin": 1, "edgemax": R5}),
        ("quad", {"ang_mind": 45, "ang_maxd": 135, "aratio": R2 / R5, "eratio": 0.5,
                  "edgemin": 1, "edgemax": 2})]}


def check(name, work):
    """(list of failures, rows for the page)"""
    tag = name.split("_", 1)[1]
    tag = "ea_" + tag
    V, C, typ, A = read_avs(os.path.join(work, tag + "_out.inp"))
    names_file = os.path.join(work, tag + ".names")
    labels = open(names_file).read().split() if os.path.isfile(names_file) else []
    bad, rows = [], []
    for i in range(len(C)):
        for b in consistency(A, i):
            bad.append("%s: %s" % (labels[i] if labels else i + 1, b))
    if tag == "ea_tets":
        worst = {}
        for i, c in enumerate(C):
            ref = tet_reference([V[k] for k in c])
            for n, x in ref.items():
                y = A[n][i]
                err = abs(x - y) / max(1.0, abs(x))
                if n not in worst or err > worst[n][0]:
                    worst[n] = (err, labels[i], x, y)
                if err > TOL:
                    bad.append("%s %s: reference %.10g, LaGriT %.10g" % (labels[i], n, x, y))
        for n in NAMES + ["vol"]:
            e, t, x, y = worst[n]
            rows.append((n, "largest difference %.1e" % e, "%s: reference %.10g, LaGriT %.10g"
                         % (t, x, y), e <= TOL))
    elif tag == "ea_thin":
        for i in range(len(C)):
            a = {n: A[n][i] for n in NAMES}
            ok = (0 <= a["ang_mind"] < 1 and 179 < a["ang_maxd"] <= 180
                  and 0 <= a["s_mind"] <= a["s_maxd"] < 1)
            if not ok:
                bad.append("h = %s: ang_mind %.4g, ang_maxd %.4g, s_mind %.4g, s_maxd %.4g"
                           % (labels[i], a["ang_mind"], a["ang_maxd"], a["s_mind"], a["s_maxd"]))
            rows.append(("h = %s" % labels[i], "ang_mind %.3g  ang_maxd %.6g" % (a["ang_mind"], a["ang_maxd"]),
                         "s_mind %.3g  s_maxd %.3g" % (a["s_mind"], a["s_maxd"]), ok))
    else:
        for i, (what, want) in enumerate(KNOWN[tag]):
            got = []
            ok = True
            for n, x in want.items():
                y = A[n][i]
                if not close(x, y):
                    ok = False
                    bad.append("%s %s: expected %.10g, LaGriT %.10g" % (what, n, x, y))
                got.append("%s %.6g" % (n, y))
            if tag == "ea_solid":
                for n, x in (("s_minr", 99999.0), ("s_maxr", -99999.0)):
                    if not close(x, A[n][i]):
                        ok = False
                        bad.append("%s %s: expected the special value %g, LaGriT %.10g"
                                   % (what, n, x, A[n][i]))
                got.append("s_minr %g s_maxr %g" % (A["s_minr"][i], A["s_maxr"][i]))
            rows.append((what, typ[i], "  ".join(got), ok))
    return bad, rows


def main(work, logdir):
    decks = sorted(f[:-4] for f in os.listdir(".") if f.startswith("ea") and f.endswith(".lgi"))
    page = []
    nfail = 0
    for d in decks:
        out = os.path.join(work, d + ".out")
        text = open(out, errors="replace").read() if os.path.isfile(out) else ""
        bad = []
        rows = []
        if "LaGriT successfully completed" not in text:
            bad.append("LaGriT did not finish")
        else:
            try:
                bad, rows = check(d, work)
            except (OSError, ValueError, IndexError, KeyError) as e:
                bad.append("output not readable (%s)" % e)
        res = "FAIL" if bad else "PASS"
        nfail += bool(bad)
        with open(os.path.join(logdir, d + ".check"), "w") as f:
            f.write("result %s\n" % res)
            for b in bad:
                f.write("why %s\n" % b)
            for r in rows:
                f.write("row %s | %s | %s | %s\n" % (r[0], r[1], r[2], "ok" if r[3] else "WRONG"))
        print("%-12s %s%s" % (d, res, "" if not bad else "  (%d: %s)" % (len(bad), bad[0])))
        desc = [l[2:].rstrip() for l in open(d + ".lgi") if l.startswith("# ") and not
                l.startswith("# check:")][1:]
        page.append((d, res, " ".join(desc), bad, rows, open(d + ".lgi").read()))
    write_page(os.path.join(logdir, "summary.html"), page)
    return 1 if nfail else 0


def write_page(path, page):
    e = html.escape
    h = ["<!doctype html><html><head><meta charset='utf-8'><title>test_element_angles</title>",
         "<style>body{font-family:sans-serif;margin:16px;max-width:1100px}"
         "table{border-collapse:collapse;margin:8px 0}td,th{border:1px solid #bbb;padding:3px 7px;"
         "font-size:13px;text-align:left}.PASS{color:#060;font-weight:bold}.FAIL{color:#b00;"
         "font-weight:bold}pre{background:#f4f4f4;padding:8px;font-size:12px}</style></head><body>",
         "<h1>test_element_angles</h1><p>LaGriT's dihedral and solid angle attributes "
         "(cmo/addatt ang_*) and quality aspect / edge_ratio / edge_min / edge_max, checked "
         "against an independent computation (check_angles.py). Run: <code>sh "
         "run_angle_tests.sh [path-to-lagrit]</code>.</p>",
         "<table><tr><th>deck</th><th>result</th><th>what</th></tr>"]
    for d, res, desc, bad, rows, deck in page:
        h.append("<tr><td><a href='#%s'>%s</a></td><td class='%s'>%s</td><td>%s</td></tr>"
                 % (d, d, res, res, e(desc)))
    h.append("</table>")
    for d, res, desc, bad, rows, deck in page:
        h.append("<h2 id='%s'>%s <span class='%s'>%s</span></h2><p>%s</p>" % (d, d, res, res, e(desc)))
        if bad:
            h.append("<p><b>Failures</b> (%d):</p><ul>%s</ul>" % (len(bad), "".join(
                "<li>%s</li>" % e(b) for b in bad[:40])))
        if rows:
            h.append("<table>")
            for r in rows:
                h.append("<tr><td>%s</td><td>%s</td><td>%s</td><td class='%s'>%s</td></tr>"
                         % (e(r[0]), e(r[1]), e(r[2]), "PASS" if r[3] else "FAIL",
                            "ok" if r[3] else "WRONG"))
            h.append("</table>")
        h.append("<details><summary>%s.lgi</summary><pre>%s</pre></details>" % (d, e(deck)))
    h.append("</body></html>")
    open(path, "w").write("\n".join(h))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2]))
