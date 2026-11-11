"""check_policy.py WORK LOGS

test_policy: judges each case of cases.txt (run in WORK/NAME by
run_policy_tests.sh), draws each mesh, writes summary.html in this
suite's folder. Python 3 standard library. Exit 1 on any FAIL.

The answers come from this file's own dual geometry (DUAL below),
written from the definitions, not from del_vor's code:

  cell point   P = G + alpha (C - G): C the circumcenter, G the
               centroid; alpha 1 and 0 give C and G exactly
  face point   (tets) the same blend of the face's circumcenter and
               centroid, with alpha(f) = min over the face's two tets;
               a boundary face's weight also kept no larger than the
               one that puts its point on the face triangle (hybrid)
  piece        triangle: the dual segment from the edge midpoint m to
               P, as its signed distance from the edge (toward the
               opposite vertex positive); tet: the polygon
               m -> F(ijk) -> P -> F(ijl), its vector area, oriented
               about the edge by the right-hand rule from k to l
  coefficient  A/d = (sum of the pieces along the edge) / |edge|
  volume       triangle: piece |e| / 4 to each end; tet: piece |e| / 6

All geometry relative to each cell's first vertex (UTM-size meshes).

Checks, by the case name's prefix:
  all       exit status (1 for the hybrid on a non-Delaunay mesh,
            STOPS; else 0)
  med_ blend_ hyb_
            geometry   every A/d and volume in the .stor against DUAL
                       with the run's alpha (from its alpha file);
                       1e-11 of the largest
            negative   the negative coefficients in the .stor: none
                       (gate cases), else MEASURED; and equal to the
                       report's last row
            balance    the volumes add up to the mesh volume (area)
  med_      exact      each vertex volume = 1/3 (1/4) of its cells
  blend_    linear     2D and networks: = (Voronoi + median) / 2
  hyb_      stage A    alpha set by stage A = DUAL's closed form;
                       alpha 1 in cells stage A and B left alone; no
                       alpha below alpha_min
            interior   cells with no boundary face keep alpha 1, or
                       MEASURED how many stage B lowered and how far
                       (in cell layers) from the boundary
            closed     3D: the dual faces around each interior vertex
                       add up to zero (face points single-valued)
            same       nothing to fix: the .stor equals the Voronoi
                       one byte for byte (after the 2 header lines)
            utm        hyb_utm: alpha and A/d as in hyb_poi3, to 1e-6:
                       the moved coordinates are doubles near 3.2e6,
                       spaced 4.7e-10 m, on cells about 0.02 m across,
                       so the two meshes themselves differ at ~2e-8
            warning    hyb_rnd3d_min, _iter: the WARNING line
"""

import math
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(SUITE, 'test_voronoi_gold'))
from exact_check import read_avs  # noqa: E402
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, Projector, ramp  # noqa: E402
from check_voronoi_gold import read_stor, stor_pairs  # noqa: E402

TOL = 1e-11          # DUAL against del_vor, of the largest value
ZTOL = 1e-12         # del_vor's zero_tol default
# cases that must end with no negative coefficient
GATE = {'med_sq', 'med_rnd2d', 'med_dfn3', 'hyb_poi3', 'hyb_utm',
        'hyb_rnd2d', 'hyb_rnd3d', 'hyb_tri_dfn', 'hyb_sq', 'hyb_dfn3',
        'hyb_mesh3d'}
# hybrid cases whose .stor must be the Voronoi one
SAME = {'hyb_sq': 'vor_sq', 'hyb_dfn3': 'vor_dfn3',
        'hyb_mesh3d': 'vor_mesh3d'}
# the hybrid on meshes that are not Delaunay: must stop
STOPS = {'hyb_flip3d', 'hyb_hextet'}
# median on meshes that are not Delaunay: must go on
ADVISORY = {'med_flip3d', 'med_hextet'}
# 2D linearity: blend case -> (Voronoi case, median case)
LINEAR = {'blend_sq': ('vor_sq', 'med_sq'),
          'blend_dfn3': ('vor_dfn3', 'med_dfn3')}


# ---- vectors ---------------------------------------------------------

def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def add(a, b):
    return (a[0] + b[0], a[1] + b[1], a[2] + b[2])


def mul(s, a):
    return (s * a[0], s * a[1], s * a[2])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def norm(a):
    return math.sqrt(dot(a, a))


def det3(a, b, c):
    return dot(a, cross(b, c))


# ---- circumcenters by small linear systems ---------------------------

def tri_circ(a, b):
    """Triangle (0, a, b): circumcenter s a + t b from the Gram system;
    returns (center, barycentric coordinates of vertices 0, a, b)."""
    aa, ab, bb = dot(a, a), dot(a, b), dot(b, b)
    den = aa * bb - ab * ab
    s = 0.5 * (aa * bb - bb * ab) / den
    t = 0.5 * (bb * aa - aa * ab) / den
    return add(mul(s, a), mul(t, b)), (1.0 - s - t, s, t)


def tet_circ(a, b, c):
    """Tet (0, a, b, c): the center x with 2 a.x = |a|^2 etc. (Cramer);
    returns (center, barycentric coordinates of the 4 vertices)."""
    r = (0.5 * dot(a, a), 0.5 * dot(b, b), 0.5 * dot(c, c))
    D = det3(a, b, c)
    # rows a, b, c; solve for x
    x = (det3((r[0], a[1], a[2]), (r[1], b[1], b[2]), (r[2], c[1], c[2])),
         det3((a[0], r[0], a[2]), (b[0], r[1], b[2]), (c[0], r[2], c[2])),
         det3((a[0], a[1], r[0]), (b[0], b[1], r[1]), (c[0], c[1], r[2])))
    x = mul(1.0 / D, x)
    # x = u a + v b + w c
    u = det3(x, b, c) / D
    v = det3(a, x, c) / D
    w = det3(a, b, x) / D
    return x, (1.0 - u - v - w, u, v, w)


def blend(g, c, a):
    if a >= 1.0:
        return c
    if a <= 0.0:
        return g
    return add(g, mul(a, sub(c, g)))


# ---- DUAL: the whole calculation -------------------------------------

def topology(m):
    """Exterior facets per cell, and face -> cells (tets)."""
    hold = {}
    nen = m.nen
    for it, c in enumerate(m.cells):
        for k in range(nen):
            f = tuple(sorted(c[:k] + c[k + 1:]))
            key = (f, m.clr[it]) if nen == 3 else f
            hold.setdefault(key, []).append(it)
    ext = [[False] * nen for _ in m.cells]
    for it, c in enumerate(m.cells):
        for k in range(nen):
            f = tuple(sorted(c[:k] + c[k + 1:]))
            key = (f, m.clr[it]) if nen == 3 else f
            if len(hold[key]) == 1:
                ext[it][k] = True
    return ext, hold


def dual(m, alpha, clamp=False, amin=0.0):
    """Coefficients {(i,j): A/d}, volumes, vector areas {(i,j): V}
    (i < j, V oriented from i to j)."""
    ext, hold = topology(m)
    A, V = {}, {}
    vol = [0.0] * len(m.xyz)
    for it, c in enumerate(m.cells):
        x0 = m.xyz[c[0]]
        x = [sub(m.xyz[v], x0) for v in c]
        a = alpha[it]
        if m.nen == 3:
            g = mul(1.0 / 3.0, add(add(x[0], x[1]), x[2]))
            cc, _ = tri_circ(x[1], x[2]) if a > 0.0 else (g, None)
            p = blend(g, cc, a)
            nhat = cross(sub(x[1], x[0]), sub(x[2], x[0]))
            nhat = mul(1.0 / norm(nhat), nhat)
            for (i, j, k) in ((0, 1, 2), (0, 2, 1), (1, 2, 0)):
                mid = mul(0.5, add(x[i], x[j]))
                e = sub(x[j], x[i])
                le = norm(e)
                eh = mul(1.0 / le, e)
                u = sub(x[k], mid)
                u = sub(u, mul(dot(u, eh), eh))
                u = mul(1.0 / norm(u), u)
                piece = dot(sub(p, mid), u)
                w = cross(nhat, eh)
                sg = 1.0 if dot(w, u) > 0 else -1.0
                vec = mul(sg, cross(sub(p, mid), nhat))
                put(A, V, vol, c[i], c[j], piece, le, vec, 4.0)
        else:
            g = mul(0.25, add(add(x[0], x[1]), add(x[2], x[3])))
            if a > 0.0:
                cc, _ = tet_circ(x[1], x[2], x[3])
            else:
                cc = g
            p = blend(g, cc, a)
            fpt = []
            for k in range(4):
                o = [q for q in range(4) if q != k]
                fv = tuple(sorted(c[q] for q in o))
                cells = hold[fv]
                if len(cells) == 2:
                    af = min(alpha[cells[0]], alpha[cells[1]])
                else:
                    af = a
                y0 = x[o[0]]
                fg = mul(1.0 / 3.0, add(add(x[o[0]], x[o[1]]), x[o[2]]))
                fc, lam = tri_circ(sub(x[o[1]], y0), sub(x[o[2]], y0))
                fc = add(fc, y0)
                if len(cells) == 1 and clamp and af > 0.0 and \
                        min(lam) < 0.0:
                    b = 1.0 / 3.0
                    af = min(af, max(amin, b / (b - min(lam))))
                fpt.append(blend(fg, fc, af))
            for i in range(4):
                for j in range(i + 1, 4):
                    k, l = [q for q in range(4) if q not in (i, j)]
                    mid = mul(0.5, add(x[i], x[j]))
                    e = sub(x[j], x[i])
                    le = norm(e)
                    fa = fpt[l]          # face (i, j, k): opposite l
                    fb = fpt[k]          # face (i, j, l): opposite k
                    vec = mul(0.5, add(cross(sub(fa, mid), sub(p, mid)),
                                       cross(sub(p, mid), sub(fb, mid))))
                    if dot(cross(sub(x[k], mid), sub(x[l], mid)), e) < 0:
                        vec = mul(-1.0, vec)
                    piece = dot(vec, e) / le
                    put(A, V, vol, c[i], c[j], piece, le, vec, 6.0)
    return A, vol, V


def put(A, V, vol, gi, gj, piece, le, vec, den):
    key = (min(gi, gj), max(gi, gj))
    if gi > gj:
        vec = mul(-1.0, vec)
    A[key] = A.get(key, 0.0) + piece / le
    V[key] = add(V.get(key, (0.0, 0.0, 0.0)), vec)
    vol[gi] += piece * le / den
    vol[gj] += piece * le / den


def stage_a_alpha(m, amin=0.0):
    """The closed form of stage A, per cell (1 if nothing to do)."""
    ext, _ = topology(m)
    out = []
    for it, c in enumerate(m.cells):
        a = 1.0
        if any(ext[it]):
            x0 = m.xyz[c[0]]
            x = [sub(m.xyz[v], x0) for v in c]
            if m.nen == 3:
                _, lam = tri_circ(x[1], x[2])
                b = 1.0 / 3.0
            else:
                _, lam = tet_circ(x[1], x[2], x[3])
                b = 0.25
            for k in range(m.nen):
                if ext[it][k] and lam[k] < 0.0:
                    a = min(a, b / (b - lam[k]))
            a = max(a, amin)
        out.append(a)
    return out


def measure(m):
    """The mesh volume (area), summed exactly (math.fsum): a running
    sum of many equal cell volumes rounds the same way each time."""
    parts = []
    for c in m.cells:
        x = [m.xyz[v] for v in c]
        if m.nen == 3:
            parts.append(0.5 * norm(cross(sub(x[1], x[0]),
                                          sub(x[2], x[0]))))
        else:
            parts.append(abs(det3(sub(x[1], x[0]), sub(x[2], x[0]),
                                  sub(x[3], x[0]))) / 6.0)
    return math.fsum(parts)


# ---- files -----------------------------------------------------------

def read_alpha_vtu(path):
    """alpha and stage per cell from an alpha file."""
    with open(path) as f:
        t = f.read()

    def arr(name):
        mt = re.search(r'Name="%s"[^>]*>(.*?)</DataArray>' % name, t, re.S)
        return mt.group(1).split()
    return ([float(v) for v in arr('alpha')],
            [int(v) for v in arr('stage')])


def report_rows(path):
    """The negative-coefficient rows of the report: label -> (b, i)."""
    rows = {}
    if not os.path.exists(path):
        return rows
    for ln in open(path):
        mt = re.match(r'\s+(Voronoi|after stage A|after stage B|cv \w+)'
                      r'\s+(\d+)\s+(\d+)\s', ln)
        if mt:
            rows[mt.group(1)] = (int(mt.group(2)), int(mt.group(3)))
    return rows


def stor_coefs(path):
    st = read_stor(path)
    c = stor_pairs(st)
    return {(i, j): -v for (i, j), v in c.items() if i < j}, st['vol']


def layers(m, lowered):
    """For each cell, the number of face steps to a cell with an
    exterior facet (0 for those)."""
    ext, hold = topology(m)
    nb = [[] for _ in m.cells]
    for cells in hold.values():
        if len(cells) == 2:
            nb[cells[0]].append(cells[1])
            nb[cells[1]].append(cells[0])
    dist = [None] * len(m.cells)
    front = [it for it in range(len(m.cells)) if any(ext[it])]
    for it in front:
        dist[it] = 0
    d = 0
    while front:
        d += 1
        nxt = []
        for it in front:
            for jt in nb[it]:
                if dist[jt] is None:
                    dist[jt] = d
                    nxt.append(jt)
        front = nxt
    return max((dist[it] for it in lowered), default=0)


# ---- pictures --------------------------------------------------------

MATCOL = ['#4c72b0', '#dd8452', '#55a868', '#c44e52', '#8172b3',
          '#937860']


def picture(path, title, m, alpha, red, black, caption):
    """The mesh colored by alpha (2D and networks: every cell; tets:
    the boundary faces, plus a dot at the centroid of each interior
    cell with alpha < 1); red: negative Voronoi coefficients; black:
    negative coefficients left. When every cell has the same alpha
    (median, blend), the cells are colored by material instead."""
    W, H = 640, 600
    s = Svg(W, H)
    mats = sorted(set(m.clr))
    if alpha and len(set(alpha)) == 1:
        colors = [MATCOL[mats.index(c) % len(MATCOL)] for c in m.clr]
        legend = 'material'
    else:
        colors = [ramp(a) for a in alpha] if alpha else \
            ['#dddddd'] * len(m.cells)
        legend = 'alpha' if alpha else None
    s.text(16, 22, title, size=13, bold=True)
    view = 'plan' if all(abs(p[2]) < 1e-30 for p in m.xyz) else 'iso'
    pr = Projector(m.xyz, 20, 40, 600, 460, view=view)
    ext, hold = topology(m)
    if m.nen == 3:
        for it, c in enumerate(m.cells):
            s.polygon([pr(m.xyz[v]) for v in c], fill=colors[it],
                      stroke='#888', width=0.3, opacity=0.85)
    else:
        for it, c in enumerate(m.cells):
            for k in range(4):
                if ext[it][k]:
                    f = [c[q] for q in range(4) if q != k]
                    s.polygon([pr(m.xyz[v]) for v in f], fill=colors[it],
                              stroke='#999', width=0.2, opacity=0.35)
        if legend == 'alpha':
            for it, c in enumerate(m.cells):
                if alpha[it] < 1.0 and not any(ext[it]):
                    g = mul(0.25, add(add(m.xyz[c[0]], m.xyz[c[1]]),
                                      add(m.xyz[c[2]], m.xyz[c[3]])))
                    q = pr(g)
                    s.circle(q[0], q[1], 3.0, fill=ramp(alpha[it]),
                             stroke='#000', width=0.5)
    rw, ro = (1.6, 1.0) if len(red) < 300 else (0.5, 0.5)
    for (i, j) in red:
        a, b = pr(m.xyz[i]), pr(m.xyz[j])
        s.line(a[0], a[1], b[0], b[1], color='#c0392b', width=rw,
               opacity=ro)
    for (i, j) in black:
        a, b = pr(m.xyz[i]), pr(m.xyz[j])
        s.line(a[0], a[1], b[0], b[1], color='#000', width=2.6)
    y = H - 72
    if legend == 'alpha':
        s.legend(20, y, 200, 12, '0 (centroid)', '1 (circumcenter)',
                 'alpha per cell')
    elif legend == 'material':
        s.text(20, y - 5, 'material (itetclr); alpha %.3g everywhere'
               % alpha[0], size=11)
        for q, c in enumerate(mats[:6]):
            s.rect(20 + 40 * q, y, 14, 12, fill=MATCOL[q])
            s.text(38 + 40 * q, y + 10, str(c), size=11)
    s.line(260, y + 4, 290, y + 4, color='#c0392b', width=1.6)
    s.text(296, y + 8, 'negative with Voronoi: %d' % len(red), size=11)
    s.line(260, y + 22, 290, y + 22, color='#000', width=2.6)
    s.text(296, y + 26, 'negative in this run: %d' % len(black), size=11)
    s.text(20, H - 18, caption, size=11)
    s.save(path)


# ---- main ------------------------------------------------------------

def main():
    work, logs = sys.argv[1], sys.argv[2]
    cases = read_cases(os.path.join(HERE, 'cases.txt'), 4)
    rows = []
    meshes = {}
    vor_neg = {}
    results = {}

    def mesh_of(d, mesh):
        p = os.path.join(d, os.path.basename(mesh))
        if mesh not in meshes:
            meshes[mesh] = read_avs(p)
        return meshes[mesh]

    for name, mesh, args, about in cases:
        d = os.path.join(work, name)
        mname = os.path.basename(mesh)
        cmd = ('del_vor -avs %s -stor %s.stor -alpha_file %s.vtu '
               '-report %s.report %s' % (mname, name, name, name, args))
        checks = []

        def ok(c, v, good):
            res = good if isinstance(good, str) else \
                ('PASS' if good else 'FAIL')
            checks.append((c, v, res))
            print('%s_%s %s %s' % (name, c, v, res))

        rc = int(open(os.path.join(d, 'run.rc')).read())
        out = open(os.path.join(d, 'run.out')).read()
        m = mesh_of(d, mesh)
        pic = name + '.svg'
        stor = os.path.join(d, name + '.stor')
        if name in STOPS:
            ok('exit', '%d (expected 1)' % rc, rc == 1)
            ok('stopped', 'not Delaunay; no .stor',
               'not Delaunay' in out and not os.path.exists(stor))
            picture(os.path.join(logs, pic), name + ': stopped', m, None,
                    [], [], 'cv hybrid needs a Delaunay mesh')
            rows.append((name, about, cmd, checks, pic))
            continue
        ok('exit', '%d (expected 0)' % rc, rc == 0)
        if rc != 0 or not os.path.exists(stor):
            rows.append((name, about, cmd, checks, None))
            continue
        coef, vol = stor_coefs(stor)
        results[name] = (coef, vol)
        neg = sorted(k for k, v in coef.items() if v < 0.0)
        kind = name.split('_')[0]
        alpha, stage = read_alpha_vtu(os.path.join(d, name + '.vtu'))
        amin = 0.0
        mt = re.search(r'-alpha_min (\S+)', args)
        if mt:
            amin = float(mt.group(1))

        if kind == 'vor':
            vor_neg[mesh] = neg
            ok('negative', '%d negative coefficients' % len(neg),
               'MEASURED')
            picture(os.path.join(logs, pic), name + ': Voronoi', m, None,
                    neg, [], 'the reference run')
            rows.append((name, about, cmd, checks, pic))
            continue

        # every policy run: DUAL with the run's own alpha
        A, dvol, V = dual(m, alpha, clamp=(kind == 'hyb'), amin=amin)
        cmax = max(abs(v) for v in A.values())
        worst = 0.0
        missing_ok = True
        for k, v in A.items():
            if k in coef:
                worst = max(worst, abs(coef[k] - v))
            else:
                i, j = k
                le = norm(sub(m.xyz[j], m.xyz[i]))
                dd = le * le if m.nen == 4 else le
                if abs(v * le) > ZTOL * dd:
                    missing_ok = False
        vmax = max(abs(v) for v in dvol)
        vworst = max(abs(a - b) for a, b in zip(vol, dvol))
        ok('geometry', 'A/d %.1e, volumes %.1e of the largest; left out '
           'only zero-area' % (worst / cmax, vworst / vmax),
           worst <= TOL * cmax and vworst <= TOL * vmax and missing_ok)
        tot = measure(m)
        sv = math.fsum(vol)
        ok('balance', 'sum of volumes %.15g, mesh %.15g: %.1e'
           % (sv, tot, (sv - tot) / tot), abs(sv - tot) <= 1e-13 * tot)
        rep = report_rows(os.path.join(d, name + '.report'))
        last = rep.get('after stage B') or rep.get('cv ' + kind_cv(kind))
        nrep = sum(last) if last else -1
        if name in GATE:
            ok('negative', '%d negative coefficients (report %d)'
               % (len(neg), nrep), len(neg) == 0 and nrep == 0)
        else:
            ok('negative', '%d negative coefficients (report %d)'
               % (len(neg), nrep), 'MEASURED' if nrep == len(neg)
               else False)

        if kind == 'med':
            share = [0.0] * len(m.xyz)
            n = m.nen
            for c in m.cells:
                x = [m.xyz[v] for v in c]
                if n == 3:
                    a = 0.5 * norm(cross(sub(x[1], x[0]), sub(x[2], x[0])))
                else:
                    a = abs(det3(sub(x[1], x[0]), sub(x[2], x[0]),
                                 sub(x[3], x[0]))) / 6.0
                for v in c:
                    share[v] += a / n
            e = max(abs(a - b) for a, b in zip(vol, share))
            ok('exact', 'volumes = 1/%d of each cell: %.1e of the largest'
               % (n, e / vmax), e <= 1e-13 * vmax)
            if name in ADVISORY:
                ok('advisory', 'not Delaunay: goes on',
                   'does not need a Delaunay mesh' in out)

        if name in LINEAR:
            v_name, m_name = LINEAR[name]
            if v_name in results and m_name in results:
                cv, cm = results[v_name][0], results[m_name][0]
                e = max(abs(coef[k] - 0.5 * (cv[k] + cm[k]))
                        for k in coef if k in cv and k in cm)
                ok('linear', '|blend - (Voronoi + median)/2| %.1e of the '
                   'largest' % (e / cmax), e <= 1e-13 * cmax)

        if kind == 'hyb':
            sa = stage_a_alpha(m, amin)
            bad_a = sum(1 for it in range(len(alpha)) if stage[it] == 1
                        and abs(alpha[it] - sa[it]) > 1e-12)
            bad_1 = sum(1 for it in range(len(alpha)) if stage[it] == 0
                        and alpha[it] != 1.0)
            bad_min = sum(1 for a in alpha if a < amin)
            ok('stage A', '%d cells set by stage A, %d by stage B; '
               'mismatches %d, %d, below alpha_min %d'
               % (stage.count(1), stage.count(2), bad_a, bad_1, bad_min),
               bad_a == 0 and bad_1 == 0 and bad_min == 0)
            ext, _ = topology(m)
            inner = [it for it in range(len(alpha))
                     if alpha[it] < 1.0 and not any(ext[it])]
            if stage.count(2) == 0:
                ok('interior', 'alpha < 1 only in cells with a boundary '
                   'face (%d inner cells lowered)' % len(inner),
                   len(inner) == 0)
            else:
                ok('interior', '%d cells with no boundary face lowered by '
                   'stage B, at most %d cell layers in'
                   % (len(inner), layers(m, inner)), 'MEASURED')
            if m.nen == 4:
                bnd = set()
                for it, c in enumerate(m.cells):
                    for k in range(4):
                        if ext[it][k]:
                            bnd.update(c[q] for q in range(4) if q != k)
                sums = {}
                for (i, j), vec in V.items():
                    sums[i] = add(sums.get(i, (0.0, 0.0, 0.0)), vec)
                    sums[j] = add(sums.get(j, (0.0, 0.0, 0.0)),
                                  mul(-1.0, vec))
                amax = max(norm(v) for v in V.values())
                e = max(norm(s) for v, s in sums.items() if v not in bnd)
                ok('closed', 'sum of vector areas around interior vertices '
                   '%.1e of the largest' % (e / amax), e <= 1e-12 * amax)
            if name in SAME:
                ref = os.path.join(work, SAME[name], SAME[name] + '.stor')
                a = open(stor).read().split('\n')[2:]
                b = open(ref).read().split('\n')[2:]
                ok('same', 'the .stor equals %s.stor after the header'
                   % SAME[name], a == b and alpha.count(1.0) == len(alpha))
            if name == 'hyb_utm' and 'hyb_poi3' in results:
                c0 = results['hyb_poi3'][0]
                e = max(abs(coef[k] - c0[k]) for k in coef if k in c0)
                a0, _ = read_alpha_vtu(os.path.join(work, 'hyb_poi3',
                                                    'hyb_poi3.vtu'))
                ea = max(abs(a - b) for a, b in zip(alpha, a0))
                st0 = read_alpha_vtu(os.path.join(work, 'hyb_poi3',
                                                  'hyb_poi3.vtu'))[1]
                ok('utm', 'alpha within %.1e, A/d within %.1e of the '
                   'largest of hyb_poi3 (the coordinates differ at '
                   '~2e-8); same cells lowered' % (ea, e / cmax),
                   ea <= 1e-6 and e <= 1e-6 * cmax and st0 == stage)
            if name in ('hyb_rnd3d_min', 'hyb_rnd3d_iter'):
                ok('warning', 'WARNING line, files written',
                   'WARNING' in out)
        red = vor_neg.get(mesh, [])
        if mesh not in vor_neg:
            # no reference run on this mesh: count from DUAL at alpha 1
            if m.nen == 4 or kind == 'hyb':
                Av, _, _ = dual(m, [1.0] * len(m.cells))
                red = []
                for k, v in Av.items():
                    i, j = k
                    le = norm(sub(m.xyz[j], m.xyz[i]))
                    dd = le * le if m.nen == 4 else le
                    if v < 0 and abs(v * le) > ZTOL * dd:
                        red.append(k)
        cap = {'med': 'median', 'blend': 'blend, alpha 0.5',
               'hyb': 'adaptive hybrid'}[kind]
        picture(os.path.join(logs, pic), '%s: %s' % (name, cap), m, alpha,
                red, neg, '%s; alpha from %.3g to %.3g'
                % (os.path.basename(mesh), min(alpha), max(alpha)))
        rows.append((name, about, cmd, checks, pic))

    what = ('runs del_vor with each control-volume policy (cv median, '
            'blend and hybrid) and checks every coefficient and volume '
            'against this suite\'s own dual geometry, written from the '
            'definitions in Python (cell points blended between centroid '
            'and circumcenter, face points single-valued). The gate of '
            'phase 4: the adaptive hybrid leaves no negative coefficient '
            'on meshes with obtuse boundary cells (LaGriT\'s Poisson '
            'mesh, random points connected by LaGriT in a square and a '
            'cube, a fracture network), and keeps alpha = 1 away from '
            'the boundary. In the pictures, cells are colored by alpha; '
            'red edges are negative with Voronoi; black edges are still '
            'negative in that run.')
    nfail = write_case_page(HERE, logs, 'test_policy', what, rows)
    sys.exit(1 if nfail else 0)


def kind_cv(kind):
    return {'med': 'median', 'blend': 'blend', 'hyb': 'hybrid'}[kind]


if __name__ == '__main__':
    main()
