"""check_delaunay.py WORK LOGS

test_delaunay: judges each case of cases.txt against exact arithmetic
(../exact_check.py), draws a picture of each, writes summary.html in
this suite's folder. Python 3 standard library. Exit 1 on any FAIL.

For each case, with tol = the case's delaunay_tol (default 1e-10) and
fatal = its delaunay_fatal (default 1e-3, raised to tol if below):

  exit      1 if some facet has delta/L < -fatal (exactly computed),
            else 0
  counts    del_vor's report against the exact numbers: interior
            (and interface) facets checked, ties |delta/L| <= tol,
            violations, boundary facets checked, boundary
            circumcenters outside (s/L > tol)
  facets    the violations file lists exactly the facets the exact
            test flags, each with the right kind (1 violation, 2
            across an interface, 3 boundary circumcenter outside); no
            file when nothing is flagged
  values    each flagged facet's delta/L (or s/L) in the file within
            1e-7 of the exact value
  worst     the report's smallest delta/L within 1e-7 of the exact one
            (plus the 5 significant digits it is printed with)
  stor      (the two -stor cases) flip2d_stor writes no .stor;
            kuhn_stor writes one whose volumes add up to 36 (1e-12)
"""

import math
import os
import re
import sys
import xml.etree.ElementTree as ET

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(SUITE, 'test_voronoi_gold'))
from exact_check import (read_avs, analyze, counts, flagged,  # noqa
                         facet_key)
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, Projector  # noqa: E402
from check_voronoi_gold import read_stor  # noqa: E402

VAL_TOL = 1e-7

# report line -> count key
LABELS = [('interior faces checked', 'interior'),
          ('interior edges checked', 'interior'),
          ('interface faces checked', 'interface'),
          ('interface edges checked', 'interface'),
          ('ties (|delta/L| <= tol)', 'ties'),
          ('violations', 'violations'),
          ('beyond delaunay_fatal', 'fatal'),
          ('boundary faces checked', 'boundary'),
          ('boundary edges checked', 'boundary'),
          ('boundary circumcenter outside', 'boundary_out')]


def read_report(path):
    got = {'interface': 0}
    worst = None
    if not os.path.exists(path):
        return None, None
    for ln in open(path):
        s = ln.strip()
        for lab, key in LABELS:
            if s.startswith(lab) and s[len(lab):].strip().isdigit():
                got[key] = int(s[len(lab):])
        if s.startswith('smallest delta/L'):
            worst = float(s.split()[2])
    return got, worst


def read_vtu(path):
    """{sorted vertex tuple: (kind, value)}"""
    root = ET.parse(path).getroot()
    arr = {}
    for da in root.iter('DataArray'):
        arr[da.get('Name')] = [float(x) for x in da.text.split()]
    nv = int(arr['offsets'][0])
    vert = [int(x) for x in arr['vertex']]
    out = {}
    for k in range(len(arr['kind'])):
        key = tuple(sorted(vert[nv * k:nv * (k + 1)]))
        out[key] = (int(arr['kind'][k]), arr['delta_over_L'][k])
    return out


def fatal_of(extra, tol):
    m = re.search(r'-delaunay_fatal\s+(\S+)', extra)
    return max(float(m.group(1)) if m else 1e-3, tol)


def tol_of(extra):
    m = re.search(r'-delaunay_tol\s+(\S+)', extra)
    return float(m.group(1)) if m else 1e-10


def picture(path, title, mesh, r, flags):
    """The mesh (2D: every edge; 3D: the boundary faces' edges) and the
    flagged facets: red = not Delaunay, orange = boundary circumcenter
    outside. Small 2D meshes also show each triangle's circumcircle
    (dashed) and the vertex numbers."""
    W, H = 620, 560
    s = Svg(W, H)
    s.text(16, 22, title, size=13, bold=True)
    xyz = mesh.xyz
    flat = all(abs(p[2]) == 0.0 for p in xyz)
    small = mesh.nen == 3 and flat and len(mesh.cells) <= 8
    box = xyz
    if small:
        # leave room for the circumcircles
        lo = [min(p[q] for p in xyz) for q in range(2)]
        hi = [max(p[q] for p in xyz) for q in range(2)]
        pad = 0.35 * max(hi[0] - lo[0], hi[1] - lo[1])
        box = xyz + [(lo[0] - pad, lo[1] - pad, 0.0),
                     (hi[0] + pad, hi[1] + pad, 0.0)]
    pr = Projector(box, 20, 40, 580, 440,
                   view='plan' if flat else 'iso')
    if mesh.nen == 3:
        edges = {tuple(sorted((c[a], c[b]))) for c in mesh.cells
                 for a, b in ((0, 1), (1, 2), (0, 2))}
    else:
        edges = set()
        for k in r.boundary:
            f = [v - 1 for v in facet_key(k, mesh.nen)]
            for a, b in ((0, 1), (1, 2), (0, 2)):
                edges.add(tuple(sorted((f[a], f[b]))))
    for a, b in edges:
        x1, y1 = pr(xyz[a])
        x2, y2 = pr(xyz[b])
        s.line(x1, y1, x2, y2, color='#b8bec6', width=0.6)
    if mesh.nen == 3 and flat and len(mesh.cells) <= 8:
        # small 2D meshes: each triangle's circumcircle and the vertices
        for c in mesh.cells:
            (ax, ay, _), (bx, by, _), (cx, cy, _) = [xyz[v] for v in c]
            dd = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
            ux = ((ax * ax + ay * ay) * (by - cy) + (bx * bx + by * by)
                  * (cy - ay) + (cx * cx + cy * cy) * (ay - by)) / dd
            uy = ((ax * ax + ay * ay) * (cx - bx) + (bx * bx + by * by)
                  * (ax - cx) + (cx * cx + cy * cy) * (bx - ax)) / dd
            rad = math.dist((ux, uy), (ax, ay))
            px, py = pr((ux, uy, 0.0))
            rr = pr((ux + rad, uy, 0.0))[0] - px
            s.items.append('<circle cx="%.2f" cy="%.2f" r="%.2f" '
                           'fill="none" stroke="#2b6cb0" '
                           'stroke-width="1" stroke-dasharray="4,3"/>'
                           % (px, py, rr))
        for i, p in enumerate(xyz):
            x, y = pr(p)
            s.circle(x, y, 4, fill='#222')
            s.text(x + 6, y - 6, str(i + 1), size=11)
    nk = {1: 0, 2: 0, 3: 0}
    for key, kind in sorted(flags.items(), key=lambda t: -t[1]):
        nk[kind] += 1
        col = '#e67e22' if kind == 3 else '#c0392b'
        pts = [pr(xyz[v - 1]) for v in key]
        if len(pts) == 3:
            s.polygon(pts, fill=col, stroke=col, width=1.0, opacity=0.75)
        else:
            s.line(pts[0][0], pts[0][1], pts[1][0], pts[1][1], color=col,
                   width=3.5)
    y = H - 50
    s.line(20, y - 4, 50, y - 4, color='#c0392b', width=4)
    s.text(58, y, 'not Delaunay: %d' % (nk[1] + nk[2]), size=12)
    s.line(260, y - 4, 290, y - 4, color='#e67e22', width=4)
    s.text(298, y, 'boundary circumcenter outside: %d' % nk[3], size=12)
    s.text(20, y + 22, '%d vertices, %d %s' % (
        len(xyz), len(mesh.cells), 'triangles' if mesh.nen == 3
        else 'tets (boundary faces drawn)'), size=11, color='#555')
    s.save(path)


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows = []
    for name, mesh, extra, about in read_cases(
            os.path.join(HERE, 'cases.txt'), 4):
        d = os.path.join(work, name)
        m = os.path.basename(mesh)
        cmd = ('del_vor -avs %s -check_only on -violations %s.vtu '
               '-report %s.report %s' % (m, name, name, extra)).strip()
        checks = []

        def put(c, v, ok):
            checks.append((c, v, 'PASS' if ok else 'FAIL'))
            print('%s_%s %s %s' % (name, c, v, 'PASS' if ok else 'FAIL'))

        try:
            rc = int(open(os.path.join(d, 'run.rc')).read())
        except (OSError, ValueError):
            put('ran', 'no run.rc', False)
            rows.append((name, about, cmd, checks, None))
            continue
        tol = tol_of(extra)
        mm = read_avs(os.path.join(d, m))
        r = analyze(mm)
        fatal = fatal_of(extra, tol)
        want = counts(r, tol)
        want_flags = flagged(r, tol, mm.nen)
        signs = dict(r.interior)
        signs.update(r.interface)
        want['fatal'] = sum(1 for k, sg in signs.items()
                            if sg < 0 and r.dl[k] < -fatal)
        put('exit', '%d (expected %d)' % (rc, 1 if want['fatal'] else 0),
            rc == (1 if want['fatal'] else 0))
        out = ' '.join(open(os.path.join(d, 'run.out')).read().split()) \
            if os.path.exists(os.path.join(d, 'run.out')) else ''
        warn = want['violations'] > 0 and not want['fatal']
        put('warning', ('a WARNING, the run goes on' if warn else
                        'no warning') + (' (found)' if ('slightly not '
                                                         'Delaunay' in out)
                                         == warn else ' (NOT as expected)'),
            ('slightly not Delaunay' in out) == warn)
        got, worst = read_report(os.path.join(d, name + '.report'))
        if got is None:
            put('report', 'missing', False)
        else:
            keys = ['interior', 'interface', 'ties', 'violations', 'fatal',
                    'boundary', 'boundary_out']
            bad = [k for k in keys if got.get(k) != want[k]]
            put('counts', ', '.join('%s %s' % (k, got.get(k))
                                    for k in keys if want[k] or got.get(k))
                + ('' if not bad else '; exact: ' + ', '.join(
                    '%s %d' % (k, want[k]) for k in bad)), not bad)
            pairs = [k for k in list(r.interior) + list(r.interface)]
            ex_worst = min(r.dl[k] for k in pairs)
            if worst is not None:
                # the report prints 5 significant digits
                put('worst', 'smallest delta/L %.4e, exact %.4e'
                    % (worst, ex_worst), abs(worst - ex_worst)
                    <= VAL_TOL + 5e-5 * abs(ex_worst))
        vtu = os.path.join(d, name + '.vtu')
        if want_flags:
            if not os.path.exists(vtu):
                put('facets', 'no violations file', False)
            else:
                got_f = read_vtu(vtu)
                same = ({k: v[0] for k, v in got_f.items()} == want_flags)
                put('facets', '%d flagged (%d exact)' % (len(got_f),
                                                        len(want_flags)),
                    same)
                exact_val = {}
                for k, v in r.dl.items():
                    exact_val.setdefault(facet_key(k, mm.nen), v)
                dev = max(abs(v[1] - exact_val.get(k, math.inf))
                          for k, v in got_f.items())
                put('values', 'largest difference %.1e' % dev,
                    dev <= VAL_TOL)
        else:
            put('facets', 'nothing flagged, no file',
                not os.path.exists(vtu))
        if name == 'flip2d_stor':
            put('stor', 'flip2d.stor not written',
                not os.path.exists(os.path.join(d, 'flip2d.stor')))
        if name == 'kuhn_stor':
            f = os.path.join(d, 'kuhn.stor')
            if os.path.exists(f):
                v = sum(read_stor(f)['vol'])
                put('stor', 'volumes add up to %.15g' % v,
                    abs(v - 36.0) <= 36e-12)
            else:
                put('stor', 'kuhn.stor missing', False)
        pic = name + '.svg'
        picture(os.path.join(logs, pic), '%s: %s' % (name, m), mm, r,
                want_flags)
        rows.append((name, about, cmd, checks, pic))

    what = ('runs del_vor\'s Delaunay check (<code>check_only on</code>) '
            'on 16 cases: the 8 meshes of the other suites, which must '
            'pass, and meshes made by make_delaunay_inputs.py with '
            'facets broken on purpose (2-2 flips in 2D, 2-3 flips in '
            '3D), a hex grid cut into tets (co-spherical everywhere), a '
            'near-tie on each side of the tolerance, and a mesh at '
            'UTM-like coordinates. The answers are worked out here with '
            'exact integer arithmetic (<code>exact_check.py</code>), '
            'independently of del_vor: the exit status, every count in '
            'del_vor\'s report, and exactly which facets its violations '
            'file lists, with their values. Pictures: red = not '
            'Delaunay, orange = a boundary cell whose circumcenter lies '
            'outside (reported, not fatal).')
    nfail = write_case_page(os.path.dirname(os.path.abspath(logs)), logs,
                            'test_delaunay', what, rows)
    print('failures %d' % nfail)
    return 1 if nfail else 0


if __name__ == '__main__':
    sys.exit(main())
