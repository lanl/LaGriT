"""check_kernels.py WORK LOGS

test_kernels: reads del_vor_selftest's output (WORK/selftest.out) and
sample cells (WORK/selftest_samples.txt), checks that every expected
check ran and passed, draws the pictures in LOGS and writes
summary.html in this suite's folder. Python 3 standard library.
Exit status 1 on any FAIL.

The measures are defined at the top of test/del_vor_selftest.F90 and
in 00README. Lines ending in PASS or FAIL are HARD; MEASURED lines are
reported, not judged. A check missing from the output is a FAIL.
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
from svgdraw import Svg, ramp, Projector  # noqa: E402

EXPECTED = [
    'cot2d_max_rel_diff', 'circ3d_max_rel_diff', 'dual3d_max_rel_diff',
    'perp3d_max_rel', 'part2d_max_rel_diff', 'part3d_max_rel_diff',
    'shift_max_rel_diff', 'closure_mesh_2D.inp_max_rel',
    'closure_mesh_3D.inp_max_rel', 'closure_simple_3D.inp_max_rel',
]

WHAT = {
    'cot2d_max_rel_diff': 'triangle A/d against cot(opposite angle)/2',
    'circ3d_max_rel_diff': 'tet circumcenter equally far from 4 vertices',
    'dual3d_max_rel_diff': 'tet A/d against the right-triangle formula',
    'perp3d_max_rel': 'tet dual pieces perpendicular to their edges',
    'p1_3d_max_rel_diff': 'tet A/d against the P1 stiffness (differ in 3D)',
    'part2d_max_rel_diff': 'vertex shares add to the triangle area, any '
                           'cell point',
    'part3d_max_rel_diff': 'vertex shares add to the tet volume, any cell '
                           'and face points',
    'shift_max_rel_diff': 'same tet at the origin and at UTM-like '
                          'coordinates',
    'shift_voronoi_det4x4_rel_diff': 'the same with lanl/voronoi\'s '
                                     'circumcenter',
    'shift_det4x4_center_error': 'lanl/voronoi circumcenter error '
                                 '(cell size 1)',
}


def what(key):
    if key in WHAT:
        return WHAT[key]
    if key.startswith('closure_') and key.endswith('_max_rel'):
        return 'net vector area around inner vertices (closed cells)'
    if key.startswith('closure_') and key.endswith('_inner_vertices'):
        return 'vertices not on the bounding box'
    return ''


def sub(a, b):
    return [a[k] - b[k] for k in range(3)]


def dot(a, b):
    return sum(a[k] * b[k] for k in range(3))


def cross(a, b):
    return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0]]


def unit(a):
    n = math.sqrt(dot(a, a))
    return [v / n for v in a]


def read_out(path):
    rows = []
    with open(path) as f:
        for ln in f:
            t = ln.split()
            if len(t) >= 3 and t[-1] in ('PASS', 'FAIL', 'MEASURED'):
                tol = t[3] if t[-1] != 'MEASURED' else ''
                rows.append((t[0], t[1], tol, t[-1]))
            elif len(t) == 2:
                rows.append((t[0], t[1], '', 'INFO'))
    return rows


def read_samples(path):
    tri = tet = None
    clos = []
    with open(path) as f:
        for ln in f:
            t = ln.split()
            if not t:
                continue
            if t[0] == 'tri':
                tri = [float(v) for v in t[1:]]
            elif t[0] == 'tet':
                tet = [float(v) for v in t[1:]]
            elif t[0] == 'closure':
                clos.append((t[1], int(t[2]),
                             [float(v) for v in t[3:6]], float(t[6])))
    return tri, tet, clos


def pic_tri(path, tri):
    """The obtuse sample triangle in its own plane: vertices, the
    circumcenter (outside), the dual pieces from the edge midpoints to
    it, and per edge A/d from the kernel and cot(angle)/2."""
    x = [tri[0:3], tri[3:6], tri[6:9]]
    pc = tri[9:12]
    got, want = tri[12:15], tri[15:18]
    e1 = unit(sub(x[1], x[0]))
    nn = unit(cross(sub(x[1], x[0]), sub(x[2], x[0])))
    e2 = cross(nn, e1)

    def flat(p):
        q = sub(p, x[0])
        return (dot(q, e1), dot(q, e2), 0.0)

    pts = [flat(p) for p in x] + [flat(pc)]
    W, H = 640, 520
    s = Svg(W, H)
    s.text(20, 24, 'cot2d: an obtuse triangle (in its own plane); '
           'dual pieces to the circumcenter', size=14, bold=True)
    pr = Projector(pts, 60, 50, 520, 330, view='plan')
    s.polygon([pr(p) for p in pts[:3]], fill='#eef2f7', stroke='#666',
              width=1.5)
    edges = [(0, 1), (0, 2), (1, 2)]
    for ie, (a, b) in enumerate(edges):
        m = [0.5 * (pts[a][k] + pts[b][k]) for k in range(3)]
        x1, y1 = pr(m)
        x2, y2 = pr(pts[3])
        col = '#c0392b' if want[ie] < 0 else '#2b6cb0'
        s.line(x1, y1, x2, y2, color=col, width=2.2)
        s.text(x1, y1 + 16, 'edge %d%d' % (a + 1, b + 1), size=11,
               anchor='middle', color='#444')
    for k in range(3):
        px, py = pr(pts[k])
        s.circle(px, py, 5, fill='#222')
        s.text(px + 7, py - 7, str(k + 1), size=13, bold=True)
    cx, cy = pr(pts[3])
    s.circle(cx, cy, 5, fill='white', stroke='#c0392b', width=2)
    s.text(cx + 8, cy + 4, 'circumcenter', size=11, color='#c0392b')
    y = 410
    s.text(40, y, 'edge', size=12, bold=True)
    s.text(140, y, 'A/d (kernel)', size=12, bold=True)
    s.text(300, y, 'cot(opposite angle)/2', size=12, bold=True)
    s.text(500, y, 'difference', size=12, bold=True)
    for ie, (a, b) in enumerate(edges):
        y += 20
        s.text(40, y, '%d-%d' % (a + 1, b + 1), size=12)
        s.text(140, y, '%.12f' % got[ie], size=12)
        s.text(300, y, '%.12f' % want[ie], size=12)
        s.text(500, y, '%.1e' % abs(got[ie] - want[ie]), size=12)
    s.text(20, H - 14, 'Red: the negative piece of the edge opposite the '
           'obtuse angle (circumcenter on the far side of it).', size=11)
    s.save(path)


def pic_tet(path, tet):
    """The sample tet (the first with its circumcenter well inside,
    every barycentric coordinate > 0.1): the 6
    dual pieces (each a quadrilateral: edge midpoint, face
    circumcenter, tet circumcenter, face circumcenter), the A/d of each
    from the kernel, from the right-triangle formula, and, for
    comparison only, the P1 stiffness."""
    x = [tet[3 * k:3 * k + 3] for k in range(4)]
    pc = tet[12:15]
    pf = [tet[15 + 3 * k:18 + 3 * k] for k in range(4)]
    got, want, p1 = tet[27:33], tet[33:39], tet[39:45]
    edges = [(0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)]
    W, H = 680, 600
    s = Svg(W, H)
    s.text(20, 24, 'dual3d: the sample tet and the dual pieces of its 6 '
           'edges', size=14, bold=True)
    pr = Projector(x + [pc] + pf, 60, 50, 560, 360, view='iso')
    for ie, (a, b) in enumerate(edges):
        c, d = [k for k in range(4) if k not in (a, b)]
        m = [0.5 * (x[a][k] + x[b][k]) for k in range(3)]
        quad = [m, pf[d], pc, pf[c]]
        s.polygon([pr(p) for p in quad], fill=ramp(ie / 5.0),
                  stroke='#333', width=0.6, opacity=0.45)
    for a, b in edges:
        x1, y1 = pr(x[a])
        x2, y2 = pr(x[b])
        s.line(x1, y1, x2, y2, color='#222', width=1.6)
    for k in range(4):
        px, py = pr(x[k])
        s.circle(px, py, 5, fill='#222')
        s.text(px + 7, py - 7, str(k + 1), size=13, bold=True)
    cx, cy = pr(pc)
    s.circle(cx, cy, 4, fill='white', stroke='#c0392b', width=2)
    s.text(cx + 7, cy + 4, 'C', size=12, color='#c0392b')
    y = 440
    s.text(40, y, 'edge', size=12, bold=True)
    s.text(100, y, 'A/d (kernel)', size=12, bold=True)
    s.text(230, y, 'right-triangle formula', size=12, bold=True)
    s.text(420, y, 'difference', size=12, bold=True)
    s.text(520, y, 'P1 stiffness', size=12, bold=True)
    for ie, (a, b) in enumerate(edges):
        y += 20
        s.rect(40, y - 11, 12, 12, fill=ramp(ie / 5.0))
        s.text(58, y, '%d-%d' % (a + 1, b + 1), size=12)
        s.text(100, y, '%.10f' % got[ie], size=12)
        s.text(230, y, '%.10f' % want[ie], size=12)
        s.text(420, y, '%.1e' % abs(got[ie] - want[ie]), size=12)
        s.text(520, y, '%.6f' % p1[ie], size=12, color='#888')
    s.text(20, H - 14, 'P1 stiffness for comparison only: in 3D it is '
           'not the Voronoi coefficient (in 2D it is).', size=11)
    s.save(path)


def pic_shift(path, vals):
    """Relative difference of A/d between a tet at the origin and the
    same tet at (7e5, 3.2e6, 1.5e3): del_vor against lanl/voronoi's
    circumcenter, log scale."""
    W, H = 640, 300
    s = Svg(W, H)
    s.text(20, 24, 'shift: same tets at the origin and at (7e5, 3.2e6, '
           '1.5e3), cells of size 1', size=14, bold=True)
    x0, x1 = 200, 600
    lo, hi = -17, 5
    for p in range(lo, hi + 1, 2):
        x = x0 + (p - lo) / (hi - lo) * (x1 - x0)
        s.line(x, 60, x, 200, color='#ddd')
        s.text(x, 216, '1e%d' % p, size=10, anchor='middle')
    bars = [('del_vor', vals.get('shift_max_rel_diff'), '#2b6cb0'),
            ('lanl/voronoi det4x4', vals.get('shift_voronoi_det4x4_rel_diff'),
             '#c0392b')]
    for k, (lab, v, col) in enumerate(bars):
        y = 80 + 55 * k
        s.text(x0 - 10, y + 18, lab, size=12, anchor='end')
        if v is None or v <= 0:
            s.text(x0 + 4, y + 18, 'missing', size=12)
            continue
        p = max(lo, min(hi, math.log10(v)))
        w = (p - lo) / (hi - lo) * (x1 - x0)
        s.rect(x0, y, w, 26, fill=col)
        s.text(x0 + w + 6, y + 18, '%.2e' % v, size=12)
    s.text(20, 250, 'Largest |A/d difference| / largest |A/d| over 2000 '
           'random tets. del_vor is limited by the rounding of the', size=11)
    s.text(20, 266, 'coordinates themselves (3.2e6 x 2.2e-16 = 7e-10); '
           'voronoi\'s 4x4 determinants lose every digit.', size=11)
    s.save(path)


def pic_closure(path, mesh_path, clos):
    """mesh_3D wireframe; each inner vertex colored by the size of its
    dual cell's net vector area (log scale, 1e-17 to 1e-10)."""
    with open(mesh_path) as f:
        lines = [ln for ln in f if ln.strip()]
    nn, nc = int(lines[0].split()[0]), int(lines[0].split()[1])
    xyz = [[float(v) for v in ln.split()[1:4]] for ln in lines[1:1 + nn]]
    cells = [[int(v) - 1 for v in ln.split()[3:]]
             for ln in lines[1 + nn:1 + nn + nc]]
    W, H = 680, 640
    s = Svg(W, H)
    s.text(20, 24, 'closure on mesh_3D: net vector area of each inner '
           'dual cell', size=14, bold=True)
    pr = Projector(xyz, 40, 50, 600, 500, view='iso')
    seen = set()
    for c in cells:
        for i in range(4):
            for j in range(i + 1, 4):
                e = (min(c[i], c[j]), max(c[i], c[j]))
                if e in seen:
                    continue
                seen.add(e)
                x1, y1 = pr(xyz[e[0]])
                x2, y2 = pr(xyz[e[1]])
                s.line(x1, y1, x2, y2, color='#ccd', width=0.7)
    lo, hi = -17.0, -10.0
    n = 0
    for name, i, p, err in clos:
        if name != 'mesh_3D.inp':
            continue
        n += 1
        x, y = pr(p)
        t = (math.log10(max(err, 1e-17)) - lo) / (hi - lo)
        s.circle(x, y, 6, fill=ramp(t), stroke='#222', width=0.8)
    s.legend(40, H - 58, 260, 12, '1e-17', '1e-10 (tolerance)',
             'relative net vector area at the %d inner vertices' % n)
    s.save(path)


def main():
    work, logs = sys.argv[1], sys.argv[2]
    nfail = 0
    out = os.path.join(work, 'selftest.out')
    rows = read_out(out) if os.path.exists(out) else []
    seen = {r[0] for r in rows}
    table = []
    for key, val, tol, res in rows:
        if res == 'FAIL':
            nfail += 1
        table.append((key, val, tol, res))
        print('%s %s %s' % (key, val, res))
    for key in EXPECTED:
        if key not in seen:
            nfail += 1
            table.append((key, 'missing', '', 'FAIL'))
            print('%s missing FAIL' % key)
    vals = {}
    for key, val, tol, res in rows:
        try:
            vals[key] = float(val)
        except ValueError:
            pass

    pics = []
    sp = os.path.join(work, 'selftest_samples.txt')
    if os.path.exists(sp):
        tri, tet, clos = read_samples(sp)
        if tri:
            pic_tri(os.path.join(logs, 'cot2d.svg'), tri)
            pics.append('cot2d.svg')
        if tet:
            pic_tet(os.path.join(logs, 'dual3d.svg'), tet)
            pics.append('dual3d.svg')
        pic_shift(os.path.join(logs, 'shift.svg'), vals)
        pics.append('shift.svg')
        if clos:
            pic_closure(os.path.join(logs, 'closure.svg'),
                        os.path.join(work, 'mesh_3D.inp'), clos)
            pics.append('closure.svg')
    else:
        nfail += 1
        print('selftest_samples missing FAIL')

    suite = os.path.dirname(os.path.abspath(logs))
    rel = os.path.relpath(logs, suite)
    h = ['<!doctype html><html><head><meta charset="utf-8">',
         '<title>test_kernels</title><style>',
         'body{font-family:Helvetica,Arial,sans-serif;margin:24px;'
         'max-width:1100px}',
         'td,th{padding:3px 8px;border-bottom:1px solid #ddd;'
         'font-size:13px;text-align:left}',
         '.PASS{color:#1a7f37;font-weight:bold}'
         '.FAIL{color:#c00;font-weight:bold}.MEASURED,.INFO{color:#666}',
         'img{max-width:100%;border:1px solid #ccc;margin:8px 0}',
         '</style></head><body>',
         '<h1>test_kernels: %s</h1>' % ('PASS' if not nfail else 'FAIL'),
         '<p>del_vor_selftest: the geometry kernels against answers '
         'worked out independently (2000 random cells, fixed seed), and '
         'the closure of the dual cells on the lanl/voronoi meshes. '
         'See 00README.</p>',
         '<table><tr><th>check</th><th>what</th><th>value</th>'
         '<th>tolerance</th><th>result</th></tr>']
    for key, val, tol, res in table:
        h.append('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td>'
                 '<td class="%s">%s</td></tr>'
                 % (key, what(key), val, tol, res, res))
    h.append('</table>')
    for p in pics:
        h.append('<img src="%s/%s" alt="%s">' % (rel, p, p))
    h.append('<p>Run logs: <a href="%s/">%s/</a></p></body></html>'
             % (rel, rel))
    with open(os.path.join(suite, 'summary.html'), 'w') as f:
        f.write('\n'.join(h) + '\n')
    print('failures %d' % nfail)
    return 1 if nfail else 0


if __name__ == '__main__':
    sys.exit(main())
