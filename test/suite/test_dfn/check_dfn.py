"""check_dfn.py WORK LOGS

test_dfn: judges each case of cases.txt (run in WORK/NAME by
run_dfn_tests.sh), draws each network, writes summary.html in this
suite's folder. Python 3 standard library. Exit 1 on any FAIL.

dfn3 (and full_mesh, when it is there):
  exit, counts  exit 0; the report's interior edges, boundary edges,
                ties, violations and fracture intersection edges equal
                the exact numbers (../exact_check.py)
  coefficients  every A/d in the .stor equals the cotangent formula,
                A(ij)/d(ij) = sum over the triangles holding ij of
                cot(angle opposite ij) / 2, summed over ALL fractures,
                so an intersection edge gets both fractures' share;
                max difference / max |A/d| <= 1e-12
  areas         every control area in the .stor equals
                sum over its triangles of (|e_ij|^2 cot(k) +
                |e_ik|^2 cot(j)) / 8 (the Voronoi share of vertex i),
                and they add up to the fractures' area; 1e-12
  orientation   no orientation error
dfn3_flip:
  exit 1, the orientation error, the turned-over triangle named,
  no .stor
full_mesh (Carl's example, when inputs/full_mesh.inp is there):
  additionally 60 intersection edges and 1942 interior edge instances
  (the numbers found while writing the design)
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(SUITE, 'test_voronoi_gold'))
from exact_check import read_avs, analyze, counts  # noqa: E402
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, Projector  # noqa: E402
from check_voronoi_gold import read_stor, stor_pairs  # noqa: E402

TOL = 1e-12
LABELS = [('interior edges checked', 'interior'),
          ('ties (|delta/L| <= tol)', 'ties'),
          ('violations', 'violations'),
          ('boundary edges checked', 'boundary'),
          ('fracture intersection edges', 'intersect')]
COLORS = {1: '#2b6cb0', 2: '#1a7f37', 3: '#b7791f', 4: '#805ad5'}


def read_report(path):
    got = {}
    if not os.path.exists(path):
        return None
    for ln in open(path):
        s = ln.strip()
        for lab, key in LABELS:
            if s.startswith(lab) and s[len(lab):].strip().isdigit():
                got[key] = int(s[len(lab):])
    return got


def sub(a, b):
    return [a[i] - b[i] for i in range(3)]


def cross(a, b):
    return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0]]


def dot(a, b):
    return sum(a[i] * b[i] for i in range(3))


def cot_at(P, k, i, j):
    u, v = sub(P[i], P[k]), sub(P[j], P[k])
    return dot(u, v) / math.sqrt(dot(cross(u, v), cross(u, v)))


def cotangent_answers(m):
    """{(i, j): A/d}, [area of each vertex], total area: the textbook
    2D Voronoi formulas, triangle by triangle, all fractures summed."""
    P = m.xyz
    coef, area = {}, [0.0] * len(P)
    total = 0.0
    for c in m.cells:
        for a in range(3):
            i, j, k = c[a], c[(a + 1) % 3], c[(a + 2) % 3]
            key = (min(i, j), max(i, j))
            coef[key] = coef.get(key, 0.0) + 0.5 * cot_at(P, k, i, j)
            # vertex i's share: its two edges, each times the cot of
            # the angle opposite it
            eij = dot(sub(P[j], P[i]), sub(P[j], P[i]))
            eik = dot(sub(P[k], P[i]), sub(P[k], P[i]))
            area[i] += (eij * cot_at(P, k, i, j)
                        + eik * cot_at(P, j, i, k)) / 8.0
        n = cross(sub(P[c[1]], P[c[0]]), sub(P[c[2]], P[c[0]]))
        total += 0.5 * math.sqrt(dot(n, n))
    return coef, area, total


def picture(path, title, m, inter, bad):
    """The fractures, each in its own color; intersection edges black;
    the triangles a message names, red."""
    W, H = 640, 560
    s = Svg(W, H)
    s.text(16, 22, title, size=13, bold=True)
    pr = Projector(m.xyz, 20, 40, 600, 440, view='iso')
    for k, c in enumerate(m.cells):
        pts = [pr(m.xyz[v]) for v in c]
        col = COLORS.get(m.clr[k], '#666')
        if k + 1 in bad:
            s.polygon(pts, fill='#c0392b', stroke='#c0392b', width=1.5,
                      opacity=0.9)
        else:
            s.polygon(pts, fill=col, stroke=col, width=0.5, opacity=0.12)
    for (a, b) in inter:
        x1, y1 = pr(m.xyz[a])
        x2, y2 = pr(m.xyz[b])
        s.line(x1, y1, x2, y2, color='#111', width=2.4)
    y = H - 40
    x = 20
    for f in sorted(set(m.clr)):
        s.rect(x, y - 10, 14, 10, fill=COLORS.get(f, '#666'))
        s.text(x + 20, y, 'fracture %d: %d triangles' % (f, m.clr.count(f)),
               size=11)
        x += 170
    s.line(20, y + 16, 50, y + 16, color='#111', width=2.4)
    s.text(58, y + 20, 'intersection edges: %d' % len(inter), size=11)
    s.save(path)


def intersection_edges(m):
    hold = {}
    for k, c in enumerate(m.cells):
        for a in range(3):
            e = tuple(sorted((c[a], c[(a + 1) % 3])))
            hold.setdefault(e, []).append(m.clr[k])
    return [e for e, cl in hold.items() if len(cl) >= 3 and
            len(set(cl)) >= 2]


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows = []
    for name, mesh, args, about in read_cases(
            os.path.join(HERE, 'cases.txt'), 4):
        d = os.path.join(work, name)
        m_name = os.path.basename(mesh)
        cmd = 'del_vor -avs %s %s -report %s.report' % (m_name, args, name)
        checks = []

        def put(c, v, ok):
            res = ok if isinstance(ok, str) else ('PASS' if ok else 'FAIL')
            checks.append((c, v, res))
            print('%s_%s %s %s' % (name, c, v, res))

        if not os.path.exists(os.path.join(d, 'run.rc')):
            if name == 'full_mesh':
                put('skipped', 'inputs/full_mesh.inp not added yet',
                    'MEASURED')
            else:
                put('ran', 'no run.rc', False)
            rows.append((name, about, cmd, checks, None))
            continue
        rc = int(open(os.path.join(d, 'run.rc')).read())
        out = open(os.path.join(d, 'run.out')).read()
        m = read_avs(os.path.join(d, m_name))
        inter = intersection_edges(m)
        bad = set()
        if name.endswith('_flip'):
            put('exit', '%d (expected 1)' % rc, rc == 1)
            put('message', 'orientation error',
                'oriented differently' in out)
            for ln in out.splitlines():
                if 'first: cells' in ln:
                    bad = {int(x) for x in ln.split() if x.isdigit()}
            flipped = int(open(os.path.join(work, 'gen', 'flipped.txt'))
                          .read()) if os.path.exists(
                os.path.join(work, 'gen', 'flipped.txt')) else None
            put('named', 'cells %s' % sorted(bad),
                flipped is None or flipped in bad)
            put('files', 'no .stor', not os.path.exists(
                os.path.join(d, name + '.stor')))
        else:
            put('exit', '%d (expected 0)' % rc, rc == 0)
            want = counts(analyze(m))
            got = read_report(os.path.join(d, name + '.report')) or {}
            keys = ['interior', 'boundary', 'ties', 'violations',
                    'intersect']
            bad_k = [k for k in keys if got.get(k) != want[k]]
            put('counts', ', '.join('%s %s' % (k, got.get(k))
                                    for k in keys), not bad_k)
            if name == 'full_mesh':
                put('design', 'intersection edges %s (60), interior '
                    'edges %s (1942)' % (got.get('intersect'),
                                         got.get('interior')),
                    got.get('intersect') == 60 and
                    got.get('interior') == 1942)
            put('orientation', 'no orientation error',
                'oriented differently' not in out)
            f = os.path.join(d, name + '.stor')
            if os.path.exists(f):
                st = read_stor(f)
                c = stor_pairs(st)
                coef, area, total = cotangent_answers(m)
                cmax = max(abs(v) for v in coef.values())
                dc = max(abs(-c.get(k, 0.0) - v) for k, v in coef.items())
                put('coefficients', 'max |A/d - cot formula| / max %.1e'
                    % (dc / cmax), dc <= TOL * cmax)
                amax = max(abs(v) for v in area)
                da = max(abs(a - b) for a, b in zip(st['vol'], area))
                put('areas', 'max |area - formula| / max %.1e; sum %.15g, '
                    'fractures %.15g' % (da / amax, sum(st['vol']), total),
                    da <= TOL * amax and
                    abs(sum(st['vol']) - total) <= TOL * total)
            else:
                put('stor', 'missing', False)
        pic = name + '.svg'
        picture(os.path.join(logs, pic), '%s: %s' % (name, m_name), m,
                inter, bad)
        rows.append((name, about, cmd, checks, pic))

    what = ('runs del_vor on fracture networks: triangles in several '
            'planes, one itetclr per fracture, crossing along shared '
            'edges (each intersection edge is in 2 triangles of each '
            'fracture). del_vor pairs each edge with the triangle of the '
            'same fracture, checks orientation fracture by fracture '
            '(either way is fine), runs the Delaunay check within each '
            'fracture, and adds every fracture\'s share into one '
            'coefficient per vertex pair. The network dfn3 is made by '
            'make_dfn_inputs.py, exactly Delaunay by construction; its '
            'counts are checked against exact arithmetic and every '
            'coefficient and area against the cotangent formula. Carl\'s '
            'dfnWorks example runs too once it is in inputs/.')
    nfail = write_case_page(os.path.dirname(os.path.abspath(logs)), logs,
                            'test_dfn', what, rows)
    print('failures %d' % nfail)
    return 1 if nfail else 0


if __name__ == '__main__':
    sys.exit(main())
