"""check_voronoi_gold.py WORK LOGS INPUTS

test_voronoi_gold: del_vor's .stor, .uge and TOUGH2 MESH files for the
three lanl/voronoi test meshes, against the gold files and against
answers computed here from the mesh itself. Python 3 standard library.
Prints one "key value" line per quantity; writes summary.html in this
suite's folder and the pictures in LOGS. Exit status 1 on any FAIL.

Measures (all relative, so the size of the mesh does not matter):

  stor_vol_diff     max |V_i - V_i(gold)| / max |V(gold)|
  stor_coef_diff    max |c_ij - c_ij(gold)| / max |c(gold)| over every
                    vertex pair in either file (c = the .stor value,
                    -A/d); a pair missing from one file counts as 0
  stor_layout       the .stor blocks fit together: row pointers,
                    sorted columns with the diagonal in each row, the
                    diagonal pointers, a symmetric matrix, NCON_MAX
  balance           |sum V_i - mesh volume| / mesh volume, the mesh
                    volume (area) computed here from the .inp
  uge_vol_diff      as stor_vol_diff, .uge against gold .uge
  uge_area_diff     max |A_ij - A_ij(gold)| / max |A(gold)| over the
                    connections in either file; a connection may be
                    missing from one file only if its area is zero
                    (|A| <= 1e-12 max |A|)
  uge_vs_stor       the .uge area / d(ij) and volumes against the
                    .stor values of the same run
  mesh_fields       TOUGH2 MESH: every record's names, material and
                    ISOT equal to gold, every number within one unit of
                    its last printed digit
  mesh_identical    MEASURED: lines that are byte-identical to gold

HARD checks: every quantity above except those marked MEASURED; the
tolerance is 1e-12 for the relative differences.
"""

import math
import os
import sys

TOL = 1e-12
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
from svgdraw import Svg, ramp, Projector  # noqa: E402

MESHES = [
    # name, dimension, gold stor, gold uge, gold MESH
    ('mesh_2D', 2, 'gold_2D.stor', 'gold_2D.uge', 'gold_2D_MESH'),
    ('mesh_3D', 3, 'gold_3D.stor', 'gold_3D.uge', 'gold_3D_MESH'),
    ('simple_3D', 3, 'gold_simple_3D.stor', None, None),
]

results = []      # (key, value, verdict)
nfail = 0


def put(key, val, verdict):
    global nfail
    if verdict == 'FAIL':
        nfail += 1
    results.append((key, val, verdict))
    print('%s %s %s' % (key, val, verdict))


def judge(key, val, tol=TOL):
    put(key, '%.3e' % val, 'PASS' if val <= tol else 'FAIL')


# ---------------------------------------------------------------- readers

def read_inp(path):
    with open(path) as f:
        lines = [ln for ln in f if ln.strip()]
    nn, nc = int(lines[0].split()[0]), int(lines[0].split()[1])
    xyz = [tuple(float(v) for v in ln.split()[1:4])
           for ln in lines[1:1 + nn]]
    cells = []
    for ln in lines[1 + nn:1 + nn + nc]:
        t = ln.split()
        cells.append([int(v) - 1 for v in t[3:]])
    return xyz, cells


def read_stor(path):
    with open(path) as f:
        lines = f.read().split('\n')
    h = [int(v) for v in lines[2].split()]
    nw, neq, n3, nac = h[:4]
    ncon_max = h[4] if len(h) > 4 else None
    ncoef = n3 - neq - 1
    t = ' '.join(lines[3:]).split()
    p = 0
    vol = [float(v) for v in t[p:p + neq]]
    p += neq
    rp = [int(v) for v in t[p:p + neq + 1]]
    p += neq + 1
    col = [int(v) for v in t[p:p + ncoef]]
    p += ncoef
    ptr = [int(v) for v in t[p:p + ncoef]]
    p += ncoef
    pad = [int(v) for v in t[p:p + neq + 1]]
    p += neq + 1
    dg = [int(v) for v in t[p:p + neq]]
    p += neq
    val = [float(v) for v in t[p:p + nw * nac]]
    return dict(nw=nw, neq=neq, ncoef=ncoef, ncon_max=ncon_max, vol=vol,
                rp=rp, col=col, ptr=ptr, pad=pad, dg=dg, val=val,
                ntail=len(t) - p - nw * nac)


def stor_pairs(s):
    c = {}
    neq = s['neq']
    for i in range(neq):
        for q in range(s['rp'][i] - neq - 1, s['rp'][i + 1] - neq - 1):
            j = s['col'][q] - 1
            if j != i:
                k = s['ptr'][q]
                c[(i, j)] = s['val'][k - 1] if k > 0 else 0.0
    return c


def stor_layout_ok(s):
    """Return a list of problems with the .stor blocks (empty if none)."""
    bad = []
    neq, ncoef = s['neq'], s['ncoef']
    rp = s['rp']
    if rp[0] != neq + 1 or rp[-1] != ncoef + neq + 1:
        bad.append('row pointers do not run from NEQ+1 to NCOEF+NEQ+1')
    nmax = 0
    for i in range(neq):
        cols = s['col'][rp[i] - neq - 1:rp[i + 1] - neq - 1]
        nmax = max(nmax, len(cols))
        if cols != sorted(cols) or len(set(cols)) != len(cols):
            bad.append('row %d columns not sorted' % (i + 1))
            break
        if (i + 1) not in cols:
            bad.append('row %d has no diagonal' % (i + 1))
            break
        q = rp[i] - neq - 1 + cols.index(i + 1)
        if s['dg'][i] != neq + 1 + q + 1:
            bad.append('diagonal pointer of row %d wrong' % (i + 1))
            break
    if s['ncon_max'] is not None and s['ncon_max'] != nmax:
        bad.append('NCON_MAX %d, longest row %d' % (s['ncon_max'], nmax))
    if any(v != 0 for v in s['pad']):
        bad.append('padding block not all 0')
    if any(k < 0 or k > s['nw'] for k in s['ptr']):
        bad.append('a pointer outside the value block')
    if s['ntail'] != 0:
        bad.append('%d values left after the last block' % s['ntail'])
    c = stor_pairs(s)
    for (i, j), v in c.items():
        if (j, i) not in c or c[(j, i)] != v:
            bad.append('matrix not symmetric at %d %d' % (i + 1, j + 1))
            break
    return bad


def read_uge(path):
    with open(path) as f:
        rows = [ln.split() for ln in f if ln.strip()]
    n = int(rows[0][1])
    cells = {int(r[0]): [float(v) for v in r[1:5]] for r in rows[1:1 + n]}
    m = int(rows[1 + n][1])
    con = {(int(r[0]), int(r[1])): [float(v) for v in r[2:6]]
           for r in rows[2 + n:2 + n + m]}
    return cells, con


def mesh_records(path):
    with open(path) as f:
        return f.read().split('\n')


def num_ok(a, b):
    """TOUGH2 fields a (ours) and b (gold): equal, or numbers within
    one unit of the last printed digit of b."""
    a, b = a.strip(), b.strip()
    if a == b:
        return True
    if not a or not b:
        return False
    try:
        x, y = float(a), float(b)
    except ValueError:
        return False
    mant = b.upper().split('E')[0]
    ndec = len(mant.split('.')[1]) if '.' in mant else 0
    expo = int(b.upper().split('E')[1]) if 'E' in b.upper() else 0
    unit = 10.0 ** (expo - ndec)
    return abs(x - y) <= 1.0001 * unit


# fields of an ELEME and a CONNE record: (start, end), 0-based
ELEME_TEXT = [(0, 5), (15, 20)]
ELEME_NUM = [(20, 30), (30, 40), (50, 60), (60, 70), (70, 80)]
CONNE_TEXT = [(0, 5), (5, 10), (29, 30)]
CONNE_NUM = [(30, 40), (40, 50), (50, 60), (60, 70)]


def compare_mesh(ours, gold):
    """Return (problems, identical line count)."""
    bad = []
    if len(ours) != len(gold):
        bad.append('%d lines, gold %d' % (len(ours), len(gold)))
    same = 0
    block = None
    for k, (a, b) in enumerate(zip(ours, gold)):
        if a == b:
            same += 1
        if b.startswith('ELEME'):
            block = 'E'
            continue
        if b.startswith('CONNE'):
            block = 'C'
            continue
        if not b.strip():
            if a.strip():
                bad.append('line %d: blank in gold' % (k + 1))
            continue
        a = a.ljust(80)
        b = b.ljust(80)
        txt, num = ((ELEME_TEXT, ELEME_NUM) if block == 'E'
                    else (CONNE_TEXT, CONNE_NUM))
        for s, e in txt:
            if a[s:e] != b[s:e]:
                bad.append('line %d cols %d-%d: %r, gold %r'
                           % (k + 1, s + 1, e, a[s:e], b[s:e]))
        for s, e in num:
            if not num_ok(a[s:e], b[s:e]):
                bad.append('line %d cols %d-%d: %r, gold %r'
                           % (k + 1, s + 1, e, a[s:e], b[s:e]))
    return bad, same


# ----------------------------------------------------------- geometry here

def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def norm(a):
    return math.sqrt(dot(a, a))


def mesh_measure(xyz, cells):
    tot = 0.0
    for c in cells:
        p = [xyz[i] for i in c]
        if len(c) == 3:
            tot += 0.5 * norm(cross(sub(p[1], p[0]), sub(p[2], p[0])))
        else:
            tot += abs(dot(sub(p[1], p[0]),
                           cross(sub(p[2], p[0]), sub(p[3], p[0])))) / 6.0
    return tot


def circumcenter_tri(p1, p2, p3):
    a, b = sub(p2, p1), sub(p3, p1)
    axb = cross(a, b)
    den = 2.0 * dot(axb, axb)
    u = (dot(a, a) * b[0] - dot(b, b) * a[0],
         dot(a, a) * b[1] - dot(b, b) * a[1],
         dot(a, a) * b[2] - dot(b, b) * a[2])
    w = cross(u, axb)
    return (p1[0] + w[0] / den, p1[1] + w[1] / den, p1[2] + w[2] / den)


def edges_of(cells):
    e = set()
    for c in cells:
        for i in range(len(c)):
            for j in range(i + 1, len(c)):
                e.add((min(c[i], c[j]), max(c[i], c[j])))
    return sorted(e)


# ---------------------------------------------------------------- pictures

def fmt(v):
    if v == 0:
        return '0'
    if 1e-3 <= abs(v) < 1e4:
        return '%.4g' % v
    return '%.3e' % v


def picture_2d(path, xyz, cells, coef, gcoef, vol):
    """mesh_2D: triangles, the Voronoi dual pieces (circumcenter to
    edge midpoints, computed here), each edge's coefficient A/d from
    del_vor's .stor (green: equal to gold, red: not) and each vertex's
    control volume."""
    W, H = 640, 600
    s = Svg(W, H)
    s.text(20, 24, 'mesh_2D: 8 triangles, Voronoi dual (blue), A/d per '
           'edge, area per vertex', size=14, bold=True)
    pr = Projector(xyz, 60, 60, 520, 470, view='plan')
    for c in cells:
        s.polygon([pr(xyz[i]) for i in c], fill='#f2f2f2', stroke='#888')
    for c in cells:
        cc = circumcenter_tri(*[xyz[i] for i in c])
        for a, b in ((c[0], c[1]), (c[0], c[2]), (c[1], c[2])):
            m = tuple(0.5 * (xyz[a][k] + xyz[b][k]) for k in range(3))
            x1, y1 = pr(cc)
            x2, y2 = pr(m)
            s.line(x1, y1, x2, y2, color='#2b6cb0', width=2.0)
    for (a, b) in edges_of(cells):
        m = tuple(0.5 * (xyz[a][k] + xyz[b][k]) for k in range(3))
        x, y = pr(m)
        v = -coef.get((a, b), 0.0)
        g = -gcoef.get((a, b), 0.0)
        ok = abs(v - g) <= TOL * max(1.0, abs(g))
        s.rect(x - 17, y - 11, 34, 15, fill='white')
        s.text(x, y, fmt(v), size=11, anchor='middle',
               color='#1a7f37' if ok else '#c00')
    for i, p in enumerate(xyz):
        x, y = pr(p)
        s.circle(x, y, 5, fill='#222')
        s.text(x + 8, y - 8, '%d: %s' % (i + 1, fmt(vol[i])), size=11,
               color='#444')
    s.text(20, H - 16, 'Edge labels: A/d from del_vor (green = gold value '
           'to 1e-12). Vertex labels: number: control area.', size=11)
    s.save(path)


def picture_3d(path, title, xyz, cells, coef, vol, zero_pairs, label):
    """A 3D mesh: every edge (gray), zero-area connections (orange,
    dashed), vertices colored by control volume (log scale); on a
    small mesh each other edge is labeled with its A/d."""
    W, H = 680, 640
    s = Svg(W, H)
    s.text(20, 24, title, size=14, bold=True)
    pr = Projector(xyz, 40, 50, 600, 500, view='iso')
    for (a, b) in edges_of(cells):
        x1, y1 = pr(xyz[a])
        x2, y2 = pr(xyz[b])
        if (a, b) in zero_pairs:
            s.line(x1, y1, x2, y2, color='#e67e22', width=1.6, dash='5,3')
        else:
            s.line(x1, y1, x2, y2, color='#9aa', width=0.8)
        if label and (a, b) not in zero_pairs:
            v = -coef.get((a, b), 0.0)
            s.text(0.5 * (x1 + x2), 0.5 * (y1 + y2) - 3, fmt(v), size=10,
                   anchor='middle', color='#555')
    lv = [math.log10(v) for v in vol if v > 0]
    lo, hi = (min(lv), max(lv)) if lv else (0.0, 1.0)
    for i, p in enumerate(xyz):
        x, y = pr(p)
        t = (math.log10(vol[i]) - lo) / (hi - lo) if hi > lo else 0.5
        s.circle(x, y, 4.5, fill=ramp(t), stroke='#333', width=0.6)
    s.legend(40, H - 58, 260, 12, '%.2e' % 10 ** lo, '%.2e' % 10 ** hi,
             'control volume (vertex color, log scale)')
    s.line(340, H - 52, 380, H - 52, color='#e67e22', width=1.6,
           dash='5,3')
    s.text(388, H - 48, 'zero-area connection (%d)' % len(zero_pairs),
           size=11)
    s.save(path)


# -------------------------------------------------------------------- main

def main():
    work, logs, inputs = sys.argv[1], sys.argv[2], sys.argv[3]
    pics = []
    rows = []
    for name, ndim, gstor, guge, gmesh in MESHES:
        xyz, cells = read_inp(os.path.join(inputs, name + '.inp'))
        k0 = len(results)
        f_stor = os.path.join(work, name + '.stor')
        f_cst = os.path.join(work, name + '_c.stor')
        f_uge = os.path.join(work, name + '.uge')
        f_mesh = os.path.join(work, name + '_MESH')
        missing = [f for f in (f_stor, f_cst, f_uge, f_mesh)
                   if not os.path.exists(f)]
        if missing:
            put(name + '_outputs', 'missing ' + ' '.join(
                os.path.basename(f) for f in missing), 'FAIL')
            rows.append((name, results[k0:]))
            continue
        put(name + '_outputs', 'all 4 written', 'PASS')

        gs = read_stor(os.path.join(inputs, gstor))
        gc = stor_pairs(gs)
        gv = gs['vol']
        vscale = max(abs(v) for v in gv)
        cscale = max(abs(v) for v in gc.values())
        s = None
        c = {}
        for tag, f in (('stor', f_stor), ('stor_compressed', f_cst)):
            s = read_stor(f)
            bad = stor_layout_ok(s)
            put('%s_%s_layout' % (name, tag), '; '.join(bad) or 'ok',
                'FAIL' if bad else 'PASS')
            if s['neq'] != gs['neq']:
                put('%s_%s_neq' % (name, tag), s['neq'], 'FAIL')
                continue
            c = stor_pairs(s)
            dv = max(abs(a - b) for a, b in zip(s['vol'], gv)) / vscale
            dc = max(abs(c.get(k, 0.0) - gc.get(k, 0.0))
                     for k in set(c) | set(gc)) / cscale
            judge('%s_%s_vol_diff' % (name, tag), dv)
            judge('%s_%s_coef_diff' % (name, tag), dc)
            put('%s_%s_values_written' % (name, tag), '%d of %d entries'
                % (s['nw'], s['ncoef']), 'MEASURED')
        s = read_stor(f_stor)
        c = stor_pairs(s)
        vol = s['vol']
        mv = mesh_measure(xyz, cells)
        judge(name + '_balance', abs(sum(vol) - mv) / mv)

        # .uge
        cells_u, con_u = read_uge(f_uge)
        amax = max(abs(v[3]) for v in con_u.values())
        d_vs = 0.0
        for (i, j), v in con_u.items():
            d = norm(sub(xyz[j - 1], xyz[i - 1]))
            d_vs = max(d_vs, abs(v[3] / d + c[(i - 1, j - 1)]))
        d_vs /= cscale
        d_vs = max(d_vs, max(abs(cells_u[i + 1][3] - vol[i])
                             for i in range(len(vol))) / vscale)
        zero = [k for k in c if k[0] < k[1]
                and k not in {(i - 1, j - 1) for (i, j) in con_u}]
        judge(name + '_uge_vs_stor', d_vs)
        if guge:
            gcells, gcon = read_uge(os.path.join(inputs, guge))
            gvs = max(abs(v[3]) for v in gcells.values())
            gas = max(abs(v[3]) for v in gcon.values())
            dv = max(abs(cells_u[i][3] - gcells[i][3])
                     for i in gcells) / gvs
            da = 0.0
            for k in set(con_u) | set(gcon):
                a = con_u.get(k)
                b = gcon.get(k)
                if a and b:
                    da = max(da, abs(a[3] - b[3]) / gas)
                    da = max(da, max(abs(a[q] - b[q]) for q in range(3))
                             / max(1.0, max(abs(b[q]) for q in range(3))))
                elif (a or b)[3] and abs((a or b)[3]) > TOL * gas:
                    da = max(da, abs((a or b)[3]) / gas)
            judge(name + '_uge_vol_diff', dv)
            judge(name + '_uge_area_diff', da)
            put(name + '_uge_connections', '%d, gold %d (zero-area '
                'left out: %d)' % (len(con_u), len(gcon), len(zero)),
                'MEASURED')

        # TOUGH2 MESH
        ours = mesh_records(f_mesh)
        if gmesh:
            gold = mesh_records(os.path.join(inputs, gmesh))
            bad, same = compare_mesh(ours, gold)
            put(name + '_mesh_fields', '; '.join(bad[:3]) or 'ok',
                'FAIL' if bad else 'PASS')
            put(name + '_mesh_identical', '%d of %d lines' % (
                same, len(gold)), 'MEASURED')
        # MESH volumes against .stor: within one unit of the 4th digit
        bad = 0
        for i, line in enumerate(ours[1:1 + len(vol)]):
            v = vol[i]
            unit = 10.0 ** (math.floor(math.log10(abs(v))) - 3) if v \
                else 1e-300
            if abs(float(line[20:30]) - v) > 1.0001 * unit:
                bad += 1
        put(name + '_mesh_vs_stor', '%d volumes differ' % bad,
            'FAIL' if bad else 'PASS')

        # picture
        zero_pairs = set(zero)
        png = name + '.svg'
        if ndim == 2:
            picture_2d(os.path.join(logs, png), xyz, cells, c, gc, vol)
        else:
            title = ('%s: %d tets; vertices colored by control volume'
                     % (name, len(cells)))
            picture_3d(os.path.join(logs, png), title, xyz, cells, c, vol,
                       zero_pairs, label=len(cells) < 10)
        pics.append((name, png))
        rows.append((name, results[k0:]))

    write_page(os.path.dirname(os.path.abspath(logs)), logs, rows, pics)
    print('failures %d' % nfail)
    return 1 if nfail else 0


def write_page(suite, logs, rows, pics):
    rel = os.path.relpath(logs, suite)
    out = ['<!doctype html><html><head><meta charset="utf-8">',
           '<title>test_voronoi_gold</title><style>',
           'body{font-family:Helvetica,Arial,sans-serif;margin:24px;'
           'max-width:1100px}',
           'td,th{padding:3px 8px;border-bottom:1px solid #ddd;'
           'font-size:13px;text-align:left}',
           '.PASS{color:#1a7f37;font-weight:bold}'
           '.FAIL{color:#c00;font-weight:bold}.MEASURED{color:#666}',
           'img{max-width:100%;border:1px solid #ccc;margin:8px 0}',
           '</style></head><body>',
           '<h1>test_voronoi_gold: %s</h1>' % ('PASS' if not nfail
                                               else 'FAIL'),
           '<p>del_vor against the lanl/voronoi test meshes. The .stor '
           'gold files were written by LaGriT (matbld2d / matbld3d); '
           'the .uge and MESH gold files by lanl/voronoi. Tolerance '
           '1e-12, relative. See 00README for each measure.</p>']
    for name, rs in rows:
        out.append('<h2>%s</h2><table><tr><th>check</th><th>value</th>'
                   '<th>result</th></tr>' % name)
        for k, v, r in rs:
            out.append('<tr><td>%s</td><td>%s</td><td class="%s">%s</td>'
                       '</tr>' % (k, v, r, r))
        out.append('</table>')
        for n, p in pics:
            if n == name:
                out.append('<img src="%s/%s" alt="%s">' % (rel, p, n))
    out.append('<p>Run logs: <a href="%s/">%s/</a></p>' % (rel, rel))
    out.append('</body></html>')
    with open(os.path.join(suite, 'summary.html'), 'w') as f:
        f.write('\n'.join(out) + '\n')


if __name__ == '__main__':
    sys.exit(main())
