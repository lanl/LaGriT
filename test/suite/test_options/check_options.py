"""check_options.py WORK LOGS

test_options: judges each case of cases.txt (run by
run_options_tests.sh in WORK/NAME) and writes summary.html in this
suite's folder, with a picture in LOGS. Python 3 standard library.
Exit status 1 on any FAIL.

Every case: the exit status is the one cases.txt expects.
Cases that must succeed write exactly the files listed in EXPECT
below and nothing else (no stray voronoi.stor). Refused cases print a
message naming the offending token and write no output file (or, for
a mesh that fails the Delaunay check, only the files in EXPECT).

Measures:
  zero set   the vertex pairs a run leaves out are exactly the pairs of
             the zero_area keep run with |A| <= zero_tol d^2 (tets) or
             zero_tol d (triangles); A and d from the keep run
  same       two .stor files hold the same matrix: same pairs, values
             within 1e-14 of the largest, same volumes
  order      two runs with the same options in a different order write
             identical files (the .stor date line aside)
  copy       the report file holds the screen output line for line
  round_off  with delaunay_tol 0, every facet flagged has
             |delta/L| < 1e-15
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), 'test_voronoi_gold'))
from svgdraw import Svg, Projector  # noqa: E402
from check_voronoi_gold import (read_stor, stor_pairs, read_uge,  # noqa
                                read_inp, edges_of)

INPUTS = {'mesh_2D.inp', 'mesh_3D.inp', 'simple_3D.inp', 'run.out',
          'run.rc'}

# files each successful case must write (and no others)
EXPECT = {
    'default_out': ['voronoi.stor'],
    'three_outputs': ['a.stor', 'a.uge', 'a_MESH'],
    'order_a': ['a.stor'], 'order_b': ['a.stor'],
    'keep': ['a.stor'], 'remove': ['a.stor'], 'tol_zero': ['a.stor'],
    'tol_big': ['a.stor'], 'cube_keep': ['a.stor'],
    'cube_remove': ['a.stor'], 'tri_keep': ['a.uge'],
    'tri_remove': ['a.uge'], 'compress_on': ['a.stor'],
    'compress_off': ['a.stor'], 'legacy_type': ['a.uge'],
    'legacy_tough': ['MESH_VORONOI'], 'legacy_compr': ['a.stor'],
    'check_only': [], 'report': ['a.report', 'a.stor'],
    'timing_on': ['a.stor'],
    'tol_zero_ties': ['a.stor', 'ties.vtu'],
    'cv_hybrid': ['a.stor', 'a.vtu'], 'cv_alpha_vor': ['a.stor', 'a.vtu'],
    'blend_alpha': ['a.uge'],
    # exit 1 by design, with the violations file
    'tol_zero_strict': ['ties.vtu'],
}

# text each refused case's message must contain
REFUSED = {
    'bad_keyword': 'zero_areas', 'no_value': 'zero_tol needs a value',
    'bad_zero_area': 'drop', 'bad_zero_tol': '-1',
    'text_zero_tol': 'small', 'bad_cv': 'centroid', 'twice': 'a.stor',
    'no_mesh': 'no mesh', 'missing_mesh': 'nothere.inp',
    'tol_zero_strict': 'not Delaunay', 'bad_tol': 'delaunay_tol -1',
    'bad_fatal': 'delaunay_fatal -1',
    'text_tol': 'delaunay_tol tiny', 'bad_check_only': 'check_only maybe',
    'report_twice': 'a.stor given twice',
    'vtu_twice': 'a.uge given twice', 'over_mesh': 'is the mesh',
    'bad_timing': 'timing maybe',
    'no_alpha': 'cv blend needs alpha',
    'alpha_not_blend': 'alpha is for cv blend',
    'bad_alpha': 'alpha 1.5', 'hyb_tol_median': 'hybrid_tol is for cv hybrid',
    'bad_alpha_min': 'alpha_min -0.1', 'bad_iter': 'hybrid_iter 0',
    'text_iter': 'hybrid_iter 2.5', 'alpha_twice': 'a.stor given twice',
}

TOL = 1e-14
results = []
nfail = 0


def put(case, check, val, verdict):
    global nfail
    if verdict == 'FAIL':
        nfail += 1
    results.append((case, check, val, verdict))
    print('%s_%s %s %s' % (case, check, val, verdict))


def read_cases():
    cases = []
    with open(os.path.join(HERE, 'cases.txt')) as f:
        for ln in f:
            if ln.startswith('#') or '|' not in ln:
                continue
            name, ex, args, about = [t.strip()
                                     for t in ln.split('|', 3)]
            cases.append((name, int(ex), args, about))
    return cases


def dist(xyz, i, j):
    return math.dist(xyz[i], xyz[j])


def zero_set(c, xyz, ndim, tol):
    """Pairs (i < j) of matrix c whose area is zero by zero_tol."""
    z = set()
    for (i, j), v in c.items():
        if i < j:
            d = dist(xyz, i, j)
            if abs(v) * d <= tol * d ** (ndim - 1):
                z.add((i, j))
    return z


def upper(c):
    return {k for k in c if k[0] < k[1]}


def same_matrix(s1, s2):
    c1, c2 = stor_pairs(s1), stor_pairs(s2)
    if set(c1) != set(c2):
        return 'different pairs'
    sc = max(abs(v) for v in c1.values())
    dc = max(abs(c1[k] - c2[k]) for k in c1) / sc
    sv = max(abs(v) for v in s1['vol'])
    dv = max(abs(a - b) for a, b in zip(s1['vol'], s2['vol'])) / sv
    if dc > TOL or dv > TOL:
        return 'values differ by %.1e, volumes by %.1e' % (dc, dv)
    return ''


def main():
    work, logs = sys.argv[1], sys.argv[2]
    gold = os.path.join(os.path.dirname(HERE), 'test_voronoi_gold',
                        'inputs')
    mesh = {n: read_inp(os.path.join(gold, n + '.inp'))
            for n in ('mesh_2D', 'mesh_3D', 'simple_3D')}
    cases = read_cases()
    ran = {}
    for name, expect, args, about in cases:
        d = os.path.join(work, name)
        try:
            rc = int(open(os.path.join(d, 'run.rc')).read())
            out = open(os.path.join(d, 'run.out')).read()
        except (OSError, ValueError):
            put(name, 'ran', 'no run.rc', 'FAIL')
            continue
        ran[name] = d
        put(name, 'exit', '%d (expected %d)' % (rc, expect),
            'PASS' if rc == expect else 'FAIL')
        made = sorted(set(os.listdir(d)) - INPUTS)
        if expect == 0:
            want = sorted(EXPECT.get(name, []))
            put(name, 'files', ' '.join(made) or 'none',
                'PASS' if made == want else 'FAIL')
        else:
            want = sorted(EXPECT.get(name, []))
            put(name, 'no_output' if not want else 'files',
                ' '.join(made) or 'none',
                'PASS' if made == want else 'FAIL')
            txt = REFUSED.get(name, '?')
            put(name, 'message', '"%s" in message' % txt,
                'PASS' if txt in out else 'FAIL')

    def stor(case):
        return read_stor(os.path.join(ran[case], 'a.stor'))

    # zero_area and zero_tol on mesh_3D, against the keep run
    xyz3 = mesh['mesh_3D'][0]
    ck = stor_pairs(stor('keep'))
    nedge = len(upper(ck))
    for case, tol in (('remove', 1e-12), ('tol_big', 0.05),
                      ('default_out', 1e-12)):
        f = 'voronoi.stor' if case == 'default_out' else 'a.stor'
        c = stor_pairs(read_stor(os.path.join(ran[case], f)))
        gone = upper(ck) - upper(c)
        want = zero_set(ck, xyz3, 3, tol)
        put(case, 'zero_set', '%d of %d edges left out (zero by %g: %d)'
            % (len(gone), nedge, tol, len(want)),
            'PASS' if gone == want and not (upper(c) - upper(ck))
            else 'FAIL')
    put('keep', 'all_edges', '%d edges' % nedge,
        'PASS' if nedge == len(edges_of(mesh['mesh_3D'][1])) else 'FAIL')
    c0 = stor_pairs(stor('tol_zero'))
    gone = upper(ck) - upper(c0)
    put('tol_zero', 'zero_set', '%d left out, all with |A| <= 1e-12 '
        'd^2' % len(gone), 'PASS' if gone <= zero_set(ck, xyz3, 3, 1e-12)
        else 'FAIL')
    # every zero_area run has the same volumes
    vk = stor('keep')
    for case in ('remove', 'tol_zero', 'tol_big'):
        sv = max(vk['vol'])
        dv = max(abs(a - b) for a, b in zip(vk['vol'],
                                           stor(case)['vol'])) / sv
        put(case, 'volumes_unchanged', '%.1e' % dv,
            'PASS' if dv <= TOL else 'FAIL')

    # triangles: .uge connections
    xyz2 = mesh['mesh_2D'][0]
    _, uk = read_uge(os.path.join(ran['tri_keep'], 'a.uge'))
    _, ur = read_uge(os.path.join(ran['tri_remove'], 'a.uge'))
    ck2 = {}
    for (i, j), v in uk.items():
        ck2[(i - 1, j - 1)] = v[3] / dist(xyz2, i - 1, j - 1)
    gone = {(i - 1, j - 1) for (i, j) in uk} - {(i - 1, j - 1)
                                                 for (i, j) in ur}
    want = zero_set(ck2, xyz2, 2, 1e-12)
    put('tri_remove', 'zero_set', '%d of %d connections left out'
        % (len(gone), len(uk)),
        'PASS' if gone == want and len(uk) == 16 else 'FAIL')

    # cube: picture of keep and remove
    ckc = stor_pairs(stor('cube_keep'))
    crc = stor_pairs(stor('cube_remove'))
    zc = upper(ckc) - upper(crc)
    put('cube_remove', 'zero_set', '%d of %d edges left out'
        % (len(zc), len(upper(ckc))),
        'PASS' if zc == zero_set(ckc, mesh['simple_3D'][0], 3, 1e-12)
        else 'FAIL')
    picture_cube(os.path.join(logs, 'zero_area.svg'), mesh['simple_3D'],
                 ckc, zc)

    # compress
    on, off = stor('compress_on'), stor('compress_off')
    bad = same_matrix(on, off)
    put('compress_off', 'same_matrix', bad or 'same', 'FAIL' if bad
        else 'PASS')
    put('compress_off', 'values_written', 'on %d, off %d of %d entries'
        % (on['nw'], off['nw'], off['ncoef']),
        'PASS' if on['nw'] < off['nw'] == off['ncoef'] else 'FAIL')
    lc = stor('legacy_compr')
    put('legacy_compr', 'compressed', '%d of %d' % (lc['nw'], lc['ncoef']),
        'PASS' if lc['nw'] < lc['ncoef'] else 'FAIL')

    # order of keywords
    a = open(os.path.join(ran['order_a'], 'a.stor')).read().split('\n')
    b = open(os.path.join(ran['order_b'], 'a.stor')).read().split('\n')
    put('order_b', 'identical', 'files equal but the date line',
        'PASS' if a[:1] + a[2:] == b[:1] + b[2:] else 'FAIL')

    # several outputs in one run, and legacy outputs
    _, u3 = read_uge(os.path.join(ran['three_outputs'], 'a.uge'))
    c3 = stor_pairs(read_stor(os.path.join(ran['three_outputs'],
                                           'a.stor')))
    nm = sum(1 for ln in open(os.path.join(ran['three_outputs'],
                                           'a_MESH'))
             if len(ln) > 10 and ln[5:10].strip() and ln[:5].strip()
             and not ln.startswith(('ELEME', 'CONNE')))
    put('three_outputs', 'consistent', '.stor %d, .uge %d, MESH %d '
        'connections' % (len(upper(c3)), len(u3), nm),
        'PASS' if len(upper(c3)) == len(u3) == nm else 'FAIL')
    _, ul = read_uge(os.path.join(ran['legacy_type'], 'a.uge'))
    put('legacy_type', 'uge', '%d connections' % len(ul),
        'PASS' if len(ul) == len(u3) else 'FAIL')

    # the mesh checks (phase 2)
    rep = open(os.path.join(ran['report'], 'a.report')).read()
    scr = open(os.path.join(ran['report'], 'run.out')).read()
    rl = [ln.rstrip() for ln in rep.splitlines()]
    sl = [ln.rstrip() for ln in scr.splitlines()]
    put('report', 'copy', '%d report lines, %d on screen' % (len(rl),
                                                             len(sl)),
        'PASS' if rl and rl == sl[:len(rl)] and
        'Delaunay check passed' in rep else 'FAIL')
    dl = vtu_values(os.path.join(ran['tol_zero_ties'], 'ties.vtu'),
                    'delta_over_L')
    tz = open(os.path.join(ran['tol_zero_ties'], 'run.out')).read()
    put('tol_zero_ties', 'warning', 'WARNING ... slightly not Delaunay'
        if 'slightly not Delaunay' in tz else 'no warning',
        'PASS' if 'slightly not Delaunay' in tz else 'FAIL')
    put('tol_zero_ties', 'round_off', '%d facets, largest abs(delta/L) '
        '%.1e' % (len(dl), max(map(abs, dl)) if dl else 0),
        'PASS' if dl and max(map(abs, dl)) < 1e-15 else 'FAIL')
    same = (open(os.path.join(ran['over_mesh'], 'mesh_3D.inp')).read() ==
            open(os.path.join(gold, 'mesh_3D.inp')).read())
    put('over_mesh', 'mesh_unchanged', 'same as before' if same else
        'CHANGED', 'PASS' if same else 'FAIL')

    out = open(os.path.join(ran['timing_on'], 'run.out')).read()
    need = ['timing  read', 'timing  assembly', 'timing  write stor',
            'timing  total', 'timing  peak memory']
    miss = [k for k in need if k not in out]
    a = open(os.path.join(ran['timing_on'], 'a.stor')).read().split('\n')
    c = open(os.path.join(ran['default_out'], 'voronoi.stor')).read() \
        .split('\n')
    put('timing_on', 'timing lines', 'missing: ' + ', '.join(miss)
        if miss else 'read, assembly, write stor, total, peak memory',
        'FAIL' if miss else 'PASS')
    put('timing_on', 'same_stor', 'same as without timing (date aside)'
        if a[:1] + a[2:] == c[:1] + c[2:] else 'differs',
        'PASS' if a[:1] + a[2:] == c[:1] + c[2:] else 'FAIL')
    write_page(work, logs)
    print('failures %d' % nfail)
    return 1 if nfail else 0


def vtu_values(path, name):
    """The numbers of the cell-data array NAME of an ASCII .vtu."""
    import xml.etree.ElementTree as ET
    for a in ET.parse(path).iter('DataArray'):
        if a.get('Name') == name:
            return [float(v) for v in (a.text or '').split()]
    return []


def picture_cube(path, mesh, c, zero):
    """simple_3D (a unit cube, 5 tets) twice: zero_area keep, every
    edge with its A/d (zero-area ones dashed); zero_area remove, the
    connections left in the matrix."""
    xyz, cells = mesh
    W, H = 900, 470
    s = Svg(W, H)
    s.text(20, 24, 'zero_area on simple_3D (unit cube, 5 tets): keep '
           'and remove', size=14, bold=True)
    for k, (title, show_zero) in enumerate(
            (('zero_area keep: one connection per mesh edge (18)', True),
             ('zero_area remove (default): %d connections'
              % (18 - len(zero)), False))):
        x0 = 20 + k * 440
        s.text(x0, 54, title, size=12)
        pr = Projector(xyz, x0 + 20, 70, 380, 340, view='iso')
        for (a, b) in edges_of(cells):
            x1, y1 = pr(xyz[a])
            x2, y2 = pr(xyz[b])
            if (a, b) in zero:
                if show_zero:
                    s.line(x1, y1, x2, y2, color='#e67e22', width=1.8,
                           dash='5,3')
                    s.text(0.5 * (x1 + x2) + 4, 0.5 * (y1 + y2) - 3, '0',
                           size=10, color='#e67e22')
                continue
            s.line(x1, y1, x2, y2, color='#2b6cb0', width=1.6)
            s.text(0.5 * (x1 + x2), 0.5 * (y1 + y2) - 3,
                   '%.3g' % -c[(a, b)], size=10, anchor='middle',
                   color='#444')
        for p in xyz:
            x, y = pr(p)
            s.circle(x, y, 4, fill='#222')
    s.text(20, H - 14, 'Blue: connections in the matrix, labeled A/d. '
           'Orange dashed: the 6 face diagonals, whose Voronoi faces '
           'have zero area (co-spherical vertices).', size=11)
    s.save(path)


def esc(t):
    return (str(t).replace('&', '&amp;').replace('<', '&lt;')
            .replace('>', '&gt;'))


def write_page(work, logs):
    """One row per case: its name with a 'What it does' note that
    opens, the command, every check with its value and result, and
    the case's result (FAIL if any check failed)."""
    suite = os.path.dirname(os.path.abspath(logs))
    rel = os.path.relpath(logs, suite)
    h = ['<!doctype html><html><head><meta charset="utf-8">',
         '<title>test_options</title><style>',
         'body{font-family:Helvetica,Arial,sans-serif;margin:24px;'
         'max-width:1150px}',
         'td,th{padding:4px 8px;border-bottom:1px solid #ddd;'
         'font-size:13px;text-align:left;vertical-align:top}',
         'th{background:#f6f8fa}',
         '.PASS{color:#1a7f37;font-weight:bold}'
         '.FAIL{color:#c00;font-weight:bold}',
         'details summary{cursor:pointer;color:#0969da;font-size:12px}',
         'details p{max-width:330px;margin:4px 0}',
         'img{max-width:100%;border:1px solid #ccc;margin:8px 0}',
         'code{font-size:12px;background:#f6f8fa}</style></head><body>',
         '<p style="font-size:13px"><a href="../summary.html">&larr; all '
         'suites</a></p>',
         '<h1>test_options: %s</h1>' % ('PASS' if not nfail else 'FAIL'),
         '<p><b>What it does:</b> runs del_vor once per case below on the '
         'three lanl/voronoi meshes and checks each run: the exit status, '
         'exactly the files it should write (and no others), and what is '
         'in them. The options are keyword / value pairs in any order: '
         '<code>stor</code>, <code>uge</code>, <code>tough2</code> '
         '(outputs), <code>zero_area remove|keep</code>, '
         '<code>zero_tol</code>, <code>compress on|off</code>, '
         '<code>cv</code>, and lanl/voronoi\'s <code>-type -o '
         '-compress</code>. Refused commands must exit 1 with a message '
         'naming the bad token and write nothing. Open "What it does" '
         'under a case for the details; the cases are listed in '
         '<code>cases.txt</code>.</p>',
         '<p>The picture: the unit cube (5 tets) with '
         '<code>zero_area keep</code> (left: all 18 edges, the 6 zero-area '
         'face diagonals dashed) and the default <code>remove</code> '
         '(right: 12 connections).</p>',
         '<img src="%s/zero_area.svg" alt="zero_area">' % rel,
         '<table><tr><th>case</th><th>command</th><th>checks</th>'
         '<th>result</th></tr>']
    by_case = {}
    for case, check, val, res in results:
        by_case.setdefault(case, []).append((check, val, res))
    for name, expect, args, about in read_cases():
        rs = by_case.get(name, [])
        res = 'FAIL' if any(r == 'FAIL' for _, _, r in rs) or not rs \
            else 'PASS'
        checks = '<br>'.join('%s: %s <span class="%s">%s</span>'
                             % (esc(c), esc(v), r, r) for c, v, r in rs)
        h.append('<tr><td>%s<details><summary>What it does</summary>'
                 '<p>%s</p></details></td><td><code>del_vor %s</code><br>'
                 '<span style="font-size:12px">expected exit %d</span>'
                 '</td><td>%s</td><td class="%s">%s</td></tr>'
                 % (esc(name), esc(about), esc(args), expect, checks,
                    res, res))
    h.append('</table><p>Run logs (each with its command and screen '
             'output): <a href="%s/">%s/</a></p></body></html>'
             % (rel, rel))
    with open(os.path.join(suite, 'summary.html'), 'w') as f:
        f.write('\n'.join(h) + '\n')


if __name__ == '__main__':
    sys.exit(main())
