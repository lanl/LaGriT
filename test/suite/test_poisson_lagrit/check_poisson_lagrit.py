"""check_poisson_lagrit.py WORK LOGS INPUTS

test_poisson_lagrit: del_vor against LaGriT's own .stor files on five
Poisson disk meshes from LaGriT's suites: test_poisson_3d cases 1 and
2 (box 0.4 x 0.4 x 0.2) and the three meshes of test_poisson_2d (unit
square). Python 3 standard library. Prints one "key value result"
line per quantity; writes summary.html in this suite's folder and the
pictures in LOGS. Exit status 1 on any FAIL.

The checks allow exactly the differences the LaGriT writer explains
(the WRITERS table below):

  3D: anothermatbld3d (dump / stor default, "matbld3d_astor") merges
      coefficients within 1e-8 of the largest |A/d| when it compresses
      and leaves out coefficients below 1e-8 of the largest.
  2D: matbld2d ("matbld2d_stor") writes every value, uncompressed.
      LaGriT computed from the full coordinates; the .inp files keep
      13 digits, which moves del_vor's results by up to 3e-11 of the
      largest (measured by rounding the coordinates again), so the
      tolerances are 5e-11 (coefficients) and 1e-11 (volumes).

  box_volume      |sum of del_vor volumes - exact| / exact, the box
                  volume (unit square area) known exactly. HARD, 1e-13.
  pairs_lagrit    no pair is in LaGriT's .stor and not in del_vor's
                  (zero_area keep). HARD.
  pairs_dropped   every pair in del_vor's and not LaGriT's has
                  |A/d| <= drop * max |A/d| (the writer's zero rule;
                  0 for matbld2d). HARD.
  coef_diff       max |c - c(LaGriT)| / max |c| over the common pairs,
                  c the .stor value -A/d. HARD, the writer's tolerance.
  vol_diff        max |V - V(LaGriT)| / max |V| over the vertices not
                  on a nonzero pair LaGriT dropped. HARD. The vertices
                  of such pairs are MEASURED: LaGriT drops the pair's
                  share of their volumes too.
  negative        the pairs with A < 0 beyond the writer's tolerance are
                  the same in both files. HARD.
  default_zero    del_vor with the default zero_area remove leaves out
                  exactly its zero-area pairs (|A| <= 1e-12 d^2 for
                  tets, 1e-12 d for triangles), all of which LaGriT left
                  out too. HARD.
  lagrit_balance  MEASURED: LaGriT's volume total against the exact one.
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), 'test_voronoi_gold'))
from svgdraw import Svg, Projector, ramp  # noqa: E402
from check_voronoi_gold import read_stor, stor_pairs, read_inp  # noqa

# case: (name, LaGriT .stor, dimension, writer, exact volume, title)
CASES = [
    ('test_3d_poi_01', 'test_3d_poi_01.stor', 3, 'astor', 0.032,
     'test_poisson_3d case 1: no buffer on the outside surfaces'),
    ('test_3d_poi_02', 'test_3d_poi_02.stor', 3, 'astor', 0.032,
     'test_poisson_3d case 2: buffer = h on the outside surfaces'),
    ('output01_square_uniform', 'output_01_square_uniform.stor', 2,
     'matbld2d', 1.0, 'test_poisson_2d: uniform h = 0.05'),
    ('output02_square_var', 'output02_square_var.stor', 2, 'matbld2d',
     1.0, 'test_poisson_2d: variable h, WIDTH_FACTOR 10'),
    ('output03_square_var', 'output03_square_var.stor', 2, 'matbld2d',
     1.0, 'test_poisson_2d: variable h, WIDTH_FACTOR 20'),
]
# writer: coefficient tolerance, volume tolerance, drop rule (relative
# to max |A/d|), negative threshold, label
WRITERS = {
    'astor': (1.01e-8, 1e-11, 1e-8, 1e-8,
              "LaGriT's compression tolerance 1e-8"),
    'matbld2d': (5e-11, 1e-11, 0.0, 1e-12,
                 'tolerance 5e-11 (13-digit .inp)'),
}
ABOUT = {
    'test_3d_poi_01': 'LaGriT built this box with createpts / poisson_disk '
    '/ 3d_box, h = 0.025, and no buffer on the outside surfaces, so '
    'boundary tets often have their circumcenters outside the box and '
    'some coefficients are negative (75 here; red in the picture). '
    'del_vor runs twice (every edge kept and compress off; the defaults) '
    'and is compared pair by pair with the .stor LaGriT wrote with '
    'dump / stor (anothermatbld3d). Purple: two real but tiny '
    'connections (A/d about 1e-10 of the largest) that LaGriT\'s 1e-8 '
    'rule left out, with their share of four vertices\' volumes.',
    'test_3d_poi_02': 'The same box with a buffer of h on the outside '
    'surfaces: the boundary vertices lie on a grid, so many sets of four '
    'are co-spherical and 871 boundary connections have zero-area faces '
    '(orange); there are no negative coefficients. Both codes leave the '
    'same 871 out.',
    'output01_square_uniform': 'LaGriT\'s test_poisson_2d: the unit '
    'square [-0.5, 0.5]^2 filled by createpts / poisson_disk / '
    '2d_polygon with uniform h = 0.05, triangulated, and written by '
    'dump / stor (matbld2d, every value written). Compared pair by pair; '
    'the picture colors each edge by its difference. LaGriT computed '
    'from full-precision coordinates and the .inp keeps 13 digits, which '
    'is where the differences of about 1e-13 come from.',
    'output02_square_var': 'The same square with a variable h: h = 0.01 '
    'along x = 0, growing as 0.01 x 10 |x| toward the sides '
    '(user_h_field_att.mlgi, WIDTH_FACTOR 10). The small triangles in the '
    'middle show the largest differences: the coordinate rounding '
    'matters most there.',
    'output03_square_var': 'As output02 with WIDTH_FACTOR 20: h grows '
    'twice as fast away from x = 0.',
}
BOX = (0.4, 0.4, 0.2)  # the 3D cases
ZERO_TOL = 1e-12       # del_vor's default zero_tol

results = []
nfail = 0


def put(key, val, verdict):
    global nfail
    if verdict == 'FAIL':
        nfail += 1
    results.append((key, val, verdict))
    print('%s %s %s' % (key, val, verdict))


def judge(key, val, tol):
    put(key, '%.3e (tol %.2e)' % (val, tol), 'PASS' if val <= tol
        else 'FAIL')


def upper(c):
    return {k for k in c if k[0] < k[1]}


def main():
    work, logs, inputs = sys.argv[1], sys.argv[2], sys.argv[3]
    rows = []
    for name, lgstor, dim, writer, exact, title in CASES:
        ctol, vtol, drop, negt, lab = WRITERS[writer]
        k0 = len(results)
        f_keep = os.path.join(work, name + '_keep.stor')
        f_def = os.path.join(work, name + '.stor')
        if not (os.path.exists(f_keep) and os.path.exists(f_def)):
            put(name + '_outputs', 'missing', 'FAIL')
            rows.append((name, title, results[k0:], None))
            continue
        xyz, cells = read_inp(os.path.join(inputs, name + '.inp'))
        lg = read_stor(os.path.join(inputs, lgstor))
        dv = read_stor(f_keep)
        de = read_stor(f_def)
        cl, cd, ce = stor_pairs(lg), stor_pairs(dv), stor_pairs(de)
        cmax = max(abs(v) for v in cd.values())
        vmax = max(abs(v) for v in dv['vol'])

        def zero(k):
            d = math.dist(xyz[k[0]], xyz[k[1]])
            return abs(cd[k]) <= ZERO_TOL * d ** (dim - 2)

        judge(name + '_box_volume', abs(sum(dv['vol']) - exact) / exact,
              1e-13)
        put(name + '_lagrit_balance', '%.3e' % (abs(sum(lg['vol']) -
                                                    exact) / exact),
            'MEASURED')
        only_lg = upper(cl) - upper(cd)
        put(name + '_pairs_lagrit', '%d pairs only in LaGriT'
            % len(only_lg), 'FAIL' if only_lg else 'PASS')
        dropped = sorted(upper(cd) - upper(cl))
        big = [k for k in dropped if abs(cd[k]) > drop * cmax
               and not zero(k)]
        put(name + '_pairs_dropped', '%d pairs left out by LaGriT, %d '
            'of them above %g max |A/d|' % (len(dropped), len(big), drop),
            'FAIL' if big else 'PASS')
        common = [k for k in cd if k in cl]
        judge(name + '_coef_diff', max(abs(cd[k] - cl[k]) for k in common)
              / cmax, ctol)

        zero_d = {k for k in dropped if zero(k)}
        real_d = [k for k in dropped if k not in zero_d]
        touched_real = {i for k in real_d for i in k}
        dvol = [abs(a - b) / vmax for a, b in zip(dv['vol'], lg['vol'])]
        judge(name + '_vol_diff', max(d for i, d in enumerate(dvol)
                                      if i not in touched_real), vtol)
        if touched_real:
            put(name + '_vol_diff_dropped', '%.3e at the %d vertices of '
                'the %d nonzero pairs LaGriT left out'
                % (max(dvol[i] for i in touched_real), len(touched_real),
                   len(real_d)), 'MEASURED')
        want = {k for k in upper(cd) if zero(k)}
        neg_d = {k for k in upper(cd) if cd[k] > negt * cmax}
        neg_l = {k for k in upper(cl) if cl[k] > negt * cmax}
        mind = -max(cd[k] for k in upper(cd) if k not in want)
        put(name + '_negative', '%d in del_vor, %d in LaGriT; minimum '
            'A/d %.6e and %.6e (zero-area pairs aside)'
            % (len(neg_d), len(neg_l), mind,
               -max(cl[k] for k in upper(cl))),
            'PASS' if neg_d == neg_l else 'FAIL')

        gone = upper(cd) - upper(ce)
        put(name + '_default_zero', '%d left out by del_vor (zero-area), '
            '%d by LaGriT' % (len(gone), len(dropped)),
            'PASS' if gone == want and gone <= set(dropped) else 'FAIL')

        pic = name + '.svg'
        if dim == 3:
            picture(os.path.join(logs, pic), name, title, xyz, cd, neg_d,
                    zero_d, real_d)
        else:
            picture_2d(os.path.join(logs, pic), name, title, xyz, cells,
                       cd, cl, cmax, ctol)
        hist = name + '_diff.svg'
        histogram(os.path.join(logs, hist), name, cd, cl, common, cmax,
                  ctol, lab)
        rows.append((name, title, results[k0:], (pic, hist)))

    write_page(logs, rows)
    print('failures %d' % nfail)
    return 1 if nfail else 0


def picture_2d(path, name, title, xyz, cells, cd, cl, cmax, ctol):
    """The triangles in the plane, each edge colored by
    |c - c(LaGriT)| / max |c| on a log scale from 1e-16 to the
    tolerance; red if above it."""
    W, H = 700, 760
    s = Svg(W, H)
    s.text(20, 24, '%s: %d vertices, %d triangles' % (name, len(xyz),
                                                       len(cells)),
           size=14, bold=True)
    s.text(20, 42, title, size=12)
    pr = Projector(xyz, 40, 60, 620, 600, view='plan')
    lo, hi = -16.0, math.log10(ctol)
    seen = set()
    for c in cells:
        for a, b in ((c[0], c[1]), (c[0], c[2]), (c[1], c[2])):
            k = (min(a, b), max(a, b))
            if k in seen:
                continue
            seen.add(k)
            x1, y1 = pr(xyz[a])
            x2, y2 = pr(xyz[b])
            if k in cl and k in cd:
                d = abs(cd[k] - cl[k]) / cmax
                col = ('#c00' if d > ctol else
                       ramp((math.log10(max(d, 1e-16)) - lo) / (hi - lo)))
            else:
                col = '#c00'
            s.line(x1, y1, x2, y2, color=col, width=1.0)
    s.legend(40, H - 58, 300, 12, '1e-16 (or equal)', '%g' % ctol,
             '|A/d - A/d(LaGriT)| / max |A/d| on each edge')
    s.save(path)


def picture(path, name, title, xyz, c, neg, zero, real_dropped):
    """The box (outline) and every vertex (gray); connections with a
    negative coefficient in red, zero-area connections (left out by
    both codes) in orange, nonzero connections LaGriT left out in
    purple."""
    W, H = 760, 600
    s = Svg(W, H)
    s.text(20, 24, '%s (%s): %d vertices' % (name, title, len(xyz)),
           size=14, bold=True)
    pr = Projector(xyz, 40, 50, 680, 440, view='iso')
    bx, by, bz = BOX
    corners = [(x, y, z) for x in (0, bx) for y in (0, by)
               for z in (0, bz)]
    for a in corners:
        for b in corners:
            if a < b and sum(1 for q in range(3) if a[q] != b[q]) == 1:
                x1, y1 = pr(a)
                x2, y2 = pr(b)
                s.line(x1, y1, x2, y2, color='#333', width=1.4)
    for p in xyz:
        x, y = pr(p)
        s.circle(x, y, 1.2, fill='#bbb')
    for keys, col, w in ((zero, '#e67e22', 1.0), (neg, '#c0392b', 2.2),
                         (real_dropped, '#7d3c98', 3.0)):
        for (i, j) in keys:
            x1, y1 = pr(xyz[i])
            x2, y2 = pr(xyz[j])
            s.line(x1, y1, x2, y2, color=col, width=w)
    y = H - 70
    for lab, col, n in (('negative A/d (same in LaGriT)', '#c0392b',
                         len(neg)),
                        ('zero area, left out by both codes', '#e67e22',
                         len(zero)),
                        ('A/d ~ 1e-9 of max: kept by del_vor, left out '
                         'by LaGriT', '#7d3c98', len(real_dropped))):
        s.line(30, y - 4, 60, y - 4, color=col, width=3)
        s.text(68, y, '%s: %d' % (lab, n), size=12)
        y += 20
    s.save(path)


def histogram(path, name, cd, cl, common, cmax, ctol, lab):
    """Number of vertex pairs by |c - c(LaGriT)| / max |c| (log bins),
    with the tolerance marked."""
    W, H = 820, 320
    s = Svg(W, H)
    s.text(20, 24, '%s: del_vor against LaGriT, per vertex pair'
           % name, size=14, bold=True)
    lo, hi = -18, -6
    bins = [0] * (hi - lo)
    nzero = 0
    for k in common:
        if k[0] > k[1]:
            continue
        d = abs(cd[k] - cl[k]) / cmax
        if d == 0:
            nzero += 1
            continue
        b = int(math.floor(math.log10(d))) - lo
        bins[min(max(b, 0), len(bins) - 1)] += 1
    x0, x1, y0, y1 = 80, 600, 50, 230
    top = max(bins + [nzero, 1])
    bw = (x1 - x0) / (len(bins) + 1)
    for k, n in enumerate([nzero] + bins):
        h = (y1 - y0) * n / top
        x = x0 + k * bw
        s.rect(x + 1, y1 - h, bw - 2, h,
               fill='#888' if k == 0 else '#2b6cb0')
        if n:
            s.text(x + bw / 2, y1 - h - 4, str(n), size=10,
                   anchor='middle')
        blab = '0' if k == 0 else '1e%d' % (lo + k - 1)
        s.text(x + bw / 2, y1 + 14, blab, size=9, anchor='middle')
    xe = x0 + (1 + (math.log10(ctol) - lo)) * bw
    s.line(xe, y0 - 5, xe, y1, color='#c0392b', width=1.5, dash='4,3')
    words = lab.split(' ')
    half = (len(words) + 1) // 2
    s.text(xe + 6, y0 + 10, ' '.join(words[:half]), size=11,
           color='#c0392b')
    s.text(xe + 6, y0 + 25, ' '.join(words[half:]), size=11,
           color='#c0392b')
    s.text(20, H - 34, '|A/d difference| / max |A/d|, bins of one '
           'decade (left edge shown); first bar: identical values.',
           size=11)
    s.text(20, H - 16, 'The peak at 1e-13 is the 13 digits of the files: '
           'the .inp coordinates and .stor values both keep 13 '
           'significant digits.', size=11)
    s.save(path)


def write_page(logs, rows):
    suite = os.path.dirname(os.path.abspath(logs))
    rel = os.path.relpath(logs, suite)
    h = ['<!doctype html><html><head><meta charset="utf-8">',
         '<title>test_poisson_lagrit</title><style>',
         'body{font-family:Helvetica,Arial,sans-serif;margin:24px;'
         'max-width:1100px}',
         'td,th{padding:3px 8px;border-bottom:1px solid #ddd;'
         'font-size:13px;text-align:left}',
         '.PASS{color:#1a7f37;font-weight:bold}'
         '.FAIL{color:#c00;font-weight:bold}.MEASURED{color:#666}',
         'img{max-width:100%;border:1px solid #ccc;margin:8px 0}',
         'td{vertical-align:top}',
         'details summary{cursor:pointer;color:#0969da;font-size:12px}',
         'details p{max-width:800px;margin:4px 0;font-size:13px}',
         '</style></head><body>',
         '<p style="font-size:13px"><a href="../summary.html">&larr; all '
         'suites</a></p>',
         '<h1>test_poisson_lagrit: %s</h1>' % ('PASS' if not nfail
                                               else 'FAIL'),
         "<p><b>What it does:</b> del_vor against LaGriT's own .stor "
         "files (dump / stor) on "
         "five Poisson disk meshes from LaGriT's suites: test_poisson_3d "
         'cases 1 and 2 (box 0.4 x 0.4 x 0.2) and the three meshes of '
         'test_poisson_2d (unit square). In 3D LaGriT\'s anothermatbld3d '
         'compresses with tolerance 1e-8 of the largest coefficient and '
         'leaves out coefficients below that; in 2D matbld2d writes '
         'every value, but the .inp files keep 13 digits (5e-11). The '
         'checks allow exactly this. See 00README.</p>']
    for name, title, rs, pics in rows:
        h.append('<h2>%s: %s</h2><details><summary>What it does'
                 '</summary><p>%s</p></details><table><tr><th>check</th>'
                 '<th>value</th><th>result</th></tr>'
                 % (name, title, ABOUT.get(name, '')))
        for k, v, r in rs:
            h.append('<tr><td>%s</td><td>%s</td><td class="%s">%s</td>'
                     '</tr>' % (k, v, r, r))
        h.append('</table>')
        for p in pics or ():
            h.append('<img src="%s/%s" alt="%s">' % (rel, p, p))
    h.append('<p>Run logs: <a href="%s/">%s/</a></p></body></html>'
             % (rel, rel))
    with open(os.path.join(suite, 'summary.html'), 'w') as f:
        f.write('\n'.join(h) + '\n')


if __name__ == '__main__':
    sys.exit(main())
