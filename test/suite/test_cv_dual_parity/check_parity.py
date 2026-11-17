"""check_parity.py WORK LOGS BIG

test_parity: for each case of cases.txt (run in WORK/NAME by
run_parity_tests.sh), compares del_vor's .stor files with LaGriT's by
../../../tools/stor_compare.py, draws the mesh with every difference
that is not printed digits, writes summary.html in this suite's
folder. Python 3 standard library. Exit 1 on any FAIL.

For each case:
  exit          both del_vor runs exit 0
  keep_vs_lg    del_vor (-zero_area keep -compress off) against LaGriT's
                default dump / stor: every difference explained
  keep_vs_none  the same against LaGriT's uncompressed file (3D only;
                LaGriT's 2D writer does not compress)
  default_vs_lg del_vor's defaults (zero-area pairs left out, compressed)
                against LaGriT's default: every difference explained
  MEASURED      the largest coefficient and volume differences, the
                labels found, the volume totals of both codes
The labels and their allowances are defined at the top of
stor_compare.py.
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(SUITE)),
                                'tools'))
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, Projector  # noqa: E402
import stor_compare  # noqa: E402

COLORS = {'round_off': '#1a7f37', 'merge': '#2b6cb0',
          'coord_digits': '#805ad5', 'abs_coords': '#b7791f',
          'dropped': '#d53f8c', 'dropped_share': '#d53f8c',
          'zero_area': '#dd6b20', 'UNEXPLAINED': '#c00'}
MAX_EDGES = 30000      # draw every mesh edge only up to this many


def labels(d):
    return ', '.join('%s %d' % (k, v) for k, v in sorted(d.items())) or '-'


def picture(path, title, mesh, res):
    """The mesh (every edge, faint, when not too many) and each pair and
    vertex whose difference is not printed digits, colored by label."""
    xyz, cells = stor_compare.read_mesh(mesh)
    W, H = 620, 560
    s = Svg(W, H)
    s.text(14, 20, title, size=13, bold=True)
    flat = len({round(p[2], 12) for p in xyz}) == 1
    pr = Projector(xyz, 20, 36, 580, 400, view='plan' if flat else 'iso')
    edges = set()
    for c in cells:
        for a in range(len(c)):
            for b in range(a + 1, len(c)):
                edges.add((min(c[a], c[b]), max(c[a], c[b])))
    whole = len(edges) <= MAX_EDGES
    if whole:
        draw = edges
    else:
        # too many to draw: the boundary only (triangles: edges in one
        # triangle, each fracture's outline; tets: the edges of the
        # faces in one tet)
        seen = {}
        for c in cells:
            if len(c) == 3:
                fs = [(c[0], c[1]), (c[1], c[2]), (c[2], c[0])]
            else:
                fs = [(c[0], c[1], c[2]), (c[0], c[1], c[3]),
                      (c[0], c[2], c[3]), (c[1], c[2], c[3])]
            for f in fs:
                k = tuple(sorted(f))
                seen[k] = seen.get(k, 0) + 1
        draw = set()
        for f, n in seen.items():
            if n == 1:
                for a in range(len(f)):
                    for b in range(a + 1, len(f)):
                        draw.add((f[a], f[b]))
    for i, j in draw:
        x1, y1 = pr(xyz[i])
        x2, y2 = pr(xyz[j])
        s.line(x1, y1, x2, y2, color='#bbb' if whole else '#888',
               width=0.4 if whole else 0.6)
    count = {}
    for (i, j), lb in res['pair_where'].items():
        x1, y1 = pr(xyz[i])
        x2, y2 = pr(xyz[j])
        s.line(x1, y1, x2, y2, color=COLORS.get(lb, '#000'),
               width=1.6 if whole else 2.6)
        count['pair ' + lb] = count.get('pair ' + lb, 0) + 1
    for i, lb in res['vertex_where'].items():
        x, y = pr(xyz[i])
        s.circle(x, y, 2.2, fill=COLORS.get(lb, '#000'))
        count['volume ' + lb] = count.get('volume ' + lb, 0) + 1
    y = 452
    s.text(14, y, 'gray: %s; colored: pairs (lines) and volumes '
           '(dots) not equal to the printed digits'
           % ('mesh edges' if whole else 'the boundary (%d edges; all '
              '%d not drawn)' % (len(draw), len(edges))),
           size=10, color='#555')
    x = 14
    y += 18
    for k in sorted(count):
        lb = k.split(' ', 1)[1]
        s.rect(x, y - 9, 12, 9, fill=COLORS.get(lb, '#000'))
        s.text(x + 16, y, '%s: %d' % (k, count[k]), size=11)
        x += 200
        if x > 450:
            x = 14
            y += 16
    if not count:
        s.text(14, y, 'every pair and volume equal to the 13 printed '
               'digits', size=11, color='#1a7f37')
    s.save(path)


def main():
    work, logs, big = sys.argv[1], sys.argv[2], sys.argv[3]
    rows = []
    for name, mesh, about in read_cases(os.path.join(HERE, 'cases.txt'), 3):
        d = os.path.join(work, name)
        cmd = ('del_vor -avs %s.inp -stor %s_keep.stor -zero_area keep '
               '-compress off; del_vor -avs %s.inp -stor %s.stor'
               % (name, name, name, name))
        checks = []

        def put(c, v, ok):
            res = ok if isinstance(ok, str) else ('PASS' if ok else 'FAIL')
            checks.append((c, v, res))
            print('%s_%s %s %s' % (name, c, v, res))

        if not os.path.exists(os.path.join(d, 'run.rc')):
            if mesh.startswith('big/'):
                put('skipped', 'not in DEL_VOR_BIG (%s)' % big, 'MEASURED')
            else:
                put('ran', 'no run.rc', False)
            rows.append((name, about, cmd, checks, None))
            continue
        rc = [int(open(os.path.join(d, f)).read())
              for f in ('run_keep.rc', 'run.rc')]
        put('exit', '%d %d (expected 0 0)' % tuple(rc), rc == [0, 0])
        if rc != [0, 0]:
            rows.append((name, about, cmd, checks, None))
            continue
        m = os.path.join(d, name + '.inp')
        keep = os.path.join(d, name + '_keep.stor')
        lg = os.path.join(d, name + '_lg.stor')
        none = os.path.join(d, name + '_lg_none.stor')
        runs = [('keep_vs_lg', keep, lg)]
        if os.path.exists(none):
            runs.append(('keep_vs_none', keep, none))
        runs.append(('default_vs_lg', os.path.join(d, name + '.stor'), lg))
        first = None
        for tag, a, b in runs:
            res, A, B = stor_compare.compare(a, b, mesh=m)
            first = first or (res, A, B)
            pp = res['pairs']
            put(tag, 'pairs %d common, only del_vor %d (%s), only LaGriT %d'
                ' (%s); values %s; volumes %s%s'
                % (pp['common'], pp['only_A'], labels(pp['only_A_labels']),
                   pp['only_B'], labels(pp['only_B_labels']),
                   labels(res['coef']['labels']),
                   labels(res['vol']['labels']),
                   '' if not (res['layout_A'] or res['layout_B']) else
                   '; layout: ' + '; '.join(res['layout_A'] +
                                            res['layout_B'])),
                not res['bad'])
        res, A, B = first
        put('largest', 'values %.1e, volumes %.1e of the largest; volume '
            'totals del_vor %.15g, LaGriT %.15g; LaGriT writer %s'
            % (res['coef']['max_rel'], res['vol']['max_rel'],
               res['vol']['sum_A'], res['vol']['sum_B'], B['writer']),
            'MEASURED')
        pic = name + '.svg'
        picture(os.path.join(logs, pic), '%s: del_vor against LaGriT '
                '(%s)' % (name, B['writer']), m, res)
        rows.append((name, about, cmd, checks, pic))

    what = ('compares del_vor\'s .stor files with the ones LaGriT\'s '
            'dump / stor wrote for the same mesh (LaGriT read the same '
            '.inp; make_parity_gold.sh made the files, with the LaGriT '
            'named in inputs/LAGRIT_VERSION). tools/stor_compare.py '
            'compares them pair by pair and vertex by vertex and labels '
            'each difference with what explains it: the 13 printed '
            'digits, round-off of small values made by cancellation, '
            'LaGriT\'s 1e-8 compression, LaGriT\'s rule that drops '
            'coefficients below 1e-8 of the largest, zero-area pairs, '
            'and LaGriT\'s 3D tet volumes computed from absolute '
            'coordinates. Any difference with no explanation fails the '
            'case. The picture shows where on the mesh the differences '
            'beyond the printed digits are.')
    nfail = write_case_page(os.path.dirname(os.path.abspath(logs)), logs,
                            'test_parity', what, rows)
    print('failures %d' % nfail)
    return 1 if nfail else 0


if __name__ == '__main__':
    sys.exit(main())
