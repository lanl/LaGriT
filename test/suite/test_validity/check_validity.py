"""check_validity.py WORK LOGS

test_validity: judges each case of cases.txt (run in WORK/NAME by
run_validity_tests.sh), draws each mesh, writes summary.html in this
suite's folder. Python 3 standard library. Exit 1 on any FAIL.

For each case:
  exit     the exit status cases.txt expects
  message  the expected text is in the screen output (spaces in a row
           count as one)
  files    a failed run wrote no .stor and no violations file; a
           passing run wrote its .stor
  first    (failed mesh checks) the cells the message names as
           "first" are drawn red in the picture
  same_as  (odd_text) its .stor equals two_materials's, apart from the
           date line: the reader gives the same numbers for CR LF line
           ends, tabs and Fortran D exponents
"""

import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, Projector  # noqa: E402


def read_mesh(path):
    """Vertices and cells as written (AVS order), any cell type."""
    with open(path) as f:
        lines = [ln for ln in f if ln.strip()]
    nn, nc = int(lines[0].split()[0]), int(lines[0].split()[1])
    xyz = [tuple(float(v) for v in ln.split()[1:4])
           for ln in lines[1:1 + nn]]
    cells = []
    for ln in lines[1 + nn:1 + nn + nc]:
        t = ln.split()
        cells.append((t[2], int(t[1]), [int(v) - 1 for v in t[3:]]))
    return xyz, cells


def picture(path, title, xyz, cells, bad):
    """Every cell's edges (fracture 2 in blue), vertex numbers, the
    cells named in the message filled red."""
    W, H = 520, 440
    s = Svg(W, H)
    s.text(14, 20, title, size=13, bold=True)
    flat = all(p[2] == 0 for p in xyz)
    pr = Projector(xyz, 30, 40, 460, 340, view='plan' if flat else 'iso')
    for k, (kind, clr, v) in enumerate(cells):
        v = [x for x in v if 0 <= x < len(xyz)]
        if k + 1 in bad and len(v) >= 3:
            for tri in ([v[0], v[1], v[2]],) if len(v) == 3 else \
                    ([v[0], v[1], v[2]], [v[0], v[1], v[3]],
                     [v[0], v[2], v[3]], [v[1], v[2], v[3]]):
                s.polygon([pr(xyz[q]) for q in tri], fill='#c0392b',
                          stroke='none', opacity=0.35)
        col = '#2b6cb0' if clr == 2 else '#444'
        for a in range(len(v)):
            for b in range(a + 1, len(v)):
                if kind == 'quad' and (a, b) in ((0, 2), (1, 3)):
                    continue
                x1, y1 = pr(xyz[v[a]])
                x2, y2 = pr(xyz[v[b]])
                s.line(x1, y1, x2, y2, color=col, width=1.2)
    for i, p in enumerate(xyz):
        x, y = pr(p)
        s.circle(x, y, 3, fill='#222')
        s.text(x + 5, y - 5, str(i + 1), size=10, color='#555')
    s.text(14, H - 14, 'red: the cells the message names as first; '
           'blue: itetclr 2', size=11, color='#555')
    s.save(path)


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows = []
    for name, expect, msg, about in read_cases(
            os.path.join(HERE, 'cases.txt'), 4):
        d = os.path.join(work, name)
        cmd = ('del_vor -avs %s.inp -stor %s.stor -report %s.report'
               % (name, name, name))
        checks = []

        def put(c, v, ok):
            checks.append((c, v, 'PASS' if ok else 'FAIL'))
            print('%s_%s %s %s' % (name, c, v, 'PASS' if ok else 'FAIL'))

        try:
            rc = int(open(os.path.join(d, 'run.rc')).read())
            out = open(os.path.join(d, 'run.out')).read()
        except (OSError, ValueError):
            put('ran', 'no run.rc', False)
            rows.append((name, about, cmd, checks, None))
            continue
        put('exit', '%d (expected %s)' % (rc, expect), rc == int(expect))
        flat = ' '.join(out.split())
        put('message', '"%s" %s' % (msg, 'found' if ' '.join(msg.split())
                                     in flat else 'NOT found'),
            ' '.join(msg.split()) in flat)
        stor = os.path.exists(os.path.join(d, name + '.stor'))
        vtu = os.path.exists(os.path.join(d, 'violations.vtu'))
        if int(expect):
            put('files', 'no .stor, no violations file',
                not stor and not vtu)
        else:
            put('files', '.stor written', stor)
        bad = set()
        for m in re.finditer(r'first: cells?\s+(\d+)(?:\s+and\s+(\d+))?',
                             out):
            bad |= {int(x) for x in m.groups() if x}
        if name == 'odd_text':
            def body(n):
                f = os.path.join(work, n, n + '.stor')
                if not os.path.exists(f):
                    return None
                return open(f).read().split('\n')[2:]
            a, b = body('odd_text'), body('two_materials')
            put('same_as', '.stor equal to two_materials\'s (date '
                'aside)' if a and a == b else 'differs from '
                'two_materials\'s', bool(a) and a == b)
            rows.append((name, about, cmd, checks, None))
            continue
        xyz, cells = read_mesh(os.path.join(d, name + '.inp'))
        pic = name + '.svg'
        picture(os.path.join(logs, pic), name, xyz, cells, bad)
        rows.append((name, about, cmd, checks, pic))

    what = ('runs del_vor on 17 small meshes made by '
            'make_validity_inputs.py. Eleven break one of the rules del_vor '
            'checks before any geometry, each a fatal error: the cell '
            'types (all triangles or all tets), cells of positive size, '
            'conformity (a tet face in at most 2 tets; a triangle edge in '
            'at most 2 triangles of one itetclr) and orientation '
            '(neighboring triangles of one itetclr run along their shared '
            'edge in opposite directions). Each must stop with exit 1, '
            'say what is wrong and name the first cell, and write '
            'nothing. Six are legal meshes that look unusual (crossing '
            'fractures, clockwise triangles, an unused vertex, two '
            'materials, wrapped vertex data, CR LF / tabs / D '
            'exponents) and must run to the end.')
    nfail = write_case_page(os.path.dirname(os.path.abspath(logs)), logs,
                            'test_validity', what, rows)
    print('failures %d' % nfail)
    return 1 if nfail else 0


if __name__ == '__main__':
    sys.exit(main())
