"""check_control_volume.py WORK LOGS

test_cv_dual_control_volume: judges each case of cases.txt, run by
run_control_volume_tests.sh (LaGriT on the deck NAME.lgi in WORK/NAME;
the cv_dual program on the mesh it wrote, in WORK/NAME/prog). Draws
each mesh with its control volumes, writes summary.html in this
suite's folder. Python 3 standard library. Exit 1 on any FAIL.

Checks:
  ran        LaGriT ran the whole deck: after.inp, written after the
             command, exists
  errors     the number of "ERROR: dump/control_volume" lines is the one
             cases.txt expects
  message    each expected text is in LaGriT's output
  nothing    no file whose name starts with bad (the failed commands'
             outputs) was written
  files      the command wrote the same files as the program, and no
             others
  bytes      every file equal byte for byte, the .stor's second line
             aside
  line2      the .stor's second line ends with (cv_dual dump /
             control_volume / MO / ...), the LaGriT command
  screen     the program's screen lines appear, in order, in LaGriT's
             output (the command sends cv_dual's lines to LaGriT's log)
  attribute  the vertex attribute is in after.inp and equals the .stor's
             volumes in every digit; with none asked for, after.inp has
             the same attributes as mesh.inp
"""

import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(SUITE, 'cv_dual_tools'))
sys.path.insert(0, os.path.join(SUITE, 'test_cv_dual_voronoi_gold'))
from exact_check import read_avs  # noqa: E402
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, ramp, Projector  # noqa: E402
from check_voronoi_gold import read_stor  # noqa: E402

# what a case folder holds besides the command's outputs
KNOWN = {'mesh.inp', 'after.inp', 'run.out', 'run.rc', 'prog',
         'lagrit.out', 'lagrit.log', 'outx3dgen', 'logx3dgen',
         'full_mesh.inp', 'two_mat.inp', 'test_3d_poi_01.inp',
         'flip3d.inp'}


def avs_attributes(path):
    """Vertex attribute names and their values (one list per name)."""
    with open(path) as f:
        lines = [ln for ln in f if ln.strip()]
    h = lines[0].split()
    nn, nc, nnd = int(h[0]), int(h[1]), int(h[2])
    if nnd == 0:
        return {}
    k = 1 + nn + nc
    ncomp = [int(v) for v in lines[k].split()[1:]]
    names = [lines[k + 1 + a].split(',')[0].strip()
             for a in range(len(ncomp))]
    rows = ' '.join(lines[k + 1 + len(ncomp):]).split()
    per = 1 + sum(ncomp)
    out = {}
    col = 1
    for a, nm in enumerate(names):
        out[nm] = [float(rows[i * per + col]) for i in range(nn)]
        col += ncomp[a]
    return out


def program_lines(text):
    """The program's screen lines but its options block and the
    reading line (the command has no mesh file)."""
    out, in_opt = [], False
    for ln in text.split('\n'):
        if ln.startswith('cv_dual: options'):
            in_opt = True
            continue
        if in_opt and ln.startswith('  '):
            continue
        in_opt = False
        if ln.startswith('cv_dual: reading ') or not ln.strip():
            continue
        if ln.startswith('STOP'):
            continue
        out.append(ln.rstrip())
    return out


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows = []
    for name, args, att, nerr, msgs, about in read_cases(
            os.path.join(HERE, 'cases.txt'), 6):
        d = os.path.join(work, name)
        checks = []

        def put(c, v, ok):
            checks.append((c, v, 'PASS' if ok else 'FAIL'))
            print('%s_%s %s %s' % (name, c, v, 'PASS' if ok else 'FAIL'))

        deck = open(os.path.join(HERE, name + '.lgi')).read()
        cmds = [ln.strip() for ln in deck.split('\n')
                if ln.startswith('dump / control_volume')]
        cmd = '\n'.join(cmds)
        if args != '-':
            cmd += '\ncv_dual -avs mesh.inp ' + args
        try:
            out = open(os.path.join(d, 'run.out'),
                       errors='replace').read()
        except OSError:
            put('ran', 'no run.out', False)
            rows.append((name, about, cmd, checks, None))
            continue
        put('ran', 'after.inp written' if os.path.exists(
            os.path.join(d, 'after.inp')) else 'no after.inp',
            os.path.exists(os.path.join(d, 'after.inp')))
        ne = out.count('ERROR: dump/control_volume')
        put('errors', '%d ERROR lines (expected %s)' % (ne, nerr),
            ne == int(nerr))
        flat = ' '.join(out.split())
        if msgs != '-':
            for m in msgs.split(';;'):
                m = ' '.join(m.split())
                put('message', '"%s" %s' % (m, 'found' if m in flat
                                             else 'NOT found'), m in flat)
        files = set(os.listdir(d)) - {name + '.lgi'}
        bad = sorted(f for f in files if f.startswith('bad'))
        put('nothing', 'no bad* file' if not bad else 'wrote '
            + ' '.join(bad), not bad)

        stor_vol = None
        if args != '-':
            p = os.path.join(d, 'prog')
            prc = -1
            try:
                prc = int(open(os.path.join(p, 'run.rc')).read())
            except (OSError, ValueError):
                pass
            pf = set(os.listdir(p)) - {'mesh.inp', 'run.out', 'run.rc'} \
                if os.path.isdir(p) else set()
            mine = files - KNOWN - set(bad)
            put('files', '%d: %s' % (len(pf), ' '.join(sorted(pf)))
                if mine == pf and prc == 0 else 'command %s, program %s '
                '(exit %d)' % (sorted(mine), sorted(pf), prc),
                mine == pf and prc == 0)
            differ = []
            for f in sorted(pf & mine):
                a = open(os.path.join(d, f), 'rb').read()
                b = open(os.path.join(p, f), 'rb').read()
                if f.endswith('.stor'):
                    la, lb = a.split(b'\n'), b.split(b'\n')
                    same = la[:1] + la[2:] == lb[:1] + lb[2:]
                    l2 = la[1].decode('latin-1').rstrip()
                    ok2 = re.search(r'\(cv_dual dump / control_volume / '
                                    r'\S+.*\)$', l2) is not None
                    put('line2', '%s: ...%s' % (f, l2[-70:]), ok2)
                    if stor_vol is None:
                        stor_vol = read_stor(os.path.join(d, f))['vol']
                else:
                    same = a == b
                if not same:
                    differ.append(f)
            put('bytes', 'all %d equal' % len(pf & mine) if not differ
                else 'differ: ' + ' '.join(differ), not differ)
            mine_l = [ln.strip() for ln in out.split('\n')]
            want = [ln.strip() for ln in program_lines(open(
                os.path.join(p, 'run.out')).read())]
            k = 0
            for ln in mine_l:
                if k < len(want) and ln == want[k]:
                    k += 1
            put('screen', '%d of %d program lines, in order' % (
                k, len(want)), k == len(want))

        if os.path.exists(os.path.join(d, 'after.inp')) and \
                os.path.exists(os.path.join(d, 'mesh.inp')):
            before = avs_attributes(os.path.join(d, 'mesh.inp'))
            after = avs_attributes(os.path.join(d, 'after.inp'))
            if att != '-':
                ok = att in after and stor_vol is not None and \
                    after[att] == stor_vol
                put('attribute', '%s: %d values equal to the .stor\'s'
                    % (att, len(stor_vol or [])) if ok else
                    '%s missing or different' % att, ok)
            else:
                ok = sorted(after) == sorted(before)
                put('attribute', 'none made: attributes %s' %
                    ' '.join(sorted(after)) if ok else 'changed: %s -> '
                    '%s' % (sorted(before), sorted(after)), ok)

        pic = name + '.svg'
        try:
            picture(os.path.join(logs, pic), name, d, att, checks)
        except (OSError, ValueError, IndexError, KeyError):
            pic = None
        rows.append((name, about, cmd, checks, pic))

    what = ('runs LaGriT\'s command <code>dump / control_volume</code> '
            '(cv_dual, called through its entry point with the mesh '
            'object\'s own arrays) on meshes LaGriT makes or reads: '
            'tets, 2D triangles, a fracture network, two materials, a '
            'Poisson mesh. Every file the command writes must equal '
            'the cv_dual program\'s on the mesh the deck wrote '
            '(<code>dump / avs</code>), the .stor\'s second line must '
            'end with the LaGriT command, cv_dual\'s lines must reach '
            'LaGriT\'s log, and <code>attribute_cv_volume</code> must '
            'put the .stor\'s volumes in a vertex attribute. Bad '
            'commands and meshes cv_dual refuses must give an ERROR, '
            'write nothing, and leave LaGriT going. The pictures show '
            'each mesh colored by its control volumes (from the '
            'attribute), gray where there is none.')
    nfail = write_case_page(HERE, logs, 'test_cv_dual_control_volume',
                            what, rows)
    sys.exit(1 if nfail else 0)


def picture(path, name, d, att, checks):
    """The mesh (an oblique view; a large tet mesh by its boundary
    faces), its vertices colored by the attribute when there is one."""
    src = os.path.join(d, 'after.inp')
    if not os.path.exists(src):
        src = os.path.join(d, 'mesh.inp')
    m = read_avs(src)
    vals = None
    if att != '-':
        vals = avs_attributes(src).get(att)
    W, H = 470, 430
    s = Svg(W, H)
    nfail = sum(1 for c in checks if c[2] == 'FAIL')
    s.text(14, 22, '%s: %s' % (name, 'PASS' if not nfail else
                               '%d FAIL' % nfail), size=14, bold=True)
    flat = all(abs(p[2] - m.xyz[0][2]) < 1e-300 for p in m.xyz)
    pr = Projector(m.xyz, 20, 40, W - 40, H - 125,
                   view='plan' if flat else 'iso')
    edges = set()
    nen = len(m.cells[0]) if m.cells else 0
    if nen == 4 and len(m.cells) > 1500:
        nf = {}
        for c in m.cells:
            for f in ((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3)):
                key = tuple(sorted(c[i] for i in f))
                nf[key] = nf.get(key, 0) + 1
        for f, n in nf.items():
            if n == 1:
                edges |= {(f[0], f[1]), (f[0], f[2]), (f[1], f[2])}
    else:
        for c in m.cells:
            for a in range(len(c)):
                for b in range(a + 1, len(c)):
                    edges.add((min(c[a], c[b]), max(c[a], c[b])))
    for a, b in edges:
        x1, y1 = pr(m.xyz[a])
        x2, y2 = pr(m.xyz[b])
        s.line(x1, y1, x2, y2, color='#bbb', width=0.5)
    r = 3.0 if len(m.xyz) < 300 else 1.6
    if vals:
        lo, hi = min(vals), max(vals)
        for p, v in zip(m.xyz, vals):
            x, y = pr(p)
            t = (v - lo) / (hi - lo) if hi > lo else 0.5
            s.circle(x, y, r, fill=ramp(t))
        s.legend(20, H - 50, 200, 10, '%.3g' % lo, '%.3g' % hi,
                 'attribute %s (control volume)' % att)
    else:
        for p in m.xyz:
            x, y = pr(p)
            s.circle(x, y, r, fill='#888')
        s.text(20, H - 40, 'no attribute asked for, or nothing computed:'
               ' vertices gray', size=11, color='#555')
    s.text(20, H - 14, '%d vertices, %d cells' % (len(m.xyz),
                                                  len(m.cells)),
           size=11, color='#555')
    s.save(path)


if __name__ == '__main__':
    main()
