"""check_dump_keyword.py WORK LOGS

test_cv_dual_dump_keyword: judges each case of cases.txt, run by
run_dump_keyword_tests.sh (LaGriT on the deck NAME.lgi in WORK/NAME;
the cv_dual program on the mesh it wrote, in WORK/NAME/progK). Draws
each mesh with its boundary areas, writes summary.html in this suite's
folder. Python 3 standard library. Exit 1 on any FAIL.

Checks (cases.txt explains the ones a case names):
  ran        LaGriT ran the whole deck: after.inp exists
  errors     the number of "ERROR: dump/" lines is the one expected
  message    each expected text is in LaGriT's output
  nothing    no file whose name starts with bad was written (but
             ROOT_violations.vtu, the flagged facets)
  progK      program run K exited 0 and each file it wrote (but its
             outside_area list) equals the deck's file of that name,
             byte for byte; a .stor's second line aside, which must end
             with (cv_dual dump / ...), the LaGriT command
  eq, zones, areaeq, arealist, boxsum, att, exists, absent, cvline2: see
             cases.txt
"""

import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(SUITE, 'cv_dual_tools'))
from exact_check import read_avs  # noqa: E402
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, ramp, Projector  # noqa: E402

ZONE_SUFFIXES = ['.fehmn', '_material.zone', '_interface.zone',
                 '_multi_mat.zone', '_outside.zone', '_parent.zone']
# a zone's outward normal: (axis, sign)
NORMAL = {'top': (2, 1), 'bottom': (2, -1), 'right_e': (0, 1),
          'left_w': (0, -1), 'back_n': (1, 1), 'front_s': (1, -1)}
LINE2 = re.compile(r'\(cv_dual dump / \S+ / .*\)$')


def avs_attributes(path):
    """Vertex attribute names of an AVS file."""
    with open(path) as f:
        lines = [ln for ln in f if ln.strip()]
    h = lines[0].split()
    nn, nc, nnd = int(h[0]), int(h[1]), int(h[2])
    if nnd == 0:
        return []
    k = 1 + nn + nc
    ncomp = [int(v) for v in lines[k].split()[1:]]
    return [lines[k + 1 + a].split(',')[0].strip()
            for a in range(len(ncomp))]


def same_bytes(a, b, name):
    """Equal byte for byte; a .stor's second line aside."""
    if name.endswith('.stor'):
        la, lb = a.split(b'\n'), b.split(b'\n')
        return la[:1] + la[2:] == lb[:1] + lb[2:]
    return a == b


def line2(path):
    with open(path, 'rb') as f:
        f.readline()
        return f.readline().decode('latin-1').rstrip()


def root_files(d, root):
    """root's files in d: root.xxx or root_xxx, by suffix."""
    out = {}
    for f in os.listdir(d):
        if f.startswith(root) and f[len(root):len(root) + 1] in ('.',
                                                                '_'):
            out[f[len(root):]] = f
    return out


def zone_blocks(path, isarea):
    """[(name, values)] of an FEHM zone or area file: each block's
    header (number, name), nnum, count, then count entries (vertex
    numbers; for an area file, count vectors)."""
    tok = open(path).read().split()
    out, i, name = [], 0, '?'
    while i < len(tok):
        if re.match(r'^\d{5}$', tok[i]) and i + 1 < len(tok):
            name = tok[i + 1]
            i += 2
        elif tok[i] == 'nnum':
            n = int(tok[i + 1])
            k = 3 if isarea else 1
            vals = tok[i + 2:i + 2 + n * k]
            if isarea:
                vals = [tuple(float(x) for x in vals[3 * j:3 * j + 3])
                        for j in range(n)]
            else:
                vals = [int(v) for v in vals]
            out.append((name, vals))
            i += 2 + n * k
        else:
            i += 1
    return out


def zone_areas(d, root, tag):
    """[(zone name, [(vertex, vector)])] from root_outside.zone and
    root_outside_TAG.area (the same order)."""
    z = zone_blocks(os.path.join(d, root + '_outside.zone'), False)
    a = zone_blocks(os.path.join(d, '%s_outside_%s.area' % (root, tag)),
                    True)
    if len(z) != len(a):
        raise ValueError('zone and area files differ in their zones')
    out = []
    for (zn, verts), (an, vecs) in zip(z, a):
        if len(verts) != len(vecs):
            raise ValueError('zone %s: %d vertices, %d vectors'
                             % (zn, len(verts), len(vecs)))
        out.append((zn, list(zip(verts, vecs))))
    return out


def area_numbers(path):
    return [v for _, vecs in zone_blocks(path, True) for vec in vecs
            for v in vec]


def outside_list(path):
    m = {}
    for ln in open(path):
        if ln.startswith('#') or not ln.strip():
            continue
        t = ln.split()
        m[int(t[0])] = tuple(float(x) for x in t[1:4])
    return m


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows = []
    for name, runs, spec, nerr, msgs, about in read_cases(
            os.path.join(HERE, 'cases.txt'), 6):
        d = os.path.join(work, name)
        checks = []
        pic_area = None

        def put(c, v, ok):
            checks.append((c, v, 'PASS' if ok else 'FAIL'))
            print('%s_%s %s %s' % (name, c, v, 'PASS' if ok else 'FAIL'))

        deck = open(os.path.join(HERE, name + '.lgi')).read()
        cmds = [ln.strip() for ln in deck.split('\n')
                if ln.startswith('dump / ') and 'avs' not in ln]
        arglists = [a.strip() for a in runs.split(';;')] \
            if runs != '-' else []
        cmd = '\n'.join(cmds + ['cv_dual -avs mesh.inp ' + a
                                for a in arglists])
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
        ne = out.count('ERROR: dump/')
        put('errors', '%d ERROR lines (expected %s)' % (ne, nerr),
            ne == int(nerr))
        flat = ' '.join(out.split())
        if msgs != '-':
            for m in msgs.split(';;'):
                m = ' '.join(m.split())
                put('message', '"%s" %s' % (m, 'found' if m in flat
                                             else 'NOT found'), m in flat)
        bad = sorted(f for f in os.listdir(d) if f.startswith('bad')
                     and not f.endswith('_violations.vtu'))
        put('nothing', 'no bad* file' if not bad else 'wrote '
            + ' '.join(bad), not bad)

        # the program runs: their files equal the deck's
        for k in range(1, len(arglists) + 1):
            p = os.path.join(d, 'prog%d' % k)
            try:
                prc = int(open(os.path.join(p, 'run.rc')).read())
                pf = sorted(set(os.listdir(p)) -
                            {'mesh.inp', 'run.out', 'run.rc'})
            except (OSError, ValueError):
                put('prog%d' % k, 'did not run', False)
                continue
            files = [f for f in pf if not f.endswith('.txt')]
            differ, l2bad = [], []
            for f in files:
                mine = os.path.join(d, f)
                if not os.path.exists(mine):
                    differ.append(f + ' (not written by LaGriT)')
                    continue
                if not same_bytes(open(mine, 'rb').read(),
                                  open(os.path.join(p, f), 'rb').read(),
                                  f):
                    differ.append(f)
                if f.endswith('.stor') and not LINE2.search(line2(mine)):
                    l2bad.append(f)
            ok = prc == 0 and files and not differ and not l2bad
            put('prog%d' % k, '%s equal to the program\'s%s' % (
                ' '.join(files), ' (line 2 ends with the command)'
                if any(f.endswith('.stor') for f in files) else '')
                if ok else 'exit %d; differ: %s; line 2: %s' % (
                    prc, ' '.join(differ) or '-',
                    ' '.join(l2bad) or 'ok'), ok)

        for s in (spec.split() if spec != '-' else []):
            kind, _, arg = s.partition(':')
            a = arg.split(',')
            try:
                if kind == 'eq':
                    f1, f2 = root_files(d, a[0]), root_files(d, a[1])
                    differ = [x for x in sorted(f1) if x not in f2 or
                              not same_bytes(
                                  open(os.path.join(d, f1[x]), 'rb').read(),
                                  open(os.path.join(d, f2[x]), 'rb').read(),
                                  x)]
                    ok = f1 and sorted(f1) == sorted(f2) and not differ
                    put('eq', '%s* = %s*: %s' % (a[0], a[1], ' '.join(
                        sorted(f1))) if ok else '%s* vs %s*: %s / %s; '
                        'differ %s' % (a[0], a[1], sorted(f1), sorted(f2),
                                       differ), ok)
                elif kind == 'zones':
                    f1, f2 = root_files(d, a[0]), root_files(d, a[1])
                    suf = [x for x in ZONE_SUFFIXES if x in f1 or x in f2]
                    differ = [x for x in suf if x not in f1 or x not in f2
                              or open(os.path.join(d, f1[x]), 'rb').read()
                              != open(os.path.join(d, f2[x]), 'rb').read()]
                    ok = suf and not differ
                    put('zones', '%s, %s: %d files equal (%s)' % (
                        a[0], a[1], len(suf), ' '.join(suf)) if ok else
                        '%s, %s differ: %s' % (a[0], a[1], differ), ok)
                elif kind == 'areaeq':
                    x = area_numbers(os.path.join(d, a[0]))
                    y = area_numbers(os.path.join(d, a[1]))
                    big = max([abs(v) for v in y] or [0.0])
                    dif = [abs(p - q) for p, q in zip(x, y)]
                    worst = max(dif or [0.0])
                    ok = len(x) == len(y) and x and \
                        worst <= 1e-12 * big
                    put('areaeq', '%s = %s: %d numbers, largest '
                        'difference %.1e (largest value %.2e)' % (
                            a[0], a[1], len(x), worst, big), ok)
                elif kind == 'arealist':
                    za = zone_areas(d, a[0], a[1])
                    lst = outside_list(os.path.join(d, 'prog%d' % (
                        next(i for i, s2 in enumerate(arglists, 1)
                             if a[2] in s2)), a[2]))
                    zm = {v: vec for _, vv in za for v, vec in vv}
                    big = max(max(abs(c) for c in v) for v in lst.values())
                    worst = max(max(abs(p - q) for p, q in
                                    zip(zm[v], lst[v])) for v in zm
                                if v in lst)
                    ok = set(zm) <= set(lst) and worst <= 1e-12 * big
                    put('arealist', '%s_outside_%s.area: %d vertices '
                        'equal %s (largest difference %.1e)' % (
                            a[0], a[1], len(zm), a[2], worst) if ok else
                        '%s: %d vertices, %d not in %s, largest '
                        'difference %.1e' % (a[0], len(zm),
                                             len(set(zm) - set(lst)),
                                             a[2], worst), ok)
                    if pic_area is None:
                        pic_area = (lst, '%s areas' % a[1])
                elif kind == 'boxsum':
                    m = read_avs(os.path.join(d, 'mesh.inp'))
                    ext = [max(p[i] for p in m.xyz) -
                           min(p[i] for p in m.xyz) for i in range(3)]
                    worst, n = 0.0, 0
                    for zn, vv in zone_areas(d, a[0], a[1]):
                        if zn not in NORMAL:
                            continue
                        ax, sg = NORMAL[zn]
                        if ext[ax] == 0.0:
                            continue    # a 2D mesh's own plane
                        want = 1.0
                        for i in range(3):
                            if i != ax and ext[i] > 0.0:
                                want *= ext[i]
                        got = sg * sum(vec[ax] for _, vec in vv)
                        worst = max(worst, abs(got - want) / want)
                        n += 1
                    ok = n > 0 and worst <= 1e-12
                    put('boxsum', '%s (%s): %d sides add up to their '
                        'areas (largest relative difference %.1e)' % (
                            a[0], a[1], n, worst), ok)
                elif kind == 'same':
                    # two files equal byte for byte (a .stor's line 2
                    # aside): exclude against the route without it
                    ok = same_bytes(open(os.path.join(d, a[0]), 'rb').read(),
                                    open(os.path.join(d, a[1]), 'rb').read(),
                                    a[0])
                    put('same', '%s %s %s' % (a[0], '=' if ok else
                                              'DIFFERS from', a[1]), ok)
                elif kind == 'ghostsum':
                    # exclude: each side's faces (shared with ghosts)
                    # add up to the side of the box Lx x Ly x Lz (a 0
                    # for the plane of a 2D mesh); every vector in a
                    # side's zone points along that side's normal
                    L = [float(x) for x in a[2:5]]
                    worst, n, off = 0.0, 0, 0.0
                    seen = []
                    for zn, vv in zone_areas(d, a[0], a[1]):
                        ax, sg = NORMAL[zn]
                        want = 1.0
                        for i in range(3):
                            if i != ax and L[i] > 0.0:
                                want *= L[i]
                        got = sg * sum(vec[ax] for _, vec in vv)
                        worst = max(worst, abs(got - want) / want)
                        off = max([off] + [abs(vec[i]) for _, vec in vv
                                           for i in range(3) if i != ax])
                        n += 1
                        seen.append(zn)
                    nside = sum(1 for x in L if x > 0.0) * 2
                    ok = n == nside and worst <= 1e-12 and \
                        off <= 1e-12 * max(L)
                    put('ghostsum', '%s: %d sides (%s) add up to their '
                        'areas (largest relative difference %.1e); '
                        'off-normal parts %.1e' % (
                            a[0], n, ' '.join(seen), worst, off), ok)
                elif kind == 'sidesum':
                    # one side's outside vectors, along its normal, add
                    # up to the value given
                    want = float(a[3])
                    got = None
                    for zn, vv in zone_areas(d, a[0], a[1]):
                        if zn == a[2]:
                            ax, sg = NORMAL[zn]
                            got = sg * sum(vec[ax] for _, vec in vv)
                    ok = got is not None and abs(got - want) <= 1e-12 * \
                        max(abs(want), 1.0)
                    put('sidesum', '%s %s: %s (expected %s)' % (
                        a[0], a[2], got, a[3]), ok)
                elif kind == 'ghostcorner':
                    # how many vertices are in 2 zones, and in 3
                    cnt = {}
                    for zn, verts in zone_blocks(
                            os.path.join(d, a[0] + '_outside.zone'), False):
                        for v in verts:
                            cnt[v] = cnt.get(v, 0) + 1
                    n2 = sum(1 for c in cnt.values() if c == 2)
                    n3 = sum(1 for c in cnt.values() if c == 3)
                    ok = n2 == int(a[1]) and n3 == int(a[2])
                    put('ghostcorner', '%s: %d outside vertices, %d in 2 '
                        'zones (expected %s), %d in 3 (expected %s)' % (
                            a[0], len(cnt), n2, a[1], n3, a[2]), ok)
                elif kind == 'att':
                    have = avs_attributes(os.path.join(d, 'after.inp'))
                    put('att', '%s %s' % (arg, 'in after.inp' if arg in
                                          have else 'NOT in after.inp'),
                        arg in have)
                elif kind == 'exists':
                    ok = os.path.exists(os.path.join(d, arg))
                    put('exists', '%s %s' % (arg, 'written' if ok else
                                             'NOT written'), ok)
                elif kind == 'absent':
                    ok = not os.path.exists(os.path.join(d, arg))
                    put('absent', '%s %s' % (arg, 'not written' if ok else
                                             'WRITTEN'), ok)
                elif kind == 'cvline2':
                    l2 = line2(os.path.join(d, arg))
                    ok = LINE2.search(l2) is not None
                    put('cvline2', '%s: ...%s' % (arg, l2[-60:]), ok)
                else:
                    put(kind, 'unknown check %s' % s, False)
            except (OSError, ValueError, IndexError, KeyError,
                    StopIteration) as e:
                put(kind, '%s: %s' % (s, e), False)

        pic = name + '.svg'
        try:
            picture(os.path.join(logs, pic), name, d, pic_area, checks)
        except (OSError, ValueError, IndexError, KeyError):
            pic = None
        rows.append((name, about, cmd, checks, pic))

    what = ('runs LaGriT\'s <code>dump / stor</code>, <code>fehm</code>, '
            '<code>pflotran</code> and <code>zone_outside</code> with the '
            'keyword <code>cv_dual</code> (the control volumes, '
            'coefficients and boundary areas by cv_dual) and with '
            '<code>matbld</code> (the default, unchanged), on meshes '
            'LaGriT makes or reads: tets, 2D triangles, a fracture '
            'network, a Poisson mesh. Each .stor and .uge must equal the '
            'cv_dual program\'s on the mesh the deck wrote; the geometry '
            'and zone files must equal matbld\'s; the boundary areas '
            'must equal the program\'s, and matbld\'s for Voronoi and '
            'median, and add up to each side of a box. Options cv_dual '
            'does not have leave the dump to matbld; bad commands give '
            'an ERROR and write nothing. The pictures show each mesh, '
            'its boundary vertices colored by the size of their boundary '
            'area.')
    nfail = write_case_page(HERE, logs, 'test_cv_dual_dump_keyword',
                            what, rows)
    sys.exit(1 if nfail else 0)


def picture(path, name, d, pic_area, checks):
    """The mesh (an oblique view; a large tet mesh by its boundary
    faces), boundary vertices colored by |boundary area|."""
    m = read_avs(os.path.join(d, 'mesh.inp'))
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
    if pic_area:
        lst, label = pic_area
        mag = {v: sum(c * c for c in vec) ** 0.5 for v, vec in lst.items()}
        lo, hi = min(mag.values()), max(mag.values())
        for v, a in mag.items():
            x, y = pr(m.xyz[v - 1])
            t = (a - lo) / (hi - lo) if hi > lo else 0.5
            s.circle(x, y, r, fill=ramp(t))
        s.legend(20, H - 50, 200, 10, '%.3g' % lo, '%.3g' % hi,
                 '|boundary area| (%s)' % label)
    else:
        s.text(20, H - 40, 'no boundary areas in this case', size=11,
               color='#555')
    s.text(20, H - 14, '%d vertices, %d cells' % (len(m.xyz),
                                                  len(m.cells)),
           size=11, color='#555')
    s.save(path)


if __name__ == '__main__':
    main()
