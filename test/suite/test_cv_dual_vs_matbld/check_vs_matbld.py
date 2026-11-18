"""check_vs_matbld.py WORK LOGS

test_cv_dual_vs_matbld: compares, case by case, the files of the two
runs of each deck (WORK/NAME/matbld as written, WORK/NAME/cv_dual with
/ cv_dual on its dump commands; run_vs_matbld_tests.sh) and explains
every difference. Writes summary.html in this suite's folder and
LOGS/results.json (every comparison's numbers, for the report). Python
3 standard library. Exit 1 on any FAIL.

Each file is judged by its kind:
  _mesh_ROOT.inp  the mesh just before a dump command (the suite's own
            dump / avs): the same vertices and cells in both runs.
            matbld adds the attributes ccoef and ij_ccoef to the mesh
            object when it finds negative coefficients; cv_dual
            changes nothing.
  .stor     ASCII: stor_compare (cv_dual_tools) with the mesh, every
            difference labeled (digits, round_off, zero_area, merge,
            nstor_magnitude, ...). A .stor matbld wrote in both runs
            (an option cv_dual does not have) must be the same file,
            line 2 aside. Hybrid: matbld's and cv_dual's hybrids are
            different methods; their negative coefficients are
            counted, the values not judged. Binary: the same file but
            the date.
  .uge      the cells' volumes and the connections' areas, labeled as
            stor_compare does (digits, round_off, zero_area).
  .area     the boundary areas: the same vertices, the numbers equal
            to their 13 digits.
  others    byte for byte.
A file one run wrote and the other did not: cv_dual's violations.vtu
(its Delaunay check flagged facets); _outside_hyb.area against
matbld's _outside_vor.area (hybrid); or every file of a dump cv_dual
refused, with its ERROR in the run's output (a hex mesh, a mesh that
is not Delaunay).
"""

import json
import math
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(SUITE, 'cv_dual_tools'))
from pagekit import read_cases, write_case_page  # noqa: E402
import stor_compare  # noqa: E402

U = 2.220446049250313e-16
SKIP = {'.inputs', 'run.out', 'run.rc', 'input.lgi', 'outx3dgen',
        'logx3dgen', 'lagrit.out', 'lagrit.log'}
DATE = re.compile(rb'[A-Z][a-z]{2} [A-Z][a-z]{2} [ \d]\d '
                  rb'\d\d:\d\d:\d\d \d{4}')


def read_avs(path):
    """(coordinates, cells, attribute names) of an AVS UCD file."""
    with open(path) as f:
        lines = [ln for ln in f if ln.strip()]
    h = lines[0].split()
    nn, nc, nnd = int(h[0]), int(h[1]), int(h[2])
    xyz = [tuple(float(v) for v in lines[1 + i].split()[1:4])
           for i in range(nn)]
    cells = [tuple(lines[1 + nn + i].split()[2:]) for i in range(nc)]
    names = []
    if nnd:
        k = 1 + nn + nc
        na = len(lines[k].split()) - 1
        names = [lines[k + 1 + a].split(',')[0].strip() for a in range(na)]
    return xyz, cells, names


def read_uge(path):
    """{id: (x, y, z, vol)}, {(i, j): (x, y, z, area)}, digits."""
    tok = open(path).read().split()
    n = int(tok[1])
    cells, p = {}, 2
    for _ in range(n):
        cells[int(tok[p])] = tuple(float(v) for v in tok[p + 1:p + 5])
        p += 5
    m = int(tok[p + 1])
    p += 2
    con = {}
    for _ in range(m):
        i, j = int(tok[p]), int(tok[p + 1])
        con[(min(i, j), max(i, j))] = tuple(float(v)
                                            for v in tok[p + 2:p + 6])
        p += 6
    dig = stor_compare.sig_digits(tok[5]) if n else 17
    return cells, con, dig


def label(a, b, scale, t_dig):
    d = abs(a - b)
    if d <= t_dig * max(abs(a), abs(b)):
        return 'digits'
    if d <= 64 * U * scale:
        return 'round_off'
    return 'UNEXPLAINED'


def line2(path):
    with open(path, 'rb') as f:
        f.readline()
        return f.readline().decode('latin-1').rstrip()


def writer(path):
    m = re.search(r'\(([^\s()]+)', line2(path))
    return m.group(1) if m else 'unknown'


def negatives(st):
    """Negative coefficients A(ij) < 0: stored values -A/d > 0, beyond
    round-off (cv_dual keeps zero-area connections: values about 0 of
    either sign)."""
    cmax = max([abs(v) for v in st['c'].values()] + [0.0])
    return sum(1 for v in st['c'].values() if v > 64 * U * cmax)


def refusals(out):
    """{root: the cv_dual line before the ERROR} of the run's output."""
    res, last_dump, last_msg = {}, None, ''
    for ln in out.split('\n'):
        s = ln.strip()
        m = re.match(r'dump/(\w+)/([^/]+)/', s)
        if m:
            last_dump = m.group(2)
        if s.startswith('cv_dual:') or s.startswith('Delaunay check'):
            last_msg = s
        if s.startswith('ERROR: dump/') and last_dump:
            res[last_dump] = last_msg
    return res


def stem(f):
    for ext in ('.stor', '.uge', '.fehmn'):
        if f.endswith(ext):
            return f[:-len(ext)]
    m = re.match(r'(.+?)_(outside|material|interface|multi_mat|parent)'
                 r'(_\w+)?\.(zone|area)$', f)
    return m.group(1) if m else None


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows, results = [], {}
    for name, deck, about in read_cases(os.path.join(HERE, 'cases.txt'),
                                        3):
        d = os.path.join(work, name)
        dm, dc = os.path.join(d, 'matbld'), os.path.join(d, 'cv_dual')
        checks, res = [], {'files': {}}
        results[name] = res

        def put(c, v, ok):
            checks.append((c, v, 'PASS' if ok else 'FAIL'))
            print('%s_%s %s %s' % (name, c, v, 'PASS' if ok else 'FAIL'))

        def info(c, v):
            checks.append((c, v, 'MEASURED'))
            print('%s_%s %s MEASURED' % (name, c, v))

        try:
            dumps = open(os.path.join(d, 'dumps.txt')).read().split('\n')
            inputs = set(open(os.path.join(dm, '.inputs')).read().split())
            out_c = open(os.path.join(dc, 'run.out'),
                         errors='replace').read()
            rc = [int(open(os.path.join(x, 'run.rc')).read())
                  for x in (dm, dc)]
        except (OSError, ValueError) as e:
            put('ran', 'did not run: %s' % e, False)
            rows.append((name, about, deck, checks, None))
            continue
        cmd = '\n'.join('dump / %s / %s / %s ... [/ cv_dual]' %
                        tuple(ln.split()) for ln in dumps if ln.strip())
        put('ran', 'both runs exit %d %d' % tuple(rc), rc == [0, 0])
        refused = refusals(out_c)
        res['refused'] = refused
        fm = set(os.listdir(dm)) - SKIP - inputs
        fc = set(os.listdir(dc)) - SKIP - inputs
        hyb_roots = {f[:-len('_outside_hyb.area')] for f in fc
                     if f.endswith('_outside_hyb.area')}

        for f in sorted(fm | fc):
            pm, pc = os.path.join(dm, f), os.path.join(dc, f)
            try:
                if f not in fc:
                    r = stem(f)
                    if r in refused:
                        put('refused', '%s: not written with cv_dual, '
                            'which refused the mesh (%s)' % (
                                f, refused[r]), True)
                        res['files'][f] = 'refused: ' + refused[r]
                    elif f.endswith('_outside_vor.area') and \
                            f[:-len('_outside_vor.area')] in hyb_roots:
                        info('hybrid', '%s (matbld) beside %s '
                             '(cv_dual hybrid)' % (f, f.replace(
                                 '_vor.', '_hyb.')))
                    else:
                        put('files', '%s written by matbld only' % f,
                            False)
                    continue
                if f not in fm:
                    if f == 'violations.vtu':
                        m = re.findall(r'violations +(\d+)\s+beyond '
                                       r'delaunay_fatal +(\d+)', out_c)
                        bo = re.findall(r'boundary circumcenter outside '
                                        r'+(\d+)', out_c)
                        info('violations', 'violations.vtu: cv_dual\'s '
                             'Delaunay check flagged facets in %d run(s): '
                             'violations %s, beyond delaunay_fatal %s, '
                             'boundary circumcenters outside the mesh %s'
                             % (len(m), max([int(v[0]) for v in m] or [0]),
                                max([int(v[1]) for v in m] or [0]),
                                max([int(v) for v in bo] or [0])))
                        res['violations'] = {'runs': m, 'outside': bo}
                    elif f.endswith('_outside_hyb.area'):
                        pass    # reported with matbld's _vor
                    else:
                        put('files', '%s written by cv_dual only' % f,
                            False)
                    continue
                compare_file(f, pm, pc, dm, put, info, res)
            except (OSError, ValueError, IndexError, KeyError,
                    ZeroDivisionError, SystemExit) as e:
                put('compare', '%s: %s' % (f, e), False)
        rows.append((name, about, cmd, checks, None))

    with open(os.path.join(logs, 'results.json'), 'w') as f:
        json.dump(results, f, indent=1, default=str)
    what = ('runs LaGriT\'s own decks that write .stor, .uge and FEHM '
            'files twice: as written (matbld, LaGriT\'s sparse matrix '
            'code) and with <code>/ cv_dual</code> on every '
            '<code>dump / stor</code>, <code>fehm</code>, '
            '<code>pflotran</code> and <code>zone_outside</code>, and '
            'compares the two runs\' files. Every difference must be '
            'explained: printed digits, round-off, zero-area '
            'connections, compression, matbld writers\' known defects '
            '(stor_compare\'s labels), a mesh cv_dual refuses (hexes, '
            'not Delaunay), or matbld\'s and cv_dual\'s different '
            'hybrids (counted, not judged). Each check line says which.')
    nfail = write_case_page(HERE, logs, 'test_cv_dual_vs_matbld', what,
                            rows)
    sys.exit(1 if nfail else 0)


def compare_file(f, pm, pc, dm, put, info, res):
    if f.startswith('_mesh_') and f.endswith('.inp'):
        xm, cm, am = read_avs(pm)
        xc, cc, ac = read_avs(pc)
        same = xm == xc and cm == cc
        extra = sorted(set(am) - set(ac))
        ok = same and set(extra) <= {'ccoef', 'ij_ccoef'} and \
            not set(ac) - set(am)
        put('mesh', '%s: the same %d vertices and %d cells%s' % (
            f, len(xm), len(cm), '; matbld added %s (negative '
            'coefficients found)' % ' '.join(extra) if extra else '')
            if ok else '%s: meshes differ' % f, ok)
        return
    bm, bc = open(pm, 'rb').read(), open(pc, 'rb').read()
    if f.endswith('.stor') and bm[:40].split() and \
            bm[:40].split()[0].isdigit():
        # matbld's graph compression (matbld3d_gstor) writes no title
        # lines: a copy with them, for stor_compare
        pm2 = pc + '.matbld_titled'
        with open(pm2, 'wb') as g:
            g.write(b'fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi '
                    b'Coefficients\n (matbld3d_gstor: no title lines in '
                    b'the file)\n' + bm)
        pm, bm = pm2, open(pm2, 'rb').read()
    if f.endswith('.stor') and not bm.startswith(b'fehmstor asci'):
        ok = DATE.sub(b'', bm) == DATE.sub(b'', bc)
        put('stor', '%s (binary): matbld wrote it in both runs, the '
            'same file but the date' % f if ok else
            '%s (binary) differs' % f, ok)
        return
    if f.endswith('.stor'):
        wc = writer(pc)
        if wc != 'cv_dual':
            lm, lc = bm.split(b'\n'), bc.split(b'\n')
            ok = lm[:1] + lm[2:] == lc[:1] + lc[2:]
            put('stor', '%s: matbld wrote it in both runs (%s), the same '
                'file' % (f, wc) if ok else '%s: differs (matbld in '
                'both runs)' % f, ok)
            return
        mesh = os.path.join(dm, '_mesh_%s.inp' % f[:-5])
        hyb = b'Hybrid' in bc.split(b'\n')[0]
        r, A, B = stor_compare.compare(pc, pm, mesh if os.path.exists(mesh)
                                       else None)
        na, nb = negatives(A), negatives(B)
        rec = {'writer_matbld': B['writer'], 'vertices': A['neq'],
               'pairs_cv_dual': len(A['c']), 'pairs_matbld': len(B['c']),
               'only_cv_dual': r['pairs']['only_A_labels'],
               'only_matbld': r['pairs']['only_B_labels'],
               'coef': r['coef'],
               'vol_max_rel': r['vol']['max_rel'],
               'vol_labels': r['vol']['labels'],
               'neg_cv_dual': na, 'neg_matbld': nb, 'hybrid': hyb,
               'unexplained': r['unexplained']}
        res['files'][f] = rec
        txt = ('%s (%s): %d vertices, pairs %d / %d; values max diff '
               '%.1e (%s); volumes %.1e (%s); negative coefficients '
               'cv_dual %d, matbld %d' % (
                   f, B['writer'], A['neq'], len(A['c']), len(B['c']),
                   r['coef']['max_rel'],
                   stor_compare.labels(r['coef']['labels']),
                   r['vol']['max_rel'],
                   stor_compare.labels(r['vol']['labels']), na, nb))
        if r['pairs']['only_A'] or r['pairs']['only_B']:
            txt += '; only cv_dual %s, only matbld %s' % (
                stor_compare.labels(r['pairs']['only_A_labels']),
                stor_compare.labels(r['pairs']['only_B_labels']))
        if hyb:
            info('hybrid', txt + ' (different hybrids: not judged)')
        else:
            put('stor', txt, not r['bad'])
        return
    if f.endswith('.uge'):
        cm, km, gm = read_uge(pm)
        cc, kc, gc = read_uge(pc)
        t_dig = 10.0 ** (1 - min(gm, gc))
        vmax = max(abs(v[3]) for v in list(cm.values()) +
                   list(cc.values())) or 1.0
        amax = max([abs(v[3]) for v in list(km.values()) +
                    list(kc.values())] + [0.0]) or 1.0
        lv, la = {}, {}
        for i in cm:
            for q in range(3):
                lb = label(cm[i][q], cc[i][q], 1.0, t_dig)
                if lb != 'digits':
                    lv['coord ' + lb] = lv.get('coord ' + lb, 0) + 1
            lb = label(cc[i][3], cm[i][3], vmax, t_dig)
            lv[lb] = lv.get(lb, 0) + 1
        mesh = os.path.join(dm, '_mesh_%s.inp' % f[:-4])
        xyz = read_avs(mesh)[0] if os.path.exists(mesh) else None

        def dist(k):
            return math.dist(xyz[k[0] - 1], xyz[k[1] - 1])
        # matbld compresses the coefficients A/d of a .uge (values
        # within eps = 1e-8 of the largest merged) and writes A = d A/d
        cmax = max([abs(v[3]) / dist(k) for k, v in km.items()] + [0.0]) \
            if xyz else 0.0
        for k in set(km) & set(kc):
            lb = label(kc[k][3], km[k][3], amax, t_dig)
            if lb == 'UNEXPLAINED' and xyz and abs(kc[k][3] - km[k][3]) \
                    <= 1e-8 * cmax * dist(k) * 1.0000001:
                lb = 'merge'
            la[lb] = la.get(lb, 0) + 1
        dim = 2 if xyz and len({p[2] for p in xyz}) == 1 else 3
        oz = {}
        for tag, only, src in (('cv_dual', set(kc) - set(km), kc),
                               ('matbld', set(km) - set(kc), km)):
            for k in only:
                a = abs(src[k][3])
                dd = math.dist(xyz[k[0] - 1], xyz[k[1] - 1]) if xyz else 1
                lb = 'zero_area' if a <= 1e-12 * dd ** (dim - 1) \
                    else 'UNEXPLAINED'
                oz[tag + ' ' + lb] = oz.get(tag + ' ' + lb, 0) + 1
        bad = any('UNEXPLAINED' in k for k in list(lv) + list(la) +
                  list(oz))
        res['files'][f] = {'cells': len(cm), 'connections_matbld': len(km),
                           'connections_cv_dual': len(kc), 'vol': lv,
                           'area': la, 'only': oz}
        put('uge', '%s: %d cells, connections %d / %d; volumes %s; areas '
            '%s%s' % (f, len(cm), len(kc), len(km),
                      stor_compare.labels(lv), stor_compare.labels(la),
                      '; only ' + stor_compare.labels(oz) if oz else ''),
            not bad)
        return
    if f.endswith('.area'):
        nm = [float(v) for v in re.findall(r'-?\d\.\d+E[+-]\d+',
                                           open(pm).read())]
        nc = [float(v) for v in re.findall(r'-?\d\.\d+E[+-]\d+',
                                           open(pc).read())]
        big = max([abs(v) for v in nm] + [0.0]) or 1.0
        worst = max([abs(a - b) for a, b in zip(nm, nc)] + [0.0])
        ok = len(nm) == len(nc) and worst <= 1e-12 * big
        put('area', '%s: %d numbers, largest difference %.1e of %.1e'
            % (f, len(nm), worst, big), ok)
        res['files'][f] = {'numbers': len(nm), 'max_diff': worst}
        return
    put('bytes', '%s: %s' % (f, 'the same' if bm == bc else 'DIFFERENT'),
        bm == bc)


if __name__ == '__main__':
    main()
