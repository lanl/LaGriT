"""stor_compare.py A.stor B.stor [options]

Compares two FEHM .stor files of one mesh, pair by pair and vertex by
vertex, and labels every difference with what explains it. Python 3
standard library; run as  python3 -I stor_compare.py ...

Usually A is del_vor's file and B is LaGriT's (dump / stor), but any
two .stor files work (del_vor against del_vor, LaGriT against LaGriT).

Options:
  --mesh MESH.inp    the mesh (AVS UCD). Needed for the zero-area rule
                     in 3D, the coordinate-rounding allowance and the
                     absolute-coordinate check; without it those labels
                     are not used.
  --eps X            LaGriT's compress_eps (default 1e-8, as
                     anothermatbld3d; set it if the run used
                     cmo / setatt / epsilon)
  --zero_tol X       del_vor's zero_tol (default 1e-12)
  --coord_digits N   the significant digits of the mesh file's
                     coordinates, when B was computed from coordinates
                     with more digits than the mesh file holds (LaGriT
                     dumping its own mesh with dump / avs). Default: not
                     used (both files computed from the mesh as given).
  --json FILE        also write the results as JSON (for test suites)
  --worst N          list the N worst pairs and vertices (default 5)

Exit status: 0 if every difference is explained, 1 if any is
UNEXPLAINED, 2 for a usage or file error.

Labels, tried in this order (the first that fits is used):

  digits       |a - b| <= 10^(1-D) max(|a|, |b|): both are the same
               number printed with D significant digits (D read from
               the files; 13 for LaGriT and del_vor)
  round_off    |a - b| <= 64 u max, u = 2.2e-16: small values that come
               from cancellation (pieces of opposite sign) carry the
               round-off of the large pieces, about u times the largest
               value; the two codes add the pieces in different orders
  coord_digits |a - b| <= 10^(1-C) (X / h) max: the coordinates were
               rounded to C digits in the mesh file, which moves each
               vertex by about 10^(1-C) X (X the largest |coordinate|)
               and each value by that over h (the shortest edge)
  merge        |a - b| <= eps max: LaGriT's compression stored one
               value for all values within eps of each other (only
               coefficients, only when a file is compressed by a
               LaGriT writer that merges: matbld3d_astor)
  abs_coords   3D LaGriT writers compute tet volumes from absolute
               coordinates (anothermatbld3d's TranslateTetToZero does
               not translate): errors up to about 10 u (X / h)^3 of
               the largest volume, u = 2.2e-16. Used for volumes and
               coefficients when that allowance exceeds the others.
  zero_area    a pair in one file only whose Voronoi face has zero area
               by del_vor's rule (|A| <= zero_tol d^2 for tets,
               zero_tol d for triangles; A = |value| d): left out by
               del_vor's zero_area remove or LaGriT's zero rule (needs
               --mesh in 3D)
  nstor_magnitude
               a = -b, |a| = |b| to the printed digits, when one file is
               LaGriT's matbld3d_nstor (dump / stor ... / none in 3D):
               that writer stores |A|/d, the magnitude of the area
               vector (matbld3d_stor.f, amat = sqrt(xamat**2 + ...)), so
               a negative coefficient comes out with the wrong sign. A
               LaGriT defect: these are real negative coefficients.
  dropped      a pair in A only, not zero-area, |a| <= eps max: LaGriT's
               writer counted it as zero and left it out; the volumes
               of its two vertices lose its share (label dropped_share)
  abs_coords is also used for a pair in A only when its value is
  within the abs_coords allowance (LaGriT's value was garbage there and
  fell below its zero rule). When that allowance exceeds 1, LaGriT's 3D
  values mean nothing for this mesh; the report says so.
  UNEXPLAINED  none of these
"""

import json
import math
import sys

U = 2.220446049250313e-16
MERGING = {'matbld3d_astor'}          # LaGriT writers that merge values
MAGNITUDE = {'matbld3d_nstor'}        # LaGriT writers that store |A|/d
THREE_D = {'matbld3d_astor', 'matbld3d_nstor', 'matbld3d_gstor',
           'Matbld3d_stor'}


def die(msg):
    print('stor_compare: ' + msg, file=sys.stderr)
    sys.exit(2)


def read_stor(path):
    """The .stor blocks, the writer's name and the printed digits."""
    try:
        with open(path) as f:
            text = f.read()
    except OSError as e:
        die('cannot read %s: %s' % (path, e))
    lines = text.split('\n')
    if len(lines) < 4 or not lines[0].startswith('fehmstor'):
        die('%s: not an ASCII .stor (first line %r)' % (path, lines[0][:40]))
    writer = 'unknown'
    if '(' in lines[1] and ')' in lines[1]:
        writer = lines[1][lines[1].rindex('(') + 1:lines[1].rindex(')')]
    h = [int(v) for v in lines[2].split()]
    nw, neq, n3, nac = h[:4]
    ncoef = n3 - neq - 1
    t = ' '.join(lines[3:]).split()
    p = 0
    vol = [float(v) for v in t[p:p + neq]]
    digits = sig_digits(t[0])
    p += neq
    rp = [int(v) for v in t[p:p + neq + 1]]
    p += neq + 1
    col = [int(v) for v in t[p:p + ncoef]]
    p += ncoef
    ptr = [int(v) for v in t[p:p + ncoef]]
    p += ncoef
    p += neq + 1                                   # padding
    dg = [int(v) for v in t[p:p + neq]]
    p += neq
    val = [float(v) for v in t[p:p + nw * nac]]
    if len(val) < nw:
        die('%s: the file ends inside the value block' % path)
    c = {}
    for i in range(neq):
        for q in range(rp[i] - neq - 1, rp[i + 1] - neq - 1):
            j = col[q] - 1
            if j > i:
                k = ptr[q]
                c[(i, j)] = val[k - 1] if k > 0 else 0.0
    return dict(path=path, writer=writer, nw=nw, neq=neq, ncoef=ncoef,
                vol=vol, c=c, digits=digits, rp=rp, col=col, ptr=ptr,
                dg=dg, compressed=nw < ncoef)


def sig_digits(tok):
    m = tok.lower().split('e')[0].lstrip('+-')
    return max(1, len(m.replace('.', '').lstrip('0')) or len(m) - 1)


def layout_problems(s):
    bad = []
    neq, rp = s['neq'], s['rp']
    if rp[0] != neq + 1 or rp[-1] != s['ncoef'] + neq + 1:
        bad.append('row pointers do not run from NEQ+1 to NCOEF+NEQ+1')
    for i in range(neq):
        cols = s['col'][rp[i] - neq - 1:rp[i + 1] - neq - 1]
        if cols != sorted(cols) or len(set(cols)) != len(cols):
            bad.append('row %d: columns not sorted' % (i + 1))
            break
        if (i + 1) not in cols:
            bad.append('row %d: no diagonal' % (i + 1))
            break
        q = rp[i] - neq - 1 + cols.index(i + 1)
        if s['dg'][i] != neq + 1 + q + 1:
            bad.append('row %d: diagonal pointer wrong' % (i + 1))
            break
    if any(k < 0 or k > s['nw'] for k in s['ptr']):
        bad.append('a pointer outside the value block')
    # symmetry: every upper pair must appear in the lower triangle too
    low = set()
    for i in range(neq):
        for q in range(rp[i] - neq - 1, rp[i + 1] - neq - 1):
            j = s['col'][q] - 1
            if j < i:
                low.add((j, i))
    if low != set(s['c']):
        bad.append('the matrix is not symmetric in its pattern')
    return bad


def read_mesh(path):
    try:
        with open(path) as f:
            head = f.readline().split()
            nn, nc = int(head[0]), int(head[1])
            xyz = [tuple(float(v) for v in f.readline().split()[1:4])
                   for _ in range(nn)]
            cells = []
            for _ in range(nc):
                t = f.readline().split()
                cells.append([int(v) - 1 for v in t[3:]])
    except (OSError, ValueError, IndexError) as e:
        die('cannot read the mesh %s: %s' % (path, e))
    return xyz, cells


def main(argv):
    opts = {'--mesh': None, '--eps': 1e-8, '--zero_tol': 1e-12,
            '--coord_digits': None, '--json': None, '--worst': 5}
    files = []
    k = 0
    while k < len(argv):
        a = argv[k]
        if a in opts:
            if k + 1 >= len(argv):
                die('%s needs a value' % a)
            v = argv[k + 1]
            try:
                opts[a] = (float(v) if a in ('--eps', '--zero_tol') else
                           int(v) if a in ('--coord_digits', '--worst')
                           else v)
            except ValueError:
                die('%s %s: not a number' % (a, v))
            k += 2
        elif a.startswith('-'):
            die('unknown option %s (see the top of this file)' % a)
        else:
            files.append(a)
            k += 1
    if len(files) != 2:
        die('usage: stor_compare.py A.stor B.stor [--mesh MESH.inp] ...')
    res, A, B = compare(files[0], files[1], opts['--mesh'], opts['--eps'],
                        opts['--zero_tol'], opts['--coord_digits'],
                        opts['--worst'])
    report(res, A, B)
    if opts['--json']:
        out = {k: v for k, v in res.items()
               if k not in ('pair_where', 'vertex_where')}
        with open(opts['--json'], 'w') as f:
            json.dump(out, f, indent=1, default=str)
    return 1 if res['bad'] else 0


def compare(fa, fb, mesh=None, eps=1e-8, ztol=1e-12, coord_digits=None,
            nworst=5):
    """Compare two .stor files; returns (results, A, B). results['bad']
    is true when a difference is UNEXPLAINED or a layout is wrong."""
    A, B = read_stor(fa), read_stor(fb)
    if A['neq'] != B['neq']:
        die('different vertex counts: %d and %d' % (A['neq'], B['neq']))
    xyz = cells = None
    if mesh:
        xyz, cells = read_mesh(mesh)
        if len(xyz) != A['neq']:
            die('the mesh has %d vertices, the .stor files %d'
                % (len(xyz), A['neq']))
    dim = 3 if (cells and len(cells[0]) == 4) or \
        B['writer'] in THREE_D or A['writer'] in THREE_D else 2
    if cells:
        dim = 3 if len(cells[0]) == 4 else 2

    ca, cb = A['c'], B['c']
    cmax = max([abs(v) for v in ca.values()] +
               [abs(v) for v in cb.values()] + [0.0]) or 1.0
    vmax = max([abs(v) for v in A['vol']] + [abs(v) for v in B['vol']]
               + [0.0]) or 1.0
    dig = min(A['digits'], B['digits'])
    t_dig = 10.0 ** (1 - dig)

    X = hmin = None
    t_coord = t_abs = 0.0
    if xyz:
        X = max(max(abs(c) for c in p) for p in xyz)
        hmin = min(math.dist(xyz[i], xyz[j]) for i, j in
                   (set(ca) | set(cb)))
        if coord_digits:
            t_coord = 10.0 ** (1 - coord_digits) * X / hmin
        if B['writer'] in THREE_D or A['writer'] in THREE_D:
            t_abs = 10 * U * (X / hmin) ** 3
    merges = (B['writer'] in MERGING and B['compressed']) or \
        (A['writer'] in MERGING and A['compressed'])
    lagrit_drops = B['writer'] in MERGING or A['writer'] in MERGING
    magnitude = B['writer'] in MAGNITUDE or A['writer'] in MAGNITUDE

    def label_value(a, b, scale, volume=False):
        d = abs(a - b)
        if d <= t_dig * max(abs(a), abs(b)):
            return 'digits'
        if d <= 64 * U * scale:
            return 'round_off'
        if t_coord and d <= t_coord * scale:
            return 'coord_digits'
        if merges and not volume and d <= eps * scale * 1.0000001:
            return 'merge'
        if t_abs and d <= t_abs * scale:
            return 'abs_coords'
        if magnitude and not volume and a * b < 0 and \
                abs(abs(a) - abs(b)) <= max(t_dig * abs(a), 64 * U * scale):
            return 'nstor_magnitude'
        return 'UNEXPLAINED'

    def zero_area(k, v):
        if xyz is None:
            return dim == 2 and abs(v) <= ztol
        d = math.dist(xyz[k[0]], xyz[k[1]])
        return abs(v) * d <= ztol * d ** (dim - 1)

    res = {'A': summary(A), 'B': summary(B), 'pairs': {}, 'coef': {},
           'vol': {}, 'worst_coef': [], 'worst_vol': []}
    only_a = sorted(set(ca) - set(cb))
    only_b = sorted(set(cb) - set(ca))
    dropped_real = []
    lab_a, lab_b = {}, {}
    where = {}          # pair -> label, for every pair not 'digits'
    for k in only_a:
        v = ca[k]
        if zero_area(k, v):
            lb = 'zero_area'
        elif lagrit_drops and abs(v) <= eps * cmax:
            lb = 'dropped'
            dropped_real.append(k)
        elif t_abs and abs(v) <= t_abs * cmax:
            lb = 'abs_coords'
            dropped_real.append(k)
        else:
            lb = 'UNEXPLAINED'
        lab_a[lb] = lab_a.get(lb, 0) + 1
        where[k] = lb
    for k in only_b:
        lb = 'zero_area' if zero_area(k, cb[k]) else 'UNEXPLAINED'
        lab_b[lb] = lab_b.get(lb, 0) + 1
        where[k] = lb
    res['pairs'] = {'common': len(set(ca) & set(cb)),
                    'only_A': len(only_a), 'only_B': len(only_b),
                    'only_A_labels': lab_a, 'only_B_labels': lab_b}

    cl = {}
    worst = []
    for k in set(ca) & set(cb):
        lb = label_value(ca[k], cb[k], cmax)
        cl[lb] = cl.get(lb, 0) + 1
        worst.append((abs(ca[k] - cb[k]), k, lb))
        if lb != 'digits':
            where[k] = lb
    worst.sort(reverse=True)
    res['coef'] = {'max_rel': (worst[0][0] / cmax) if worst else 0.0,
                   'labels': cl}
    res['worst_coef'] = [(k[0] + 1, k[1] + 1, ca[k], cb[k], d / cmax, lb)
                         for d, k, lb in worst[:nworst]]

    touched = {i for k in dropped_real for i in k}
    vl = {}
    wv = []
    vwhere = {}         # vertex -> label, for every vertex not 'digits'
    for i, (a, b) in enumerate(zip(A['vol'], B['vol'])):
        lb = label_value(a, b, vmax, volume=True)
        if lb in ('UNEXPLAINED', 'abs_coords') and i in touched:
            lb = 'dropped_share'
        vl[lb] = vl.get(lb, 0) + 1
        wv.append((abs(a - b), i, lb))
        if lb != 'digits':
            vwhere[i] = lb
    wv.sort(reverse=True)
    res['vol'] = {'max_rel': wv[0][0] / vmax if wv else 0.0, 'labels': vl,
                  'sum_A': sum(A['vol']), 'sum_B': sum(B['vol'])}
    res['worst_vol'] = [(i + 1, A['vol'][i], B['vol'][i], d / vmax, lb)
                        for d, i, lb in wv[:nworst]]
    res['allowances'] = {'digits': t_dig, 'coord_digits': t_coord,
                         'merge': eps if merges else 0.0,
                         'abs_coords': t_abs, 'X': X, 'hmin': hmin}
    res['unexplained'] = (lab_a.get('UNEXPLAINED', 0) +
                          lab_b.get('UNEXPLAINED', 0) +
                          cl.get('UNEXPLAINED', 0) +
                          vl.get('UNEXPLAINED', 0))
    res['layout_A'] = layout_problems(A)
    res['layout_B'] = layout_problems(B)
    res['bad'] = bool(res['unexplained'] or res['layout_A'] or
                      res['layout_B'])
    res['dim'] = dim
    res['pair_where'] = where
    res['vertex_where'] = vwhere
    res['dropped_pairs'] = [(i + 1, j + 1) for i, j in dropped_real]
    return res, A, B


def summary(s):
    return {'path': s['path'], 'writer': s['writer'], 'vertices': s['neq'],
            'pairs': len(s['c']), 'values': s['nw'],
            'compressed': s['compressed'], 'digits': s['digits']}


def labels(d):
    return ', '.join('%s %d' % (k, v) for k, v in sorted(d.items())) or '-'


def report(res, A, B):
    p = print
    for tag, s in (('A', A), ('B', B)):
        p('stor_compare: %s %s' % (tag, s['path']))
        p('    writer %s, %d vertices, %d pairs, %d values (%s), '
          '%d digits' % (s['writer'], s['neq'], len(s['c']), s['nw'],
                         'compressed' if s['compressed'] else
                         'uncompressed', s['digits']))
    al = res['allowances']
    p('allowances: digits %.1e of the value; of the largest: round_off '
      '%.1e,' % (al['digits'], 64 * U))
    p('    coord_digits %.1e, merge %.1e, abs_coords %.1e'
      % (al['coord_digits'], al['merge'], al['abs_coords']))
    if al['abs_coords'] > 1:
        p('WARNING: the abs_coords allowance exceeds 1: LaGriT\'s 3D '
          'values mean nothing')
        p('    for this mesh (coordinates %.3g, shortest edge %.3g)'
          % (al['X'], al['hmin']))
    pr = res['pairs']
    p('pairs       common %d; only in A %d (%s); only in B %d (%s)'
      % (pr['common'], pr['only_A'], labels(pr['only_A_labels']),
         pr['only_B'], labels(pr['only_B_labels'])))
    p('values      max |a - b| / max %.2e; %s'
      % (res['coef']['max_rel'], labels(res['coef']['labels'])))
    for i, j, a, b, d, lb in res['worst_coef']:
        p('    pair %d %d  %.13e  %.13e  %.1e  %s' % (i, j, a, b, d, lb))
    v = res['vol']
    p('volumes     max |a - b| / max %.2e; %s'
      % (v['max_rel'], labels(v['labels'])))
    p('    sum A %.15g  sum B %.15g' % (v['sum_A'], v['sum_B']))
    for i, a, b, d, lb in res['worst_vol']:
        p('    vertex %d  %.13e  %.13e  %.1e  %s' % (i, a, b, d, lb))
    for tag in ('A', 'B'):
        lp = res['layout_' + tag]
        p('layout %s    %s' % (tag, '; '.join(lp) if lp else 'ok'))
    if res['unexplained'] or res['layout_A'] or res['layout_B']:
        p('stor_compare: UNEXPLAINED differences: %d' % res['unexplained'])
    else:
        p('stor_compare: every difference explained')


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
