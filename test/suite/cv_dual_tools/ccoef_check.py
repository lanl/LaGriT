"""ccoef_check.py: checks of dump / control_volume's ccoef_* vertex
attributes (test_cv_dual_control_volume, case cv09_ccoef). Python 3
standard library.

    from ccoef_check import KW, check_ccoef
    check_ccoef(atts, xyz, cv_stor, matbld_stor, rename) ->
        [(check, detail, ok)]

atts: the vertex attributes of the mesh after the command (name ->
values); xyz: its vertices; cv_stor: the command's own .stor, written
with zero_area keep and compress off; matbld_stor: dump / stor / ...
/ matbld on the same mesh; rename: {keyword: attribute name} where the
deck gave a name.

Each attribute is computed again here, per vertex, over its
connections that are not zero-area (|A| <= 1e-12 d^2, cv_dual's
zero_tol): A(ij) = (A/d) d from a .stor's value and d from the
vertex coordinates (math.dist):
  - from the command's .stor: every A/d equal, A and d within a
    rounding or two (computed again here), every partner vertex the
    same;
  - from matbld's .stor (an independent code): equal within 1e-7 of
    the largest |A| or |A/d|, except at vertices whose extreme is a
    connection matbld leaves out (|A/d| below 1e-8 of the largest:
    LaGriT parking lot 80), counted and named;
  - matbld's ccoef / ij_ccoef (its most negative coefficient and that
    connection's other vertex): equal to ccoef_areabydist_min (as
    |A/d|) and ccoef_ij_areabydist_min wherever matbld found one.
"""
import math

from stor_compare import read_stor

KW = ['ccoef_area_min', 'ccoef_area_max', 'ccoef_ij_area_min',
      'ccoef_ij_area_max', 'ccoef_areabydist_min', 'ccoef_areabydist_max',
      'ccoef_ij_areabydist_min', 'ccoef_ij_areabydist_max',
      'ccoef_distance_min', 'ccoef_distance_max']
VALUE_OF = {2: 0, 3: 1, 6: 4, 7: 5}      # partner row -> its value row


def stats(xyz, c, ztol=1e-12):
    """The ten values per vertex, and the kept connections
    {(i, j): (A, A/d, d)}, from c[(i, j)] = -A/d (a .stor's value;
    i < j, 0-based)."""
    n = len(xyz)
    s = [None] * n
    pairs = {}
    for (i, j), v in sorted(c.items()):
        d = math.dist(xyz[i], xyz[j])
        q = -v
        a = q * d
        if abs(a) <= ztol * d * d:
            continue
        pairs[(i, j)] = (a, q, d)
        for vv, w in ((i, j), (j, i)):
            if s[vv] is None:
                s[vv] = [a, a, w + 1, w + 1, q, q, w + 1, w + 1, d, d]
                continue
            r = s[vv]
            if a < r[0]:
                r[0], r[2] = a, w + 1
            if a > r[1]:
                r[1], r[3] = a, w + 1
            if q < r[4]:
                r[4], r[6] = q, w + 1
            if q > r[5]:
                r[5], r[7] = q, w + 1
            r[8] = min(r[8], d)
            r[9] = max(r[9], d)
    return [r if r is not None else [0.0] * 10 for r in s], pairs


def check_ccoef(atts, xyz, cv_stor, matbld_stor, rename):
    out = []
    names = [rename.get(k, k) for k in KW]
    missing = [nm for nm in names if nm not in atts]
    out.append(('ccoef_made', 'the 10 attributes made' if not missing
                else 'missing: ' + ' '.join(missing), not missing))
    if missing:
        return out
    n = len(xyz)
    sa, pa = stats(xyz, read_stor(cv_stor)['c'])
    bad = []
    for r, k in enumerate(KW):
        got = atts[names[r]]
        if r in VALUE_OF:
            off = [v for v in range(n) if int(got[v]) != int(sa[v][r])]
        else:
            # A and d are computed again here (A = (A/d) d, d by
            # math.dist): within a rounding or two; A/d exactly
            off = [v for v in range(n) if not (got[v] == sa[v][r] or (
                r in (0, 1, 8, 9) and abs(got[v] - sa[v][r]) <= 9e-16 *
                abs(sa[v][r])))]
        if off:
            bad.append('%s at %d vertices (vertex %d)' % (k, len(off),
                                                          off[0] + 1))
    out.append(('ccoef_own', 'each of the 10 equal to the command\'s '
                '.stor at all %d vertices' % n if not bad else
                'differ: ' + '; '.join(bad), not bad))

    sb, pb = stats(xyz, read_stor(matbld_stor)['c'])
    qmax = max(abs(q) for (_, q, _) in pb.values())
    amax = max(abs(a) for (a, _, _) in pb.values())
    dropped = set()
    for (i, j), (a, q, d) in pa.items():
        if (i, j) not in pb and abs(q) <= 1e-8 * qmax:
            dropped |= {i, j}
    bad = []
    for r, k in enumerate(KW):
        if r in VALUE_OF:
            continue
        tol = 1e-7 * (qmax if 4 <= r <= 5 else amax if r <= 1 else 1.0)
        off = [v for v in range(n)
               if abs(sb[v][r] - atts[names[r]][v]) > tol and v not in
               dropped]
        if off:
            bad.append('%s at %d vertices' % (k, len(off)))
    nd = len(dropped)
    out.append(('ccoef_matbld', 'the values equal matbld\'s .stor within '
                '1e-7 of the largest, but at %d vertices whose extreme '
                'is a connection matbld leaves out (|A/d| below 1e-8 of '
                'the largest; parking lot 80)' % nd if not bad else
                'differ: ' + '; '.join(bad), not bad))

    if 'ccoef' in atts and 'ij_ccoef' in atts:
        neg = [v for v in range(n) if atts['ccoef'][v] != 0.0]
        val = [v for v in neg if atts[names[4]][v] < 0 and abs(
            abs(atts['ccoef'][v]) - abs(atts[names[4]][v])) <=
            1e-7 * qmax]
        ij = [v for v in neg if int(atts['ij_ccoef'][v]) ==
              int(atts[names[6]][v])]
        ok = len(val) == len(neg) == len(ij) and len(neg) > 0
        out.append(('ccoef_vs_matbld_ccoef', '%d vertices with a negative '
                    'coefficient (matbld\'s ccoef): ccoef_areabydist_min '
                    'agrees at %d, ccoef_ij_areabydist_min = ij_ccoef at '
                    '%d' % (len(neg), len(val), len(ij)), ok))
    return out
