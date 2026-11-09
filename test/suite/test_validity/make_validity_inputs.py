"""make_validity_inputs.py OUTDIR

The small meshes of test_validity, each breaking one rule (or, for the
last few, legal meshes that look unusual). Python 3 standard library.
Tets are given here in LaGriT's vertex order and written in AVS order
(LaGriT 1 2 3 4 = AVS 1 2 4 3), as LaGriT's dump / avs does.
"""

import math
import os
import sys


def write_avs(path, xyz, cells, kinds, clr=None, raw=False):
    """kinds: 'tri', 'tet' or another AVS name per cell. raw: write the
    vertex numbers as given (no LaGriT -> AVS reorder)."""
    clr = clr or [1] * len(cells)
    with open(path, 'w') as f:
        f.write('%d %d 0 0 0\n' % (len(xyz), len(cells)))
        for i, p in enumerate(xyz):
            f.write('%d %.17g %.17g %.17g\n' % (i + 1, p[0], p[1], p[2]))
        for k, (c, kind) in enumerate(zip(cells, kinds)):
            v = list(c)
            if kind == 'tet' and not raw:
                v = [v[0], v[1], v[3], v[2]]
            f.write('%d %d %s %s\n' % (k + 1, clr[k], kind,
                                       ' '.join(str(x + 1) for x in v)))


def det(p, q, r, s):
    a = [q[i] - p[i] for i in range(3)]
    b = [r[i] - p[i] for i in range(3)]
    c = [s[i] - p[i] for i in range(3)]
    return (a[0] * (b[1] * c[2] - b[2] * c[1])
            - a[1] * (b[0] * c[2] - b[2] * c[0])
            + a[2] * (b[0] * c[1] - b[1] * c[0]))


def positive(xyz, t):
    """The tet t, its last two vertices swapped if needed, so that
    det > 0 in LaGriT's order."""
    t = list(t)
    if det(*[xyz[v] for v in t]) < 0:
        t[2], t[3] = t[3], t[2]
    return t


def main():
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)

    def w(name, *a, **k):
        write_avs(os.path.join(out, name + '.inp'), *a, **k)

    # two tets sharing a face: the legal base for several cases
    xyz2 = [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1), (1, 1, 1)]
    t1 = positive(xyz2, [0, 1, 2, 3])
    t2 = positive(xyz2, [1, 2, 3, 4])

    # a unit square of 2 counterclockwise triangles
    sq = [(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)]

    w('mixed', xyz2, [[0, 1, 2], t1], ['tri', 'tet'])
    w('quad', sq, [[0, 1, 2, 3]], ['quad'])
    w('missing_vertex', sq, [[0, 1, 2], [0, 2, 9]], ['tri', 'tri'])
    # second tet inverted (negative volume)
    w('inverted', xyz2, [t1, [t2[0], t2[1], t2[3], t2[2]]],
      ['tet', 'tet'])
    # four coplanar vertices
    flat = [(0, 0, 0), (1, 0, 0), (0, 1, 0), (1, 1, 0), (0, 0, 1)]
    w('flat_tet', flat, [positive(flat, [0, 1, 2, 4]), [0, 1, 3, 2]],
      ['tet', 'tet'])
    w('repeat_vertex', xyz2, [t1, [1, 2, 3, 3]], ['tet', 'tet'])
    w('flat_tri', [(0, 0, 0), (1, 0, 0), (2, 0, 0), (0, 1, 0)],
      [[0, 1, 3], [0, 1, 2]], ['tri', 'tri'])
    # one face (0,1,2) in three tets: apexes 3 (above), 4 (below) and
    # 5 (above again)
    f3 = [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0.2, 0.2, 1), (0.2, 0.2, -1),
          (0.4, 0.3, 0.5)]
    w('face_in_3_tets', f3, [positive(f3, [0, 1, 2, k]) for k in (3, 4, 5)],
      ['tet'] * 3)
    # one edge (0,1) in three triangles of one itetclr (a book)
    bk = [(0, 0, 0), (1, 0, 0), (0.5, 1, 0), (0.5, -1, 0), (0.5, 0, 1)]
    w('edge_in_3_tris', bk, [[0, 1, 2], [1, 0, 3], [0, 1, 4]],
      ['tri'] * 3)
    # a fan of 4 triangles around the center, one turned over
    fan = [(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0), (0.5, 0.5, 0)]
    w('flipped_tri', fan, [[0, 1, 4], [1, 2, 4], [4, 3, 2], [3, 0, 4]],
      ['tri'] * 4)
    # a Moebius strip: 12 segments, half a turn
    n, rad, wid = 12, 1.0, 0.25
    mb = []
    for k in range(n):
        th = 2 * math.pi * k / n
        for s in (wid, -wid):
            r = rad + s * math.cos(th / 2)
            mb.append((r * math.cos(th), r * math.sin(th),
                       s * math.sin(th / 2)))
    cells = []
    for k in range(n):
        tk, bk_ = 2 * k, 2 * k + 1
        if k < n - 1:
            tn, bn = 2 * k + 2, 2 * k + 3
        else:           # the twist: top meets bottom
            tn, bn = 1, 0
        cells += [[tk, bk_, bn], [tk, bn, tn]]
    w('moebius', mb, cells, ['tri'] * len(cells))

    # legal: two fractures crossing along the edge (0,1,0)-(0,0,0)...
    # fracture 1 in the plane z = 0, fracture 2 in the plane x = 0,
    # oriented the other way
    xf = [(0, 0, 0), (0, 1, 0), (-1, 0, 0), (-1, 1, 0), (1, 0, 0),
          (1, 1, 0), (0, 0, -1), (0, 1, -1), (0, 0, 1), (0, 1, 1)]
    cf = [[0, 1, 2], [2, 1, 3], [0, 4, 1], [1, 4, 5],
          [0, 6, 1], [1, 6, 7], [0, 1, 8], [8, 1, 9]]
    w('two_fractures', xf, cf, ['tri'] * 8, clr=[1, 1, 1, 1, 2, 2, 2, 2])
    # legal: all triangles clockwise
    w('clockwise', fan, [[0, 4, 1], [1, 4, 2], [2, 4, 3], [3, 4, 0]],
      ['tri'] * 4)
    # legal, with a warning: a vertex in no cell
    w('unused_vertex', xyz2 + [(5, 5, 5)], [t1, t2], ['tet', 'tet'])
    # legal: two materials sharing a face
    w('two_materials', xyz2, [t1, t2], ['tet', 'tet'], clr=[1, 2])
    print('wrote 15 meshes in', out)


if __name__ == '__main__':
    main()
