"""make_delaunay_inputs.py OUTDIR

The generated meshes of test_delaunay (the same on every machine; no
random numbers). Python 3 standard library.

  flip2d.inp   output01_square_uniform (LaGriT test_poisson_2d) with 5
               interior edges flipped (2-2 flips), chosen far apart,
               each giving a facet that is not Delaunay
  flip3d.inp   test_3d_poi_01 (LaGriT test_poisson_3d case 1) with 3
               2-3 flips: tets (a,b,c,d) and (a,b,c,e) replaced by
               (a,b,d,e), (b,c,d,e), (c,a,d,e)
  kuhn.inp     a 4 x 3 x 3 grid of unit cubes, 6 tets per cube along
               the main diagonal: every vertex set co-spherical, many
               ties, no violation
  near_tie.inp 4 vertices, 2 triangles: (0,0) (1,0) (1,1) (0,1+1e-6)
               cut along the wrong diagonal: delta/L = -5e-7
  utm.inp      test_3d_poi_01 moved by (7e5, 3.2e6, 1.5e3), written
               with 17 significant digits

Which facets break Delaunay is NOT recorded here: the checker works it
out exactly (exact_check.py) from the files themselves.
"""

import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
from exact_check import read_avs, analyze  # noqa: E402

LAGRIT = os.path.join(SUITE, 'test_poisson_lagrit', 'inputs')


def write_avs(path, xyz, cells, clr, kind, digits=13):
    """AVS UCD with LaGriT's layout; tets given in LaGriT order are
    written back in AVS order (LaGriT 1 2 3 4 -> AVS 1 2 4 3)."""
    with open(path, 'w') as f:
        f.write('%10d %10d %10d %10d %10d\n' % (len(xyz), len(cells),
                                                 0, 0, 0))
        fmt = '%%06d  %%.%de  %%.%de  %%.%de\n' % (digits - 1, digits - 1,
                                                   digits - 1)
        for i, p in enumerate(xyz):
            f.write(fmt % (i + 1, p[0], p[1], p[2]))
        for k, c in enumerate(cells):
            v = c if kind == 'tri' else [c[0], c[1], c[3], c[2]]
            f.write('%06d %6d   %s  %s\n' % (k + 1, clr[k], kind,
                                             ' '.join('%d' % (x + 1)
                                                      for x in v)))


def sub(a, b):
    return [a[i] - b[i] for i in range(3)]


def cross(a, b):
    return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0]]


def dot(a, b):
    return sum(a[i] * b[i] for i in range(3))


def orient3(p, q, r, s):
    return dot(sub(q, p), cross(sub(r, p), sub(s, p)))


def flip2d(m, nflip=5):
    """2-2 flips of interior edges with a convex quadrilateral; each
    chosen edge must make the new edge not Delaunay, and the flips are
    at least 0.25 apart (the mesh is the unit square)."""
    edge_cells = {}
    for it, c in enumerate(m.cells):
        for a, b in ((0, 1), (1, 2), (2, 0)):
            key = tuple(sorted((c[a], c[b])))
            edge_cells.setdefault(key, []).append(it)
    used = set()
    picked = []
    cells = [list(c) for c in m.cells]
    done = 0
    # visit edges in a scrambled but fixed order, so the flips spread
    # over the square
    for key in sorted(edge_cells,
                      key=lambda k: (k[0] * 2654435761 + k[1]) % 4294967296):
        ts = edge_cells[key]
        if len(ts) != 2 or done == nflip:
            continue
        t1, t2 = ts
        a, b = key
        c = [v for v in cells[t1] if v not in key][0]
        d = [v for v in cells[t2] if v not in key][0]
        quad = {a, b, c, d}
        P = m.xyz
        mid = [(P[a][q] + P[b][q]) / 2 for q in range(3)]
        if quad & used or any(math.dist(mid, x) < 0.25 for x in picked):
            continue
        # convex: c and d on opposite sides of ab, a and b of cd
        s1 = cross(sub(P[b], P[a]), sub(P[c], P[a]))[2]
        s2 = cross(sub(P[b], P[a]), sub(P[d], P[a]))[2]
        s3 = cross(sub(P[d], P[c]), sub(P[a], P[c]))[2]
        s4 = cross(sub(P[d], P[c]), sub(P[b], P[c]))[2]
        if not (s1 * s2 < 0 and s3 * s4 < 0):
            continue
        # keep each triangle's orientation (counterclockwise in z)
        n1 = [c, a, d] if cross(sub(P[a], P[c]),
                                sub(P[d], P[c]))[2] > 0 else [c, d, a]
        n2 = [c, d, b] if cross(sub(P[d], P[c]),
                                sub(P[b], P[c]))[2] > 0 else [c, b, d]
        trial = [list(x) for x in cells]
        trial[t1], trial[t2] = n1, n2
        mm = type(m)()
        mm.xyz, mm.cells, mm.clr, mm.nen = m.xyz, trial, m.clr, 3
        r = analyze(mm)
        name = tuple(sorted((c + 1, d + 1)))
        sign = [s for k, s in r.interior.items() if k[:2] == name]
        if not sign or sign[0] >= 0:
            continue
        cells = trial
        used |= quad
        picked.append(mid)
        done += 1
    return cells, done


def flip3d(m, nflip=3):
    """2-3 flips where segment de crosses the inside of face abc, at
    least 0.15 apart (the box is 0.4 x 0.4 x 0.2)."""
    P = m.xyz
    face_cells = {}
    for it, c in enumerate(m.cells):
        for f in ((1, 2, 3), (0, 3, 2), (0, 1, 3), (0, 2, 1)):
            key = tuple(sorted(c[q] for q in f))
            face_cells.setdefault(key, []).append(it)
    cells = [list(c) for c in m.cells]
    used = set()
    picked = []
    removed = []
    added = []
    for key in sorted(face_cells):
        ts = face_cells[key]
        if len(ts) != 2 or len(added) == 3 * nflip:
            continue
        t1, t2 = ts
        a, b, c = key
        d = [v for v in cells[t1] if v not in key][0]
        e = [v for v in cells[t2] if v not in key][0]
        mid = [(P[a][q] + P[b][q] + P[c][q]) / 3 for q in range(3)]
        if {a, b, c, d, e} & used or \
                any(math.dist(mid, x) < 0.15 for x in picked):
            continue
        # de crosses triangle abc: d and e on opposite sides of abc,
        # and de passes inside each edge
        if orient3(P[a], P[b], P[c], P[d]) * \
                orient3(P[a], P[b], P[c], P[e]) >= 0:
            continue
        sides = [orient3(P[d], P[e], P[a], P[b]),
                 orient3(P[d], P[e], P[b], P[c]),
                 orient3(P[d], P[e], P[c], P[a])]
        if not (all(s > 0 for s in sides) or all(s < 0 for s in sides)):
            continue
        new = []
        for x, y in ((a, b), (b, c), (c, a)):
            t = [x, y, d, e]
            if orient3(*[P[v] for v in t]) < 0:
                t = [y, x, d, e]
            new.append(t)
        removed += [t1, t2]
        added += new
        used |= {a, b, c, d, e}
        picked.append(mid)
    out = [c for i, c in enumerate(cells) if i not in set(removed)]
    out += added
    return out, len(added) // 3


def kuhn(nx=4, ny=3, nz=3):
    def vid(i, j, k):
        return i + (nx + 1) * (j + (ny + 1) * k)
    xyz = [(float(i), float(j), float(k)) for k in range(nz + 1)
           for j in range(ny + 1) for i in range(nx + 1)]
    cells = []
    perms = [(0, 1, 2), (0, 2, 1), (1, 0, 2), (1, 2, 0), (2, 0, 1),
             (2, 1, 0)]
    for k in range(nz):
        for j in range(ny):
            for i in range(nx):
                for p in perms:
                    v = [0, 0, 0]
                    t = [vid(i, j, k)]
                    for ax in p:
                        v[ax] += 1
                        t.append(vid(i + v[0], j + v[1], k + v[2]))
                    if orient3(*[xyz[q] for q in t]) < 0:
                        t[2], t[3] = t[3], t[2]
                    cells.append(t)
    return xyz, cells


def main():
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)

    m = read_avs(os.path.join(LAGRIT, 'output01_square_uniform.inp'))
    cells, n = flip2d(m)
    write_avs(os.path.join(out, 'flip2d.inp'), m.xyz, cells, m.clr, 'tri')
    print('flip2d.inp: %d edges flipped' % n)

    m = read_avs(os.path.join(LAGRIT, 'test_3d_poi_01.inp'))
    cells, n = flip3d(m)
    write_avs(os.path.join(out, 'flip3d.inp'), m.xyz, cells,
              [1] * len(cells), 'tet')
    print('flip3d.inp: %d 2-3 flips' % n)

    xyz, cells = kuhn()
    write_avs(os.path.join(out, 'kuhn.inp'), xyz, cells, [1] * len(cells),
              'tet')
    print('kuhn.inp: %d tets' % len(cells))

    xyz = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0),
           (0.0, 1.0 + 1e-6, 0.0)]
    write_avs(os.path.join(out, 'near_tie.inp'), xyz, [[0, 1, 3], [1, 2, 3]],
              [1, 1], 'tri', digits=17)
    print('near_tie.inp: 2 triangles')

    m = read_avs(os.path.join(LAGRIT, 'test_3d_poi_01.inp'))
    off = (7.0e5, 3.2e6, 1.5e3)
    xyz = [(p[0] + off[0], p[1] + off[1], p[2] + off[2]) for p in m.xyz]
    write_avs(os.path.join(out, 'utm.inp'), xyz, m.cells, m.clr, 'tet',
              digits=17)
    print('utm.inp: moved by (7e5, 3.2e6, 1.5e3)')


if __name__ == '__main__':
    main()
