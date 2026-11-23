#!/usr/bin/env python3
"""make_medial_inputs.py WORK

Writes the AVS inputs of test_medial_axis into WORK: 2D line meshes in
the xy plane (loops counterclockwise, so the segment normals point
out) and 3D closed triangle surfaces with outward normals. Python 3,
standard library.
"""
import math
import os
import random
import sys


def write_avs(path, pts, cells, ctype):
    with open(path, "w") as f:
        f.write("%d %d 0 0 0\n" % (len(pts), len(cells)))
        for i, p in enumerate(pts):
            z = p[2] if len(p) > 2 else 0.0
            f.write("%d %.15g %.15g %.15g\n" % (i + 1, p[0], p[1], z))
        for k, c in enumerate(cells):
            f.write("%d 1 %s %s\n" % (k + 1, ctype,
                                      " ".join(str(v + 1) for v in c)))


def loop(n, start=0):
    return [(start + i, start + (i + 1) % n) for i in range(n)]


def polyline(corners, ds):
    """Points along a closed polygon, every ds or less, corners kept."""
    out = []
    for k in range(len(corners)):
        a, b = corners[k], corners[(k + 1) % len(corners)]
        L = math.hypot(b[0] - a[0], b[1] - a[1])
        n = max(1, int(math.ceil(L / ds - 1e-9)))
        for i in range(n):
            t = i / n
            out.append((a[0] + t * (b[0] - a[0]), a[1] + t * (b[1] - a[1])))
    return out


# ------------------------------------------------------------------ 2D
def circle(work):
    N = 128
    pts = [(math.cos(2 * math.pi * i / N), math.sin(2 * math.pi * i / N))
           for i in range(N)]
    write_avs(os.path.join(work, "ma01_circle.inp"), pts, loop(N), "line")


def slab(work):
    pts = polyline([(0, 0), (10, 0), (10, 1), (0, 1)], 0.1)
    write_avs(os.path.join(work, "ma03_slab.inp"), pts, loop(len(pts)),
              "line")


def sawtooth(work):
    # teeth of height 0.5 and width 1 along the bottom, flat top at 2
    c = []
    for i in range(6):
        c += [(float(i), 0.0), (i + 0.5, 0.5)]
    c += [(6.0, 0.0), (6.0, 2.0), (0.0, 2.0)]
    pts = polyline(c, 0.05)
    write_avs(os.path.join(work, "ma05_sawtooth.inp"), pts,
              loop(len(pts)), "line")


def interface(work):
    # rectangle 10 x 3 (counterclockwise) and an interface at y = 1
    # from left to right: material below 1 thick, above 2 thick
    ds = 0.25
    box = polyline([(0, 0), (10, 0), (10, 1), (10, 3), (0, 3), (0, 1)], ds)
    n = len(box)
    segs = loop(n)
    key = {(round(p[0], 9), round(p[1], 9)): i for i, p in enumerate(box)}
    pts = list(box)
    m = int(round(10 / ds))
    ids = []
    for i in range(m + 1):
        p = (i * ds, 1.0)
        k = (round(p[0], 9), round(p[1], 9))
        if k in key:
            ids.append(key[k])
        else:
            pts.append(p)
            ids.append(len(pts) - 1)
    segs += [(ids[i], ids[i + 1]) for i in range(m)]
    write_avs(os.path.join(work, "ma06_interface.inp"), pts, segs, "line")


# ------------------------------------------------------------------ 3D
def icosphere(path, level):
    t = (1 + math.sqrt(5)) / 2
    V = [(-1, t, 0), (1, t, 0), (-1, -t, 0), (1, -t, 0), (0, -1, t),
         (0, 1, t), (0, -1, -t), (0, 1, -t), (t, 0, -1), (t, 0, 1),
         (-t, 0, -1), (-t, 0, 1)]
    V = [tuple(c / math.sqrt(sum(x * x for x in v)) for c in v) for v in V]
    F = [(0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11),
         (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8),
         (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9),
         (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)]
    for _ in range(level):
        mid = {}

        def m(a, b):
            k = (min(a, b), max(a, b))
            if k not in mid:
                p = [(V[a][i] + V[b][i]) / 2 for i in range(3)]
                r = math.sqrt(sum(x * x for x in p))
                V.append(tuple(x / r for x in p))
                mid[k] = len(V) - 1
            return mid[k]
        G = []
        for a, b, c in F:
            ab, bc, ca = m(a, b), m(b, c), m(c, a)
            G += [(a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca)]
        F = G
    write_avs(path, V, F, "tri")


def block(path, Lx, Ly, ni, nj, nk, top):
    """The boundary of the mapped block 0<x<Lx, 0<y<Ly, 0<z<top(x,y),
    triangles with outward normals; vertices that coincide (a top of
    0) are merged and triangles with no area dropped."""
    def X(i, j, k):
        x, y = Lx * i / ni, Ly * j / nj
        return (x, y, top(x, y) * k / nk)
    pts, key, tris = [], {}, []

    def vid(i, j, k):
        p = X(i, j, k)
        kk = tuple(round(c, 9) for c in p)
        if kk not in key:
            key[kk] = len(pts)
            pts.append(p)
        return key[kk]

    def quad(a, b, c, d, flip):
        q = [vid(*a), vid(*b), vid(*c), vid(*d)]
        if flip:
            q.reverse()
        for t in ((q[0], q[1], q[2]), (q[0], q[2], q[3])):
            A, B, C = (pts[v] for v in t)
            u = [B[i] - A[i] for i in range(3)]
            w = [C[i] - A[i] for i in range(3)]
            n = (u[1] * w[2] - u[2] * w[1], u[2] * w[0] - u[0] * w[2],
                 u[0] * w[1] - u[1] * w[0])
            if math.sqrt(sum(x * x for x in n)) > 1e-12:
                tris.append(t)
    for i in range(ni):
        for j in range(nj):
            quad((i, j, nk), (i + 1, j, nk), (i + 1, j + 1, nk),
                 (i, j + 1, nk), False)
            quad((i, j, 0), (i + 1, j, 0), (i + 1, j + 1, 0),
                 (i, j + 1, 0), True)
    for j in range(nj):
        for k in range(nk):
            quad((ni, j, k), (ni, j + 1, k), (ni, j + 1, k + 1),
                 (ni, j, k + 1), False)
            quad((0, j, k), (0, j + 1, k), (0, j + 1, k + 1),
                 (0, j, k + 1), True)
    for i in range(ni):
        for k in range(nk):
            quad((i, 0, k), (i + 1, 0, k), (i + 1, 0, k + 1),
                 (i, 0, k + 1), False)
            quad((i, nj, k), (i + 1, nj, k), (i + 1, nj, k + 1),
                 (i, nj, k + 1), True)
    vol = 0.0                       # outward normals: volume > 0
    for a, b, c in tris:
        A, B, C = pts[a], pts[b], pts[c]
        vol += (A[0] * (B[1] * C[2] - B[2] * C[1]) -
                A[1] * (B[0] * C[2] - B[2] * C[0]) +
                A[2] * (B[0] * C[1] - B[1] * C[0])) / 6
    assert vol > 0, path
    write_avs(path, pts, tris, "tri")


def main(work):
    os.makedirs(work, exist_ok=True)
    circle(work)
    icosphere(os.path.join(work, "ma02_sphere.inp"), 3)
    slab(work)
    block(os.path.join(work, "ma04_box.inp"), 10, 10, 20, 20, 2,
          lambda x, y: 1.0)
    sawtooth(work)
    interface(work)
    block(os.path.join(work, "ma07_wedge.inp"), 10, 4, 40, 16, 4,
          lambda x, y: 0.2 * x)
    block(os.path.join(work, "ma08_dome.inp"), 8, 8, 24, 24, 3,
          lambda x, y: 1 + 0.8 * math.exp(-((x - 4) ** 2 +
                                              (y - 4) ** 2) / 2))
    # ma09: a tet, quads, and points with no cells (refused)
    write_avs(os.path.join(work, "ma09_tet.inp"),
              [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)],
              [(0, 1, 2, 3)], "tet")
    write_avs(os.path.join(work, "ma09_quad.inp"),
              [(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)],
              [(0, 1, 2, 3)], "quad")
    write_avs(os.path.join(work, "ma09_points.inp"),
              [(0, 0, 0), (1, 0, 0)], [], "line")
    # ma10: timing, a layer 20 x 20 with two domes (41,600 triangles)
    block(os.path.join(work, "ma10_biglayer.inp"), 20, 20, 100, 100, 4,
          lambda x, y: 1 + 0.8 * math.exp(-((x - 6) ** 2 +
                                              (y - 7) ** 2) / 4)
          + 1.5 * math.exp(-((x - 14) ** 2 + (y - 12) ** 2) / 9))
    more(work)


# ------------------------------------------------- added Nov 23 (ma11+)
def oriented(pts, quads, outward):
    """Triangles from quads (a,b,c,d), each turned so its normal
    points along outward(centroid); triangles with no area dropped."""
    tris = []
    for q in quads:
        for t in ((q[0], q[1], q[2]), (q[0], q[2], q[3])):
            A, B, C = (pts[v] for v in t)
            u = [B[i] - A[i] for i in range(3)]
            w = [C[i] - A[i] for i in range(3)]
            n = (u[1] * w[2] - u[2] * w[1], u[2] * w[0] - u[0] * w[2],
                 u[0] * w[1] - u[1] * w[0])
            if math.sqrt(sum(x * x for x in n)) <= 1e-12:
                continue
            g = [(A[i] + B[i] + C[i]) / 3 for i in range(3)]
            d = outward(g)
            if sum(n[i] * d[i] for i in range(3)) < 0:
                t = (t[0], t[2], t[1])
            tris.append(t)
    return tris


def volume(pts, tris):
    v = 0.0
    for a, b, c in tris:
        A, B, C = pts[a], pts[b], pts[c]
        v += (A[0] * (B[1] * C[2] - B[2] * C[1]) -
              A[1] * (B[0] * C[2] - B[2] * C[0]) +
              A[2] * (B[0] * C[1] - B[1] * C[0])) / 6
    return v


def ellipse(path, a, b, N):
    pts = [(a * math.cos(2 * math.pi * i / N), b * math.sin(2 * math.pi * i / N))
           for i in range(N)]
    write_avs(path, pts, loop(N), "line")


def ellipsoid(path, a, level):
    # the icosphere stretched along x: every vertex on the ellipsoid
    # x^2/a^2 + y^2 + z^2 = 1
    icosphere(path, level)
    lines = open(path).read().split("\n")
    nn = int(lines[0].split()[0])
    for i in range(1, nn + 1):
        w = lines[i].split()
        lines[i] = "%s %.15g %s %s" % (w[0], a * float(w[1]), w[2], w[3])
    open(path, "w").write("\n".join(lines))


def annulus(path, r1, r2, n1, n2):
    outer = [(r2 * math.cos(2 * math.pi * i / n2),
              r2 * math.sin(2 * math.pi * i / n2)) for i in range(n2)]
    inner = [(r1 * math.cos(-2 * math.pi * i / n1),
              r1 * math.sin(-2 * math.pi * i / n1)) for i in range(n1)]
    write_avs(path, outer + inner, loop(n2) + loop(n1, n2), "line")


def tube(path, r1, r2, L, nt, nz, nr):
    """A pipe: walls at radii r1 < r2, 0 < z < L, annular caps."""
    pts, key = [], {}

    def vid(r, t, z):
        p = (r * math.cos(2 * math.pi * t / nt),
             r * math.sin(2 * math.pi * t / nt), z)
        k = tuple(round(c, 9) for c in p)
        if k not in key:
            key[k] = len(pts)
            pts.append(p)
        return key[k]
    quads = []
    for t in range(nt):
        for k in range(nz):
            z0, z1 = L * k / nz, L * (k + 1) / nz
            for r in (r1, r2):
                quads.append((vid(r, t, z0), vid(r, t + 1, z0),
                              vid(r, t + 1, z1), vid(r, t, z1)))
        for j in range(nr):
            ra = r1 + (r2 - r1) * j / nr
            rb = r1 + (r2 - r1) * (j + 1) / nr
            for z in (0.0, L):
                quads.append((vid(ra, t, z), vid(rb, t, z),
                              vid(rb, t + 1, z), vid(ra, t + 1, z)))

    def outward(g):
        x, y, z = g
        r = math.hypot(x, y)
        if z < 1e-9:
            return (0, 0, -1)
        if z > L - 1e-9:
            return (0, 0, 1)
        if r > 0.5 * (r1 + r2):
            return (x, y, 0)
        return (-x, -y, 0)
    tris = oriented(pts, quads, outward)
    assert volume(pts, tris) > 0
    write_avs(path, pts, tris, "tri")


def arc(c, R, a0, a1, ds):
    n = max(2, int(math.ceil(abs(a1 - a0) * R / ds)))
    return [(c[0] + R * math.cos(a0 + (a1 - a0) * i / n),
             c[1] + R * math.sin(a0 + (a1 - a0) * i / n)) for i in range(n)]


def neck(path, w, ds_arc, ds_line):
    """Two disks of radius 1 at (-2,0) and (2,0) joined by a channel
    of half-width w (a dumbbell), counterclockwise."""
    x0 = 2 - math.sqrt(1 - w * w)
    a = math.asin(w)
    n = int(math.ceil(2 * x0 / ds_line))
    pts = [(-x0 + 2 * x0 * i / n, -w) for i in range(n)]
    pts += arc((2, 0), 1, math.pi + a, 3 * math.pi - a, ds_arc)
    pts += [(x0 - 2 * x0 * i / n, w) for i in range(n)]
    pts += arc((-2, 0), 1, a, 2 * math.pi - a, ds_arc)
    write_avs(path, pts, loop(len(pts)), "line")


def noisy_slab(path, eps, seed):
    """The slab 10 x 1 (segments 0.05) with its long faces moved up or
    down by up to eps (uniform, fixed seed); corners not moved."""
    rnd = random.Random(seed)
    pts = polyline([(0, 0), (10, 0), (10, 1), (0, 1)], 0.05)
    out = []
    for x, y in pts:
        if y in (0.0, 1.0) and 0 < x < 10:
            y += rnd.uniform(-eps, eps)
        out.append((x, y))
    write_avs(path, out, loop(len(out)), "line")


def shifted(src, dst, d):
    lines = open(src).read().split("\n")
    nn = int(lines[0].split()[0])
    for i in range(1, nn + 1):
        w = lines[i].split()
        lines[i] = "%s %.15g %.15g %.15g" % (
            w[0], float(w[1]) + d[0], float(w[2]) + d[1], float(w[3]) + d[2])
    open(dst, "w").write("\n".join(lines))


def interface3d(path, L, n, zs):
    """A box L x L x zs[-1] with an interior sheet at z = zs[1] (two
    materials); the sheet's rim is on edges of 3 or 4 triangles."""
    nk = len(zs) - 1
    pts, key = [], {}

    def vid(i, j, k):
        p = (L * i / n, L * j / n, zs[k])
        kk = tuple(round(c, 9) for c in p)
        if kk not in key:
            key[kk] = len(pts)
            pts.append(p)
        return key[kk]
    quads = []
    for i in range(n):
        for j in range(n):
            for k in range(nk + 1):    # bottom, sheet(s), top
                quads.append((vid(i, j, k), vid(i + 1, j, k),
                              vid(i + 1, j + 1, k), vid(i, j + 1, k)))
    for a in range(n):
        for k in range(nk):
            for fix in (0, n):
                quads.append((vid(fix, a, k), vid(fix, a + 1, k),
                              vid(fix, a + 1, k + 1), vid(fix, a, k + 1)))
                quads.append((vid(a, fix, k), vid(a + 1, fix, k),
                              vid(a + 1, fix, k + 1), vid(a, fix, k + 1)))

    def outward(g):
        x, y, z = g
        e = 1e-9
        if x < e:
            return (-1, 0, 0)
        if x > L - e:
            return (1, 0, 0)
        if y < e:
            return (0, -1, 0)
        if y > L - e:
            return (0, 1, 0)
        if z < e:
            return (0, 0, -1)
        return (0, 0, 1)               # the top, and the sheet: +z
    write_avs(path, pts, oriented(pts, quads, outward), "tri")


def more(work):
    j = os.path.join
    ellipse(j(work, "ma11_ellipse.inp"), 2.0, 1.0, 256)
    ellipsoid(j(work, "ma12_ellipsoid.inp"), 2.0, 4)
    annulus(j(work, "ma13_annulus.inp"), 1.0, 2.0, 128, 256)
    tube(j(work, "ma14_tube.inp"), 1.0, 1.5, 6.0, 64, 24, 2)
    neck(j(work, "ma15_neck.inp"), 0.2, 2 * math.pi / 128, 0.05)
    noisy_slab(j(work, "ma16_noise_small.inp"), 0.002, 16)
    noisy_slab(j(work, "ma16_noise_large.inp"), 0.01, 16)
    for N in (16, 32, 64, 128, 256, 512):
        ellipse(j(work, "ma17_circle_%d.inp" % N), 1.0, 1.0, N)
    for lev in (1, 2, 3, 4):
        icosphere(j(work, "ma17_sphere_%d.inp" % lev), lev)
    shifted(j(work, "ma03_slab.inp"), j(work, "ma18_slab_utm.inp"),
            (500000.0, 4000000.0, 0.0))
    shifted(j(work, "ma04_box.inp"), j(work, "ma18_box_utm.inp"),
            (500000.0, 4000000.0, 1500.0))
    interface3d(j(work, "ma19_interface3d.inp"), 6.0, 12, [0.0, 1.0, 3.0])


if __name__ == "__main__":
    main(sys.argv[1])
