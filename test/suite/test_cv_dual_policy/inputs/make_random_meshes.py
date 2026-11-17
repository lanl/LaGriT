"""make_random_meshes.py: the points of test_policy's random meshes.

Writes pts2d.txt and pts3d.txt here (Python 3 standard library, fixed
seeds, so the same points on every machine). LaGriT's connect then
makes the meshes (make_random_meshes.lgi):

  cd test/suite/test_policy/inputs
  python3 -I make_random_meshes.py
  lagrit < make_random_meshes.lgi

rnd2d: the unit square: its 4 corners, 40 points on its sides, 260
       inside (304 vertices, 562 triangles).
rnd3d: the unit cube: its 8 corners, 150 points on its faces, 600
       inside (758 vertices, 4547 tets).

Random points on a boundary make obtuse boundary triangles and
boundary tets whose circumcenter lies outside: negative Voronoi
coefficients that the hybrid must remove, many more than a mesh
generator would leave. The .inp files made this way are kept in git,
so the suite does not need LaGriT.
"""

import random

random.seed(7)
pts = [(0, 0), (1, 0), (1, 1), (0, 1)]
for _ in range(40):
    s = random.randrange(4)
    t = random.random()
    pts.append([(t, 0), (1, t), (t, 1), (0, t)][s])
for _ in range(260):
    pts.append((random.random(), random.random()))
with open('pts2d.txt', 'w') as f:
    for p in pts:
        f.write('%.17g %.17g 0.\n' % p)

random.seed(4)
pts = []
for x in (0, 1):
    for y in (0, 1):
        for z in (0, 1):
            pts.append((x, y, z))
for _ in range(150):
    f = random.randrange(6)
    a, b = random.random(), random.random()
    ax = f // 2
    p = [a, b]
    p.insert(ax, float(f % 2))
    pts.append(tuple(p))
for _ in range(600):
    pts.append((random.random(), random.random(), random.random()))
with open('pts3d.inp', 'w') as f:
    f.write('%d 0 0 0 0\n' % len(pts))
    for i, p in enumerate(pts, 1):
        f.write('%d %.17g %.17g %.17g\n' % (i, p[0], p[1], p[2]))
