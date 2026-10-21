#!/usr/bin/env python3
"""
run_one.py <polygon>.poly [ccw|cw]

Runs standalone/tri_driver on one test polygon and judges the result
with ../check_triangulate.py, exactly as the suite judges LaGriT's
output. The mesh is written to standalone_<name>.inp here.
"""
import os
import subprocess
import sys

here = os.path.dirname(os.path.abspath(__file__))
poly = sys.argv[1]
orient = sys.argv[2] if len(sys.argv) > 2 else "ccw"
with open(poly) as f:
    n = int(f.readline().split()[0])
    pts = [f.readline().split()[1:3] for _ in range(n)]
txt = os.path.join(here, "poly.txt")
with open(txt, "w") as f:
    f.write("%d\n" % n)
    for x, y in pts:
        f.write("%s %s\n" % (x, y))
p = subprocess.run([os.path.join(here, "tri_driver"), txt, orient],
                   capture_output=True, text=True)
lines = p.stdout.split("\n")
for l in lines:
    if l.startswith(("attempt", "result", "dropped")):
        print(l)
res = [l for l in lines if l.startswith("result")]
if not res or res[0].split()[1] != "OK":
    sys.exit(1)
cells = [l.split() for l in lines[lines.index(res[0]) + 1:] if l.strip()]
keep = n - (1 if any(l.startswith("dropped") for l in lines) else 0)
out = os.path.join(here, "standalone_" +
                   os.path.basename(poly)[:-5] + ".inp")
with open(out, "w") as f:
    f.write("%d %d 0 0 0\n" % (keep, len(cells)))
    for i in range(keep):
        f.write("%d %s %s 0.0\n" % (i + 1, pts[i][0], pts[i][1]))
    for k, c in enumerate(cells):
        f.write("%d 1 tri %s %s %s\n" % (k + 1, c[0], c[1], c[2]))
sys.exit(subprocess.call([sys.executable,
                          os.path.join(here, "..", "check_triangulate.py"),
                          poly, out]))
