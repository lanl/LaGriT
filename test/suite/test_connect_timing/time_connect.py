#!/usr/bin/env python3
"""time_connect.py LAGRIT LOGDIR WORKDIR [big]

Times LaGriT's connect (Delaunay) in 2D (triplane, triangles) and 3D
(tet) as the number of vertices grows, for four vertex orders, and
checks every mesh. Called by run_connect_timing.sh; Python 3, standard
library only. User, Oct 17 2026: "is it linear, better than linear,
worse than linear ... test if vertex order (random vs. sort/reorder
based on xyz coordinates) makes a difference".

The points
  The 4 (2D) or 8 (3D) corners of the unit box, then uniformly random
  points inside it (seed 1, Python's random; the same set for every
  order), as LaGriT's connect documentation does: the corners make the
  convex hull the unit box (area or volume 1).
Sizes
  quick (default):   1000, 2000, 4000, 8000, 16000 vertices
  BIG=1:             1000, 2000, ... 256000 (doubling), about 5 minutes
Orders (the same points; only the order in the mesh object differs)
  random   as generated
  xyz      LaGriT's multi-key sort / index / ascending / ikey /
           xic yic zic, then reorder. y breaks ties in x, z ties in y;
           random coordinates never tie, so the result is the order of
           x alone: a sweep along x
  xyz_bins the same multi-key sort on binned coordinates (all LaGriT
           commands): x is cut into K slabs (2D: xbin yic zic) or x and y
           into K x K columns (3D: xbin ybin zic), K = BIN_C N^(1/dim)
           rounded (BIN_C = 0.25, the best of 0.06 to 2 at 64000
           vertices: 2D 63 slabs, 3D 10 x 10 columns). The bins: xs = xic * K
           (math / multiply into a VDOUBLE), then cmo / copyatt into a
           VINT, which truncates (the coordinates are >= 0, so
           truncation = floor). The vertices run along y in one slab,
           then the next slab: neighbours in the list are neighbours in
           space. (math / multiply straight into a VINT gives a wrong
           value, 9 for every vertex in a test: see 00README.)
  morton   a Z-order (Morton) curve over the box (written by this
           script): nearby points are near each other in the list
Timing
  timed deck  read / avs, imt 1, itp 0, [sort, reorder], connect, finish
  base deck   the same without connect
  connect time = (fastest timed run) - (fastest base run): LaGriT's
  start-up, reading and sorting are not counted. 3 runs each up to
  32000 vertices, 1 above (run-to-run noise is small there).
Checks (one more run, not timed)
  the timed deck + itetclr 1 + quality / volume / 0 / lt 0. (inverted
  cells) + quality / volume / 0 / itetclr (total volume), and, for the
  smaller sizes, a dump of the mesh for the checks of check_connect.py
  (every vertex used; Delaunay: no vertex inside a cell's circumcircle
  or circumsphere, for each pair of neighbouring cells).
Writes LOGDIR/results.txt (one line per case: key=value; ...) and keeps
each case's decks and logs in WORKDIR.
"""
import os
import random
import subprocess
import sys
import time

QUICK = [1000, 2000, 4000, 8000, 16000]
BIG = [1000 * 2 ** k for k in range(9)]          # 1000 ... 256000
ORDERS = ["random", "xyz", "xyz_bins", "morton"]
BIN_C = 0.25                                     # bins: K = BIN_C N^(1/dim)
PIC_N = {2: 1000, 3: 3000}                       # meshes drawn on the page
DUMP_MAX = {2: 32000, 3: 8000}                   # meshes checked in full
SEED = 1


def points(dim, n):
    r = random.Random(SEED)
    if dim == 3:
        P = [(x, y, z) for x in (0.0, 1.0) for y in (0.0, 1.0) for z in (0.0, 1.0)]
    else:
        P = [(x, y, 0.0) for x in (0.0, 1.0) for y in (0.0, 1.0)]
    while len(P) < n:
        P.append((r.random(), r.random(), r.random() if dim == 3 else 0.0))
    return P


def morton_key(p, dim, bits=20):
    """the point's place on a Z-order (Morton) curve. Each coordinate
    (0 to 1) becomes a 20-bit integer (0 to 2^20 - 1); the key takes
    their bits in turn from the top: x19 y19 (z19) x18 y18 (z18) ...
    The first dim bits say which half (2D: quadrant, 3D: octant) of the
    box the point is in, the next dim bits which quadrant of that, and
    so on: sorting by the key visits the box quadrant by quadrant,
    recursively, in a Z pattern, so points close in the list are close
    in space. 2D: 40-bit key, 3D: 60-bit (Python integers are exact)."""
    q = [min(int(c * (1 << bits)), (1 << bits) - 1) for c in p[:dim]]
    key = 0
    for b in range(bits - 1, -1, -1):
        for c in q:
            key = (key << 1) | ((c >> b) & 1)
    return key


def write_avs(path, P):
    with open(path, "w") as f:
        f.write("%d 0 0 0 0\n" % len(P))
        for i, p in enumerate(P):
            f.write("%d %.15e %.15e %.15e\n" % ((i + 1,) + p))


def nbins(dim, n):
    return max(1, int(round(BIN_C * n ** (1.0 / dim))))


def deck(dim, pts, order, connect, check, dump, n=0, picture=False):
    L = ["cmo / create / mo_pts / / / %s" % ("tet" if dim == 3 else "triplane"),
         "read / avs / %s / mo_pts" % pts,
         "cmo / setatt / mo_pts / imt / 1,0,0 / 1",
         "cmo / setatt / mo_pts / itp / 1,0,0 / 0"]
    if order == "xyz":
        L += ["sort / mo_pts / index / ascending / ikey / xic yic zic",
              "reorder / mo_pts / ikey"]
    if order == "xyz_bins":
        K = nbins(dim, n)
        for c in "xy"[:dim - 1]:
            L += ["cmo / addatt / mo_pts / %ss / VDOUBLE / scalar / nnodes" % c,
                  "cmo / addatt / mo_pts / %sbin / VINT / scalar / nnodes" % c,
                  "math / multiply / mo_pts / %ss / 1,0,0 / mo_pts / %sic / %d."
                  % (c, c, K),
                  "cmo / copyatt / mo_pts mo_pts / %sbin %ss" % (c, c)]
        L += ["sort / mo_pts / index / ascending / ikey / "
              + ("xbin yic zic" if dim == 2 else "xbin ybin zic"),
              "reorder / mo_pts / ikey"]
    if connect:
        L.append("connect")
    if picture:
        L += ["cmo / set_id / mo_pts / node / vnum",
              "dump / avs / pic.inp / mo_pts",
              "finish"]
        return "\n".join(L) + "\n"
    if check:
        L += ["cmo / setatt / mo_pts / itetclr / 1,0,0 / 1",
              "quality / volume / 0 / lt 0.",
              "quality / volume / 0 / itetclr"]
        if dump:
            L.append("dump / avs / mesh.inp / mo_pts")
    L.append("finish")
    return "\n".join(L) + "\n"


def run(lagrit, d, deckname):
    t0 = time.perf_counter()
    with open(os.path.join(d, deckname)) as fin, \
            open(os.path.join(d, deckname + ".out"), "w") as fout:
        rc = subprocess.call([lagrit], stdin=fin, stdout=fout,
                             stderr=subprocess.STDOUT, cwd=d)
    return time.perf_counter() - t0, rc


def parse_check(text):
    r = {"cells": None, "negative": None, "volume": None, "finished": False}
    lines = text.split("\n")
    for k, l in enumerate(lines):
        w = l.split()
        if l.strip().startswith("Found") and "elements lt" in l:
            r["negative"] = int(w[1])
        if "total elements evaluated" in l:
            r["cells"] = int(w[0])
        if l.split()[:2] == ["Color", "Num."] and k + 1 < len(lines):
            v = lines[k + 1].split()
            if len(v) >= 3:
                r["volume"] = float(v[2].replace("D", "E"))
        if "LaGriT successfully completed" in l:
            r["finished"] = True
    return r


def main(argv):
    lagrit, logdir, work = argv[1:4]
    big = len(argv) > 4 and argv[4] == "big"
    sizes = BIG if big else QUICK
    out = open(os.path.join(logdir, "results.txt"), "w")
    for dim in (2, 3):
        for n in sizes:
            P = points(dim, n)
            Pm = sorted(P, key=lambda p: morton_key(p, dim))
            reps = 3 if n <= 32000 else 1
            for order in ORDERS:
                d = os.path.join(work, "d%d_n%06d_%s" % (dim, n, order))
                os.makedirs(d, exist_ok=True)
                write_avs(os.path.join(d, "pts.inp"), Pm if order == "morton" else P)
                dump = n <= DUMP_MAX[dim]
                for name, c, chk in (("timed.lgi", True, False),
                                     ("base.lgi", False, False),
                                     ("check.lgi", True, True)):
                    open(os.path.join(d, name), "w").write(
                        deck(dim, "pts.inp", order, c, chk, dump, n))
                tt, tb, rcs = [], [], []
                for _ in range(reps):
                    t, rc = run(lagrit, d, "timed.lgi")
                    tt.append(t)
                    rcs.append(rc)
                    t, rc = run(lagrit, d, "base.lgi")
                    tb.append(t)
                    rcs.append(rc)
                _, rc = run(lagrit, d, "check.lgi")
                rcs.append(rc)
                c = parse_check(open(os.path.join(d, "check.lgi.out"),
                                     errors="replace").read())
                os.remove(os.path.join(d, "pts.inp"))
                rec = dict(dim=dim, n=n, order=order, reps=reps,
                           t_connect="%.4f" % max(min(tt) - min(tb), 0.0),
                           t_timed="%.4f" % min(tt), t_base="%.4f" % min(tb),
                           rc=max(rcs), cells=c["cells"], negative=c["negative"],
                           volume=c["volume"], finished=c["finished"],
                           dir=os.path.basename(d), dumped=dump)
                out.write("; ".join("%s=%s" % kv for kv in rec.items()) + "\n")
                out.flush()
                print("  %dD %7d %-7s connect %8.3f s  cells %s" % (
                    dim, n, order, max(min(tt) - min(tb), 0.0), c["cells"]),
                    flush=True)
    out.close()
    pictures(lagrit, work)
    return 0


def pictures(lagrit, work):
    """one small mesh per dimension and order, for the page: the
    vertices numbered (vnum) after the order, so the picture shows it"""
    for dim in (2, 3):
        n = PIC_N[dim]
        P = points(dim, n)
        Pm = sorted(P, key=lambda p: morton_key(p, dim))
        for order in ORDERS:
            d = os.path.join(work, "pic_d%d_%s" % (dim, order))
            os.makedirs(d, exist_ok=True)
            write_avs(os.path.join(d, "pts.inp"), Pm if order == "morton" else P)
            open(os.path.join(d, "pic.lgi"), "w").write(
                deck(dim, "pts.inp", order, True, False, False, n, picture=True))
            _, rc = run(lagrit, d, "pic.lgi")
            print("  picture %dD %d %-8s %s" % (dim, n, order,
                  "ok" if rc == 0 else "exit %d" % rc), flush=True)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
