#!/usr/bin/env python3
"""dfn_tets.py: a 3D Poisson disk tet mesh around a DFN, with the DFN's
resolution next to the fractures and coarser away from them.

QUICK START

  python3 dfn_tets.py --dfn full_mesh.inp --json metadata.json --out my_mesh
  open my_mesh/summary.html

  The mesh is my_mesh/tets.inp (AVS). Before a big run, see what it
  will cost:

  python3 dfn_tets.py --dfn full_mesh.inp --json metadata.json --plan-only

CHANGING SETTINGS

  Every setting has a default and can be changed on the command line
  (python3 dfn_tets.py --help lists them), or kept in a settings file:

  python3 dfn_tets.py --write-settings my.ini     # all settings, explained
  (edit my.ini)
  python3 dfn_tets.py --settings my.ini --dfn full_mesh.inp --json metadata.json

  The command line wins over the settings file, which wins over the
  defaults.

THE DOMAIN

  --json FILE   a dfnWorks metadata.json: the box is "domain_size",
                centred on 0 (10 means -5 to 5)
  --box X0 Y0 Z0 X1 Y1 Z1   or give the box corners yourself

THE h FIELD (h is the spacing poisson_disk is asked for)

  h(x) = C * h_dfn(nearest DFN vertex) + SLOPE * max(0, distance to the
  DFN - FLAT * H_MIN), clipped to [H_MIN, H_MAX]
    h_dfn  the length of the DFN edges at each DFN vertex: their mean
           (stat = mean) or the shortest (stat = min), computed by
           LaGriT (cmo / addatt / mo_dfn / edge_lengths_mean / h_dfn)
    C      poisson_disk's h is the smallest distance it allows between
           vertices, so tet edges come out longer than h (about 1.67 h
           in 3D). C = 0.6 with stat = mean gives tet edges about equal
           to the DFN's next to the fractures with hex_grid and FLAT 0
           (measured on the dfnWorks test cases: median ratio 1.02 to
           1.07). With h_lookup octree, whose fill is maximal, use
           C = 0.74 with FLAT 3 (ratio 0.98 to 1.02; see octree.ini).
    SLOPE  how fast h grows away from the fractures; 0.2 or less keeps
           the tets well shaped
    FLAT   h stays at its DFN value out to FLAT * H_MIN from the DFN
           (default 0); FLAT 3 keeps about two layers of tets at the
           finest size next to the fractures
    H_MIN  the smallest C * h_dfn
    H_MAX  hmax_factor * H_MIN, but at most the smallest box size /
           min_cells

COST

  poisson_disk reads h from a lookup lattice over the whole box, spaced
  about h_spacing_scalar / 1.5, at about 165 bytes per lattice point.
  That lattice sets the memory, not the mesh. h_spacing_scalar =
  hscalar_factor * H_MIN; hscalar_factor = 2 (the most poisson_disk
  allows) makes the lattice 8 times smaller than 1 for a few percent
  in accuracy. With --h-lookup octree (a LaGriT that has it) there is
  no lattice: memory and time follow the number of tet vertices, about
  3.6 KB and 2.5 ms each, plus about 1.2 KB per DFN vertex with either lookup. A
  run whose predicted memory is over max_memory_gb is not started
  (exit code 3); --plan-only shows the prediction.

OUTPUT (in --out)

  tets.inp            the mesh (after improve_tets)
  summary.html        this run: plan, resolution, grading, angles
  result.json         the numbers on the page, for scripts
  section.html        the page's case section (dfn_tets_suite.py joins
                      these into one page)
  run.lgi, h_dfn.mlgi the LaGriT deck and h-field file, to rerun by hand
  run.out             LaGriT's output
  angles_before.table, angles_after.table, vertex_stats.table
                      LaGriT's tables behind the page

Python 3.8 or later, standard library only. Needs a LaGriT with
improve_tets and cmo / addatt / edge_lengths_* (--lagrit).
"""
import argparse
import configparser
import html
import json
import math
import os
import shutil
import subprocess
import sys
import time

E = html.escape
BLUE, ORANGE, INK, INK2, GRID = "#2a78d6", "#eb6834", "#1f2328", "#57606a", "#e6e6e3"

# measured on a 2.6 GHz Intel i7 (dfnWorks test cases, Oct 2026)
BYTES_PER_LATTICE_POINT = 165
# h_lookup octree: the octree and its neighbour grid, the tet mesh and
# improve_tets (the eight dfnWorks cases at full size, maximal fill,
# Oct 31: 2.7 to 4.2 KB, larger for the smaller meshes)
OCT_BYTES_PER_EST_VERTEX = 3600
# the DFN mesh read in (mo_dfn), with either h lookup
BYTES_PER_DFN_VERTEX = 1200
SECONDS_PER_LATTICE_POINT = 2.0e-6
SECONDS_PER_EST_VERTEX = 1.8e-3
# h_lookup octree with its maximal fill, sampling to improve_tets (the
# same runs: 2.1 to 2.7 ms per vertex)
OCT_SECONDS_PER_EST_VERTEX = 2.5e-3


def ram_bytes():
    try:
        return os.sysconf("SC_PAGE_SIZE") * os.sysconf("SC_PHYS_PAGES")
    except (ValueError, OSError, AttributeError):
        return 16e9


# ------------------------------------------------------------ settings
# name, default, type, help, group. One list drives the command line,
# the settings file (--write-settings) and the page.
SETTINGS = [
    ("dfn", None, "path", "the DFN: an AVS triangle mesh of the fractures (dfnWorks full_mesh.inp)", "input"),
    ("json", None, "path", "dfnWorks metadata.json: the box is domain_size, centred on 0", "input"),
    ("box", None, "box", "the box instead of --json: X0 Y0 Z0 X1 Y1 Z1", "input"),
    ("out", None, "path", "output folder (default: dfn_tets_<name of the DFN's folder>)", "input"),
    ("name", None, "str", "a name for this mesh on the page (default: the DFN's folder name)", "input"),
    ("stat", "mean", "str", "h_dfn: mean or min of the DFN edge lengths at each DFN vertex", "h field"),
    ("c", 0.6, "float", "h = C * h_dfn next to the DFN (0.6 with mean: tet edges = DFN edges)", "h field"),
    ("slope", 0.2, "float", "dh/dx away from the DFN (0.2 or less keeps tets well shaped)", "h field"),
    ("flat", 0.0, "float", "h does not grow until flat * H_MIN from the DFN, then grows with slope (0: grows from "
     "the DFN)", "h field"),
    ("hmax_factor", 10.0, "float", "H_MAX = hmax_factor * H_MIN ...", "h field"),
    ("min_cells", 5.0, "float", "... but at most the smallest box size / min_cells", "h field"),
    ("hscalar_factor", 2.0, "float", "h_spacing_scalar = hscalar_factor * H_MIN, 1 to 2; 2 = lookup lattice 8x smaller", "poisson_disk"),
    ("seed", 1, "int", "poisson_disk random seed", "poisson_disk"),
    ("h_lookup", "hex_grid", "str", "poisson_disk's h lookup: hex_grid (a uniform lattice over the box) or octree "
     "(adapted to h; needs a LaGriT with h_lookup octree)", "poisson_disk"),
    ("octree_k_factor", 0.75, "float", "with h_lookup octree: octree leaf size <= h / octree_k_factor", "poisson_disk"),
    ("octree_dump", False, "bool", "with h_lookup octree: also write the octree leaves as octree_leaves.inp", "poisson_disk"),
    ("improve", True, "bool", "run improve_tets on the mesh", "after"),
    ("lagrit", None, "path", "the LaGriT executable (default: lagrit on PATH)", "run"),
    ("max_memory_gb", None, "float", "do not start a run predicted to need more (default: 60% of this computer's memory)", "run"),
    ("keep_hfield", False, "bool", "also write the h lookup lattice as h_field.inp (large: one vertex per lattice point; "
     "hex_grid only, see octree_dump)", "run"),
]
DEFAULTS = {n: d for n, d, t, h, g in SETTINGS}
TYPES = {n: t for n, d, t, h, g in SETTINGS}


def to_bool(v):
    if isinstance(v, bool):
        return v
    s = str(v).strip().lower()
    if s in ("1", "yes", "true", "on", "y"):
        return True
    if s in ("0", "no", "false", "off", "n"):
        return False
    raise ValueError("not yes or no: %r" % v)


def convert(name, v):
    t = TYPES[name]
    if v is None or v == "":
        return None
    if t == "float":
        return float(v)
    if t == "int":
        return int(v)
    if t == "bool":
        return to_bool(v)
    if t == "box":
        vals = [float(x) for x in (v.replace(",", " ").split() if isinstance(v, str) else v)]
        if len(vals) != 6:
            raise ValueError("box needs 6 numbers: X0 Y0 Z0 X1 Y1 Z1")
        return vals
    return v


def write_settings(path):
    o = ["# Settings for dfn_tets.py. Use with: python3 dfn_tets.py --settings %s" % os.path.basename(path),
         "# A setting left empty takes its default. The command line wins over this file.",
         "# Paths are relative to the folder you run dfn_tets.py from.", "", "[dfn_tets]"]
    group = None
    for n, d, t, h, g in SETTINGS:
        if g != group:
            o += ["", "# ---- %s" % g]
            group = g
        o.append("# %s" % h)
        dv = "" if d is None else ("yes" if d is True else "no" if d is False else d)
        o.append("%s = %s" % (n, dv))
    with open(path, "w") as f:
        f.write("\n".join(o) + "\n")


def get_settings(argv=None):
    ap = argparse.ArgumentParser(
        prog="dfn_tets.py", formatter_class=argparse.RawDescriptionHelpFormatter,
        description="A 3D Poisson disk tet mesh around a DFN. Run with no arguments for the quick start.",
        epilog="See the top of dfn_tets.py for the h field, costs and output files.")
    ap.add_argument("--settings", help="a settings file (make one with --write-settings)")
    ap.add_argument("--write-settings", metavar="FILE", help="write every setting with its default and stop")
    ap.add_argument("--plan-only", action="store_true", help="show size, time and memory; do not run LaGriT")
    groups = {}
    for n, d, t, h, g in SETTINGS:
        grp = groups.get(g) or ap.add_argument_group(g)
        groups[g] = grp
        flag = "--" + n.replace("_", "-")
        dshow = "" if d is None else " (default %s)" % ("yes" if d is True else "no" if d is False else d)
        h = h.replace("%", "%%")
        if t == "bool":
            grp.add_argument(flag, dest=n, default=None, metavar="yes|no", help=h + dshow)
        elif t == "box":
            grp.add_argument(flag, dest=n, default=None, nargs=6, type=float,
                             metavar=("X0", "Y0", "Z0", "X1", "Y1", "Z1"), help=h)
        else:
            grp.add_argument(flag, dest=n, default=None, help=h + dshow)
    ap.add_argument("--no-improve", dest="improve_off", action="store_true", help=argparse.SUPPRESS)
    if argv is None:
        argv = sys.argv[1:]
    if not argv:
        print(__doc__)
        sys.exit(0)
    a = ap.parse_args(argv)
    if a.write_settings:
        write_settings(a.write_settings)
        print("wrote %s: edit it, then run python3 dfn_tets.py --settings %s" % (a.write_settings, a.write_settings))
        sys.exit(0)
    S = dict(DEFAULTS)
    src = {n: "default" for n in S}
    if a.settings:
        cp = configparser.ConfigParser(inline_comment_prefixes=("#", ";"))
        if not cp.read(a.settings):
            sys.exit("cannot read the settings file %s" % a.settings)
        sec = cp["dfn_tets"] if cp.has_section("dfn_tets") else {}
        for k, v in sec.items():
            if k not in S:
                sys.exit("%s: unknown setting %r (run --write-settings to see them all)" % (a.settings, k))
            try:
                cv = convert(k, v)
            except ValueError as e:
                sys.exit("%s: %s = %s: %s" % (a.settings, k, v, e))
            if cv is not None:
                S[k], src[k] = cv, "settings file"
    for n in S:
        v = getattr(a, n)
        if v is not None:
            try:
                S[n], src[n] = convert(n, v), "command line"
            except ValueError as e:
                sys.exit("--%s %s: %s" % (n.replace("_", "-"), v, e))
    if a.improve_off:
        S["improve"], src["improve"] = False, "command line"
    S["plan_only"] = a.plan_only
    # checks, with the fix in the message
    if not S["dfn"]:
        sys.exit("give the DFN: --dfn full_mesh.inp (run with no arguments for the quick start)")
    if not os.path.isfile(S["dfn"]):
        sys.exit("no DFN file %s" % S["dfn"])
    if not S["json"] and not S["box"]:
        sys.exit("give the box: --json metadata.json, or --box X0 Y0 Z0 X1 Y1 Z1")
    if S["json"] and S["box"]:
        sys.exit("give --json or --box, not both")
    if S["h_lookup"] not in ("hex_grid", "octree"):
        sys.exit("h_lookup must be hex_grid or octree, not %r" % S["h_lookup"])
    if not S["octree_k_factor"] > 0:
        sys.exit("octree_k_factor must be more than 0")
    if S["stat"] not in ("mean", "min"):
        sys.exit("stat must be mean or min, not %r" % S["stat"])
    if not 1.0 <= S["hscalar_factor"] <= 2.0:
        sys.exit("hscalar_factor must be 1 to 2 (poisson_disk needs h >= 0.49 h_spacing_scalar)")
    if S["flat"] < 0:
        sys.exit("flat must be 0 or more")
    for n in ("c", "slope", "hmax_factor", "min_cells"):
        if S[n] <= 0:
            sys.exit("%s must be more than 0" % n)
    if S["box"]:
        b = S["box"]
        if not all(b[k + 3] > b[k] for k in range(3)):
            sys.exit("box: each of X1 Y1 Z1 must be more than X0 Y0 Z0")
    dfn_dir = os.path.basename(os.path.dirname(os.path.abspath(S["dfn"]))) or "dfn"
    S["name"] = S["name"] or dfn_dir
    S["out"] = os.path.abspath(os.path.expanduser(S["out"] or ("dfn_tets_" + S["name"])))
    lg = S["lagrit"] or shutil.which("lagrit")
    if not lg:
        sys.exit("no LaGriT: give --lagrit /path/to/lagrit")
    S["lagrit"] = os.path.abspath(os.path.expanduser(lg)) if os.sep in lg else lg
    if S["max_memory_gb"] is None:
        S["max_memory_gb"] = round(0.6 * ram_bytes() / 1e9, 1)
        src["max_memory_gb"] = "60% of memory"
    S["_source"] = src
    return S


def settings_for_page(S):
    return {n: S[n] for n, d, t, h, g in SETTINGS if n not in ("out",)}


# ------------------------------------------------------------ reading
def read_avs(path, want_att=()):
    with open(path) as f:
        head = f.readline().split()
        nn, nc, nnd = int(head[0]), int(head[1]), int(head[2])
        V = [None] * nn
        for i in range(nn):
            w = f.readline().split()
            V[i] = (float(w[1]), float(w[2]), float(w[3]))
        C = []
        types = {}
        for _ in range(nc):
            w = f.readline().split()
            t = w[2].lower()
            types[t] = types.get(t, 0) + 1
            C.append((t, [int(x) - 1 for x in w[3:]]))
        att = {}
        if nnd and want_att:
            w = f.readline().split()
            na, sizes = int(w[0]), [int(x) for x in w[1:]]
            names = [f.readline().partition(",")[0].strip() for _ in range(na)]
            for i in range(nn):
                w = f.readline().split()[1:]
                col = 0
                for n, sz in zip(names, sizes):
                    if n in want_att:
                        att.setdefault(n, []).append(float(w[col]))
                    col += sz
    return V, C, types, att


def unique_edges(C):
    E2 = set()
    for t, ids in C:
        n = len(ids)
        pairs = [(a, b) for a in range(n) for b in range(a + 1, n)] if t == "tet" else \
                [(a, (a + 1) % n) for a in range(n)]
        for a, b in pairs:
            i, j = ids[a], ids[b]
            E2.add((i, j) if i < j else (j, i))
    return E2


class Nearest:
    """nearest DFN vertex: fine buckets for points near the DFN (exact), and
    a coarse grid whose cells know their nearest DFN vertex (label
    propagation) for points far from it (to within a small fraction of the
    distance). A plain ring search took hours on the large DFNs."""
    def __init__(self, P, cell, lo=None, hi=None, ncoarse=48):
        self.P, self.cell = P, cell
        self.g = {}
        for i, p in enumerate(P):
            self.g.setdefault(self.key(p), []).append(i)
        # coarse grid over the box and the DFN
        lo = [min([p[k] for p in P] + ([lo[k]] if lo else [])) for k in range(3)]
        hi = [max([p[k] for p in P] + ([hi[k]] if hi else [])) for k in range(3)]
        G = max(max(hi[k] - lo[k] for k in range(3)) / ncoarse, 2 * cell)
        self.G, self.lo = G, lo
        self.n = [int((hi[k] - lo[k]) / G) + 1 for k in range(3)]
        nx, ny, nz = self.n
        cen = lambda c: (lo[0] + (c % nx + 0.5) * G, lo[1] + (c // nx % ny + 0.5) * G,
                         lo[2] + (c // (nx * ny) + 0.5) * G)
        lab = [-1] * (nx * ny * nz)
        dd = [float("inf")] * (nx * ny * nz)
        # seed: each cell from the DFN vertices in it and its 26 neighbours
        for i, p in enumerate(P):
            c = self.ckey(p)
            ix, iy, iz = c % nx, c // nx % ny, c // (nx * ny)
            for dz in (-1, 0, 1):
                for dy in (-1, 0, 1):
                    for dx in (-1, 0, 1):
                        jx, jy, jz = ix + dx, iy + dy, iz + dz
                        if 0 <= jx < nx and 0 <= jy < ny and 0 <= jz < nz:
                            j = jx + nx * (jy + ny * jz)
                            d = math.dist(p, cen(j))
                            if d < dd[j]:
                                dd[j], lab[j] = d, i
        # forward and backward sweeps over the 26 neighbours, until no change
        offs = [(dx, dy, dz) for dz in (-1, 0, 1) for dy in (-1, 0, 1) for dx in (-1, 0, 1)
                if (dx, dy, dz) != (0, 0, 0)]
        for sweep in range(60):
            changed = 0
            order = range(nx * ny * nz) if sweep % 2 == 0 else range(nx * ny * nz - 1, -1, -1)
            for c in order:
                ix, iy, iz = c % nx, c // nx % ny, c // (nx * ny)
                pc = cen(c)
                for dx, dy, dz in offs:
                    jx, jy, jz = ix + dx, iy + dy, iz + dz
                    if 0 <= jx < nx and 0 <= jy < ny and 0 <= jz < nz:
                        l = lab[jx + nx * (jy + ny * jz)]
                        if l >= 0 and l != lab[c]:
                            d = math.dist(P[l], pc)
                            if d < dd[c]:
                                dd[c], lab[c] = d, l
                                changed += 1
            if changed == 0 and sweep > 0:
                break
        self.lab = lab

    def key(self, p):
        return (int(math.floor(p[0] / self.cell)), int(math.floor(p[1] / self.cell)),
                int(math.floor(p[2] / self.cell)))

    def ckey(self, p):
        nx, ny, nz = self.n
        ix, iy, iz = (min(self.n[k] - 1, max(0, int((p[k] - self.lo[k]) / self.G))) for k in range(3))
        return ix + nx * (iy + ny * iz)

    def query(self, p):
        nx, ny, nz = self.n
        c = self.ckey(p)
        ix, iy, iz = c % nx, c // nx % ny, c // (nx * ny)
        best, bi = float("inf"), -1
        for dz in (-2, -1, 0, 1, 2):
            for dy in (-2, -1, 0, 1, 2):
                for dx in (-2, -1, 0, 1, 2):
                    jx, jy, jz = ix + dx, iy + dy, iz + dz
                    if 0 <= jx < nx and 0 <= jy < ny and 0 <= jz < nz:
                        l = self.lab[jx + nx * (jy + ny * jz)]
                        if l >= 0:
                            q = self.P[l]
                            d = (q[0] - p[0]) ** 2 + (q[1] - p[1]) ** 2 + (q[2] - p[2]) ** 2
                            if d < best:
                                best, bi = d, l
        U = math.sqrt(best)
        if U > 3 * self.G:
            return U, bi          # far from the DFN: the coarse answer
        rmax = int(U / self.cell) + 1
        # near: exact, ring search on the fine buckets out to the bound U
        kx, ky, kz = self.key(p)
        for r in range(0, rmax + 1):
            for dx in range(-r, r + 1):
                for dy in range(-r, r + 1):
                    for dz in range(-r, r + 1):
                        if max(abs(dx), abs(dy), abs(dz)) != r:
                            continue
                        for i in self.g.get((kx + dx, ky + dy, kz + dz), ()):
                            q = self.P[i]
                            d = (q[0] - p[0]) ** 2 + (q[1] - p[1]) ** 2 + (q[2] - p[2]) ** 2
                            if d < best:
                                best, bi = d, i
            if math.sqrt(best) <= r * self.cell:
                break
        return math.sqrt(best), bi


# ------------------------------------------------------------ the deck
MLGI = """# h field for createpts / poisson_disk, from the DFN mo_dfn:
#   h = H_C * h_dfn(nearest DFN vertex)
#       + H_SLOPE * max(0, distance to the DFN - H_FLAT)
#   clipped to [H_MIN, H_MAX]
# Built in h_field_att itself with one work attribute (d_dfn, which
# compute / distance_field adds as permanent: DELATT removes it), to
# keep the lookup lattice small.
compute / distance_field / mo_poi_h_field / mo_dfn / d_dfn
interpolate / voronoi / mo_poi_h_field h_field_att / 1,0,0 / &
       mo_dfn h_dfn
math / multiply / mo_poi_h_field / h_field_att / 1,0,0 / &
       mo_poi_h_field / h_field_att / H_C
math / sub / mo_poi_h_field / d_dfn / 1,0,0 / &
       mo_poi_h_field / d_dfn / H_FLAT
math / floor / mo_poi_h_field / d_dfn / 1,0,0 / &
       mo_poi_h_field / d_dfn / 0.0
math / multiply / mo_poi_h_field / d_dfn / 1,0,0 / &
       mo_poi_h_field / d_dfn / H_SLOPE
math / add / mo_poi_h_field / h_field_att / 1,0,0 / &
       mo_poi_h_field / h_field_att / mo_poi_h_field / d_dfn
cmo / DELATT / mo_poi_h_field / d_dfn
math / floor / mo_poi_h_field / h_field_att / 1,0,0 / &
       mo_poi_h_field / h_field_att / H_MIN
math / ceiling / mo_poi_h_field / h_field_att / 1,0,0 / &
       mo_poi_h_field / h_field_att / H_MAX
cmo / printatt / mo_poi_h_field / h_field_att / minmax
{dump}finish
"""

DUMP_H = """# --keep-hfield: the lookup lattice with h_field_att (the h asked for)
dump / avs / h_field.inp / mo_poi_h_field
"""

DECK = """# {case}: 3D Poisson disk tets, resolution from the DFN
read / avs / dfn.inp / mo_dfn
cmo / addatt / mo_dfn / edge_lengths_{stat} / h_dfn
{href_dfn}cmo / printatt / mo_dfn / -xyz- / minmax
cmo / printatt / mo_dfn / h_dfn / minmax
define / POI_XMIN / {x0!r}
define / POI_YMIN / {y0!r}
define / POI_ZMIN / {z0!r}
define / POI_XMAX / {x1!r}
define / POI_YMAX / {y1!r}
define / POI_ZMAX / {z1!r}
define / IF_POI_EDGE_BUFFER / 0
define / POI_EDGE_BUFFER / 1.0
define / H_C / {c!r}
define / H_SLOPE / {slope!r}
define / H_FLAT / {hflat!r}
define / H_MIN / {hmin!r}
define / H_MAX / {hmax!r}
createpts / poisson_disk / 3d_box / mo_tet / {hscalar!r} / connect / &
      h_dfn.mlgi / poisson_seed {seed}{lookup}
{improve}quality
# for the page: dihedral angles of each tet (cell attributes only)
cmo / addatt / mo_tet / ang_mind / ang_min
cmo / addatt / mo_tet / ang_maxd / ang_max
dump / avs / angles_after.table / mo_tet / 0 0 0 1
cmo / delatt / mo_tet / ang_min
cmo / delatt / mo_tet / ang_max
# for the page: at each tet vertex, the mean length of its edges, the
# distance to the DFN, and h_dfn of the nearest DFN vertex
cmo / addatt / mo_tet / edge_lengths_mean / el_mean
compute / distance_field / mo_tet / mo_dfn / d_dfn
cmo / addatt / mo_tet / h_near / VDOUBLE / scalar / nnodes
interpolate / voronoi / mo_tet h_near / 1,0,0 / mo_dfn h_dfn
{href_tet}# only these columns in the table: leave the standard vertex
# attributes out of the AVS output, then put them back for the mesh
cmo / modatt / mo_tet / imt1 / ioflag / l
cmo / modatt / mo_tet / itp1 / ioflag / l
cmo / modatt / mo_tet / icr1 / ioflag / l
cmo / modatt / mo_tet / isn1 / ioflag / l
dump / avs / vertex_stats.table / mo_tet / 0 0 1 0
cmo / modatt / mo_tet / imt1 / ioflag / gal
cmo / modatt / mo_tet / itp1 / ioflag / gal
cmo / modatt / mo_tet / icr1 / ioflag / gal
cmo / modatt / mo_tet / isn1 / ioflag / gal
cmo / delatt / mo_tet / el_mean
cmo / DELATT / mo_tet / d_dfn
cmo / delatt / mo_tet / h_near
{href_del}# the mesh
dump / avs / tets.inp / mo_tet
finish
"""

IMPROVE = """# dihedral angles before improve_tets
cmo / addatt / mo_tet / ang_mind / ang_min
cmo / addatt / mo_tet / ang_maxd / ang_max
dump / avs / angles_before.table / mo_tet / 0 0 0 1
cmo / delatt / mo_tet / ang_min
cmo / delatt / mo_tet / ang_max
improve_tets / mo_tet
"""


# ------------------------------------------------------------ analysis
ANG_MIN_BINS = (0.0, 75.0, 2.5)      # lo, hi, width (degrees)
ANG_MAX_BINS = (60.0, 180.0, 5.0)
RATIO_BINS = (0.0, 3.0, 0.1)


def binner(lo, hi, w):
    n = int(round((hi - lo) / w))
    c = [0] * n
    def add(v):
        c[max(0, min(n - 1, int((v - lo) / w)))] += 1
    return c, add


def read_table(path):
    """an attribute-only AVS file from dump / avs / ... / 0 0 1 0 or
    0 0 0 1: yields (names, rows), rows one list of floats at a time"""
    f = open(path)
    head = f.readline().split()
    nn, nc, nva, nca = int(head[0]), int(head[1]), int(head[2]), int(head[3])
    n_att = nva or nca
    f.readline()                                  # the component sizes
    names = [f.readline().split(",")[0].strip() for _ in range(n_att)]
    def rows():
        for line in f:
            w = line.split()
            if len(w) == n_att + 1:
                yield [float(x) for x in w[1:]]
        f.close()
    return names, rows()


def read_angles(path):
    names, rows = read_table(path)
    i_lo, i_hi = names.index("ang_min"), names.index("ang_max")
    cmin, add_min = binner(*ANG_MIN_BINS)
    cmax, add_max = binner(*ANG_MAX_BINS)
    n, lo, hi = 0, float("inf"), 0.0
    for r in rows:
        n += 1
        add_min(r[i_lo])
        add_max(r[i_hi])
        lo, hi = min(lo, r[i_lo]), max(hi, r[i_hi])
    return {"ntet": n, "min": lo, "max": hi, "cmin": cmin, "cmax": cmax}


def analyse(od, P, a):
    """everything per tet or per vertex comes from LaGriT (the dumps of
    the deck); here only binning, one line at a time"""
    out = {}
    t0 = time.time()
    for tag in ("before", "after"):
        p = os.path.join(od, "angles_%s.table" % tag)
        if os.path.isfile(p):
            out[tag] = read_angles(p)
    p = os.path.join(od, "vertex_stats.table")
    if "after" in out and os.path.isfile(p):
        names, rows = read_table(p)
        ie, idd, ih = names.index("el_mean"), names.index("d_dfn"), names.index("h_near")
        ir = names.index("h_ref") if "h_ref" in names else ih
        from array import array
        ratios, prof, nv, eh = [], {}, 0, array("d")
        n_cap, d_far = 0, 0.0
        binw = P["hmin"]
        for r in rows:
            nv += 1
            el, d, hn, hr = r[ie], r[idd], r[ih], r[ir]
            if el <= 0:
                continue
            hraw = a.c * hn + a.slope * max(0.0, d - a.flat * P["hmin"])
            h_here = min(P["hmax"], max(P["hmin"], hraw))
            n_cap += hraw >= P["hmax"]
            d_far = max(d_far, d)
            if d <= h_here and hr > 0:
                ratios.append(el / hr)
            eh.append(el / h_here)
            b = int(d / binw)
            s_ = prof.setdefault(b, [0.0, 0, 0.0])
            s_[0] += el
            s_[1] += 1
            s_[2] += h_here
        ratios.sort()
        eh = sorted(eh)
        # prof: (distance, mean tet edge, vertices, mean h asked for at those vertices)
        out["after"].update({"nv": nv, "ratios": ratios, "eh_median": q(eh, .5),
                             "frac_hmax": n_cap / max(1, len(eh)), "d_far": d_far,
                             "eh_p05": q(eh, .05), "eh_p95": q(eh, .95),
                             "prof": sorted((b * binw + binw / 2, v[0] / v[1], v[1], v[2] / v[1])
                                            for b, v in prof.items())})
    say("  analysed (LaGriT's tables): %.0f s" % (time.time() - t0))
    return out


def mem(nbytes):
    return ("%.1f GB" % (nbytes / 1e9)) if nbytes >= 1e9 else ("%.0f MB" % (nbytes / 1e6))


def say(msg):
    print(msg, flush=True)


def q(s, f):
    return s[min(len(s) - 1, int(f * (len(s) - 1)))] if s else float("nan")


# ------------------------------------------------------------ pictures
def hist_svg(title, before, after, lo, hi, wbin, xlabel, step=None, labels=("before improve_tets", "after"),
             unit="tets"):
    """before, after: counts per bin (after may be None)"""
    nb = int(round((hi - lo) / wbin))
    cb, ca = before, after if after is not None else [0] * nb
    W, H, L, R, T, B = 520, 236, 52, 12, 46, 40
    pw, ph = W - L - R, H - T - B
    ymax = 10 ** math.ceil(math.log10(max(max(cb), max(ca), 1) + 1))
    y = lambda c: T + ph - ph * math.log10(1 + c) / math.log10(1 + ymax)
    o = ['<svg viewBox="0 0 %d %d" width="%d" role="img" aria-label="%s">' % (W, H, W, E(title)),
         '<text x="%d" y="16" font-size="13" fill="%s">%s</text>' % (L, INK, E(title))]
    d = 1
    while d <= ymax:
        o.append('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="%s"/><text x="%d" y="%.1f" font-size="10" '
                 'fill="%s" text-anchor="end" dominant-baseline="middle">%s</text>'
                 % (L, L + pw, y(d), y(d), GRID, L - 6, y(d), INK2, "{:,}".format(d)))
        d *= 10
    bw = pw / nb
    for k in range(nb):
        for j, (c, col, lab) in enumerate(((cb[k], BLUE, "before"), (ca[k], ORANGE, "after"))):
            if c == 0 or (after is None and j):
                continue
            w = (bw - 2) / 2
            o.append('<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" fill="%s" rx="1"><title>%s: %s %s, '
                     '%g to %g</title></rect>' % (L + k * bw + 1 + j * w, y(c), max(w - 0.5, 0.6),
                                                       T + ph - y(c), col, lab, "{:,}".format(c), unit,
                                                       lo + k * wbin, lo + (k + 1) * wbin))
    o.append('<line x1="%d" x2="%d" y1="%d" y2="%d" stroke="%s"/>' % (L, L + pw, T + ph, T + ph, INK2))
    step = step or (10 if hi - lo <= 90 else 20)
    v = math.ceil(lo / step - 1e-9) * step
    while v <= hi + 1e-9:
        xx = L + pw * (v - lo) / (hi - lo)
        o.append('<text x="%.1f" y="%d" font-size="10" fill="%s" text-anchor="middle">%g</text>' % (xx, T + ph + 15, INK2, v))
        v += step
    o.append('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">%s</text>' % (L + pw / 2, H - 6, INK2, E(xlabel)))
    for j, (col, lab) in enumerate(((BLUE, labels[0]), (ORANGE, labels[1]))):
        if after is None and j:
            continue
        o.append('<rect x="%d" y="24" width="10" height="10" rx="2" fill="%s"/><text x="%d" y="33" font-size="11" '
                 'fill="%s">%s</text>' % (L + j * 170, col, L + 14 + j * 170, INK, lab))
    o.append("</svg>")
    return "\n".join(o)


def profile_svg(prof, P, a, K):
    """mean tet edge length and the mean h asked for, against distance to
    the DFN, over the same tet vertices; and K times h, K = median of
    (mean tet edge / h asked for) over all tet vertices"""
    W, H, L, R, T, B = 520, 250, 56, 14, 30, 42
    pw, ph = W - L - R, H - T - B
    prof = [p for p in prof if p[2] >= 5]
    xmax = max([p[0] for p in prof] or [1])
    ymax = max([p[1] for p in prof] + [K * p[3] for p in prof] + [1e-9]) * 1.08
    X = lambda v: L + pw * v / xmax
    Y = lambda v: T + ph - ph * v / ymax
    o = ['<svg viewBox="0 0 %d %d" width="%d" role="img" aria-label="grading">' % (W, H, W),
         '<text x="%d" y="16" font-size="13" fill="%s">Tet edge length and the h asked for, against distance to the DFN</text>'
         % (L, INK)]
    for k in range(5):
        v = ymax * k / 4
        o.append('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="%s"/><text x="%d" y="%.1f" font-size="10" fill="%s" '
                 'text-anchor="end" dominant-baseline="middle">%.3g</text>' % (L, L + pw, Y(v), Y(v), GRID, L - 6, Y(v), INK2, v))
    for k in range(5):
        v = xmax * k / 4
        o.append('<text x="%.1f" y="%d" font-size="10" fill="%s" text-anchor="middle">%.3g</text>' % (X(v), T + ph + 15, INK2, v))
    line = lambda pts, col, extra: o.append('<polyline fill="none" stroke="%s" %s points="%s"/>' % (
        col, extra, " ".join("%.1f,%.1f" % (X(x), Y(y)) for x, y in pts)))
    line([(d, h) for d, m, n, h in prof], INK2, 'stroke-width="1.5" stroke-dasharray="5 4"')
    line([(d, K * h) for d, m, n, h in prof], ORANGE, 'stroke-width="1.5" stroke-dasharray="2 3"')
    line([(d, m) for d, m, n, h in prof], BLUE, 'stroke-width="2"')
    for d, m, n, h in prof:
        o.append('<circle cx="%.1f" cy="%.1f" r="3" fill="%s"><title>d %.3g: mean edge %.3g, h asked %.3g, edge/h %.2f '
                 '(%d vertices)</title></circle>' % (X(d), Y(m), BLUE, d, m, h, m / h, n))
    o.append('<line x1="%d" x2="%d" y1="%d" y2="%d" stroke="%s"/>' % (L, L + pw, T + ph, T + ph, INK2))
    o.append('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">distance from the DFN (to its nearest vertex)</text>'
             % (L + pw / 2, H - 6, INK2))
    for k, (col, dash, lab) in enumerate(((BLUE, "", "mean tet edge at the vertices"),
                                          (ORANGE, 'stroke-dasharray="2 3"', "%.2f &times; h asked for (median edge / h)" % K),
                                          (INK2, 'stroke-dasharray="5 4"', "h asked for at the same vertices"))):
        y0 = 40 + 16 * k
        o.append('<line x1="%d" x2="%d" y1="%d" y2="%d" stroke="%s" stroke-width="2" %s/><text x="%d" y="%d" font-size="11" '
                 'fill="%s">%s</text>' % (L + 10, L + 34, y0, y0, col, dash, L + 40, y0 + 4, INK, lab))
    o.append("</svg>")
    return "\n".join(o)


def ratio_profile_svg(prof, K):
    """mean tet edge / h asked for, against distance to the DFN"""
    W, H, L, R, T, B = 520, 210, 56, 14, 30, 42
    pw, ph = W - L - R, H - T - B
    prof = [p for p in prof if p[2] >= 5]
    xmax = max([p[0] for p in prof] or [1])
    rs = [m / h for d, m, n, h in prof]
    lo, hi = min(rs + [1.0]) * 0.95, max(rs + [2.0]) * 1.03
    X = lambda v: L + pw * v / xmax
    Y = lambda v: T + ph - ph * (v - lo) / (hi - lo)
    o = ['<svg viewBox="0 0 %d %d" width="%d" role="img" aria-label="edge over h">' % (W, H, W),
         '<text x="%d" y="16" font-size="13" fill="%s">Tet edge / h asked for, against distance to the DFN</text>' % (L, INK)]
    for k in range(5):
        v = lo + (hi - lo) * k / 4
        o.append('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="%s"/><text x="%d" y="%.1f" font-size="10" fill="%s" '
                 'text-anchor="end" dominant-baseline="middle">%.2f</text>' % (L, L + pw, Y(v), Y(v), GRID, L - 6, Y(v), INK2, v))
    for k in range(5):
        v = xmax * k / 4
        o.append('<text x="%.1f" y="%d" font-size="10" fill="%s" text-anchor="middle">%.3g</text>' % (X(v), T + ph + 15, INK2, v))
    o.append('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="%s" stroke-dasharray="2 3" stroke-width="1.5"/>'
             % (L, L + pw, Y(K), Y(K), ORANGE))
    o.append('<text x="%d" y="%.1f" font-size="10" fill="%s" text-anchor="end">median %.2f</text>' % (L + pw - 4, Y(K) - 5, ORANGE, K))
    o.append('<polyline fill="none" stroke="%s" stroke-width="2" points="%s"/>' % (
        BLUE, " ".join("%.1f,%.1f" % (X(d), Y(m / h)) for d, m, n, h in prof)))
    for d, m, n, h in prof:
        o.append('<circle cx="%.1f" cy="%.1f" r="3" fill="%s"><title>d %.3g: edge/h %.2f (%d vertices)</title></circle>'
                 % (X(d), Y(m / h), BLUE, d, m / h, n))
    o.append('<line x1="%d" x2="%d" y1="%d" y2="%d" stroke="%s"/>' % (L, L + pw, T + ph, T + ph, INK2))
    o.append('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">distance from the DFN (to its nearest vertex)</text>'
             % (L + pw / 2, H - 6, INK2))
    o.append("</svg>")
    return "\n".join(o)


RAMP = ["#0d366b", "#104281", "#184f95", "#256abf", "#3987e5", "#5598e7", "#86b6ef", "#b7d3f6"]


def plane_cut(P, z0):
    """points where the edges of a cell (all vertex pairs) cross z = z0"""
    pts = []
    n = len(P)
    for i in range(n):
        for j in range(i + 1, n):
            a, b = P[i], P[j]
            if (a[2] - z0) * (b[2] - z0) < 0:
                t = (z0 - a[2]) / (b[2] - a[2])
                pts.append((a[0] + t * (b[0] - a[0]), a[1] + t * (b[1] - a[1])))
    return pts


def slice_svg(tet_avs, P, z0=0.0):
    """the tets cut by the plane z = z0, coloured by their mean edge
    length (darker = finer), with the DFN's traces in that plane"""
    from array import array
    (x0, x1), (y0, y1) = P["box"][0], P["box"][1]
    W = 470
    sc = (W - 70) / max(x1 - x0, y1 - y0)
    H = int((y1 - y0) * sc) + 92
    X = lambda x: 50 + (x - x0) * sc
    Y = lambda y: H - 58 - (y - y0) * sc
    polys = []
    with open(tet_avs) as f:
        head = f.readline().split()
        nn, nc = int(head[0]), int(head[1])
        X3 = array("d")
        for _ in range(nn):
            w = f.readline().split()
            X3.extend((float(w[1]), float(w[2]), float(w[3])))
        cells = []
        for _ in range(nc):
            w = f.readline().split()
            if w[2].lower() != "tet":
                continue
            ids = [int(v) - 1 for v in w[3:7]]
            zs = [X3[3 * i + 2] for i in ids]
            if min(zs) < z0 < max(zs):
                cells.append(ids)
    for ids in cells:
        Q = [(X3[3 * i], X3[3 * i + 1], X3[3 * i + 2]) for i in ids]
        pts = plane_cut(Q, z0)
        if len(pts) < 3:
            continue
        cx = sum(p[0] for p in pts) / len(pts)
        cy = sum(p[1] for p in pts) / len(pts)
        pts.sort(key=lambda p: math.atan2(p[1] - cy, p[0] - cx))
        el = sum(math.dist(Q[i], Q[j]) for i in range(4) for j in range(i + 1, 4)) / 6
        polys.append((el, pts))
    if not polys:
        return ""
    lo, hi = P["hmin"] * 1.2, P["hmax"] * 1.7
    def col(el):
        u = (math.log(el) - math.log(lo)) / (math.log(hi) - math.log(lo))
        return RAMP[max(0, min(len(RAMP) - 1, int(u * len(RAMP))))]
    o = ['<svg viewBox="0 0 %d %d" width="%d" role="img" aria-label="slice">' % (W, H, W),
         '<text x="50" y="16" font-size="13" fill="%s">Slice z = %g: tets coloured by mean edge length</text>' % (INK, z0)]
    for el, pts in polys:
        o.append('<polygon points="%s" fill="%s" stroke="#ffffff" stroke-width="0.3"/>'
                 % (" ".join("%.1f,%.1f" % (X(p[0]), Y(p[1])) for p in pts), col(el)))
    cid = "clip%d" % (abs(hash((x0, y0, x1, y1, z0, len(P["C"])))) % 10 ** 8)
    o.append('<clipPath id="%s"><rect x="%.1f" y="%.1f" width="%.1f" height="%.1f"/></clipPath><g clip-path="url(#%s)">'
             % (cid, X(x0), Y(y1), (x1 - x0) * sc, (y1 - y0) * sc, cid))
    for t, ids in P["C"]:
        Q = [P["V"][i] for i in ids]
        if max(q[0] for q in Q) < x0 or min(q[0] for q in Q) > x1 or max(q[1] for q in Q) < y0 \
                or min(q[1] for q in Q) > y1:
            continue
        pts = plane_cut(Q, z0)
        if len(pts) == 2:
            o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="2"/>'
                     % (X(pts[0][0]), Y(pts[0][1]), X(pts[1][0]), Y(pts[1][1]), ORANGE))
    o.append("</g>")
    o.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="none" stroke="%s"/>'
             % (X(x0), Y(y1), (x1 - x0) * sc, (y1 - y0) * sc, INK2))
    # legend: ramp, fine to coarse, and the DFN trace
    lx, ly = 50, H - 40
    for k, c in enumerate(RAMP):
        o.append('<rect x="%d" y="%d" width="18" height="10" fill="%s"/>' % (lx + k * 18, ly, c))
    o.append('<text x="%d" y="%d" font-size="10" fill="%s">%.3g</text>' % (lx, ly + 21, INK2, lo))
    o.append('<text x="%d" y="%d" font-size="10" fill="%s" text-anchor="end">%.3g</text>'
             % (lx + 18 * len(RAMP), ly + 21, INK2, hi))
    o.append('<text x="%d" y="%d" font-size="10" fill="%s">mean tet edge (darker = finer)</text>'
             % (lx + 18 * len(RAMP) + 8, ly + 9, INK2))
    o.append('<line x1="%d" x2="%d" y1="%d" y2="%d" stroke="%s" stroke-width="2"/><text x="%d" y="%d" font-size="10" '
             'fill="%s">where the fractures (DFN) cross the slice</text>'
             % (lx, lx + 20, ly + 33, ly + 33, ORANGE, lx + 26, ly + 37, INK2))
    o.append("</svg>")
    return "\n".join(o)


def report_lines(text):
    out, on = [], False
    for l in text.splitlines():
        if l.startswith("improve_tets:"):
            on = True
        if not on:
            continue
        if "Enter a command" in l or l.strip() == "quality":
            break
        if "% done" not in l:
            out.append(l.rstrip())
    return out[:120]



# ------------------------------------------------------------ plan
def plan(S):
    """read the DFN, size the h field and the run"""
    a = S["_ns"]
    if S["box"]:
        b = S["box"]
        box = [(b[0], b[3]), (b[1], b[4]), (b[2], b[5])]
        meta = None
    else:
        meta = json.load(open(S["json"]))
        if "domain_size" not in meta:
            sys.exit("%s has no domain_size" % S["json"])
        Ls = [float(x) for x in meta["domain_size"]]
        box = [(-L / 2.0, L / 2.0) for L in Ls]
    Ls = [hi - lo for lo, hi in box]
    t0 = time.time()
    V, C, types, _ = read_avs(S["dfn"])
    say("  read DFN: %s vertices, %s cells, %.0f s" % ("{:,}".format(len(V)), "{:,}".format(len(C)), time.time() - t0))
    if not V or not C:
        sys.exit("the DFN %s has no vertices or no cells" % S["dfn"])
    bb = [(min(p[k] for p in V), max(p[k] for p in V)) for k in range(3)]
    # the DFN statistic, as edge_lengths_<stat> will compute it
    nb = [[] for _ in V]
    for i, j in unique_edges(C):
        d = math.dist(V[i], V[j])
        nb[i].append(d)
        nb[j].append(d)
    hd = [(min(d) if a.stat == "min" else sum(d) / len(d)) if d else 0.0 for d in nb]
    hd_ref = [sum(d) / len(d) if d else 0.0 for d in nb]
    hpos = sorted(x for x in hd if x > 0)
    hmin = a.c * hpos[0]
    hmax = max(min(a.hmax_factor * hmin, min(Ls) / a.min_cells), hmin)
    # A box smaller than the DFN: only DFN vertices within reach of the box
    # matter (beyond (H_MAX - H_MIN) / SLOPE from it, h is H_MAX anyway), so
    # H_MIN comes from those, and the planning looks only at them. LaGriT
    # still gets the whole DFN.
    reach = (hmax - hmin) / a.slope + hmax + a.flat * hmin
    local = [i for i, p in enumerate(V)
             if all(box[k][0] - reach <= p[k] <= box[k][1] + reach for k in range(3))]
    if not local:
        sys.exit("no DFN vertex within %.4g of the box: the h field would be H_MAX everywhere" % reach)
    if len(local) < len(V):
        hloc = sorted(hd[i] for i in local if hd[i] > 0)
        hmin = a.c * hloc[0]
        hmax = max(min(a.hmax_factor * hmin, min(Ls) / a.min_cells), hmin)
    hmax_by = "hmax_factor" if a.hmax_factor * hmin <= min(Ls) / a.min_cells else "min_cells"
    hscalar = a.hscalar_factor * hmin
    lattice = 1
    for L in Ls:
        lattice *= int((L + 2 * hscalar) / hscalar * 1.5) + 1
    # vertex estimate: integral of 0.7 / h^3 over the box (nearest DFN vertex)
    hsort = sorted(hd_ref[i] for i in local if hd_ref[i] > 0)
    Vl = [V[i] for i in local]
    near = Nearest(Vl, 2.0 * hsort[len(hsort) // 2], [b[0] for b in box], [b[1] for b in box])
    n = 24
    est = 0.0
    h_reach = 0.0
    for ix in range(n):
        for iy in range(n):
            for iz in range(n):
                p = tuple(box[k][0] + (q_ + 0.5) * Ls[k] / n for k, q_ in enumerate((ix, iy, iz)))
                d, iv = near.query(p)
                h = min(hmax, max(hmin, a.c * hd[local[iv]] + a.slope * max(0.0, d - a.flat * hmin)))
                h_reach = max(h_reach, h)
                est += 0.7 / h ** 3
    est *= Ls[0] * Ls[1] * Ls[2] / n ** 3
    notes = []
    if set(types) != {"tri"}:
        notes.append("the DFN has cells other than triangles: %s" % types)
    outside = [("xyz"[k]) for k, (lo, hi) in enumerate(box)
               if bb[k][0] < lo - 1e-9 * (hi - lo) or bb[k][1] > hi + 1e-9 * (hi - lo)]
    if outside and not S["box"]:
        notes.append("the DFN reaches outside the box in %s" % " ".join(outside))
    info = []
    if outside and S["box"]:
        info.append("The DFN reaches beyond this box (in %s); all of it shapes h, and the planning used the %s "
                    "DFN vertices within %.3g of the box." % (" ".join(outside), "{:,}".format(len(local)), reach))
    return {"box": box, "Ls": Ls, "bbox": bb, "types": types, "nv_dfn": len(V), "ntri": len(C),
            "hd": hd, "hd_ref": hd_ref, "hmin": hmin, "hscalar": hscalar, "hmax": hmax, "lattice": lattice,
            "est": est, "h_reach": h_reach,
            "mem_pred": (BYTES_PER_LATTICE_POINT * lattice if a.h_lookup != "octree" else
                         OCT_BYTES_PER_EST_VERTEX * est) + BYTES_PER_DFN_VERTEX * len(V),
            "time_pred": (SECONDS_PER_LATTICE_POINT * lattice if a.h_lookup != "octree" else 0.0)
                         + (OCT_SECONDS_PER_EST_VERTEX
                            if a.h_lookup == "octree"
                            else SECONDS_PER_EST_VERTEX) * est,
            "hmax_by": hmax_by, "V": V, "C": C, "notes": notes, "info": info, "plan_s": time.time() - t0}


def hms(s):
    s = int(round(s))
    return "%d s" % s if s < 120 else ("%d min" % round(s / 60) if s < 7200 else "%.1f h" % (s / 3600))


# ------------------------------------------------------------ run
def run(S, P):
    a, od = S["_ns"], S["out"]
    os.makedirs(od, exist_ok=True)
    for f in ("run.lgi", "h_dfn.mlgi", "run.out", "tets.inp", "angles_before.table", "angles_after.table",
              "vertex_stats.table", "h_field.inp", "octree_leaves.inp", "dfn.inp", "result.json", "section.html",
              "summary.html"):
        p = os.path.join(od, f)
        if os.path.isfile(p) or os.path.islink(p):
            os.remove(p)
    src = os.path.abspath(S["dfn"])
    try:
        os.symlink(src, os.path.join(od, "dfn.inp"))
    except OSError:
        shutil.copyfile(src, os.path.join(od, "dfn.inp"))
    # with h_lookup octree the .mlgi runs on batches of octree vertices,
    # not on a lattice: keep_hfield does nothing (octree_dump writes the
    # octree instead)
    dump_h = a.keep_hfield and a.h_lookup != "octree"
    open(os.path.join(od, "h_dfn.mlgi"), "w").write(MLGI.format(dump=DUMP_H if dump_h else ""))
    ref = a.stat != "mean"
    (x0, x1), (y0, y1), (z0, z1) = P["box"]
    deck = DECK.format(case=S["name"], stat=a.stat, x0=x0, y0=y0, z0=z0, x1=x1, y1=y1, z1=z1,
                       c=a.c, slope=a.slope, hmin=round(P["hmin"], 10), hmax=round(P["hmax"], 10),
                       hflat=round(a.flat * P["hmin"], 10),
                       hscalar=round(P["hscalar"], 10),
                       seed=a.seed, improve=IMPROVE if a.improve else "# improve_tets skipped\n",
                       href_dfn="cmo / addatt / mo_dfn / edge_lengths_mean / h_ref\n" if ref else "",
                       href_tet=("cmo / addatt / mo_tet / h_ref / VDOUBLE / scalar / nnodes\n"
                                 "interpolate / voronoi / mo_tet h_ref / 1,0,0 / mo_dfn h_ref\n") if ref else "",
                       href_del="cmo / delatt / mo_tet / h_ref\n" if ref else "",
                       # h_lookup always given: without it, LaGriT V3.340 uses the
                       # octree for a control file such as h_dfn.mlgi
                       lookup=(" / &\n      h_lookup hex_grid" if a.h_lookup != "octree" else
                               " / &\n      h_lookup octree / octree_k_factor %r" % a.octree_k_factor +
                               (" / &\n      octree_dump octree_leaves.inp" if a.octree_dump else "")))
    open(os.path.join(od, "run.lgi"), "w").write(deck)
    t0 = time.time()
    with open(os.path.join(od, "run.lgi")) as fin, open(os.path.join(od, "run.out"), "w") as fout:
        try:
            pr = subprocess.Popen([S["lagrit"]], stdin=fin, stdout=fout, stderr=subprocess.STDOUT, cwd=od)
        except OSError as e:
            sys.exit("cannot run LaGriT %s: %s" % (S["lagrit"], e))
        # wait4: this LaGriT's own peak memory (kB on Linux, bytes on macOS)
        _, status, ru = os.wait4(pr.pid, 0)
        rc = os.waitstatus_to_exitcode(status) if hasattr(os, "waitstatus_to_exitcode") else status >> 8
    peak = ru.ru_maxrss * (1 if sys.platform == "darwin" else 1024)
    return rc, time.time() - t0, peak


# ------------------------------------------------------------ page
CSS = """<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1150px;color:#1f2328;background:#fff}
h1{font-size:20px}h2{font-size:17px;margin:28px 0 6px}p{font-size:14px;line-height:1.45;max-width:1000px}
table{border-collapse:collapse;font-size:12px;margin:6px 0}td,th{border-bottom:1px solid #d0d7de;padding:3px 7px;text-align:right;white-space:nowrap}
th{white-space:normal;vertical-align:bottom;background:#f6f8fa}.scroll{overflow-x:auto}
td:first-child,th:first-child{text-align:left}.bad{color:#cf222e;font-weight:600;white-space:normal;text-align:left}
.row{display:flex;flex-wrap:wrap;gap:12px}pre{font-size:11.5px;background:#f6f8fa;padding:8px;white-space:pre-wrap}
.set td{text-align:left;white-space:normal}.set td:first-child{font-family:ui-monospace,Menlo,monospace}</style>"""

INTRO = ("<p>h = C &times; h_dfn(nearest DFN vertex) + SLOPE &times; max(0, distance to the DFN &minus; FLAT &times; "
         "H_MIN), clipped to [H_MIN, H_MAX]; "
         "h_dfn = edge_lengths_mean (or _min) of the DFN. Every number below is computed by LaGriT on every tet and "
         "tet vertex (ang_mind, ang_maxd, edge_lengths_mean, compute / distance_field, interpolate / voronoi); the "
         "page only bins them. <b>Near-DFN ratio</b>: at each tet vertex within one local h of the DFN, the mean "
         "length of its edges divided by the DFN's mean edge length at the nearest DFN vertex (1 = the DFN's "
         "resolution), whichever statistic sets h. <b>Tet edge / h asked</b>: at every tet vertex, the mean length of its "
         "edges divided by the h asked for there; poisson_disk's h is the smallest spacing it allows, so this is "
         "about 1.4&ndash;1.7, and C &lt; 1 makes up for it.</p>")


def settings_table(R):
    o = ["<details><summary>Settings</summary><table class='set'>"]
    for n, d, t, h, g in SETTINGS:
        if n == "out":
            continue
        v = R["settings"].get(n)
        o.append("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>" % (
            E(n), E(" ".join("%g" % x for x in v) if isinstance(v, list) else str(v)),
            E(R["source"].get(n, "")), E(h)))
    o.append("</table></details>")
    return "".join(o)


def table_html(results):
    o = ['<div class="scroll"><table><tr><th>case</th><th>DFN vertices / triangles</th><th>H_MIN / H_MAX</th>'
         '<th>tet vertices at H_MAX</th>'
         '<th>lattice</th><th>tet vertices: estimate / made</th><th>tets</th>'
         '<th>near-DFN ratio: median (5&ndash;95%)</th><th>tet edge / h asked: median (5&ndash;95%)</th>'
         '<th>smallest angle before &rarr; after</th>'
         '<th>largest angle before &rarr; after</th><th>LaGriT time: predicted / took</th>'
         '<th>LaGriT memory: predicted / peak</th></tr>']
    for R in results:
        P, X = R["plan"], R.get("res") or {}
        dash = "-"
        o.append("<tr><td><a href='#%s'>%s</a></td><td>%s / %s</td><td>%.4g / %.4g%s</td><td>%s</td><td>%s</td><td>%s / %s</td>"
                 "<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s / %s</td><td>%s / %s</td></tr>" % (
                     E(R["name"]), E(R["name"]), "{:,}".format(P["nv_dfn"]), "{:,}".format(P["ntri"]),
                     P["hmin"], P["hmax"], " (box / min_cells)" if P.get("hmax_by") == "min_cells" else "",
                     ("%.1f%%" % (100 * X["frac_hmax"])) if "frac_hmax" in X else dash,
                     "octree" if R["settings"].get("h_lookup") == "octree" else "{:,}".format(P["lattice"]),
                     "{:,}".format(int(P["est"])),
                     "{:,}".format(X["nv"]) if X.get("nv") else dash,
                     ("{:,} &rarr; {:,}".format(X["ntet_before"], X["ntet"]) if X.get("ntet_before")
                      else "{:,}".format(X["ntet"]) if X.get("ntet") else dash),
                     ("%.2f (%.2f&ndash;%.2f)" % (X["ratio_median"], X["ratio_p05"], X["ratio_p95"]))
                     if X.get("ratio_median") else dash,
                     ("%.2f (%.2f&ndash;%.2f)" % (X["eh_median"], X["eh_p05"], X["eh_p95"]))
                     if X.get("eh_median") else dash,
                     ("%.1f&deg; &rarr; %.1f&deg;" % (X["min_before"], X["min_after"])) if "min_before" in X
                     else ("%.1f&deg;" % X["min_after"]) if "min_after" in X else dash,
                     ("%.1f&deg; &rarr; %.1f&deg;" % (X["max_before"], X["max_after"])) if "max_before" in X
                     else ("%.1f&deg;" % X["max_after"]) if "max_after" in X else dash,
                     hms(P["time_pred"]), hms(R["time"]) if R.get("time") else dash,
                     mem(P["mem_pred"]), mem(R["peak"]) if R.get("peak") else dash))
        if R["notes"]:
            o.append("<tr><td></td><td colspan='12' class='bad'>%s</td></tr>" % E("; ".join(R["notes"])))
    o.append("</table></div>")
    return "\n".join(o)


def section_html(S, P, X, R):
    a = S["_ns"]
    o = ["<h2 id='%s'>%s</h2>" % (E(S["name"]), E(S["name"]))]
    hd = sorted(x for x in P["hd"] if x > 0)
    o.append("<p>DFN <code>%s</code>: %s vertices, %s triangles; h_dfn (%s): %.4g to %.4g, median %.4g. "
             "Box %s (DFN bounding box %s). h field: H_MIN %.4g, H_MAX %.4g; h_spacing_scalar %.4g; %s.%s</p>" % (
                 E(os.path.abspath(S["dfn"])), "{:,}".format(P["nv_dfn"]), "{:,}".format(P["ntri"]), E(a.stat),
                 hd[0], hd[-1], q(hd, .5),
                 " &times; ".join("[%.4g, %.4g]" % bb for bb in P["box"]),
                 " &times; ".join("[%.4g, %.4g]" % bb for bb in P["bbox"]),
                 P["hmin"], P["hmax"], P["hscalar"],
                 "h lookup octree" if S["h_lookup"] == "octree" else
                 "lookup lattice %s points" % "{:,}".format(P["lattice"]),
                 (" Mesh: <code>%s</code>." % E(os.path.join(S["out"], "tets.inp")))
                 if os.path.isfile(os.path.join(S["out"], "tets.inp")) else ""))
    for n in P.get("info", []):
        o.append("<p>%s</p>" % E(n))
    for n in R["notes"]:
        o.append("<p class='bad'>%s</p>" % E(n))
    o.append(settings_table(R))
    f, b = (X or {}).get("after"), (X or {}).get("before")
    if f:
        figs = []
        tet_file = os.path.join(S["out"], "tets.inp")
        if os.path.isfile(tet_file):
            # a little off the middle: DFN vertices often lie exactly on z = 0
            zc = 0.5 * (P["box"][2][0] + P["box"][2][1]) + 0.0137 * (P["box"][2][1] - P["box"][2][0])
            figs.append(slice_svg(tet_file, P, round(zc, 4)))
        if f.get("prof"):
            figs.append(profile_svg(f["prof"], P, a, f["eh_median"]))
            figs.append(ratio_profile_svg(f["prof"], f["eh_median"]))
        if f.get("ratios"):
            cr, add = binner(*RATIO_BINS)
            for v in f["ratios"]:
                add(v)
            figs.append(hist_svg("Near-DFN tet edge length / DFN mean edge length", cr, None, *RATIO_BINS,
                                 "ratio (1 = same as the DFN)", step=0.5,
                                 labels=("tet vertices within one h of the DFN", ""), unit="vertices"))
        labs = ("before improve_tets", "after") if b else ("tets (no improve_tets)", "")
        figs.append(hist_svg("Smallest dihedral angle of each tet", (b or f)["cmin"], f["cmin"] if b else None,
                             *ANG_MIN_BINS, "degrees", labels=labs))
        figs.append(hist_svg("Largest dihedral angle of each tet", (b or f)["cmax"], f["cmax"] if b else None,
                             *ANG_MAX_BINS, "degrees", labels=labs))
        o.append("<div class='row'>%s</div>" % "".join(figs))
    if R.get("report"):
        o.append("<details><summary>improve_tets report</summary><pre>%s</pre></details>" % E("\n".join(R["report"])))
    return "\n".join(o)


def page_html(title, intro, results, sections):
    return ("<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' "
            "content='width=device-width, initial-scale=1'><title>%s</title>%s</head><body><h1>%s</h1>%s%s%s"
            "</body></html>\n" % (E(title), CSS, E(title), intro, table_html(results), "\n".join(sections)))


# ------------------------------------------------------------ main
def main(argv=None):
    S = get_settings(argv)
    S["_ns"] = argparse.Namespace(**{k: v for k, v in S.items() if not k.startswith("_")})
    say("%s\n  DFN    %s\n  box    %s\n  out    %s\n  LaGriT %s" % (
        S["name"], os.path.abspath(S["dfn"]),
        os.path.abspath(S["json"]) if S["json"] else " ".join("%g" % x for x in S["box"]), S["out"], S["lagrit"]))
    P = plan(S)
    say("  plan (%.0f s): h %.4g..%.4g, h_spacing_scalar %.4g; %s; about %s tet vertices"
        % (P["plan_s"], P["hmin"], P["hmax"], P["hscalar"],
           "h lookup octree" if S["h_lookup"] == "octree" else "lookup lattice " + "{:,}".format(P["lattice"]),
           "{:,}".format(int(P["est"]))))
    if P["h_reach"] > 12 * P["hscalar"] and S["h_lookup"] != "octree":
        say("  NOTE: h reaches about %.3g, %.0f x h_spacing_scalar. poisson_disk's neighbour grid is spaced by\n"
            "        h_spacing_scalar, so its sampling slows as (largest h / h_spacing_scalar)^3: measured 9x slower\n"
            "        at 26x than at 5x (constant_nonuniform). The time below does not include that.\n"
            "        --h-lookup octree does not slow down (with a LaGriT that has it)."
            % (P["h_reach"], P["h_reach"] / P["hscalar"]))
    if P["hmax_by"] == "min_cells":
        say("  NOTE: H_MAX %.4g is the smallest box size / min_cells (%g), not hmax_factor x H_MIN (%.4g)"
            % (P["hmax"], S["min_cells"], S["hmax_factor"] * P["hmin"]))
    say("  predicted: LaGriT memory about %s (limit %.1f GB), time about %s on a 2.6 GHz i7"
        % (mem(P["mem_pred"]), S["max_memory_gb"], hms(P["time_pred"])))
    R = {"name": S["name"], "settings": settings_for_page(S), "source": S["_source"], "notes": list(P["notes"]),
         "plan": {k: P[k] for k in ("box", "Ls", "bbox", "nv_dfn", "ntri", "hmin", "hscalar", "hmax", "lattice",
                                    "est", "mem_pred", "time_pred", "hmax_by", "h_reach")},
         "status": None, "time": None, "peak": None, "res": None, "out": S["out"]}
    for n in P["notes"]:
        say("  NOTE: %s" % n)
    os.makedirs(S["out"], exist_ok=True)
    X = None
    rc_out = 0
    if S["plan_only"]:
        R["status"] = "plan only"
        R["notes"].append("plan only: LaGriT not run")
        if P["mem_pred"] > S["max_memory_gb"] * 1e9:
            R["notes"].append("would not run: predicted LaGriT memory %s is over max_memory_gb %.1f GB"
                              % (mem(P["mem_pred"]), S["max_memory_gb"]))
            say("  WOULD NOT RUN: predicted memory %s > max_memory_gb %.1f GB" % (mem(P["mem_pred"]), S["max_memory_gb"]))
    elif P["mem_pred"] > S["max_memory_gb"] * 1e9:
        R["status"] = "skipped"
        R["notes"].append("not run: predicted LaGriT memory %s is over max_memory_gb %.1f GB (raise max_memory_gb, "
                          "or hscalar_factor up to 2)" % (mem(P["mem_pred"]), S["max_memory_gb"]))
        say("  NOT RUN: predicted memory %s > max_memory_gb %.1f GB" % (mem(P["mem_pred"]), S["max_memory_gb"]))
        rc_out = 3
    else:
        say("  LaGriT running (its output: %s) ..." % os.path.join(S["out"], "run.out"))
        rc, dt, peak = run(S, P)
        R["time"], R["peak"] = dt, peak
        say("  LaGriT done, %s, peak memory %s" % (hms(dt), mem(peak)))
        text = open(os.path.join(S["out"], "run.out"), errors="replace").read()
        R["report"] = report_lines(text)
        ok = rc == 0 and "LaGriT successfully completed" in text
        if not ok:
            R["notes"].append("LaGriT did not finish (exit %d): see run.out" % rc)
        for bad in ("ERROR", "Program received"):
            if bad in text:
                R["notes"].append('"%s" in run.out' % bad)
        X = analyse(S["out"], P, S["_ns"])
        f, b = X.get("after"), X.get("before")
        if f:
            rat = f.get("ratios") or []
            R["res"] = {"nv": f.get("nv"), "ntet": f["ntet"], "min_after": f["min"], "max_after": f["max"]}
            if rat:
                R["res"].update({"ratio_p05": q(rat, .05), "ratio_median": q(rat, .5), "ratio_p95": q(rat, .95)})
            if "eh_median" in f:
                R["res"].update({"eh_median": f["eh_median"], "eh_p05": f["eh_p05"], "eh_p95": f["eh_p95"],
                                 "frac_hmax": f["frac_hmax"], "d_far": f["d_far"]})
            if b:
                R["res"].update({"ntet_before": b["ntet"], "min_before": b["min"], "max_before": b["max"]})
            say("  result: %s vertices, %s tets, near-DFN ratio %s, angles %.1f to %.1f degrees" % (
                "{:,}".format(f.get("nv") or 0), "{:,}".format(f["ntet"]),
                "%.2f" % q(rat, .5) if rat else "-", f["min"], f["max"]))
        R["status"] = "done" if ok and f else "failed"
        if R["status"] == "failed":
            rc_out = 1
        if S["keep_hfield"] and os.path.isfile(os.path.join(S["out"], "h_field.inp")):
            say("  h field kept: %s (%s)" % (os.path.join(S["out"], "h_field.inp"),
                                             mem(os.path.getsize(os.path.join(S["out"], "h_field.inp")))))
    sec = section_html(S, P, X, R)
    open(os.path.join(S["out"], "section.html"), "w").write(sec)
    json.dump(R, open(os.path.join(S["out"], "result.json"), "w"), indent=1)
    open(os.path.join(S["out"], "summary.html"), "w").write(
        page_html("DFN-driven Poisson disk tets: %s" % S["name"], INTRO, [R], [sec]))
    say("  page: %s" % os.path.join(S["out"], "summary.html"))
    return rc_out


if __name__ == "__main__":
    sys.exit(main())
