#!/usr/bin/env python3
"""
check_test_poisson_3d.py - checker for the 3D Poisson disk harnesses:
test_Poisson_3D.lgi (this directory) and poisson_3d_letters.lgi
(../test_poisson_3d_refine_letters). Python 3 standard library only.

Called by run_test_poisson_3d.sh / run_poisson_3d_letters.sh, once per
row:

    check_test_poisson_3d.py outputs CASES            files the deck writes
    check_test_poisson_3d.py ids     CASES            case ids, in order
    check_test_poisson_3d.py run     CASES STDOUT OUT STATUS
    check_test_poisson_3d.py deck    CASES
    check_test_poisson_3d.py case    CASES ID OUT SVGDIR
    check_test_poisson_3d.py merge   CASES
    check_test_poisson_3d.py field   CASES

    STDOUT = lagrit's captured stdout (full message text)
    OUT    = the copied lagrit.out (used to split the log per run;
             it truncates driver messages, see row_run)
    STATUS = lagrit exit status

Every row prints one "key value" line per quantity, then
    result PASS | FAIL | SKIP
    summary <one line for the runner's table>
and exits 1 on FAIL. Only HARD checks decide the result; everything else
is REPORTED, NOT JUDGED (lines prefixed "hard_" are the judged ones).

----------------------------------------------------------------------
DEFINITIONS (the README points here)
----------------------------------------------------------------------

h at a node
    uniform runs: H_REFINE_SIZE. radial runs: make_field(), a copy of
    test_user_resolution.mlgi, because the driver releases
    mo_poi_h_field before returning:
        dist = distance to the nearest of line_n points spaced evenly on
               x=line_x, y=line_y, z = box zmin..zmax (inclusive)
        h    = min(ceil, max(floor, slope*dist + icpt))
    distance runs (letters): a copy of letters_h_field.mlgi:
        dist = distance to the nearest VERTEX of the mesh file `src`
               (compute/distance_field uses vertices only)
        h    = min(ceil, max(floor, slope*dist + icpt))
    "variable" below means radial or distance.

face node
    a node lying on a box face (within TOL_REL * largest box extent).
    interior node = every other node.

pair separation  m = d / max(h_p, h_q)       (user's rule, as in 2D)
    Every pair with d < SEARCH * max(h_p,h_q) is found (each node
    searches SEARCH*h_own, and the node with the larger h always reaches
    the pair). Pairs are split into three classes:
        II  both interior      FF  both face      FI  face - interior
    Reported per class: min m, number of pairs with m < 1 and m < 0.9
    (m < 1 means m < 1 - REL_EPS, so exact lattice spacing is not counted).
    With an edge buffer of b, FI pairs sit about b*h apart BY DESIGN,
    so FI min m ~ buf is expected, not a fault.

nearest neighbour (NN)  d_nn / h_p
    Interior nodes: distance to the nearest node of any kind, over h at
    the node. Reported as p05 / median / p95. If no node lies within
    SEARCH*h_p, the search widens to 2*SEARCH*h_p.
    Face nodes: distance to the nearest OTHER FACE node over h at the
    node ("face spacing"). On a radial run this is where the uniform
    face lattice meets a coarser field (see face_nn_over_h).

tracking (variable runs)   interior NN d/h split into 5 equal bands of h;
    median per band; spread = largest band median / smallest.

wall gap   distance from an interior node to the nearest box face, / h.
    With a buffer, the sampler box is the user box shrunk by buf*h
    (h = H_REFINE_SIZE, NOT the field), so min wall gap ~ buf.
    wall_gap_min_over_h divides by H_REFINE_SIZE; interior_wall_gap_lt0.1h
    counts interior nodes closer to a face than 0.1 x h AT THE NODE.

hull   hull_faces_off_box = boundary triangles not lying on one box face;
    mesh_volume_missing_over_box = 1 - (sum of tet volumes)/(box volume).
    Judged only for buffered runs (hull = box); reported otherwise.

face lattice   along the box edge lines (nodes on two faces): mean
    spacing / h per axis. Compared with the brick the driver builds:
    n = ceiling(L/h) points per axis -> spacing L/(n-1).

tets
    signed volume D = (b-a).((c-a)x(d-a)); every tet must share the
    sign of the majority and |D|/6 > TOL_VOL*h^3.
    radius ratio q = 3 r_in / R_circ, 1 for a regular tet, -> 0 for a
    sliver.  min dihedral angle per tet, degrees.
    faces: each triangle shared by at most 2 tets; a face used once is
    a hull face. With a buffer the hull must be the box itself: every
    hull face on a box face and total volume = box volume.

stor (FEHM format, LaGriT docs "FEHMN Stor File Format")
    Healthy off-diagonal area/distance coefficients are <= 0 in the
    file ("flow is from high to low"). A POSITIVE value is what LaGriT
    calls a negative coupling coefficient. Counted per distinct node
    pair; the diagonal is skipped. As in LaGriT's own report, only values
    above STOR_SIG_REL x max|coef| count ("significant"); smaller
    positive values are reported separately as tiny. Verified on the
    real case 05: 180 pairs, max +0.01196, identical to LaGriT.

----------------------------------------------------------------------
HARD checks
    run   : lagrit exit status 0; one createpts echo and one driver exit
            line per case in lagrit.out; no bare ERROR/WARNING line there;
            on stdout (if captured): no POISSON DISK error/warning block
            and one clean "poisson_disk_3d exit" per case.
    deck  : every case line matches the defines in effect at its infile.
    case  : .inp written and parses, tets only; NEQ of the .stor =
            nnodes; all nodes inside the box; no duplicate nodes; no
            degenerate or inverted tet; no orphan node; no face shared by
            >2 tets; buffered: interior nodes inside the sample box, hull
            = box (faces and volume); stor_neg=0 cases: no positive
            off-diagonal coefficient; e_vol (if dumped) > 0.
    merge : node and element counts = sums over the cases.
    field : (only if the dump exists; a missing dump FAILS when the
            field line says required=1) max relative error vs make_field.
            distance field: h_field_att vs the formula of the dumped
            dfield at every point, and vs the checker's own distance at
            a sample of points (row_field_distance).
"""

import math
import os
import re
import sys
from array import array

TOL_REL = 1.0e-8        # face / containment tolerance, x largest extent
TOL_VOL = 1.0e-10       # degenerate tet if volume < TOL_VOL * h^3
SEARCH = 1.5            # pair / NN search radius in units of h
FIELD_TOL = 1.0e-8      # field row relative error limit
MAX_DOTS = 8000         # cap on dots per SVG panel
DUP_REL = 1.0e-6        # duplicate node if d < DUP_REL * h
STOR_SIG_REL = 1.0e-8   # .stor wrong-sign coef counts above this x max|coef|
REL_EPS = 1.0e-9        # "d < h" means d/h < 1 - REL_EPS (rounding)


# ======================================================================
# cases file
# ======================================================================

def read_cases(path):
    c = {"cases": [], "box": None, "deck": None, "merge": None,
         "field": None}
    with open(path) as f:
        for raw in f:
            line = raw.split("#", 1)[0].strip()
            if not line:
                continue
            w = line.split()
            if w[0] == "box":
                c["box"] = [float(v) for v in w[1:7]]
            elif w[0] == "deck":
                c["deck"] = w[1]
            elif w[0] == "merge":
                c["merge"] = w[1]
            elif w[0] == "field":
                c["field"] = dict(kv.split("=", 1) for kv in w[1:])
            elif w[0] == "case":
                d = {"id": w[1]}
                for kv in w[2:]:
                    k, v = kv.split("=", 1)
                    d[k] = v
                for k in ("h", "buf", "ztrans", "floor", "ceil", "slope",
                          "icpt", "line_x", "line_y"):
                    if k in d:
                        d[k] = float(d[k])
                for k in ("ifbuf", "line_n"):
                    if k in d:
                        d[k] = int(d[k])
                c["cases"].append(d)
    return c


def find_case(cases, cid):
    for d in cases["cases"]:
        if d["id"] == cid:
            return d
    sys.exit("no case %s in cases file" % cid)


# ======================================================================
# the h field
# ======================================================================

class DistanceField:
    """
    field=distance (test_poisson_3d_refine_letters, letters_h_field.mlgi):
        d = distance to the NEAREST VERTEX of the source mesh `src`
            (compute/distance_field measures to vertices only)
        h = min(ceil, max(floor, slope*d + icpt))
    Nearest vertex by a ring search over a uniform grid of the source
    vertices. With slope > 0, h = ceil for every d >= dcap =
    (ceil - icpt) / slope, so the search stops there and reports dcap.
    """
    def __init__(self, case):
        m = read_avs(case["src"], want_cells=False)
        self.sx, self.sy, self.sz = m["x"], m["y"], m["z"]
        self.fl, self.ce = case["floor"], case["ceil"]
        self.sl, self.ic = case["slope"], case["icpt"]
        self.dcap = (self.ce - self.ic) / self.sl
        cs = max(self.dcap / 12.0, 1e-12)
        self.cs = cs
        self.grid = {}
        for i in range(m["nn"]):
            k = (int(math.floor(self.sx[i] / cs)),
                 int(math.floor(self.sy[i] / cs)),
                 int(math.floor(self.sz[i] / cs)))
            self.grid.setdefault(k, []).append(i)
        ks = list(self.grid)
        self.lo = [min(k[a] for k in ks) for a in range(3)]
        self.hi = [max(k[a] for k in ks) for a in range(3)]
        self.nsrc = m["nn"]

    def d(self, x, y, z):
        cs, g, sx, sy, sz = self.cs, self.grid, self.sx, self.sy, self.sz
        q = (int(math.floor(x / cs)), int(math.floor(y / cs)),
             int(math.floor(z / cs)))
        lo, hi = self.lo, self.hi
        best = float("inf")
        kmax = int(self.dcap / cs) + 2
        for k in range(kmax + 1):
            r0 = [max(lo[a], q[a] - k) for a in range(3)]
            r1 = [min(hi[a], q[a] + k) for a in range(3)]
            for a in range(r0[0], r1[0] + 1):
                ea = abs(a - q[0]) == k
                for b in range(r0[1], r1[1] + 1):
                    eb = ea or abs(b - q[1]) == k
                    for c in range(r0[2], r1[2] + 1):
                        if not (eb or abs(c - q[2]) == k):
                            continue        # inside: done in an earlier ring
                        lst = g.get((a, b, c))
                        if lst is None:
                            continue
                        for j in lst:
                            dx = sx[j] - x; dy = sy[j] - y; dz = sz[j] - z
                            v = dx * dx + dy * dy + dz * dz
                            if v < best:
                                best = v
            # every unvisited cell is more than k*cs away
            if best <= (k * cs) ** 2:
                return math.sqrt(best)
            if k * cs >= self.dcap:
                break
        return min(math.sqrt(best), self.dcap) if best < 1e300 else self.dcap

    def h_of_d(self, d):
        v = self.sl * d + self.ic
        v = self.fl if v < self.fl else v     # math/floor, then ceiling
        v = self.ce if v > self.ce else v
        return v

    def h(self, x, y, z):
        return self.h_of_d(self.d(x, y, z))


_DFIELDS = {}


def distance_field(case):
    key = (case["src"], case["floor"], case["ceil"], case["slope"],
           case["icpt"])
    if key not in _DFIELDS:
        _DFIELDS[key] = DistanceField(case)
    return _DFIELDS[key]


def make_field(case, box):
    """Return h(x, y, z) for a case. Copy of test_user_resolution.mlgi
    (radial) or letters_h_field.mlgi (distance)."""
    if case.get("field", "uniform") == "uniform":
        h0 = case["h"]
        return lambda x, y, z: h0
    if case.get("field") == "distance":
        return distance_field(case).h
    lx, ly, n = case["line_x"], case["line_y"], case["line_n"]
    z0, z1 = box[4], box[5]
    dz = (z1 - z0) / (n - 1)
    fl, ce, sl, ic = case["floor"], case["ceil"], case["slope"], case["icpt"]

    def h(x, y, z):
        k = int(round((z - z0) / dz))
        k = 0 if k < 0 else (n - 1 if k > n - 1 else k)
        zk = z0 + k * dz
        d = math.sqrt((x - lx) ** 2 + (y - ly) ** 2 + (z - zk) ** 2)
        v = sl * d + ic
        v = fl if v < fl else v       # math/floor first, as in the .mlgi
        v = ce if v > ce else v       # then math/ceiling
        return v
    return h


def radial_r(case, x, y):
    return math.sqrt((x - case["line_x"]) ** 2 + (y - case["line_y"]) ** 2)


# ======================================================================
# file readers
# ======================================================================

def read_avs(path, want_cells=True):
    """AVS UCD ascii as written by dump/avs. Returns a dict."""
    with open(path) as f:
        lines = f.read().split("\n")
    hdr = lines[0].split()
    nn, ne, nnd, ncd = int(hdr[0]), int(hdr[1]), int(hdr[2]), int(hdr[3])
    x = array("d", bytes(8 * nn))
    y = array("d", bytes(8 * nn))
    z = array("d", bytes(8 * nn))
    ids = {}
    for i in range(nn):
        w = lines[1 + i].split()
        ids[int(w[0])] = i
        x[i] = float(w[1]); y[i] = float(w[2]); z[i] = float(w[3])
    seq = all(ids.get(i + 1) == i for i in (0, nn - 1)) and len(ids) == nn
    tets = array("l")
    types = {}
    p = 1 + nn
    if want_cells:
        for j in range(ne):
            w = lines[p + j].split()
            t = w[2]
            types[t] = types.get(t, 0) + 1
            if t == "tet":
                v = w[3:7]
                if seq:
                    tets.extend((int(v[0]) - 1, int(v[1]) - 1,
                                 int(v[2]) - 1, int(v[3]) - 1))
                else:
                    tets.extend(ids[int(a)] for a in v)
    p += ne

    def read_data(p, nrec):
        names = []
        if p >= len(lines) or not lines[p].strip():
            return p, {}
        w = lines[p].split()
        nf = int(w[0])
        sizes = [int(s) for s in w[1:1 + nf]]
        for k in range(nf):
            names.append(lines[p + 1 + k].split(",")[0].strip())
        p += 1 + nf
        cols = {n: array("d") for n in names}
        offs = []
        o = 1
        for s in sizes:
            offs.append(o)
            o += s
        for r in range(nrec):
            w = lines[p + r].split()
            for n, o in zip(names, offs):
                cols[n].append(float(w[o]))
        return p + nrec, cols

    nodeatt, cellatt = {}, {}
    if nnd > 0:
        p, nodeatt = read_data(p, nn)
    if ncd > 0 and want_cells:
        p, cellatt = read_data(p, ne)
    return {"nn": nn, "ne": ne, "x": x, "y": y, "z": z, "tets": tets,
            "types": types, "nodeatt": nodeatt, "cellatt": cellatt}


def read_avs_header(path):
    with open(path) as f:
        w = f.readline().split()
    return int(w[0]), int(w[1])


def read_stor(path):
    """FEHM ascii .stor. Returns a dict, or raises ValueError."""
    with open(path) as f:
        f.readline()
        f.readline()
        p = f.readline().split()
        nwc, neq, nsz, nac = int(p[0]), int(p[1]), int(p[2]), int(p[3])
        tok = f.read().split()
    ncoef = nsz - neq - 1
    need = neq + nsz + ncoef + (neq + 1) + neq + nwc * nac
    if len(tok) != need:
        raise ValueError("stor: %d values after header, expected %d"
                         % (len(tok), need))
    q = 0
    vol = [float(t) for t in tok[q:q + neq]]; q += neq
    irow = [int(t) for t in tok[q:q + neq + 1]]; q += neq + 1
    jcol = [int(t) for t in tok[q:q + ncoef]]; q += ncoef
    ptr = [int(t) for t in tok[q:q + ncoef]]; q += ncoef
    q += neq + 1                                     # padding zeros
    q += neq                                         # diagonal pointers
    coef = [float(t) for t in tok[q:q + nwc * nac]]
    if irow[0] != neq + 1 or irow[-1] != nsz:
        raise ValueError("stor: row pointer block inconsistent")
    if nac == 1:
        val = coef
    elif nac == 4:
        val = coef[3 * nwc:4 * nwc]                  # scalar part
    else:
        raise ValueError("stor: NUM_AREA_COEF=%d (vector only) "
                         "not supported" % nac)
    # Count distinct node pairs, not matrix entries: the matrix is
    # symmetric in practice, but a lone wrong-sign entry must still count.
    # LaGriT counts a wrong-sign coefficient as "significant" only above
    # 1e-8 x the largest |coefficient| ("Number of 'zero' (< 10e-8 *max)
    # coefs" in its report); smaller ones (seen: +1.8e-19) are noise.
    amax = max((abs(v) for v in val), default=0.0)
    sig = STOR_SIG_REL * amax
    pos, tiny, zero = set(), set(), set()
    noff = 0
    vmax, vmin = -1e300, 1e300
    for i in range(neq):
        for k in range(irow[i] - neq - 1, irow[i + 1] - neq - 1):
            j = jcol[k] - 1
            if j == i:
                continue
            noff += 1
            pk = ptr[k]
            v = 0.0 if pk == 0 else val[pk - 1]
            if v > sig:
                pos.add((min(i, j), max(i, j)))
            elif v > 0.0:
                tiny.add((min(i, j), max(i, j)))
            elif v == 0.0:
                zero.add((min(i, j), max(i, j)))
            if v > vmax:
                vmax = v
            if v < vmin:
                vmin = v
    return {"neq": neq, "nwc": nwc, "nac": nac, "ncoef": ncoef,
            "vol_min": min(vol), "vol_sum": sum(vol),
            "vol_nonpos": sum(1 for v in vol if v <= 0.0),
            "pairs": noff // 2, "pos_pairs": len(pos),
            "tiny_pos_pairs": len(tiny),
            "zero_pairs": len(zero), "coef_max": vmax, "coef_min": vmin}


def is_poisson_cmd(line):
    """LaGriT echoes every command into lagrit.out. The driver's own
    banners are NOT usable there: lagrit.out shows each message cut at
    its first blank and right justified ('===' for the Begin banner),
    so the echoed createpts command marks the start of each run."""
    return line.replace(" ", "").lower().startswith(
        "createpts/poisson_disk/3d_box")


SKIP_LINES = ("has negative coeff", "Negative coef at row")


def log_segments(path):
    """Split lagrit.out into per-run pieces at each 3D createpts echo.
    The per-tet / per-coefficient listings are counted, not kept."""
    if not os.path.exists(path):
        return []
    segs, cur = [], None
    with open(path, errors="replace") as f:
        for line in f:
            if is_poisson_cmd(line):
                cur = {"lines": [], "pcc_tets": 0, "neg_rows": 0}
                segs.append(cur)
            if cur is None:
                continue
            if SKIP_LINES[0] in line:
                cur["pcc_tets"] += 1
            elif SKIP_LINES[1] in line:
                cur["neg_rows"] += 1
            else:
                cur["lines"].append(line.rstrip())
    return segs


# ======================================================================
# statistics helpers
# ======================================================================

def pct(sorted_vals, p):
    if not sorted_vals:
        return float("nan")
    k = min(len(sorted_vals) - 1, max(0, int(round(p * (len(sorted_vals) - 1)))))
    return sorted_vals[k]


def fmt(v):
    if isinstance(v, float):
        if v != v:
            return "nan"
        return "%.4g" % v
    return str(v)


class Row:
    def __init__(self):
        self.lines = []
        self.fail = []
        self.skip = False

    def put(self, k, v):
        self.lines.append("%s %s" % (k, fmt(v)))

    def hard(self, k, ok, v):
        self.lines.append("hard_%s %s %s" % (k, "ok" if ok else "FAIL",
                                             fmt(v)))
        if not ok:
            self.fail.append(k)

    def finish(self, summary):
        for l in self.lines:
            print(l)
        if self.skip:
            res = "SKIP"
        else:
            res = "FAIL" if self.fail else "PASS"
        if self.fail:
            summary = "failed: " + ",".join(self.fail) + " | " + summary
        print("result " + res)
        print("summary " + summary)
        sys.exit(1 if res == "FAIL" else 0)


# ======================================================================
# geometry
# ======================================================================

def face_masks(m, box, tol):
    """bit k set when the node lies on box face k (xmin,xmax,...)."""
    x, y, z = m["x"], m["y"], m["z"]
    mask = bytearray(m["nn"])
    for i in range(m["nn"]):
        b = 0
        if abs(x[i] - box[0]) <= tol: b |= 1
        if abs(x[i] - box[1]) <= tol: b |= 2
        if abs(y[i] - box[2]) <= tol: b |= 4
        if abs(y[i] - box[3]) <= tol: b |= 8
        if abs(z[i] - box[4]) <= tol: b |= 16
        if abs(z[i] - box[5]) <= tol: b |= 32
        mask[i] = b
    return mask


def neighbour_search(m, hn, mask):
    """
    Pair separation and nearest neighbours. See DEFINITIONS.
    hn = h at each node. Points are bucketed into h classes a factor 2
    wide; each class gets its own grid (cell = SEARCH * class h max) over
    ALL points, so a query always scans 27 cells.
    """
    x, y, z = m["x"], m["y"], m["z"]
    n = m["nn"]
    hmin = min(hn)
    cls = [int(math.log(hn[i] / hmin, 2) + 1e-9) for i in range(n)]
    ncls = max(cls) + 1
    nn_any = array("d", [float("inf")]) * n       # interior NN distance
    nn_face = array("d", [float("inf")]) * n      # face -> face NN
    nn_fi = array("d", [float("inf")]) * n        # face -> interior, /max h
    pmin = {"II": float("inf"), "FF": float("inf"), "FI": float("inf")}
    plt1 = {"II": 0, "FF": 0, "FI": 0}
    plt09 = {"II": 0, "FF": 0, "FI": 0}
    ndup = 0
    for c in range(ncls):
        members = [i for i in range(n) if cls[i] == c]
        if not members:
            continue
        cell = SEARCH * max(hn[i] for i in members)
        for radius_mult, todo in ((1, members), (2, None)):
            if todo is None:
                # widen for nodes that found no nearest neighbour
                todo = [i for i in members
                        if (mask[i] == 0 and nn_any[i] == float("inf"))
                        or (mask[i] != 0 and nn_face[i] == float("inf"))]
                if not todo:
                    break
            cw = cell * radius_mult
            grid = {}
            inv = 1.0 / cw
            for j in range(n):
                key = (int(x[j] * inv), int(y[j] * inv), int(z[j] * inv))
                lst = grid.get(key)
                if lst is None:
                    grid[key] = [j]
                else:
                    lst.append(j)
            for i in todo:
                xi, yi, zi, hi = x[i], y[i], z[i], hn[i]
                fi = mask[i] != 0
                r2 = (SEARCH * radius_mult * hi) ** 2
                ci, cj, ck = int(xi * inv), int(yi * inv), int(zi * inv)
                best_any = nn_any[i] ** 2 if nn_any[i] < 1e300 else 1e300
                best_face = nn_face[i] ** 2 if nn_face[i] < 1e300 else 1e300
                for a in (ci - 1, ci, ci + 1):
                    for b in (cj - 1, cj, cj + 1):
                        for cc in (ck - 1, ck, ck + 1):
                            lst = grid.get((a, b, cc))
                            if lst is None:
                                continue
                            for j in lst:
                                if j == i:
                                    continue
                                dx = x[j] - xi; dy = y[j] - yi; dz = z[j] - zi
                                d2 = dx * dx + dy * dy + dz * dz
                                if d2 >= r2:
                                    continue
                                fj = mask[j] != 0
                                if d2 < best_any:
                                    best_any = d2
                                if fi and fj and d2 < best_face:
                                    best_face = d2
                                if radius_mult != 1:
                                    continue
                                hj = hn[j]
                                # count each pair once, from the side
                                # of the larger h (which always sees it)
                                if hj > hi or (hj == hi and j < i):
                                    continue
                                d = math.sqrt(d2)
                                if d < DUP_REL * hi:
                                    ndup += 1
                                mm = d / hi
                                k = ("FF" if fi and fj else
                                     "FI" if fi or fj else "II")
                                if mm < pmin[k]:
                                    pmin[k] = mm
                                if mm < 1.0 - REL_EPS:
                                    plt1[k] += 1
                                    if mm < 0.9:
                                        plt09[k] += 1
                                if k == "FI":
                                    f = i if fi else j
                                    if mm < nn_fi[f]:
                                        nn_fi[f] = mm
                nn_any[i] = math.sqrt(best_any) if best_any < 1e300 else float("inf")
                nn_face[i] = math.sqrt(best_face) if best_face < 1e300 else float("inf")
    return {"nn_any": nn_any, "nn_face": nn_face, "nn_fi": nn_fi,
            "pmin": pmin, "plt1": plt1, "plt09": plt09, "ndup": ndup}


def tet_measures(m, h_ref):
    """Signed volumes, radius ratio, min dihedral (deg), per tet."""
    x, y, z, t = m["x"], m["y"], m["z"], m["tets"]
    nt = len(t) // 4
    sgn = array("b", bytes(nt))
    vol = array("d", bytes(8 * nt))
    rr = array("d", bytes(8 * nt))
    dih = array("d", bytes(8 * nt))
    sqrt, acos = math.sqrt, math.acos
    r2d = 180.0 / math.pi
    for e in range(nt):
        a, b, c, d = t[4 * e], t[4 * e + 1], t[4 * e + 2], t[4 * e + 3]
        ax, ay, az = x[a], y[a], z[a]
        ux, uy, uz = x[b] - ax, y[b] - ay, z[b] - az
        vx, vy, vz = x[c] - ax, y[c] - ay, z[c] - az
        wx, wy, wz = x[d] - ax, y[d] - ay, z[d] - az
        # v x w, w x u, u x v
        vwx, vwy, vwz = vy * wz - vz * wy, vz * wx - vx * wz, vx * wy - vy * wx
        wux, wuy, wuz = wy * uz - wz * uy, wz * ux - wx * uz, wx * uy - wy * ux
        uvx, uvy, uvz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
        D = ux * vwx + uy * vwy + uz * vwz
        vol[e] = abs(D) / 6.0
        sgn[e] = 1 if D > 0 else (-1 if D < 0 else 0)
        if D == 0.0:
            continue
        # outward (for D>0) face normals: n_d = v x u, n_c = u x w,
        # n_b = w x v, n_a = -(n_b+n_c+n_d). Sign is irrelevant below as
        # long as all four are consistent.
        ndx, ndy, ndz = -uvx, -uvy, -uvz
        ncx, ncy, ncz = -wux, -wuy, -wuz
        nbx, nby, nbz = -vwx, -vwy, -vwz
        nax, nay, naz = -(nbx + ncx + ndx), -(nby + ncy + ndy), -(nbz + ncz + ndz)
        la = sqrt(nax * nax + nay * nay + naz * naz)
        lb = sqrt(nbx * nbx + nby * nby + nbz * nbz)
        lc = sqrt(ncx * ncx + ncy * ncy + ncz * ncz)
        ld = sqrt(ndx * ndx + ndy * ndy + ndz * ndz)
        S = 0.5 * (la + lb + lc + ld)
        uu = ux * ux + uy * uy + uz * uz
        vv = vx * vx + vy * vy + vz * vz
        ww = wx * wx + wy * wy + wz * wz
        alx = uu * vwx + vv * wux + ww * uvx
        aly = uu * vwy + vv * wuy + ww * uvy
        alz = uu * vwz + vv * wuz + ww * uvz
        al = sqrt(alx * alx + aly * aly + alz * alz)
        rr[e] = 3.0 * D * D / (S * al) if S * al > 0 else 0.0
        # dihedral between faces k,l: cos = -(nk.nl)/(|nk||nl|)
        cmax = max(
            -(nax * nbx + nay * nby + naz * nbz) / (la * lb),
            -(nax * ncx + nay * ncy + naz * ncz) / (la * lc),
            -(nax * ndx + nay * ndy + naz * ndz) / (la * ld),
            -(nbx * ncx + nby * ncy + nbz * ncz) / (lb * lc),
            -(nbx * ndx + nby * ndy + nbz * ndz) / (lb * ld),
            -(ncx * ndx + ncy * ndy + ncz * ndz) / (lc * ld))
        cmax = 1.0 if cmax > 1.0 else cmax
        dih[e] = acos(cmax) * r2d
    return {"sgn": sgn, "vol": vol, "rr": rr, "dih": dih}


def face_check(m, mask):
    """Faces used by >2 tets, hull faces, hull faces not on a box face."""
    t = m["tets"]
    n = m["nn"]
    keys = []
    app = keys.append
    for e in range(len(t) // 4):
        a, b, c, d = sorted(t[4 * e:4 * e + 4])
        app((a * n + b) * n + c)
        app((a * n + b) * n + d)
        app((a * n + c) * n + d)
        app((b * n + c) * n + d)
    keys.sort()
    over = hull = hull_off = 0
    i, L = 0, len(keys)
    while i < L:
        j = i + 1
        while j < L and keys[j] == keys[i]:
            j += 1
        cnt = j - i
        if cnt > 2:
            over += 1
        elif cnt == 1:
            hull += 1
            k = keys[i]
            c = k % n; k //= n
            b = k % n; a = k // n
            if not (mask[a] & mask[b] & mask[c]):
                hull_off += 1
        i = j
    return over, hull, hull_off


# ======================================================================
# SVG
# ======================================================================

def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


HALO = ('paint-order="stroke" stroke="#fff" stroke-width="3" '
        'stroke-linejoin="round"')


class Svg:
    def __init__(self, w, h, uid):
        self.w, self.h, self.uid, self.parts = w, h, uid, []
        self.nclip = 0

    def add(self, s):
        self.parts.append(s)

    def text(self, x, y, s, size=11, anchor="start", color="#222",
             halo=False):
        self.add('<text x="%.1f" y="%.1f" font-size="%d" '
                 'text-anchor="%s" fill="%s" %s>%s</text>'
                 % (x, y, size, anchor, color, HALO if halo else "",
                    esc(s)))

    def clip(self, x, y, w, h):
        self.nclip += 1
        cid = "%s_clip%d" % (self.uid, self.nclip)
        self.add('<clipPath id="%s"><rect x="%.1f" y="%.1f" width="%.1f" '
                 'height="%.1f"/></clipPath>' % (cid, x, y, w, h))
        return cid

    def render(self):
        return ('<svg xmlns="http://www.w3.org/2000/svg" width="%d" '
                'height="%d" viewBox="0 0 %d %d" font-family="sans-serif">'
                '<rect width="100%%" height="100%%" fill="#fff"/>%s</svg>\n'
                % (self.w, self.h, self.w, self.h, "".join(self.parts)))


def thin(idx, cap):
    if len(idx) <= cap:
        return idx
    step = len(idx) / float(cap)
    return [idx[int(k * step)] for k in range(cap)]


def panel_points(svg, x0, y0, pw, ph, title, us, vs, flags,
                 urange, vrange, boxes, ulab, vlab):
    """Scatter of a slab: flags 1 = face node (grey), 0 = interior."""
    svg.text(x0, y0 - 6, title, 12)
    svg.add('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" '
            'fill="#fafafa" stroke="#999"/>' % (x0, y0, pw, ph))
    cid = svg.clip(x0, y0, pw, ph)
    # 3 % padding so nodes ON the box faces are drawn whole, inside
    pu = 0.03 * (urange[1] - urange[0])
    pv = 0.03 * (vrange[1] - vrange[0]) * pw / ph
    urange = (urange[0] - pu, urange[1] + pu)
    vrange = (vrange[0] - pv, vrange[1] + pv)
    su = pw / (urange[1] - urange[0])
    sv = ph / (vrange[1] - vrange[0])
    out = ['<g clip-path="url(#%s)">' % cid]
    for (u0, u1, v0, v1, style) in boxes:
        out.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" '
                   'fill="none" %s/>' % (x0 + (u0 - urange[0]) * su,
                                         y0 + ph - (v1 - vrange[0]) * sv,
                                         (u1 - u0) * su, (v1 - v0) * sv,
                                         style))
    # box outline, then interior nodes, then face nodes on top
    out.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" '
               'fill="none" stroke="#444" stroke-width="0.8"/>'
               % (x0 + pu * su, y0 + pv * sv, pw - 2 * pu * su,
                  ph - 2 * pv * sv))
    for want in (0, 1):
        for u, v, f in zip(us, vs, flags):
            if bool(f) != bool(want):
                continue
            out.append('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s"/>'
                       % (x0 + (u - urange[0]) * su,
                          y0 + ph - (v - vrange[0]) * sv,
                          "1.6" if f else "1.3",
                          "#777" if f else "#1f5fbf"))
    out.append("</g>")
    svg.add("".join(out))
    svg.text(x0 + pw / 2, y0 + ph + 14, ulab, 10, "middle")
    svg.text(x0 - 4, y0 + ph / 2, vlab, 10, "end")


def panel_hist(svg, x0, y0, pw, ph, title, series, lo, hi, nbins, refs,
               xlab):
    """series = [(values, colour, label)], refs = [(x, label)]."""
    svg.text(x0, y0 - 6, title, 12)
    svg.add('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" '
            'fill="#fafafa" stroke="#999"/>' % (x0, y0, pw, ph))
    counts = []
    for vals, col, lab in series:
        cnt = [0] * nbins
        for v in vals:
            if v != v or v == float("inf"):
                continue
            k = int((v - lo) / (hi - lo) * nbins)
            k = 0 if k < 0 else (nbins - 1 if k >= nbins else k)
            cnt[k] += 1
        tot = float(sum(cnt)) or 1.0
        counts.append([c / tot for c in cnt])
    top = max([max(c) for c in counts] + [1e-9])
    bw = pw / nbins
    nser = len(series)
    for s, (vals, col, lab) in enumerate(series):
        for k, f in enumerate(counts[s]):
            if f <= 0:
                continue
            bh = f / top * (ph - 4)
            svg.add('<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" '
                    'fill="%s" fill-opacity="0.75"/>'
                    % (x0 + k * bw + s * bw / nser, y0 + ph - bh,
                       bw / nser, bh, col))
        svg.text(x0 + pw - 4, y0 + 14 + 13 * s, lab, 10, "end", col, True)
    # reference labels near the TOP, one row each, so they clear the
    # bars and each other
    for r, (xv, lab) in enumerate(refs):
        px = x0 + (xv - lo) / (hi - lo) * pw
        svg.add('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                'stroke="#c00" stroke-dasharray="4 3"/>'
                % (px, y0, px, y0 + ph))
        svg.text(px + 3, y0 + 13 + 13 * r, lab, 10, "start", "#c00", True)
    for k in range(5):
        v = lo + (hi - lo) * k / 4.0
        svg.text(x0 + pw * k / 4.0, y0 + ph + 12, "%g" % round(v, 3), 9,
                 "middle")
    svg.text(x0 + pw / 2, y0 + ph + 25, xlab, 10, "middle")


def panel_tracking(svg, x0, y0, pw, ph, rs, ds, curve, rmax, dmax,
                   what="axis", rlab="r = distance from the field axis",
                   clab="h(r)"):
    svg.text(x0, y0 - 6, "interior NN distance vs distance from " + what,
             12)
    svg.add('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" '
            'fill="#fafafa" stroke="#999"/>' % (x0, y0, pw, ph))
    cid = svg.clip(x0, y0, pw, ph)
    out = ['<g clip-path="url(#%s)">' % cid]
    for r, d in zip(rs, ds):
        out.append('<circle cx="%.1f" cy="%.1f" r="1.1" fill="#1f5fbf" '
                   'fill-opacity="0.5"/>'
                   % (x0 + r / rmax * pw, y0 + ph - d / dmax * ph))
    pts = " ".join("%.1f,%.1f" % (x0 + r / rmax * pw, y0 + ph - hh / dmax * ph)
                   for r, hh in curve)
    out.append('<polyline points="%s" fill="none" stroke="#c00" '
               'stroke-width="1.5"/>' % pts)
    out.append("</g>")
    svg.add("".join(out))
    r_lab, h_lab = curve[int(len(curve) * 0.8)]
    svg.text(x0 + r_lab / rmax * pw, y0 + ph - h_lab / dmax * ph - 6,
             clab, 10, "middle", "#c00", True)
    for k in range(5):
        svg.text(x0 + pw * k / 4.0, y0 + ph + 12, "%g" % round(rmax * k / 4, 3),
                 9, "middle")
        svg.text(x0 - 3, y0 + ph - ph * k / 4.0 + 3,
                 "%g" % round(dmax * k / 4, 3), 9, "end")
    svg.text(x0 + pw / 2, y0 + ph + 25, rlab, 10, "middle")


# ======================================================================
# rows
# ======================================================================

def row_case(cases, cid, outlog, svgdir):
    R = Row()
    box = cases["box"]
    case = find_case(cases, cid)
    h0 = case["h"]
    ifbuf, buf = case["ifbuf"], case["buf"]
    radial = case.get("field") == "radial"
    variable = case.get("field", "uniform") != "uniform"
    L = max(box[1] - box[0], box[3] - box[2], box[5] - box[4])
    tol = TOL_REL * L
    boxvol = (box[1] - box[0]) * (box[3] - box[2]) * (box[5] - box[4])
    R.put("case", cid)
    R.put("h", h0)
    R.put("field", case.get("field"))
    R.put("edge_buffer", "%d x %g h" % (ifbuf, buf))

    # ---- log segment for this run --------------------------------------
    segs = log_segments(outlog)
    idx = [d["id"] for d in cases["cases"]].index(cid)
    seg = segs[idx] if idx < len(segs) else []
    LOGKEYS = ("Total negative ccoefs found", "Number of Negative Coef",
               "Significant Negative Coefs", "(Aij/xij) max",
               "Total Volume", "Volume min")
    if seg:
        nerr = sum(1 for l in seg["lines"]
                   if l.strip() in ("ERROR", "WARNING")
                   or "POISSON DISK:" in l)
        R.hard("log_no_poisson_error", nerr == 0, nerr)
        for l in seg["lines"]:
            s = " ".join(l.split())
            if any(k in s for k in LOGKEYS):
                R.put("log", s)
        R.put("log_pcc_tet_lines", seg["pcc_tets"])
        R.put("log_negative_coef_rows", seg["neg_rows"])
    else:
        R.put("log", "no segment %d in %s" % (idx + 1, outlog))

    # ---- the .inp -------------------------------------------------------
    inp = case["inp"]
    if not os.path.exists(inp):
        R.hard("inp_written", False, inp)
        R.finish("no " + inp)
    try:
        m = read_avs(inp)
    except Exception as e:                       # noqa: broad on purpose
        R.hard("inp_parses", False, repr(e)[:60])
        R.finish("unreadable " + inp)
    nn, ne = m["nn"], m["ne"]
    ntet = len(m["tets"]) // 4
    R.put("nnodes", nn)
    R.put("ntets", ntet)
    R.hard("tets_only", ntet == ne and ne > 0,
           " ".join("%s=%d" % kv for kv in sorted(m["types"].items())))
    R.put("density_nodes_x_h3_per_vol", nn * h0 ** 3 / boxvol)

    x, y, z = m["x"], m["y"], m["z"]
    nout = sum(1 for i in range(nn)
               if not (box[0] - tol <= x[i] <= box[1] + tol and
                       box[2] - tol <= y[i] <= box[3] + tol and
                       box[4] - tol <= z[i] <= box[5] + tol))
    R.hard("inside_box", nout == 0, nout)

    mask = face_masks(m, box, tol)
    face = [i for i in range(nn) if mask[i]]
    inner = [i for i in range(nn) if not mask[i]]
    R.put("face_nodes", len(face))
    R.put("interior_nodes", len(inner))

    hfun = make_field(case, box)
    hn = array("d", (hfun(x[i], y[i], z[i]) for i in range(nn)))

    # wall gap and sample box
    gap = [min(x[i] - box[0], box[1] - x[i], y[i] - box[2],
               box[3] - y[i], z[i] - box[4], box[5] - z[i]) for i in inner]
    if gap:
        R.put("wall_gap_min_over_h", min(gap) / h0)
        # near-wall interior vertices, judged by h at the vertex
        R.put("interior_wall_gap_lt0.1h",
              sum(1 for g, i in zip(gap, inner) if g < 0.1 * hn[i]))
    if ifbuf == 1:
        s = buf * h0
        nbad = sum(1 for g in gap if g < s - tol)
        R.hard("interior_in_sample_box", nbad == 0, nbad)
        # brick prediction, n = ceiling(L/h) points per axis
        nx = [math.ceil((box[2 * k + 1] - box[2 * k]) / h0) for k in range(3)]
        pred = nx[0] * nx[1] * nx[2] - max(0, nx[0] - 2) * \
            max(0, nx[1] - 2) * max(0, nx[2] - 2)
        R.put("face_nodes_brick_prediction", pred)
        for k, ax, cmask in ((0, "x", 4 | 16), (1, "y", 1 | 16),
                             (2, "z", 1 | 4)):
            arr = (x, y, z)[k]
            on = sorted(arr[i] for i in face if (mask[i] & cmask) == cmask)
            if len(on) > 1:
                sp = (on[-1] - on[0]) / (len(on) - 1)
                R.put("face_lattice_d%s_over_h" % ax, sp / h0)
                R.put("face_lattice_d%s_predicted_over_h" % ax,
                      (box[2 * k + 1] - box[2 * k]) / (nx[k] - 1) / h0)

    # ---- neighbours -----------------------------------------------------
    ns = neighbour_search(m, hn, mask)
    R.hard("no_duplicate_nodes", ns["ndup"] == 0, ns["ndup"])
    for k in ("II", "FI", "FF"):
        if ns["pmin"][k] < float("inf"):
            R.put("sep_%s_min" % k, ns["pmin"][k])
            R.put("sep_%s_pairs_lt1" % k, ns["plt1"][k])
            R.put("sep_%s_pairs_lt0.9" % k, ns["plt09"][k])
    nnh = sorted(ns["nn_any"][i] / hn[i] for i in inner)
    if nnh:
        R.put("nn_interior_over_h_p05", pct(nnh, 0.05))
        R.put("nn_interior_over_h_median", pct(nnh, 0.5))
        R.put("nn_interior_over_h_p95", pct(nnh, 0.95))
        R.put("nn_interior_unresolved", sum(1 for v in nnh
                                            if v == float("inf")))
    fnn = sorted(ns["nn_face"][i] / h0 for i in face)
    if fnn:
        R.put("face_nn_over_href_median", pct(fnn, 0.5))
    if variable and face:
        fnf = sorted(ns["nn_face"][i] / hn[i] for i in face)
        R.put("face_nn_over_h_min", fnf[0])
        R.put("face_nn_over_h_median", pct(fnf, 0.5))
    band_meds = []
    if variable and inner:
        hlo = min(hn[i] for i in inner)
        hhi = max(hn[i] for i in inner)
        nb = 5
        bands = [[] for _ in range(nb)]
        for i in inner:
            k = int((hn[i] - hlo) / (hhi - hlo + 1e-300) * nb)
            bands[min(k, nb - 1)].append(ns["nn_any"][i] / hn[i])
        for k, b in enumerate(bands):
            b.sort()
            if b:
                band_meds.append(pct(b, 0.5))
                R.put("track_band%d_h_%.4g_%.4g_median" %
                      (k + 1, hlo + (hhi - hlo) * k / nb,
                       hlo + (hhi - hlo) * (k + 1) / nb), pct(b, 0.5))
        if band_meds:
            R.put("track_spread", max(band_meds) / min(band_meds))

    # ---- tets -----------------------------------------------------------
    tm = tet_measures(m, h0)
    npos = sum(1 for s in tm["sgn"] if s > 0)
    nneg = sum(1 for s in tm["sgn"] if s < 0)
    nmin = min(npos, nneg)
    R.hard("no_inverted_tets", nmin == 0, "%d of %d" % (nmin, ntet))
    hmin_field = min(hn)
    ndeg = sum(1 for v in tm["vol"] if v < TOL_VOL * hmin_field ** 3)
    R.hard("no_degenerate_tets", ndeg == 0, ndeg)
    vsum = sum(tm["vol"])
    R.put("mesh_volume_over_box", vsum / boxvol)
    R.put("mesh_volume_missing_over_box", 1.0 - vsum / boxvol)
    used = bytearray(nn)
    for v in m["tets"]:
        used[v] = 1
    orph = nn - sum(used)
    R.hard("no_orphan_nodes", orph == 0, orph)
    over, hull, hull_off = face_check(m, mask)
    R.hard("faces_shared_le_2", over == 0, over)
    R.put("hull_faces", hull)
    R.put("hull_faces_off_box", hull_off)
    if ifbuf == 1:
        R.hard("hull_is_box", hull_off == 0, hull_off)
        R.hard("volume_is_box", abs(vsum / boxvol - 1.0) < 1e-6,
               vsum / boxvol)
    rrs = sorted(tm["rr"])
    dih = sorted(tm["dih"])
    R.put("radius_ratio_min", rrs[0])
    R.put("radius_ratio_median", pct(rrs, 0.5))
    R.put("dihedral_min_deg", dih[0])
    R.put("dihedral_p01_deg", pct(dih, 0.01))
    R.put("dihedral_median_deg", pct(dih, 0.5))
    R.put("tets_dihedral_lt5", sum(1 for d in dih if d < 5.0))
    R.put("tets_dihedral_lt10", sum(1 for d in dih if d < 10.0))
    for name, col in sorted(m["cellatt"].items()):
        R.put("att_%s_min" % name, min(col))
        R.put("att_%s_max" % name, max(col))
    if "e_vol" in m["cellatt"]:
        nev = sum(1 for v in m["cellatt"]["e_vol"] if v <= 0.0)
        R.hard("e_vol_positive", nev == 0, nev)

    # ---- the .stor ------------------------------------------------------
    st_summary = ""
    stor = case.get("stor")
    if stor and os.path.exists(stor):
        try:
            s = read_stor(stor)
            R.hard("stor_neq_eq_nnodes", s["neq"] == nn, s["neq"])
            R.put("stor_pairs", s["pairs"])
            R.put("stor_positive_coef_pairs", s["pos_pairs"])
            R.put("stor_tiny_positive_coef_pairs", s["tiny_pos_pairs"])
            R.put("stor_zero_coef_pairs", s["zero_pairs"])
            R.put("stor_coef_max", s["coef_max"])
            R.put("stor_coef_min", s["coef_min"])
            R.put("stor_voronoi_vol_min", s["vol_min"])
            R.put("stor_voronoi_vol_sum_over_box", s["vol_sum"] / boxvol)
            R.put("stor_voronoi_vol_nonpositive", s["vol_nonpos"])
            if case.get("stor_neg") == "0":
                R.hard("stor_no_positive_coefs", s["pos_pairs"] == 0,
                       s["pos_pairs"])
            st_summary = " +coef %d" % s["pos_pairs"]
        except ValueError as e:
            R.hard("stor_parses", False, str(e))
    elif stor:
        R.hard("stor_written", False, stor)
    else:
        R.put("stor", "none (this deck writes no .stor)")

    # ---- picture --------------------------------------------------------
    # Square-ish boxes: top view (300 x 300) beside the side view. Wide
    # boxes (x extent > 1.8 x y extent, e.g. the letters demo): both views
    # full width, one above the other, true aspect ratio.
    t = h0 * (2.0 if variable else 1.0)
    zm = 0.5 * (box[4] + box[5])
    ym = 0.5 * (box[2] + box[3])
    wide = (box[1] - box[0]) > 1.8 * (box[3] - box[2])
    if wide:
        pw1 = 880.0
        ph1 = pw1 * (box[3] - box[2]) / (box[1] - box[0])
        ph2 = pw1 * (box[5] - box[4]) / (box[1] - box[0])
        top = (30, 50, pw1, ph1)
        side = (30, 50 + ph1 + 50, pw1, ph2)
        y_h = side[1] + ph2 + 70
    else:
        top = (30, 50, 300, 300)
        pw = 560.0
        side = (350, 50, pw, pw * (box[5] - box[4]) / (box[1] - box[0]))
        y_h = 400
    tall = (variable or face)
    svg = Svg(920, int(y_h + (360 if tall else 160)), "c" + cid)
    svg.text(10, 18, "case %s  %s  h %g  %s  buffer %s  nodes %d  tets %d"
             % (cid, inp, h0, case.get("field"),
                ("%g h" % buf) if ifbuf else "off", nn, ntet), 13)
    def slab(coord, cm):
        """interior nodes within t/2 of cm, plus face nodes from the face
        lattice layer nearest cm (a thin slab can fall between layers)"""
        pick = [i for i in inner if abs(coord[i] - cm) < t / 2]
        if face:
            c0 = min((coord[i] for i in face), key=lambda v: abs(v - cm))
            pick += [i for i in face if abs(coord[i] - c0) < t / 2]
        return thin(pick, MAX_DOTS)
    sel = slab(z, zm)
    boxes = []
    if ifbuf:
        s = buf * h0
        boxes.append((box[0] + s, box[1] - s, box[2] + s, box[3] - s,
                      'stroke="#c00" stroke-dasharray="4 3"'))
    panel_points(svg, top[0], top[1], top[2], top[3],
                 "top view, slab |z - %g| < %g" % (zm, t / 2),
                 [x[i] for i in sel], [y[i] for i in sel],
                 [mask[i] for i in sel], (box[0], box[1]), (box[2], box[3]),
                 boxes, "x", "y")
    sel = slab(y, ym)
    boxes = []
    if ifbuf:
        s = buf * h0
        boxes.append((box[0] + s, box[1] - s, box[4] + s, box[5] - s,
                      'stroke="#c00" stroke-dasharray="4 3"'))
    panel_points(svg, side[0], side[1], side[2], side[3],
                 "side view, slab |y - %g| < %g" % (ym, t / 2),
                 [x[i] for i in sel], [z[i] for i in sel],
                 [mask[i] for i in sel], (box[0], box[1]), (box[4], box[5]),
                 boxes, "x", "z")
    svg.text(side[0], side[1] + side[3] + 36,
             "blue = interior node, grey = face node"
             + (", dashed red = sampler box (shrunk by buf*h)"
                if ifbuf else ""), 10)
    fi_vals = [ns["nn_fi"][i] for i in face]
    series = [([ns["nn_any"][i] / hn[i] for i in inner], "#1f5fbf",
               "interior: nearest node d / h")]
    if face:
        series.append((fi_vals, "#e07b00",
                       "face: nearest interior d / max(h)"))
    panel_hist(svg, 30, y_h, 420, 120, "nearest-neighbour spacing",
               series, 0.0, 2.0, 40, [(1.0, "d = h")], "d / h")
    panel_hist(svg, 490, y_h, 420, 120,
               "smallest dihedral angle per tet",
               [(tm["dih"], "#555", "%d tets" % ntet)], 0.0, 90.0, 45,
               [(5.0, "5 deg sliver"), (10.0, "10 deg")], "degrees")
    if radial:
        rsel = thin(inner, 4000)
        rmax = math.sqrt(0.5) * (box[1] - box[0])
        curve = [(rmax * k / 100.0,
                  hfun(case["line_x"] + rmax * k / 100.0 / math.sqrt(2),
                       case["line_y"] + rmax * k / 100.0 / math.sqrt(2),
                       zm)) for k in range(101)]
        dmax = 2.0 * max(hh for _, hh in curve)
        panel_tracking(svg, 30, y_h + 190, 420, 120,
                       [radial_r(case, x[i], y[i]) for i in rsel],
                       [ns["nn_any"][i] for i in rsel], curve, rmax, dmax)
    elif case.get("field") == "distance":
        df = distance_field(case)
        rsel = thin(inner, 4000)
        rmax = 1.25 * df.dcap
        curve = [(rmax * k / 100.0, df.h_of_d(rmax * k / 100.0))
                 for k in range(101)]
        dmax = 2.0 * max(hh for _, hh in curve)
        panel_tracking(svg, 30, y_h + 190, 420, 120,
                       [df.d(x[i], y[i], z[i]) for i in rsel],
                       [ns["nn_any"][i] for i in rsel], curve, rmax, dmax,
                       "source", "d = distance to the nearest source vertex"
                       " (capped)", "h(d)")
    if face:
        panel_hist(svg, 490, y_h + 190, 420, 120,
                   "face nodes: spacing to nearest face node / h(field)",
                   [([ns["nn_face"][i] / hn[i] for i in face], "#888",
                     "%d face nodes" % len(face))],
                   0.0, 2.0, 40, [(1.0, "d = h")], "d / h at the node")
    with open(os.path.join(svgdir, "case_%s.svg" % cid), "w") as f:
        f.write(svg.render())

    summ = "%d nodes %d tets, II min %s, NN med %s, dih min %s%s" % (
        nn, ntet, fmt(ns["pmin"]["II"]),
        fmt(pct(nnh, 0.5)) if nnh else "-", fmt(dih[0]), st_summary)
    R.finish(summ)


def row_run(cases, stdoutf, outf, status):
    """
    The lagrit.out of Oct 1 2026 had every driver message cut at its
    FIRST BLANK and right justified (Begin banner '===', exit line
    'poisson_disk_3d', an error banner would be 'ERROR'); the Oct 6 run
    of the current build wrote full text to stdout. So lagrit.out is
    only trusted to show THAT a driver exit line was written (either
    form is counted); what it said is read from stdout, which the
    runner captures.
    """
    R = Row()
    ncase = len(cases["cases"])
    R.hard("lagrit_exit_status", status == "0", status)
    out = (open(outf, errors="replace").read().split("\n")
           if os.path.exists(outf) else [])
    std = (open(stdoutf, errors="replace").read()
           if os.path.exists(stdoutf) else "")
    nrun = sum(1 for l in out if is_poisson_cmd(l))
    R.hard("poisson_runs_eq_cases", nrun == ncase, nrun)
    # Either form: the full "poisson_disk_3d exit ..." line, or the bare
    # "poisson_disk_3d" an older build left in lagrit.out (see docstring).
    nexit = sum(1 for l in out
                if l.strip() == "poisson_disk_3d"
                or l.strip().startswith("poisson_disk_3d exit"))
    R.hard("exit_lines_eq_cases", nexit == ncase, nexit)
    ntrunc = sum(1 for l in out if l.strip() in ("ERROR", "WARNING"))
    R.hard("no_bare_error_warning_lines_in_lagrit_out", ntrunc == 0,
           ntrunc)
    if std.strip():
        ne = std.count("ERROR POISSON DISK:")
        nw = std.count("WARNING POISSON DISK:")
        R.hard("no_poisson_error_block_stdout", ne == 0, ne)
        R.hard("no_poisson_warning_block_stdout", nw == 0, nw)
        nclean = len(re.findall(r"poisson_disk_3d exit\s*$", std, re.M))
        R.hard("clean_exits_stdout_eq_cases", nclean == ncase, nclean)
    else:
        R.put("stdout", "empty or not captured: exit status text not "
              "checked (lagrit.out truncates it)")
    other = [l.strip() for l in out
             if ("ERROR" in l or "WARNING" in l)
             and l.strip() not in ("ERROR", "WARNING")
             and not l.lstrip().startswith("#")]
    R.put("other_error_warning_lines", len(other))
    for l in other[:5]:
        R.put("other", " ".join(l.split())[:100])
    R.finish("exit %s, %d runs, %d exit lines" % (status, nrun, nexit))


DECK_KEYS = {"H_REFINE_SIZE": "h", "IF_POI_EDGE_BUFFER": "ifbuf",
             "POI_EDGE_BUFFER": "buf", "Z_TRANSLATE": "ztrans",
             "H_FLOOR": "floor", "H_CEILING": "ceil", "SLOPE": "slope",
             "INTERCEPT": "icpt", "AVS_OUT": "inp", "STOR_OUT": "stor"}


def deck_lines(path):
    """Command lines of a deck: comments dropped, '&' continuations
    joined."""
    out, cur = [], ""
    for raw in open(path):
        line = raw.split("#", 1)[0].strip()
        if not line and not cur:
            continue
        if line.endswith("&"):
            cur += line[:-1] + " "
            continue
        out.append((cur + line).strip())
        cur = ""
    if cur:
        out.append(cur.strip())
    return out


def slash_tokens(line):
    return [t.strip() for t in line.split("/")]


def mlgi_distance_source(mlgi):
    """Source mesh object of compute/distance_field in an .mlgi, or None."""
    if not os.path.exists(mlgi):
        return None
    for line in deck_lines(mlgi):
        w = slash_tokens(line)
        if len(w) >= 4 and w[0].lower() == "compute" and \
                w[1].lower() == "distance_field":
            return w[3]
    return None


def row_deck(cases):
    """
    Runs are found two ways: an 'infile X' line (test_Poisson_3D.lgi:
    the createpts is inside X), or a createpts/poisson_disk/3d_box line
    in the deck itself (poisson_3d_letters.lgi), whose last token, if it
    ends in .mlgi, is the field script. The defines in effect at that
    line must match the case line.
    """
    R = Row()
    deck = cases["deck"]
    defs, runs = {}, []
    dumps = {}                      # mesh object -> file it was dumped to
    for line in deck_lines(deck):
        w = slash_tokens(line)
        if len(w) >= 3 and w[0].lower() == "define":
            defs[w[1]] = w[2]
        elif line.lower().startswith("infile"):
            runs.append((line.split()[1], dict(defs), dict(dumps), True))
        elif is_poisson_cmd(line):
            last = w[-1] if w[-1].lower().endswith(".mlgi") else ""
            runs.append((last, dict(defs), dict(dumps), False))
        elif len(w) >= 4 and w[0].lower() == "dump" and \
                w[1].lower() == "avs":
            dumps[w[3]] = w[2]
    box = [float(defs.get(k, "nan")) for k in
           ("POI_XMIN", "POI_XMAX", "POI_YMIN", "POI_YMAX", "POI_ZMIN",
            "POI_ZMAX")]
    R.hard("box_matches", box == cases["box"], " ".join(map(str, box)))
    R.hard("case_count", len(runs) == len(cases["cases"]), len(runs))
    bad = []
    for (mlgi, d, dumps, via_infile), c in zip(runs, cases["cases"]):
        if mlgi != c.get("mlgi", ""):
            bad.append("%s:mlgi" % c["id"])
        for dk, ck in DECK_KEYS.items():
            if ck not in c:
                continue
            dv = d.get(dk)
            cv = c[ck]
            if ck == "stor":
                dv = (dv or "") + ".stor"
            if isinstance(cv, (int, float)):
                ok = dv is not None and float(dv) == float(cv)
            else:
                ok = dv == cv
            if not ok:
                bad.append("%s:%s" % (c["id"], ck))
        if via_infile:
            exp_field = ("radial" if "user_res" in mlgi else "uniform")
        else:
            srcmo = mlgi_distance_source(mlgi) if mlgi else None
            exp_field = "distance" if srcmo else "uniform"
            # the field's source mesh must be the file the checker reads
            if srcmo and dumps.get(srcmo) != c.get("src"):
                bad.append("%s:src" % c["id"])
        if c.get("field") != exp_field:
            bad.append("%s:field" % c["id"])
    R.hard("cases_match_defines", not bad, ",".join(bad) or "all")
    R.finish("%d runs, %s" % (len(runs), "match" if not bad else
                              "MISMATCH " + ",".join(bad)))


def row_merge(cases):
    R = Row()
    f = cases["merge"]
    if not os.path.exists(f):
        R.hard("merge_written", False, f)
        R.finish("no " + f)
    mn, me = read_avs_header(f)
    sn = se = 0
    for c in cases["cases"]:
        if os.path.exists(c["inp"]):
            a, b = read_avs_header(c["inp"])
            sn += a
            se += b
    R.hard("nodes_eq_sum", mn == sn, "%d vs %d" % (mn, sn))
    R.hard("elements_eq_sum", me == se, "%d vs %d" % (me, se))
    R.finish("%d nodes, %d elements" % (mn, me))


def row_field(cases):
    R = Row()
    fd = cases.get("field") or {}
    f = fd.get("dump")
    if not f or not os.path.exists(f):
        R.put("field_dump", "absent")
        if fd.get("required") == "1":
            # the deck always writes it (letters): missing = a failed run
            R.hard("field_dump_written", False, f)
            R.finish("no field dump %s" % f)
        R.skip = True
        R.finish("no field dump %s (its dump line is commented out by "
                 "default)" % f)
    case = find_case(cases, fd["case"])
    m = read_avs(f, want_cells=False)
    att = m["nodeatt"].get("h_field_att")
    if att is None:
        R.hard("has_h_field_att", False, ",".join(m["nodeatt"]))
        R.finish("no h_field_att in " + f)
    if case.get("field") == "distance":
        row_field_distance(R, m, att, case, fd)
    hf = make_field(case, cases["box"])
    worst = 0.0
    for i in range(m["nn"]):
        e = abs(att[i] - hf(m["x"][i], m["y"][i], m["z"][i])) / att[i]
        worst = max(worst, e)
    R.put("nodes", m["nn"])
    R.put("field_min", min(att))
    R.put("field_max", max(att))
    R.hard("max_rel_err", worst < FIELD_TOL, worst)
    R.finish("%d nodes, max rel err %s (case %s)" % (m["nn"], fmt(worst),
                                                     fd["case"]))


FIELD_SAMPLE = 20000     # distance field: points checked against exact d


def row_field_distance(R, m, att, case, fd):
    """
    The dump carries both dfield (LaGriT's distance) and h_field_att.
    Checked: (a) at EVERY point, h_field_att = the .mlgi formula applied
    to the dumped dfield; (b) at every k-th point (at most FIELD_SAMPLE),
    h_field_att = the formula applied to the checker's own distance to
    the source vertices. (b) is a sample because the exact search is
    slow in pure Python on a field mesh of ~10^5-10^6 points.
    """
    df = distance_field(case)
    dfield = m["nodeatt"].get("dfield")
    if dfield is None:
        R.hard("has_dfield", False, ",".join(m["nodeatt"]))
        R.finish("no dfield in " + fd["dump"])
    worst_a = 0.0
    for i in range(m["nn"]):
        e = abs(att[i] - df.h_of_d(dfield[i])) / att[i]
        worst_a = max(worst_a, e)
    stride = max(1, m["nn"] // FIELD_SAMPLE)
    worst_b, worst_d, nchk = 0.0, 0.0, 0
    for i in range(0, m["nn"], stride):
        d = df.d(m["x"][i], m["y"][i], m["z"][i])
        nchk += 1
        worst_b = max(worst_b, abs(att[i] - df.h_of_d(d)) / att[i])
        if d < df.dcap:
            worst_d = max(worst_d, abs(dfield[i] - d))
    R.put("nodes", m["nn"])
    R.put("source_vertices", df.nsrc)
    R.put("field_min", min(att))
    R.put("field_max", max(att))
    R.put("dfield_min", min(dfield))
    R.put("dfield_max", max(dfield))
    R.put("points_at_floor", sum(1 for v in att if v <= df.fl))
    R.put("points_at_ceiling", sum(1 for v in att if v >= df.ce))
    R.put("exact_distance_points_checked", nchk)
    R.put("dfield_max_abs_err_below_dcap", worst_d)
    R.hard("formula_of_dfield_max_rel_err", worst_a < FIELD_TOL, worst_a)
    R.hard("exact_distance_max_rel_err", worst_b < FIELD_TOL, worst_b)
    R.finish("%d points, formula err %s, exact (%d points) err %s"
             % (m["nn"], fmt(worst_a), nchk, fmt(worst_b)))


def main(argv):
    if len(argv) < 3:
        sys.exit(__doc__)
    mode, cases = argv[1], read_cases(argv[2])
    if mode == "outputs":
        names = []
        for c in cases["cases"]:
            names += [c["inp"]] + ([c["stor"]] if c.get("stor") else [])
            if c.get("src"):
                names.append(c["src"])
        if cases.get("merge"):
            names.append(cases["merge"])
        if cases.get("field"):
            names.append(cases["field"]["dump"])
        print(" ".join(names))
    elif mode == "ids":
        print(" ".join(c["id"] for c in cases["cases"]))
    elif mode == "run":
        row_run(cases, argv[3], argv[4], argv[5])
    elif mode == "deck":
        row_deck(cases)
    elif mode == "case":
        row_case(cases, argv[3], argv[4], argv[5])
    elif mode == "merge":
        row_merge(cases)
    elif mode == "field":
        row_field(cases)
    else:
        sys.exit("unknown mode " + mode)


if __name__ == "__main__":
    main(sys.argv)
