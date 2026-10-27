#!/usr/bin/env python3
"""check_crush.py DECK WORKDIR LOGDIR

Judges one test_crush_thin_tets deck and measures what crush_thin_tets
did (Python 3, standard library only). Prints "key value" lines (the
.check file) and writes LOGDIR/<tag>.json for the page.

HARD checks (decide PASS / FAIL):
  finished       LaGriT printed "LaGriT successfully completed"
  clean          no "Illegal option" message (a command LaGriT could not
                 parse, e.g. a line longer than 80 columns cut short)
  valid          every tet after crush has positive volume
  volume         total volume unchanged (relative 1e-9)
  expectation    "# expect: removed": no tet left below TOLCRUSH
  pset           "# pset: z < 0.5" (the deck crushes with that pset):
                 every tet with no vertex in the pset is left as it was

MEASURED (reported, not judged):
  vertices, tets; relative width (crush's own measure: smallest altitude
  / sqrt(sum of squared edges / 9), 1 regular, 0 flat): minimum and the
  number of tets below 0.05, 0.1, 0.2, 0.3; LaGriT's aspect ratio
  (aratio) and dihedral angles (ang_mind, ang_maxd) minimum / maximum.

  Carl Gable's classes (ex_id_tet_types, from LaGriT's own attributes,
  computed by ct_measure.mlgi):
    SLIVER  ang_mind <= 10 and ang_maxd >= 170 and s_maxd <= 10
    CAP     s_maxd >= 170
    NEEDLE  eratio <= 0.1 and not ang_mind <= 10
    WEDGE   eratio <= 0.1 and ang_mind <= 10
    poor    ang_mind <= 10 or ang_maxd >= 170 (any of the above or
            other bad angles)
  A tet can be in more than one class.

  crush's view: each tet below TOLCRUSH classified in crush_thin_tets'
  own order (crushlib.classify): NEEDLE, WEDGE, CAP, SLIVER, UNMATCHED.

  trips: from LaGriT's output, for each crush_thin_tets call, the
  quality report crush prints at the start of every trip and at the end
  (aspect ratio counts) and the merges / edge refinements of each trip.

  survivors: every tet still below TOLCRUSH after crush, with what
  crush_thin_tets would do with it now and why not (crushlib.Mesh.replay,
  the same decisions as src/crush_thin_tets.f).
"""
import json
import math
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import crushlib as cl  # noqa: E402

TOLV = 1e-9
BINS = (0.05, 0.1, 0.2, 0.3)


def read_cells(path):
    """vertices, tets (0-based), vertex atts, cell atts"""
    L = open(path).read().split("\n")
    h = [int(x) for x in L[0].split()[:4]]
    nn, nc, nnd, ncd = h
    V = [tuple(map(float, l.split()[1:4])) for l in L[1:1 + nn]]
    C = [[int(x) - 1 for x in l.split()[3:]] for l in L[1 + nn:1 + nn + nc]]
    k = 1 + nn + nc
    va, ca = {}, {}
    if nnd:
        na = int(L[k].split()[0])
        names = [L[k + 1 + i].split(",")[0].strip() for i in range(na)]
        rows = [l.split() for l in L[k + 1 + na:k + 1 + na + nn]]
        for j, n in enumerate(names):
            va[n] = [float(r[1 + j]) for r in rows]
        k += 1 + na + nn
    if ncd:
        na = int(L[k].split()[0])
        names = [L[k + 1 + i].split(",")[0].strip() for i in range(na)]
        rows = [l.split() for l in L[k + 1 + na:k + 1 + na + nc]]
        for j, n in enumerate(names):
            ca[n] = [float(r[1 + j]) for r in rows]
    return V, C, va, ca


def carl_classes(ca):
    n = len(ca["ang_mind"])
    out = {"SLIVER": 0, "CAP": 0, "NEEDLE": 0, "WEDGE": 0, "poor": 0}
    for i in range(n):
        mind, maxd, smax, er = ca["ang_mind"][i], ca["ang_maxd"][i], ca["s_maxd"][i], ca["eratio"][i]
        small, big = mind <= 10.0, maxd >= 170.0
        if small and big and smax <= 10.0:
            out["SLIVER"] += 1
        if smax >= 170.0:
            out["CAP"] += 1
        if er <= 0.1 and not small:
            out["NEEDLE"] += 1
        if er <= 0.1 and small:
            out["WEDGE"] += 1
        if small or big:
            out["poor"] += 1
    return out


def measure(base, work, tol_list):
    """measurements of one written mesh (base = ctNN_before etc.)"""
    V, C, va, _ = read_cells(os.path.join(work, base + ".inp"))
    _, _, _, ca = read_cells(os.path.join(work, base + "_m.inp"))
    P = [[V[k] for k in c] for c in C]
    rw = [cl.relwidth(p) for p in P]
    m = {"vertices": len(V), "tets": len(C),
         "volume": sum(cl.volume(p) for p in P),
         "min_relwidth": min(rw),
         "below": {str(b): sum(1 for r in rw if r < b) for b in BINS},
         "min_aspect": min(ca["aratio"]), "min_dihedral": min(ca["ang_mind"]),
         "max_dihedral": max(ca["ang_maxd"]),
         "carl": carl_classes(ca),
         "crush_view": {}}
    for t in tol_list:
        kinds = {k: 0 for k in cl.KINDS}
        for p, r in zip(P, rw):
            if r < t:
                kinds[cl.classify(p, t)] += 1
        m["crush_view"][str(t)] = kinds
    hist = [0] * 21                      # relwidth in steps of 0.025 to 0.5+
    for r in rw:
        hist[min(20, max(0, int(r / 0.025)))] += 1
    m["hist"] = hist
    # Carl-poor tets that crush never looks at (relative width above the
    # largest TOLCRUSH used)
    m["angle_poor_not_candidate"] = sum(
        1 for i, r in enumerate(rw)
        if (ca["ang_mind"][i] <= 10 or ca["ang_maxd"][i] >= 170) and r >= max(tol_list))
    return m, V, C, va, rw


def parse_trips(out_text):
    """per crush_thin_tets call: list of trip records"""
    calls = []
    cur = None
    q = None
    for line in out_text.split("\n"):
        s = line.strip()
        if s.lower().startswith("crush_thin_tets/") or s.lower().startswith("crush_thin_tets /"):
            cur = {"trips": [], "processed": None}
            calls.append(cur)
            continue
        if cur is None:
            continue
        if s.startswith("elements with aspect ratio"):
            if q is None:
                q = {"aspect_bins": [], "merged": 0, "edge_tets": None}
                cur["trips"].append(q)
            q["aspect_bins"].append(int(s.split()[-1]))
        elif s.startswith("min aspect ratio"):
            q["min_aspect"] = float(s.split()[4])
        elif s.endswith("total elements evaluated.") and q is not None:
            q["elements"] = int(s.split()[0])
            q = None
        elif s.startswith("Merging") and cur["trips"]:
            cur["trips"][-1]["merged"] = int(s.split()[1])
        elif s.startswith("Edge-refined tets:") and cur["trips"]:
            cur["trips"][-1]["edge_tets"] = s
        elif s.startswith("crush_thin_tets processed:"):
            mm = re.search(r"processed:\s+(\d+)\s+tets in\s+(\d+)\s+trips", s)
            if mm:
                cur["processed"] = int(mm.group(1))
                cur["ntrips"] = int(mm.group(2))
            cur = None
    for c in calls:
        for t in c["trips"]:
            b = t["aspect_bins"]
            t["aspect_below_0.1"] = sum(b[:4]) if len(b) >= 4 else None
            t["aspect_below_0.2"] = sum(b[:5]) if len(b) >= 5 else None
    return calls


def reason_category(action, why):
    if action:
        return "crush would act now (no trips left, or a neighbour was changed first)"
    w = " ".join(why)
    if "switched off" in w and all(("CAP" in x) for x in why):
        return "CAP: the CAP case is switched off"
    if "CAP:" in w and "not 1% better" in w and all(("CAP" in x) for x in why):
        return "CAP: merging it with its face would leave a neighbour no better (not 1% better)"
    if "no case matched" in w:
        return "matched no case (distances not below TOLCRUSH, or feet outside)"
    if "not allowed by point types" in w or "neither end may merge" in w:
        if "not 1% better" in w:
            return "boundary/material types forbid some merges; the rest not 1% better"
        return "boundary/material point types forbid the merge"
    if "not 1% better" in w:
        if "switched off" in w:
            return "not 1% better (and CAP case switched off)"
        return "every possible collapse leaves a neighbour no better (not 1% better)"
    return "other"


def pic_box(V, C, rw, lim):
    """(box, all_in_box): the region the pictures show. When the thin
    tets are few and close together (a planted tet), the box around them
    plus one median edge length on every side, and every tet in it is
    drawn (the local mesh before and after); otherwise the whole mesh,
    thin tets only."""
    thin = [i for i, r in enumerate(rw) if r < lim]
    xs = [p[0] for p in V]
    ys = [p[1] for p in V]
    zs = [p[2] for p in V]
    full = (min(xs), min(ys), min(zs), max(xs), max(ys), max(zs))
    if not thin or len(thin) > 40:
        return full, False
    pts = [V[k] for i in thin for k in C[i]]
    edges = sorted(cl.norm(cl.sub(V[C[i][a]], V[C[i][b]])) for i in thin for a, b in cl.EDGES)
    m = edges[len(edges) // 2]
    box = tuple(min(p[k] for p in pts) - m for k in range(3)) + tuple(max(p[k] for p in pts) + m for k in range(3))
    box = tuple(max(box[k], full[k]) for k in range(3)) + tuple(min(box[3 + k], full[3 + k]) for k in range(3))
    return box, True


def subset_avs(path, V, C, rw, lim, box, all_in_box=False):
    """the tets below lim (or, all_in_box, every tet with its centroid in
    the box), with relwidth as a cell attribute, plus the box corners as
    unused vertices (one frame for before and after)"""
    if all_in_box:
        def inside(c):
            g = [sum(V[k][m] for k in c) / 4.0 for m in range(3)]
            return all(box[m] <= g[m] <= box[3 + m] for m in range(3))
        keep = [i for i, c in enumerate(C) if inside(c)]
    else:
        keep = [i for i, r in enumerate(rw) if r < lim]
    used = sorted({k for i in keep for k in C[i]})
    ren = {k: j for j, k in enumerate(used)}
    W = [V[k] for k in used]
    corners = [(x, y, z) for x in (box[0], box[3]) for y in (box[1], box[4]) for z in (box[2], box[5])]
    W += corners
    with open(path, "w") as f:
        f.write("%d %d 0 1 0\n" % (len(W), len(keep)))
        for i, p in enumerate(W):
            f.write("%d %.10g %.10g %.10g\n" % (i + 1, p[0], p[1], p[2]))
        for j, i in enumerate(keep):
            f.write("%d 1 tet %s\n" % (j + 1, " ".join(str(ren[k] + 1) for k in C[i])))
        f.write("1 1\nrelwidth, real\n")
        for j, i in enumerate(keep):
            f.write("%d %.6g\n" % (j + 1, rw[i]))
    return len(keep)


def main(deck, work, logdir):
    name = os.path.basename(deck)[:-4]
    spec = expect = pset = None
    cont = False
    for l in open(deck):
        if l.startswith("# pset:"):
            ax, op, val = l.split(":", 1)[1].split()
            assert op == "<", "only '<' is supported in # pset:"
            pset = ("xyz".index(ax), float(val))
        if l.startswith("# check:"):
            spec = [s.strip() for s in l.split(":", 1)[1].split("|")]
        if cont and l.startswith("#   "):
            expect += " " + l[4:].strip()
            continue
        cont = False
        if l.startswith("# expect:"):
            expect = l.split(":", 1)[1].strip()
            cont = True
    tag = spec[1]
    tols = [float(t) for t in spec[3].split(",")]
    out_path = os.path.join(work, name + ".out")
    text = open(out_path, errors="replace").read() if os.path.isfile(out_path) else ""
    finished = "LaGriT successfully completed" in text
    res = {"deck": name, "tag": tag, "expect": expect, "tols": tols, "finished": finished}
    print("finished %s" % finished)
    try:
        before, Vb, Cb, vab, rwb = measure(tag + "_before", work, tols)
    except (OSError, KeyError, ValueError, IndexError) as e:
        print("result FAIL\nwhy before mesh not readable (%s)" % e)
        json.dump(res, open(os.path.join(logdir, tag + ".json"), "w"))
        return 1
    res["before"] = before
    lim = max(0.3, max(tols))
    box, local = pic_box(Vb, Cb, rwb, lim)
    res["pic_local"] = local
    res["pic_before"] = subset_avs(os.path.join(work, tag + "_pic_before.inp"), Vb, Cb, rwb, lim, box)
    calls = parse_trips(text)
    res["after"] = {}
    ok = finished
    why_fail = []
    if not finished:
        why_fail.append("LaGriT did not finish")
    nill = text.count("Illegal option")
    res["illegal"] = nill
    print("illegal_option_messages %d" % nill)
    if nill:
        ok = False
        why_fail.append("LaGriT printed %d 'Illegal option' messages" % nill)
    for i, t in enumerate(tols):
        base = "%s_after_%s" % (tag, spec[3].split(",")[i])
        try:
            a, Va, Ca, vaa, rwa = measure(base, work, tols)
        except (OSError, KeyError, ValueError, IndexError) as e:
            ok = False
            why_fail.append("%s not readable (%s)" % (base, e))
            continue
        a["trips"] = calls[i] if i < len(calls) else None
        a["valid"] = a["min_relwidth"] > 0
        a["volume_ok"] = abs(a["volume"] - before["volume"]) <= TOLV * abs(before["volume"])
        if not a["valid"]:
            ok = False
            why_fail.append("TOLCRUSH %g: an inverted or flat tet (min relwidth %.3g)" % (t, a["min_relwidth"]))
        if not a["volume_ok"]:
            ok = False
            why_fail.append("TOLCRUSH %g: volume %.12g, was %.12g" % (t, a["volume"], before["volume"]))
        # survivors and why
        mesh = cl.Mesh(Va, Ca, vaa.get("itp1", [0] * len(Va)))
        # Which scale did this LaGriT use? Before the Oct 2026 fix every
        # tet was measured against the scale length of mesh vertices 1-4
        # (taken from the mesh before crush: crush never moves them).
        # crush stops only when a trip schedules nothing, so if it stopped
        # early and the tet-scale replay says it would still act, this
        # LaGriT has the old scale.
        old_scale = cl.Mesh(Vb, Cb, vab.get("itp1", [0] * len(Vb))).scale_v1234()
        tr = calls[i] if i < len(calls) else {}
        stopped_early = tr.get("ntrips", 10) < 10
        # Which crush_thin_tets is this? crush stops only when a trip does
        # nothing, so when it stopped early the right model is one under
        # which it would do nothing more. Newest first.
        models = [("tet scale, CAP case on, tolerance ladder (Oct 2026)", None, True, True),
                  ("tet scale, CAP case on, no ladder", None, True, False),
                  ("tet scale, CAP case off, no ladder", None, False, False),
                  ("scale of vertices 1-4, CAP case off (before Oct 2026)", old_scale, False, False)]
        # With a pset crush works only on tets whose vertices are all in
        # it, or were added by crush (not in the mesh before).
        if pset:
            old_pts = set(tuple(round(c, 9) for c in p) for p in Vb)
            inside = [tuple(round(c, 9) for c in p) not in old_pts or p[pset[0]] < pset[1]
                      for p in Va]
            def processed(k):
                return all(inside[q] for q in Ca[k])
            # every tet with no vertex in the pset is left as it was
            def key(V, c):
                return tuple(sorted(tuple(round(x, 9) for x in V[q]) for q in c))
            after_set = set(key(Va, c) for c in Ca)
            outside_b = [c for c in Cb if all(Vb[q][pset[0]] >= pset[1] for q in c)]
            moved = sum(1 for c in outside_b if key(Vb, c) not in after_set)
            a["pset_outside"] = len(outside_b)
            a["pset_changed"] = moved
            print("tol_%g_pset_outside_tets %d changed %d" % (t, len(outside_b), moved))
            if moved:
                ok = False
                why_fail.append("TOLCRUSH %g: %d tets with no vertex in the pset changed"
                                % (t, moved))
        else:
            def processed(k):
                return True
        thin = [k for k, r in enumerate(rwa) if r < t and processed(k)]
        chosen = models[0]
        if stopped_early:
            for mdl in models:
                if not any(mesh.replay(k, t, mdl[1], mdl[2], mdl[3])[0] for k in thin):
                    chosen = mdl
                    break
        a["scale_model"] = chosen[0]
        sc, capon, lad = chosen[1], chosen[2], chosen[3]
        a["old_scale"] = old_scale
        surv = []
        cats = {}
        for k, r in enumerate(rwa):
            if r < t:
                if processed(k):
                    act, why = mesh.replay(k, t, sc, capon, lad)
                    cat = reason_category(act, why)
                else:
                    act, why = None, ["a vertex is not in the pset"]
                    cat = "outside the pset (not processed)"
                cats[cat] = cats.get(cat, 0) + 1
                nb = sum(1 for q in Ca[k] if mesh.itp[q] >= 10)
                surv.append({"tet": k + 1, "relwidth": r,
                             "kind": cl.classify([Va[q] for q in Ca[k]], t,
                                             cl.LADDER if lad else cl.NO_LADDER),
                             "boundary_vertices": nb, "action": act, "why": why, "category": cat})
        surv.sort(key=lambda s: s["relwidth"])
        a["survivors"] = surv[:60]
        a["n_survivors"] = len(surv)
        a["reasons"] = cats
        a["pic"] = subset_avs(os.path.join(work, base + "_pic.inp"), Va, Ca, rwa, lim, box)
        res["after"][str(t)] = a
        print("tol_%g_tets %d" % (t, a["tets"]))
        print("tol_%g_min_relwidth %.4f" % (t, a["min_relwidth"]))
        print("tol_%g_survivors %d" % (t, len(surv)))
        print("tol_%g_scale_model %s" % (t, a["scale_model"]))
        for c, n in sorted(cats.items(), key=lambda q: -q[1]):
            print("tol_%g_reason %d %s" % (t, n, c))
        if expect == "removed" and surv:
            ok = False
            why_fail.append("expected every tet below TOLCRUSH %g removed; %d remain" % (t, len(surv)))
    print("before_min_relwidth %.4f" % before["min_relwidth"])
    print("before_below_tol %s" % before["below"])
    result = "PASS" if ok else "FAIL"
    res["result"] = result
    res["why"] = why_fail
    json.dump(res, open(os.path.join(logdir, tag + ".json"), "w"), indent=1)
    print("result %s" % result)
    if why_fail:
        print("why %s" % "; ".join(why_fail))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2], sys.argv[3]))
