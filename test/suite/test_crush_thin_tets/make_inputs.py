#!/usr/bin/env python3
"""make_inputs.py DIR

Writes the planted test meshes of test_crush_thin_tets (Python 3,
standard library only; deterministic).

Every mesh starts as the same regular lattice: 6 x 6 x 6 vertices on the
box [0,5]^3, each unit cube cut into 6 tets (750 tets, all relative
width 0.41). One vertex is then moved so that one tet it belongs to
becomes the chosen kind of thin tet, and every tet stays valid (positive
volume):

  NEEDLE  the vertex moved close to another vertex of the tet
  WEDGE   the vertex moved close to an edge of the tet it is not on
  CAP     the vertex moved close to the middle of the opposite face
  SLIVER  the vertex moved so that one edge of the tet nearly crosses
          the opposite edge (all four vertices nearly in one plane)

Each kind is made twice: INTERIOR (the vertex at (2,2,2), every vertex
of the thin tet inside the box) and BOUNDARY (the vertex at (2,2,0) on
the bottom face, moved only within that face, so it stays a boundary
vertex; for CAP, the vertex at (2,2,1) moved down over the middle of a
face lying in the bottom boundary). The placement is searched: of the candidate positions that keep
every tet valid and give the target tet the chosen kind at relative
width <= 0.06, the one that makes the fewest other thin tets is used.

Also written:
  jitter.inp  the lattice with every vertex moved at random by up to
              0.4 (boundary vertices only within their faces; seed 7),
              keeping every tet valid: many thin tets of every kind.

planted.txt lists each mesh: kind, place, the moved vertex, the thin tet
(1-based vertices) and its relative width.
"""
import math
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import crushlib as cl  # noqa: E402

N = 6


def lattice():
    idx = lambda i, j, k: i + N * (j + N * k)
    V = [(float(i), float(j), float(k)) for k in range(N) for j in range(N) for i in range(N)]
    T = []
    for k in range(N - 1):
        for j in range(N - 1):
            for i in range(N - 1):
                c = [idx(i + a, j + b, k + e) for e in (0, 1) for b in (0, 1) for a in (0, 1)]
                for p1, p2 in ((1, 3), (3, 2), (2, 6), (6, 4), (4, 5), (5, 1)):
                    T.append([c[0], c[p1], c[p2], c[7]])
    T = [t if cl.det6([V[q] for q in t]) < 0 else [t[0], t[2], t[1], t[3]] for t in T]
    return V, T, idx


def star(T, p):
    return [t for t, c in enumerate(T) if p in c]


def try_position(V, T, st, p, X):
    """relwidths of p's star with p at X (None if any tet inverts)"""
    old = V[p]
    V[p] = X
    rw = [cl.relwidth([V[q] for q in T[t]]) for t in st]
    V[p] = old
    return None if min(rw) <= 0 else rw


def lerp(a, b, f):
    return tuple(a[m] + f * (b[m] - a[m]) for m in range(3))


def candidates(V, T, t, p, kind):
    """target positions for vertex p that make tet t the given kind"""
    c = T[t]
    others = [q for q in c if q != p]
    P0 = V[p]
    out = []
    fs = (0.02, 0.03, 0.04, 0.06, 0.08)
    if kind == "NEEDLE":
        for q in others:
            for f in fs:
                out.append(lerp(V[q], P0, f))
    elif kind == "WEDGE":
        for a in others:
            for b in others:
                if a >= b:
                    continue
                w = cl.sub(V[b], V[a])
                lam = cl.dot(cl.sub(P0, V[a]), w) / cl.dot(w, w)
                for lam2 in (0.3, 0.5, 0.7):
                    F = lerp(V[a], V[b], lam2)
                    for f in fs:
                        out.append(lerp(F, P0, f))
    elif kind == "CAP":
        a, b, cc = others
        for wts in ((1 / 3, 1 / 3, 1 / 3), (0.5, 0.25, 0.25), (0.25, 0.5, 0.25), (0.25, 0.25, 0.5)):
            G = tuple(wts[0] * V[a][m] + wts[1] * V[b][m] + wts[2] * V[cc][m] for m in range(3))
            for f in fs:
                out.append(lerp(G, P0, f))
    elif kind == "SLIVER":
        for a in others:
            b, cc = [q for q in others if q != a]
            m = lerp(V[b], V[cc], 0.5)
            n = cl.cross(cl.sub(V[b], V[a]), cl.sub(V[cc], V[a]))
            nn = cl.norm(n)
            n = tuple(x / nn for x in n)
            side = 1.0 if cl.dot(cl.sub(P0, V[a]), n) > 0 else -1.0
            for beyond in (0.2, 0.35, 0.5):
                base = tuple(m[k] + beyond * (m[k] - V[a][k]) for k in range(3))
                for f in (0.03, 0.05, 0.08):
                    out.append(tuple(base[k] + side * f * n[k] for k in range(3)))
    return out


def plant(kind, place):
    V, T, idx = lattice()
    # a BOUNDARY CAP is a vertex just inside the box over the middle of
    # a boundary face (a vertex cannot reach a face by sliding within its
    # own boundary face)
    bcap = kind == "CAP" and place == "BOUNDARY"
    p = idx(2, 2, 2) if place == "INTERIOR" else (idx(2, 2, 1) if bcap else idx(2, 2, 0))
    st = star(T, p)
    best = None
    for t in st:
        if bcap and not all(V[q][2] == 0.0 for q in T[t] if q != p):
            continue
        for X in candidates(V, T, t, p, kind):
            if place == "BOUNDARY" and not bcap:
                X = (X[0], X[1], 0.0)               # stay in the bottom face
            rw = try_position(V, T, st, p, X)
            if rw is None:
                continue
            V2 = list(V)
            V2[p] = X
            P = [V2[q] for q in T[t]]
            r = cl.relwidth(P)
            if r > 0.06 or cl.classify(P, 0.1) != kind:
                continue
            nthin = sum(1 for x in rw if x < 0.1)
            key = (nthin, -r)                       # fewest other thin tets
            if best is None or key < best[0]:
                best = (key, t, X, r)
    if best is None:
        return None
    _, t, X, r = best
    V[p] = X
    return V, T, p, t, r


def jitter(seed=7, amp=0.4):
    V, T, idx = lattice()
    r = random.Random(seed)
    stars = [[] for _ in V]
    for t, c in enumerate(T):
        for q in c:
            stars[q].append(t)
    for i in range(N):
        for j in range(N):
            for k in range(N):
                p = idx(i, j, k)
                for _ in range(20):
                    d = [r.uniform(-amp, amp) for _ in range(3)]
                    for m, ijk in enumerate((i, j, k)):
                        if ijk in (0, N - 1):
                            d[m] = 0.0            # stay in the boundary face
                    X = tuple(V[p][m] + d[m] for m in range(3))
                    if try_position(V, T, stars[p], p, X) is not None:
                        V[p] = X
                        break
    return V, T


def main(d):
    lines = ["# mesh kind place moved_vertex thin_tet relwidth"]
    for place in ("INTERIOR", "BOUNDARY"):
        for kind in cl.TYPES:
            res = plant(kind, place)
            name = "%s_%s" % (kind.lower(), place.lower())
            if res is None:
                lines.append("%s %s %s none none none" % (name, kind, place))
                continue
            V, T, p, t, r = res
            cl.write_avs(os.path.join(d, name + ".inp"), V, T)
            lines.append("%s %s %s %d %s %.4f" % (name, kind, place, p + 1,
                                                 ",".join(str(q + 1) for q in T[t]), r))
    V, T = jitter()
    cl.write_avs(os.path.join(d, "jitter.inp"), V, T)
    open(os.path.join(d, "planted.txt"), "w").write("\n".join(lines) + "\n")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else ".")
