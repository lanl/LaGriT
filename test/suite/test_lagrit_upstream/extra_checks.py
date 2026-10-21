#!/usr/bin/env python3
"""extra_checks.py: checks of our own for some LaGriT tests, beyond
the log and the output files (test_lagrit_upstream). Python 3, standard
library only.

CHECKS[test](work_dir) -> (ok, [lines of explanation]). A failed check
makes the test FAIL. User, Oct 17 2026.

pset   "Can you determine if the pset IO is correct": every vertex set
       file the deck writes is read back (ASCII, and the binary Fortran
       records of 'pset / pset1 / write / out_pset04 / binary') and its
       vertices compared with the sets' definitions in the deck, applied
       to the mesh's own vertices (output_mesh_psets.inp):
         pset1 = geom / rtp / 0,0,0 / 5,360,180: the vertices within
                 radius 5 of the origin
         pset2 = geom / xyz / 7,7,7 / 10,10,10: x, y and z from 7 to 10
       and the file names the deck tests: .vertexset added only when it
       is not there; -def-, -all- and a blank name write every set, one
       file each (<name>_pset1, <name>_pset2).
"""
import os
import struct

TOL = 1e-9


def read_vertices(path):
    L = open(path).read().split("\n")
    nn = int(L[0].split()[0])
    return [tuple(float(x) for x in l.split()[1:4]) for l in L[1:1 + nn]]


def read_vertexset(path):
    """{set name: [vertex numbers]} from an ASCII or binary file"""
    b = open(path, "rb").read()
    if b.startswith(b"pset ascii"):
        w = b.decode().split()
        k, out = 3, {}
        while k < len(w):
            name, count = w[k], int(w[k + 2])
            out[name] = [int(x) for x in w[k + 3:k + 3 + count]]
            k += 3 + count
        return out
    # Fortran unformatted: [4-byte length][payload][4-byte length]
    recs, k = [], 0
    while k + 4 <= len(b):
        n = struct.unpack("<i", b[k:k + 4])[0]
        recs.append(b[k + 4:k + 4 + n])
        k += 8 + n
    if not recs or not recs[0].startswith(b"pset unformatted"):
        raise ValueError("not a vertex set file")
    out, r = {}, 1
    while r + 1 < len(recs):
        name = recs[r][:32].decode().strip()
        count = struct.unpack("<q", recs[r][40:48])[0]
        ids = list(struct.unpack("<%dq" % count, recs[r + 1][:8 * count]))
        out[name] = ids
        r += 2
    return out


def check_pset(work):
    lines = []
    V = read_vertices(os.path.join(work, "output_mesh_psets.inp"))
    want = {
        "pset1": [i + 1 for i, p in enumerate(V)
                  if (p[0] ** 2 + p[1] ** 2 + p[2] ** 2) ** 0.5 <= 5 + TOL],
        "pset2": [i + 1 for i, p in enumerate(V)
                  if all(7 - TOL <= c <= 10 + TOL for c in p)]}
    lines.append("expected from the deck's definitions on the mesh's %d "
                 "vertices: pset1 %d vertices (r <= 5), pset2 %d (7 to 10)"
                 % (len(V), len(want["pset1"]), len(want["pset2"])))
    files = {"out_pset01.vertexset": ["pset1"],
             "out_pset02.vertexset": ["pset2"],
             "out_pset03.vertexset": ["pset1"],
             "out_pset04.vertexset": ["pset1"],      # binary
             "out_pset05.vertexset": ["pset1"],
             "out_pset06.vertexset": ["pset1"],
             "out_pset07.vertexset": ["pset1"]}
    for base in ("out_pset08", "out_pset09", "out_pset10"):
        for s in ("pset1", "pset2"):
            files["%s_%s.vertexset" % (base, s)] = [s]
    ok = True
    for f, sets in sorted(files.items()):
        path = os.path.join(work, f)
        if not os.path.isfile(path):
            lines.append("%s: MISSING" % f)
            ok = False
            continue
        try:
            got = read_vertexset(path)
        except (ValueError, IndexError, struct.error) as e:
            lines.append("%s: unreadable (%s)" % (f, e))
            ok = False
            continue
        for s in sets:
            if s not in got:
                lines.append("%s: no set %s (has %s)" % (f, s, ", ".join(got)))
                ok = False
            elif sorted(got[s]) != want[s]:
                extra = set(got[s]) - set(want[s])
                miss = set(want[s]) - set(got[s])
                lines.append("%s: %s WRONG: %d extra, %d missing vertices"
                             % (f, s, len(extra), len(miss)))
                ok = False
            else:
                lines.append("%s: %s correct (%d vertices%s)"
                             % (f, s, len(got[s]),
                                ", binary" if f == "out_pset04.vertexset" else ""))
    for bad in ("out_pset02.vertexset.vertexset", "out_pset08.vertexset",
                "out_pset09.vertexset", "out_pset10.vertexset"):
        if os.path.exists(os.path.join(work, bad)):
            lines.append("%s written: wrong name" % bad)
            ok = False
    return ok, lines


CHECKS = {"pset": check_pset}
