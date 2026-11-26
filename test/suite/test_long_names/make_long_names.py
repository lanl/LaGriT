#!/usr/bin/env python3
"""make_long_names.py WORKDIR

Writes the decks of test_long_names (Python 3, standard library only)
and WORKDIR/expect.json, what each deck must produce. The decks are
generated because they hold file names of exact lengths (40, 100, 132
and 140 characters) that are awkward to read in a deck kept in git.

A word token is kept with 32 characters. Since parking lot 4, a
command that takes a file name gets it in full, up to 132 characters;
a longer word that is not taken in full gives, after the command, a
WARNING that names how much of it was used.

Every deck ends with  cmo / status / brief  so the checker sees which
mesh objects exist, and with  finish.

  ln01_dump    a tet mesh dumped with 100-character file names in
               every format that takes one (avs, gmv, lagrit, fehm,
               stor, pflotran, zone, zone_imt, zone_outside, coord,
               tecplot, geofest, dump / control_volume and the short
               form dump / FILE.inp / mo), each file written under its
               full name, nothing under a name cut to 32 characters,
               no WARNING
  ln02_read    the files of a dump read back with read / avs, the
               short form read / FILE.inp and read / lagrit: the same
               vertices and cells
  ln03_stl     a triangle surface dumped with dump / stl to a
               100-character file name
  ln04_infile  infile of a file with a 100-character name: its
               commands run
  ln05_sets    cmo / readatt from a 100-character file; pset / write
               and eltset / write to 100-character names
  ln06_limits  the limits: a 140-character file name is cut to 132
               (WARNING ... uses the first 132); a 40-character mesh
               object name is cut to 32 (WARNING after the command);
               a define keeps 32 characters of its value (WARNING), so
               a file named by a define is cut
"""
import json
import os
import sys


def name(n, tail, tag):
    """a file name of exactly n characters, starting with the deck's
    tag (so the names cut to 32 differ between decks) and ending in
    tail"""
    body = tag + "_" + "abcdefghij" * 20
    s = body[:n - len(tail)] + tail
    assert len(s) == n, (len(s), n)
    return s


def write(work, fname, lines):
    with open(os.path.join(work, fname), "w", newline="\n") as f:
        f.write("\n".join(lines) + "\n")


def tet_mesh(mo):
    return ["cmo / create / %s / / / tet" % mo,
            "createpts / xyz / 3,3,3 / 0. 0. 0. / 1. 1. 1. / 1,1,1",
            "cmo / setatt / %s / imt / 1" % mo,
            "connect",
            "resetpts / itp"]


def main(work):
    E = {}
    end = ["cmo / status / brief", "finish"]
    cut = lambda s: s[:32]

    # ln01: every dump format that takes a file name
    N = {k: name(100, t, "ln01") for k, t in [
        ("avs", ".inp"), ("gmv", ".gmv"), ("lagrit", ".lg"), ("fehm", "_f"),
        ("stor", "_s"), ("pflotran", "_p"), ("zone", "_z"), ("zone_imt", "_zi"),
        ("zone_outside", "_zo"), ("coord", "_c"), ("tecplot", ".plt"),
        ("geofest", ".gf"), ("cv", "_cv.stor"), ("short", "_3t.inp")]}
    dumps = ["dump / %s / %s / mo" % (k, N[k]) for k in
             ("avs", "gmv", "lagrit", "fehm", "stor", "pflotran", "zone",
              "zone_imt", "zone_outside", "coord", "tecplot", "geofest")]
    dumps += ["dump / control_volume / mo / stor, %s" % N["cv"],
              "dump / %s / mo" % N["short"]]
    write(work, "ln01_dump.lgi", tet_mesh("mo") + dumps + end)
    files = [N["avs"], N["gmv"], N["lagrit"], N["fehm"] + ".fehmn",
             N["fehm"] + ".stor", N["fehm"] + "_material.zone",
             N["fehm"] + "_outside.zone", N["stor"] + ".stor",
             N["pflotran"] + ".uge", N["zone"] + "_material.zone",
             N["zone_imt"] + "_material.zone", N["zone_outside"] + "_outside.zone",
             N["coord"] + ".fehmn", N["tecplot"], N["geofest"], N["cv"],
             N["short"]]
    E["ln01_dump"] = {"mo": ["mo"], "no_mo": [], "files": files,
                      "no_files": [cut(f) for f in files] + [cut(f) + ".stor" for f in files],
                      "has": [], "count": {"WARNING: token": 0, "ERROR": 0}}

    # ln02: read back
    A, L = name(100, ".inp", "ln02"), name(100, ".lg", "ln02")
    write(work, "ln02_read.lgi", tet_mesh("mo") +
          ["dump / avs / %s / mo" % A,
           "dump / lagrit / %s / mo / ascii" % L,
           "read / avs / %s / mo_a" % A,
           "read / %s / mo_s" % A,
           "cmo / delete / mo",
           "read / lagrit / %s / mo / ascii" % L,
           "cmo / select / mo_a", "quality",
           "cmo / select / mo_s", "quality"] + end)
    E["ln02_read"] = {"mo": ["mo", "mo_a", "mo_s"], "no_mo": [],
                      "files": [A, L], "no_files": [cut(A), cut(L)],
                      "has": [],
                      "count": {"WARNING: token": 0, "ERROR": 0,
                                "All elements have volume  2.0833333E-02": 2}}

    # ln03: dump / stl of a triangle surface
    S = name(100, ".stl", "ln03")
    write(work, "ln03_stl.lgi", [
        "cmo / create / mo_tri / / / triplane",
        "createpts / xyz / 3,3,1 / 0. 0. 0. / 1. 1. 0. / 1,1,1",
        "cmo / setatt / mo_tri / imt / 1",
        "connect",
        "cmo / setatt / mo_tri / ndimensions_geom / 3",
        "dump / stl / %s / mo_tri" % S] + end)
    E["ln03_stl"] = {"mo": ["mo_tri"], "no_mo": [], "files": [S], "no_files": [cut(S)],
                     "has": [], "count": {"WARNING: token": 0, "ERROR": 0}}

    # ln04: infile of a file with a long name
    I = name(100, ".mlgi", "ln04")
    write(work, I, ["cmo / create / mo_sub / / / tet", "finish"])
    write(work, "ln04_infile.lgi", ["infile / " + I,
                                    "cmo / create / mo_after / / / tet"] + end)
    E["ln04_infile"] = {"mo": ["mo_sub", "mo_after"], "no_mo": [], "files": [], "no_files": [],
                        "has": [], "count": {"WARNING: token": 0, "ERROR": 0}}

    # ln05: cmo / readatt, pset / write, eltset / write
    R, P, Q = name(100, ".dat", "ln05"), name(100, "_p", "ln05"), name(100, "_e", "ln05")
    write(work, R, ["0. 0. 0. 7.", "1. 0. 0. 8.", "0. 1. 0. 9."])
    write(work, "ln05_sets.lgi", tet_mesh("mo") + [
        "cmo / create / mo_pts / / / tet",
        "cmo / addatt / mo_pts / val / vdouble / scalar / nnodes",
        "cmo / readatt / mo_pts / xic,yic,zic,val / 1,0,0 / " + R,
        "cmo / printatt / mo_pts / val / minmax",
        "cmo / select / mo",
        "pset / p_low / attribute / zic / 1,0,0 / lt 0.4",
        "pset / p_low / write / %s / ascii" % P,
        "eltset / e_all / itetclr / eq 1",
        "eltset / e_all / write / %s / ascii" % Q] + end)
    E["ln05_sets"] = {"mo": ["mo", "mo_pts"], "no_mo": [],
                      "files": [P + ".vertexset", Q + ".cellset"],
                      "no_files": [cut(P) + ".vertexset", cut(Q) + ".cellset"],
                      "has": [" val                7.000000000E+00  9.000000000E+00"],
                      "count": {"WARNING: token": 0}}

    # ln06: the limits
    F140 = name(140, ".inp", "ln06")
    mo40 = "mo_" + "abcdefghij" * 3 + "klmnopq"     # 40 characters
    D50 = name(50, ".inp", "ln06")
    write(work, "ln06_limits.lgi", tet_mesh("mo") + [
        "dump / avs / %s / mo" % F140,
        "cmo / create / %s / / / tet" % mo40,
        "define / DFILE / " + D50,
        "dump / avs / DFILE / mo"] + end)
    E["ln06_limits"] = {"mo": ["mo", mo40[:32]], "no_mo": [],
                        "files": [F140[:132], D50[:32]], "no_files": [F140, D50],
                        "has": ["WARNING: token 3 has 140 characters; LaGriT uses the first 132:",
                                "WARNING: token 3 has 40 characters; LaGriT uses the first 32:",
                                '  "' + mo40[:32] + '"',
                                "WARNING: token 3 has 50 characters; LaGriT uses the first 32:"],
                        "count": {"WARNING: token": 3},
                        "order": [["cmo/create/" + mo40, "WARNING: token 3 has 40"]]}

    json.dump(E, open(os.path.join(work, "expect.json"), "w"), indent=1)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
