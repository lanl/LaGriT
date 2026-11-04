#!/usr/bin/env python3
"""make_connect_options.py WORKDIR

Writes the decks of test_connect_options (Python 3, standard library
only) and WORKDIR/expect.json, what each deck must produce. Each deck
runs one form of the connect command on one of three small meshes and
ends with  cmo / status / brief  (the checker reads the node and
element counts there) and  finish.

  A  3D, two materials split by a tilted plane (surface / intrface),
     6 x 6 x 6 points: the interface pass adds points unless noadd
  B  3D, 5 x 5 x 5 points, a subset chosen by a point range or a pset
  C  2D (triplane), two materials split by a tilted plane, 7 x 7
     points: the interface pass adds points unless noadd

Counts are compared with other decks of the same mesh (same as, more
than), not with fixed numbers, so the suite does not depend on how
connect breaks ties.
"""
import json
import os
import sys

SETUP = {
"A": """cmo / create / mo
surface / s1 / reflect / box / 0. 0. 0. / 1. 1. 1.
surface / p / intrface / plane / 0. 0. .55 / 1. 0. .55 / 1. 1. .45
region / r1 / le s1 and le p
region / r2 / le s1 and gt p
mregion / m1 / le s1 and lt p
mregion / m2 / le s1 and gt p
createpts / xyz / 6 6 6 / 0. 0. 0. / 1. 1. 1. / 1 1 1
setpts
""",
"B": """cmo / create / mo
createpts / xyz / 5 5 5 / 0. 0. 0. / 1. 1. 1. / 1 1 1
cmo / setatt / mo / imt / 1 0 0 / 1
cmo / setatt / mo / itp / 1 0 0 / 0
pset / p1 / seq / 1 60 1
""",
"C": """cmo / create / mo / / / triplane
surface / s1 / reflect / box / -.1 -.1 -1. / 1.1 1.1 1.
surface / p / intrface / plane / .45 0. 0. / .55 1. 0. / .55 1. 1.
region / r1 / le s1 and le p
region / r2 / le s1 and gt p
mregion / m1 / le s1 and lt p
mregion / m2 / le s1 and gt p
createpts / xyz / 7 7 1 / 0. 0. 0. / 1. 1. 0. / 1 1 1
setpts
""",
}
BIG3 = "-10. -10. -10. / 10. -10. -10. / 0. 10. -10. / 0. 0. 10."
BIG2 = "-5. -5. 0. / 5. -5. 0. / 0. 5. 0."
DEP = "WARNING: connect: the coordinates of the enclosing"
IGN = "tokens after the point range ignored"

# deck: (setup, connect command, expectations, what it tests, V3.3.4)
# expectations: nodes / elements: ("eq", n) | ("gt", n) | ("same", deck)
DECKS = {
"oa01_connect": ("A", "connect", {"nodes": ("gt", 216)},
    "Plain connect: the interface pass adds points where edges cross the tilted plane.",
    "points added (334 nodes)"),
"oa02_noadd": ("A", "connect / noadd", {"nodes": ("eq", 216)},
    "noadd as token 2: no points added.", "same"),
"oa03_delaunay_noadd": ("A", "connect / delaunay / noadd",
    {"nodes": ("eq", 216), "elements": ("same", "oa02_noadd")},
    "noadd after delaunay: the same mesh as oa02.", "same"),
"oa04_range_noadd": ("A", "connect / delaunay / 1 0 0 / noadd",
    {"nodes": ("eq", 216), "elements": ("same", "oa02_noadd")},
    "A point range followed by noadd. V3.3.4 read 12 coordinates of an enclosing tet from the tokens after the range "
    "(noadd and whatever lay past the end of the command) and made no tets.",
    "0 elements"),
"oa05_noadd_first": ("A", "connect / noadd / old_order",
    {"nodes": ("eq", 216)},
    "noadd not the last token. V3.3.4 looked for noadd only in the last token, so it was ignored and points were added.",
    "noadd ignored (334 nodes)"),
"oa06_check_noadd": ("A", "connect / check_interface / noadd",
    {"nodes": ("eq", 216)},
    "check_interface and noadd together; V3.3.4 could take only one of them (the last token).",
    "check_interface ignored"),
"oa07_check": ("A", "connect / check_interface",
    {"nodes": ("gt", 216)},
    "check_interface alone: the interface pass runs, with the extra check of interface-to-interface edges.",
    "same"),
"oa08_bigtet": ("A", "connect / delaunay / 1 0 0 / %s / noadd" % BIG3,
    {"nodes": ("eq", 216), "elements": ("gt", 0), "has": [DEP]},
    "The coordinates of the enclosing tetrahedron given: used, with a WARNING that they are deprecated.",
    "used, no warning"),
"oa09_partial": ("A", "connect / delaunay / 1 0 0 / -10. -10. -10.",
    {"nodes": ("same", "oa01_connect"), "elements": ("same", "oa01_connect"), "has": [IGN]},
    "Three numbers after the point range, not the 12 an enclosing tetrahedron needs: a WARNING, ignored. "
    "V3.3.4 read 12 numbers, 9 of them past the end of the command.",
    "garbage coordinates"),
"ob01_range": ("B", "connect / delaunay / 1 60 1", {"nodes": ("eq", 125), "elements": ("gt", 0)},
    "Points 1 to 60 only.", "same"),
"ob02_range_nodelaunay": ("B", "connect / 1 60 1",
    {"elements": ("same", "ob01_range")},
    "The point range without delaunay in token 2. V3.3.4 took 1 as the algorithm name and read the range from "
    "60 1, and made no tets.", "0 elements"),
"ob03_pset": ("B", "connect / pset get p1",
    {"elements": ("same", "ob01_range")},
    "A pset without delaunay in token 2 (an example in CONNECT1.md). V3.3.4 read pset as the algorithm name and "
    "connected every point.", "all 125 points"),
"ob04_delaunay_pset": ("B", "connect / delaunay / pset get p1",
    {"elements": ("same", "ob01_range")},
    "The same pset after delaunay.", "same"),
"ob05_all": ("B", "connect / delaunay / 1 0 0", {"nodes": ("eq", 125), "elements": ("gt", 0)},
    "1 0 0: every point.", "same"),
"oc01_connect": ("C", "connect", {"nodes": ("gt", 49)},
    "2D: the interface pass adds points where edges cross the plane.", "same (61 nodes)"),
"oc02_noadd": ("C", "connect / noadd", {"nodes": ("eq", 49)},
    "2D noadd. V3.3.4 recognised noadd in 2D only as token 18 and ignored it here.",
    "noadd ignored (61 nodes)"),
"oc03_range_noadd": ("C", "connect / delaunay / 1 0 0 / noadd",
    {"nodes": ("eq", 49), "elements": ("same", "oc02_noadd")},
    "2D: a point range and noadd.", "noadd ignored"),
"oc04_bigtri": ("C", "connect / delaunay / 1 0 0 / %s / noadd" % BIG2,
    {"nodes": ("eq", 49), "elements": ("same", "oc02_noadd"), "has": [DEP]},
    "2D: the 9 coordinates of the enclosing triangle: used, with the deprecation WARNING.",
    "noadd ignored, no warning"),
"oc05_check": ("C", "connect / check_interface",
    {"nodes": ("same", "oc01_connect"), "has": ["WARNING: connect: check_interface is for 3D connect"]},
    "2D: check_interface exists only in 3D: a WARNING, then a plain connect.", "ignored silently"),
"oc06_old_order_noadd": ("C", "connect / old_order / noadd", {"nodes": ("eq", 49)},
    "2D: old_order and noadd together.", "-"),
}


def main(work):
    os.makedirs(work, exist_ok=True)
    exp = {}
    for deck, (s, cmd, e, about, old) in DECKS.items():
        with open(os.path.join(work, deck + ".lgi"), "w") as f:
            f.write("* test_connect_options %s\n" % deck)
            f.write(SETUP[s])
            f.write(cmd + "\n")
            f.write("dump / avs / out_%s.inp / mo / 1 1 1 0\n" % deck)
            f.write("cmo / status / mo / brief\nfinish\n")
        exp[deck] = dict(e, setup=s, cmd=cmd, about=about, old=old)
    json.dump(exp, open(os.path.join(work, "expect.json"), "w"), indent=1)
    print("wrote %d decks to %s" % (len(DECKS), work))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
