#!/usr/bin/env python3
"""make_cmo_commands.py WORKDIR

Writes the decks of test_cmo_commands (Python 3, standard library only)
and WORKDIR/expect.json, what each deck must produce: texts that must
appear ("has"), texts that must not ("lacks"), and texts counted
("count"), all in LaGriT's output.

Every deck ends with  finish.

  cc01_delatt_all   cmo / delatt / mo / -all- deletes the temporary
                    attributes (two added with cmo / addatt, which
                    makes temporary ones by default) and keeps a
                    permanent one; cmo / DELATT / mo / -all- then
                    deletes the permanent one too and keeps the
                    standard attributes (GitHub #163, #108: both were
                    "Attribute does not exist: -all-")
  cc02_status_gone  cmo / status of a deleted mesh object names that
                    mesh object (GitHub #159: it named the current one)
  cc03_math_imt     math with imt (not imt1) as its source: the values
                    of imt1 (GitHub #31: a segmentation fault); an
                    unknown attribute: an ERROR, LaGriT goes on
  cc04_setatt_word  cmo / setatt with an undefined name for the value:
                    an ERROR, the attribute unchanged (GitHub #55: set
                    to garbage, 6.95e-310); a defined name is used
"""
import json
import os
import sys


def write(work, name, lines):
    with open(os.path.join(work, name), "w", newline="\n") as f:
        f.write("\n".join(lines) + "\n")


PTS = "createpts / xyz / 2,2,2 / 0. 0. 0. / 1. 1. 1. / 1,1,1"
ROW = " %s VDOU"                         # an attribute row in cmo / status


def main(work):
    E = {}

    write(work, "cc01_delatt_all.lgi", [
        "cmo / create / mo / / / tet", PTS,
        "cmo / addatt / mo / t_one / vdouble / scalar / nnodes",
        "cmo / addatt / mo / t_two / vdouble / scalar / nnodes",
        "cmo / addatt / mo / p_one / vdouble / scalar / nnodes / linear / permanent",
        "cmo / delatt / mo / -all-",
        "cmo / status / mo",
        "cmo / DELATT / mo / -all-",
        "cmo / status / mo",
        "cmo / printatt / mo / imt1 / minmax",
        "finish"])
    E["cc01_delatt_all"] = {
        "has": ["delatt -all- mo: deleted 2 temporary attributes",
                "DELATT -all- mo: deleted 1 attributes",
                " imt1                             0                0"],
        "lacks": ["does not exist: -all-", "Segmentation fault"],
        "count": {ROW % "t_one": 0, ROW % "t_two": 0, ROW % "p_one": 1}}

    write(work, "cc02_status_gone.lgi", [
        "cmo / create / mo1 / / / tet",
        "cmo / create / mo2 / / / tet",
        "cmo / delete / mo1",
        "cmo / status / mo1",
        "finish"])
    E["cc02_status_gone"] = {
        "has": ["Mesh Object does not exist: mo1"],
        "lacks": ["does not exist: mo2"], "count": {}}

    write(work, "cc03_math_imt.lgi", [
        "cmo / create / mo / / / tet", PTS,
        "cmo / setatt / mo / imt / 2",
        "cmo / addatt / mo / b / vint / scalar / nnodes",
        "math / add / mo / b / 1,0,0 / mo / imt / 3",
        "cmo / printatt / mo / b / minmax",
        "math / add / mo / b / 1,0,0 / mo / no_such_att / 3",
        "cmo / printatt / mo / b / minmax",
        "finish"])
    E["cc03_math_imt"] = {
        "has": ["ERROR math: no attribute no_such_att in mo"],
        "lacks": ["Segmentation fault"],
        "count": {" b                                5                5": 2}}

    write(work, "cc04_setatt_word.lgi", [
        "cmo / create / mo / / / tet", PTS,
        "cmo / addatt / mo / a / vdouble / scalar / nnodes",
        "cmo / setatt / mo / a / 1,0,0 / UNDEFINED_NAME",
        "cmo / printatt / mo / a / minmax",
        "define / VAL / 2.5",
        "cmo / setatt / mo / a / 1,0,0 / VAL",
        "cmo / printatt / mo / a / minmax",
        "finish"])
    E["cc04_setatt_word"] = {
        "has": ["ERROR cmo/setatt: the value UNDEFINED_NAME is not a number, for a",
                " a                  0.000000000E+00  0.000000000E+00",
                " a                  2.500000000E+00  2.500000000E+00"],
        "lacks": ["-310"], "count": {}}

    json.dump(E, open(os.path.join(work, "expect.json"), "w"), indent=1)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
