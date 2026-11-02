#!/usr/bin/env python3
"""make_input_lines.py WORKDIR

Writes the decks of test_input_lines (Python 3, standard library only)
and WORKDIR/expect.json, what each deck must produce. The decks are
generated because they hold lines of exact lengths (81, 1024, 1025,
3000 and 1,000,000 characters) that are awkward to keep in git.

Every deck ends with  cmo / status / brief  so the checker sees which
mesh objects exist, and with  finish.

  il01_80      a command exactly 80 characters long: no NOTE
  il02_note    commands of 81, 500 and 1024 characters: used, one NOTE
               each; a command whose cell type is past column 80
               (LaGriT V3.3.4 made a tet mesh object, now hex)
  il03_long    commands of 1025 and 3000 characters: ERROR each, not
               run; the commands after them run
  il04_huge    a command of 1,000,000 characters: ERROR, not run
  il05_amp     a 1100-character line ending in &, then continuation
               lines that would make a mesh object: the whole command
               is skipped, its continuation lines are not run on their
               own; then a continuation line of 1100 characters
  il06_joined  a command over 20 lines of about 900 characters each,
               over 16384 joined: ERROR, skipped; LaGriT does not give
               up the deck (the old behaviour) and runs what follows
  il07_comment comment lines of 200 and 5000 characters: no message
  il08_tokens  a 40-character mesh object name (WARNING, the first 32
               are used); a 40-character number (read in full, no
               warning)
  il09_infile  the limits inside a file read with infile: the messages
               give that file's name and line numbers
  il10_crlf    an infile with CRLF line ends and no newline at the end
"""
import json
import os
import sys


def pad_cmd(head, tail, n):
    """head + blanks + tail, exactly n characters long"""
    s = head + " " * (n - len(head) - len(tail)) + tail
    assert len(s) == n, (len(s), n)
    return s


def write(work, name, lines):
    with open(os.path.join(work, name), "w", newline="\n") as f:
        f.write("\n".join(lines) + "\n")


def main(work):
    E = {}
    end = ["cmo / status / brief", "finish"]

    write(work, "il01_80.lgi", [pad_cmd("cmo / create / mo80", "/ / / tet", 80)] + end)
    E["il01_80"] = {"mo": ["mo80"], "no_mo": [], "has": [], "count": {"NOTE:": 0, "ERROR": 0}}

    write(work, "il02_note.lgi", [
        pad_cmd("cmo / create / mo81", "/ / / tet", 81),
        pad_cmd("cmo / create / mo500", "/ / / tet", 500),
        pad_cmd("cmo / create / mo1024", "/ / / tet", 1024),
        "cmo / create / mo_type" + " " * (80 - 22) + "/ / / hex",
        "cmo / status / mo_type / brief"] + end)
    E["il02_note"] = {"mo": ["mo81", "mo500", "mo1024", "mo_type"], "no_mo": [],
                      "has": ["NOTE: line 1 of the input has 81 characters;",
                              "NOTE: line 3 of the input has 1024 characters;",
                              "LaGriT V3.3.4 and earlier used only the first 80 of them.",
                              "element type =                hex"],
                      "count": {"NOTE:": 4, "ERROR": 0}}

    write(work, "il03_long.lgi", [
        pad_cmd("cmo / create / mo1025", "/ / / tet", 1025),
        pad_cmd("cmo / create / mo3000", "/ / / tet", 3000),
        "cmo / create / mo_after / / / tet"] + end)
    E["il03_long"] = {"mo": ["mo_after"], "no_mo": ["mo1025", "mo3000"],
                      "has": ["ERROR: line 1 of the input is too long:",
                              "1025 characters, more than 1024.",
                              "3000 characters, more than 1024.",
                              "Command skipped. Split a long command over lines ending in &."],
                      "count": {"is too long:": 2}}

    write(work, "il04_huge.lgi", [
        pad_cmd("cmo / create / mo_huge", "/ / / tet", 1000000),
        "cmo / create / mo_after / / / tet"] + end)
    E["il04_huge"] = {"mo": ["mo_after"], "no_mo": ["mo_huge"],
                      "has": ["1000000 characters, more than 1024."], "count": {"is too long:": 1}}

    write(work, "il05_amp.lgi", [
        pad_cmd("cmo / create / mo_amp", "&", 1100),
        "/ / / tet &",
        "cmo / create / mo_leak / / / tet",
        "cmo / create / mo_ok1 / / / tet",
        "cmo / create / mo_amp2 &",
        pad_cmd("/ /", "/ tet &", 1100),
        "cmo / create / mo_leak2 / / / tet",
        "cmo / create / mo_ok2 / / / tet"] + end)
    E["il05_amp"] = {"mo": ["mo_ok1", "mo_ok2"], "no_mo": ["mo_amp", "mo_leak", "mo_amp2", "mo_leak2"],
                     "has": ["ERROR: line 1 of the input is too long:",
                             "ERROR: line 6 of the input is too long:"],
                     "count": {"is too long:": 2}}

    piece = "/ " + " ".join(["1"] * 440)          # about 900 characters per line
    write(work, "il06_joined.lgi", ["cmo / create / mo_join / / / tet &"] +
          [piece + " &"] * 19 + [piece] +
          ["cmo / create / mo_after / / / tet"] + end)
    E["il06_joined"] = {"mo": ["mo_after"], "no_mo": ["mo_join"],
                        "has": ["ERROR: the command at line 1 of the input is too long:",
                                "joined with its continuation lines it has over 16384 characters."],
                        "count": {"is too long:": 1}}

    write(work, "il07_comment.lgi", ["# " + "c" * 198, "# " + "x" * 4998,
                                     "cmo / create / mo_c / / / tet"] + end)
    E["il07_comment"] = {"mo": ["mo_c"], "no_mo": [], "has": [], "count": {"NOTE:": 0, "ERROR": 0}}

    name40 = "mo_" + "abcdefghij" * 3 + "klmnopq"      # 40 characters
    write(work, "il08_tokens.lgi", [
        "cmo / create / " + name40 + " / / / tet",
        "cmo / create / mo_num / / / hex",
        "createpts / brick / xyz / 2 2 2 / 0. 0. 0. / &",
        "      1.00000000000000000000000000000000000000 1. 1. / 1 1 1",
        "cmo / printatt / mo_num / xic / minmax"] + end)
    E["il08_tokens"] = {"mo": [name40[:32], "mo_num"], "no_mo": [],
                        "has": ["WARNING: token 3 has 40 characters; LaGriT uses the first 32:",
                                '"' + name40[:32] + '"',
                                "0.000000000E+00  1.000000000E+00 1.000000000E+00"],
                        "count": {"WARNING: token": 1}}

    write(work, "il09_sub.mlgi", ["# a file read with infile",
                                  pad_cmd("cmo / create / mo_s81", "/ / / tet", 81),
                                  pad_cmd("cmo / create / mo_s2000", "/ / / tet", 2000),
                                  "cmo / create / mo_s_after / / / tet",
                                  "finish"])
    write(work, "il09_infile.lgi", ["cmo / create / mo_main / / / tet",
                                    "infile / il09_sub.mlgi",
                                    pad_cmd("cmo / create / mo_m85", "/ / / tet", 85)] + end)
    E["il09_infile"] = {"mo": ["mo_main", "mo_s81", "mo_s_after", "mo_m85"], "no_mo": ["mo_s2000"],
                        "has": ["NOTE: line 2 of il09_sub.mlgi has 81 characters;",
                                "ERROR: line 3 of il09_sub.mlgi is too long:",
                                "NOTE: line 3 of the input has 85 characters;"],
                        "count": {"is too long:": 1}}

    with open(os.path.join(work, "il10_sub.mlgi"), "wb") as f:
        f.write(b"cmo / create / mo_crlf1 / / / tet\r\n"
                + (pad_cmd("cmo / create / mo_crlf90", "/ / / tet", 90)).encode() + b"\r\n"
                + b"finish")                           # no newline at the end
    write(work, "il10_crlf.lgi", ["infile / il10_sub.mlgi",
                                  "cmo / create / mo_after / / / tet"] + end)
    E["il10_crlf"] = {"mo": ["mo_crlf1", "mo_crlf90", "mo_after"], "no_mo": [],
                      "has": ["NOTE: line 2 of il10_sub.mlgi has 90 characters;"],
                      "count": {"ERROR": 0}}

    json.dump(E, open(os.path.join(work, "expect.json"), "w"), indent=1)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
