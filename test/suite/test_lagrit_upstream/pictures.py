#!/usr/bin/env python3
"""pictures.py: the picture run and the picture list of a test
(test_lagrit_upstream). Python 3, standard library only.

The picture run is a SECOND run of a LaGriT test deck, in
work/_pic_<test>, with dump commands added; it is drawn, never judged.
It is made when the test wrote no AVS mesh file (one dump of the
current mesh object before the first finish line, as before), or when
pictures.txt asks for extra dumps (insert, interp).

Command line (used by run_upstream_tests.sh):
  pictures.py deck TEST DECK WROTE OUT
      write the picture-run deck for TEST (DECK = its input.lgi, WROTE
      = yes if the test wrote an AVS mesh file) to OUT; exit 3 if no
      picture run is needed
Used by check_upstream.py: jobs(...) -> the pictures to draw.
"""
import fnmatch
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
FINAL = "zz_final_mesh.inp"
MANIFEST = "zz_interp_manifest.txt"
INTERP = re.compile(r"^\s*(intrp|interpolate)\b", re.I)


def load(path=os.path.join(HERE, "pictures.txt")):
    """test -> [(kind, [fields])]"""
    out = {}
    if not os.path.isfile(path):
        return out
    for line in open(path):
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        f = [x.strip() for x in line.rstrip("\n").split(" | ")]
        if len(f) >= 2:
            out.setdefault(f[0], []).append((f[1], f[2:]))
    return out


def is_comment(line):
    s = line.strip()
    return not s or s[0] in "#*!"


def commands(lines):
    """[(first index, last index, joined text)] of the deck's commands;
    a line ending in & continues on the next"""
    out, i = [], 0
    while i < len(lines):
        if is_comment(lines[i]):
            i += 1
            continue
        j, text = i, lines[i].rstrip()
        while text.endswith("&") and j + 1 < len(lines):
            j += 1
            text = text[:-1] + " " + lines[j].strip()
        out.append((i, j, text))
        i = j + 1
    return out


def interp_parts(text):
    """(sink, sink attribute, source, source attribute) of an intrp /
    interpolate command: tokens are split by / , and blanks, so the
    range (1,0,0 or pset,get,NAME) is always three tokens"""
    t = [x for x in re.split(r"[/,\s]+", text.split(";")[0]) if x]
    if len(t) < 9:
        return None
    return t[2], t[3], t[7], t[8]


def pic_deck(test, deck, wrote):
    """(new deck text, manifest lines) or None"""
    D = load().get(test, [])
    lines = open(deck, errors="replace").read().split("\n")
    after = {}                              # line index -> [commands]
    manifest = []
    cmds = commands(lines)
    for kind, f in D:
        if kind == "insert" and len(f) >= 2:
            rx = re.compile(f[0], re.I)
            for first, last, text in cmds:
                if rx.search(text):
                    after.setdefault(last, []).append(f[1])
                    break
    if any(kind == "interp" for kind, _ in D):
        n = 0
        for first, last, text in cmds:
            if not INTERP.match(text):
                continue
            p = interp_parts(text)
            if not p:
                continue
            n += 1
            sink, satt, src, ratt = p
            # make sure dump / avs writes both attributes (a system
            # attribute such as ialias is not written by default)
            after.setdefault(last, []).extend([
                "cmo / modatt / %s / %s / ioflag / agl" % (src, ratt),
                "cmo / modatt / %s / %s / ioflag / agl" % (sink, satt),
                "dump / avs / zz_i%02d_src.inp / %s" % (n, src),
                "dump / avs / zz_i%02d_sink.inp / %s" % (n, sink)])
            manifest.append("%02d|%s|%s|%s|%s|%s" % (n, src, ratt, sink, satt,
                                                     " ".join(text.split())))
    final = not wrote
    if not after and not final:
        return None
    out, done = [], False
    for k, line in enumerate(lines):
        if final and not done and re.match(r"^\s*finish\s*$", line, re.I):
            out.append("dump / avs / %s" % FINAL)
            done = True
        out.append(line)
        out.extend(after.get(k, []))
    if final and not done:
        out.append("dump / avs / %s" % FINAL)
    return "\n".join(out), manifest


def jobs(test, own, work_dir, pic_dir):
    """the pictures of a test, in order: [dict(file, title, color,
    caption, pair)]. own: the AVS mesh files the test wrote (names)."""
    D = load().get(test, [])
    hide = [f[0] for k, f in D if k == "hide" and f]
    colors = [(f[0], f[1]) for k, f in D if k == "color" and len(f) >= 2]
    interp = any(k == "interp" for k, _ in D)
    out = []
    if not interp:
        for name in own:
            if any(fnmatch.fnmatch(name, g) for g in hide):
                continue
            color = "mat"
            for g, c in colors:
                if fnmatch.fnmatch(name, g):
                    color = c
            out.append(dict(file=os.path.join(work_dir, name), title=name,
                            color=color, caption="", pair=None))
        if not own and os.path.isfile(os.path.join(pic_dir, FINAL)):
            out.append(dict(file=os.path.join(pic_dir, FINAL),
                            title="end state: the current mesh object at "
                                  "finish (extra run)",
                            color="mat", caption="", pair=None))
    man = os.path.join(pic_dir, MANIFEST)
    if interp and os.path.isfile(man):
        for line in open(man):
            n, src, ratt, sink, satt, cmd = line.rstrip("\n").split("|", 5)
            for role, mo, att in (("src", src, ratt), ("sink", sink, satt)):
                out.append(dict(
                    file=os.path.join(pic_dir, "zz_i%s_%s.inp" % (n, role)),
                    title="%s %s: %s %s" % (
                        n, "source" if role == "src" else "sink", mo, att),
                    color="attr:" + att, caption=cmd if role == "src" else "",
                    pair=n))
    for k, f in D:
        if k == "draw" and f:
            name = f[0]
            path = os.path.join(pic_dir, name)
            if not os.path.isfile(path):
                path = os.path.join(work_dir, name)
            out.append(dict(file=path, title=f[2] if len(f) > 2 else name,
                            color=f[1] if len(f) > 1 else "mat",
                            caption="", pair=None))
    return out


def main(argv):
    if len(argv) == 6 and argv[1] == "deck":
        _, _, test, deck, wrote, out = argv
        r = pic_deck(test, deck, wrote == "yes")
        if r is None:
            return 3
        text, manifest = r
        open(out, "w").write(text)
        if manifest:
            open(os.path.join(os.path.dirname(out), MANIFEST), "w").write(
                "\n".join(manifest) + "\n")
        return 0
    print(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
