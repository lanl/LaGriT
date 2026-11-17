"""check_entry.py WORK LOGS

test_entry: judges each case of cases.txt (run by run_entry_tests.sh:
the program in WORK/NAME/prog, the harness calling the LaGriT entry
point in WORK/NAME/entry). Draws each mesh with its control volumes,
writes summary.html in this suite's folder. Python 3 standard library.
Exit 1 on any FAIL.

Checks:
  ierr     each call returned the ierr cases.txt expects (the harness
           prints 'harness: call K: ierr = N'), and the harness did not
           stop (del_vor never stops: exit 0, or 3 when the last call
           returned an error)
  message  the expected text is in the harness output
  program  the program ran with exit 0 (cases with arguments)
  files    the entry point wrote the same files as the program (the
           mesh and the run's own files aside); with no program run,
           it wrote none
  bytes    every file equal byte for byte, apart from the .stor's line
           2 (date and command) and the report's options lines (the
           program names its mesh file and says it is reading it)
  line2    the .stor's line 2 ends with (del_vor OPTIONS), the options
           of the last call
  screen   the last call's log lines equal the program's screen output
           (options lines aside); with -sink every one came through the
           hook (lagrit| ...), without it straight to the screen
  volumes  the volumes the entry point returned equal the .stor's
           (every digit); with check_only on, none returned
"""

import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
sys.path.insert(0, os.path.join(SUITE, 'test_voronoi_gold'))
from exact_check import read_avs  # noqa: E402
from pagekit import read_cases, write_case_page  # noqa: E402
from svgdraw import Svg, ramp, Projector  # noqa: E402
from check_voronoi_gold import read_stor  # noqa: E402

RUNFILES = {'run.out', 'run.rc', 'vol.txt'}


def drop_options(lines):
    """Lines without the options block (del_vor: options and its
    indented continuation lines) and the 'del_vor: reading' line."""
    out = []
    in_opt = False
    for ln in lines:
        if ln.startswith('del_vor: options'):
            in_opt = True
            continue
        if in_opt and ln.startswith('  '):
            continue
        in_opt = False
        if ln.startswith('del_vor: reading '):
            continue
        out.append(ln)
    return out


def last_call_lines(lines):
    """The log lines of the last call: between the last two
    'harness: call' lines."""
    marks = [k for k, ln in enumerate(lines)
             if ln.startswith('harness: call ')]
    if not marks:
        return lines
    a = marks[-2] + 1 if len(marks) > 1 else 0
    return lines[a:marks[-1]]


def file_set(d, mesh):
    if not os.path.isdir(d):
        return set()
    return {f for f in os.listdir(d)
            if f not in RUNFILES and f != mesh}


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows = []
    nfail_cases = 0
    for (name, mesh, args, flags, calls, ierr, msg,
         about) in read_cases(os.path.join(HERE, 'cases.txt'), 8):
        checks = []

        def put(c, v, ok):
            checks.append((c, v, 'PASS' if ok else 'FAIL'))
            print('%s_%s %s %s' % (name, c, v, 'PASS' if ok else 'FAIL'))

        mname = os.path.basename(mesh)
        de = os.path.join(work, name, 'entry')
        dp = os.path.join(work, name, 'prog')
        call_list = [c.strip() for c in calls.split(';')]
        call_list = [args if c == '=' else c for c in call_list]
        cmd = 'del_vor_entry_test %s vol.txt %s %s' % (
            mname, '' if flags == '-' else flags,
            ' '.join("'%s'" % c for c in call_list))
        if args != '-':
            cmd = 'del_vor -avs %s %s<br>' % (mname, args) + cmd
        try:
            rc = int(open(os.path.join(de, 'run.rc')).read())
            out = open(os.path.join(de, 'run.out')).read().split('\n')
        except (OSError, ValueError):
            put('ran', 'no run.rc', False)
            rows.append((name, about, cmd, checks, None))
            continue
        got = [int(m.group(1)) for ln in out
               for m in [re.match(r'harness: call \d+: ierr = (-?\d+)',
                                  ln)] if m]
        want = [int(v) for v in ierr.split()]
        stopped_ok = rc == (3 if want and want[-1] else 0)
        put('ierr', '%s (expected %s), harness exit %d'
            % (' '.join(map(str, got)) or 'none', ierr, rc),
            got == want and stopped_ok)
        if msg != '-':
            flat = ' '.join(' '.join(out).split())
            found = ' '.join(msg.split()) in flat
            put('message', '"%s" %s' % (msg, 'found' if found
                                         else 'NOT found'), found)

        fe = file_set(de, mname)
        vol = None
        if os.path.exists(os.path.join(de, 'vol.txt')):
            vol = [float(v) for v in
                   open(os.path.join(de, 'vol.txt')).read().split()]
        if args == '-':
            put('files', 'none written' if not fe else
                'wrote ' + ' '.join(sorted(fe)), not fe)
        else:
            try:
                prc = int(open(os.path.join(dp, 'run.rc')).read())
                pout = open(os.path.join(dp, 'run.out')).read()
            except (OSError, ValueError):
                prc, pout = -1, ''
            put('program', 'exit %d' % prc, prc == 0)
            fp = file_set(dp, mname)
            put('files', '%d files: %s' % (len(fp), ' '.join(sorted(fp)))
                if fe == fp else 'entry %s, program %s'
                % (sorted(fe), sorted(fp)), fe == fp)
            bad = []
            stors = []
            for f in sorted(fe & fp):
                a = open(os.path.join(de, f), 'rb').read()
                b = open(os.path.join(dp, f), 'rb').read()
                if f.endswith('.stor'):
                    stors.append(f)
                    la, lb = a.split(b'\n'), b.split(b'\n')
                    same = la[:1] + la[2:] == lb[:1] + lb[2:]
                    tail = '(del_vor %s)' % call_list[-1].strip()
                    l2 = la[1].decode('latin-1').rstrip()
                    put('line2', '%s: ...%s' % (f, l2[-60:]),
                        l2.endswith(tail))
                elif f.endswith('.report'):
                    same = (drop_options(a.decode().split('\n')) ==
                            drop_options(b.decode().split('\n')))
                else:
                    same = a == b
                if not same:
                    bad.append(f)
            put('bytes', 'all %d equal' % len(fe & fp) if not bad else
                'differ: ' + ' '.join(bad), not bad)

            lines = last_call_lines(out)
            if flags != '-' and '-sink' in flags.split():
                hooked = all(ln.startswith('lagrit| ') for ln in lines)
                lines = [ln[8:] for ln in lines if
                         ln.startswith('lagrit| ')]
            else:
                hooked = not any(ln.startswith('lagrit| ')
                                 for ln in lines)
            mine = drop_options(lines)
            theirs = drop_options(pout.split('\n'))
            while theirs and theirs[-1] == '':
                theirs.pop()
            put('screen', '%d lines equal, %s' % (
                len(mine), 'all through the hook' if '-sink' in flags
                else 'none through the hook')
                if mine == theirs and hooked else
                'differ (%d vs %d lines; hook %s)'
                % (len(mine), len(theirs), hooked),
                mine == theirs and hooked)
            if 'check_only on' in args:
                put('volumes', 'none returned' if vol is None else
                    'returned', vol is None)
            elif stors:
                sv = read_stor(os.path.join(dp, stors[0]))['vol']
                put('volumes', '%d equal to the .stor\'s' % len(sv)
                    if vol == sv else 'differ', vol == sv)

        pic = name + '.svg'
        try:
            picture(os.path.join(logs, pic), name, mesh_path(work, mesh),
                    vol, checks, flags)
        except (OSError, ValueError, IndexError):
            pic = None
        if any(c[2] == 'FAIL' for c in checks):
            nfail_cases += 1
        rows.append((name, about, cmd, checks, pic))

    what = ('calls the LaGriT entry point, del_vor_lagrit_entry, from a '
            'harness the way LaGriT will (fixed-form Fortran built with '
            'LaGriT\'s gfortran flags, 8-byte integers, no interface), '
            'with the mesh arrays used in place. Each case runs the '
            'program and the entry point on the same mesh and options: '
            'every file must be equal byte for byte (the .stor\'s date '
            'and command aside), the screen output too (through the '
            'log hook), and the volumes returned must equal the '
            '.stor\'s. Errors (bad options, bad meshes, parent/child '
            'chains, LaGriT arrays not in the expected layout) must come '
            'back in ierr with a message, write nothing, and leave the '
            'next call working. The pictures show each mesh, colored by '
            'the control volume the entry point returned.')
    nfail = write_case_page(HERE, logs, 'test_entry', what, rows)
    sys.exit(1 if nfail else 0)


def mesh_path(work, mesh):
    if mesh.startswith('work/'):
        return os.path.join(work, mesh[5:])
    return os.path.join(HERE, mesh)


def read_itp(path, nn):
    """itp1 of each vertex from the AVS file (0 if it has none)."""
    with open(path) as f:
        lines = [ln for ln in f if ln.strip()]
    h = lines[0].split()
    k = 1 + nn + int(h[1])
    if int(h[2]) == 0 or len(lines) <= k:
        return [0] * nn
    ncomp = [int(v) for v in lines[k].split()[1:]]
    names = [lines[k + 1 + a].split(',')[0].strip().lower()
             for a in range(len(ncomp))]
    if 'itp1' not in names:
        return [0] * nn
    col = 1 + sum(ncomp[:names.index('itp1')])
    vals = ' '.join(lines[k + 1 + len(ncomp):]).split()
    per = 1 + sum(ncomp)
    return [int(float(vals[i * per + col])) for i in range(nn)]


def picture(path, name, mpath, vol, checks, flags):
    """The mesh (3D: an oblique view; a large one by the edges of its
    boundary faces), its vertices colored by the control volume the
    entry point returned; gray when none came back (an error, or
    check_only). Red: what an error case is about (parents, itp1 =
    41, ringed; the tampered cell 1 filled)."""
    m = read_avs(mpath)
    itp = read_itp(mpath, len(m.xyz))
    W, H = 470, 430
    s = Svg(W, H)
    nfail = sum(1 for c in checks if c[2] == 'FAIL')
    s.text(14, 22, '%s: %s' % (name, 'PASS' if not nfail else
                               '%d FAIL' % nfail), size=14, bold=True)
    flat = all(abs(p[2] - m.xyz[0][2]) < 1e-300 for p in m.xyz)
    pr = Projector(m.xyz, 20, 40, W - 40, H - 125,
                   view='plan' if flat else 'iso')
    edges = set()
    for c in m.cells:
        for a in range(len(c)):
            for b in range(a + 1, len(c)):
                edges.add((min(c[a], c[b]), max(c[a], c[b])))
    if len(edges) > 6000 and m.nen == 4:
        # only the boundary: the edges of faces in one tet
        nf = {}
        for c in m.cells:
            for f in ((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3)):
                key = tuple(sorted(c[i] for i in f))
                nf[key] = nf.get(key, 0) + 1
        edges = set()
        for f, n in nf.items():
            if n == 1:
                edges |= {(f[0], f[1]), (f[0], f[2]), (f[1], f[2])}
    if len(edges) <= 30000:
        for a, b in edges:
            x1, y1 = pr(m.xyz[a])
            x2, y2 = pr(m.xyz[b])
            s.line(x1, y1, x2, y2, color='#bbb', width=0.5)
    if '-tamper' in flags.split():
        s.polygon([pr(m.xyz[v]) for v in m.cells[0]], fill='#d62728',
                  opacity=0.45)
        s.text(20, H - 64, 'red: cell 1, the one the harness spoiled '
               '(%s)' % flags.split()[-1], size=11, color='#d62728')
    r = 3.5 if len(m.xyz) < 200 else 1.6
    if vol and len(vol) == len(m.xyz):
        lo, hi = min(vol), max(vol)
        for p, v in zip(m.xyz, vol):
            x, y = pr(p)
            t = (v - lo) / (hi - lo) if hi > lo else 0.5
            s.circle(x, y, r, fill=ramp(t))
        s.legend(20, H - 50, 200, 10, '%.3g' % lo, '%.3g' % hi,
                 'control volume returned by the entry point')
    else:
        for p, t in zip(m.xyz, itp):
            x, y = pr(p)
            s.circle(x, y, r, fill='#888')
            if t == 41:
                s.circle(x, y, r + 4, fill='none', stroke='#d62728',
                         width=2)
        if 41 in itp:
            s.text(20, H - 64, 'red rings: parents (itp1 = 41)',
                   size=11, color='#d62728')
        s.text(20, H - 40, 'no volumes returned (an error, or '
               'check_only): vertices gray', size=11, color='#555')
    s.text(20, H - 14, '%d vertices, %d %s' % (
        len(m.xyz), len(m.cells), 'triangles' if m.nen == 3 else 'tets'),
        size=11, color='#555')
    s.save(path)


if __name__ == '__main__':
    main()
