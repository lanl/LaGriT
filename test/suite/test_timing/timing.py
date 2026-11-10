"""timing.py DEL_VOR MESHDIR WORK LOGS [LAGRIT]

test_timing: times del_vor, step by step, on the meshes of MESHDIR
(tet_N.inp, tri_N.inp, from make_timing_meshes.sh; and dfn_*.inp, any
fracture networks put there), and LaGriT's dump / stor on the same
meshes when LAGRIT is given. Writes LOGS/timing.csv (one row per
mesh, program and step), the pictures, and summary.html in this
suite's folder. Python 3 standard library.

Each measurement is the best (smallest) of REPEAT runs (environment,
default 3): the best run is the one least disturbed by other work on
the machine.

del_vor:  del_vor -avs M.inp -stor M.stor -delaunay_tol 1 -timing on
          (the per-step wall and CPU times and the peak memory are
          del_vor's own timing block). delaunay_tol 1 so the run never
          stops: LaGriT's connect leaves a few facets that are not
          Delaunay (delta/L down to -3.6e-4 at a million tets,
          confirmed in exact arithmetic); with the defaults they are a
          warning, but a bigger mesh might go beyond delaunay_fatal
          (1e-3). The check still runs in full and is timed; the
          number of such facets is reported (not_delaunay).
LaGriT:   two runs: read / avs only, and read / avs + dump / stor;
          dump / stor's time is the difference of the two wall
          times. Peak memory of each from getrusage of the child.

Checks (per mesh family, sizes in increasing order):
  slope_total   the slope of log(time) against log(cells) of del_vor's
                total wall time, fitted over the sizes whose total is
                at least 0.05 s: 1 means linear. PASS when <= 1.2.
                MEASURED (not judged) with fewer than 3 such sizes or
                a largest time under 0.5 s: too small to tell.
  slope_STEP    the same for every step that takes at least 5 % of
                the total at the largest size. MEASURED.
  memory        del_vor's peak memory per cell at the largest size.
                MEASURED.
  memory_units  del_vor's own peak memory (timing on) within 25 % of
                the one measured from outside (getrusage of the child)
                at every size. PASS / FAIL.
  vs_lagrit     del_vor total, and the steps that stand for dump /
                stor (edge CSR + assembly + summary + write stor),
                against LaGriT's dump / stor and read + dump / stor, at
                the largest size. MEASURED.
"""

import math
import os
import re
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.dirname(HERE)
sys.path.insert(0, SUITE)
from svgdraw import Svg  # noqa: E402

REPEAT = int(os.environ.get('REPEAT', '3'))
STOR_STEPS = ('edge CSR', 'assembly', 'zero-area mask', 'summary',
              'write stor')
COLORS = ['#2b6cb0', '#1a7f37', '#c05621', '#805ad5', '#d53f8c',
          '#319795', '#000000', '#a0aec0', '#d69e2e', '#c00']

# run one child and report its wall time and peak memory: a fresh
# Python per child, so RUSAGE_CHILDREN holds that child alone
WRAP = r'''
import resource, subprocess, sys, time
t0 = time.perf_counter()
with open(sys.argv[2], 'w') as out, open(sys.argv[3]) as inp:
    rc = subprocess.call(sys.argv[4:], stdout=out, stderr=out, stdin=inp,
                         cwd=sys.argv[1])
dt = time.perf_counter() - t0
mx = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
print(rc, dt, mx * (1 if sys.platform == 'darwin' else 1024) / 1e6)
'''


def run(cwd, cmd, out, stdin=os.devnull):
    r = subprocess.run([sys.executable, '-I', '-c', WRAP, cwd, out, stdin]
                       + cmd, capture_output=True, text=True)
    rc, dt, mb = r.stdout.split()
    return int(rc), float(dt), float(mb)


def mesh_size(path):
    with open(path) as f:
        t = f.readline().split()
    return int(t[0]), int(t[1])


def del_vor_steps(path):
    st, mb = {}, None
    for ln in open(path):
        m = re.match(r'timing  (.{20}) wall\s+([\d.]+) s  cpu\s+([\d.]+)',
                     ln)
        if m:
            st[m.group(1).strip()] = (float(m.group(2)),
                                      float(m.group(3)))
        m = re.match(r'timing  peak memory\s+([\d.]+) MB', ln)
        if m:
            mb = float(m.group(1))
    return st, mb


def edges_of(path):
    for ln in open(path):
        m = re.match(r'\s+edges\s+(\d+)', ln)
        if m:
            return int(m.group(1))
    return 0


def not_delaunay(path):
    """Facets with delta/L < -1e-10 (the default delaunay_tol): from
    the histogram of the report, the three bins below -1e-10."""
    n, worst = 0, None
    for ln in open(path):
        m = re.match(r'\s+(-inf|-1\.0E-02|-1\.0E-06) to\s+\S+\s+(\d+)$', ln)
        if m:
            n += int(m.group(2))
        m = re.match(r'\s+smallest delta/L\s+(\S+)', ln)
        if m:
            worst = float(m.group(1))
    return n, worst


def fit_slope(xs, ys):
    """Least-squares slope of log y against log x."""
    lx = [math.log(x) for x in xs]
    ly = [math.log(y) for y in ys]
    n = len(lx)
    mx, my = sum(lx) / n, sum(ly) / n
    sxx = sum((a - mx) ** 2 for a in lx)
    if sxx == 0:
        return float('nan')
    return sum((a - mx) * (b - my) for a, b in zip(lx, ly)) / sxx


def measure(dv, lagrit, meshdir, work, logs, name):
    """All runs for one mesh; returns a dict of results."""
    src = os.path.join(meshdir, name + '.inp')
    nv, nc = mesh_size(src)
    d = os.path.join(work, name)
    os.makedirs(d, exist_ok=True)
    inp = os.path.join(d, 'mesh.inp')
    if not os.path.exists(inp):
        os.symlink(src, inp)
    best = None
    for k in range(REPEAT):
        out = os.path.join(d, 'del_vor_%d.out' % k)
        rc, dt, mb = run(d, [dv, '-avs', 'mesh.inp', '-stor', 'mesh.stor',
                             '-delaunay_tol', '1', '-timing', 'on'],
                         out)
        st, pmb = del_vor_steps(out)
        if rc != 0 or 'total' not in st:
            return {'name': name, 'error': 'del_vor exit %d (%s)'
                    % (rc, out)}
        if best is None or st['total'][0] < best[0]['total'][0]:
            best = (st, pmb, dt, out, mb)
    st, pmb, dt, out, mb_os = best
    res = {'name': name, 'vertices': nv, 'cells': nc,
           'edges': edges_of(out), 'steps': st, 'peak_mb': pmb,
           'wall_outside': dt, 'not_delaunay': not_delaunay(out),
           'peak_mb_os': mb_os}
    with open(os.path.join(logs, name + '_del_vor.out'), 'w') as f:
        f.write(open(out).read())
    if lagrit:
        for tag, deck in (('read', 'read / avs / mesh.inp / mo\nfinish\n'),
                          ('read_stor', 'read / avs / mesh.inp / mo\n'
                           'dump / stor / lg / mo\nfinish\n')):
            dk = os.path.join(d, tag + '.lgi')
            with open(dk, 'w') as f:
                f.write(deck)
            ts = []
            for k in range(REPEAT):
                rc, t, mb = run(d, [lagrit], os.path.join(d, tag + '.out'),
                                stdin=dk)
                if rc != 0:
                    res['lagrit_error'] = 'LaGriT exit %d (%s)' % (rc, dk)
                    break
                ts.append((t, mb))
            if ts:
                res['lagrit_' + tag] = min(ts)
        for f in ('lg.stor', 'mesh.stor'):
            p = os.path.join(d, f)
            if os.path.exists(p):
                os.remove(p)
    return res


def plot(path, title, fam, rows, keys, getter):
    """Log-log plot of time against cells, one line per key."""
    W, H = 800, 440
    x0, y0, x1, y1 = 70, 40, 470, 380
    s = Svg(W, H)
    s.text(14, 22, title, size=13, bold=True)
    pts = {k: [(r['cells'], getter(r, k)) for r in rows
               if getter(r, k) and getter(r, k) > 0] for k in keys}
    allx = [x for v in pts.values() for x, _ in v]
    ally = [y for v in pts.values() for _, y in v]
    if not allx:
        s.text(70, 200, 'no data', size=12)
        s.save(path)
        return
    lx0, lx1 = math.floor(math.log10(min(allx))), \
        math.ceil(math.log10(max(allx)))
    ly0, ly1 = math.floor(math.log10(min(ally))), \
        math.ceil(math.log10(max(ally)))
    lx1 = max(lx1, lx0 + 1)
    ly1 = max(ly1, ly0 + 1)

    def X(v):
        return x0 + (math.log10(v) - lx0) / (lx1 - lx0) * (x1 - x0)

    def Y(v):
        return y1 - (math.log10(v) - ly0) / (ly1 - ly0) * (y1 - y0)
    s.rect(x0, y0, x1 - x0, y1 - y0, fill='#fafafa', stroke='#999')
    for e in range(lx0, lx1 + 1):
        s.line(X(10 ** e), y0, X(10 ** e), y1, color='#ddd', width=0.6)
        s.text(X(10 ** e), y1 + 16, '1e%d' % e, size=10, anchor='middle')
    for e in range(ly0, ly1 + 1):
        s.line(x0, Y(10 ** e), x1, Y(10 ** e), color='#ddd', width=0.6)
        s.text(x0 - 6, Y(10 ** e) + 4, '1e%d s' % e, size=10, anchor='end')
    s.text((x0 + x1) / 2, y1 + 32, 'cells (%s)' % fam, size=11,
           anchor='middle')
    # a slope-1 guide through the largest total
    tot = pts.get(keys[0], [])
    if tot:
        cx, cy = tot[-1]
        a, b = 10 ** lx0, 10 ** lx1
        s.line(X(a), Y(cy * a / cx), X(b), Y(cy * b / cx), color='#999',
               width=1, dash='5,4')
    ly = y0 + 6
    for i, k in enumerate(keys):
        v = pts[k]
        if not v:
            continue
        col = COLORS[i % len(COLORS)]
        s.polyline([(X(a), Y(b)) for a, b in v], color=col, width=1.8)
        for a, b in v:
            s.circle(X(a), Y(b), 2.6, fill=col)
        sl = fit_slope([a for a, _ in v], [b for _, b in v]) \
            if len(v) >= 2 else float('nan')
        s.line(x1 + 14, ly, x1 + 34, ly, color=col, width=2)
        s.text(x1 + 40, ly + 4, '%s (slope %.2f)' % (k, sl), size=10)
        ly += 16
    s.line(x1 + 14, ly, x1 + 34, ly, color='#999', width=1, dash='5,4')
    s.text(x1 + 40, ly + 4, 'slope 1 (linear)', size=10, color='#555')
    s.save(path)


def picture_mesh(path, title, meshfile):
    """The smallest mesh of a family: every edge (2D) or the boundary
    edges (3D), in an oblique view."""
    sys.path.insert(0, os.path.join(SUITE, 'test_voronoi_gold'))
    from svgdraw import Projector
    from check_voronoi_gold import read_inp
    xyz, cells = read_inp(meshfile)
    W, H = 420, 380
    s = Svg(W, H)
    s.text(12, 20, title, size=12, bold=True)
    flat = len({round(p[2], 12) for p in xyz}) == 1
    pr = Projector(xyz, 20, 34, 380, 320, view='plan' if flat else 'iso')
    seen = {}
    for c in cells:
        if len(c) == 3:
            for a in range(3):
                e = tuple(sorted((c[a], c[(a + 1) % 3])))
                seen[e] = 1
        else:
            for f in ((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3)):
                k = tuple(sorted(c[i] for i in f))
                seen[k] = seen.get(k, 0) + 1
    for k, n in seen.items():
        if len(k) == 2 or n == 1:
            for a in range(len(k)):
                for b in range(a + 1, len(k)):
                    x1, y1 = pr(xyz[k[a]])
                    x2, y2 = pr(xyz[k[b]])
                    s.line(x1, y1, x2, y2, color='#556', width=0.5)
    s.save(path)


def main():
    dv, meshdir, work, logs = sys.argv[1:5]
    lagrit = sys.argv[5] if len(sys.argv) > 5 and sys.argv[5] else None
    names = sorted((f[:-4] for f in os.listdir(meshdir)
                    if f.endswith('.inp') and
                    re.match(r'(tet|tri)_\d+$|dfn_', f[:-4])),
                   key=lambda n: (n.split('_')[0], mesh_size(
                       os.path.join(meshdir, n + '.inp'))[1]))
    results = []
    for n in names:
        t0 = time.perf_counter()
        r = measure(dv, lagrit, meshdir, work, logs, n)
        print('timing_%s %s (%.1f s)' % (n, 'error: ' + r['error'] if
                                        'error' in r else '%d cells, '
                                        'del_vor %.3f s' % (r['cells'],
                                        r['steps']['total'][0]),
                                        time.perf_counter() - t0))
        results.append(r)

    # timing.csv
    csv = os.path.join(logs, 'timing.csv')
    with open(csv, 'w') as f:
        f.write('mesh,vertices,cells,edges,program,step,wall_s,cpu_s,'
                'peak_mb\n')
        for r in results:
            if 'error' in r:
                continue
            base = '%s,%d,%d,%d' % (r['name'], r['vertices'], r['cells'],
                                    r['edges'])
            for k, (w, c) in r['steps'].items():
                f.write('%s,del_vor,%s,%.6f,%.6f,%s\n' % (
                    base, k, w, c, r['peak_mb'] if k == 'total' else ''))
            for tag in ('read', 'read_stor'):
                if 'lagrit_' + tag in r:
                    t, mb = r['lagrit_' + tag]
                    f.write('%s,lagrit,%s,%.6f,,%.1f\n' % (base, tag, t, mb))

    checks_all, figs = [], []
    nfail = 0
    for fam in ('tri', 'tet', 'dfn'):
        rows = [r for r in results if r['name'].startswith(fam) and
                'error' not in r]
        bad = [r for r in results if r['name'].startswith(fam) and
               'error' in r]
        checks = []

        def put(c, v, res):
            checks.append((c, v, res))
            print('timing_%s_%s %s %s' % (fam, c, v, res))
        for r in bad:
            put('ran', '%s: %s' % (r['name'], r['error']), 'FAIL')
        if not rows:
            if bad:
                checks_all.append((fam, checks))
            continue
        big = rows[-1]
        tot = big['steps']['total'][0]
        use = [r for r in rows if r['steps']['total'][0] >= 0.05]
        if len(use) >= 2:
            sl = fit_slope([r['cells'] for r in use],
                           [r['steps']['total'][0] for r in use])
            judge = len(use) >= 3 and tot >= 0.5
            put('slope_total', '%.2f over %d sizes (%d to %d cells)'
                % (sl, len(use), use[0]['cells'], use[-1]['cells']),
                ('PASS' if sl <= 1.2 else 'FAIL') if judge else 'MEASURED')
        else:
            put('slope_total', 'too few sizes over 0.05 s', 'MEASURED')
        steps = [k for k, (w, _) in big['steps'].items()
                 if k != 'total' and w >= 0.05 * tot]
        for k in steps:
            u = [r for r in rows if r['steps'].get(k, (0,))[0] > 0.005]
            if len(u) >= 2:
                put('slope ' + k, '%.2f (%.0f %% of the total at the '
                    'largest size)' % (fit_slope(
                        [r['cells'] for r in u],
                        [r['steps'][k][0] for r in u]),
                        100 * big['steps'][k][0] / tot), 'MEASURED')
        nd = [(r['name'], r['not_delaunay']) for r in rows
              if r['not_delaunay'][0]]
        put('not_delaunay', '; '.join(
            '%s: %d facets with delta/L < -1e-10 (smallest %.2e)'
            % (n, k, w) for n, (k, w) in nd) if nd else
            'every facet Delaunay within 1e-10', 'MEASURED')
        if big['peak_mb']:
            put('memory', '%.0f MB peak at %d cells: %.0f bytes per cell'
                % (big['peak_mb'], big['cells'],
                   big['peak_mb'] * 1e6 / big['cells']), 'MEASURED')
        # del_vor's own peak memory against the one measured from
        # outside (getrusage of the child): the units of ru_maxrss
        # differ between systems, so a mistake shows here
        off = [(r['name'], r['peak_mb'], r['peak_mb_os']) for r in rows
               if r['peak_mb'] and r['peak_mb_os'] and
               not 0.8 <= r['peak_mb'] / r['peak_mb_os'] <= 1.25]
        put('memory_units', 'del_vor\'s peak memory agrees with the one '
            'measured from outside (within 25 %) at every size' if not off
            else '; '.join('%s: del_vor %.1f MB, from outside %.1f MB'
                           % o for o in off), 'FAIL' if off else 'PASS')
        if 'lagrit_read_stor' in big and 'lagrit_read' in big:
            lr, lmb = big['lagrit_read']
            ls, smb = big['lagrit_read_stor']
            dvs = sum(big['steps'].get(k, (0,))[0] for k in STOR_STEPS)
            put('vs_lagrit', 'at %d cells: del_vor total %.2f s (read '
                '%.2f, coefficients and .stor %.2f), peak %.0f MB; '
                'LaGriT read %.2f s, read + dump / stor %.2f s (dump / '
                'stor %.2f), peak %.0f MB'
                % (big['cells'], tot, big['steps']['read'][0], dvs,
                   big['peak_mb'] or 0, lr, ls, ls - lr, smb), 'MEASURED')
        nfail += sum(1 for c in checks if c[2] == 'FAIL')
        keys = ['total'] + [k for k in big['steps'] if k != 'total' and
                            big['steps'][k][0] >= 0.02 * tot]
        f1 = 'steps_%s.svg' % fam
        plot(os.path.join(logs, f1), 'del_vor wall time per step: %s'
             % fam, fam, rows, keys,
             lambda r, k: r['steps'].get(k, (None,))[0])
        figs.append(f1)
        if any('lagrit_read_stor' in r for r in rows):
            f2 = 'lagrit_%s.svg' % fam
            g = {'del_vor total': lambda r: r['steps']['total'][0],
                 'del_vor coefficients + .stor':
                 lambda r: sum(r['steps'].get(k, (0,))[0]
                               for k in STOR_STEPS),
                 'LaGriT read + dump / stor':
                 lambda r: r.get('lagrit_read_stor', (None,))[0],
                 'LaGriT dump / stor':
                 lambda r: (r['lagrit_read_stor'][0] - r['lagrit_read'][0])
                 if 'lagrit_read' in r else None}
            plot(os.path.join(logs, f2), 'del_vor against LaGriT: %s'
                 % fam, fam, rows, list(g), lambda r, k: g[k](r))
            figs.append(f2)
        f3 = 'mesh_%s.svg' % fam
        picture_mesh(os.path.join(logs, f3), 'smallest %s mesh: %s '
                     '(%d cells)' % (fam, rows[0]['name'], rows[0]['cells']),
                     os.path.join(meshdir, rows[0]['name'] + '.inp'))
        figs.append(f3)
        checks_all.append((fam, checks))
    write_page(logs, results, checks_all, figs, nfail, lagrit)
    print('failures %d' % nfail)
    return 1 if nfail else 0


def esc(t):
    return (str(t).replace('&', '&amp;').replace('<', '&lt;')
            .replace('>', '&gt;'))


def write_page(logs, results, checks_all, figs, nfail, lagrit):
    suite = os.path.dirname(os.path.abspath(logs))
    rel = os.path.relpath(logs, suite)
    about = {
        'tri': 'Jittered square grids of triangles made by LaGriT '
        '(make_timing_meshes.sh: createpts, perturb, connect on a '
        'triplane mesh object). Each size runs del_vor -timing on and, '
        'with LaGriT, read / avs alone and read / avs + dump / stor.',
        'tet': 'Jittered cube grids of tets made by LaGriT the same '
        'way. 10 million tets is about 1.6 million vertices and a '
        '0.7 GB .inp.',
        'dfn': 'Fracture networks put in the mesh folder as dfn_*.inp '
        '(dfnWorks full_mesh.inp files).'}
    h = ['<!doctype html><html><head><meta charset="utf-8">',
         '<title>test_timing</title><style>',
         'body{font-family:Helvetica,Arial,sans-serif;margin:24px;'
         'max-width:1250px}',
         'td,th{padding:4px 8px;border-bottom:1px solid #ddd;'
         'font-size:13px;text-align:left;vertical-align:top}',
         'th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:bold}'
         '.FAIL{color:#c00;font-weight:bold}.MEASURED{color:#666}',
         'details summary{cursor:pointer;color:#0969da;font-size:12px}',
         'img{border:1px solid #ccc;margin:4px}</style></head><body>',
         '<p style="font-size:13px"><a href="../summary.html">&larr; all '
         'suites</a></p>',
         '<h1>test_timing: %s</h1>' % ('PASS' if not nfail else 'FAIL'),
         '<p><b>What it does:</b> times del_vor step by step on meshes of '
         'growing size and fits log(time) against log(cells): a slope of '
         '1 is linear. With LaGriT (%s) it also times LaGriT\'s read / avs '
         'and dump / stor on the same meshes. Each time is the best of '
         '%d runs. del_vor runs with -delaunay_tol 1: LaGriT\'s connect '
         'leaves a few facets that are not Delaunay (delta/L down to '
         '-3.6e-4 at a million tets; not_delaunay counts them): with the '
         'defaults a warning, but a bigger mesh might go beyond '
         'delaunay_fatal; the check still runs and is timed. '
         'All numbers: '
         '<a href="%s/timing.csv">timing.csv</a>.</p>'
         % (esc(lagrit) if lagrit else 'not given', REPEAT, rel),
         '<table><tr><th>meshes</th><th>checks</th></tr>']
    for fam, checks in checks_all:
        cl = '<br>'.join('%s: %s <span class="%s">%s</span>'
                         % (esc(c), esc(v), r, r) for c, v, r in checks)
        h.append('<tr><td>%s<details><summary>What it does</summary>'
                 '<p style="max-width:300px">%s</p></details></td>'
                 '<td>%s</td></tr>' % (fam, esc(about[fam]), cl))
    h.append('</table><h2>Sizes</h2><table><tr><th>mesh</th>'
             '<th>vertices</th><th>cells</th><th>edges</th>'
             '<th>del_vor s</th><th>peak MB</th><th>LaGriT read s</th>'
             '<th>LaGriT read + dump/stor s</th></tr>')
    for r in results:
        if 'error' in r:
            h.append('<tr><td>%s</td><td colspan="7" class="FAIL">%s</td>'
                     '</tr>' % (esc(r['name']), esc(r['error'])))
            continue
        h.append('<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td>'
                 '<td>%.3f</td><td>%.0f</td><td>%s</td><td>%s</td></tr>'
                 % (r['name'], r['vertices'], r['cells'], r['edges'],
                    r['steps']['total'][0], r['peak_mb'] or 0,
                    '%.3f' % r['lagrit_read'][0] if 'lagrit_read' in r
                    else '', '%.3f' % r['lagrit_read_stor'][0]
                    if 'lagrit_read_stor' in r else ''))
    h.append('</table><h2>Pictures</h2><p>')
    for f in figs:
        h.append('<a href="%s/%s"><img src="%s/%s" width="400"></a>'
                 % (rel, f, rel, f))
    h.append('</p><p>Run logs: <a href="%s/">%s/</a></p></body></html>'
             % (rel, rel))
    with open(os.path.join(suite, 'summary.html'), 'w') as f:
        f.write('\n'.join(h) + '\n')


if __name__ == '__main__':
    sys.exit(main())
