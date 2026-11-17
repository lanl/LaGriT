"""pagekit.py: the summary.html of a suite whose tests are cases, one
row per case: its name with a "What it does" note that opens, the
command, every check with its value and result, a picture. Python 3
standard library. Used by test_delaunay, test_validity, test_dfn.

    from pagekit import read_cases, write_case_page
"""

import os


def esc(t):
    return (str(t).replace('&', '&amp;').replace('<', '&lt;')
            .replace('>', '&gt;'))


def read_cases(path, nfields):
    """cases.txt: '|'-separated lines, # comments; the last field (what
    it does) may hold '|' no more."""
    cases = []
    with open(path) as f:
        for ln in f:
            if ln.startswith('#') or '|' not in ln:
                continue
            cases.append([t.strip() for t in ln.split('|', nfields - 1)])
    return cases


def write_case_page(suite_dir, logs, title, what_html, rows, extra_html=''):
    """rows: (name, about, command, [(check, value, result)], picture
    file name in logs or None). Writes suite_dir/summary.html. Returns
    the number of failed checks."""
    rel = os.path.relpath(logs, suite_dir)
    nfail = sum(1 for r in rows for c in r[3] if c[2] == 'FAIL')
    nfail += sum(1 for r in rows if not r[3])
    h = ['<!doctype html><html><head><meta charset="utf-8">',
         '<title>%s</title><style>' % esc(title),
         'body{font-family:Helvetica,Arial,sans-serif;margin:24px;'
         'max-width:1250px}',
         'td,th{padding:4px 8px;border-bottom:1px solid #ddd;'
         'font-size:13px;text-align:left;vertical-align:top}',
         'th{background:#f6f8fa}',
         '.PASS{color:#1a7f37;font-weight:bold}'
         '.FAIL{color:#c00;font-weight:bold}'
         '.MEASURED{color:#666}',
         'details summary{cursor:pointer;color:#0969da;font-size:12px}',
         'details p{max-width:300px;margin:4px 0}',
         'code{font-size:12px;background:#f6f8fa}',
         'img{border:1px solid #ccc}</style></head><body>',
         '<p style="font-size:13px"><a href="../summary.html">&larr; all '
         'suites</a></p>',
         '<h1>%s: %s</h1>' % (esc(title), 'PASS' if not nfail else 'FAIL'),
         '<p><b>What it does:</b> %s</p>' % what_html, extra_html,
         '<table><tr><th>case</th><th>command</th><th>checks</th>'
         '<th>result</th><th>picture (click to enlarge)</th></tr>']
    for name, about, cmd, checks, pic in rows:
        res = 'FAIL' if (not checks or any(c[2] == 'FAIL'
                                            for c in checks)) else 'PASS'
        cl = '<br>'.join('%s: %s <span class="%s">%s</span>'
                         % (esc(c), esc(v), r, r) for c, v, r in checks)
        img = ('<a href="%s/%s"><img src="%s/%s" width="300"></a>'
               % (rel, pic, rel, pic)) if pic else ''
        h.append('<tr><td>%s<details><summary>What it does</summary>'
                 '<p>%s</p></details></td><td><code>%s</code></td>'
                 '<td>%s</td><td class="%s">%s</td><td>%s</td></tr>'
                 % (esc(name), esc(about), esc(cmd), cl, res, res, img))
    h.append('</table><p>Run logs (each with its command and screen '
             'output): <a href="%s/">%s/</a></p></body></html>'
             % (rel, rel))
    with open(os.path.join(suite_dir, 'summary.html'), 'w') as f:
        f.write('\n'.join(h) + '\n')
    return nfail
