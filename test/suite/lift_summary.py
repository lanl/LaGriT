#!/usr/bin/env python3
"""lift_summary.py SUITE_DIR

Each suite's checker writes its page as SUITE_DIR/logs/summary.html.
This puts the page one folder up, at SUITE_DIR/summary.html, where it is
easy to find (and where test/suite/summary.html links to it):

  - relative links and pictures in the page (href="x.check",
    src="pics/a.svg") get logs/ in front, so they still point into logs/;
  - ../../summary.html (the top-level page) becomes ../summary.html, and
    links up into the suite folder (../x) become x;
  - a link back to the top-level page is added when the page has none;
  - a page with no "What it does" paragraph gets one from
    SUITE_DIR/what_it_does.txt, after its heading;
  - logs/summary.html becomes a one-line page that forwards to the new
    place, so an old bookmark still works;
  - links in other pages in logs/ (demo.html) back to summary.html are
    pointed at ../summary.html.

Run by every suite runner on exit (a trap) and so also when a suite is
run alone. Python 3, standard library. Exit 0 always: a missing page is
reported, not an error (the runner's own exit status is what counts).
"""
import html
import os
import re
import sys

STUB = "lift_summary.py: forwarded"
BACK = ('<p style="margin:0 0 8px;font-size:13px"><a href="../summary.html">'
        '&larr; all suites (top-level summary)</a></p>')
REF = re.compile(r'\b(href|src)="([^"]*)"')


def fix(m):
    attr, url = m.group(1), m.group(2)
    if (not url or url.startswith(("#", "/", "data:", "mailto:", "javascript:"))
            or re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*:", url)):
        return m.group(0)
    if url.startswith("../../"):            # the top-level page and above
        url = url[3:]
    elif url.startswith("../"):             # the suite folder
        url = url[3:]
    else:                                   # in logs/
        url = "logs/" + url
    return '%s="%s"' % (attr, url)


def main():
    suite = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else ".")
    logs = os.path.join(suite, "logs")
    src = os.path.join(logs, "summary.html")
    dst = os.path.join(suite, "summary.html")
    if not os.path.isfile(src):
        if os.path.isfile(dst):
            os.remove(dst)                  # no stale page from a past run
        print("summary page: none written (see %s/logs/)" % os.path.basename(suite))
        return
    page = open(src, encoding="utf-8", errors="replace").read()
    if STUB in page:                        # already lifted (a second call)
        print("summary page: %s/summary.html" % os.path.basename(suite))
        return
    page = REF.sub(fix, page)
    what = os.path.join(suite, "what_it_does.txt")
    if "What it does" not in page and os.path.isfile(what):
        txt = open(what, encoding="utf-8").read().strip()
        para = ('<p style="font-size:14px;line-height:1.45;max-width:1000px">'
                '<b>What it does:</b> %s</p>' % html.escape(txt))
        if "</h1>" in page:
            page = page.replace("</h1>", "</h1>\n" + para, 1)
        else:
            page = re.sub(r"(<body[^>]*>)", r"\1\n" + para, page, count=1)
    if 'href="../summary.html"' not in page:
        page = re.sub(r"(<body[^>]*>)", r"\1\n" + BACK, page, count=1)
    with open(dst, "w", encoding="utf-8") as f:
        f.write(page)
    with open(src, "w", encoding="utf-8") as f:
        f.write('<!doctype html><html><head><meta charset="utf-8">'
                '<meta http-equiv="refresh" content="0; url=../summary.html">'
                '<!-- %s --></head><body><p>This page is now '
                '<a href="../summary.html">one folder up</a>.</p></body></html>\n' % STUB)
    for fn in os.listdir(logs):             # other pages in logs/
        if fn.endswith(".html") and fn != "summary.html":
            p = os.path.join(logs, fn)
            s = open(p, encoding="utf-8", errors="replace").read()
            t = s.replace('href="summary.html"', 'href="../summary.html"')
            if t != s:
                open(p, "w", encoding="utf-8").write(t)
    print("summary page: %s/summary.html" % os.path.basename(suite))


if __name__ == "__main__":
    main()
