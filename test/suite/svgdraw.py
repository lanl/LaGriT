"""svgdraw.py: small hand-written SVG pictures for the test pages.

Python 3 standard library only. Used by the checkers in each suite:

    from svgdraw import Svg, ramp, Projector

Svg       a picture: lines, polygons, circles, text, a color legend.
ramp      a color for a value in [0, 1] (dark blue -> teal -> yellow).
Projector maps 3D points to the picture: an oblique view of a box,
          scaled to fit a given area.
"""

import math

_RAMP = [(0.00, (49, 54, 149)), (0.25, (69, 117, 180)),
         (0.50, (38, 166, 154)), (0.75, (171, 221, 64)),
         (1.00, (253, 231, 37))]


def ramp(t):
    """Color for t in [0, 1] as '#rrggbb'."""
    t = min(1.0, max(0.0, t))
    for (t0, c0), (t1, c1) in zip(_RAMP, _RAMP[1:]):
        if t <= t1:
            u = (t - t0) / (t1 - t0) if t1 > t0 else 0.0
            c = [round(a + u * (b - a)) for a, b in zip(c0, c1)]
            return '#%02x%02x%02x' % tuple(c)
    return '#%02x%02x%02x' % _RAMP[-1][1]


def esc(s):
    return (str(s).replace('&', '&amp;').replace('<', '&lt;')
            .replace('>', '&gt;'))


class Svg:
    def __init__(self, width, height):
        self.w = width
        self.h = height
        self.items = []

    def line(self, x1, y1, x2, y2, color='#555', width=1.0, dash=None,
             opacity=1.0):
        d = ' stroke-dasharray="%s"' % dash if dash else ''
        self.items.append(
            '<line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" stroke="%s" '
            'stroke-width="%.2f" stroke-opacity="%.2f"%s/>'
            % (x1, y1, x2, y2, color, width, opacity, d))

    def polyline(self, pts, color='#555', width=1.0, dash=None):
        d = ' stroke-dasharray="%s"' % dash if dash else ''
        p = ' '.join('%.2f,%.2f' % q for q in pts)
        self.items.append('<polyline points="%s" fill="none" stroke="%s" '
                          'stroke-width="%.2f"%s/>' % (p, color, width, d))

    def polygon(self, pts, fill='#ccc', stroke='none', width=1.0,
                opacity=1.0):
        p = ' '.join('%.2f,%.2f' % q for q in pts)
        self.items.append('<polygon points="%s" fill="%s" fill-opacity='
                          '"%.2f" stroke="%s" stroke-width="%.2f"/>'
                          % (p, fill, opacity, stroke, width))

    def circle(self, x, y, r, fill='#000', stroke='none', width=1.0):
        self.items.append('<circle cx="%.2f" cy="%.2f" r="%.2f" fill="%s" '
                          'stroke="%s" stroke-width="%.2f"/>'
                          % (x, y, r, fill, stroke, width))

    def rect(self, x, y, w, h, fill='#eee', stroke='none'):
        self.items.append('<rect x="%.2f" y="%.2f" width="%.2f" '
                          'height="%.2f" fill="%s" stroke="%s"/>'
                          % (x, y, w, h, fill, stroke))

    def text(self, x, y, s, size=12, anchor='start', color='#222',
             bold=False):
        b = ' font-weight="bold"' if bold else ''
        self.items.append('<text x="%.2f" y="%.2f" font-size="%d" '
                          'text-anchor="%s" fill="%s" font-family='
                          '"Helvetica, Arial, sans-serif"%s>%s</text>'
                          % (x, y, size, anchor, color, b, esc(s)))

    def legend(self, x, y, w, h, lo_label, hi_label, title=''):
        """A horizontal color bar from ramp(0) to ramp(1)."""
        n = 40
        for k in range(n):
            self.rect(x + k * w / n, y, w / n + 0.5, h, fill=ramp(k / (n - 1)))
        self.text(x, y + h + 13, lo_label, size=11)
        self.text(x + w, y + h + 13, hi_label, size=11, anchor='end')
        if title:
            self.text(x, y - 5, title, size=11)

    def save(self, path):
        with open(path, 'w') as f:
            f.write('<svg xmlns="http://www.w3.org/2000/svg" width="%d" '
                    'height="%d" viewBox="0 0 %d %d">\n'
                    % (self.w, self.h, self.w, self.h))
            f.write('<rect width="100%" height="100%" fill="white"/>\n')
            for it in self.items:
                f.write(it + '\n')
            f.write('</svg>\n')


class Projector:
    """Oblique view of 3D points, scaled into the box (x0, y0, w, h).

    view='plan' looks down z (x right, y up); view='iso' turns the
    box 35 degrees about z and looks down on it from 25 degrees above,
    so all three axes show and no two corners of a box coincide.
    """

    def __init__(self, pts, x0, y0, w, h, view='iso'):
        self.view = view
        q = [self._raw(p) for p in pts]
        xs = [a for a, _ in q]
        ys = [b for _, b in q]
        self.xmin, xmax = min(xs), max(xs)
        self.ymin, ymax = min(ys), max(ys)
        sx = (xmax - self.xmin) or 1.0
        sy = (ymax - self.ymin) or 1.0
        self.s = min(w / sx, h / sy)
        self.x0 = x0 + 0.5 * (w - self.s * sx)
        self.y0 = y0 + 0.5 * (h - self.s * sy)
        self.hh = self.s * sy

    def _raw(self, p):
        x, y, z = p
        if self.view == 'plan':
            return x, y
        a, e = math.radians(35), math.radians(25)
        u = math.cos(a) * x - math.sin(a) * y
        depth = math.sin(a) * x + math.cos(a) * y
        v = math.sin(e) * depth + math.cos(e) * z
        return u, v

    def __call__(self, p):
        u, v = self._raw(p)
        return (self.x0 + self.s * (u - self.xmin),
                self.y0 + self.hh - self.s * (v - self.ymin))
