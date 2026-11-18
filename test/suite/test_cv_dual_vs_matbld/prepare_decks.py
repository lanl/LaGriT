"""prepare_decks.py DECK DIR_MATBLD DIR_CV_DUAL

test_cv_dual_vs_matbld: writes two copies of a LaGriT deck, input.lgi
in each folder. Before every dump / stor, fehm, pflotran or
zone_outside line both copies get

    dump / avs / _mesh_ROOT.inp / MO

(the mesh as the dump command sees it, for stor_compare's zero-area
and coordinate rules; ROOT the dump's file root, MO its mesh object,
or the current one). In the cv_dual copy each such dump line ends with
/ cv_dual. Every other line is copied as it is. Prints the dump lines
found. Python 3 standard library.
"""
import re
import sys

KINDS = ('stor', 'fehm', 'pflotran', 'pflo', 'zone_outside')
KEYWORDS = {'ascii', 'binary', 'unformatted', 'scalar', 'vector', 'both',
            'area_scalar', 'area_vector', 'area_both', 'graph', 'coefs',
            'none', 'all', 'keepatt', 'delatt', 'keepatt_voronoi',
            'keepatt_median', 'keepatt_area', 'hybrid', 'nohybrid',
            'nofilter_zero', 'filter_zero', 'verbose', '-def-'}


def dump_line(line):
    """(kind, root, mo) of a dump line this suite changes, else None."""
    s = line.strip()
    if not s or s[0] in '#*':
        return None
    t = [w for w in re.split(r'[\s/,]+', s) if w]
    if len(t) < 3 or t[0].lower() != 'dump' or t[1].lower() not in KINDS:
        return None
    mo = t[3] if len(t) > 3 and t[3].lower() not in KEYWORDS else ''
    return t[1].lower(), t[2], mo


def main():
    deck, d_mb, d_cv = sys.argv[1:4]
    mb, cv = [], []
    for line in open(deck):
        line = line.rstrip('\n')
        dl = dump_line(line)
        if dl:
            kind, root, mo = dl
            av = 'dump / avs / _mesh_%s.inp%s' % (root, ' / ' + mo
                                                   if mo else '')
            mb += [av, line]
            cv += [av, line.rstrip() + ' / cv_dual']
            print('%s %s %s' % (kind, root, mo or '(current)'))
        else:
            mb.append(line)
            cv.append(line)
    for d, lines in ((d_mb, mb), (d_cv, cv)):
        with open(d + '/input.lgi', 'w') as f:
            f.write('\n'.join(lines) + '\n')


if __name__ == '__main__':
    main()
