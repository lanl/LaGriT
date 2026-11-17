"""make_imt_inputs.py MESH_3D.inp OUTDIR

Writes OUTDIR/imt_short.inp and OUTDIR/imt_long.inp: mesh_3D.inp with
its vertex material set to 1 or 2 (by vertex number), the attribute
named imt1 in the first and integer_material_type in the second. The
imt_short and imt_long cases run both: the TOUGH2 files must be the
same, with 2 materials. Python 3 standard library."""
import os
import sys


def main():
    src, out = sys.argv[1], sys.argv[2]
    lines = open(src).read().split('\n')
    nn, nc = (int(x) for x in lines[0].split()[:2])
    k = 1 + nn + nc                     # the vertex data header
    names = lines[k + 1:k + 5]
    assert names[0].startswith('imt1'), names
    body = []
    for i, ln in enumerate(lines[k + 5:k + 5 + nn]):
        t = ln.split()
        t[1] = '%d' % (1 + i % 2)
        body.append('  '.join(t))
    for fname, label in (('imt_short.inp', 'imt1, integer'),
                         ('imt_long.inp', 'integer_material_type, integer')):
        new = (lines[:k + 1] + [label] + names[1:] + body +
               lines[k + 5 + nn:])
        with open(os.path.join(out, fname), 'w') as f:
            f.write('\n'.join(new))


if __name__ == '__main__':
    main()
