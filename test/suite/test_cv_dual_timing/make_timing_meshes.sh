#!/bin/sh
#
# make_timing_meshes.sh /path/to/lagrit OUTDIR
#
# The meshes of test_timing, made by LaGriT (createpts, a jitter of
# the interior vertices, connect, dump / avs), written to OUTDIR as
#   tet_N.inp   about N tets: an n x n x n jittered grid in the unit
#               cube, n = round((N / 6.45)^(1/3))
#   tri_N.inp   about N triangles: an n x n jittered grid in the unit
#               square (a triplane mesh object), n = round((N / 2)^(1/2))
# for the sizes in SIZES_TET and SIZES_TRI (environment; defaults
# below). A mesh already in OUTDIR is not made again. The jitter is a
# fifth of the grid spacing, so the vertices are in general position
# away from the boundary, like a mesh of real points.
#
# Time: LaGriT's connect is about 7 s per million tets on Claude's
# sandbox; a 10-million-tet mesh is about 0.7 GB of .inp.
# POSIX sh.

LAGRIT=${1:-}
OUT=${2:-}
[ -n "$LAGRIT" ] && [ -x "$LAGRIT" ] && [ -n "$OUT" ] || {
    echo "usage: sh make_timing_meshes.sh /path/to/lagrit OUTDIR" >&2
    exit 2; }
SIZES_TET=${SIZES_TET:-"10000 30000 100000 300000 1000000"}
SIZES_TRI=${SIZES_TRI:-"10000 100000 1000000"}
mkdir -p "$OUT" || exit 2

make () {   # make KIND N n
    kind=$1; N=$2; n=$3
    f=$OUT/${kind}_$N.inp
    [ -f "$f" ] && return 0
    d=$OUT/.make_${kind}_$N
    rm -rf "$d"; mkdir -p "$d"
    h=$(python3 -I -c "print(1.0 / ($n - 1))")
    j=$(python3 -I -c "print(0.2 / ($n - 1))")
    e=$(python3 -I -c "print(0.5 / ($n - 1))")
    if [ "$kind" = tet ]; then
        cat > "$d/m.lgi" <<EOF
cmo / create / mo / / / tet
createpts / xyz / $n $n $n / 0. 0. 0. / 1. 1. 1. / 1 1 1
pset / pin / geom / xyz / 1 0 0 / $e $e $e / 1. 1. 1.
pset / pout / geom / xyz / 1 0 0 / 0. 0. 0. / $(python3 -I -c "print(1 - $e)") $(python3 -I -c "print(1 - $e)") $(python3 -I -c "print(1 - $e)")
pset / pmid / inter / pin pout
perturb / pset get pmid / $j $j $j
connect
dump / avs / m.inp / mo
finish
EOF
    else
        cat > "$d/m.lgi" <<EOF
cmo / create / mo / / / triplane
createpts / xyz / $n $n 1 / 0. 0. 0. / 1. 1. 0. / 1 1 1
pset / pin / geom / xyz / 1 0 0 / $e $e -1. / 1. 1. 1.
pset / pout / geom / xyz / 1 0 0 / 0. 0. -1. / $(python3 -I -c "print(1 - $e)") $(python3 -I -c "print(1 - $e)") 1.
pset / pmid / inter / pin pout
perturb / pset get pmid / $j $j 0.
connect
dump / avs / m.inp / mo
finish
EOF
    fi
    echo "make_timing_meshes: ${kind}_$N (n = $n, spacing $h)"
    ( cd "$d" && "$LAGRIT" < m.lgi > m.out 2>&1 ) && [ -s "$d/m.inp" ] ||
        { echo "LaGriT failed; see $d/m.out" >&2; return 1; }
    mv "$d/m.inp" "$f" && rm -rf "$d"
}

for N in $SIZES_TET; do
    n=$(python3 -I -c "print(max(3, round(($N / 6.45) ** (1 / 3))))")
    make tet "$N" "$n" || exit 1
done
for N in $SIZES_TRI; do
    n=$(python3 -I -c "print(max(3, round(($N / 2) ** 0.5)))")
    make tri "$N" "$n" || exit 1
done
echo "make_timing_meshes: done ($OUT)"
