#!/bin/sh
#
# make_parity_gold.sh /path/to/lagrit [BIG_FOLDER]
#
# Makes the LaGriT .stor files of test_parity: for every mesh of
# cases.txt, LaGriT reads the same .inp del_vor reads and writes
#   NAME_lg.stor        dump / stor / NAME_lg / mo   (LaGriT's default)
#   NAME_lg_none.stor   dump / stor / NAME_lg_none / mo / ascii / none
# into inputs/, with the deck (inputs/gold.lgi), LaGriT's screen output
# (inputs/gold.out) and LaGriT's version (inputs/LAGRIT_VERSION).
# It also makes the generated meshes (two_mat.inp by LaGriT, utm_tet.inp
# by make_utm_mesh.py).
#
# With BIG_FOLDER (or DEL_VOR_BIG set): also the big cases (cases with
# a mesh path starting with big/), into BIG_FOLDER/parity/.
#
# The suite itself does not need LaGriT: run_parity_tests.sh compares
# del_vor with the files made here. Run this only to remake them (a
# new LaGriT, a new case). About 1 minute (more with the big cases).
# POSIX sh.

LAGRIT=${1:-}
BIG=${2:-${DEL_VOR_BIG:-}}
[ -n "$LAGRIT" ] && [ -x "$LAGRIT" ] || {
    echo "usage: sh make_parity_gold.sh /path/to/lagrit [BIG_FOLDER]" >&2
    exit 2; }
case $LAGRIT in /*) ;; *) LAGRIT=$(pwd)/$LAGRIT ;; esac
cd "$(dirname "$0")" || exit 2
[ -f ./make_parity_gold.sh ] || { echo "not in test_parity" >&2; exit 2; }
HERE=$(pwd)
IN=$HERE/inputs
T=$HERE/work_gold
rm -rf "$T"
mkdir -p "$T" "$IN"

# the generated meshes
cat > "$T/two_mat.lgi" <<'EOF'
* two_mat: a jittered 7 x 7 x 5 grid, imt1 2 above z = 0.31,
* connected (LaGriT puts the material interface on tet faces)
cmo / create / mo1 / / / tet
createpts / xyz / 7 7 5 / 0. 0. 0. / 1. 1. 0.6 / 1 1 1
cmo / setatt / mo1 / imt1 / 1 0 0 / 1
pset / ptop / attribute / zic / 1 0 0 / gt 0.31
cmo / setatt / mo1 / imt1 / pset get ptop / 2
pset / pin / geom / xyz / 1 0 0 / 0.05 0.05 0.05 / 0.95 0.95 0.55
perturb / pset get pin / 0.03 0.03 0.03
connect
dump / avs / two_mat.inp / mo1
finish
EOF
( cd "$T" && "$LAGRIT" < two_mat.lgi > two_mat.out 2>&1 ) ||
    { echo "LaGriT failed on two_mat.lgi; see $T/two_mat.out" >&2; exit 1; }
cp "$T/two_mat.inp" "$T/two_mat.lgi" "$IN/"
python3 -I make_utm_mesh.py ../test_poisson_lagrit/inputs/test_3d_poi_01.inp \
    "$IN/utm_tet.inp" || exit 1

# one deck for every case: read, dump / stor twice, release
DECK=$T/gold.lgi
: > "$DECK"
grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name mesh about; do
    name=$(echo "$name" | tr -d ' ')
    mesh=$(echo "$mesh" | tr -d ' ')
    case $mesh in
        big/*) [ -n "$BIG" ] || continue
               src=$BIG/${mesh#big/} ;;
        *) src=$mesh ;;
    esac
    [ -f "$src" ] || { echo "missing mesh $src" >&2; exit 1; }
    cp "$src" "$T/$name.inp"
    cat >> "$DECK" <<EOF
read / avs / $name.inp / mo_$name
dump / stor / ${name}_lg / mo_$name
dump / stor / ${name}_lg_none / mo_$name / ascii / none
cmo / release / mo_$name
EOF
done || exit 1
echo "finish" >> "$DECK"
( cd "$T" && "$LAGRIT" < gold.lgi > gold.out 2>&1 ) ||
    { echo "LaGriT failed on gold.lgi; see $T/gold.out" >&2; exit 1; }

# keep the results: small cases in inputs/, big ones in BIG/parity/
grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name mesh about; do
    name=$(echo "$name" | tr -d ' ')
    mesh=$(echo "$mesh" | tr -d ' ')
    case $mesh in
        big/*) [ -n "$BIG" ] || continue
               mkdir -p "$BIG/parity"; dest=$BIG/parity ;;
        *) dest=$IN ;;
    esac
    for f in ${name}_lg.stor ${name}_lg_none.stor; do
        [ -f "$T/$f" ] || { echo "LaGriT wrote no $f" >&2; exit 1; }
    done
    cp "$T/${name}_lg.stor" "$dest/"
    rm -f "$dest/${name}_lg_none.stor"
    # LaGriT's 2D writer does not compress: keep one copy when the two
    # are the same (apart from the date line)
    tail -n +3 "$T/${name}_lg_none.stor" > "$T/n.body"
    tail -n +3 "$T/${name}_lg.stor" > "$T/a.body"
    cmp -s "$T/n.body" "$T/a.body" ||
        cp "$T/${name}_lg_none.stor" "$dest/"
done || exit 1
cp "$T/gold.lgi" "$T/gold.out" "$IN/"
grep -m1 "Program:" "$T/gold.out" | tr -s ' *' ' ' | sed 's/^ //' \
    > "$IN/LAGRIT_VERSION"
src=$(cd "$(dirname "$LAGRIT")/.." 2>/dev/null &&
      git log -1 --format='lanl/LaGriT %h %cd %s' --date=short 2>/dev/null)
[ -n "$src" ] && echo "$src" >> "$IN/LAGRIT_VERSION"
rm -rf "$T"
echo "make_parity_gold: done; LaGriT: $(head -1 "$IN/LAGRIT_VERSION")"
