#!/bin/sh
#
# run_parity_tests.sh [path-to-del_vor]
#
# test_parity: del_vor's .stor against LaGriT's .stor of the same mesh,
# one case per line of cases.txt; every difference must be explained
# by ../../../tools/stor_compare.py. See 00README.
#
# Big cases (mesh path big/...) use the folder DEL_VOR_BIG (default
# ~/lagrit_work/del_vor_dev/big_inputs), outside git; without their
# files they are SKIPPED. LaGriT is not needed here: its .stor files
# were made by make_parity_gold.sh.
# POSIX sh, Python 3 (standard library). WORK and LOGS may be set to
# other folders.

# Default: the del_vor built in this checkout (build/del_vor), else
# del_vor on PATH; or pass a path as the first argument.
_root=$(cd "$(dirname "$0")/../../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    DEL_VOR=$1
elif [ -n "$_root" ] && [ -x "$_root/build/del_vor" ]; then
    DEL_VOR=$_root/build/del_vor
else
    DEL_VOR=del_vor
fi
case $DEL_VOR in
    */*) [ -f "$DEL_VOR" ] &&
         DEL_VOR="$(cd "$(dirname "$DEL_VOR")" && pwd)/$(basename "$DEL_VOR")" ;;
    *)   _p=$(command -v "$DEL_VOR" 2>/dev/null) && DEL_VOR=$_p ;;
esac
if [ ! -f "$DEL_VOR" ] || [ ! -x "$DEL_VOR" ]; then
    echo "cannot run del_vor '$DEL_VOR': not an executable file," >&2
    echo "nor a command on PATH. Pass a path, e.g. ../../../build/del_vor" >&2
    exit 2
fi
BIG=${DEL_VOR_BIG:-$HOME/lagrit_work/del_vor_dev/big_inputs}

# Every rm below is relative to this directory: go there, and stop if
# this is not it.
cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_parity_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "del_vor : $DEL_VOR"
echo "big     : $BIG"
grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name mesh about; do
    name=$(echo "$name" | tr -d ' ')
    mesh=$(echo "$mesh" | tr -d ' ')
    case $mesh in
        big/*) src=$BIG/${mesh#big/}; gold=$BIG/parity ;;
        *)     src=$mesh; gold=inputs ;;
    esac
    [ -f "$src" ] && [ -f "$gold/${name}_lg.stor" ] || continue
    d=$WORK/$name
    mkdir -p "$d"
    cp "$src" "$d/$name.inp"
    cp "$gold/${name}_lg.stor" "$d/"
    [ -f "$gold/${name}_lg_none.stor" ] && cp "$gold/${name}_lg_none.stor" "$d/"
    ( cd "$d" &&
      "$DEL_VOR" -avs "$name.inp" -stor "${name}_keep.stor" \
          -zero_area keep -compress off > run_keep.out 2>&1
      echo $? > run_keep.rc
      "$DEL_VOR" -avs "$name.inp" -stor "$name.stor" > run.out 2>&1
      echo $? > run.rc )
    for r in keep ""; do
        f=$d/run${r:+_$r}.out
        { echo "del_vor -avs $name.inp -stor ${name}${r:+_$r}.stor" \
               "${r:+-zero_area keep -compress off}"
          cat "$f"; } > "$LOGDIR/$name${r:+_$r}.out"
    done
done
python3 -I check_parity.py "$WORK" "$LOGDIR" "$BIG"
rc=$?
echo
exit $rc
