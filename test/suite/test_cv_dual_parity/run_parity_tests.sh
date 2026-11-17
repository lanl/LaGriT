#!/bin/sh
#
# run_parity_tests.sh [path-to-cv_dual]
#
# test_cv_dual_parity: cv_dual's .stor against LaGriT's .stor of the same mesh,
# one case per line of cases.txt; every difference must be explained
# by ../cv_dual_tools/stor_compare.py. See 00README.
#
# Big cases (mesh path big/...) use the folder CV_DUAL_BIG (default
# ~/lagrit_work/del_vor_dev/big_inputs), outside git; without their
# files they are SKIPPED. LaGriT is not needed here: its .stor files
# were made by make_parity_gold.sh.
# POSIX sh, Python 3 (standard library). WORK and LOGS may be set to
# other folders.

# Default: the cv_dual built in this checkout (build/cv_dual), else
# cv_dual on PATH; or pass a path as the first argument.
_root=$(cd "$(dirname "$0")/../../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    CV_DUAL=$1
    # from LaGriT's run_all.sh: lagrit's path; cv_dual is beside it
    case $(basename "$1") in
        cv_dual*) ;;
        *) CV_DUAL=$(dirname "$1")/cv_dual ;;
    esac
elif [ -n "$_root" ] && [ -x "$_root/build/cv_dual" ]; then
    CV_DUAL=$_root/build/cv_dual
else
    CV_DUAL=cv_dual
fi
case $CV_DUAL in
    */*) [ -f "$CV_DUAL" ] &&
         CV_DUAL="$(cd "$(dirname "$CV_DUAL")" && pwd)/$(basename "$CV_DUAL")" ;;
    *)   _p=$(command -v "$CV_DUAL" 2>/dev/null) && CV_DUAL=$_p ;;
esac
if [ ! -f "$CV_DUAL" ] || [ ! -x "$CV_DUAL" ]; then
    echo "cannot run cv_dual '$CV_DUAL': not an executable file," >&2
    echo "nor a command on PATH. Pass a path, e.g. ../../../build/cv_dual" >&2
    exit 2
fi
BIG=${CV_DUAL_BIG:-$HOME/lagrit_work/del_vor_dev/big_inputs}

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

echo "cv_dual : $CV_DUAL"
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
      "$CV_DUAL" -avs "$name.inp" -stor "${name}_keep.stor" \
          -zero_area keep -compress off > run_keep.out 2>&1
      echo $? > run_keep.rc
      "$CV_DUAL" -avs "$name.inp" -stor "$name.stor" > run.out 2>&1
      echo $? > run.rc )
    for r in keep ""; do
        f=$d/run${r:+_$r}.out
        { echo "cv_dual -avs $name.inp -stor ${name}${r:+_$r}.stor" \
               "${r:+-zero_area keep -compress off}"
          cat "$f"; } > "$LOGDIR/$name${r:+_$r}.out"
    done
done
python3 -I check_parity.py "$WORK" "$LOGDIR" "$BIG"
rc=$?
echo
exit $rc
