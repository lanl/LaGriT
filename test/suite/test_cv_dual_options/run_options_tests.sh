#!/bin/sh
#
# run_options_tests.sh [path-to-cv_dual]
#
# test_cv_dual_options: cv_dual's keyword options (outputs, zero_area,
# zero_tol, compress, cv, lanl/voronoi's -type -o -compress), and
# refused commands, one case per line of cases.txt. See 00README.
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

# Every rm below is relative to this directory: go there, and stop if
# this is not it.
cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_options_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "cv_dual : $CV_DUAL"
G=../test_cv_dual_voronoi_gold/inputs
# mesh_3D with two vertex materials, the attribute named imt1 and
# integer_material_type (the imt_short and imt_long cases)
python3 -I make_imt_inputs.py "$G/mesh_3D.inp" "$WORK" || exit 2
grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name expect args about; do
    name=$(echo "$name" | tr -d ' ')
    mkdir -p "$WORK/$name"
    cp "$G/mesh_2D.inp" "$G/mesh_3D.inp" "$G/simple_3D.inp" \
       "$WORK/imt_short.inp" "$WORK/imt_long.inp" "$WORK/$name"/
    # shellcheck disable=SC2086
    ( cd "$WORK/$name" && "$CV_DUAL" $args > run.out 2>&1
      echo $? > run.rc )
    { echo "cv_dual$args"; cat "$WORK/$name/run.out"; } \
        > "$LOGDIR/$name.out"
done
python3 -I check_options.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
