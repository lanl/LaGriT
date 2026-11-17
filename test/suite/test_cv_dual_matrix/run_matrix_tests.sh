#!/bin/sh
#
# run_matrix_tests.sh [path-to-cv_dual]
#
# test_cv_dual_matrix: the matrix file (matrix FILE, matrix_bins N), one case
# per line of cases.txt, read back by test_cv_dual_paraview's vtu_read.py and
# checked against the .stor and the mesh. See 00README.
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
if [ ! -f "./run_matrix_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "cv_dual : $CV_DUAL"
grep -v '^#' cases.txt | grep '|' | while IFS='|' read -r name mesh args about; do
    name=$(echo "$name" | tr -d ' ')
    mesh=$(echo "$mesh" | tr -d ' ')
    mkdir -p "$WORK/$name"
    cp "$mesh" "$WORK/$name/"
    m=$(basename "$mesh")
    # the ParaView files too: the mesh file's alpha and the faces
    # file's vector areas are checked against the matrix file
    # shellcheck disable=SC2086
    ( cd "$WORK/$name" && "$CV_DUAL" -avs "$m" -stor "$name.stor" \
          -matrix "${name}_matrix.vtu" -paraview "$name" \
          -paraview_faces on -report "$name.report" $args \
          > run.out 2>&1 < /dev/null
      echo $? > run.rc )
    { echo "cv_dual -avs $m -stor $name.stor -matrix ${name}_matrix.vtu -paraview $name -paraview_faces on -report $name.report $args"
      cat "$WORK/$name/run.out"; } > "$LOGDIR/$name.out"
done
python3 -I check_matrix.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
