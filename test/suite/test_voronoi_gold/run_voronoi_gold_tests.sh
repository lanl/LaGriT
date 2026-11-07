#!/bin/sh
#
# run_voronoi_gold_tests.sh [path-to-del_vor]
#
# test_voronoi_gold: del_vor on the three lanl/voronoi test meshes,
# each written as .stor, compressed .stor, .uge and TOUGH2 MESH, judged
# against the gold files and against answers computed from the meshes.
# See 00README. POSIX sh, Python 3 (standard library). WORK and LOGS
# may be set to other folders.

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

# Every rm below is relative to this directory: go there, and stop if
# this is not it.
cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_voronoi_gold_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "del_vor : $DEL_VOR"
rc=0
for m in mesh_2D mesh_3D simple_3D; do
    cp "inputs/$m.inp" "$WORK"/
    ( cd "$WORK" &&
      "$DEL_VOR" -avs "$m.inp" -o "$m.stor" > "$m.stor.out" 2>&1 &&
      "$DEL_VOR" -avs "$m.inp" -o "${m}_c.stor" -compress \
          > "${m}_c.stor.out" 2>&1 &&
      "$DEL_VOR" -avs "$m.inp" -type pflotran -o "$m.uge" \
          > "$m.uge.out" 2>&1 &&
      "$DEL_VOR" -avs "$m.inp" -type tough2 -o "${m}_MESH" \
          > "${m}_MESH.out" 2>&1 ) || { echo "del_vor failed on $m"; rc=1; }
    cp "$WORK/$m".*.out "$WORK/${m}"_*.out "$LOGDIR"/ 2>/dev/null
done
python3 -I check_voronoi_gold.py "$WORK" "$LOGDIR" inputs || rc=1
echo
exit $rc
