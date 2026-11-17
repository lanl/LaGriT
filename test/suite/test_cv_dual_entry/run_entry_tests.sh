#!/bin/sh
#
# run_entry_tests.sh [path-to-del_vor]
#
# test_entry: the LaGriT entry point, del_vor_lagrit_entry, called by
# the harness del_vor_entry_test (built next to del_vor) the way LaGriT
# will call it; each case of cases.txt compared with the program. See
# 00README. POSIX sh, Python 3 (standard library). WORK and LOGS may
# be set to other folders.

# Default: the del_vor built in this checkout (build/del_vor), else
# del_vor on PATH; or pass a path as the first argument. The harness
# is the del_vor_entry_test in the same folder.
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
HARNESS=$(dirname "$DEL_VOR")/del_vor_entry_test
if [ ! -x "$HARNESS" ]; then
    echo "no harness $HARNESS (built with del_vor by CMake)" >&2
    exit 2
fi

# Every rm below is relative to this directory: go there, and stop if
# this is not it.
cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_entry_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
rm -f summary.html

echo "del_vor : $DEL_VOR"
echo "harness : $HARNESS"
# meshes borrowed from two other suites' generators
python3 -I ../test_validity/make_validity_inputs.py "$WORK/gen_v" \
    > "$LOGDIR/make_inputs.out" || exit 1
python3 -I ../test_delaunay/make_delaunay_inputs.py "$WORK/gen_d" \
    >> "$LOGDIR/make_inputs.out" || exit 1

grep -v '^#' cases.txt | grep '|' |
while IFS='|' read -r name mesh args flags calls ierr msg about; do
    name=$(echo "$name" | tr -d ' ')
    mesh=$(echo "$mesh" | tr -d ' ')
    case $mesh in work/*) mesh=$WORK/${mesh#work/} ;; esac
    args=$(echo "$args" | sed 's/^ *//;s/ *$//')
    flags=$(echo "$flags" | sed 's/^ *//;s/ *$//')
    calls=$(echo "$calls" | sed 's/^ *//;s/ *$//')
    m=$(basename "$mesh")
    if [ "$args" != "-" ]; then
        mkdir -p "$WORK/$name/prog"
        cp "$mesh" "$WORK/$name/prog/"
        # shellcheck disable=SC2086
        ( cd "$WORK/$name/prog" && "$DEL_VOR" -avs "$m" $args \
              > run.out 2>&1 < /dev/null
          echo $? > run.rc )
    fi
    mkdir -p "$WORK/$name/entry"
    cp "$mesh" "$WORK/$name/entry/"
    set -- "$m" vol.txt
    # shellcheck disable=SC2086
    [ "$flags" != "-" ] && set -- "$@" $flags
    rest=$calls
    while [ -n "$rest" ]; do
        c=${rest%%;*}
        if [ "$c" = "$rest" ]; then rest=; else rest=${rest#*;}; fi
        c=$(echo "$c" | sed 's/^ *//;s/ *$//')
        [ "$c" = "=" ] && c=$args
        set -- "$@" "$c"
    done
    ( cd "$WORK/$name/entry" && "$HARNESS" "$@" > run.out 2>&1 < /dev/null
      echo $? > run.rc )
    { echo "del_vor_entry_test $*"
      cat "$WORK/$name/entry/run.out"; } > "$LOGDIR/$name.out"
done
python3 -I check_entry.py "$WORK" "$LOGDIR"
rc=$?
echo
exit $rc
