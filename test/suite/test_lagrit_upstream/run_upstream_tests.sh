#!/bin/sh
#######################################################################
# run_upstream_tests.sh [path-to-lagrit]
#
# Runs LaGriT's OWN test suite (the test/level01 folders of the LaGriT
# source tree; test/level02 too with LEVEL2=1, which needs a build
# with Exodus) and judges every test the way LaGriT's runtests.py
# does, plus two checks of ours. See 00README.
#
# Where the tests come from (the first that exists, always printed):
#   $LAGRIT_TESTS              a test/ folder given by hand
#   ../.. from this runner     test/ of the LaGriT checkout holding this
#                              suite (test/suite/test_lagrit_upstream)
#   $LAGDEV/test               another source tree
#   <lagrit's folder>/../test  e.g. $LAGDEV/build/lagrit -> $LAGDEV/test
# The tests are NOT copied into this suite (about 105 MB, and they
# change with LaGriT): each run copies them into work/ and runs there,
# so the source tree is never written to.
#
# Each test:  cd work/<test>;  lagrit -log logx3dgen -out outx3dgen
#             < input.lgi   (as testlagrit/run_lagrit.py does)
# Judged by check_upstream.py:
#   PASS   LaGriT's own diff (testlagrit/diff.py, loaded from the source
#          tree) finds outx3dgen equal to reference/outx3dgen, AND
#          lagrit exited normally (no crash, exit status 0) AND the log
#          ends with "LaGriT successfully completed" (our two checks;
#          LaGriT's runtests.py does not make them)
#   KNOWN  a test listed in known.txt: it failed, as documented there
#   FIXED  a test listed in known.txt now passes: update known.txt
#   FAIL   anything else
# Exit status: nonzero on any FAIL.
#
# POSIX sh. Python 3, standard library only.
#######################################################################

# Default: the lagrit built in this LaGriT checkout (build/lagrit), else
# lagrit on PATH; or pass a path as the first argument.
_root=$(cd "$(dirname "$0")/../../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    LAGRIT=$1
elif [ -n "$_root" ] && [ -x "$_root/build/lagrit" ]; then
    LAGRIT=$_root/build/lagrit
else
    LAGRIT=lagrit
fi

# Fail early and clearly if lagrit cannot be run (otherwise every row
# fails with exit status 127). sh does not see bash aliases or shell
# functions: pass a path, or a name of a file on PATH.
case $LAGRIT in
    */*) [ -f "$LAGRIT" ] &&
         LAGRIT="$(cd "$(dirname "$LAGRIT")" && pwd)/$(basename "$LAGRIT")" ;;
    *)   _p=$(command -v "$LAGRIT" 2>/dev/null) && LAGRIT=$_p ;;
esac
if [ ! -f "$LAGRIT" ] || [ ! -x "$LAGRIT" ]; then
    echo "cannot run lagrit '$LAGRIT': not an executable file," >&2
    echo "nor a command on PATH (a bash alias or function is not" >&2
    echo "visible to sh). Pass a path, e.g. ../../build/lagrit" >&2
    exit 2
fi

# Find LaGriT's test/ folder (see the header).
LAGEXE_REAL=$LAGRIT
if [ -L "$LAGEXE_REAL" ]; then           # lagrit_dev is a link in ~/bin
    _l=$(readlink "$LAGEXE_REAL")
    case $_l in
        /*) LAGEXE_REAL=$_l ;;
        *)  LAGEXE_REAL=$(dirname "$LAGEXE_REAL")/$_l ;;
    esac
fi
TESTS=""
for _t in "${LAGRIT_TESTS:-}" "$(dirname "$0")/../.." "${LAGDEV:+$LAGDEV/test}" \
          "$(dirname "$LAGEXE_REAL")/../test"; do
    if [ -n "$_t" ] && [ -d "$_t/level01" ] && [ -f "$_t/testlagrit/diff.py" ]; then
        TESTS=$(cd "$_t" && pwd)
        break
    fi
done
if [ -z "$TESTS" ]; then
    echo "cannot find LaGriT's test/ folder (with level01/ and" >&2
    echo "testlagrit/diff.py): set LAGDEV (the LaGriT source tree) or" >&2
    echo "LAGRIT_TESTS (its test/ folder)" >&2
    exit 2
fi

# Every rm below is relative to this directory: go there, and stop if
# this is not it (the runner itself must be here; never $HOME or /).
cd "$(dirname "$0")" || exit 2
if [ ! -f "./$(basename "$0")" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
LOGDIR=logs
WORK=work
mkdir -p $LOGDIR $WORK
rm -rf "${LOGDIR:?}"/pics
rm -f "${LOGDIR:?}"/*
rm -rf "${WORK:?}"/*

LEVELS="level01"
[ "${LEVEL2:-0}" = 1 ] && LEVELS="level01 level02"

echo "lagrit : $LAGRIT"
echo "tests  : $TESTS ($LEVELS)"
# which LaGriT tree the tests came from (blank if not a git tree)
_rev=$(git -C "$TESTS/.." log -1 --format='%h %s' 2>/dev/null)
[ -n "$_rev" ] && echo "tree   : $_rev"
{
    echo "lagrit|$LAGRIT"
    echo "tests|$TESTS"
    echo "levels|$LEVELS"
    echo "tree|$_rev"
} > $LOGDIR/run_info.txt

# Run each test that has a reference/ folder (as LaGriT's
# helper_functions.collect_test_dirs does; test_results is not a test).
: > $LOGDIR/tests.txt
for lev in $LEVELS; do
    [ -d "$TESTS/$lev" ] || { echo "no $TESTS/$lev: skipped"; continue; }
    for src in "$TESTS/$lev"/*/; do
        src=${src%/}
        name=$(basename "$src")
        [ "$name" = test_results ] && continue
        [ -d "$src/reference" ] || continue
        [ -f "$src/input.lgi" ] || continue
        dst=$WORK/$name
        mkdir -p "$dst"
        # inputs only; the reference stays in the source tree
        for f in "$src"/* "$src"/.[!.]*; do
            [ -e "$f" ] || continue
            [ "$(basename "$f")" = reference ] && continue
            cp -R "$f" "$dst"/
        done
        rm -f "$dst/outx3dgen" "$dst/logx3dgen"
        t0=$(date +%s)
        (cd "$dst" && "$LAGRIT" -log logx3dgen -out outx3dgen \
             < input.lgi > stdout.txt 2>&1)
        rc=$?
        t1=$(date +%s)
        echo "$lev|$name|$rc|$((t1 - t0))" >> $LOGDIR/tests.txt
        cp "$dst/stdout.txt" "$LOGDIR/$name.stdout" 2>/dev/null
        cp "$dst/outx3dgen" "$LOGDIR/$name.outx3dgen" 2>/dev/null
        # Pictures only: the picture run (pictures.py): a SECOND run of
        # the deck in work/_pic_<test> with dumps added, when the test
        # wrote no AVS mesh file (*.inp not among its inputs) or
        # pictures.txt asks for more. It is drawn, never judged: the
        # judged run above is LaGriT's deck unchanged.
        wrote=no
        for f in "$dst"/*.inp; do
            [ -e "$f" ] || continue
            [ -e "$src/$(basename "$f")" ] && continue
            wrote=yes; break
        done
        pic=$WORK/_pic_$name
        mkdir -p "$pic"
        if python3 -I pictures.py deck "$name" "$src/input.lgi" $wrote \
               "$pic/zz_deck.lgi"; then
            for f in "$src"/* "$src"/.[!.]*; do
                [ -e "$f" ] || continue
                [ "$(basename "$f")" = reference ] && continue
                [ "$(basename "$f")" = input.lgi ] && continue
                cp -R "$f" "$pic"/
            done
            mv "$pic/zz_deck.lgi" "$pic/input.lgi"
            (cd "$pic" && "$LAGRIT" -log logpic -out outpic \
                 < input.lgi > stdout.txt 2>&1)
        else
            rmdir "$pic" 2>/dev/null
        fi
        printf '.'
    done
done
echo

python3 -I check_upstream.py "$TESTS" "$LOGDIR" "$WORK" known.txt
