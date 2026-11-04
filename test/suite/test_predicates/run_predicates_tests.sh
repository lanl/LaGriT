#!/bin/sh
#
# run_predicates_tests.sh
#
# test_predicates: compiles small drivers for src/lg_predicates2d.c and
# src/lg_predicates3d.c with the C compiler (cc, or $CC) and compares
# every answer with exact rational arithmetic. See 00README. POSIX sh,
# Python 3 (standard library), a C compiler with __int128 (gcc, clang).
# WORK and LOGS may be set to other folders. No lagrit needed.

cd "$(dirname "$0")" || exit 2
if [ ! -f "./run_predicates_tests.sh" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
SRC=../../../src
WORK=${WORK:-work}
LOGDIR=${LOGS:-logs}
CC=${CC:-cc}
mkdir -p "$WORK" "$LOGDIR"
rm -rf "${WORK:?}"/* "${LOGDIR:?}"/*
$CC -O2 -I"$SRC" -o "$WORK/pred3d" pred3d_driver.c "$SRC/lg_predicates3d.c" -lm || exit 1
$CC -O2 -I"$SRC" -o "$WORK/pred2d" pred2d_driver.c "$SRC/lg_predicates2d.c" -lm || exit 1
python3 -I check_predicates.py "$WORK" "$LOGDIR"
rc=$?
echo
echo "summary page: $LOGDIR/summary.html"
exit $rc
