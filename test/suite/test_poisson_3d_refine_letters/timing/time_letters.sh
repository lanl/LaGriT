#!/bin/sh
#
# time_letters.sh [path-to-lagrit]
#
# Where does the letters demo spend its time? Runs four decks, each one
# stage further than the last, and prints the wall time of each and the
# difference, i.e. the cost of that stage:
#
#   t0_prep.lgi    read the letters, extrude, triangulate, refine
#   t1_field.lgi   + the h field on the driver's lookup lattice
#                    (510 x 172 x 40 = 3.5 M points at h 0.16)
#   t2_sample.lgi  + createpts / poisson_disk with no_connect
#                    (the driver rebuilds the field, then samples)
#   full deck      ../poisson_3d_letters.lgi (+ connect, quality, dump)
#
# So:  field    = t1 - t0
#      sampling = t2 - t1   (field built again inside createpts)
#      connect  = full - t2 (with quality and the dump of the tet mesh)
#
# Not a test: nothing is judged. Outputs stay in this directory.
# POSIX sh. Needs the same LaGriT as the suite (default: build/lagrit of this checkout).
# Default: the lagrit built in this LaGriT checkout (build/lagrit), else
# lagrit on PATH; or pass a path as the first argument.
_root=$(cd "$(dirname "$0")/../../../.." 2>/dev/null && pwd)
if [ -n "${1:-}" ]; then
    LAGRIT=$1
elif [ -n "$_root" ] && [ -x "$_root/build/lagrit" ]; then
    LAGRIT=$_root/build/lagrit
else
    LAGRIT=lagrit
fi
case $LAGRIT in
    */*) LAGRIT="$(cd "$(dirname "$LAGRIT")" && pwd)/$(basename "$LAGRIT")" ;;
    *)   LAGRIT=$(command -v "$LAGRIT") || { echo "cannot find $1" >&2; exit 2; } ;;
esac
# Every rm below is relative to this directory: go there, and stop if
# this is not it (the runner itself must be here; never $HOME or /).
cd "$(dirname "$0")" || exit 2
if [ ! -f "./$(basename "$0")" ] || [ "$(pwd)" = "$HOME" ] || [ "$(pwd)" = / ]; then
    echo "$0: not in its own directory ($(pwd)); nothing deleted" >&2
    exit 2
fi
# LaGriT splits every command at "/", so a deck cannot name a file as
# ../x: copy the two inputs here and use plain names.
cp ../letters_vector.inp ../letters_h_field.mlgi . || exit 2
echo "lagrit: $LAGRIT"
run () {   # deck label
    t0=$(date +%s)
    "$LAGRIT" < "$1" > "${1%.lgi}.stdout" 2>&1
    st=$?
    t1=$(date +%s)
    echo $((t1 - t0))
    if [ $st -ne 0 ]; then
        echo "  $1 exit status $st, see ${1%.lgi}.stdout" >&2
        echo FAILED
    fi
}
echo "running t0_prep.lgi ...";   a=$(run t0_prep.lgi)
echo "running t1_field.lgi ...";  b=$(run t1_field.lgi)
echo "running t2_sample.lgi ..."; c=$(run t2_sample.lgi)
echo "running the full deck ..."
cp ../poisson_3d_letters.lgi full_deck.lgi
d=$(run full_deck.lgi)
case "$a$b$c$d" in
    *FAILED*) echo "a deck failed: no timing table (see the .stdout files)" >&2
              exit 1 ;;
esac
{
echo "# time_letters.sh  $(date '+%Y-%m-%d %H:%M')  lagrit: $LAGRIT"
echo
printf '%-34s %6s s\n' "t0 prep (read, extrude, refine)" "$a"
printf '%-34s %6s s\n' "t1 = t0 + h field on 3.5 M points" "$b"
printf '%-34s %6s s\n' "t2 = t0 + createpts no_connect" "$c"
printf '%-34s %6s s\n' "full deck" "$d"
echo
printf '%-34s %6s s\n' "field (t1 - t0)" $((b - a))
printf '%-34s %6s s\n' "sampling (t2 - t1)" $((c - b))
printf '%-34s %6s s\n' "connect, quality, dump (full - t2)" $((d - c))
} | tee last_timing.txt
