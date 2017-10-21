#!/bin/sh
#
# Produce output consumable by dieharder from ./seed-from-aslr
# Run dieharder with:
#   ./seed-from-aslr-test.sh > foo
#   dieharder -g 202 -f foo -a

n=50000

cat <<EOF
#==================================================================
# generator seed-from-aslr
#==================================================================
type: d
count: $n
numbit: 32
EOF
while [ "$n" -gt 0 ]; do
    ./seed-from-aslr
    n=$(($n - 1))
done
