#!/bin/sh

# Copyright (C) 2022 Hao WANG
# License: GPL v3, or (at your option) any later version

. "./scripts/common.sh"
ITEM="test"

test_files="tests/*.test"

for t in $test_files; do
    info $t
    (if ! $EMACS -Q --batch -l puni.el -l tests/common.el \
          --eval "(run-test \"$t\")"; then
         error "A test in $t failed."
     fi
     exit 0
    ) || exit 1
done
