#!/bin/sh

# Copyright (C) 2020 Hao WANG
# License: GPL v3, or (at your option) any later version

. "./scripts/common.sh"
ITEM="compile"

for f in *.el; do
    info "$f"
    ($EMACS -Q --batch --eval "(setq byte-compile-error-on-warn t)" \
            -L . -f batch-byte-compile $f 2>&1 \
         | grep . && error "Failed compiling $f." || exit 0
    ) || exit 1
done

pass "Byte compilation succeeded :)"
exit 0
