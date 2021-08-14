#!/bin/sh

# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

. "./scripts/common.sh"

## Check long lines

ITEM="style, longline"

for f in *.el; do
    if [ $f != "citre-core-tables.el" ]; then
        info "$f"
        # Allow long lines that are the first lines of the file, contains web
        # links, or are the first lines of docstrings.
        grep -n '.\{80,\}' $f \
            | grep -v "\(^1:\)\|\(http://\)\|\(https://\)\|\(^[0-9]\+:  \"\)" \
            && error "Long line found in $f."
    fi
done

for f in scripts/*.sh; do
    info "$f"
    grep -n '.\{80,\}' $f | grep -v "\(http://\)\|\(https://\)" \
        && error "Lone line found in $f";
done

## Check indentation

ITEM="style, indent"

for f in *.el; do
    info "$f"
    # Some macros may have indent declarations. We (eval-buffer) to apply them.
    (if ! $EMACS -Q --batch -L . \
          --eval "(setq inhibit-message t)" \
          --eval "(find-file \"$f\")" \
          --eval "(eval-buffer)" \
          --eval "(indent-region (point-min) (point-max))" \
          --eval "(when (buffer-modified-p) (kill-emacs 1))"; then
         error "Wrong indentation in $f."
     fi
     exit 0
    ) || exit 1
done

## Checkdoc

ITEM="style, checkdoc"

for f in *.el; do
    info "$f"
    (if $EMACS -Q --batch -l ./scripts/checkdoc-hack.el\
               --eval "(setq checkdoc-verb-check-experimental-flag nil)" \
               --eval "(checkdoc-file \"$f\")" 2>&1 \
            | grep .; then
         error "Checkdoc failed in $f."
     fi
     exit 0
    ) || exit 1
done

ITEM="style"

pass "Style check passed :)"
exit 0
