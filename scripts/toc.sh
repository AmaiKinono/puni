#!/bin/sh

# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

. "./scripts/common.sh"

MARKDOWN_TOC=${MARKDOWN_TOC:=markdown-toc}

ITEM="toc"
FILE="README.md"

info "$FILE"
(if ! $MARKDOWN_TOC -i $FILE; then
    error "Error when creating TOC."
 fi
 exit 0
) || exit 1
