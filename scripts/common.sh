#!/bin/sh

# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

## Executables

EMACS=${EMACS:=emacs}

## Helper functions

export PASSED_FACE="\033[1;32m"
export ERROR_FACE="\033[1;31m"
export NORMAL_FACE="\033[0m"

info()
{
    printf '[%s] %s\n' "$ITEM" "$@"
}

pass()
{
    printf $PASSED_FACE
    printf '[%s] %s' "$ITEM" "$@"
    printf "$NORMAL_FACE\n"
}

error()
{
    printf $ERROR_FACE 1>&2
    printf '[%s] %s' "$ITEM" "$@" 1>&2
    printf "$NORMAL_FACE\n" 1>&2
    exit 1
}
