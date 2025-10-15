#! /usr/bin/env bash

# Compile baksnapperd backend

# SPDX-FileCopyrightText: 2025  Fredrik Salomonsson <plattfot@posteo.net>
#
# SPDX-License-Identifier: GPL-3.0-or-later
read -rd '' help <<EOF
Usage: $0 [OPTIONS...] BAKSNAPPERD COMMON --output OUTPUT

Copy section between @COMMON_START@/@COMMON_END@ in COMMON and replace
@COMMON_INCLUDE@ in BAKSNAPPERD.

Options:

-o, --output PATH  Where it should write the result to.

-h, --help         Print this help and then exit.

EOF

function error {
    echo "[ERROR] $1" 1>&2
    exit 1
}

if ! _args=$(getopt --name "baksnapper" \
             --options "ho:" \
             --long "output:" \
             --long help \
             -- "$@")
then
    error "Try '$0 --help for more information.'"
fi

eval set -- "$_args"

# Parse options
while [[ $# -gt 0 ]]
do
key=$1
case $key in
    -o|--output)
        p_output=$2
        shift 2
        ;;
    --)
        shift
        break
        ;;
esac
done

[[ $# -lt 2 ]] && error "Need to specify a backend and common"
[[ -z "$p_output" ]] && error "No output is specified"

mkdir -p "$(dirname "$p_output")"
cat <(sed -n '/@COMMON_INCLUDE@/q;p' "$1") \
    <(sed -n '/@COMMON_START@/,/@COMMON_END@/p' "$2"|sed ';1d;$d') \
    <(sed '0,/@COMMON_INCLUDE@/d' "$1") > "$p_output"
chmod +x "$p_output"
