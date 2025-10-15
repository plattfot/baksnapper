#! /usr/bin/env bash

# Common baksnapperd commands

# SPDX-FileCopyrightText: 2025  Fredrik Salomonsson <plattfot@posteo.net>
#
# SPDX-License-Identifier: GPL-3.0-or-later

case "$1" in
# @COMMON_START@
    list-snapshots) # List snapshots at backup location
        shift
        find "$1" -mindepth 1 -maxdepth 1 -type d -printf "%f\n" | sort -g
        ;;
    link-latest)
        shift
        declare -a snapshots
        if [[ -e "$1/latest" ]]; then
            if [[ -h "$1/latest" ]]; then
                rm "$1/latest"
            else
                error "$1/latest exists and is not a symbolic link. Link is not created."
            fi
        fi
        for dir in $(find "$1" -maxdepth 1 -mindepth 1 -type d -printf "%P\n"|sort --version-sort)
        do
            if [[ -d "$1/$dir/snapshot" && ! -h "$1/$dir" ]]; then
                snapshots+=("$dir")
            fi
        done
        if  [ ${#snapshots[@]} -ne 0 ]; then
            ln -sfn "${snapshots[-1]}" "$1/latest"
        else
            error "link-latest: No suitable snapshots at $1 (${#snapshots[@]})"
        fi
        ;;
    test-connection)
        exit 0
        ;;
# @COMMON_END@
esac
