#! /usr/bin/env bash

# Baksnapperd-denotbak - Daemon used by baksnapper when backup DeNotebak snapshots

# SPDX-FileCopyrightText: 2025  Fredrik Salomonsson <plattfot@posteo.net>
#
# SPDX-License-Identifier: GPL-3.0-or-later

read -rd '' version <<EOF
baksnapperd-denotebak (baksnapper) 2.4.0
Copyright (C) 2025  Fredrik Salomonsson
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
EOF

shopt -s extglob

function error {
    echo "[ERROR] $1" 1>&2
    exit 1
}

function warning {
    echo -e "[Warning] $1" 1>&2
}

case "$1" in
    version)
        # Return what version of the API this is using.
        #
        # It always a single integer.
        # No input.
        echo 4
        ;;
    snapshot-type)
        # Return what type of snapshot this backend supports
        #
        # No input.
        echo denotebak
        ;;
    list-snapshots)
        # List snapshots at location.
        #
        # Stripping any tags as they are volatile.
        # 1 [in]: Path to the directory the snapshots are expected to be in.
        shift
        find "$1" -mindepth 1 -maxdepth 1 -type d -printf "%f\n" | sed 's/__.*//' | sort -g
        ;;
    verify-snapshot)
        # Verify that a snapshot exist.
        #
        # 1 [in]: File path to a snapshot — sans tags.
        # Return 0 if it exist, non-zero otherwise.
        shift
        test -d "$1"?(__*) || error "Snapshot $1 doesn't exist."
        ;;
    incomplete-snapshot)
        # Check if a snapshot is incomplete.
        #
        # 1 [in]: Path to the directory the snapshot is in.
        # 2 [in]: Name of the snapshot — sans tags.
        # Return 0 if the snapshot is incomplete, non-zero otherwise.
        shift
        [[ $(btrfs property get "$1/$2"?(__*) ro) != "ro=true" ]]
        ;;
    create-location)
        # Prepare destination to receive a snapshot.
        #
        # 1 [in]: Path to the directory the snapshots will be in.
        shift
        mkdir -p "$1"
        ;;
    receive-snapshot)
        # Receive a snapshot.
        #
        # 1 [in]: Path to the directory the snapshot will be in.
        # 2 [in]: Name of the snapshot — sans tags.
        shift
        btrfs receive "$1"
        ;;
    send-snapshot)
        # Send a snapshot.
        #
        # 1 [in]: Path to the directory the snapshot is in.
        # 2 [in]: Name of the snapshot — sans tags.
        shift
        btrfs send "$1/$2"?(__*)
        ;;
    send-incremental-snapshot)
        # Only send what is different from the parent snapshot
        #
        # 1 [in]: File path to the parent snapshot — sans tags.
        # 2 [in]: File path to the snapshot — sans tags.
        shift
        btrfs send -p "$1"?(__*) "$2"?(__*)
        ;;
    remove-snapshots)
        # Remove all snapshots listed in the input.
        #
        # 1 [in]: Path to the directory the snapshots are in.
        # 2+ [in]: Name of the snapshots — sans tags.
        shift
        dest_root=$1
        shift
        for snapshot in "$@"
        do
            if btrfs subvolume show "$dest_root/$snapshot"?(__*) &> /dev/null
            then
                echo "Deleting snapshot $snapshot"
                btrfs subvolume delete "$dest_root/$snapshot"?(__*)
            else
                warning "Snapshot $snapshot does not match a DeNotebak snapshot, ignoring it."
            fi
        done
        ;;
    remove-broken-snapshot)
        # Remove a broken snapshot.
        #
        # 1 [in]: Path to the directory the snapshot is in.
        # 2 [in]: Name of the snapshot — sans tags.
        shift
        dest_root=$1
        snapshot=$2
        if btrfs subvolume show "$dest_root/$snapshot"?(__*) &> /dev/null
        then
            btrfs subvolume delete "$dest_root/$snapshot"?(__*)
        fi
        ;;
    link-latest)
        # Create symlink `latest` to the latest snapshot.
        #
        # 1 [in]: Path to the directory the snapshots are in.
        set -x
        shift
        declare -a snapshots
        if [[ -e "$1/latest" ]]; then
            if [[ -h "$1/latest" ]]; then
                rm "$1/latest"
            else
                error "$1/latest exists and is not a symbolic link. Link is not created."
            fi
        fi
        for dir in $(find "$1" -maxdepth 1 -mindepth 1 -type d -printf "%P\n"|sort)
        do
            if btrfs subvolume show "$1/$dir" &> /dev/null && [[ ! -h "$1/$dir" ]]
            then
                snapshots+=("$dir")
            fi
        done
        if  [ ${#snapshots[@]} -ne 0 ]; then
            ln -sfn "${snapshots[-1]}" "$1/latest"
        else
            error "link-latest: No suitable snapshots at $1 (${#snapshots[@]})"
        fi
        ;;
    --version)
        echo -e "$version"
        exit 0
        ;;
    *)
        error "Unrecognized command '$1', bailing out!"
        ;;
esac
