#! /usr/bin/env bash

# Baksnapperd - Daemon used by baksnapper when backup snapper snapshots

# SPDX-FileCopyrightText: 2015-2024  Fredrik Salomonsson <plattfot@posteo.net>
# SPDX-FileCopyrightText: 2021-2022  Nathan Dehnel
# SPDX-FileCopyrightText: 2023       Juergen Gleiss
#
# SPDX-License-Identifier: GPL-3.0-or-later

read -rd '' version <<EOF
baksnapperd (baksnapper) 2.4.0
Copyright (C) 2015-2025  Fredrik Salomonsson
Copyright (C) 2021-2022  Nathan Dehnel
Copyright (C) 2023       Juergen Gleiss
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
EOF

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
        echo 3
        ;;
    list-snapshots)
        # List snapshots at location.
        #
        # Stripping any tags as they are volatile.
        # 1 [in]: Path to the directory the snapshots are expected to be in.
        shift
        find "$1" -mindepth 1 -maxdepth 1 -type d -printf "%f\n" | sort -g
        ;;
    get-snapper-root)
        # Return the location of the .snapshots directory
        #
        # 1: Name of the config
        shift
        snapper --no-dbus -c "$1" get-config | grep SUBVOLUME | awk '{ print $3 }'
        ;;
    verify-snapshot)
        # Verify that a snapshot exist.
        #
        # 1 [in]: File path to a snapshot
        # Return 0 if it exist, non-zero otherwise.
        shift
        find "$1" &> /dev/null || error "Snapshot $1 doesn't exist."
        ;;
    incomplete-snapshot)
        # Check if a snapshot is incomplete.
        #
        # 1 [in]: Path to the directory the snapshot is in.
        # 2 [in]: Name of the snapshot.
        # Return 0 if the snapshot is incomplete, non-zero otherwise.
        shift
        [[ ! -f "$1/$2/info.xml" ]] || \
        [[ ! -d "$1/$2/snapshot" ]] || \
        [[ $(btrfs property get "$1/$2/snapshot" ro) != "ro=true" ]]
        ;;
    create-config)
        # Prepare destination to receive a snapshot.
        #
        # 1 [in]: Path to the directory the snapshots will be in.
        shift
        mkdir -p "$1"
        ;;
    create-snapshot)
        # Prepare destination to receive a snapshot.
        #
        # 1 [in]: path to the directory the snapshot will be in.
        # 2 [in]: name of the snapshot.
        shift
        mkdir -p "$1"/"$2"
        ;;
    receive-info)
        # Receive the info.xml for a snapshot
        #
        # 1 [in]: Path to the directory the snapshot will be in.
        # 2 [in]: Name of the snapshot
        shift
        info="$1/$2/info.xml"
        [ -e "$info" ] && rm -f -- "$info"
        touch "$info"
        # Read from stdin
        while read -r line || [[ -n $line ]]
        do
            echo "$line" >> "$info"
        done
        ;;
    receive-snapshot)
        # Receive a snapshot.
        #
        # 1 [in]: Path to the directory the snapshot will be in.
        # 2 [in]: Name of the snapshot.
        shift
        btrfs receive "$1/$2"
        ;;
    send-info)
        # Send the info.xml for a snapshot.
        #
        # 1 [in]: Path to the directory the snapshot is in.
        # 2 [in]: Name of the snapshot.
        shift
        cat "$1/$2/info.xml"
        ;;
    send-snapshot)
        # Send a snapshot.
        #
        # 1 [in]: Path to the directory the snapshot is in.
        # 2 [in]: Name of the snapshot.
        shift
        btrfs send "$1/$2/snapshot"
        ;;
    send-incremental-snapshot)
        # Only send what is different from the parent snapshot
        #
        # 1 [in]: File path to the parent snapshot.
        # 2 [in]: File path to the snapshot.
        shift
        btrfs send -p "$1/snapshot" "$2/snapshot"
        ;;
    remove-snapshots)
        # Remove all snapshots listed in the input.
        #
        # 1 [in]: Path to the directory the snapshots are in.
        # 2+ [in]: Name of the snapshots
        shift
        dest_root=$1
        shift
        for snapshot in "$@"
        do
            # Only delete directories containing info.xml and snapshot
            mapfile -t content < <(find "$dest_root/$snapshot" \
                                        -maxdepth 1 -mindepth 1 -printf "%f\n" 2> /dev/null | \
                                       sort)
            if [[ ${#content[@]} == 2 && \
                  "${content[0]}" == "info.xml" && \
                  "${content[1]}" == "snapshot" ]]; then
                echo "Deleting snapshot $snapshot"
                btrfs subvolume delete "$dest_root/$snapshot/snapshot"
                rm -r -- "${dest_root:?}/$snapshot"
            else
                warning "Snapshot $snapshot doesn't match a snapper snapshot, "\
                        "ignoring it."
            fi
        done
        ;;
    remove-broken-snapshot)
        # Remove a broken snapshot.
        #
        # 1 [in]: Path to the directory the snapshot is in.
        # 2 [in]: Name of the snapshot.
        shift
        dest_root=$1
        shift
        snapshot=$1
        if [[ -d "$dest_root/$snapshot/snapshot" ]]
        then
            btrfs subvolume delete "$dest_root/$snapshot/snapshot"
        fi
        rm -r -- "${dest_root:?}/$snapshot"
        ;;
    link-latest)
        # Create symlink `latest` to the latest snapshot.
        #
        # 1 [in]: Path to the directory the snapshots are in.
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
        # A test to make sure this backend listen to commands.
        #
        # No input
        # Always return 0 when called successfully.
        exit 0
        ;;
    --version)
        echo -e "$version"
        exit 0
        ;;
    *)
        error "Unrecognized command, bailing out!"
        ;;
esac
