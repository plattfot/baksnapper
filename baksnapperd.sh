#! /bin/bash

# Baksnapperd - Daemon used by baksnapper when backup via ssh

# Copyright (C) 2015-2021  Fredrik Salomonsson <plattfot@posteo.net>
# Copyright (C) 2021  Nathan Dehnel

# This file is part of baksnapper

# Baksnapper is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Baksnapper is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

read -rd '' version <<EOF
baksnapperd (baksnapper) 2.0.0
Copyright (C) 2015-2021  Fredrik Salomonsson
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
    version) # Return what version of the API it's using, always one integer
        echo 2
        ;;
    list-snapshots) # List snapshots at backup location
        shift
        find "$1" -mindepth 1 -maxdepth 1 -type d -printf "%f\n" | sort -g
        ;;
    get-snapper-root) # Return the location of the .snapshots directory
        shift
        snapper -c "$1" get-config | grep SUBVOLUME | awk '{ print $3 }'
        ;;
    verify-snapshot)
        shift
        find "$1" &> /dev/null || error "Snapshot $1 doesn't exist."
        ;;
    create-config)
        shift
        mkdir -p "$1"
        ;;
    create-snapshot)
        shift
        mkdir -p "$1"/"$2"
        ;;
    receive-info)
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
        shift
        btrfs receive "$1/$2"
        ;;
    send-info)
        shift
        cat "$1/$2/info.xml"
        ;;
    send-snapshot)
        shift
        btrfs send "$1/$2/snapshot"
        ;;
    send-incremental-snapshot)
        shift
        btrfs send -p "$1/snapshot" "$2/snapshot"
        ;;
    remove-snapshots)
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
                  ${content[0]} == "info.xml" && \
                  ${content[1]} == "snapshot" ]]; then
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
        shift
        dest_root=$1
        shift
        snapshot=$1
        if [[ -d $dest_root/$snapshot/snapshot ]]
        then
            btrfs subvolume delete "$dest_root/$snapshot/snapshot"
        fi
        rm -r -- "${dest_root:?}/$snapshot"
        ;;
    link-latest)
        shift
        declare -a snapshots
        for dir in "$1"/*; do
            if [[ -d "$dir/snapshot" && ! -h "$dir" ]]; then
                snapshots+=("$dir")
            fi
        done
        if ! [ ${#snapshots[@]} -eq 0 ]; then
            ln -sfn "${snapshots[-1]}" "$1/latest-tmp"
            mv -T "$1/latest-tmp" "$1/latest"
        elif [[ -h "$1/latest" ]]
        then
            rm "$1/latest"
        fi
        ;;
    test-connection)
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
