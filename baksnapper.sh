#! /bin/bash

# Baksnapper - Backup snapper snapshots to backup location using
# btrfs' incremental send and receive

# Copyright (C) 2015-2021  Fredrik Salomonsson <plattfot@posteo.net>

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

read -rd '' help <<EOF
Usage: $0 [OPTIONS...] [ADDRESS:]PATH

Backup snapper snapshot to PATH using btrfs incremental send and
receive. ADDRESS is specified for remote backups.

Options:

-a, --all         Send all snapshots in the source directory.  Default
                  is to only send the last one.

--config NAME     Name of config.

--configfile NAME Name of config file to use.

--private-key KEY Specify the private KEY file to use when connecting
                  to a remote backup location.

-p, --prune       Prune the backups by deleting snapshots that isn't
                  in the source directory.

--delete LIST     Delete the snapshots at the backup location that are
                  listed in the list then exit.  The list is comma
                  separated.

--daemon BIN      Set the name of the baksnapperd, default is to call
                  baksnapperd.

--delete-all      Delete all backup snapshots for config

--snapshot NUMBER Backup specific snapshot NUMBER, default is the last one.

--type TYPE       Specify either to backup snapshots to a server
                  (push) or to backup snapshots from a server
                  (pull). Default is to push.
                  
--link            Create a "latest" directory linked to the latest snapshot

-v, --verbose     Verbose print out.

-h, --help        Print this help and then exit.

--version         Print version and then exit

Example:

1)
$0 --config root /mnt/backup
Backup the last root snapshot to /mnt/backup, if it is the first time
it will send the whole snapshot otherwise it will just send what have
changed.

2)
$0 --delete 1,2,3,4 -c root /mnt/backup
Delete the root's snapshots 1,2,3 and 4 for from /mnt/backup, will
output a warning if a snapshot doesn't exist.

3)
$0 --config root foo:/mnt/backup
Same as example 1 except it will send the backups to the remote
machine named foo.

Note:
This script most likely needs root to be able to backup snapshots.

Exit status:
\t0 if ok.
\t1 if option error (e.g. wrong flag etc).

Author:
Fredrik "PlaTFooT" Salomonsson
EOF

read -rd '' version <<EOF
baksnapper (baksnapper) 2.0.0
Copyright (C) 2015-2021  Fredrik Salomonsson
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
EOF

LINK=0

function error {
    echo "[ERROR] $1" 1>&2
    exit 1
}

function warning {
    echo -e "[Warning] $1" 1>&2
}

function parse-full-path {
    if [[ $1 =~ \(.*?\):\(.*\) ]]
    then
        p_ssh_address=${BASH_REMATCH[1]}
        ssh=${ssh-"ssh $p_ssh_address"}
        p_dest=${BASH_REMATCH[2]}
    else
        p_dest=${p_dest-"$1"}
    fi
}

function read-config {

    function get-value {
        _value=$(echo "$1" | sed -Ee 's/^[A-Z_ ]+=[ ]*(.*?)/\1/' -e 's/#.*//')
    }

    function parse-bool {
        if [[ "$1" =~ YES|yes|Yes|1 ]]
        then
            _bool=1
        else
            _bool=0
        fi
    }

    while read -r line; do
        case $line in
            CONFIG*=*)
                get-value "$line"
                p_config=${p_config-"$_value"}
            ;;
            PATH*=*)
                get-value "$line"
                parse-full-path "$_value"
            ;;
            DAEMON*=*)
                get-value "$line"
                p_baksnapperd=${p_baksnapperd-$_value}
                ;;
            PRUNE*=*)
                get-value "$line"
                parse-bool "$_value"
                p_prune=${p_prune-$_bool}
                ;;
            ALL*=*)
                get-value "$line"
                parse-bool "$_value"
                p_all=${p_all-$_bool}
                ;;
            VERBOSE*=*)
                get-value "$line"
                parse-bool "$_value"
                p_verbose=${p_verbose-$_bool}
                ;;
            LINK*=*)
                get-value "$line"
                parse-bool "$_value"
                p_link=${p_link-$_bool}
                ;;
            TYPE*=*)
                get-value "$line"
                p_type=${p_type-$_value}
                ;;
            PRIVATE_KEY*=*)
                get-value "$line"
                p_ssh_args=${p_ssh_args-" -i $_value"}
                ;;
            *)
                ;;
        esac
    done < "$1"
}

# Use getopt to parse the command-line arguments

if ! _args=$(getopt --name "baksnapper" --options "adhvp" --long "config:,configfile:,delete:,daemon:,private-key:,snapshot:,type:,all,delete-all,help,prune,verbose,version" -- "$@")
then
    error "Try '$0 --help for more information.'"
fi

eval set -- "$_args"

# Parse options
while [[ $# -gt 0 ]]
do
key=$1
case $key in
    -a|--all)
        p_all=1
        shift
        ;;
    --config)
        p_config="$2"
        shift 2
        ;;
    --configfile)
        read-config "$2"
        shift 2
        ;;
    --delete)
        p_delete=1
        p_delete_list=${2//,/ }
        shift 2
        ;;
    --delete-all)
        shift
        p_delete_all=1
        ;;
    --daemon)
        p_baksnapperd=$2
        shift 2
        ;;
    -h|--help)
        echo -e "$help"
        exit 0
        ;;
    --private-key)
        p_ssh_args=" -i $2"
        shift 2
        ;;
    -p|--prune)
        p_prune=1
        shift
        ;;
    --snapshot)
        p_snapshot=$2
        shift 2
        ;;
    --type)
        p_type=$2
        shift 2
        ;;
    --link)
        p_link=1
        shift
        ;;
    -v|--verbose)
        p_verbose=1
        shift
        ;;
    --version)
        echo -e "$version"
        exit 0
        ;;
    --)
        shift
        break
        ;;
esac
done

while [[ $# -gt 0 ]]
do
    case $1 in
        *)
            parse-full-path "$1"
            shift
            break
            ;;
    esac
done


if [[ $p_verbose == 1 ]]
then
    set -x
fi

p_baksnapperd=${p_baksnapperd=baksnapperd}
p_all=${p_all=0}

# Error checks
#[[ $USER != root ]] && error "Need to be root to run this script!"
[[ -z $p_config ]] && error "You need to specify the config name to backup!"
[[ -z $p_dest ]] && error "No path specified!"

function exit-msg {
    notify-send -u critical "Done backing up $p_config. Safe to turn off computer."
}

# Only run notify-send if installed
if hash notify-send 2> /dev/null
then
    notify-send -u critical "Backing up $p_config. Do not turn off computer!"
    trap exit-msg EXIT
fi

regex='(.*?):(.*)'
if [[ $p_dest =~ $regex ]]
then
    address="${BASH_REMATCH[1]}"
    ssh="ssh $p_ssh_args $address"
    dest="${BASH_REMATCH[2]}"
else
    dest=$p_dest
fi

if hash notify-send 2> /dev/null
then
    has_notify=1
fi

function exit-msg {
    notify-send -u critical "Done backing up $p_config. Safe to turn off computer."
}

if [[ $has_notify == 1 ]]
then
    notify-send -u critical "Backing up $p_config. Do not turn off computer!"
    trap exit-msg EXIT
fi

if [[ -n "$ssh" ]]
then
    # Sanity check for ssh
    $ssh -q test-connection || error "Unable to connect to $address"
fi

case ${p_type="push"} in
    pull|PULL)
        sender=${ssh-$p_baksnapperd}
        receiver=$p_baksnapperd
    ;;
    push|PUSH)
        sender=$p_baksnapperd
        receiver=${ssh-$p_baksnapperd}
    ;;
    *)
        error "Unknown type! $p_type"
    ;;
esac

if ! receiver_version=$($receiver version)
then
    receiver_version=1
fi

if ! sender_version=$($sender version)
then
    sender_version=1
fi

if [[ "$receiver_version" -gt 2 ]]
then
    error "receiver is too new, need to use version 2 or 1"
fi

if [[ "$sender_version" -gt 2 ]]
then
    error "sender is too new, need to use version 2 or 1"
fi

# Get the subvolume to backup
case $sender_version in
    2)
        subvolume=$($sender get-snapper-root "$p_config")
        ;;
    1)
        if ! subvolume=$($sender list-snapper-snapshots "$p_config")
        then
            error "Something went wrong when fetching the snapper root from sender"
        fi
        ;;
    *)
        error "Unknown version for sender '$sender_version'"
        ;;
esac

src_root=${subvolume:?}/.snapshots
dest_root="$dest/$p_config"

$receiver create-config "$dest_root" || error "Problem creating config at backup location"

# List all the snapshots available
case $sender_version in
    2)
        mapfile -t src_snapshots < <($sender list-snapshots "$src_root")
        ;;
    1)
        mapfile -d' ' -t src_snapshots < <($sender list-snapshots "$src_root"|tr -d '\n')
        ;;
    *)
        error "Unknown version for sender '$sender_version'"
        ;;
esac
num_src_snapshots=${#src_snapshots[@]}

# List all the snapshots at the backup location
case $receiver_version in
    2)
        mapfile -t dest_snapshots < <($receiver list-snapshots "$dest_root")
        ;;
    1)
        mapfile -d' ' -t dest_snapshots < <($receiver list-snapshots "$dest_root"|tr -d '\n')
        ;;
    *)
        error "Unknown version for receiver '$receiver_version'"
        ;;
esac
num_dest_snapshots=${#dest_snapshots[@]}

if [[ -z $p_snapshot ]]
then
    p_snapshot=${src_snapshots[num_src_snapshots-1]}
else
    $sender verify-snapshot "$src_root/$p_snapshot" || exit 1
fi

################################################################################
# Compare source and destination location and sort the snapshots into:
# common: exist at both locations
# only_in_src: only exist at the source
# only_in_dest: only exist at the destination

idx_src=0
idx_dest=0

common=()
only_in_src=()
only_in_dest=()

# NOTE: the snapshots must be sorted in ascending order hence the
# "sort -g" when getting the snapshots.
while (( idx_src < num_src_snapshots && idx_dest < num_dest_snapshots ))
do
    if [ "${src_snapshots[idx_src]}" -eq "${dest_snapshots[idx_dest]}" ]
    then
        common+=("${src_snapshots[idx_src]}")
        ((++idx_src))
        ((++idx_dest))
    elif [ "${src_snapshots[idx_src]}" -lt "${dest_snapshots[idx_dest]}" ]
    then
        only_in_src+=("${src_snapshots[idx_src]}")
        ((++idx_src))
    else
        only_in_dest+=("${dest_snapshots[idx_dest]}")
        ((++idx_dest))
    fi
done

# Add the rest to respective array.
for (( ; idx_dest < num_dest_snapshots; ++idx_dest ))
do
    only_in_dest+=("${dest_snapshots[idx_dest]}")
done

for (( ; idx_src < num_src_snapshots; ++idx_src ))
do
    only_in_src+=("${src_snapshots[idx_src]}")
done
################################################################################
# First argument is the snapshot to send.
function single-backup {
    $receiver create-snapshot "$dest_root" "$1" ||
        error "Failed to create snapshot at backup location!"

    if ! $sender send-info "$src_root" "$1" | $receiver receive-info "$dest_root" "$1"
    then
        $receiver remove-broken-snapshot "$dest_root" "$1"
        error "Failed to send snapshot info!"
    fi

    if ! $sender send-snapshot "$src_root" "$1" | $receiver receive-snapshot "$dest_root" "$1"
    then
        $receiver remove-broken-snapshot "$dest_root" "$1"
        error "Failed to send snapshot!"
    fi
    if [ $p_link -eq 1 ]
    then
        $receiver link-latest "$dest_root"
    fi
}
# First argument is the reference snapshot and the second is the
# snapshot to backup. It will only send the difference between the
# two.
function incremental-backup {
    echo "Incremental backup"

    $receiver create-snapshot "$dest_root" "$2" ||
        error "Failed to create snapshot at backup location!"

    if ! $sender send-info "$src_root" "$2" | $receiver receive-info "$dest_root" "$2"
    then
        $receiver remove-broken-snapshot "$dest_root" "$1"
        error "Failed to send snapshot info!"
    fi

    if ! $sender send-incremental-snapshot "$subvolume/.snapshots/"{"$1","$2"} \
            | $receiver receive-snapshot "$dest_root" "$2"
    then
        $receiver remove-broken-snapshot "$dest_root" "$1"
        error "Failed to send snapshot!"
    fi
    if [ $p_link -eq 1 ]
    then
        $receiver link-latest "$dest_root"
    fi
}

# Main logic for the backup
function backup {
    if [[ $num_src_snapshots == 0 ]]
    then
        error "No snapshots found."
    fi

    local num_src_only=${#only_in_src[@]}
    if [ "$num_src_only" -eq 0 ]
    then
        echo "Already backed up all snapshots"
        return 0
    fi

    # Destination doesn't have any snapshots, send the whole snapshot.
    if [[ "${#common[@]}" == 0 ]]
    then
        echo "Initialize backup"
        if [ $p_all -eq 1 ]
        then
            # Send the first snapshot as a whole then the rest will be
            # sent incremental.
            local snapshot=${only_in_src[0]}
            # Pop the one that got sent from the array.
            only_in_src=("${only_in_src[@]:1}")
            ((--num_src_only))
            single-backup "$snapshot"
            common=("$snapshot")
        else
            # Send the specified snapshot
            single-backup "$p_snapshot"
            return 0
        fi
    fi

    if [[ $p_all == 0 ]]
    then
        local common_last=${common[${#common[@]}-1]}
        # Check that it's not already synced
        if [[ "$common_last" == "$p_snapshot" ]]
        then
            error "Already synced the last snapshot."
        fi
        incremental-backup "$common_last" "$p_snapshot"
        return 0
    else
        # Find the first common snapshot that is lower than the first
        # source only snapshot. This will be the start of the incremental
        # backup. If no one is found it will use the lowest common one.
        local first_src_snapshot=${only_in_src[0]}
        local idx=${#common[@]}-1
        for (( ; idx >= 0; --idx ))
        do
            if [[ ${common[idx]} -lt $first_src_snapshot ]]
            then
                break
            fi
        done
        incremental-backup "${common[idx]}" "$first_src_snapshot"

        for (( idx=1; idx < num_src_only; ++idx ))
        do
            incremental-backup "${only_in_src[idx-1]}" "${only_in_src[idx]}"
        done
    fi
}

# Main:
if [[ ${p_delete_all-0} == 1 ]]
then
    echo -n "Are you sure you want to delete all backup snapshots from $dest_root? (y/N): "
    while true
    do
        read -r answer
        case $answer in
            y|Y)
                $receiver remove-snapshots "$dest_root" "${dest_snapshots[@]}"
                if [ $p_link -eq 1 ]
                then
                    $receiver link-latest "$dest_root"
                fi
                break
                ;;
            n|N|"")
                ;;
            *)
                echo -ne "Please answer y or n.\n(y/N): "
                ;;
        esac
    done
elif [[ ${p_delete-0} == 0 ]]
then
    backup
else
    $receiver remove-snapshots "$dest_root" "${p_delete_list[@]}"
    if [ $p_link -eq 1 ]
    then
        $receiver link-latest "$dest_root"
    fi
fi

if [[ ${p_prune-0} == 1 ]]
then
    $receiver remove-snapshots "$dest_root" "${only_in_dest[@]}"
    if [ $p_link -eq 1 ]
    then
        $receiver link-latest "$dest_root"
    fi
fi
