#! /bin/bash

# Baksnapper - Backup snapper snapshots to backup location using
# btrfs' incremental send and receive

# Copyright (C) 2015  Fredrik Salomonsson

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

read -rd '' help <<EOF
Usage: $0 [OPTIONS]... [PATH]

Backup snapper snapshot to [PATH] using btrfs incremental send and
receive.

Options:
\t-c <name>, --config <name>\tName of config.
\t-a, --all\t\t\tSend all snapshots in the source directory. 
\t\t\t\t\tDefault is to only send the last one.
\t-p, --prune\t\t\tPrune the backups by deleting snapshots that 
\t\t\t\t\tisn't in the source directory.
\t-d <list>, --delete <list>\tDelete the snapshots at the backup
\t\t\t\t\tlocation that are listed in the list then exit.
\t\t\t\t\tThe list is comma separated.
\t-s <address>, --ssh <address>\tBackup to a server at address <address>.
\t-v, --verbose\t\t\tVerbose print out.
\t-h, --help\t\t\tPrint this help and then exit.

Example: 
$0 -c root /mnt/backup
Backup the last root snapshot to /mnt/backup, if it is the first time
it will send the whole snapshot otherwise it will just send what have
changed.

$0 -d 1,2,3,4 -c root /mnt/backup
Delete the root's snapshots 1,2,3 and 4 for from /mnt/backup, will
output a warning if a snapshot doesn't exist.

Note:
This doesn't support option stacking e.g. -pc <name>. Instead you
need to separate each option i.e. -p -c <name>
Also this script needs root to be able to backup snapshots.

Exit status:
\t0 if ok.
\t1 if option error (e.g. wrong flag etc).

Author:
Fredrik "PlaTFooT" Salomonsson
EOF

function error {
    echo "[ERROR] $1" 1>&2
    exit 1
}

function warning {
    echo -e "[Warning] $1" 1>&2
}

# If first argument is 1 print the rest. 
function printv {
    if [[ $1 = 1 ]]; then
        shift
        echo -e $@
    fi
}

# Parameter values, postfix p_ to indicate that they are parameters.
p_all=0 
p_prune=0
p_verbose=0
p_delete=0
ssh=""
# Parse options
while [[ $# > 0 ]]
do
key=$1
case $key in
    -c|--config)
        p_config="$2"
        shift 2
        ;;
    -v|--verbose)
        p_verbose=1
        shift
        ;;
    -p|--prune)
        p_prune=1
        shift
        ;;
    -a|--all)
        p_all=1
        shift
        ;;
    -h|--help)
        echo -e "$help"
        exit 0
        ;;
    -d|--delete)
        p_delete=1
        p_delete_list=${2//,/ }
        shift 2
        ;;
    -s|--ssh)
        ssh_address="$2"
        ssh="ssh $ssh_address"
        shift 2
        ;;
    -*)
        #unknown option
        error "Unknown option $1, see --help"
        shift
        ;;
    *)
        p_dest=$1
        shift
        ;;
esac
done


# Error checks
[[ $USER != root ]] && error "Need to be root to run this script!"
[[ -z $p_config ]] && error "You need to specify the config name to backup!"
[[ -z $p_dest ]] && error "No path specified!"

# Get the subvolume to backup
subvolume=$(snapper -c $p_config get-config | grep SUBVOLUME | awk '{ print $3 }')
printv $p_verbose "subvolume=$subvolume"

src_root=$subvolume/.snapshots
dest_root=$p_dest/$p_config

if [ -n "$ssh" ]; then
    # Sanity check for ssh
    $ssh -q exit
    [ $? -gt 0 ] && error "Unable to connect to $ssh"
fi

$ssh test ! -d $p_dest && error "Backup path specified isn't a directory!"
$ssh test -d $dest_root || $ssh mkdir -p $dest_root

printv $p_verbose "p_config=${p_config}"
printv $p_verbose "p_dest=${p_dest}"
printv $p_verbose "p_prune=${p_prune}"
printv $p_verbose "p_all=${p_all}"
printv $p_verbose "p_delete=${p_delete}"
printv $p_verbose "ssh = ${ssh}"

# List all the snapshots available
src_snapshots=($(find $src_root -mindepth 1 -maxdepth 1 -printf "%f " | sort -g))
num_src_snapshots=${#src_snapshots[@]}

# List all the snapshots at the backup location
dest_snapshots=($(find $dest_root -mindepth 1 -maxdepth 1 -printf "%f " | sort -g))
num_dest_snapshots=${#dest_snapshots[@]}

printv $p_verbose "src_snapshots=${src_snapshots[@]}"
printv $p_verbose "dest_snapshots=${dest_snapshots[@]}"
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
while (( $idx_src < $num_src_snapshots && $idx_dest < $num_dest_snapshots ))
do
    if [ ${src_snapshots[idx_src]} -eq ${dest_snapshots[idx_dest]} ]; then
        common+=(${src_snapshots[idx_src]})
        ((++idx_src))
        ((++idx_dest))
    elif [ ${src_snapshots[idx_src]} -lt ${dest_snapshots[idx_dest]} ]; then
        only_in_src+=(${src_snapshots[idx_src]})
        ((++idx_src))
    else
        only_in_dest+=(${dest_snapshots[idx_dest]})
        ((++idx_dest))
    fi
done

# Add the rest to respective array.
for (( ; idx_dest < num_dest_snapshots; ++idx_dest ))
do
    only_in_dest+=(${dest_snapshots[idx_dest]})
done

for (( ; idx_src < num_src_snapshots; ++idx_src ))
do
    only_in_src+=(${src_snapshots[idx_src]})
done
################################################################################

printv $p_verbose "common=" "${common[@]}"
printv $p_verbose "only_in_src=" "${only_in_src[@]}"
printv $p_verbose "only_in_dest=" "${only_in_dest[@]}"

# First argument is the snapshot to send.
function single_backup {
    printv $p_verbose "Sending snapshot $1."
    local dest_dir=$dest_root/$1
    local src_dir=$src_root/$1

    mkdir -p $dest_dir
    cp $src_dir/info.xml $dest_dir
    btrfs send $src_dir/snapshot| btrfs receive $dest_dir
}
# First argument is the reference snapshot and the second is the
# snapshot to backup. It will only send the difference between the
# two.
function incremental_backup {
    echo "Incremental backup"
    printv $p_verbose "Backing up snapshot $2 using snapshot $1 as reference."

    local dest_dir=$p_dest/$p_config/$2
    local src_dir=$subvolume/.snapshots/$2
    local ref_dir=$subvolume/.snapshots/$1

    mkdir -p $dest_dir
    cp $src_dir/info.xml $dest_dir
    btrfs send -p $ref_dir/snapshot $src_dir/snapshot| btrfs receive $dest_dir
}

# Main logic for the backup
function backup {
    if [ $num_src_snapshots -eq 0 ]; then
        error "No snapshots found."
    fi
    
    local num_src_only=${#only_in_src[@]}
    if [ $num_src_only -eq 0 ]; then
        echo "Already backed up all snapshots"
        return 0
    fi
    
    # Destination doesn't have any snapshots, send the whole snapshot.
    if [ -z $common ]; then
        echo "Initialize backup"
        if [ $p_all -eq 1 ]; then
            # Send the first snapshot as a whole then the rest will be
            # sent incremental.
            local snapshot=${src_snapshots[0]} 
            single_backup $snapshot
            common=$snapshot
        else
            # Just send the last one and exit.
            local snapshot=${src_snapshots[$num_src_snapshots-1]}
            single_backup $snapshot
            exit 0
        fi
    fi

    if [[ $p_all == 0 ]]; then
        local common_last=${common[(( ${#common[@]}-1 ))]}
        local snapshot=${src_snapshots[num_src_snapshots-1]}

        # Check that it's not already synced
        if [[ $common_last == ${snapshot[num_src_snapshots-1]} ]]; then
            error "Already synced the last snapshot."
        fi

        incremental_backup $common_last $snapshot
    else
        # Find the first common snapshot that is lower than the first
        # source only snapshot. This will be the start of the incremental
        # backup. If no one is found it will use the lowest common one.
        local first_src_snapshot=${only_in_src[0]}
        local idx=${#common[@]}-1
        for (( ; idx >= 0; --idx ))
        do
            if [[ ${common[idx]} -lt $first_src_snapshot ]]; then
                break
            fi
        done
        incremental_backup ${common[idx]} $first_src_snapshot

        for (( idx=1; idx < $num_src_only; ++idx ))
        do
            incremental_backup ${only_in_src[idx-1]} ${only_in_src[idx]}
        done
    fi
}

# Arguments snapshots to delete
function remove_snapshots {
    printv $p_verbose "snapshots to delete = $1"
    for snapshot in $@
    do
        # Only delete directories containing info.xmp and snapshot
        local content=($(find $dest_root/$snapshot \
                              -maxdepth 1 -mindepth 1 -printf "%f " 2> /dev/null))
        if [[ ${#content[@]} == 2 && \
              ${content[0]} == "info.xml" && \
              ${content[1]}="snapshot" ]]; then
            printv $p_verbose "Deleting snapshot $snapshot"
            btrfs subvolume delete $dest_root/$snapshot/snapshot
            rm -r -- $dest_root/$snapshot
        else
            warning "Snapshot $snapshot doesn't match a snapper snapshot, ignoring it."
        fi
    done    
}

# Main:
if [ $p_delete -eq 0 ]; then
    backup 
else
    remove_snapshots $p_delete_list
fi

if [ $p_prune -eq 1 ]; then
    remove_snapshots $only_in_dest
fi
