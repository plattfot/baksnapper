#! /bin/bash

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
    echo -e "[ERROR] $1" 1>&2
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
    -*)
        #unknown option
        echo "[ERROR] Unknown option $1"
        echo -e $help
        exit 1
        shift
        ;;
    *)
        p_dest=$1
        shift
        ;;
esac
done

# Error checks
if [[ $USER != root ]]; then
    error "Need to be root to run this script!"
fi

if [[ -z $p_config ]]; then
    error "You need to specify the config name to backup!"
fi

if [[ -z $p_dest ]]; then
    error "No path specified!"
fi

if [ ! -e $p_dest ]; then
    mkdir -p $p_dest
fi

if [ ! -d $p_dest ]; then
    error "Backup path specified isn't a directory!"
fi 

printv $p_verbose "p_config=${p_config}"
printv $p_verbose "p_dest=${p_dest}"
printv $p_verbose "p_prune=${p_prune}"
printv $p_verbose "p_all=${p_all}"
printv $p_verbose "p_delete=${p_delete}"

# Get the subvolume to backup
subvolume=$(snapper -c $p_config get-config | grep SUBVOLUME | awk '{ print $3 }')
printv $p_verbose "subvolume=$subvolume"

src_root=$subvolume/.snapshots
dest_root=$p_dest/$p_config
diff=$(diff $src_root $dest_root)

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

    # List all the snapshots available
    local snapshots=($(find $subvolume/.snapshots -mindepth 1 -maxdepth 1 -printf "%f "))
    local num_snapshots=${#snapshots[@]}

    if [ $num_snapshots -eq 0 ]; then
        error "No snapshots found."
    fi

    # Get the snapshots the source and destination shares.
    common=($(echo "$diff"| sed -En "s|Common sub.*?${dest_root}/([0-9]+)|\1|p"| sort -g))
    # And what is only in source
    only_in_src=($(echo "$diff"| sed -En "s|Only in ${src_root}: ([0-9]+)|\1|p"| sort -g))
    
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
            local snapshot=${snapshots[0]} 
            single_backup $snapshot
            common=$snapshot
        else
            # Just send the last one and exit.
            local snapshot=${snapshots[$num_snapshots-1]}
            single_backup $snapshot
            exit 0
        fi
    fi

    if [[ $p_all == 0 ]]; then
        local common_last=${common[(( ${#common[@]}-1 ))]}
        local snapshot=${snapshots[num_snapshots-1]}

        # Check that it's not already synced
        if [[ $common_last == ${snapshot[num_snapshots-1]} ]]; then
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
        local content=($(find $dest_root/$snapshot -maxdepth 1 -mindepth 1 -printf "%f " 2> /dev/null))
        if [[ ${#content[@]} == 2 && ${content[0]} == "info.xml" && ${content[1]="snapshot"} ]]; then
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
    only_in_dest=($(echo "$diff"| sed -En "s|Only in ${dest_root}: ([0-9]+)|\1|p"| sort -g))
    remove_snapshots $only_in_dest
fi
