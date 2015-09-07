#! /bin/bash

read -rd '' help <<EOF
Usage: $0 [OPTIONS]... [PATH]

\tBackup snapper snapshot to [PATH] using btrfs incremental send and
\treceive.

Options:
\t-c <name> --config=<name>\t Name of config.
\t-p --prune\t\t\t Prune the backups by deleting snapshots that 
\t\t\t\t\t isn't in the source directory.
\t-a --all\t\t\t Send all snapshots in the source directory. 
\t\t\t\t\t Default is to only send the last one.
\t-v --verbose\t\t\t Verbose print out.
\t-h --help\t\t\t Print this help and then exit.

Example:
\t$0 -c root /mnt/backup

Note:
\tThis doesn't support option stacking e.g. -ic <name>. Instead you
\tneed to separate each option i.e. -i -c <name>
Also this script needs root to be able to backup snapshots.

Exit status:
\t0 if ok.
\t1 if option error (e.g. wrong flag etc).

Author:
\tFredrik Salomonsson
EOF

function throw_error {
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

# Default values
all=0 
prune=0
verbose=0

# Parse options
while [[ $# > 0 ]]
do
key=$1
case $key in
    --config=*)
        config="${1#*=}"
        shift # past argument=value
        ;;
    -c)
        config="$2"
        shift 2
        ;;
    -v|--verbose)
        verbose=1
        shift
        ;;
    -p|--prune)
        prune=1
        shift
        ;;
    -a|--all)
        all=1
        shift
        ;;
    -h|--help)
        echo -e "$help"
        exit 0
        ;;
    -*)
        #unknown option
        echo "[ERROR] Unknown option $1"
        echo -e $help
        exit 1
        shift
        ;;
    *)
        dest=$1
        shift
        ;;
esac
done

# Error checks
if [[ $USER != root ]]; then
    throw_error "Need to be root to run this script!"
fi

if [[ -z $config ]]; then
    throw_error "You need to specify the config name to backup!"
fi

if [[ -z $dest ]]; then
    throw_error "No path specified!"
fi

if [ ! -e $dest ]; then
    mkdir -p $dest
fi

if [ ! -d $dest ]; then
    throw_error "Backup path specified isn't a directory!"
fi 

printv $verbose "config=${config}"
printv $verbose "dest=${dest}"
printv $verbose "prune=${prune}"
printv $verbose "all=${all}"

# Get the subvolume to backup
subvolume=$(snapper -c $config get-config | grep SUBVOLUME | awk '{ print $3 }')
printv $verbose "subvolume=$subvolume"

# List all the snapshots available

snapshots=($(find $subvolume/.snapshots -mindepth 1 -maxdepth 1 -printf "%f "))

num_snapshots=${#snapshots[@]}
if (( $num_snapshots == 0 )); then
    throw_error "No snapshots found."
fi

src_root=$subvolume/.snapshots
dest_root=$dest/$config
diff=$(diff $src_root $dest_root)

common=($(echo "$diff"| sed -En "s|Common sub.*?${dest_root}/([0-9]+)|\1|p"| sort -g))
only_in_src=($(echo "$diff"| sed -En "s|Only in ${src_root}: ([0-9]+)|\1|p"| sort -g))
only_in_dest=($(echo "$diff"| sed -En "s|Only in ${dest_root}: ([0-9]+)|\1|p"| sort -g))

# Destination doesn't have any snapshots incommon with source.
# Send the whole snapshot.
if [[ -z $common ]]; then
    echo "Initialize backup"
    snapshot=${snapshots[0]} 
    
    printv $verbose "snapshot=$snapshot"
    dest_dir=$dest_root/$snapshot
    src_dir=$src_root/$snapshot

    mkdir -p $dest_dir
    cp $src_dir/info.xml $dest_dir
    btrfs send $src_dir/snapshot| btrfs receive $dest_dir
    
    common=$snapshot
fi

# First argument is the reference snapshot and the second is the
# snapshot to backup. It will only send the difference between the
# two.
function incremental_backup {
    echo "Incremental backup"
    printv $verbose "Backing up snapshot $2 using snapshot $1 as reference."

    dest_dir=$dest/$config/$2
    src_dir=$subvolume/.snapshots/$2
    ref_dir=$subvolume/.snapshots/$1

    mkdir -p $dest_dir
    cp $src_dir/info.xml $dest_dir
    btrfs send -p $ref_dir/snapshot $src_dir/snapshot| btrfs receive $dest_dir
}

if [[ $all == 0 ]]; then
    common_last=${common[(( ${#common[@]}-1 ))]}
    snapshot=${snapshots[num_snapshots-1]}

    # Check that it's not already synced
    if [[ $common_last == ${snapshot[num_snapshots-1]} ]]; then
        throw_error "Already synced the last snapshot"
    fi

    incremental_backup $common_last $snapshot
else
    num_src_only=${#only_in_src[@]}

    if [[ $num_src_only != 0 ]]; then
        # Find the first common snapshot that is lower than the first
        # source only snapshot. This will be the start of the incremental
        # backup. If no one is found it will use the lowest common one.
        first_src_snapshot=${only_in_src[0]}
        idx=${#common[@]}-1
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
    else
        printv $verbose "All snapshots are backed up."
    fi
fi

if (( $prune == 1 )); then
    printv $verbose "snapshots to delete = $only_in_dest"
    for snapshot in $only_in_dest
    do
        # Only delete directories containing info.xmp and snapshot
        content=($(find $dest_root/$snapshot -maxdepth 1 -mindepth 1 -printf "%f "))
        if [[ ${#content[@]} == 2 && ${content[0]} == "info.xml" && ${content[1]="snapshot"} ]]; then
            echo "Deleting snapshot $snapshot"
            btrfs subvolume delete $snapshot/snapshot
            rm -r -- $snapshot
        else
            warning "Snapshot $snapshot doesn't match a snapper snapshot, ignoring it."
        fi
    done
fi
