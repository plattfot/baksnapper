#! /bin/bash

help="Usage: $0 [OPTIONS]... [PATH]\n"\
"Backup snapper snapshot to [PATH] using btrfs incremental send and receive."\
"Options:\n"\
"\t-c <name>|--config=<name>\t Name of config.\n"\
"\t-i |--init\t Initial backup, will send the whole snapshot.\n"\
"\t-v |--verbose\t Verbose print out.\n"\
"\t-h|--help\t Print this help and then exit.\n"\
"Example:\n"\
"\t$0 -c root /mnt/backup"\
"Note:\n"\
"This doesn't support option stacking e.g. -ic <name>. Instead you need to separate each option i.e. -i -c <name>\n"\
"Also this need root to be able to backup"\
"Exit status:\n"\
" 0 if ok.\n"\
" 1 if option error (e.g. wrong flag etc).\n"\
""

function throw_error {
    echo -e "[ERROR] $1"
    exit 1
}

function printv {
    if [[ $1 = 1 ]]; then
        echo -e $2
    fi
}

# Default values
init=0
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
    -i|--init)
        init=1
        shift
        ;;
    -c)
        config="$2"
        shift 2
        ;;
    -v|--verbose)
        verbose=1
        shift
        ;;
    -h|--help)
        echo -e $help
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

printv $verbose "config=${config}"
printv $verbose "dest=${dest}"
printv $verbose "init=${init}"

# Error checks
if [[ -z $config ]]; then
    throw_error "You need to specify the config name to backup!"
fi

if [[ -z $dest ]]; then
    throw_error "No path specified!"
fi

# Get the subvolume to backup
subvolume=$(sudo snapper -c $config get-config | grep SUBVOLUME | awk '{ print $3 }')

# List all the snapshots available
snapshots=$(find $subvolume/.snapshots -mindepth 1 -maxdepth 1 -printf "%f ")
snapshots_a=($snapshots)

printv $verbose "snapshots=$snapshots"

if [[ $init = 1 ]]; then
    echo "Initialize backup"
    snapshot=${snapshots_a[0]} 
    
    printv $verbose "snapshot=$snapshot"
    dest_dir=$dest/$config/$snapshot
    src_dir=$subvolume/.snapshots/$snapshot

    echo "mkdir $dest_dir"
    echo "cp $src_dir/info.xml $dest_dir"
    echo "sudo btrfs send $src_dir/snapshot | btrfs receive $dest_dir"
else
    echo "Incremental backup"
    reference=${snapshots_a[0]}
    num_snapshots=${#snapshots_a[@]}
    snapshot=${snapshots_a[$num_snapshots-1]}

    printv $verbose "reference=$reference"
    printv $verbose "snapshot=$snapshot"

    dest_dir=$dest/$config/$snapshot
    src_dir=$subvolume/.snapshots/$snapshot
    ref_dir=$subvolume/.snapshots/$reference
    echo "mkdir $dest_dir"
    echo "cp $src_dir/info.xml $dest_dir"
    echo "sudo btrfs send -p $ref_dir/snapshot $src_dir/snapshot | btrfs receive $dest_dir"
fi

printv $verbose"subvolume=$subvolume"


