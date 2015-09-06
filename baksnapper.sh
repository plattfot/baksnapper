#! /bin/bash

help="Usage: $0 [OPTIONS]... [PATH]\n\n"\
"\tBackup snapper snapshot to [PATH] using btrfs incremental send and\n"\
"\treceive.\n\n"\
"Options:\n"\
"\t-c <name> --config=<name>\t Name of config.\n"\
"\t-i --init\t\t\t Initial backup, will send the whole snapshot.\n"\
"\t-p --prune\t\t\t Prune the backups by deleting snapshots that \n"\
"\t\t\t\t\t isn't in the source directory.\n"\
"\t-a --all\t\t\t Send all snapshots in the source directory. \n"\
"\t\t\t\t\t Default is to only send the last one.\n"\
"\t-v --verbose\t\t\t Verbose print out.\n"\
"\t-h --help\t\t\t Print this help and then exit.\n"\
"Example:\n"\
"\t$0 -c root /mnt/backup"\
"Note:\n"\
"\tThis doesn't support option stacking e.g. -ic <name>. Instead you\n"\
"\tneed to separate each option i.e. -i -c <name>\n"\
"Also this need root to be able to backup"\
"Exit status:\n"\
" 0 if ok.\n"\
" 1 if option error (e.g. wrong flag etc).\n"\
"Author:\n"\
"\tFredrik Salomonsson"\
""

function throw_error {
    echo -e "[ERROR] $1"
    exit 1
}

function warning {
    echo -e "[Warning] "
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

if [[ $USER != root ]]; then
    throw_error "Need to be root to run this script!"
fi

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
printv $verbose "prune=${prune}"
printv $verbose "all=${all}"

# Error checks
if [[ -z $config ]]; then
    throw_error "You need to specify the config name to backup!"
fi

if [[ -z $dest ]]; then
    throw_error "No path specified!"
fi

# Get the subvolume to backup
subvolume=$(snapper -c $config get-config | grep SUBVOLUME | awk '{ print $3 }')
printv $verbose "subvolume=$subvolume"

# List all the snapshots available

snapshots=($(find $subvolume/.snapshots -mindepth 1 -maxdepth 1 -printf "%f "))

num_snapshots=${#snapshots[@]}
if [[ $num_snapshots == 0 ]]; then
    throw_error "No snapshots found."
fi

src_root=$subvolume/.snapshots
dest_root=$dest/$config
diff=$(diff $src_root $dest_root)

common=($(echo "$diff"| sed -En "s|Common sub.*?${dest_root}/([0-9]+)|\1|p"| sort -g))
only_in_src=($(echo "$diff"| sed -En "s|Only in ${src_root}: ([0-9]+)|\1|p"| sort -g))
only_in_dest=($(echo "$diff"| sed -En "s|Only in ${dest_root}: ([0-9]+)|\1|p"| sort -g))

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

function incremental_backup {
    echo "Incremental backup"

    printv $verbose "reference=$1"
    printv $verbose "snapshot=$2"

    dest_dir=$dest/$config/$2
    src_dir=$subvolume/.snapshots/$2
    ref_dir=$subvolume/.snapshots/$1

    mkdir -p $dest_dir
    cp $src_dir/info.xml $dest_dir
    btrfs send -p $ref_dir/snapshot $src_dir/snapshot| btrfs receive $dest_dir
}

if [[ $all == 0 ]]; then
    common_last=
    if [[ ${common[(( ${#common[@]}-1 ))]} == ${snapshot[num_snapshots-1]} ]]; then
        echo "Already synced the last snapshot"
    fi
else
    echo "Sync all"
fi

# if (( $init == 1 )); then
#     echo "Initialize backup"
#     snapshot=${snapshots[0]} 
    
#     printv $verbose "snapshot=$snapshot"
#     dest_dir=$dest_root/$snapshot
#     src_dir=$src_root/$snapshot

#     mkdir -p $dest_dir
#     cp $src_dir/info.xml $dest_dir
#     btrfs send $src_dir/snapshot| btrfs receive $dest_dir
# else
#     echo "Incremental backup"
#     reference=${snapshots[0]}
#     snapshot=${snapshots[$num_snapshots-1]}

#     printv $verbose "reference=$reference"
#     printv $verbose "snapshot=$snapshot"

#     dest_dir=$dest/$config/$snapshot
#     src_dir=$subvolume/.snapshots/$snapshot
#     ref_dir=$subvolume/.snapshots/$reference

#     mkdir -p $dest_dir
#     cp $src_dir/info.xml $dest_dir
#     btrfs send -p $ref_dir/snapshot $src_dir/snapshot| btrfs receive $dest_dir
# fi

# if (( $prune == 1 )); then
#     snapshots=$(echo $diff | grep -i $dest | awk '{ print $2 }')
#     printv $verbose "snapshots to delete = $snapshots"
#     for snapshot in $snapshots
#     do
#         # Only delete directories containing info.xmp and snapshot
#         content=($(find $dest_root/$snapshot -maxdepth 1 -mindepth 1 -printf "%f "))
#         if [[ ${#content[@]} == 2 && ${content[0]} == "info.xml" && ${content[1]="snapshot"} ]]; then
#             echo "Deleting snapshot $snapshot"
#             btrfs subvolume delete $snapshot/snapshot
#             rm -r -- $snapshot
#         else
#             warning "Snapshot $snapshot doesn't match a snapper snapshot, ignoring it."
#         fi
#     done
# fi



