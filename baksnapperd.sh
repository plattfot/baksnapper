#! /bin/bash

BS_LOCK=/tmp/baksnapperd.lock

function error {
    echo "[ERROR] $1" 1>&2
    exit 1
}

function warning {
    echo -e "[Warning] $1" 1>&2
}

function check-lock {
    [ ! -e $BS_LOCK ] && error "No lock file found, "\
                               "Run init before calling any other command"
}

case "$1" in 
    init)
        shift
        [ -e $BS_LOCK ] && error "Lock already exist. "\
                                 "Cannot backup to multiple locations at once!"
        [ -d $1 ] || error "Backup directory doesn't exist!"
        echo $1 > $BS_LOCK
        ;;
    fin)
        rm -f -- $BS_LOCK
        ;;
    list-snapshots)
        shift
        check-lock
        snapshots=$(find $(cat $BS_LOCK)/$1 -mindepth 1 -maxdepth 1 \
                         -printf "%f\n" | sort -g)
        echo "$snapshots"
        ;;
    create-config)
        shift
        check-lock
        mkdir -p $(cat $BS_LOCK)/$1
        ;;
    create-snapshot)
        shift
        check-lock
        mkdir -p $(cat $BS_LOCK)/$1/$2
        ;;
    receive-info)
        shift
        info=$(cat $BS_LOCK)/$1/$2/info.xml
        [ -e $info ] && rm -f -- $info
        touch $info
        # Read from stdin
        while read -r line || [[ -n $line ]]
        do
            echo $line >> $info
        done
        ;;
    receive-snapshot)
        shift
        check-lock
        btrfs receive $(cat $BS_LOCK)/$1/$2
        ;;
    remove_snapshots)
        shift
        check-lock
        dest_root=$(cat $BS_LOCK)/$1
        shift
        for snapshot in $@
        do
            # Only delete directories containing info.xmp and snapshot
            content=($(find $dest_root/$snapshot \
                            -maxdepth 1 -mindepth 1 -printf "%f\n" 2> /dev/null | sort ))
            echo ${content[@]}
            if [[ ${#content[@]} == 2 && \
                        ${content[0]} == "info.xml" && \
                        ${content[1]}="snapshot" ]]; then
                echo "Deleting snapshot $snapshot"
                btrfs subvolume delete $dest_root/$snapshot/snapshot
                rm -r -- $dest_root/$snapshot
            else
                warning "Snapshot $snapshot doesn't match a snapper snapshot, "\
                        "ignoring it."
            fi
        done    

        ;;
    test-connection)
        exit 0
        ;;
    *)
        error "Unrecognized command, bailing out!"
        ;;
esac
