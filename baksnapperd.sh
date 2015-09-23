#! /bin/bash

BS_LOCK=/tmp/baksnapperd.lock

function error {
    echo "[ERROR] $1" 1>&2
    exit 1
}

function warning {
    echo -e "[Warning] $1" 1>&2
}

case "$1" in 
    list-snapshots)
        shift
        snapshots=$(find $1 -mindepth 1 -maxdepth 1 \
                         -printf "%f\n" | sort -g)
        echo "$snapshots"
        ;;
    create-config)
        shift
        mkdir -p $1
        ;;
    create-snapshot)
        shift
        mkdir -p $1/$2
        ;;
    receive-info)
        shift
        info=$1/$2/info.xml
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
        echo "$1/$2"
        btrfs receive $1/$2
        ;;
    remove_snapshots)
        shift
        dest_root=$1
        shift
        for snapshot in $@
        do
            # Only delete directories containing info.xml and snapshot
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
