#! /bin/bash

if [[ ! -z "$1" ]]; then
    subvolume=$(sudo snapper -c $1 get-config | grep SUBVOLUME | awk '{ print $3 }')
    echo $subvolume
    
    
else
    echo "Usage: $0 <snapper config name>"
fi
