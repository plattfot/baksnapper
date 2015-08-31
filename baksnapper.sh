#! /bin/bash

help="Usage: $0 [OPTIONS]... [PATH]\n"\
"Backup snapper snapshot to [PATH] using btrfs incremental send and receive."\
"Options:\n"\
"\t-c <name>|--config=<name>\t Name of config.\n"\
"\t-i |--init\t Initial backup, will send the whole snapshot.\n"\
"\t-h|--help\t Print this help and then exit.\n"\
"Example:\n"\
"\t$0 -c root /mnt/backup"\
"Note:\n"\
"This doesn't support option stacking e.g. -ic <name>. Instead you need to separate each option i.e. -i -c <name>\n"\
"Exit status:\n"\
" 0 if ok.\n"\
" 1 if option error (e.g. wrong flag etc).\n"\
""

init=0
#for i in "$@"
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
    -h|--help)
        echo -e $help
        exit 0
        ;;
    -*)
        #unknown option
        echo "Unknown option $1"
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

echo "config=${config}"
echo "dest=${dest}"
echo "init=${init}"
if [[ -z $config ]]; then
    echo "you need to specify the config name to backup!"
    echo -e $help
    exit 1
fi
# if [[ ! -z "$dest" ]]; then
    
#     subvolume=$(sudo snapper -c $1 get-config | grep SUBVOLUME | awk '{ print $3 }')
#     echo $subvolume
    
    
# else
#     echo "Usage: $0 <snapper config name>"
# fi
