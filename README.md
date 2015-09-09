# Baksnapper - backup script for snapper
Baksnapper is a script for backing up snapshots created by the program snapper using btrfs send and receive.

**Disclaimer**: Baksnapper is still very new therefore it haven't been fully tested yet, so use with caution.

## Table of content
- [Installation](#installation)
  - [From source](#from-source)
  - [Arch Linux](#arch-linux)
- [Features](#features)
- [Usage](#usage)
- [Remote backup over ssh](#remote-backup-over-ssh)
- [Limitations](#limitations)

## Installation

### From source
Clone the reposity
```bash
$ git clone git@github.com:plattfot/baksnapper.git
$ cd baksnapper
$ make install PREFIX=<install dir>
```
Default it will be installed in /usr/bin

### Arch Linux
Clone my aur repo and then build the package using the PKBUILD:
```bash
$ git clone git@bitbucket.org:plattfot/aur.git
$ cd aur/baksnapper
$ makepkg -ic
```
## Features
What the latest release supports, and what the goals are for the next
release.

- [x] Backup to local destination.
- [x] Backup to remote destination using ssh.
- [x] Backup all snapshots.
- [x] Backup last snapshot only.
- [x] Backup specific snapshot.
- [x] Prune discarded snapshots.
- [x] Delete specific snapshots.
- [x] Delete all snapshots.

## Usage

First set up snapper to take snapshots of one of your subvolumes, see
this [page](https://wiki.archlinux.org/index.php/Snapper) on how to do
it for Arch Linux.

For example if you want to back up you snapshots from your home
subvolumes to an external hdd mounted at /mnt/backup, run

```bash
$ baksnapper -c home /mnt/backup
```

This will send the last snapshots of home to
/mnt/backup/home/<snapshot nr>

To backup all snapshots run:
```bash
$ baksnapper --config home --all /mnt/backup
```

This will basically mirror what's in /home/.snapshots to
/mnt/backup/home.

To remove backups that snapper has discarded add the flag -p/--prune:

$ baksnapper --config home --all --prune /mnt/backup

To delete specific backups from the backup directory, run:

```bash
$ baksnapper --config home /mnt/backup --delete 1,2,3,63 
```

This will only delete 1,2,3 and 63 if they only contain a info.xml
file and a snapshot dir. Otherwise it will issue a warning and skip
the directory.

For help use the -h/--help flag
```bash
$ baksnapper --help
```
## Remote backup over ssh

In order to be able to backup to a remote location you first need to
install baksnapper on the remote machine, or atleast copy over the
baksnapperd script.

Next up is to create a ssh key that baksnapper can use to access the
remote machine.

**Note:** that you need to run this as root, since baksnapper needs
root access to copy the snapshots.

```bash
$ su -
$ ssh-keygen -t rsa -b 4096 -C "$(whoami)@$(hostname)-$(date -I)"
```

Leave the passphrase blank and set the file to something like
/home/username/.ssh/baksnapper_rsa

Append the public key (the one you just created), to
.ssh/authorized_keys for the root account on the **remote machine**.
Open up the .ssh/authorized_keys file on the remote machine and
prepend this to the key

```
command="baksnapperd $SSH_ORIGINAL_COMMAND"
```

Your file should look something like this after.
```
command="baksnapperd $SSH_ORIGINAL_COMMAND" ssh-rsa <BAKSNAPPERD-PUBLIC-KEY>
```

After that test that everything is working by running.
```bash
$ (ssh <remote machine> test-connection) && echo "Connection works"
```

It should print out "Connection works".

If everything is working you can now use remote backup with baksnapper
by using the -s or --ssh flag. For example:

```bash
$ baksnapper --config home --all --ssh /mnt/backup 
```

## Limitations

Known limitations for version 1.0.0.

You cannot pack option flags i.e:

```bash
$ baksnapper -apc home /mnt/backup
```

instead you have to do:

```bash
$ baksnapper -a -p -c home /mnt/backup
```



