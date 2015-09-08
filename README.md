# Baksnapper - backup script for snapper
Baksnapper is a script for backing up snapshots created by the program snapper using btrfs send and receive.

**Disclaimer**: Baksnapper is still very new therefore it haven't been fully tested yet, so use with caution.

## Table of content
- [Installation](#installation)
  - [From source](#from-source)
  - [Arch Linux](#arch-linux)
- [Usage](#usage)
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

## Limitations

Known limitations for version 0.1.0.

It can only backup locally, i.e. you cannot send backups over ssh to a
remote server.

You cannot pack option flags i.e:

```bash
$ baksnapper -apc home /mnt/backup
```

instead you have to do:

```bash
$ baksnapper -a -p -c home /mnt/backup
```

You cannot specify which snapshot to backup, it's either the last one
or all of them.


