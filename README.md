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
- [Config file](#config-file)
- [Systemd]($systemd)
- [Limitations](#limitations)

## Installation

### From source
Clone the reposity
```bash
$ git clone git@github.com:plattfot/baksnapper.git
$ cd baksnapper
$ make install PREFIX=<install dir>
```

Default the scripts will be installed in /usr/bin, the systemd files
in /usr/lib/systemd/system and the config file in /etc/baksnapper/example. If
you are only packaging up the source please change BSCONF_ROOT to
where the final path for baksnapper/example will be. Otherwise the
systemd unit files will not work.

For example
```bash
$ make install PREFIX=pkg BSCONF_ROOT=/etc
```

Will install everything into the directory pkg, and the systemd unit
file will search for the config files in /etc/baksnapper/.

### Arch Linux
Clone this repo and then build the package using the PKBUILD:
```bash
$ git clone https://github.com/plattfot/baksnapper.git
$ cd build-aux/baksnapper
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
- [x] Config file
- [x] Generic systemd files
- [x] Example systemd files
- [x] Backup from remote destination using ssh (Pull)
- [ ] Run as none root user.

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

This will mirror what's in /home/.snapshots to /mnt/backup/home.

To remove backups that snapper has discarded add the flag -p/--prune:
```bash
$ baksnapper --config home --all --prune /mnt/backup
```

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

If everything is working you can now use remote backup with
baksnapper.  It uses the same syntax as scp,
i.e. <address>:<path>. For example backup to machine named foo:

```bash
$ baksnapper --config home --all foo:/mnt/backup
```

**Tip:** If your are using a nonstandard ssh port you can specify it
in the .ssh/config, for example
```bash
Host foo
Port 666
```

**Tip:** If you have multiple keys, you can specify what key
baksnapper should use either with the option
```bash
$ baksnapper config home --all foo:/mnt/backup --private-key /root/.ssh/<BAKSNAPPERD-PRIVATE-KEY>
```
Or specify that in the Config file using PRIVATE_KEY.

## Config file

It can be tiresome to pass the options to baksnapper all the time, you
can therefore also create a config files in which you specify the
options you wish baksnapper to use. This is also what the systemd unit
file is using, more on that later.  To read a config file use the
command line option -f <conf> or --configfile <conf>

The config file syntax is pretty simple: <COMMAND> = <VALUE>
\# for comments.

For boolean parameters YES, yes, yes and 1 are interpret as on/true and
anything else as off/false.

The supported options are:
* CONFIG: The snapper config to backup. Same as --config.
* PATH: Destination to backup to. For remote backup use
        hostname:/backup/to. Where 'hostname' is the name of the
        server and '/backup/to' is the path to the location it should
        write the backups to on the remote host.
* PRUNE: Prune the backups by deleting snapshots that isn't in the source directory.
       Same as -p, --prune
* ALL: Send all snapshots in the soruce directory. Same as -a, --all
* VERBOSE: Verbose print out, same as -v, --verbose.
* DAEMON: Name of the baksnapper daemon, default is baksnapperd. Same as --daemon
* TYPE: Define if it should push the backup or pull. Default is push.
  	Pull can be used to transfer a backup from a remote server to
  	the current machine.
* PRIVATE_KEY: The private key file to use when connecting to a remote
  server. Same as --private-key.

**Note:** Command line options will take precendence over config file options.
**Note:** No variable expansion is supported in the config files, for
example you cannot for example type ~/backup, instead you need to
expand ~ yourself i.e /home/bob/backup if $USER = bob.

## Systemd

Baksnapper also supplies a timer and service file so that you can use
systemd to handle the backup.
For example to use root.bsconf you simply call

```bash
systemctl start baksnapper@root.bsconf.timer
```

Where it looks for config files depends on what BSCONF_ROOT was set to
when installing the package, by default this will be /etc/baksnapper/.

## Limitations

Known limitations for version 0.4.0.

You cannot pack option flags i.e:

```bash
$ baksnapper -apc home /mnt/backup
```

instead you have to do:

```bash
$ baksnapper -a -p -c home /mnt/backup
```

Config files doesn't support variable expansion,
