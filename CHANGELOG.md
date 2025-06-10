<!-- SPDX-FileCopyrightText: 2025 Fredrik Salomonsson <plattfot@posteo.net> -->
<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Unreleased entries are located in [CHANGELOG.d](./CHANGELOG.d).

## [2.4.1] - 2025-06-09
### Fixed
- Backing up snapshot with the wrong parent. [#27](https://github.com/plattfot/baksnapper/issues/27)

## [2.4.0] - 2025-06-08
### Added
- Meson build support. [#28](https://github.com/plattfot/baksnapper/issues/28)
- Summary report; prints destination, snapper ids, bytes of data transfer, start time, end time and duration — by Jürgen Gleiss.
- A simple test framework. [#29](https://github.com/plattfot/baksnapper/issues/29)
### Changed
- Migrate to a conflictless Keep A Changelog. [#40](https://github.com/plattfot/baksnapper/issues/40)
### Fixed
- `--delete` does not handle multiple snapshots.
- Latest link won't be created in all cases — by Jürgen Gleiss.

## [2.3.0] - 2025-01-04
### Added
- Cleanup broken snapshots.

## [2.2.3] - 2023-04-02
### Fixed
- Fix bad array subscript when there are no source snapshots.

## [2.2.2] - 2023-03-19
### Fixed
- Version 3.0 of REUSE specification compliant.

## [2.2.1] - 2023-02-21
### Fixed
- Proper sort the snapshots to always link the latest — by Juergen Gleiss.

## [2.2.0] - 2022-08-18
### Fixed
- `baksnapper` to run without dbus — by Nathan Dehnel.
- Issue that `baksnapper` fails when there is only one snapshot to backup.

## [2.1.1] - 2021-04-01
### Fixed
- Delay startup until after network-online.target for systemd timer — by Nathan Dehnel.

## [2.1.0] - 2021-01-23
### Added
- Feature for creating a symlink to the latest snapshot (`--link`) — by Nathan Dehnel.

## [2.0.0] - 2020-08-14
### Fixed
- Cleaned up a bunch of warnings and errors reported by `shellcheck`.

## [1.0.2] - 2019-06-25
### Removed
- Executable bit on systemd units, to fix warning.

## [1.0.1] - 2019-06-18
### Fixed
- Call to `baksnapperd` was broken, only `--daemon` was working.

## [1.0.0] - 2019-06-09
### Added
- Cleaning up snapshots if it fails.
### Changed
- `--verbose` now uses `set -x`.
### Fixed
- Errors in the examples in help text.
### Removed
- Deprecated --ssh option.

## [0.9.1] - 2018-09-23
### Added
- Added note about ssh connection in the `README.md` for the config file.
### Changed
- Updated the example config file.
### Fixed
- Wasn't parsing the `PRIVATE_KEY` in the config file.

## [0.9.0] - 2018-09-13
### Added
- Option to specify what private key to use (`--private-key`).
- Config option `PRIVATE_KEY`, equivalent to `--private-key`.

## [0.8.0] - 2018-09-09
### Changed
- Using getopt (`util-linux`) to parse the options.
### Removed
- Some short options that takes additional arguments.

## [0.7.1] - 2018-01-01
### Fixed
-  Missing help message for `--configfile`.

## [0.7.0] - 2017-09-17
### Added
- Option that prints out the version (`--version`).
### Changed
- ssh syntax. Removed the --ssh option, use ADDRESS:PATH instead.
### Fixed
- Error messagn when libnotify isn't installed.
- Bug in incremental send that left `info.xml` empty at destination.

## [0.3.0] - 2016-03-07
### Added
- Support for libnotify to notify the user when `baksnapper` is running.

## [0.2.0] - 2015-09-08
### Added
- Template systemd files.
- Remote backup support via ssh, (`-s <address>`/`--ssh <address>`).
- Option to send specific snapshot (`-S <number>`/`--snapshot <number>`).
- Option to delete all snapshots (`--delete-all`).
- Option to specify name of `baksnapperd` (`--daemon`).
### Changed
- Split the script into two parts, `baksnapper` and `baksnapperd`.

## [0.1.0] - 2015-09-07
### Added
- Support for backing up the latest snapshot (default behavior).
- Support for backing up all snapshots in source dir (`-a`/`--all`).
- Support for deleting specific snapshots from the backup (`-d <list>`/`--delete <list>`).
- Support for pruning the backup directory (`-p`/`--prune`).
