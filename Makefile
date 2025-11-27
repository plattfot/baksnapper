# SPDX-FileCopyrightText: 2023-2025 Fredrik Salomonsson <plattfot@posteo.net>
#
# SPDX-License-Identifier: GPL-3.0-or-later

PREFIX ?= /
PREFIX_BIN ?= $(PREFIX)/usr/bin
PREFIX_LIB ?= $(PREFIX)/usr/lib
PREFIX_ETC ?= $(PREFIX)/etc
SYSTEMD_CONF ?= $(PREFIX_ETC)
.PHONY: usage
usage:
	@echo "To install baksnapper run make install. The default is to install the"
	@echo "script to '/'. To change this behaviour add PREFIX=<path> to the"
	@echo "make install command"
	@echo "For a more fine grain control there are four different variables "
	@echo "you can change."
	@echo ""
	@echo "PREFIX_BIN: Where the baksnapper scripts will end up."
	@echo "            Default is PREFIX/usr/bin."
	@echo "PREFIX_LIB: Where the systemd scripts will end up."
	@echo "            Default is PREFIX/usr/lib."
	@echo "PREFIX_ETC: Where the example config file will end up."
	@echo "            Default is PREFIX/etc."
	@echo "BSCONF_ROOT: Path to where the systemd service unit will look for"
	@echo "             config files, default is PREFIX_ETC."

$(PREFIX_BIN) $(PREFIX_LIB)/systemd/system $(PREFIX_ETC)/baksnapper/example:
	@install -d $@

.PHONY: install bin systemd etc
install: bin systemd

__root_dir=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
__version=$(shell $(__root_dir)build-aux/version)
bin: | $(PREFIX_BIN)
	@sed 's/@VERSION@/${__version}/' baksnapper.sh > $|/baksnapper
	@chmod 744 $|/baksnapper
	@sed 's/@VERSION@/${__version}/' baksnapperd.sh > $|/baksnapperd
	@chmod 744 $|/baksnapperd

systemd: etc | $(PREFIX_LIB)/systemd/system
	@install --mode=644 systemd/baksnapper@.timer $|/
	@sed "s|@BSCONF_ROOT@|$(BSCONF_ROOT)|" \
	     systemd/baksnapper@.service.in \
	     > $|/baksnapper@.service
	@chmod 644 $|/baksnapper@.service

etc: | $(PREFIX_ETC)/baksnapper/example
	@install systemd/example.bsconf $|/root.bsconf

