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

$(PREFIX_BIN) $(PREFIX_LIB)/systemd/system $(PREFIX_ETC)/baksnapper:
	@install -d $@

.PHONY: install bin systemd etc
install: bin systemd

bin: | $(PREFIX_BIN)
	@install  baksnapper.sh $|/baksnapper
	@install  baksnapperd.sh $|/baksnapperd

systemd: etc | $(PREFIX_LIB)/systemd/system
	@install systemd/baksnapper@.timer $|/
	@sed "s|<BSCONF_ROOT>|$(BSCONF_ROOT)|" systemd/baksnapper@.service.template \
	     > $|/baksnapper@.service
	@chmod 755 $|/baksnapper@.service

etc: | $(PREFIX_ETC)/baksnapper
	@install systemd/example.bsconf $|/

