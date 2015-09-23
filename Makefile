PREFIX ?= /usr/bin
.PHONY: usage
usage:
	@echo "To install baksnapper run make install. The default is to install the"
	@echo "script to /usr/bin, to change this behaviour add PREFIX=<path> to the"
	@echo "make install command"
	@echo "To install the systemd unit files run make systemd"

$(PREFIX):
	mkdir -p $@

.PHONY: install
install: | $(PREFIX)
	@echo "Installing baksnapper to $(PREFIX)"
	@cp baksnapper.sh $(PREFIX)/baksnapper
	@cp baksnapperd.sh $(PREFIX)/baksnapperd

.PHONY: uninstall
uninstall:
	@echo "Uninstalling baksnapper from $(PREFIX)"
	@test -d $(PREFIX) && rm -f -- $(PREFIX)/baksnapper
	@test -d $(PREFIX) && rm -f -- $(PREFIX)/baksnapperd

.PHONY: systemd
systemd:
	@mkdir -p /etc/baksnapper
	@cp systemd/example.bsconf
	@cp systemd/baksnapper@* /usr/lib/systemd/system/
