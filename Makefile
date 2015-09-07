PREFIX ?= /usr/bin
.PHONY: usage
usage:
	@echo "To install baksnapper run make install. The default is to install the"
	@echo "script to /usr/bin, to change this behaviour add PREFIX=<path> to the"
	@echo "make install command"

$(PREFIX):
	mkdir -p $@

.PHONY: install
install: | $(PREFIX)
	@echo "Installing baksnapper to $(PREFIX)"
	@cp baksnapper.sh $(PREFIX)/baksnapper

.PHONY: uninstall
uninstall:
	@echo "Uninstalling baksnapper from $(PREFIX)"
	@test -d $(PREFIX) && rm -f -- $(PREFIX)/baksnapper
