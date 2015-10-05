VERSION=0.7.0
COMPONENT_INTERNAL=watchdog
COMPONENT=fifo-watchdog
DEPS="erlang" "coreutils" "sudo"

include ../../_build/default/lib/fifo_utils/priv/pkg.mk

.PHONY: prepare

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT)
	cp -r ../$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)

package: prepare $(FILE).tgz

clean: clean-pkg
	-rm *.tgz
