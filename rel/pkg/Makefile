VERSION=0.7.0
COMPONENT_INTERNAL=watchdog
COMPONENT=fifo-watchdog
DEPS="erlang>=16.1.2" "coreutils>=8.22nb1" "sudo>=1.7.10p9"

include ../../deps/fifo_utils/priv/pkg.mk

.PHONY: prepare

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT)
	cp -r ../$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)

package: prepare $(FILE).tgz

clean: clean-pkg
	-rm *.tgz
