GPRBUILD ?= gprbuild
PROJECT ?= harness

ifdef INSTRUMENTED
GPROPTS += --implicit-with=gnatcov_rts
endif

all:
	$(GPRBUILD) $(GPROPTS) -p $(BUILD_FLAGS) -P$(PROJECT)

ifndef CUSTOM_CLEAN
clean:
	-gprclean -P$(PROJECT)
	-gprclean -Ptested_lib/testlib
	rm -rf obj tested_lib/obj
endif

.PHONY: all clean
