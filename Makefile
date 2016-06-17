RTS =
TARGET =
GPRBUILD  = gprbuild
GPRCLEAN = gprclean
GPRINSTALL = gprinstall

INSTALL:=$(shell exec=`which gprbuild`;if [ ! -x "$$exec" ]; then unset exec;fi;echo $$exec | sed -e 's/\/bin\/$(GPRBUILD).*//')

ifeq ($(RTS),)
   RTS=full
   RTS_CONF =
else
   RTS_CONF = --RTS=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET=native
   TARGET_CONF =
else
   TARGET_CONF = --target=$(TARGET)
endif

MODE = Install

CONF_ARGS = $(TARGET_CONF) $(RTS_CONF)

GPROPTS = $(CONF_ARGS) -XMODE=$(MODE) -XRUNTIME=$(RTS) -XPLATFORM=$(TARGET)

.PHONY: all clean targets install_clean install

all:
	$(GPRBUILD) -p $(GPROPTS) lib/gnat/aunit.gpr

clean-lib:
	$(RM) -fr lib/aunit lib/aunit-obj

clean: clean-lib
	-${MAKE} -C docs clean

install-clean:
ifneq (,$(wildcard $(INSTALL)/lib/gnat/manifests/aunit))
	-$(GPRINSTALL) $(GPROPTS) --uninstall --prefix=$(INSTALL) \
		--project-subdir=lib/gnat aunit
endif

install: install-clean
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(INSTALL) \
		--no-build-var --project-subdir=lib/gnat lib/gnat/aunit.gpr

.PHONY: doc
doc:
	${MAKE} -C doc

RM	= rm
