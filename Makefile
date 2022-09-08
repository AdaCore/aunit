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

GPROPTS = $(CONF_ARGS) -XAUNIT_BUILD_MODE=$(MODE) -XAUNIT_RUNTIME=$(RTS) \
		-XAUNIT_PLATFORM=$(TARGET)

# For the 64 bits architectures, the large code model has to be used.
# with rtp-large, gprconfig ensures that -mcmodel=large is used,
# but it is managed here for the default (kernel).
GPROPTS_EXTRA=
ifneq ($(strip $(filter aarch64-wrs-vxworks7r2 powerpc64-wrs-vxworks7r2 x86_64-wrs-vxworks7r2,$(TARGET))),)
ifeq (${RTS_CONF},)
# This covers the kernel RTS because for rtp, the RTS_OPT variable is defined to --RTS=rtp.
# kernel is the default and the RTS_OPT is not set in that case.
GPROPTS_EXTRA+=-cargs -mcmodel=large -largs -mcmodel=large
endif
endif

.PHONY: all clean targets install_clean install

all:
	$(GPRBUILD) -p $(GPROPTS) lib/gnat/aunit.gpr ${GPROPTS_EXTRA}

clean-lib:
	$(RM) -fr lib/aunit lib/aunit-obj

clean: clean-lib
	-${MAKE} -C doc clean

install-clean-legacy:
ifneq (,$(wildcard $(INSTALL)/lib/gnat/manifests/aunit))
	-$(GPRINSTALL) $(GPROPTS) --uninstall --prefix=$(INSTALL) \
		--project-subdir=lib/gnat aunit
endif

install-clean: install-clean-legacy
ifneq (,$(wildcard $(INSTALL)/share/gpr/manifests/aunit))
	-$(GPRINSTALL) $(GPROPTS) --uninstall --prefix=$(INSTALL) aunit
endif

install: install-clean
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(INSTALL) \
		--no-build-var lib/gnat/aunit.gpr

.PHONY: doc
doc:
	${MAKE} -C doc

RM	= rm
