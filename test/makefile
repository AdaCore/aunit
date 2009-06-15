GPRCONFIG = gprconfig
GPRBUILD  = gprbuild
GPRCLEAN  = gprclean

.PHONY: all test force

all: test

-include Makefile.zfp

RTS =
TARGET =

ifeq ($(RTS),)
   RTS = full
   RTS_CONF =
else
   RTS_CONF = ,,$(RTS)
endif

ifeq ($(TARGET),)
   TARGET = native
   CONF_ARGS =
else
   CONF_ARGS = --target=$(TARGET)
endif

CONF_FILE = $(TARGET)-$(RTS).cgpr

ifeq ($(TARGET),powerpc-elf)
   RUN = ./support/run-ppc-elf
else
   RUN =
endif

test: $(SUPPORTLIB) aunit_tests.gpr
	$(GPRCONFIG) --config=Ada$(RTS_CONF) --config=C --config=ASM --batch $(CONF_ARGS) -o $(CONF_FILE)
	$(GPRBUILD) -p -Paunit_tests -XRUNTIME=$(RTS) -XPLATFORM=$(TARGET) --config=$(CONF_FILE) $(LARGS)
	-$(RUN) $(TARGET)-$(RTS)/aunit_harness

aunit_tests.gpr: aunit_tests.gpr.in force
ifeq ($(shell ls ../aunit/aunit_build.gpr 2> /dev/null),../aunit/aunit_build.gpr)
	@cat $< | sed -e 's%with "aunit"%with "../aunit/aunit_build"%' -e 's%with "aunit_shared"%with "../support/aunit_shared"%' > $@
else
	@cp $< $@
endif

clean:
	$(RM) -rf obj support/obj support/lib $(TARGET)-$(RTS)

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
