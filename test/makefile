GPRCONFIG = gprconfig
GPRBUILD  = gprbuild
GPRCLEAN  = gprclean

.PHONY: all test

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

test: $(SUPPORTLIB)
	$(GPRCONFIG) --config=Ada$(RTS_CONF) --config=C --config=ASM --batch $(CONF_ARGS) -o $(CONF_FILE)
	$(GPRBUILD) -p -Paunit_tests -XRUNTIME=$(RTS) -XPLATFORM=$(TARGET) --config=$(CONF_FILE) $(LARGS)
	-$(RUN) $(TARGET)-$(RTS)/aunit_harness

clean:
	$(RM) -rf obj support/obj support/lib $(TARGET)-$(RTS)

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
