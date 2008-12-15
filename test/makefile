.PHONY: all test

all: test

-include Makefile.zfp

RTS =
TARGET =

ifeq ($(RTS),)
   RTS = full
   RTS_CONF =
   RTS_ARG = -XRUNTIME=full
else
   RTS_CONF = ,,$(RTS)
   RTS_ARG = -XRUNTIME=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET = native
   TARGET_CONF =
   TARGET_ARG = -XPLATFORM=native
else
   TARGET_CONF = --target=$(TARGET)
   TARGET_ARG = -XPLATFORM=$(TARGET)
endif

ifeq ($(SUPPORTLIB),)
GPRBUILD_FLAGS = $(TARGET_ARG) $(RTS_ARG)
else
GPRBUILD_FLAGS = $(TARGET_ARG) $(RTS_ARG)
endif

GPRCONFIG = gprconfig
GPRBUILD  = gprbuild
GPRCLEAN  = gprclean

ifeq ($(TARGET),powerpc-elf)
   RUN = ./support/run-ppc-elf
else
   RUN =
endif

test: $(SUPPORTLIB)
	$(GPRCONFIG) $(TARGET_CONF) --config=Ada$(RTS_CONF) --config=C --config=ASM --batch
	$(GPRBUILD) -p $(TARGET_CONF) -Paunit_tests $(GPRBUILD_FLAGS) $(LARGS)
	-$(RUN) $(TARGET)-$(RTS)/aunit_harness

clean: $(CLEANSUPPORTLIB)
	-$(GPRCLEAN) $(TARGET_CONF) -f -r -Paunit_tests $(GPRBUILD_FLAGS)
	$(RM) -rf obj support/obj support/lib $(TARGET)-$(RTS)

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
