RTS =
TARGET =

ifeq ($(RTS),)
   RTS_CONF =
   RTS_ARG = -XRUNTIME=full
else
   RTS_CONF = ,,$(RTS)
   RTS_ARG = -XRUNTIME=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET_CONF =
   TARGET_ARG = -XPLATFORM=native
else
   TARGET_CONF = --target=$(TARGET)
ifneq ($(filter %-wrs-vxworksae,$(TARGET)),)
   TARGET_ARG = -XPLATFORM=vxworksae
else
   TARGET_ARG = -XPLATFORM=$(TARGET)
endif
endif

GPRBUILD_FLAGS = $(TARGET_ARG) $(RTS_ARG)

GPRCONFIG = gprconfig
GPRBUILD  = gprbuild
GPRCLEAN  = gprclean

ifeq ($(TARGET),powerpc-elf)
   RUN = ./support/run-ppc-elf
else
   RUN =
endif

all:
	echo $(TARGET_ARG)
	$(GPRCONFIG) $(TARGET_CONF) --config=Ada$(RTS_CONF) --config=C --config=ASM --batch
	$(GPRBUILD) $(TARGET_CONF) -Paunit_tests -p $(GPRBUILD_FLAGS)
	-$(RUN) ./aunit_harness

clean:
	-$(GPRCLEAN) -f -r -Paunit_tests $(GPRBUILD_FLAGS)

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
