RTS =
TARGET =

ifeq ($(RTS),)
   RTS_CONF =
   RTS_ARG = -XRTS=full
else
   RTS_CONF = ,,$(RTS)
   RTS_ARG = -XRTS=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET_CONF =
   TARGET_ARG = -XTARGET=native
else
   TARGET_CONF = --target=$(TARGET)
ifneq ($(filter %-wrs-vxworksae,$(TARGET)),)
   TARGET_ARG = -XTARGET=vxworksae
else
   TARGET_ARG = -XTARGET=$(TARGET)
endif
endif

GPRBUILD_FLAGS = $(TARGET_ARG) $(RTS_ARG)

GPRCONFIG = gprconfig
GPRBUILD  = gprbuild
GPRCLEAN  = gprclean

ifeq ($(TARGET),powerpc-elf)
   RUN = ./run
else
   RUN =
endif

all:
	echo $(TARGET_ARG)
	-$(MKDIR) obj
	-$(MKDIR) support/obj
	-$(MKDIR) support/lib
	$(GPRCONFIG) $(TARGET_CONF) --config=Ada$(RTS_CONF) --config=C --config=ASM --batch -o gprconf.cgpr
	$(GPRBUILD) --config=gprconf.cgpr -Paunit_tests $(GPRBUILD_FLAGS)
	-$(RUN) ./aunit_harness

clean:
	-$(GPRCLEAN) -f -r -Paunit_tests $(GPRBUILD_FLAGS)

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
