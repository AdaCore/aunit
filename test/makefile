GPRBUILD  = gprbuild
GPRCLEAN  = gprclean

.PHONY: all test force

all: test

RTS =
TARGET =

ifeq ($(RTS),)
   RTS = full
   RTS_CONF =
else
   RTS_CONF = --RTS=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET = native
   TARGET_CONF =
else
   TARGET_CONF = --target=$(TARGET)
endif

CONF_ARGS = $(TARGET_CONF) $(RTS_CONF)

ifeq ($(OS),Windows_NT)
 ifeq ($(TARGET),native)
   exeext=.exe
 endif
endif

ifeq ($(findstring vxworks,$(TARGET)),vxworks)
   exeext=.out
endif

ifeq ($(TARGET),powerpc-elf)
   RUN = ./support/run-ppc-elf
   VARIANT=-XVARIANT=powerpc-elf-psim
else
   RUN =
endif

build: $(SUPPORTLIB) aunit_tests.gpr
	$(GPRBUILD) -p -Paunit_tests $(CONF_ARGS) -XRUNTIME=$(RTS) -XPLATFORM=$(TARGET) $(VARIANT) $(LARGS)

run: build
	-$(RUN) ./exe/$(TARGET)-$(RTS)/aunit_harness$(exeext)

test: build
	-$(RUN) ./exe/$(TARGET)-$(RTS)/aunit_harness$(exeext) >& test.out.full
	cat test.out.full | grep "^[TSFU]" > test.out
	diff -b test.out expected.out
	@echo
	@echo "[OK] AUNIT TEST IS SUCCESSFUL"

aunit_tests.gpr: aunit_tests.gpr.in force
	rm -f $@
	touch $@
ifneq ($(RTS),full)
	echo "with \"zfp_support\";" >> $@
endif
ifeq ($(shell ls ../aunit/aunit_build.gpr 2> /dev/null),../aunit/aunit_build.gpr)
	@cat $< | sed -e 's%with "aunit"%with "../aunit/aunit_build"%' -e 's%with "aunit_shared"%with "../support/aunit_shared"%' >> $@
else
	@cat $< >> $@
endif

clean:
	$(RM) -rf obj exe support/obj support/lib *.cgpr test.out

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
