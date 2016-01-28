GPRBUILD  = gprbuild
GPRCLEAN  = gprclean

.PHONY: all test force

all: test

RTS =
TARGET =
PROJECT_PATH_ARG =

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

RUN=
ifeq ($(TARGET),powerpc-elf)
   RUN=./support/run-ppc-elf
endif

build:
	$(PROJECT_PATH_ARG) $(GPRBUILD) -p -Paunit_tests $(CONF_ARGS) $(LARGS)

run: build
	-$(RUN) ./exe/$(TARGET)-$(RTS)/aunit_harness$(exeext)

test: build
	-$(RUN) ./exe/aunit_harness$(exeext) > test.out.full 2>&1
	egrep "^Total|^Success|^Fail|^Unexp" test.out.full > test.out
	diff -b test.out expected.out
	@echo
	@echo "[OK] AUNIT TEST IS SUCCESSFUL"

clean:
	$(RM) -rf obj exe support/obj support/lib *.cgpr test.out

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
