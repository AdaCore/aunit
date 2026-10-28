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

# Full runtimes support file-based XML checks. Other runtimes use the
# portable result checks; override TEST_FILEIO for a custom full runtime.
TEST_FILEIO ?= $(if $(filter full,$(RTS)),fileio,nofileio)

CONF_ARGS = $(TARGET_CONF) $(RTS_CONF) \
            -XAUNIT_TEST_FILEIO=$(TEST_FILEIO)

ifeq ($(OS),Windows_NT)
 ifeq ($(TARGET),native)
   exeext=.exe
 endif
endif

ifeq ($(findstring vxworks,$(TARGET)),vxworks)
  ifeq ($(findstring rtp,$(RTS)),rtp)
    exeext=.vxe
  else ifeq ($(findstring rtp,$(RTS_CONF)),rtp)
    # For light-tasking-rtp
    exeext=.vxe
  else
    exeext=.out
  endif
endif

RUN=

build:
	$(PROJECT_PATH_ARG) $(GPRBUILD) -p -Paunit_tests $(CONF_ARGS) $(LARGS)

test: build
	$(RUN) ./exe/aunit_harness_suites$(exeext) > xunit-suite.xml
	$(RUN) ./exe/aunit_harness_cases$(exeext) > xunit-cases.xml
	$(RUN) ./exe/aunit_harness_cases_filter$(exeext) > xunit-cases-filter.xml
	$(RUN) ./exe/aunit_harness_fixtures$(exeext) > xunit-fixtures.xml
	$(RUN) ./exe/aunit_harness_results$(exeext) > xunit-results.xml
	$(RUN) ./exe/aunit_harness_package_filters$(exeext) > xunit-package-filter.xml

clean:
	$(RM) -rf obj exe support/obj support/lib *.cgpr test.out \
		  xunit-*.xml

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
