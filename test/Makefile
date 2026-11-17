GPRBUILD  = gprbuild
GPRCLEAN  = gprclean
GNATCOV = gnatcov
GNATCOV_LEVEL ?= stmt
GNATCOV_DUMP_CHANNEL ?= bin-file

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

ifdef INSTRUMENTED
COVERAGE_BUILD = --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts
endif

build: instrument
	$(PROJECT_PATH_ARG) $(GPRBUILD) -p -Paunit_tests $(CONF_ARGS) $(COVERAGE_BUILD) $(LARGS)

.PHONY: instrument
instrument:
ifdef INSTRUMENTED
	$(PROJECT_PATH_ARG) $(GNATCOV) instrument -P aunit_tests.gpr $(CONF_ARGS) \
		--level=$(GNATCOV_LEVEL) --projects=aunit --externally-built-projects \
		--dump-filename-env-var=GNATCOV_TRACE_DIR \
		--dump-channel=$(GNATCOV_DUMP_CHANNEL)
endif

test: build
	$(RUN) ./exe/aunit_harness_suites$(exeext) > xunit-suite.xml
	$(RUN) ./exe/aunit_harness_cases$(exeext) > xunit-cases.xml
	$(RUN) ./exe/aunit_harness_cases_filter$(exeext) > xunit-cases-filter.xml
	$(RUN) ./exe/aunit_harness_fixtures$(exeext) > xunit-fixtures.xml
	$(RUN) ./exe/aunit_harness_results$(exeext) > xunit-results.xml
	$(RUN) ./exe/aunit_harness_package_filters_named_first$(exeext) $(if $(filter base64-stdout,$(GNATCOV_DUMP_CHANNEL)),> coverage-package-filters-named-first.out)
	$(RUN) ./exe/aunit_harness_package_filters_unnamed_first$(exeext) $(if $(filter base64-stdout,$(GNATCOV_DUMP_CHANNEL)),> coverage-package-filters-unnamed-first.out)

clean:
	$(RM) -rf obj exe support/obj support/lib *.cgpr test.out \
		  xunit-*.xml coverage-*.out

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
