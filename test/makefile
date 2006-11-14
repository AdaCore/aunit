RTS =
TOOL_PREFIX =

TEST_PROJECT = aunit_tests

ifeq ($(RTS),zfp)
   TEST_PROJECT = aunit_tests_zfp
endif

ifeq ($(TOOL_PREFIX),)
   GNATMAKE  = gnatmake
   GNATCLEAN = gnatclean
else
   GNATMAKE  = $(TOOL_PREFIX)-gnatmake
   GNATCLEAN = $(TOOL_PREFIX)-gnatclean
endif

ifeq ($(RTS),)
   ADA_FLAGS =
else
   ADA_FLAGS = --RTS=$(RTS)
endif

all:
	-$(MKDIR) obj
	-$(MKDIR) zfp_support/obj
	-$(MKDIR) zfp_support/lib
	$(GNATMAKE) $(ADA_FLAGS) -P$(TEST_PROJECT) $(GPR_FLAGS)
	./aunit_harness

clean:
	-$(GNATCLEAN) -f -r -P$(TEST_PROJECT) $(GPR_FLAGS)

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
