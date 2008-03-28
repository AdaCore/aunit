RTS =
TARGET =
GPRCONFIG = gprconfig
GPRBUILD  = gprbuild
GNATCLEAN = gnatclean

# INSTALL	= @prefix@
INSTALL = $(shell which $(GPRBUILD) | sed -e 's/\/bin\/.*//')

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

# Install directories

I_INC   = $(INSTALL)/include/aunit
I_LIB   = $(INSTALL)/lib/aunit
I_GPR   = $(INSTALL)/lib/gnat
I_TPL   = $(INSTALL)/share/examples/aunit
I_DOC   = $(INSTALL)/share/doc/aunit
I_PLG   = $(INSTALL)/share/gps/plug-ins

all:
	@$(MKDIR) aunit/obj
	@$(MKDIR) aunit/lib
	$(GPRCONFIG) $(TARGET_CONF) --config=Ada$(RTS_CONF) --config=C --batch -o gprconf.cgpr
	$(GPRBUILD) --config=gprconf.cgpr -Paunit/aunit_build $(GPRBUILD_FLAGS)

clean:
	-$(GNATCLEAN) -f -r -Paunit/aunit_build $(GPR_FLAGS)
	$(RM) -f $(I_GPR)/aunit.gpr
	-$(RMDIR) aunit/obj
	-${MAKE} -C docs clean

install_clean:
	$(RM) -fr $(I_DOC)
	$(RM) -fr $(I_TPL)
	$(RM) -f $(I_PLG)/aunit.xml
	$(RM) -fr $(I_LIB)
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_GPR)/aunit.gpr

install: install_clean all
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_PLG)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_INC)
	-$(CP) docs/*.html docs/*.info docs/*.pdf docs/*.txt $(I_DOC)
	-$(CP) template/*.ad[bs] template/*.gpr $(I_TPL)
	-$(CP) support/aunit.xml $(I_PLG)
	$(CP) support/aunit.gpr $(I_GPR)
	-$(CP) aunit/lib/* $(I_LIB)
	gnat list -s -d -Paunit/aunit_build $(GPR_FLAGS) | sort | uniq | sed -e 's/^ *//' -e 's/\\/\\\\/g' | while read f; do \
	  if [ "$$f" != "" ]; then \
	    echo "installing $$f into $(I_INC)"; \
	    $(CP) "$$f" "$(I_INC)"; \
	  fi; \
	done
	$(CHMOD) 444 $(I_DOC)/*
	$(CHMOD) 444 $(I_TPL)/*
	$(CHMOD) 444 $(I_PLG)/aunit.xml
	$(CHMOD) 444 $(I_GPR)/aunit.gpr
	$(CHMOD) 444 $(I_LIB)/*
	$(CHMOD) 444 $(I_INC)/*
	@echo $(SRC_LIST)
	@echo '------------------------------------------------------------------'
	@echo '--  AUnit has now been installed.'
	@echo '--  To be able to use the library, you may need to update your'
	@echo '--  ADA_PROJECT_PATH or GPR_PROJECT_PATH to point to the path'
	@echo '--  $(I_GPR)'
	@echo '------------------------------------------------------------------'

doc:
	${MAKE} -C docs

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp
CHMOD	= chmod
