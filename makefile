INSTALL	= /usr/gnat
RTS =
TOOL_PREFIX =
# tell wether the runtime supports the exceptions
SUPPORT_EXCEPTION = yes
# tell wether the runtime supports Ada.Calendar
SUPPORT_CALENDAR = yes

ifeq ($(RTS),zfp)
   SUPPORT_EXCEPTION = no
   SUPPORT_CALENDAR = no
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

# Install directories

I_INC   = $(INSTALL)/include/aunit
I_LIB   = $(INSTALL)/lib/aunit
I_GPR   = $(INSTALL)/lib/gnat
I_TPL   = $(INSTALL)/share/examples/aunit
I_DOC   = $(INSTALL)/share/doc/aunit
I_PLG   = $(INSTALL)/share/gps/plug-ins

GPR_FLAGS_INSTALL = -XINSTALL_DIR=$(INSTALL)

ifneq ($(SUPPORT_EXCEPTION),yes)
   GPR_FLAGS_EXCEPTION = -XSUPPORT_EXCEPTION=no
else
   GPR_FLAGS_EXCEPTION = -XSUPPORT_EXCEPTION=yes
endif

ifneq ($(SUPPORT_CALENDAR),yes)
   GPR_FLAGS_CALENDAR = -XSUPPORT_CALENDAR=no
else
   GPR_FLAGS_CALENDAR := -XSUPPORT_CALENDAR=yes
endif

GPR_FLAGS = $(GPR_FLAGS_INSTALL) $(GPR_FLAGS_EXCEPTION) $(GPR_FLAGS_CALENDAR)

all:
	@$(MKDIR) aunit/obj
	@$(MKDIR) aunit/lib
	$(GNATMAKE) $(ADA_FLAGS) -Paunit/aunit_build $(GPR_FLAGS)

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
	$(CP) aunit/lib/* $(I_LIB)
	gnat list -s -d -Paunit/aunit_build $(GPR_FLAGS) | sort | uniq | sed -e 's/^ *//' -e 's/\\/\\\\/g' | while read f; do \
	  if [ '$$f' != '' ]; then \
	    echo $(CP) "$$f" "$(I_INC)"; \
	    $(CP) "$$f" "$(I_INC)"; \
	  fi; \
	done
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
CP	= install -m 644
