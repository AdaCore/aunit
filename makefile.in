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
	@$(MKDIR) $(I_GPR)
	@# The following mkdirs are a workaround for missing gnatmake -p
	@# switch in versions prior to GNAT Pro 6.0.0
	@$(MKDIR) aunit/obj
	@$(MKDIR) $(INSTALL)/lib/aunit
	@$(MKDIR) $(INSTALL)/include/aunit
	$(GNATMAKE) $(ADA_FLAGS) -Paunit/aunit_build $(GPR_FLAGS)
	$(CP) support/aunit.gpr $(I_GPR)
	@echo '------------------------------------------------------------------'
	@echo '--  AUnit has now been installed.'
	@echo '--  To be able to use the library, you may need to update your'
	@echo '--  ADA_PROJECT_PATH or GPR_PROJECT_PATH to point to the path'
	@echo '--  $(I_GPR)'
	@echo '------------------------------------------------------------------'

clean:
	-$(GNATCLEAN) -f -r -Paunit/aunit_build $(GPR_FLAGS)
	$(RM) -f $(I_GPR)/aunit.gpr
	-$(RMDIR) aunit/obj
	-${MAKE} -C docs clean

install_clean: clean
	$(RM) -fr $(I_DOC)
	$(RM) -fr $(I_TPL)
	$(RM) -f $(I_PLG)/aunit.xml

install: install_clean all
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_PLG)
	-$(CP) docs/*.html docs/*.info docs/*.pdf docs/*.txt $(I_DOC)
	-$(CP) template/*.ad[bs] template/*.gpr $(I_TPL)
	-$(CP) support/aunit.xml $(I_PLG)

doc:
	${MAKE} -C docs

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
