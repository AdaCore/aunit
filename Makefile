RTS =
TARGET =
GPRBUILD  = gprbuild
GPRCLEAN = gprclean

INSTALL = $(shell which $(GPRBUILD) 2> /dev/null | sed -e 's/\/bin\/gprbuild.*//')

ifeq ($(RTS),)
   RTS=full
   RTS_CONF =
else
   RTS_CONF = --RTS=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET=native
   TARGET_CONF =
else
   TARGET_CONF = --target=$(TARGET)
endif

CONF_ARGS = $(TARGET_CONF) $(RTS_CONF)

# Install directories

I_INC   = $(INSTALL)/include/aunit
I_LIB   = $(INSTALL)/lib/aunit
I_GPR   = $(INSTALL)/lib/gnat
I_TPL   = $(INSTALL)/share/examples/aunit
I_DOC   = $(INSTALL)/share/doc/aunit
I_PLG   = $(INSTALL)/share/gps/plug-ins

.PHONY: all clean targets installed-targets install_clean install

all: support/aunit_shared.gpr
	$(GPRBUILD) -Paunit/aunit_build -p -XRUNTIME=$(RTS) -XPLATFORM=$(TARGET) $(CONF_ARGS)

installed-targets:
	@printf "$(TARGET)\n" >> installed-targets
	@printf "native\n" >> installed-targets

targets: installed-targets
# make sure that "native" is in the targets list, as it is the default in
# the project template
	@$(RM) -f targets
	for f in $(shell cat installed-targets | sort -u); \
	  do if [ -f targets ]; then \
	       printf ", \"$$f\"" >> targets; \
	     else  \
	       printf "\"$$f\"" >> targets; \
	     fi; \
	done

support/aunit_shared.gpr: support/aunit_shared.gpr.in targets
	cat $< | sed -e 's/@TARGETS@/$(shell cat targets)/' > $@

clean:
	$(RM) -fr aunit/obj
	$(RM) -fr aunit/lib
	-${MAKE} -C docs clean
	$(RM) -f installed-targets
	$(RM) -f targets
	$(RM) -f support/aunit_shared.gpr
	$(RM) -f *.cgpr

install_clean:
ifeq ($(INSTALL),)
	@echo 'Error when installing: $$INSTALL is empty...'
	@echo "Please set an installation path before installing !"
else
	-$(CHMOD) -R 777 $(I_DOC)
	$(RM) -fr $(I_DOC)
	-$(CHMOD) -R 777 $(I_TPL)
	$(RM) -fr $(I_TPL)
	$(RM) -f $(I_PLG)/aunit.xml
	-$(CHMOD) -R 777 $(I_LIB)
	$(RM) -fr $(I_LIB)
	-$(CHMOD) -R 777 $(I_INC)
	$(RM) -fr $(I_INC)
	$(RM) -f $(I_GPR)/aunit.gpr
	$(RM) -f $(I_GPR)/aunit_shared.gpr
endif

install: install_clean support/aunit_shared.gpr
ifneq ($(INSTALL),)
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_PLG)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_INC)
	-$(CP) docs/*.html docs/*.info docs/*.pdf docs/*.txt $(I_DOC)
	-$(CP) support/aunit.xml $(I_PLG)
	$(CP) support/*.gpr $(I_GPR)
	$(CP) -r examples/* $(I_TPL)
	-$(CP) -r aunit/lib/* $(I_LIB)
	$(CP) -r aunit/framework $(I_INC)
	$(CP) -r aunit/containers $(I_INC)
	$(CP) -r aunit/reporters $(I_INC)
	@echo $(SRC_LIST)
	@echo '------------------------------------------------------------------'
	@echo '--  AUnit has now been installed.'
	@echo '--  To be able to use the library, you may need to update your'
	@echo '--  ADA_PROJECT_PATH or GPR_PROJECT_PATH to point to the path'
	@echo '--  $(I_GPR)'
	@echo '------------------------------------------------------------------'
endif

doc:
	${MAKE} -C docs

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp
CHMOD	= chmod
