RTS =
TARGET =
GPRCONFIG = gprconfig
GPRBUILD  = gprbuild
GPRCLEAN = gprclean

# INSTALL	= @prefix@
INSTALL = $(shell which $(GPRBUILD) 2> /dev/null | sed -e 's/\/bin\/gprbuild.*//')

ifeq ($(RTS),)
   RTS_CONF =
   RTS_ARG = -XRUNTIME=full
else
   RTS_CONF = ,,$(RTS)
   RTS_ARG = -XRUNTIME=$(RTS)
endif

ifeq ($(TARGET),)
   TARGET_CONF =
   TARGET_ARG = -XPLATFORM=native
else
   TARGET_CONF = --target=$(TARGET)
   TARGET_ARG = --target=$(TARGET) -XPLATFORM=$(TARGET)
endif

GPRBUILD_FLAGS = $(TARGET_ARG) $(RTS_ARG)

# Install directories

I_INC   = $(INSTALL)/include/aunit
I_LIB   = $(INSTALL)/lib/aunit
I_GPR   = $(INSTALL)/lib/gnat
I_TPL   = $(INSTALL)/share/examples/aunit
I_DOC   = $(INSTALL)/share/doc/aunit
I_PLG   = $(INSTALL)/share/gps/plug-ins

.PHONY: all clean install_clean install

all:
	$(GPRCONFIG) $(TARGET_CONF) --config=Ada$(RTS_CONF) --config=C --batch
	$(GPRBUILD) -Paunit/aunit_build -p -f $(GPRBUILD_FLAGS)

clean:
	$(RM) -fr aunit/obj
	$(RM) -fr aunit/lib
	-${MAKE} -C docs clean

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
endif

install: all install_clean
ifneq ($(INSTALL),)
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_PLG)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_INC)
	-$(CP) docs/*.html docs/*.info docs/*.pdf docs/*.txt $(I_DOC)
	-$(CP) support/aunit.xml $(I_PLG)
	$(CP) support/aunit.gpr $(I_GPR)
	$(CP) -r examples/* $(I_TPL)
	-$(CP) -r aunit/lib/* $(I_LIB)
	$(CP) -r aunit/framework $(I_INC)
	$(CP) -r aunit/containers $(I_INC)
	$(CP) -r aunit/reporters $(I_INC)
	-$(CHMOD) 444 $(I_DOC)/*
	$(CHMOD) 444 $(I_PLG)/aunit.xml
	$(CHMOD) 444 $(I_GPR)/aunit.gpr
	find $(I_TPL) -type f -exec $(CHMOD) 444 {} \;
	find $(I_LIB) -type f -exec $(CHMOD) 444 {} \;
	find $(I_INC) -type f -exec $(CHMOD) 444 {} \;
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
