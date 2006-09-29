INSTALL	:= /usr/gnat

# Runtimes used to install
# can take the values : full zfp cert kernel
RUNTIME := full
GPRMAKE := gprmake
GNATMAKE := gnatmake
GNATCLEAN := gnatclean

# Install directories

SUFFIX := $(shell \
  if [ "$(RUNTIME)" == "full" ]; then \
    echo ""; \
  else \
    echo -$(RUNTIME); \
  fi)

I_INC   := $(INSTALL)/include/aunit$(SUFFIX)
I_LIB   := $(INSTALL)/lib/aunit$(SUFFIX)
I_GPR   := $(INSTALL)/lib/gnat
I_TPL   := $(INSTALL)/share/examples/aunit
I_DOC   := $(INSTALL)/share/doc/aunit
I_PLG   := $(INSTALL)/share/gps/plug-ins

INCLUDE_PATHS := $(shell \
  if [ "$(RUNTIME)" == "full" ]; then \
    echo aunit/framework \
         aunit/framework/native \
         aunit/text_reporter \
         aunit/containers; \
  elif [ "$(RUNTIME)" == "kernel" ]; then \
    echo aunit/framework \
         aunit/framework/native \
         aunit/text_reporter \
         aunit/containers; \
  elif [ "$(RUNTIME)" == "zfp" ]; then \
    echo aunit/framework \
         aunit/framework/cert \
         aunit/framework/zfp \
         aunit/text_reporter \
         aunit/containers; \
  elif [ "$(RUNTIME)" == "cert" ]; then \
    echo aunit/framework \
         aunit/framework/zfp \
         aunit/framework/cert \
         aunit/text_reporter \
         aunit/containers; \
  fi)

ZFP_SUPPORT_GPR := $(shell \
  if [ "$(RUNTIME)" == "zfp" ]; then \
    echo 'with \"zfp_support\";'; \
  else \
    echo ""; \
  fi)

all:
	$(MKDIR) aunit/obj$(SUFFIX)
	$(MKDIR) aunit/lib$(SUFFIX)
	sed "s/<zfp_support>/$(ZFP_SUPPORT_GPR)/" aunit.gpr.in > aunit.gpr
	$(GNATMAKE) -Paunit -XRuntime=$(RUNTIME) -XMode="build"

clean:
	-$(GNATCLEAN) -Paunit -XRuntime=$(RUNTIME) -XMode="build"
	-$(GNATCLEAN) -Paunit_tests -XRuntime=$(RUNTIME) -XMode="build"
	-$(RMDIR) aunit/obj$(SUFFIX)
	-$(RMDIR) aunit/lib$(SUFFIX)
	-$(RMDIR) obj
	-$(RM) aunit.gpr aunit_tests.gpr
	-${MAKE} -C docs clean

install_clean:
	$(RM) -fr $(I_INC)
	$(RM) -fr $(I_LIB)
	$(RM) -fr $(INSTALL)/share/examples/aunit
	$(RM) -fr $(I_DOC)
	$(RM) -f $(I_GPR)/aunit.gpr

install_dirs: install_clean
	$(MKDIR) $(I_INC)
	$(MKDIR) $(I_LIB)
	$(MKDIR) $(I_DOC)
	$(MKDIR) $(I_GPR)
	$(MKDIR) $(I_TPL)
	$(MKDIR) $(I_PLG)

install: all install_dirs
	@for dir in $(INCLUDE_PATHS); do \
	  echo $(CP) $$dir/\*.ad\* $(I_INC); \
	  $(CP) $$dir/*.ad* $(I_INC); \
	done
	$(CP) aunit/lib$(SUFFIX)/* $(I_LIB)
	-$(CP) template/*.ad* template/*.gpr $(I_TPL)
	-$(CP) docs/*.html docs/*.info docs/*.pdf docs/*.txt $(I_DOC)
	sed 's/<RUNTIME>/'$(RUNTIME)'/' support/aunit.gpr > $(I_GPR)/aunit.gpr
	-$(CP) support/aunit.xml $(I_PLG)

doc:
	${MAKE} -C docs

force:

test: force
	-$(MKDIR) obj
	sed "s/<zfp_support>/$(ZFP_SUPPORT_GPR)/" \
          aunit_tests.gpr.in > aunit_tests.gpr
	$(GNATMAKE) -Paunit_tests -XRuntime=$(RUNTIME)
	./aunit_harness

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
