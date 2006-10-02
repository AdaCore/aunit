INSTALL	= /usr/gnat

GNATMAKE  = gnatmake
GNATCLEAN = gnatclean

# tell wether the runtime supports the exceptions
SUPPORT_EXCEPTION = yes
# tell wether the runtime supports Ada.Calendar
SUPPORT_CALENDAR = yes

# Install directories

I_INC   = $(INSTALL)/include/aunit
I_LIB   = $(INSTALL)/lib/aunit
I_GPR   = $(INSTALL)/lib/gnat
I_TPL   = $(INSTALL)/share/examples/aunit
I_DOC   = $(INSTALL)/share/doc/aunit
I_PLG   = $(INSTALL)/share/gps/plug-ins

all: setup
	$(MKDIR) aunit/obj
	$(MKDIR) aunit/lib
	$(GNATMAKE) -Paunit

setup:
	$(MKDIR) aunit/include
	$(CP) aunit/framework/*.ad[bs] aunit/include/
	$(CP) aunit/text_reporter/*.ad[bs] aunit/include/
	$(CP) aunit/containers/*.ad[bs] aunit/include/
	@if [ "$(SUPPORT_EXCEPTION)" != "yes" ]; then \
	  echo "* no support for exception *"; \
	  $(CP) aunit/framework/noexception/*.ad[bs] aunit/include/; \
	fi
	@if [ "$(SUPPORT_CALENDAR)" != "yes" ]; then \
	  echo "* no support for calendar *"; \
	  $(CP) aunit/framework/nocalendar/*.ad[bs] aunit/include/; \
	fi

clean:
	-$(GNATCLEAN) -Paunit
	-$(GNATCLEAN) -Paunit_tests
	-$(RM) aunit/include/*
	-$(RMDIR) aunit/include
	-$(RMDIR) aunit/obj
	-$(RMDIR) aunit/lib
	-$(RMDIR) obj
	-${MAKE} -C docs clean
	-$(RM) aunit_zfp.gpr

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

install: install_dirs
	$(CP) aunit/include/* $(I_INC)
	$(CP) aunit/lib/* $(I_LIB)
	$(CP) support/aunit.gpr $(I_GPR)
	-$(CP) template/*.ad[bs] template/*.gpr $(I_TPL)
	-$(CP) docs/*.html docs/*.info docs/*.pdf docs/*.txt $(I_DOC)
	-$(CP) support/aunit.xml $(I_PLG)

doc:
	${MAKE} -C docs

test: aunit_tests.gpr
	-$(MKDIR) obj
	$(GNATMAKE) -Paunit_tests
	./aunit_harness

zfp:
	$(MKDIR) aunit/obj
	$(MKDIR) aunit/lib
	echo "with \"zfp_support\"; \
" > aunit_zfp.gpr
	cat aunit.gpr | sed -e 's/AUnit/AUnit_zfp/' >> aunit_zfp.gpr
	${MAKE} SUPPORT_EXCEPTION=no SUPPORT_CALENDAR=no build_zfp

build_zfp: setup
	$(GNATMAKE) -Paunit_zfp

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
