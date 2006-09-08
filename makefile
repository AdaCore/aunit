
# $Id$

INSTALL	= /opt

# Install directories

I_INC   = $(INSTALL)/include/aunit
I_LIB   = $(INSTALL)/lib/aunit
I_GPR   = $(INSTALL)/lib/gnat
I_TPL   = $(INSTALL)/share/examples/aunit
I_DOC   = $(INSTALL)/share/doc/aunit
I_PLG   = $(INSTALL)/share/gps/plug-ins

all:
	$(MKDIR) aunit/obj
	$(MKDIR) aunit/lib
	gnatmake -Paunit

clean:
	-gnatclean -Paunit
	-gnatclean -Paunit_tests
	-$(RMDIR) aunit/obj
	-$(RMDIR) aunit/lib
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

install: install_dirs
	$(CP) aunit/framework/*.ad* aunit/text_reporter/*.ad* $(I_INC)
	$(CP) aunit/lib/* $(I_LIB)
	$(CP) template/*.ad* template/*.gpr $(I_TPL)
	-$(CP) docs/*.html docs/*.info docs/*.pdf docs/*.txt $(I_DOC)
	$(CP) support/aunit.gpr $(I_GPR)
	$(CP) support/aunit.xml $(I_PLG)

doc:
	${MAKE} -C docs

force:

test: force
	gnatmake -Paunit_tests
	./aunit_harness

RMDIR	= rmdir
MKDIR	= mkdir -p
RM	= rm
CP	= cp -p
