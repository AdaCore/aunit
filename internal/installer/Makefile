include ../../version_information
VERSION=${MAJOR_VERSION}${MINOR_VERSION}
ifneq (${MINOR_VERSION},)
  PVERSION=$(shell echo ${VERSION} | sed -e 's/\([^a-zA-Z]*\).*/\1.0/')
else
  PVERSION=$(shell echo ${VERSION} | sed -e 's/\([^a-zA-Z]*\).*/\1.0.0.0/')
endif

all:
	cat nsis\\aunit.nsi | \
          sed -e 's/^!define VERSION .*/!define VERSION "${VERSION}"/' \
              -e 's/^!define PVERSION .*/!define PVERSION "${PVERSION}"/' > \
          nsis\\aunit-version.nsi
	gnatmake -p -Psetup_utility/setup_utility
	makensis /V2 nsis\\aunit-version.nsi
	mv nsis/*.exe .

clean:
	rm -f *.exe nsis/aunit-version.nsi
	gnatclean -Psetup_utility/setup_utility