AC_PREREQ(2.57)
AC_INIT(aunit, , report@adacore.com)
AC_CONFIG_SRCDIR(aunit/framework/aunit_framework.ads)
AC_PREFIX_DEFAULT(/usr/gnat)

##########################################
# Initialization.
##########################################

AC_CANONICAL_SYSTEM

##########################################
# Check platform properties
##########################################

case "$host_os" in
  lynxos*) MKDIR_P="mkdir -p -f" ;;
  *)       MKDIR_P="mkdir -p" ;;
esac
AC_SUBST(MKDIR_P)

##########################################
# Output generated files
##########################################

AC_OUTPUT([makefile], [], [])
