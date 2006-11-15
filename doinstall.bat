@ECHO OFF

REM === Test defered variable evaluation
SET I=
SET I=!I!
IF NOT '%I%' == '' (
  CMD /V/C .\DOINSTALL.BAT
  GOTO END
)

REM === reset of the used variables
SET GNATROOT=
SET GNATMAKE=
SET SUPPORT_EXCEPTION=
SET SUPPORT_CALENDAR=
SET INSTALL=
SET RUNTIME=
SET TOOL_PREFIX=

REM ===== CONFIGURATION PHASE ===========

REM Retrieving the GNAT compiler from registry
SET GNATROOT=
ECHO.>GNATROOT.TXT
FOR /F "skip=7 tokens=1,3" %%A IN ('REG QUERY "HKLM\Software\Ada Core Technologies\GNAT" /S') DO (
  if %%A == ROOT (
    ECHO %%B >> GNATROOT.TXT
    SET GNATROOT=%%B
  )
)

ECHO.
ECHO *** COMPILER ***
ECHO.
:SELECT_COMPILER

SET CHOICE=
IF '!GNATROOT!' == '' (
  ECHO Please specify the full path to gnatmake:
  SET /P CHOICE=
  IF '!CHOICE!' == '' (
    ECHO *** Error ! Please enter a valid path for gnatmake
    GOTO SELECT_COMPILER
  )

  GOTO ANALYSE_PATH
) ELSE (
  ECHO Please specify the compiler you want to use
  ECHO ** Select one from the list, or directly enter gnatmake's full path:
  SET /A I = O
  FOR /F %%A IN ('TYPE GNATROOT.TXT') DO (
    FOR /F %%B IN ('DIR /B %%A\bin\*gnatmake.exe') DO (
      SET /A I=!I!+1
      ECHO !I!- %%B found in %%A
    )
  )
  SET /P CHOICE=
  IF '!CHOICE!' == '' (
    ECHO *** Error ! Please select a compiler from the list, or enter a valid path
    GOTO SELECT_COMPILER
  )
)
SET GNATMAKE=
SET GNATROOT=

IF '!CHOICE!' LEQ '!I!' (
  SET /A I=0
  FOR /F %%A IN ('TYPE GNATROOT.TXT') DO (
    FOR /F %%B IN ('DIR /B %%A\bin\*gnatmake.exe') DO (
      SET /A I=!I!+1
      IF !I! == !CHOICE! (
        SET GNATMAKE=%%A\bin\%%B
        SET GNATROOT=%%A
        GOTO COMPILER_END
      )
    )
  )
)

:ANALYSE_PATH

DIR /B !CHOICE! > NUL
IF %ERRORLEVEL% NEQ 0 (
  ECHO *** Error ! Path !CHOICE! does not exist
  GOTO SELECT_COMPILER
)
SET GNATMAKE=!CHOICE!

SET OLD_PWD=!CD!
CD !CHOICE!\..\..
IF %ERRORLEVEL% == 0 (
  SET GNATROOT=!CD!
)
CD !OLD_PWD!
:COMPILER_END

REM We do not need GNATROOT.TXT anymore.
DEL GNATROOT.TXT


ECHO Using !GNATMAKE! to compile AUnit


ECHO.
ECHO *** INSTALLATION DIRECTORY ***
ECHO.
ECHO Please enter AUnit base installation directory
ECHO ** Press enter for installing in '!GNATROOT!':
SET /P INSTALL=
IF '%INSTALL%' == '' SET INSTALL=%GNATROOT%

if NOT '%RUNTIME%' == '' GOTO SKIP_RUNTIME
ECHO.
ECHO *** RUN-TIME CONFIGURATION ***
ECHO.
ECHO Please enter the run-time you want to use when compiling AUnit:
ECHO ** Press enter for default run-time:'
SET /P RUNTIME=
:SKIP_RUNTIME


ECHO.
ECHO *** AUNIT SUPPORT: EXCEPTIONS ***
ECHO.
:SELECT_EXCEPTION
ECHO AUnit support for exceptions
ECHO ** Choose 1 or 2, or press enter for default
ECHO 1- Enable (default)
ECHO 2- Disable
SET CHOICE=
SET /P CHOICE=
SET SUPPORT_EXCEPTION=
IF '!CHOICE!' == '' SET SUPPORT_EXCEPTION=yes
IF '!CHOICE!' == '1' SET SUPPORT_EXCEPTION=yes
IF '!CHOICE!' == '2' SET SUPPORT_EXCEPTION=no
IF '!SUPPORT_EXCEPTION!' == '' (
  ECHO.
  ECHO * please type 1 or 2, or just press enter for default *
  ECHO.
  GOTO SELECT_EXCEPTION
)

ECHO.
ECHO *** AUNIT SUPPORT: ADA.CALENDAR ***
ECHO.
:SELECT_CALENDAR
ECHO AUnit support for Ada.Calendar
ECHO ** Choose 1 or 2, or press enter for default
ECHO 1- Enable (default)
ECHO 2- Disable
SET CHOICE=
SET /P CHOICE=
SET SUPPORT_CALENDAR=
IF '!CHOICE!' == '' SET SUPPORT_CALENDAR=yes
IF '!CHOICE!' == '1' SET SUPPORT_CALENDAR=yes
IF '!CHOICE!' == '2' SET SUPPORT_CALENDAR=no
IF '%SUPPORT_CALENDAR%' == '' (
  ECHO.
  ECHO * please type 1 or 2, or just press enter for default *
  ECHO.
  GOTO SELECT_CALENDAR
)

REM === end of user parameters. Do not change below this line

:SETUP_VARS
IF x%RUNTIME% EQU x (
  SET ADA_FLAGS=
) ELSE (
  SET ADA_FLAGS=--RTS=%RUNTIME%
)

:SETUP
SET I_GPR=%INSTALL%\lib\gnat
SET I_TPL=%INSTALL%\share\examples\aunit
SET I_DOC=%INSTALL%\share\doc\aunit
SET I_PLG=%INSTALL%\share\gps\plug-ins
SET I_INC=%INSTALL%\include\aunit
SET I_LIB=%INSTALL%\lib\aunit

ECHO.
ECHO **********************************
ECHO *** BUILDING THE AUNIT LIBRARY ***
ECHO **********************************
ECHO.

IF NOT %SUPPORT_EXCEPTION% EQU yes (
  ECHO * no support for exceptions *
  SET GPR_FLAGS_EXCEPTION=-XSUPPORT_EXCEPTION=no
) ELSE (
  ECHO AUnit set-up with exceptions support
  SET GPR_FLAGS_EXCEPTION=-XSUPPORT_EXCEPTION=yes
)
IF NOT %SUPPORT_CALENDAR% EQU yes (
  ECHO * no support for Ada.Calendar *
  SET GPR_FLAGS_CALENDAR=-XSUPPORT_CALENDAR=no
) ELSE (
  ECHO AUnit set-up with Ada.Calendar support
  SET GPR_FLAGS_CALENDAR=-XSUPPORT_CALENDAR=yes
)
SET GPR_FLAGS=!GPR_FLAGS_EXCEPTION! !GPR_FLAGS_CALENDAR!

REM The following mkdirs are a workaround for missing gnatmake -p
REM switch in versions prior to GNAT Pro 6.0.0
MKDIR aunit\lib 2> NUL
MKDIR aunit\obj 2> NUL

ECHO %GNATMAKE% %ADA_FLAGS% -Paunit/aunit_build %GPR_FLAGS%
%GNATMAKE% %ADA_FLAGS% -Paunit/aunit_build %GPR_FLAGS%

:INSTALL
ECHO.
ECHO ************************
ECHO *** INSTALLING AUNIT ***
ECHO ************************
ECHO.
MKDIR "%I_DOC%" 2> NUL
MKDIR "%I_GPR%" 2> NUL
MKDIR "%I_TPL%" 2> NUL
MKDIR "%I_PLG%" 2> NUL
MKDIR "%I_LIB%" 2> NUL
MKDIR "%I_INC%" 2> NUL

ECHO.
ECHO uninstalling previous AUnit library in %INSTALL% if needed
DEL /Q "%I_GPR%\aunit.gpr"
DEL /Q "%I_TPL%\*.ad*"
DEL /Q "%I_DOC%\*.*"
DEL /Q/F "%I_INC%\*.*"
DEL /Q/F "%I_LIB%\*.*"

ECHO.
ECHO copying examples in %I_TPL%
COPY template\*.* "%I_TPL%" > NUL
ECHO.
ECHO copying documentation in %I_DOC%
COPY docs\*.html "%I_DOC%" > NUL 2> NUL
COPY docs\*.pdf "%I_DOC%" > NUL 2> NUL
COPY docs\*.txt "%I_DOC%" > NUL 2> NUL
ECHO.
ECHO copying GPS plug-in in %I_PLG%
COPY support\aunit.xml "%I_PLG%" > NUL

ECHO.
ECHO copying aunit.gpr in %I_GPR%
COPY support\aunit.gpr "%I_GPR%" > NUL

ECHO.
ECHO copying AUnit source files in %I_INC%
GNAT LIST -s -d -Paunit/aunit_build %GPR_FLAGS% | sort > files.txt
SET PREV=
FOR /F %%A IN ('TYPE files.txt') DO (
  IF !PREV! NEQ %%A (
    COPY "%%A" "%I_INC%" > NUL
  )
  SET PREV=%%A
)
DEL files.txt

ECHO.
ECHO copying AUnit lib files in %I_LIB%
COPY aunit\lib\*.* "%I_LIB%" > NUL

:END
