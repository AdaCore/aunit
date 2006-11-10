@ECHO OFF

REM === reset of the used variables
SET GNATROOT=
SET SUPPORT_EXCEPTION=
SET SUPPORT_CALENDAR=
SET BUILD_AUNIT=
SET INSTALL=
SET RUNTIME=
SET TOOL_PREFIX=

REM === quick zfp configuration
IF '%1%' == 'zfp' (
  SET SUPPORT_EXCEPTION=no
  SET SUPPORT_CALENDAR=no
  SET RUNTIME=zfp
  GOTO BUILD_SETUP
)

REM === user parameters: customize your installation here

ECHO.
ECHO *** AUNIT SUPPORT: EXCEPTIONS ***
ECHO.
:SELECT_EXCEPTION
ECHO Disable support for exceptions ?
ECHO  ('no' (default) or 'yes')
SET DISABLE_EXCEPTION=
SET /P DISABLE_EXCEPTION=
IF '%DISABLE_EXCEPTION%' == '' SET SUPPORT_EXCEPTION=yes
IF '%DISABLE_EXCEPTION%' == 'yes' SET SUPPORT_EXCEPTION=no
IF '%DISABLE_EXCEPTION%' == 'no' SET SUPPORT_EXCEPTION=yes
IF '%SUPPORT_EXCEPTION%' == '' (
  ECHO.
  ECHO * please type yes or no, or just press enter for default *
  ECHO.
  GOTO SELECT_EXCEPTION
)

ECHO.
ECHO *** AUNIT SUPPORT: ADA.CALENDAR ***
ECHO.
:SELECT_CALENDAR
ECHO Disable support for Ada.Calendar ?
ECHO  ('no' (default) or 'yes')
SET /P DISABLE_CALENDAR=
SET DISABLE_CALENDAR=
IF '%DISABLE_CALENDAR%' == '' SET SUPPORT_CALENDAR=yes
IF '%DISABLE_CALENDAR%' == 'yes' SET SUPPORT_CALENDAR=no
IF '%DISABLE_CALENDAR%' == 'no' SET SUPPORT_CALENDAR=yes
IF '%SUPPORT_CALENDAR%' == '' (
  ECHO.
  ECHO * please type yes or no, or just press enter for default *
  ECHO.
  GOTO SELECT_CALENDAR
)

:BUILD_SETUP
ECHO.
ECHO *** AUNIT COMPILATION ***
ECHO.
ECHO Do you want to build AUnit now ?
ECHO  ('yes' (default) or 'no')
SET /P BUILD_AUNIT=
if '%BUILD_AUNIT%' == '' SET BUILD_AUNIT=yes

REM Retrieving the GNAT compiler from registry

FOR /F "skip=7 tokens=3" %%A in ('REG QUERY "HKLM\Software\Ada Core Technologies\GNAT" /S') DO (
  SET GNATROOT=%%A
)

IF NOT %BUILD_AUNIT% == yes GOTO SKIP_COMPILER

ECHO.
ECHO *** COMPILER ***
ECHO.
ECHO Do you want to use the compiler found in %GNATROOT% ?
ECHO  (press enter or specify a compiler directory):
SET /P NEWGNATROOT=
IF NOT '%NEWGNATROOT%' == '' (
  IF '%NEWGNATROOT~-1%' == '\' (
    SET GNATROOT=%NEWGNATROOT~0,-1%
  ) ELSE (
    SET GNATROOT=%NEWGNATROOT%
  )
)

:SKIP_COMPILER
ECHO.
ECHO *** INSTALLATION DIRECTORY ***
ECHO.
ECHO Please enter AUnit base installation directory
ECHO  (just press enter for installing in '%GNATROOT%'):
SET /P INSTALL=
IF '%INSTALL%' == '' SET INSTALL=%GNATROOT%

IF NOT %BUILD_AUNIT% == yes GOTO SETUP

if NOT '%RUNTIME%' == '' GOTO SKIP_RUNTIME
ECHO.
ECHO *** RUN-TIME CONFIGURATION ***
ECHO.
ECHO Please enter the run-time you want to use when compiling AUnit:
ECHO  (just press enter for default run-time):'
SET /P RUNTIME=
:SKIP_RUNTIME

ECHO.
ECHO *** CROSS-COMPILATION ***
ECHO.
ECHO If you use a gnat cross-compiler, enter the tools prefix.
ECHO  (Press enter to use the native compiler)
SET /P TOOL_PREFIX=

REM === end of user parameters. Do not change below this line

:SETUP_VARS
IF x%RUNTIME% EQU x (
  SET ADA_FLAGS=
) ELSE (
  SET ADA_FLAGS=--RTS=%RUNTIME%
)

IF x%TOOL_PREFIX% EQU x (
  SET GNATMAKE=gnatmake
  SET GNATCLEAN=gnatclean
) ELSE (
  SET GNATMAKE=%TOOL_PREFIX%-gnatmake
  SET GNATCLEAN=%TOOL_PREFIX%-gnatclean
)

:SETUP
SET I_INC=%INSTALL%\include\aunit
SET I_LIB=%INSTALL%\lib\aunit
SET I_GPR=%INSTALL%\lib\gnat
SET I_TPL=%INSTALL%\share\examples\aunit
SET I_DOC=%INSTALL%\share\doc\aunit
SET I_PLG=%INSTALL%\share\gps\plug-ins

ECHO.
ECHO ******************************
ECHO *** SETTING UP THE SOURCES ***
ECHO ******************************
ECHO.

MKDIR aunit\include 2> NUL
MKDIR aunit\obj 2> NUL
MKDIR aunit\lib 2> NUL

COPY aunit\framework\*.ad? aunit\include > NUL
COPY aunit\text_reporter\*.ad? aunit\include > NUL
COPY aunit\containers\*.ad? aunit\include > NUL

IF NOT %SUPPORT_EXCEPTION% EQU yes (
  ECHO * no support for exceptions *
  COPY aunit\framework\noexception\*.ad* aunit\include > NUL
) ELSE (
  ECHO AUnit set-up with exception support
)
IF NOT %SUPPORT_CALENDAR% EQU yes (
  ECHO * no support for Ada.Calendar *
  COPY aunit\framework\nocalendar\*.ad* aunit\include > NUL
) ELSE (
  ECHO AUnit set-up with Ada.Calendar support
)

IF NOT %BUILD_AUNIT% == yes GOTO INSTALL
ECHO.
ECHO **********************************
ECHO *** BUILDING THE AUNIT LIBRARY ***
ECHO **********************************
ECHO.

ECHO %GNATROOT%\bin\%GNATMAKE% %ADA_FLAGS% -Paunit
%GNATROOT%\bin\%GNATMAKE% %ADA_FLAGS% -Paunit

:INSTALL
ECHO.
ECHO ************************
ECHO *** INSTALLING AUNIT ***
ECHO ************************
ECHO.
MKDIR "%I_INC%" 2> NUL
MKDIR "%I_LIB%" 2> NUL
MKDIR "%I_DOC%" 2> NUL
MKDIR "%I_GPR%" 2> NUL
MKDIR "%I_TPL%" 2> NUL
MKDIR "%I_PLG%" 2> NUL

ECHO copying sources in %I_INC%
COPY aunit\include\* "%I_INC%" > NUL 2> NUL
ECHO copying library in %I_LIB%
COPY aunit\lib\* "%I_LIB%" > NUL 2> NUL
ECHO copying aunit.gpr in %I_GPR%
COPY support\aunit.gpr "%I_GPR%" > NUL 2> NUL
ECHO copying examples in %I_TPL%
COPY template\*.ad* template\*.gpr "%I_TPL%" > NUL 2> NUL
ECHO copying documentation in %I_DOC%
COPY docs\*.html docs\*.info docs\*.pdf docs\*.txt "%I_DOC%" > NUL 2> NUL
ECHO copying GPS plug-in in %I_PLG%
COPY support\aunit.xml "%I_PLG%" > NUL 2> NUL

:END
