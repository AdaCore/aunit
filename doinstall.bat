@ECHO OFF

REM === Test defered variable evaluation
SET I=
SET I=!I!
IF NOT '%I%' == '' (
  CMD /V/C .\DOINSTALL.BAT
  GOTO END
)

SETLOCAL

REM === reset of the used variables
SET GPRBUILDROOT=
SET GNAT=
SET INSTALL=
SET RUNTIME=
SET TOOL_PREFIX=

REM ===== CONFIGURATION PHASE ===========

REM Retrieving the GNAT compiler from registry
SET GPRBUILDROOT=
REG QUERY "HKLM\Software\Ada Core Technologies\GPRBUILD" /v ROOT > REGS.TXT | GOTO MISSINGGPRBUILD
FOR /F "skip=2 tokens=1,3" %%A IN (REGS.TXT) DO (
  if %%A == ROOT (
    SET GPRBUILDROOT=%%B
  )
)
DEL REGS.TXT

CLS
ECHO.
ECHO.
ECHO.
ECHO **************************
ECHO *** PREPARING TO SETUP ***
ECHO **************************
ECHO.
:SELECT_GPRBUILD

SET CHOICE=
IF '!GPRBUILDROOT!' == '' (
  ECHO Please specify the full path to gprbuild.exe:
  SET /P CHOICE=
  IF '!CHOICE!' == '' (
    ECHO *** Error ! Please enter a valid path for gprbuild.exe
    GOTO SELECT_GPRBUILD
  )

)

:SELECT_TARGET
SET /A I = 0
SET TARGETS=
ECHO.>TARGETSCHOICE.TXT
for /F "skip=1" %%A IN ('%GPRBUILDROOT%\bin\gprconfig --show-targets') DO (
  ECHO %%A >> TARGETSCHOICE.TXT
  SET /A I=!I!+1
  SET LAST_TARGET=%%A
)

IF '!I!' LEQ '1' (
  ECHO %LAST_TARGET% > TARGETS.TXT
) ELSE (
  ECHO.>TARGETS.TXT
  SET ONESELECTED=
:SELECT_TARGETS
  SET /A J=O
  SET OPTIONS=
  CLS
  ECHO *** Select the targets you want AUnit to be compiled for.
  FOR /F %%A IN (TARGETSCHOICE.TXT) DO (
    SET FOUND=0
    FOR /F %%B IN (TARGETS.TXT) DO (
      IF '%%A' == '%%B' (
        SET FOUND=1
      )
    )
    SET /A J=!J!+1
    IF '!FOUND!' == '0' (
      SET OPTIONS=!OPTIONS!!J!
      SET NUM= ^(!J!^)
    ) ELSE (
      SET NUM=*^(!J!^)
    )
    ECHO !NUM! %%A
  )
  SET CHOICE=
  IF '!ONESELECTED!' == '' (
    SET /P CHOICE="Enter a target number: "
  ) ELSE (
    SET /P CHOICE="Enter a target number, or <enter> to continue the setup: "
  )
  IF '!CHOICE!' LEQ '' (
    IF '!ONESELECTED!' == '' (
      GOTO SELECT_TARGETS
    ) ELSE (
      GOTO END_SELECT_TARGETS
    )
  )
  SET /A J=0
  FOR /F %%A IN (TARGETSCHOICE.TXT) DO (
    SET /A J=!J!+1
    SET FOUND=0
    FOR /F %%B IN (TARGETS.TXT) DO (
      IF '%%A' == '%%B' (
        SET FOUND=1
      )
    )
    IF !FOUND! LEQ '0' (
      IF '!J!' == '!CHOICE!' (
        SET /A I=!I!-1
        ECHO %%A >> TARGETS.TXT
        SET ONESELECTED=1
      )
    )
  )
  IF '!I!' NEQ '0' (
    GOTO SELECT_TARGETS
  )
)
DEL TARGETSCHOICE.TXT

:END_SELECT_TARGETS

ECHO ECHO OFF>                   RUN.BAT

FOR /F %%A IN (TARGETS.TXT) DO (
  CALL :SELECT_RUNTIME %%A RUN.BAT
)
DEL TARGETS.TXT

ECHO GOTO END>>                  RUN.BAT
ECHO ^:ERROR>>                   RUN.BAT
ECHO ECHO *** BUILD FAILED ***>> RUN.BAT
ECHo ECHO.>>                     RUN.BAT
ECHO EXIT /B 1>>                 RUN.BAT
ECHO ^:END>>                     RUN.BAT

GOTO END_SELECT_RUNTIME

:SELECT_RUNTIME
  SET TARGET=%1
  SET SCRIPT=%2
  ECHO.>OPTS.TXT
:START_SELECT_RUNTIME
  CLS
  ECHO * Selected Platform^:
  ECHO !TARGET!
  ECHO.
  ECHO * Runtime^(s^)^:
  TYPE OPTS.TXT
  ECHO.
  ECHO Please select the compiler you want to use for this platform:
  ECHO.
  %GPRBUILDROOT%\bin\gprconfig --target=!TARGET! --config=Ada,* --config=C -o !TARGET!.cgpr || GOTO ERROR
  ECHO.>RTS.TXT
  FOR /F "tokens=1,4" %%B IN (!TARGET!.cgpr) DO (
    IF "%%B" == "Binder'Required_Switches" (
      ECHO %%C>>RTS.TXT
    )
  )
  FINDSTR RTS= RTS.TXT>RTSOPT.TXT || ECHO ^(^"--RTS=full^"^)^;>RTSOPT.TXT
  DEL RTS.TXT
  for /F "delims=^)" %%B in (RTSOPT.TXT) DO (SET OPT=%%B)
  SET RUNTIME=!OPT:~8,-1!
  ECHO !RUNTIME!>>OPTS.TXT
  DEL RTSOPT.TXT
  RENAME !TARGET!.cgpr !TARGET!-!RUNTIME!.cgpr
  IF "!TARGET!" == "pentium-mingw32msv" (
    SET PLATFORM="native"
  ) ELSE (
    ECHO !TARGET!>TMP.TXT
    FINDSTR vxworksae TMP.TXT
    DEL TMP.TXT
    IF %ERRORLEVEL% == 1 (
      SET PLATFORM=!TARGET!
    ) ELSE (
      SET PLATFORM=vxworksae
    )
  )
  ECHO %GPRBUILDROOT%\bin\gprbuild --config=!TARGET!-!RUNTIME!.cgpr -XRUNTIME=!RUNTIME! -XPLATFORM=!PLATFORM! -p -f -Paunit/aunit_build.gpr ^|^| GOTO ERROR >> !SCRIPT!
  CLS
  ECHO * Selected Platform *
  ECHO !TARGET!
  ECHO.
  ECHO * Runtime^(s^)      *
  TYPE OPTS.TXT
  ECHO.
  ECHO Do you want to add another runtime for this platform ?
  CHOICE /C YN /M "Press Y for Yes, N for No."
  IF "%ERRORLEVEL%" NEQ "2" ( GOTO START_SELECT_RUNTIME )
  DEL OPTS.TXT
  EXIT /B

:END_SELECT_RUNTIME

CLS
ECHO.
ECHO *** NOW COMPILING AUNIT ***
ECHO.
CALL RUN.BAT || GOTO ERROR
DEL RUN.BAT
CLS
ECHO.
ECHO *** AUNIT COMPILED SUCCESSFULLY ***
ECHO.
ECHO Please enter AUnit base installation directory
ECHO ** Press enter for installing in '%GPRBUILDROOT%':
SET /P INSTALL=

IF '%INSTALL%' == '' SET INSTALL=%GPRBUILDROOT%
SET I_GPR=%INSTALL%\lib\gnat
SET I_TPL=%INSTALL%\share\examples\aunit
SET I_DOC=%INSTALL%\share\doc\aunit
SET I_PLG=%INSTALL%\share\gps\plug-ins
SET I_INC=%INSTALL%\include\aunit
SET I_LIB=%INSTALL%\lib\aunit

ECHO.
ECHO ************************
ECHO *** INSTALLING AUNIT ***
ECHO ************************
ECHO.

ECHO.
ECHO uninstalling previous AUnit library in %INSTALL% if needed
DEL /Q "%I_GPR%\aunit.gpr" 2> NUL
DEL /Q "%I_TPL%\*.ad*" 2> NUL
DEL /Q "%I_DOC%\*.*" 2> NUL
RMDIR /Q/S "%I_INC%" 2> NUL
RMDIR /Q/S "%I_LIB%" 2> NUL

MKDIR "%I_DOC%" 2> NUL
MKDIR "%I_GPR%" 2> NUL
MKDIR "%I_TPL%" 2> NUL
MKDIR "%I_PLG%" 2> NUL
MKDIR "%I_LIB%" 2> NUL
MKDIR "%I_INC%" 2> NUL

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
MKDIR "%I_INC%\framework" 2> NUL
MKDIR "%I_INC%\reporters" 2> NUL
MKDIR "%I_INC%\containers" 2> NUL
XCOPY /S/Q /EXCLUDE:support\exclude.txt aunit\framework "%I_INC%\framework"   > NUL
XCOPY /S/Q /EXCLUDE:support\exclude.txt aunit\reporters "%I_INC%\reporters"   > NUL
XCOPY /S/Q /EXCLUDE:support\exclude.txt aunit\containers "%I_INC%\containers" > NUL

ECHO.
ECHO copying AUnit lib files in %I_LIB%
XCOPY /S/Q aunit\lib\*.* "%I_LIB%" > NUL

ECHO.
ECHO *******************************************
ECHO * AUnit has been successfully installed ! *
ECHO *******************************************
ECHO.
ECHO Press enter to close this window
SET /P FINISH=

GOTO END

:MISSINGGPRBUILD
ECHO.
ECHO GPRBUILD IS MISSING ON YOUR SYSTEM. PLEASE INSTALL IT FIRST !
ECHO.
ECHO.

:ERROR
ECHO.
ECHO ******************************************************
ECHO * !!! AN ERROR OCCURED DURING THIS SETUP PROCESS !!! *
ECHO *              PLEASE TRY AGAIN.                     *
ECHO *                                                    *
ECHO *           AUNIT IS NOT INSTALLED                   *
ECHO ******************************************************
ECHO Type ^<Enter^> to terminate
SET /P CHOICE=
EXIT /B 1
:END
