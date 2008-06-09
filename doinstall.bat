@ECHO OFF
SETLOCAL

REM === Test defered variable evaluation
SET I=
SET I=!I!
IF NOT '%I%' == '' (
  CMD /V/C .\DOINSTALL.BAT
  GOTO END
)

GOTO START

REM *********************
REM * UTILITY FUNCTIONS *
REM *********************

REM ==== FINDINPATH ==========================================================
REM Walks through the PATH and searches for gprbuild.exe
REM OUTPUT: PATH.TXT

:FINDINPATH
SETLOCAL ENABLEEXTENSIONS
SET INTPATH="%PATH:;=" "%"
ECHO.> PATH.TXT
CALL :SHIFTPATH %INTPATH%
EXIT /B %ERRORLEVEL%

:SHIFTPATH
IF "%~1" NEQ "" (
  DIR "%~1\gprbuild.exe" > NUL 2> NUL
  IF !ERRORLEVEL! == 0 (
    ECHO %~1\..> PATH.TXT
    EXIT /B 0
  )
  SHIFT
  GOTO SHIFTPATH
)
EXIT /B 1

REM ==== FINDINREGISTRY =======================================================
REM Tries to retrieve gprbuild installation dir from GPRBUILD registry key
REM OUTPUT: PATH.TXT

:FINDINREGISTRY
ECHO.> PATH.TXT
REG QUERY "HKLM\Software\Ada Core Technologies\GPRBUILD" /v ROOT > REGS.TXT 2> NUL
IF !ERRORLEVEL! == 0 (
  ECHO FOUND IN REGISTRY
  FOR /F "skip=2 tokens=1,3" %%A IN (REGS.TXT) DO (
    if %%A == ROOT (
      ECHO %%B> PATH.TXT
      DEL REGS.TXT
      EXIT /B 0
    )
  )
)
DEL REGS.TXT
EXIT /B 1

REM ==== FINDINGNATREGISTRY ===================================================
REM Retrieving gprbuild from installed gnat compiler
REM OUTPUT: PATH.TXT

:FINDINGNATREGISTRY
ECHO.> PATH.TXT
REG QUERY "HKLM\Software\Ada Core Technologies" /S > REGS.TXT

FOR /F "skip=9 tokens=1,3" %%A IN (REGS.TXT) DO (
  IF "%%A" == "ROOT" (
    DIR "%%B\bin\gprbuild.exe" > NUL 2> NUL
    IF !ERRORLEVEL! == 0 (
      ECHO %%B> PATH.TXT
      DEL REGS.TXT
      EXIT /B 0
    )
  )
)
DEL REGS.TXT
EXIT /B 1

REM ==== FINDRTS =============================================================
REM Uses gprconfig to retrieve all possible runtimes of a specific target
REM OUTPUT: RTS.TXT

:FINDRTS
"%GPRBUILDROOT%\bin\gprconfig" --target=%TARGET% --config=Ada,* --config=C > OUTPUT.TXT < NUL
ECHO.>RTS.TXT
FOR /F "delims=^( skip=3 tokens=2,3" %%A IN (OUTPUT.TXT) DO (
  SET /A OK=0
  FOR /F "tokens=4" %%C IN ('ECHO %%A') DO (
    IF "%%C" == "Ada" ( SET /A OK=1 )
  )
  IF !OK! == 1 (
    FOR /F %%C IN ('ECHO %%B') DO (
      SET RTS=%%C
      IF "%%C" == "sjlj" ( SET RTS=full )
      IF "%%C" == "zcx" ( SET RTS=full )
      IF "%%C" == "default" ( SET RTS=full )
      ECHO !RTS!>> RTS.TXT
    )
  )
)
DEL OUTPUT.TXT
SORT RTS.TXT > RTS2.TXT
SET PREV=
ECHO.>RTS.TXT
FOR /F %%A IN (RTS2.TXT) DO (
  IF "!PREV!" NEQ "%%A" (
    ECHO %%A >> RTS.TXT
    SET PREV=%%A
  )
)
DEL RTS2.TXT
EXIT /B 1

REM ***************************************************************************
REM ***************************************************************************
REM ****                                                                   ****
REM ****                    START THE SCRIPT                               ****
REM ****                                                                   ****
REM ***************************************************************************
REM ***************************************************************************

:START

REM === reset of the used variables
SET GPRBUILDROOT=
SET GNAT=
SET INSTALL=
SET RUNTIME=
SET TOOL_PREFIX=

REM ===== CONFIGURATION PHASE ================================================

CLS
ECHO.
ECHO.
ECHO.
ECHO **************************
ECHO *** PREPARING TO SETUP ***
ECHO **************************
ECHO.

REM Retrieving gprbuild path

CALL :FINDINPATH
IF %ERRORLEVEL% == 1 (
  CALL :FINDINREGISTRY
  IF %ERRORLEVEL% == 1 (
    CALL :FINDINGNATREGISTRY
  )
)
IF %ERRORLEVEL% == 0 (
  FOR /F "delims=;" %%A IN (PATH.TXT) DO (
    SET GPRBUILDROOT=%%A
  )
)
DEL PATH.TXT

IF '!GPRBUILDROOT!' == '' ( GOTO MISSINGGPRBUILD )

:SELECT_TARGET
SET /A I = 0
ECHO.>TARGETSCHOICE.TXT
FOR /F "skip=1" %%A IN ('"!GPRBUILDROOT!\bin\gprconfig" --show-targets') DO (
  SET TARGET=%%A
  CALL :FINDRTS
  FOR /F %%B IN (RTS.TXT) DO (
    SET LAST_TARGET=%%A ^(%%B^)
    ECHO !LAST_TARGET!>> TARGETSCHOICE.TXT
    SET /A I=!I!+1
  )
  DEL RTS.TXT
)

IF "!I!" EQU "1" (
  ECHO %LAST_TARGET% > TARGETS.TXT
) ELSE (
  ECHO.>TARGETS.TXT
  SET ONESELECTED=
:SELECT_TARGETS
  SET /A J=O
  SET OPTIONS=
  CLS
  ECHO *** Select the targets you want AUnit to be compiled for.
  FOR /F "delims=;" %%A IN (TARGETSCHOICE.TXT) DO (
    SET FOUND=0
    FOR /F "delims=;" %%B IN (TARGETS.TXT) DO (
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
  IF "!CHOICE!" EQU "" (
    IF '!ONESELECTED!' == '' (
      GOTO SELECT_TARGETS
    ) ELSE (
      GOTO END_SELECT_TARGETS
    )
  )
  SET /A J=0
  FOR /F "delims=;" %%A IN (TARGETSCHOICE.TXT) DO (
    SET /A J=!J!+1
    SET FOUND=0
    FOR /F "delims=;" %%B IN (TARGETS.TXT) DO (
      IF '%%A' == '%%B' (
        SET FOUND=1
      )
    )
    IF "!FOUND!" EQU "0" (
      IF '!J!' == '!CHOICE!' (
        SET /A I=!I!-1
        ECHO %%A>> TARGETS.TXT
        SET ONESELECTED=1
      )
    )
  )
  IF '!I!' NEQ '0' (
    GOTO SELECT_TARGETS
  )
)

:END_SELECT_TARGETS
DEL TARGETSCHOICE.TXT

CLS
ECHO.
ECHO.
ECHO.
ECHO ****************************
ECHO *** PREPARING TO COMPILE ***
ECHO ****************************
ECHO.

ECHO.>                   RUN.BAT

FOR /F "delims=;" %%A IN (TARGETS.TXT) DO (
  FOR /F "delims=^(^) tokens=1,2" %%B IN ('ECHO %%A') DO (
    SET TARGET=%%~nxB
    SET RTS=%%C
    DEL !TARGET!-!RTS!.cgpr
    IF !RTS! == full (SET RUNTIME=) ELSE (SET RUNTIME=!RTS!)
    "!GPRBUILDROOT!\bin\gprconfig" --batch --target=!TARGET! --config=Ada,,!RUNTIME! --config=C -o !TARGET!-!RTS!.cgpr 1> NUL 2> NUL || GOTO ERROR

    IF "!TARGET!" == "pentium-mingw32msv" (
      SET PLATFORM="native"
    ) ELSE (
      SET PLATFORM=!TARGET!
    )

    ECHO ECHO "!GPRBUILDROOT!\bin\gprbuild" --config=!TARGET!-!RTS!.cgpr -XRUNTIME=!RTS! -XPLATFORM=!PLATFORM! -p -f -Paunit/aunit_build.gpr>> RUN.BAT
    ECHO "!GPRBUILDROOT!\bin\gprbuild" --config=!TARGET!-!RTS!.cgpr -XRUNTIME=!RTS! -XPLATFORM=!PLATFORM! -p -f -Paunit/aunit_build.gpr ^|^| GOTO ERROR>> RUN.BAT
  )
)
DEL TARGETS.TXT

ECHO GOTO END>>                  RUN.BAT
ECHO ^:ERROR>>                   RUN.BAT
ECHO ECHO *** BUILD FAILED ***>> RUN.BAT
ECHO ECHO.>>                     RUN.BAT
ECHO EXIT /B 1 >>                RUN.BAT
ECHO ^:END>>                     RUN.BAT

ECHO.
ECHO *** NOW COMPILING AUNIT ***
ECHO.
CALL RUN.BAT || GOTO ERROR
DEL RUN.BAT
IF !ERRORLEVEL! == 1 (
  GOTO ERROR
)
ECHO.
ECHO *** AUNIT COMPILED SUCCESSFULLY ***
ECHO.
ECHO Type ^<Enter^> to continue
SET /P TOTO=
CLS
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
ATTRIB -R "%I_GPR%\aunit.gpr" 2> NUL
DEL /Q "%I_GPR%\aunit.gpr" 2> NUL
ATTRIB -R "%I_TPL%" 2> NUL
RMDIR /Q/S "%I_TPL%" 2> NUL
ATTRIB -R "%I_DOC%\*.*" 2> NUL
DEL /Q "%I_DOC%\*.*" 2> NUL
ATTRIB -R "%I_PLG%\aunit.xml" 2> NUL
DEL /Q "%I_PLG%\aunit.xml"
ATTRIB -R "%I_INC%" 2> NUL
RMDIR /Q/S "%I_INC%" 2> NUL
ATTRIB -R "%I_LIB%" 2> NUL
RMDIR /Q/S "%I_LIB%" 2> NUL

MKDIR "%I_DOC%" 2> NUL
MKDIR "%I_GPR%" 2> NUL
MKDIR "%I_TPL%" 2> NUL
MKDIR "%I_PLG%" 2> NUL
MKDIR "%I_LIB%" 2> NUL
MKDIR "%I_INC%" 2> NUL

ECHO.
ECHO copying examples in %I_TPL%
XCOPY /S/Q /EXCLUDE:support\exclude.txt examples "%I_TPL%" > NUL || GOTO ERROR
ECHO.
ECHO copying documentation in %I_DOC%
COPY docs\*.html "%I_DOC%" > NUL || GOTO ERROR
COPY docs\*.pdf "%I_DOC%" > NUL || GOTO ERROR
COPY docs\*.txt "%I_DOC%" > NUL || GOTO ERROR
ECHO.
ECHO copying GPS plug-in in %I_PLG%
COPY support\aunit.xml "%I_PLG%" > NUL || GOTO ERROR

ECHO.
ECHO copying aunit.gpr in %I_GPR%
COPY support\aunit.gpr "%I_GPR%" > NUL || GOTO ERROR

ECHO.
ECHO copying AUnit source files in %I_INC%
MKDIR "%I_INC%\framework"
MKDIR "%I_INC%\reporters"
MKDIR "%I_INC%\containers"
XCOPY /S/Q /EXCLUDE:support\exclude.txt aunit\framework "%I_INC%\framework"   > NUL || GOTO ERROR
XCOPY /S/Q /EXCLUDE:support\exclude.txt aunit\reporters "%I_INC%\reporters"   > NUL || GOTO ERROR
XCOPY /S/Q /EXCLUDE:support\exclude.txt aunit\containers "%I_INC%\containers" > NUL || GOTO ERROR

ECHO.
ECHO copying AUnit lib files in %I_LIB%
XCOPY /S/Q aunit\lib\*.* "%I_LIB%" > NUL || GOTO ERROR

ECHO.
ECHO *******************************************
ECHO * AUnit has been successfully installed ! *
ECHO *******************************************
ECHO.
ECHO Press enter to close this window
SET /P FINISH=

GOTO END

REM ***************************
REM * ERROR DISPLAY FUNCTIONS *
REM ***************************

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
