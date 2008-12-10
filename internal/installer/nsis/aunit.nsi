;--------------------------------
;Define application defines

!define APPNAME "AUnit"
!define SHORTNAME "aunit"
!define VERSION "Unknown"
!define PVERSION "1.0.0.0"

!define APPNAMEANDVERSION "${APPNAME} ${VERSION}"

!define PRJ "..\..\..\"
!define RES "..\resources\"
!define UTILITY "..\setup_utility\setup_utility.exe"

;--------------------------------
;Modern UI
!include "MUI2.nsh"
!include "Sections.nsh"
!include "InstallOptions.nsh"
!include "FileFunc.nsh"

SetCompressor /SOLID lzma

ReserveFile "${UTILITY}"

RequestExecutionLevel admin

Name "${APPNAMEANDVERSION}"
Caption "${APPNAME} ${VERSION} Setup"

;--------------------------------
; Interface Settings

!define MUI_ABORTWARNING
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "${RES}gnaticon.bmp"
!define MUI_HEADERIMAGE_BITMAP_NOSTRETCH
!define MUI_WELCOMEFINISHPAGE_BITMAP "welcome.bmp"
!define MUI_COMPONENTSPAGE_SMALLDESC

;--------------------------------
;General Settings

VIAddVersionKey  "ProductName" "${APPNAME}"
VIAddVersionKey  "Comments" ""
VIAddVersionKey  "CompanyName" "AdaCore"
VIAddVersionKey  "FileDescription" "${APPNAMEANDVERSION} Installer"
VIAddVersionKey  "FileVersion" "${PVERSION}"
VIAddVersionKey  "LegalCopyright" "Copyright ©AdaCore"
VIProductVersion "${PVERSION}"

;--------------------------------
;Pages

!insertmacro Locate
!insertmacro MUI_PAGE_WELCOME
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE onLicenseLeave
  !insertmacro MUI_PAGE_LICENSE "${RES}license.txt"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

;Uninstall properties
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Language
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_RESERVEFILE_LANGDLL

;Show install details.
ShowInstDetails show

BrandingText "©2008 AdaCore"
Icon "${RES}11_installer.ico"
UninstallIcon "${RES}11_installer.ico"
OutFile "${SHORTNAME}-${VERSION}.exe"
InstallDir "C:\AUnit"
InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Ada Core Technologies\AUnit" ROOT

;--------------------------------
; variables

Var NUM ;the target section
Var MAX ;max target section
Var FNUM ;field number as returnrd by .ini file
Var GPRBUILDROOT ;root directory of gprbuild

Function CopyCb
   StrCpy $1 $R8 '' $R2

   StrCmp $R6 '' 0 +3
   CreateDirectory '$R1$1\$R7'
   goto endCopyFile
   
   CreateDirectory '$R1$1'
   CopyFiles '$R9' '$R1$1'
   
   endCopyFile:
   Push $0
FunctionEnd

;--------------------------------
;Installer Sections

SectionGroup /e "Compiled library" SecGrp
Section "-" BaseSecNum
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
Section /o "-"
SectionEnd
SectionGroupEnd

;-------------------------------------------------------------
; this section is in charge of installing the compiled library.

Section "-installbin"
  ; NUM is the section number
  StrCpy $NUM "${BaseSecNum}"
  ; FNUM is the Field number, as set in aunit.ini
  StrCpy $FNUM "1"

  ; start of loop for compilations
  compilesections:

  ; if NUM is MAX, we finished the compilation process.
  StrCmp $MAX $NUM compileend

  ; We get the flags from section $NUM
  SectionGetFlags $NUM $R1
  ; And see if the flag SF_SELECTED is set
  IntOp $R1 $R1 & ${SF_SELECTED}
  ; $R1==0 => not selected, we skip compilation for this section
  StrCmp "0" $R1 continuecompile

  ; Get the values needed for the compilation process
  ReadINIStr $R0 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Target"
  ReadINIStr $R1 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Version"
  ReadINIStr $R2 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Runtime"
  ReadINIStr $R3 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Path"
  ReadINIStr $R4 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "XRUNTIME"
  ReadINIStr $R5 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "XPLATFORM"

  ; We first create a config file for the target/runtime
  nsExec::ExecToLog '"$GPRBUILDROOT\bin\gprconfig" --target=$R0 --config=Ada,$R1,$R2,"$R3" --config=C --batch'
  Pop $0
  ; if exit code is not 0, we abort
  StrCmp $0 "0" 0 abortcompile

  ; Actual compilation
  nsExec::ExecToLog '"$GPRBUILDROOT\bin\gprbuild" --target=$R0 -p -P $PLUGINSDIR\aunit\aunit_build -XRUNTIME=$R4 -XPLATFORM=$R5'
  Pop $0
  ; if exit code is not 0, we abort
  StrCmp $0 "0" 0 abortcompile

  continuecompile:

  ; $NUM := $NUM + 1; $FNUM := $FNUM + 1
  IntOp $NUM $NUM + 1
  IntOp $FNUM $FNUM + 1
  ; and we return to the begining of the loop
  Goto compilesections

  ; compilation error occured: we abort installation
  abortcompile:
  MessageBox MB_OK|MB_ICONSTOP "The compilation failed. Aborting the installation."
  abort

  ; Now that everything is compiled, we install the files
  compileend:

  SetOutPath "$INSTDIR\lib\gnat"
  File "${PRJ}support\aunit.gpr"
  SetOutPath "$INSTDIR\include\aunit"
  File /r /x *.gpr /x *.cgpr /x *~ /x *.adc /x .svn "${PRJ}aunit\*.*"
  SetOutPath "$INSTDIR\lib\aunit"
  ; next 4 lines: recursive copy of aunit\lib to INSTDIR\lib\aunit
  StrCpy $R0 "$PLUGINSDIR\aunit\lib"
  StrCpy $R1 "$INSTDIR\lib\aunit"
  StrLen $R2 $R0
  ${Locate} "$R0" "/L=FDE" "CopyCb"

  ; copy also README and COPYING
  SetOutPath "$INSTDIR\share\doc\aunit"
  File "${PRJ}README"
  File "${PRJ}COPYING"

  ;Write the installation path into the registry
  WriteRegStr HKLM "SOFTWARE\Ada Core Technologies\AUnit" "ROOT" "$INSTDIR"

  ;Write the uninstall keys for Windows
  WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}-${VERSION}" "UninstallString" '"$INSTDIR\uninstall-${SHORTNAME}.exe"'
  WriteRegExpandStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}-${VERSION}" "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}-${VERSION}" "DisplayName" "${APPNAMEANDVERSION}"
  WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}-${VERSION}" "DisplayIcon" "$COMMONFILES\AdaCore\gnaticons.dll,22"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}-${VERSION}" "DisplayVersion" "${VERSION}"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}-${VERSION}" "NoModify" "1"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}-${VERSION}" "NoRepair" "1"
  WriteUninstaller "$INSTDIR\uninstall-${SHORTNAME}.exe"

SectionEnd

Section "Documentation" SecDoc
  SetOutPath "$INSTDIR\share\doc\aunit"
  File /r /x .svn /x *.texi /x makefile "${PRJ}docs\*.*"
  SetOutPath "$INSTDIR\share\gps\plug-ins"
  File "/oname=$INSTDIR\share\gps\plug-ins\aunit.xml" "${PRJ}support\aunit.xml"
SectionEnd

Section "Examples" SecExamples
  SetOutPath "$INSTDIR\share\examples\aunit"
  File /r /x .svn "${PRJ}examples\*.*"
SectionEnd

Section /o "Sources" SecSrc
  SetOutPath "$INSTDIR\src\aunit"
  File /r /x .svn /x internal /x *~ "${PRJ}*.*"
SectionEnd

;------------------------------------------------------------
; Description initialisation for all sections
; the description is seen in the page MUI_PAGE_COMPONENTS

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDoc} "The AUnit documentation"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecExamples} "A set of AUnit examples"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrc} "The AUnit source package"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecGrp} "The AUnit library"
  !verbose push
  ${elseif} $0 >= ${BaseSecNum}
    IntOp $NUM $0 - ${BaseSecNum}
    IntOp $NUM $NUM + 1
    ReadINIStr $R1 "$PLUGINSDIR\aunit.ini" "Field $NUM" "Target"
    ReadINIStr $R2 "$PLUGINSDIR\aunit.ini" "Field $NUM" "Version"
    ReadINIStr $R3 "$PLUGINSDIR\aunit.ini" "Field $NUM" "Runtime"
    ReadINIStr $R4 "$PLUGINSDIR\aunit.ini" "Field $NUM" "Path"
    ; Clear previous page
    SendMessage $mui.ComponentsPage.DescriptionText ${WM_SETTEXT} 0 "STR:"
    EnableWindow $mui.ComponentsPage.DescriptionText 1
    SendMessage $mui.ComponentsPage.DescriptionText ${WM_SETTEXT} 0 "STR:Compile using GNAT $R2 for $R1 ($R3 runtime) found in $R4"
  !verbose pop
!insertmacro MUI_FUNCTION_DESCRIPTION_END

;---------------------------------------------------------------
; This function is called after the user accepted the license
; it is in charge of running the external setup utility that
; determines available configurations, and place them into 
; the aunit.ini file.
Function onLicenseLeave
  !verbose push

  Banner::show /NOUNLOAD "Analyzing installed software"

  nsExec::Exec '"$PLUGINSDIR\setup_utility.exe"'
  Pop $0
  StrCmp "0" $0 0 abortinstall

  Banner::Destroy

  ; Now that the tool is run, we initialize some global variable:
  ; $INSTDIR: default installation path (gprbuild root dir)
  ; $MAX: last compilation section containing actual data
  ReadINIStr $GPRBUILDROOT "$PLUGINSDIR\aunit.ini" "Settings" "Install"
  StrCpy $INSTDIR $GPRBUILDROOT
  ReadINIStr $0 "$PLUGINSDIR\aunit.ini" "Settings" "NumFields"
  StrCpy $MAX $0
  IntOp $MAX $MAX + ${BaseSecNum}

  ; Now we are initializing the Sec number (NUM) and Field number (FNum)
  StrCpy $NUM "${BaseSecNum}"
  StrCpy $FNUM "1"

  fillsections:
  StrCmp $MAX $NUM endoninit

  ReadINIStr $R1 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Name"
  ; Set the text for section NUM to 'Field FNUM->Name'
  SectionSetText $NUM "$R1"

  IntOp $NUM $NUM + 1
  IntOp $FNUM $FNUM + 1
  Goto fillsections

  goto endoninit

  abortinstall:
  MessageBox MB_OK|MB_ICONSTOP "Installation wizard could not find gprconfig. Make sure this is installed before executing this setup."
  abort "Can't install"

  endoninit:
  !verbose pop
FunctionEnd

Function .onInit
  ;Extract InstallOptions INI files
  InitPluginsDir
  File /oname=$PLUGINSDIR\setup_utility.exe "${UTILITY}"
  SetOutPath "$PLUGINSDIR"
  File /r /x .svn "${PRJ}aunit"
FunctionEnd

;--------------------------------
;Uninstaller Section
Section "Uninstall"

  RMDir /r "$INSTDIR\src\aunit"
  RMDir "$INSTDIR\src"
  RMDir /r "$INSTDIR\include\aunit"
  RMDir "$INSTDIR\include"
  RMDir /r "$INSTDIR\lib\aunit"
  Delete "$INSTDIR\lib\gnat\aunit.gpr"
  RMDir "$INSTDIR\lib\gnat"
  RMDir "$INSTDIR\lib"
  RMDir /r "$INSTDIR\share\doc\aunit"
  RMDir "$INSTDIR\share\doc"
  RMDir /r "$INSTDIR\share\examples\aunit"
  RMDir "$INSTDIR\share\examples"
  Delete "$INSTDIR\share\gps\plug-ins\aunit.xml"
  RMDir "$INSTDIR\share\gps\plug-ins"
  RMDir "$INSTDIR\share\gps"
  RMDir "$INSTDIR\share"
  Delete "$INSTDIR\uninstall-${SHORTNAME}.exe"
  RMDir "$INSTDIR"

  ; Delete other shortcuts
  DeleteRegKey HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APPNAME}-${VERSION}"
  DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Ada Core Technologies\AUnit"
        
SectionEnd

; eof
