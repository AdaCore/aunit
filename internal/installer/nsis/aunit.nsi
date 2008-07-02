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
VIAddVersionKey  "FileVersion" "1.0.0.0"
VIAddVersionKey  "LegalCopyright" "Copyright ©AdaCore"
VIProductVersion "${PVERSION}"

;--------------------------------
;Pages

!insertmacro Locate
!insertmacro un.Locate
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "${RES}license.txt"
!define MUI_PAGE_CUSTOMFUNCTION_PRE onComponents
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
!define BASESECNUM 2 ;first target section

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

Section "AUnit Sources" SecSrc
  SectionIn RO

  SetOutPath "$INSTDIR\src\aunit"
  File /r /x .svn /x internal "${PRJ}*.*"
SectionEnd

SectionGroup /e "Compiled library" SecGrp
Section "-"
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

Section "-Real binary installation procedure"

  StrCpy $NUM "${BASESECNUM}"
  StrCpy $FNUM "1"

  compilesections:
  StrCmp $MAX $NUM +2
  Goto +2
  Goto compileend

  ReadINIStr $R0 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Target"
  SectionGetFlags $NUM $R1
  IntOp $R1 $R1 & ${SF_SELECTED}

  StrCmp "0" $R1 +2
  Goto compile
  Goto continuecompile

  compile:
  ReadINIStr $R1 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Version"
  ReadINIStr $R2 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Runtime"
  ReadINIStr $R3 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Path"
  ReadINIStr $R4 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "XRUNTIME"
  ReadINIStr $R5 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "XPLATFORM"

  nsExec::ExecToLog /OEM '"$GPRBUILDROOT\bin\gprconfig" --target=$R0 --config=Ada,$R1,$R2,"$R3" --config=C --batch -o config.cgpr'
  Pop $0
  StrCmp "0" $0 +2
  Goto abortcompile

  nsExec::ExecToLog /OEM '"$GPRBUILDROOT\bin\gprbuild" --config=config.cgpr -p -P $PLUGINSDIR\aunit\aunit_build -XRUNTIME=$R4 -XPLATFORM=$R5'
  Pop $0
  StrCmp "0" $0 +2
  Goto abortcompile

  continuecompile:

  IntOp $NUM $NUM + 1
  IntOp $FNUM $FNUM + 1
  Goto compilesections

  abortcompile:
  MessageBox MB_OK|MB_ICONSTOP "The compilation failed. Aborting the installation."
  abort

  compileend:

  SetOutPath "$INSTDIR\lib\gnat"
  File "/oname=$INSTDIR\lib\gnat\aunit.gpr" "${PRJ}support\aunit.gpr"
  SetOutPath "$INSTDIR\include\aunit"
  File /r /x *.gpr /x *.cgpr /x *~ /x *.adc /x .svn "${PRJ}aunit\*.*"
  SetOutPath "$INSTDIR\lib\aunit"
  StrCpy $R0 "$PLUGINSDIR\aunit\lib"
  StrCpy $R1 "$INSTDIR\lib\aunit"
  StrLen $R2 $R0
  ${Locate} "$R0" "/L=FDE" "CopyCb"
  Rename "$PLUGINSDIR\aunit\lib\*.*" "$INSTDIR\lib\aunit"

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

Function onComponents
  Banner::show /NOUNLOAD "Analyzing installed software"
  Banner::getWindow /NOUNLOAD
  Pop $1

  nsExec::Exec '"$PLUGINSDIR\setup_utility.exe"'
  Pop $0
  StrCmp "0" $0 +2
  Goto abortinstall

  Banner::Destroy
  InstallOptions::initDialog /NOUNLOAD "$PLUGINSDIR\aunit.ini"

  ReadINIStr $GPRBUILDROOT "$PLUGINSDIR\aunit.ini" "Settings" "Install"
  StrCpy $INSTDIR $GPRBUILDROOT
  ReadINIStr $0 "$PLUGINSDIR\aunit.ini" "Settings" "NumFields"
  StrCpy $MAX $0
  IntOp $MAX $MAX + ${BASESECNUM}
  StrCpy $NUM "${BASESECNUM}"
  StrCpy $FNUM "1"

  fillsections:
  StrCmp $MAX $NUM +2
  Goto +2
  Goto endoninit

  ReadINIStr $R1 "$PLUGINSDIR\aunit.ini" "Field $FNUM" "Name"
  SectionSetText $NUM "$R1"

  IntOp $NUM $NUM + 1
  IntOp $FNUM $FNUM + 1
  Goto fillsections

  goto endoninit

  abortinstall:
  MessageBox MB_OK|MB_ICONSTOP "Installation wizard could not find gprconfig. Make sure this is installed before executing this setup."
  abort "Can't install"

  endoninit:
FunctionEnd

Function .onInit

  ;Extract InstallOptions INI files
  InitPluginsDir
  File /oname=$PLUGINSDIR\setup_utility.exe "${UTILITY}"
  SetOutPath "$PLUGINSDIR"
  File /r /x .svn "${PRJ}aunit"
FunctionEnd

LangString DESC_SecSrc ${LANG_ENGLISH} "Installation of AUnit Sources."
LangString DESC_SecGrp ${LANG_ENGLISH} "Compiled library."
LangString DESC_SecDoc ${LANG_ENGLISH} "The AUnit documentation."
LangString DESC_SecExamples ${LANG_ENGLISH} "A set of examples"

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecSrc} $(DESC_SecSrc)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecGrp} $(DESC_SecGrp)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDoc} $(DESC_SecDoc)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecExamples} $(DESC_SecExamples)
!insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section
Var Deleted

;Callback for removing empty directories
Function un.DelEmptyCb
   RmDir $R9
   StrCpy $Deleted "1"
   Push $0
FunctionEnd

Section "Uninstall"

  RMDir /r "$INSTDIR\src\aunit"
  RMDir /r "$INSTDIR\include\aunit"
  RMDir /r "$INSTDIR\lib\aunit"
  RMDir /r "$INSTDIR\share\doc\aunit"
  RMDir /r "$INSTDIR\share\examples\aunit"
  Delete "$INSTDIR\lib\gnat\aunit.gpr"
  Delete "$INSTDIR\share\gps\plug-ins\aunit.xml"

  deleteEmptyDirs:
  StrCpy $Deleted "0"
  ${un.Locate} "$INSTDIR" "/L=DE" "un.DelEmptyCb"
  StrCmp $Deleted "0" +2
  Goto deleteEmptyDirs

  ; Delete other shortcuts
  DeleteRegKey HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\AUnit"
  DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Ada Core Technologies\AUnit"
        
SectionEnd

; eof
