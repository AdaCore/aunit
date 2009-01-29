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
!include "logiclib.nsh"
!include "Sections.nsh"
!include "InstallOptions.nsh"
!include "FileFunc.nsh"
!include "WinMessages.nsh"
SetCompressor /SOLID lzma

ReserveFile "${UTILITY}"

RequestExecutionLevel admin

Name "${APPNAMEANDVERSION}"
Caption "${APPNAMEANDVERSION} Setup"

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

Var ALTERNATIVE_GUI_TITLE
Var ALTERNATIVE_TEXT
!define MUI_DIRECTORYPAGE_VARIABLE          $INSTDIR   ;selected by user
!define MUI_DIRECTORYPAGE_TEXT_DESTINATION  $ALTERNATIVE_GUI_TITLE  ;caption above path
!define MUI_DIRECTORYPAGE_TEXT_TOP          $ALTERNATIVE_TEXT  ;descriptive text

!insertmacro Locate
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "${RES}license.txt"
!define MUI_PAGE_CUSTOMFUNCTION_PRE GnatDirectoryPre
!define MUI_PAGE_CUSTOMFUNCTION_SHOW GnatDirectoryShow
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE GnatDirectoryLeave
!insertmacro MUI_PAGE_DIRECTORY
!define MUI_PAGE_CUSTOMFUNCTION_PRE InstallDirectoryPre
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_INSTFILES
;!insertmacro MUI_PAGE_FINISH

;Uninstall properties
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Language
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_RESERVEFILE_LANGDLL

;Show install details.
ShowInstDetails show
SpaceTexts 'none'
BrandingText "©2008-2009 AdaCore"
Icon "${RES}11_installer.ico"
UninstallIcon "${RES}11_installer.ico"
OutFile "${SHORTNAME}-${VERSION}.exe"
InstallDir "C:\"
InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Ada Core Technologies\AUnit" ROOT

;--------------------------------
; variables

Var SECNUM ;current section
Var COMP ;compiler id
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
Section "-" BaseGrpNum
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
Section /o "-" LastGrpNum
SectionEnd
SectionGroupEnd 

;-------------------------------------------------------------
; this section is in charge of installing the compiled library.

Section "-installbin"
  ; NUM is the section number
  StrCpy $SECNUM "${BaseGrpNum}"
  ReadINIStr $R6 "$PLUGINSDIR\aunit.ini" "Settings" "NumCompilers"

  ; install all selected sections
  ${For} $COMP 1 $R6
    ${If} ${SectionIsSelected} $SECNUM
      ; Get the values needed for the compilation process
      ReadINIStr $R0 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Target"
      ReadINIStr $R1 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Version"
      ReadINIStr $R2 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Runtime"
      ReadINIStr $R3 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Path"
      ReadINIStr $R4 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "XRUNTIME"
      ReadINIStr $R5 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "XPLATFORM"
      
      ; We first create a config file for the target/runtime
      nsExec::ExecToLog '"$GPRBUILDROOT\bin\gprconfig" --target=$R0 --config=Ada,$R1,$R2,"$R3" --config=C --batch -o $R4-$R5.cgpr'
      Pop $0
      ; if exit code is not 0, we abort
      ${If} $0 != 0
        MessageBox MB_OK|MB_ICONSTOP "The compilation failed. Aborting the installation."
        abort
      ${EndIf}

      ; Actual compilation
      nsExec::ExecToLog '"$GPRBUILDROOT\bin\gprbuild" -f -p -P $PLUGINSDIR\aunit\aunit_build -XRUNTIME=$R4 -XPLATFORM=$R5 --config=$R4-$R5.cgpr'
      Pop $0
      ; if exit code is not 0, we abort
      ${If} $0 != 0
        MessageBox MB_OK|MB_ICONSTOP "The compilation failed. Aborting the installation."
        abort
      ${EndIf}
    ${EndIf}
    
    IntOp $SECNUM $SECNUM + 1
  ${Next}
  
  ; Now that everything is compiled, we install the files
  
  ; install aunit.gpr and aunit_shared.gpr
  SetOutPath "$INSTDIR\lib\gnat"
  File "${PRJ}support\aunit.gpr"
  ; install the aunit_shared.gpr file created by this installer
  CopyFiles "$PLUGINSDIR\support\aunit_shared.gpr" "$INSTDIR\lib\gnat"
  ; install sources
  SetOutPath "$INSTDIR\include\aunit"
  File /r /x *.gpr /x *.cgpr /x *~ /x *.adc /x .svn "${PRJ}aunit\*.*"
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
  ${elseif} $0 >= ${BaseGrpNum}
    IntOp $COMP $0 - ${BaseGrpNum}
    IntOp $COMP $COMP + 1
    ReadINIStr $R0 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Target"
    ReadINIStr $R1 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Version"
    ReadINIStr $R2 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Runtime"
    ; Clear previous page
    SendMessage $mui.ComponentsPage.DescriptionText ${WM_SETTEXT} 0 "STR:"
    EnableWindow $mui.ComponentsPage.DescriptionText 1
    SendMessage $mui.ComponentsPage.DescriptionText ${WM_SETTEXT} 0 "STR:Compile using GNAT $R1 for $R0 ($R2 runtime)"
  !verbose pop
!insertmacro MUI_FUNCTION_DESCRIPTION_END

; DirectoryPre: determine what we need to show in the directory page.
Function GnatDirectoryPre
  ; GNAT compiler selection. Let's initialize INSTDIR to the gprbuild directory
  nsExec::Exec '"$PLUGINSDIR\setup_utility.exe" -findgprbuild'
  Pop $0
  ${If} $0 != 0
    MessageBox MB_OK|MB_ICONSTOP "Installation wizard could not find gprconfig. Make sure this is installed before executing this setup."
    Abort
  ${EndIf}
  
  ; Now that the tool is run, we initialize some global variable:
  ; $INSTDIR: default installation path (gprbuild root dir)
  ReadINIStr $GPRBUILDROOT "$PLUGINSDIR\aunit-pre.ini" "Settings" "Install"
  StrCpy $INSTDIR $GPRBUILDROOT

  StrCpy $ALTERNATIVE_GUI_TITLE "GNAT Compiler location"
  StrCpy $ALTERNATIVE_TEXT "The following GNAT compiler will be used to compile ${APPNAME}.$\r$\n$\r$\nTo use another compiler, click Browse and select its installation folder. Click Next to continue."
FunctionEnd

Function GnatDirectoryShow
  !insertmacro MUI_HEADER_TEXT "Compiler Selection" "GNAT Compiler toolchain selection"
FunctionEnd

; DirectoryLeave: initialize the sections to show the available compilation targets.
Function GnatDirectoryLeave
  ; Retrieve all GNAT compilers in the selected path
  Banner::show /NOUNLOAD "Analyzing installed software"
  nsExec::Exec '"$PLUGINSDIR\setup_utility.exe" -path "$INSTDIR"'
  Banner::Destroy

  Pop $0
  ${If} $0 != 0
    MessageBox MB_OK|MB_ICONSTOP "Installation wizard could not find a GNAT compiler in the selected path. Please make sure to select a GNAT compiler installation path."
    Abort
  ${EndIf}

  StrCpy $SECNUM ${BaseGrpNum}
  ReadINIStr $R0 "$PLUGINSDIR\aunit.ini" "Settings" "NumCompilers"

  ${For} $COMP 1 $R0
    ReadINIStr $0 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Target"
    ReadINIStr $1 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Version"
    ReadINIStr $2 "$PLUGINSDIR\aunit.ini" "Compiler $COMP" "Runtime"

    ${If} $0 == "i686-pc-mingw32"
    ${OrIf} $0 == "pentium-mingw32msv"
      SectionSetText $SECNUM "GNAT $1 ($2 runtime)"
    ${Else}
      SectionSetText $SECNUM "GNAT $1 for $0 ($2 runtime)"
    ${EndIf}
    IntOp $SECNUM $SECNUM + 1
  ${Next}
FunctionEnd

Function InstallDirectoryPre
  StrCpy $ALTERNATIVE_GUI_TITLE "Destination Folder"
  StrCpy $ALTERNATIVE_TEXT "Setup will install ${APPNAME} in the following folder.$\r$\n$\r$\nTo install in a different folder, click Browse and select another folder. Click Next to continue."
FunctionEnd

Function .onInit
  ;Extract InstallOptions INI files
  InitPluginsDir
  File /oname=$PLUGINSDIR\setup_utility.exe "${UTILITY}"
  SetOutPath "$PLUGINSDIR"
  File /r /x .svn "${PRJ}aunit"
  File /r /x .svn "${PRJ}support"
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
  Delete "$INSTDIR\lib\gnat\aunit_shared.gpr"
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
