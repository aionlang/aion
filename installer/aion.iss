; Inno Setup script for the Windows installer of Aion.  Built by .github/workflows/release.yml, or by hand:
;
;   "C:\Program Files (x86)\Inno Setup 6\ISCC.exe" /DAppVersion=0.3.2 /DOutName=aion-setup installer\aion.iss
;
; after `cargo build --release --features bundled-zig` (see docs/LOCAL-BUILDING.MD), so that target\release\aion.exe
; has its C compiler inside.  The installer is written to dist\<OutName>.exe.
;
; It installs for the current user (no administrator needed; the dialog offers "all users" too), and can add the
; folder to PATH.  Silent install for scripts:  setup.exe /VERYSILENT /NORESTART  [/CURRENTUSER | /ALLUSERS]  [/DIR=...]

#ifndef AppVersion
  #define AppVersion "0.0.0"
#endif
#ifndef OutName
  #define OutName "aion-setup"
#endif

[Setup]
AppId={{D64BAEDA-C28A-46B8-BE9B-0BEA5E95D2A4}
AppName=Aion
AppVersion={#AppVersion}
AppPublisher=The Aion authors
AppPublisherURL=https://github.com/aionlang/aion
AppSupportURL=https://github.com/aionlang/aion/issues
DefaultDirName={autopf}\Aion
DisableProgramGroupPage=yes
DisableDirPage=auto
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
ChangesEnvironment=yes
OutputDir=..\dist
OutputBaseFilename={#OutName}
; aion.exe holds an archive that is already compressed, so more compression only costs time.
Compression=lzma2/fast
SolidCompression=no
WizardStyle=modern
UninstallDisplayIcon={app}\aion.exe
UninstallDisplayName=Aion {#AppVersion}

[Files]
Source: "..\target\release\aion.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\LICENSE.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\README.md"; DestDir: "{app}"; Flags: ignoreversion

[Tasks]
Name: "addtopath"; Description: "Add Aion to PATH (so that ""aion"" works in any terminal)"; GroupDescription: "Terminal:"

[Run]
; Unpack the C compiler that is inside aion.exe and build its C library, so that the first program does not wait.
Filename: "{app}\aion.exe"; Parameters: "warmup"; StatusMsg: "Preparing the C compiler (about a minute, once)..."; \
    Flags: runhidden waituntilterminated

[UninstallDelete]
; What aion.exe unpacked and built (the C compiler, its library, built programs) lives here.
Type: filesandordirs; Name: "{localappdata}\aion"

[Registry]
; The user's PATH, or the machine's when installing for all users.
Root: HKCU; Subkey: "Environment"; ValueType: expandsz; ValueName: "Path"; ValueData: "{olddata};{app}"; \
    Tasks: addtopath; Check: (not IsAdminInstallMode) and NeedsAddPath(ExpandConstant('{app}'))
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: expandsz; ValueName: "Path"; \
    ValueData: "{olddata};{app}"; Tasks: addtopath; Check: IsAdminInstallMode and NeedsAddPath(ExpandConstant('{app}'))

[Code]
const
  UserEnvironment = 'Environment';
  MachineEnvironment = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';

function EnvironmentKey(): String;
begin
  if IsAdminInstallMode then Result := MachineEnvironment else Result := UserEnvironment;
end;

function EnvironmentRoot(): Integer;
begin
  if IsAdminInstallMode then Result := HKLM else Result := HKCU;
end;

{ Is `Dir` missing from PATH?  (Compared without regard to case, between the semicolons.) }
function NeedsAddPath(Dir: String): Boolean;
var
  Path: String;
begin
  if not RegQueryStringValue(EnvironmentRoot(), EnvironmentKey(), 'Path', Path) then
  begin
    Result := True;
    exit;
  end;
  Result := Pos(';' + Lowercase(Dir) + ';', ';' + Lowercase(Path) + ';') = 0;
end;

{ Take the install folder out of PATH again when Aion is uninstalled. }
procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Path, Dir: String;
begin
  if CurUninstallStep <> usPostUninstall then exit;
  Dir := ExpandConstant('{app}');
  if not RegQueryStringValue(EnvironmentRoot(), EnvironmentKey(), 'Path', Path) then exit;
  StringChangeEx(Path, ';' + Dir, '', True);
  StringChangeEx(Path, Dir + ';', '', True);
  if CompareText(Path, Dir) = 0 then Path := '';
  RegWriteExpandStringValue(EnvironmentRoot(), EnvironmentKey(), 'Path', Path);
end;
