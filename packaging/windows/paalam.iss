; paalam — Windows installer (Inno Setup 6)
;
; Policy (#11 / distribution policy in docs FINDINGS):
;   - per-user install ({localappdata}\Programs\paalam). No administrator rights needed.
;   - Start menu shortcut + uninstaller.
;   - Ship win-x64 / win-arm64 as two separate setup.exe per architecture.
;   - Contents are the self-contained publish output (.NET runtime bundled, env-independent startup).
;   - File association is NOT done by the installer.
;     paalam has an in-app per-user association button, so leave it to that.
;
; Compilation is expected via `build.ps1` (automates publish->ISCC). Manually:
;   ISCC.exe /DArch=x64 packaging\windows\paalam.iss
; A self-contained publish must already exist in dist\publish-win-<Arch>\.

#ifndef Arch
  #define Arch "x64"          ; build.ps1 overrides this via /DArch=...
#endif

; publish output dir (relative to this .iss). build.ps1 may override via /DSourceDir.
#ifndef SourceDir
  #define SourceDir "..\..\dist\publish-win-" + Arch
#endif

#define MyAppName "paalam"
#define MyAppPublisher "SANO Masatoshi"
#define MyAppURL "https://github.com/snmsts/paalam"
#define MyAppExe "paalam.exe"

; Pull the version from the apphost (assumes the published exe exists).
#define MyAppVersion GetVersionNumbersString(SourceDir + "\" + MyAppExe)

[Setup]
; AppId is fixed. Used for upgrade/uninstall identity, so never change it.
AppId={{8F3A1C2E-5B7D-4E9A-9C1F-2A6B8D4E0F71}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}/issues
AppUpdatesURL={#MyAppURL}/releases

; per-user: no admin needed. Under lowest privileges {autopf} resolves to {localappdata}\Programs.
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
AllowNoIcons=yes

; Architecture restriction. x64 build -> x64compatible (also allows emulation on arm64),
; arm64 build -> arm64 native machines only.
#if Arch == "arm64"
  ArchitecturesAllowed=arm64
  ArchitecturesInstallIn64BitMode=arm64
#else
  ArchitecturesAllowed=x64compatible
  ArchitecturesInstallIn64BitMode=x64compatible
#endif

; Installer/uninstaller appearance.
SetupIconFile=..\..\app.ico
UninstallDisplayIcon={app}\{#MyAppExe}
WizardStyle=modern
LicenseFile=..\..\LICENSE
Compression=lzma2/max
SolidCompression=yes

; Output (dist\paalam-setup-<Arch>.exe).
OutputDir=..\..\dist
OutputBaseFilename=paalam-setup-{#Arch}

[Languages]
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "english";  MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
; Lay down the entire publish output (apphost / dll / native / dotcl-fasl / contrib).
Source: "{#SourceDir}\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExe}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExe}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExe}"; Description: "{cm:LaunchProgram,{#MyAppName}}"; Flags: nowait postinstall skipifsilent
