; paalam — Windows インストーラ (Inno Setup 6)
;
; 方針 (#11 / docs FINDINGS の配布方針):
;   - per-user インストール ({localappdata}\Programs\paalam)。管理者権限不要。
;   - スタートメニューのショートカット + アンインストーラ。
;   - win-x64 / win-arm64 の 2 アーキテクチャを別 setup.exe で配布。
;   - 中身は self-contained publish の出力 (.NET ランタイム同梱、環境非依存で起動)。
;   - ファイル関連付けはインストーラでは行わない。
;     paalam はアプリ内に per-user 関連付けボタンを持つので、そちらに任せる。
;
; コンパイルは `build.ps1` 経由が前提 (publish→ISCC を自動化)。手動なら:
;   ISCC.exe /DArch=x64 packaging\windows\paalam.iss
; 事前に self-contained publish が dist\publish-win-<Arch>\ に出ている必要がある。

#ifndef Arch
  #define Arch "x64"          ; build.ps1 が /DArch=... で上書きする
#endif

; publish 出力ディレクトリ (この .iss からの相対)。build.ps1 が /DSourceDir で上書き可。
#ifndef SourceDir
  #define SourceDir "..\..\dist\publish-win-" + Arch
#endif

#define MyAppName "paalam"
#define MyAppPublisher "SANO Masatoshi"
#define MyAppURL "https://github.com/snmsts/paalam"
#define MyAppExe "paalam.exe"

; apphost からバージョンを拾う (publish 済みの exe が存在する前提)。
#define MyAppVersion GetVersionNumbersString(SourceDir + "\" + MyAppExe)

[Setup]
; AppId は固定。アップグレード/アンインストールの同一性に使うので変えないこと。
AppId={{8F3A1C2E-5B7D-4E9A-9C1F-2A6B8D4E0F71}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}/issues
AppUpdatesURL={#MyAppURL}/releases

; per-user: 管理者不要。{autopf} は lowest 権限では {localappdata}\Programs に解決される。
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
AllowNoIcons=yes

; アーキテクチャ制限。x64 ビルドは x64compatible (arm64 上のエミュレーションも可) に、
; arm64 ビルドは arm64 ネイティブ機にのみ。
#if Arch == "arm64"
  ArchitecturesAllowed=arm64
  ArchitecturesInstallIn64BitMode=arm64
#else
  ArchitecturesAllowed=x64compatible
  ArchitecturesInstallIn64BitMode=x64compatible
#endif

; インストーラ/アンインストーラの見た目。
SetupIconFile=..\..\app.ico
UninstallDisplayIcon={app}\{#MyAppExe}
WizardStyle=modern
LicenseFile=..\..\LICENSE
Compression=lzma2/max
SolidCompression=yes

; 出力 (dist\paalam-setup-<Arch>.exe)。
OutputDir=..\..\dist
OutputBaseFilename=paalam-setup-{#Arch}

[Languages]
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "english";  MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
; publish 出力一式 (apphost / dll / native / dotcl-fasl / contrib) を丸ごと配置。
Source: "{#SourceDir}\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs ignoreversion

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExe}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExe}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExe}"; Description: "{cm:LaunchProgram,{#MyAppName}}"; Flags: nowait postinstall skipifsilent
