<#
.SYNOPSIS
  paalam の Windows インストーラ (setup.exe) をビルドする。

.DESCRIPTION
  各 RID を self-contained で publish し、Inno Setup (ISCC) で per-user
  インストーラを生成する。出力は dist\paalam-setup-<arch>.exe。

  方針 (#11): per-user / 管理者不要 / win-x64 + win-arm64 両対応 /
  ファイル関連付けはインストーラでは行わない (アプリ内ボタン任せ)。

.PARAMETER Arch
  ビルド対象。x64 / arm64 / both (既定: both)。

.PARAMETER Config
  ビルド構成 (既定: Release)。

.EXAMPLE
  pwsh -File packaging\windows\build.ps1                 # x64 + arm64 両方
  pwsh -File packaging\windows\build.ps1 -Arch x64       # x64 のみ
#>
[CmdletBinding()]
param(
  [ValidateSet('x64', 'arm64', 'both')]
  [string]$Arch = 'both',
  [string]$Config = 'Release'
)

$ErrorActionPreference = 'Stop'

# リポジトリルート (このスクリプトの 2 つ上) へ。
$RepoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
Set-Location $RepoRoot

# ISCC.exe を探す (winget は per-user に入れることがある)。
function Find-ISCC {
  $cmd = Get-Command iscc.exe -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }
  $candidates = @(
    "$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe",
    "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe",
    "$env:ProgramFiles\Inno Setup 6\ISCC.exe"
  )
  foreach ($c in $candidates) { if (Test-Path $c) { return $c } }
  throw "ISCC.exe が見つかりません。Inno Setup 6 を入れてください: winget install --id JRSoftware.InnoSetup"
}

$ISCC = Find-ISCC
Write-Host "==> ISCC: $ISCC"

$archs = if ($Arch -eq 'both') { @('x64', 'arm64') } else { @($Arch) }

foreach ($a in $archs) {
  $rid = "win-$a"
  $publishDir = "dist\publish-$rid"

  Write-Host "`n==> publish (self-contained) $rid"
  # self-contained: 一般ユーザに .NET 10 ランタイムが無くても起動できるよう同梱する。
  & dotnet publish -c $Config -r $rid --self-contained true -o $publishDir
  if ($LASTEXITCODE -ne 0) { throw "dotnet publish 失敗 ($rid)" }

  Write-Host "==> ISCC $a"
  & $ISCC "/DArch=$a" "packaging\windows\paalam.iss"
  if ($LASTEXITCODE -ne 0) { throw "ISCC 失敗 ($a)" }

  Write-Host "==> 完成: dist\paalam-setup-$a.exe"
}

Write-Host "`n全アーキテクチャ完了。"
Get-ChildItem dist\paalam-setup-*.exe | ForEach-Object {
  "  {0}  ({1:N1} MB)" -f $_.Name, ($_.Length / 1MB)
}
