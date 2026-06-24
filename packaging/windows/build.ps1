<#
.SYNOPSIS
  Build the paalam Windows installer (setup.exe).

.DESCRIPTION
  Publish each RID self-contained and generate a per-user installer with Inno Setup (ISCC).
  Output is dist\paalam-setup-<arch>.exe.

  Policy (#11): per-user / no admin required / win-x64 + win-arm64 both /
  file association is not done by the installer (left to the in-app button).

.PARAMETER Arch
  Build target. x64 / arm64 / both (default: both).

.PARAMETER Config
  Build configuration (default: Release).

.EXAMPLE
  pwsh -File packaging\windows\build.ps1                 # both x64 + arm64
  pwsh -File packaging\windows\build.ps1 -Arch x64       # x64 only
#>
[CmdletBinding()]
param(
  [ValidateSet('x64', 'arm64', 'both')]
  [string]$Arch = 'both',
  [string]$Config = 'Release'
)

$ErrorActionPreference = 'Stop'

# Go to the repo root (two levels above this script).
$RepoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
Set-Location $RepoRoot

# Find ISCC.exe (winget may install it per-user).
function Find-ISCC {
  $cmd = Get-Command iscc.exe -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }
  $candidates = @(
    "$env:LOCALAPPDATA\Programs\Inno Setup 6\ISCC.exe",
    "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe",
    "$env:ProgramFiles\Inno Setup 6\ISCC.exe"
  )
  foreach ($c in $candidates) { if (Test-Path $c) { return $c } }
  throw "ISCC.exe not found. Install Inno Setup 6: winget install --id JRSoftware.InnoSetup"
}

$ISCC = Find-ISCC
Write-Host "==> ISCC: $ISCC"

$archs = if ($Arch -eq 'both') { @('x64', 'arm64') } else { @($Arch) }

foreach ($a in $archs) {
  $rid = "win-$a"
  $publishDir = "dist\publish-$rid"

  Write-Host "`n==> publish (self-contained) $rid"
  # self-contained: bundle the runtime so it starts even without .NET 10 installed for the user.
  & dotnet publish -c $Config -r $rid --self-contained true -o $publishDir
  if ($LASTEXITCODE -ne 0) { throw "dotnet publish failed ($rid)" }

  Write-Host "==> ISCC $a"
  & $ISCC "/DArch=$a" "packaging\windows\paalam.iss"
  if ($LASTEXITCODE -ne 0) { throw "ISCC failed ($a)" }

  Write-Host "==> done: dist\paalam-setup-$a.exe"
}

Write-Host "`nAll architectures done."
Get-ChildItem dist\paalam-setup-*.exe | ForEach-Object {
  "  {0}  ({1:N1} MB)" -f $_.Name, ($_.Length / 1MB)
}
