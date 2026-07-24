param(
    [string]$ProjectRoot = (Get-Location).Path,
    [string]$StageRoot = 'C:\tmp\uad_ttm_stable_stage_20260712',
    [string]$BackupRoot = 'C:\tmp\uad_ttm_numeric_backup_20260712'
)

$ErrorActionPreference = 'Stop'

function Get-NormalizedSha256([string]$Path) {
    (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
}

function Assert-ChildPath([string]$Child, [string]$Parent, [string]$Label) {
    $childFull = [System.IO.Path]::GetFullPath($Child)
    $parentFull = [System.IO.Path]::GetFullPath($Parent).TrimEnd('\') + '\'
    if (-not $childFull.StartsWith($parentFull, [System.StringComparison]::OrdinalIgnoreCase)) {
        throw "$Label is outside its required parent: $childFull"
    }
}

$ProjectRoot = (Resolve-Path -LiteralPath $ProjectRoot).Path
$StageRoot = (Resolve-Path -LiteralPath $StageRoot).Path
$resultsDir = Join-Path $ProjectRoot 'workflows\results'
$routingDir = Join-Path $resultsDir 'decompose\routing'
$reportFile = Join-Path $resultsDir 'id_migration_20260712\ttm_staging_validation.csv'
$sentinelFile = Join-Path $resultsDir 'ID_MIGRATION_IN_PROGRESS'
$completeFile = Join-Path $resultsDir 'ID_SCHEME_LOCATION_V1'
$stageActual = Join-Path $StageRoot 'actual'
$stageCounterfactual = Join-Path $StageRoot 'counterfactual'
$backupActual = Join-Path $BackupRoot 'actual'
$backupRouting = Join-Path $BackupRoot 'routing'

Assert-ChildPath $StageRoot 'C:\tmp' 'StageRoot'
Assert-ChildPath $BackupRoot 'C:\tmp' 'BackupRoot'
Assert-ChildPath $resultsDir $ProjectRoot 'results directory'

if (-not (Test-Path -LiteralPath $reportFile -PathType Leaf)) {
    throw "Validation report not found: $reportFile"
}
if (Test-Path -LiteralPath $BackupRoot) {
    throw "BackupRoot already exists; refusing to mix migrations: $BackupRoot"
}
if ((Test-Path -LiteralPath $sentinelFile) -or
    (Test-Path -LiteralPath $completeFile)) {
    throw 'Migration sentinel or completion marker already exists'
}

$report = @(Import-Csv -LiteralPath $reportFile)
$actualRows = @($report | Where-Object kind -eq 'actual')
$counterfactualRows = @($report | Where-Object kind -eq 'counterfactual')
if ($report.Count -ne 2956 -or $actualRows.Count -ne 16 -or
    $counterfactualRows.Count -ne 2940) {
    throw "Unexpected report inventory: total=$($report.Count), actual=$($actualRows.Count), counterfactual=$($counterfactualRows.Count)"
}
if (@($report | Where-Object { $_.verified -ne 'TRUE' }).Count -ne 0) {
    throw 'Validation report contains an unverified file'
}
if (@($report | Where-Object { $_.raw_n_rows -ne $_.staged_n_rows }).Count -ne 0) {
    throw 'Validation report contains a row-count mismatch'
}

$activeRoutingItems = @(Get-ChildItem -LiteralPath $routingDir -Force)
$stageRoutingItems = @(Get-ChildItem -LiteralPath $stageCounterfactual -Force)
if ($activeRoutingItems.Count -ne 2940 -or
    @($activeRoutingItems | Where-Object { $_.PSIsContainer -or $_.Extension -ne '.parquet' }).Count -ne 0) {
    throw 'Active counterfactual routing directory is not exactly 2,940 Parquet files'
}
if ($stageRoutingItems.Count -ne 2940 -or
    @($stageRoutingItems | Where-Object { $_.PSIsContainer -or $_.Extension -ne '.parquet' }).Count -ne 0) {
    throw 'Staged counterfactual directory is not exactly 2,940 Parquet files'
}
if (@(Get-ChildItem -LiteralPath $stageActual -File).Count -ne 16) {
    throw 'Staged actual directory is not exactly 16 files'
}

Write-Host 'Verifying all original and staged SHA-256 values before promotion...'
foreach ($row in $report) {
    $activeFile = Join-Path $ProjectRoot $row.input_file
    $stageSubdir = if ($row.kind -eq 'actual') { 'actual' } else { 'counterfactual' }
    $stagedFile = Join-Path (Join-Path $StageRoot $stageSubdir) ([IO.Path]::GetFileName($row.input_file))
    if (-not (Test-Path -LiteralPath $activeFile -PathType Leaf) -or
        -not (Test-Path -LiteralPath $stagedFile -PathType Leaf)) {
        throw "Missing active or staged file for $($row.input_file)"
    }
    if ((Get-NormalizedSha256 $activeFile) -ne $row.input_sha256.ToLowerInvariant()) {
        throw "Original SHA-256 changed: $activeFile"
    }
    if ((Get-NormalizedSha256 $stagedFile) -ne $row.staged_sha256.ToLowerInvariant()) {
        throw "Staged SHA-256 changed: $stagedFile"
    }
}

New-Item -ItemType Directory -Path $backupActual -Force | Out-Null
Set-Content -LiteralPath $sentinelFile -Value "started=$(Get-Date -Format o)" -Encoding utf8NoBOM

try {
    foreach ($row in $actualRows) {
        $activeFile = Join-Path $ProjectRoot $row.input_file
        Move-Item -LiteralPath $activeFile -Destination $backupActual
    }
    Move-Item -LiteralPath $routingDir -Destination $backupRouting

    foreach ($row in $actualRows) {
        $filename = [IO.Path]::GetFileName($row.input_file)
        Move-Item -LiteralPath (Join-Path $stageActual $filename) -Destination $resultsDir
    }
    Move-Item -LiteralPath $stageCounterfactual -Destination $routingDir

    Write-Host 'Verifying promoted SHA-256 values at active paths...'
    foreach ($row in $report) {
        $activeFile = Join-Path $ProjectRoot $row.input_file
        if ((Get-NormalizedSha256 $activeFile) -ne $row.staged_sha256.ToLowerInvariant()) {
            throw "Promoted SHA-256 mismatch: $activeFile"
        }
    }

    @(
        'scheme=location_id_v1'
        "completed=$(Get-Date -Format o)"
        'actual_files=16'
        'counterfactual_files=2940'
        'validation_report=workflows/results/id_migration_20260712/ttm_staging_validation.csv'
        "backup=$BackupRoot"
    ) | Set-Content -LiteralPath $completeFile -Encoding utf8NoBOM
    Remove-Item -LiteralPath $sentinelFile -Force
    Write-Host "Promotion complete. Backup retained at: $BackupRoot"
}
catch {
    Write-Error "Promotion failed; attempting rollback: $($_.Exception.Message)"

    if (Test-Path -LiteralPath $routingDir -PathType Container) {
        if (-not (Test-Path -LiteralPath $stageCounterfactual)) {
            Move-Item -LiteralPath $routingDir -Destination $stageCounterfactual
        }
    }
    if ((Test-Path -LiteralPath $backupRouting -PathType Container) -and
        -not (Test-Path -LiteralPath $routingDir)) {
        Move-Item -LiteralPath $backupRouting -Destination $routingDir
    }

    foreach ($row in $actualRows) {
        $filename = [IO.Path]::GetFileName($row.input_file)
        $activeFile = Join-Path $resultsDir $filename
        $stagedFile = Join-Path $stageActual $filename
        $backupFile = Join-Path $backupActual $filename
        if ((Test-Path -LiteralPath $activeFile -PathType Leaf) -and
            -not (Test-Path -LiteralPath $stagedFile)) {
            Move-Item -LiteralPath $activeFile -Destination $stageActual
        }
        if ((Test-Path -LiteralPath $backupFile -PathType Leaf) -and
            -not (Test-Path -LiteralPath $activeFile)) {
            Move-Item -LiteralPath $backupFile -Destination $resultsDir
        }
    }
    throw
}
