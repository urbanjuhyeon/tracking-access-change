$ErrorActionPreference = "Stop"

$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = Resolve-Path (Join-Path $scriptRoot "..")
$docsRoot = Join-Path $repoRoot "docs"
$outputRoot = Join-Path $docsRoot "_book-anonymous"

Push-Location $repoRoot
try {
    & quarto render $docsRoot --profile anonymous --to html
    if ($LASTEXITCODE -ne 0) {
        throw "Quarto render failed with exit code $LASTEXITCODE."
    }
}
finally {
    Pop-Location
}

if (-not (Test-Path -LiteralPath (Join-Path $outputRoot "index.html"))) {
    throw "Anonymous site output was not created at $outputRoot."
}

Copy-Item -LiteralPath (Join-Path $scriptRoot "robots.txt") `
    -Destination (Join-Path $outputRoot "robots.txt") -Force
Copy-Item -LiteralPath (Join-Path $scriptRoot "_headers") `
    -Destination (Join-Path $outputRoot "_headers") -Force

$identityPatterns = @(
    "Juhyeon Park",
    "urbanjuhyeon",
    "juhyeonpark.com",
    "github.com/urbanjuhyeon",
    "C:\Users\urban",
    "file:///",
    "tracking-access-change"
)

$textFiles = Get-ChildItem -LiteralPath $outputRoot -Recurse -File |
    Where-Object { $_.Extension -in @(".html", ".json", ".xml", ".txt") -or $_.Name -eq "_headers" }

$identityHits = $textFiles | Select-String -SimpleMatch -Pattern $identityPatterns
if ($identityHits) {
    $details = $identityHits |
        ForEach-Object { "$($_.Path):$($_.LineNumber): $($_.Line.Trim())" } |
        Out-String
    throw "Identity markers remain in the anonymous output:`n$details"
}

$indexHtml = Get-Content -Raw (Join-Path $outputRoot "index.html")
if ($indexHtml -notmatch '<meta name="author" content="Anonymous">') {
    throw "Anonymous author metadata is missing from index.html."
}
if ($indexHtml -notmatch 'noindex, nofollow, noarchive, nosnippet') {
    throw "Search-engine exclusion metadata is missing from index.html."
}

$binaryIdentityPatterns = @(
    "Juhyeon",
    "urbanjuhyeon",
    "juhyeonpark"
)
$metadataChunkTypes = @("tEXt", "zTXt", "iTXt", "eXIf")
$imageProblems = @()

Get-ChildItem -LiteralPath $outputRoot -Recurse -Filter "*.png" -File |
    ForEach-Object {
        $bytes = [System.IO.File]::ReadAllBytes($_.FullName)
        $ascii = [System.Text.Encoding]::ASCII.GetString($bytes)
        foreach ($marker in $binaryIdentityPatterns) {
            if ($ascii.IndexOf($marker, [System.StringComparison]::OrdinalIgnoreCase) -ge 0) {
                $imageProblems += "$($_.FullName): identity marker '$marker'"
            }
        }

        $offset = 8
        while ($offset + 12 -le $bytes.Length) {
            $lengthBytes = [byte[]]$bytes[$offset..($offset + 3)]
            [Array]::Reverse($lengthBytes)
            $length = [BitConverter]::ToUInt32($lengthBytes, 0)
            $type = [System.Text.Encoding]::ASCII.GetString($bytes, $offset + 4, 4)
            if ($type -in $metadataChunkTypes) {
                $imageProblems += "$($_.FullName): PNG metadata chunk '$type'"
            }
            $offset += 12 + $length
            if ($type -eq "IEND") {
                break
            }
        }
    }

if ($imageProblems) {
    throw "Image metadata or identity markers remain in the anonymous output:`n$($imageProblems -join "`n")"
}

Write-Output "Anonymous site built and identity scan passed: $outputRoot"
