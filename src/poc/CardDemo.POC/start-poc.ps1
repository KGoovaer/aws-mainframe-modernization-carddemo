# Quick Start Script for CardDemo POC
# This script builds, tests, and runs the POC application

Write-Host "======================================" -ForegroundColor Cyan
Write-Host "CardDemo POC - Quick Start" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""

# Navigate to POC directory
$pocPath = "c:\Users\Jeroen.haegebaert\Source\Repos\ae-nv\aws-mainframe-modernization-carddemo\src\poc\CardDemo.POC"
Set-Location $pocPath

Write-Host "Step 1: Restoring dependencies..." -ForegroundColor Yellow
dotnet restore
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to restore dependencies" -ForegroundColor Red
    exit 1
}
Write-Host "✓ Dependencies restored" -ForegroundColor Green
Write-Host ""

Write-Host "Step 2: Building solution..." -ForegroundColor Yellow
dotnet build --no-restore
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Build failed" -ForegroundColor Red
    exit 1
}
Write-Host "✓ Build successful" -ForegroundColor Green
Write-Host ""

Write-Host "Step 3: Running tests..." -ForegroundColor Yellow
dotnet test --no-build --verbosity quiet
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Tests failed" -ForegroundColor Red
    exit 1
}
Write-Host "✓ All tests passed" -ForegroundColor Green
Write-Host ""

Write-Host "======================================" -ForegroundColor Cyan
Write-Host "✓ POC is ready!" -ForegroundColor Green
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "To start the application, run:" -ForegroundColor Yellow
Write-Host "  cd CardDemo.POC.Web" -ForegroundColor White
Write-Host "  dotnet run" -ForegroundColor White
Write-Host ""
Write-Host "Once running, access:" -ForegroundColor Yellow
Write-Host "  • Blazor UI:  http://localhost:5000" -ForegroundColor White
Write-Host "  • Swagger:    http://localhost:5000/swagger" -ForegroundColor White
Write-Host ""
