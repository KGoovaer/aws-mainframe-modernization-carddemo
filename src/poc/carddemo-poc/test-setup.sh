#!/bin/bash

# Test Backend and Frontend Separately

POC_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "=========================================="
echo "CardDemo POC - Diagnostic Test"
echo "=========================================="
echo ""

# Test 1: Check Java
echo "1. Checking Java..."
java --version
echo ""

# Test 2: Check Maven Wrapper
echo "2. Checking Maven Wrapper..."
if [ -f "./mvnw" ]; then
    echo "✅ mvnw exists"
    ./mvnw --version | head -3
else
    echo "❌ mvnw NOT found"
fi
echo ""

# Test 3: Check Node/npm
echo "3. Checking Node and npm..."
node --version
npm --version
echo ""

# Test 4: Check frontend directory
echo "4. Checking frontend directory..."
if [ -d "./frontend" ]; then
    echo "✅ frontend directory exists"
    ls -la frontend/ | grep -E "package.json|angular.json|src"
else
    echo "❌ frontend directory NOT found"
fi
echo ""

# Test 5: Try installing frontend dependencies
echo "5. Installing frontend dependencies..."
cd frontend
npm install --prefer-offline
INSTALL_STATUS=$?
cd ..

if [ $INSTALL_STATUS -eq 0 ]; then
    echo "✅ npm install successful"
else
    echo "❌ npm install failed with status: $INSTALL_STATUS"
fi
echo ""

# Test 6: Check if node_modules was created
echo "6. Checking if node_modules was created..."
if [ -d "./frontend/node_modules" ]; then
    echo "✅ node_modules directory created"
    echo "   Size: $(du -sh frontend/node_modules 2>/dev/null | cut -f1)"
else
    echo "❌ node_modules NOT created"
fi
echo ""

echo "=========================================="
echo "Diagnostic complete"
echo "=========================================="
