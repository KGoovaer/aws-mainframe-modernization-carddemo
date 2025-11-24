#!/bin/bash

# Start Angular Development Server

cd "$(dirname "$0")/frontend"

echo "=========================================="
echo "CardDemo POC - Starting Angular Frontend"
echo "=========================================="
echo ""

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
    echo "📦 Installing dependencies..."
    npm install
    echo ""
fi

echo "🚀 Starting development server..."
echo ""
echo "Frontend will be available at: http://localhost:4200"
echo "Backend API should be running at: http://localhost:8080"
echo ""
echo "Default credentials:"
echo "  User ID:  ADMIN01"
echo "  Password: ADMIN01"
echo ""
echo "Press Ctrl+C to stop the server"
echo "=========================================="
echo ""

npm start
