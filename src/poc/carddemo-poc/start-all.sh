#!/bin/bash

# Complete POC Startup Script
# Starts both backend and frontend in separate terminal windows/tabs

POC_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "=========================================="
echo "CardDemo POC - Complete Startup"
echo "=========================================="
echo ""
echo "This script will start:"
echo "  1. Backend (Spring Boot) on http://localhost:8080"
echo "  2. Frontend (Angular) on http://localhost:4200"
echo ""
echo "Press Ctrl+C to stop"
echo "=========================================="
echo ""

# Check if running on macOS
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "🍎 macOS detected - Opening separate Terminal windows..."
    
    # Start backend in new terminal
    osascript -e "tell application \"Terminal\"
        do script \"cd '$POC_DIR' && echo '🚀 Starting Backend...' && ./start-poc.sh\"
    end tell"
    
    # Wait a bit for backend to start
    sleep 3
    
    # Start frontend in new terminal
    osascript -e "tell application \"Terminal\"
        do script \"cd '$POC_DIR' && echo '🎨 Starting Frontend...' && ./start-frontend.sh\"
    end tell"
    
    echo ""
    echo "✅ Both services starting in separate Terminal windows"
    echo ""
    echo "Waiting for services to start..."
    sleep 5
    
    echo ""
    echo "Services should now be starting. Check the new Terminal windows."
    echo ""
    echo "Access the application:"
    echo "  Frontend: http://localhost:4200 (may take 30-60 seconds)"
    echo "  Backend API: http://localhost:8080/api (should be ready in ~10 seconds)"
    echo "  H2 Console: http://localhost:8080/h2-console"
    echo ""
    echo "Default credentials: ADMIN01 / ADMIN01"
    echo ""
    echo "To stop: Close the Terminal windows or press Ctrl+C in each window"
    echo ""
    echo "💡 Tip: Wait ~60 seconds for both services to fully start before accessing the frontend."
    
else
    # Linux or other OS - start in background
    echo "🐧 Linux/Unix detected - Starting services..."
    
    # Start backend in background
    cd "$POC_DIR"
    ./start-poc.sh > backend.log 2>&1 &
    BACKEND_PID=$!
    echo "Backend started with PID: $BACKEND_PID"
    
    # Wait for backend
    sleep 5
    
    # Start frontend in background
    ./start-frontend.sh > frontend.log 2>&1 &
    FRONTEND_PID=$!
    echo "Frontend started with PID: $FRONTEND_PID"
    
    echo ""
    echo "✅ Both services started"
    echo ""
    echo "Access the application:"
    echo "  Frontend: http://localhost:4200"
    echo "  Backend API: http://localhost:8080/api"
    echo "  H2 Console: http://localhost:8080/h2-console"
    echo ""
    echo "Default credentials: ADMIN01 / ADMIN01"
    echo ""
    echo "Logs:"
    echo "  Backend: $POC_DIR/backend.log"
    echo "  Frontend: $POC_DIR/frontend.log"
    echo ""
    echo "To stop services:"
    echo "  kill $BACKEND_PID $FRONTEND_PID"
fi
