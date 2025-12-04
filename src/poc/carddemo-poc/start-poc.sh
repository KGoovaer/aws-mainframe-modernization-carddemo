#!/bin/bash

# Start CardDemo POC backend
# This script builds and runs the Spring Boot application

echo "========================================="
echo "CardDemo POC - Starting Backend"
echo "========================================="
echo ""

# Navigate to POC directory
cd "$(dirname "$0")"

# Check if Java is installed
if ! command -v java &> /dev/null; then
    echo "ERROR: Java 21 is required but not found."
    echo "Please install Java 21 or higher."
    exit 1
fi

# Check Java version
JAVA_VERSION=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}' | cut -d'.' -f1)
if [ "$JAVA_VERSION" -lt 21 ]; then
    echo "ERROR: Java 21 or higher is required. Found Java $JAVA_VERSION"
    exit 1
fi

echo "✓ Java $JAVA_VERSION detected"
echo ""

echo "✓ Using Maven Wrapper (no Maven installation needed)"
echo ""

# Clean and run
echo "Building and starting Spring Boot application..."
echo "This may take a moment on first run (Maven Wrapper will download Maven if needed)..."
echo ""

./mvnw spring-boot:run

# If the above fails, try building first
if [ $? -ne 0 ]; then
    echo ""
    echo "Build failed. Trying clean build..."
    ./mvnw clean package
    if [ $? -eq 0 ]; then
        echo ""
        echo "Running application..."
        java -jar target/carddemo-poc-1.0.0-SNAPSHOT.jar
    fi
fi
