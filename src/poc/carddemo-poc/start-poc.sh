#!/bin/bash

# Start CardDemo POC backend
# This script builds and runs the Spring Boot application

echo "========================================="
echo "CardDemo POC - Starting Backend"
echo "========================================="
echo ""

# Navigate to POC directory
cd "$(dirname "$0")"

# Check if JAVA_HOME is set and points to Java 21
if [ -n "$JAVA_HOME" ]; then
    JAVA_CMD="$JAVA_HOME/bin/java"
    if [ -f "$JAVA_CMD" ] || [ -f "$JAVA_CMD.exe" ]; then
        echo "Using JAVA_HOME: $JAVA_HOME"
        JAVA_VERSION=$("$JAVA_CMD" -version 2>&1 | awk -F '"' '/version/ {print $2}' | cut -d'.' -f1)
        
        if [ "$JAVA_VERSION" -eq 21 ]; then
            echo "✓ Java 21 detected from JAVA_HOME"
            export PATH="$JAVA_HOME/bin:$PATH"
        else
            echo "WARNING: JAVA_HOME points to Java $JAVA_VERSION, but Java 21 is required."
            echo "Looking for Java 21 in PATH..."
            JAVA_CMD="java"
        fi
    else
        echo "WARNING: JAVA_HOME is set but Java not found at $JAVA_HOME"
        echo "Looking for Java 21 in PATH..."
        JAVA_CMD="java"
    fi
else
    JAVA_CMD="java"
fi

# Check if Java is installed
if ! command -v "$JAVA_CMD" &> /dev/null; then
    echo "ERROR: Java 21 is required but not found."
    echo "Please install Java 21 and set JAVA_HOME environment variable."
    echo "Example: export JAVA_HOME=/path/to/jdk-21"
    exit 1
fi

# Check Java version
JAVA_VERSION=$("$JAVA_CMD" -version 2>&1 | awk -F '"' '/version/ {print $2}' | cut -d'.' -f1)
if [ "$JAVA_VERSION" -ne 21 ]; then
    echo "ERROR: Java 21 is required. Found Java $JAVA_VERSION"
    echo ""
    echo "Please ensure Java 21 is installed and set JAVA_HOME:"
    echo "  export JAVA_HOME=/path/to/jdk-21"
    echo "  export PATH=\$JAVA_HOME/bin:\$PATH"
    exit 1
fi

echo "✓ Java 21 detected"
echo ""

echo "✓ Using Maven Wrapper (no Maven installation needed)"
echo ""

# Detect if running on Windows - Git Bash handles ./mvnw correctly
MVN_CMD="./mvnw"
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" || "$OSTYPE" == "cygwin" ]] || uname -r | grep -qi microsoft; then
    echo "🪟 Windows detected"
    # Git Bash on Windows: ./mvnw will automatically call mvnw.cmd
    # No need to change the command
fi

# Set JAVA_HOME for Maven if not already set
if [ -n "$JAVA_HOME" ]; then
    export JAVA_HOME
fi

# Clean and run
echo "Building and starting Spring Boot application..."
echo "This may take a moment on first run (Maven Wrapper will download Maven if needed)..."
echo ""

$MVN_CMD spring-boot:run

# If the above fails, try building first
if [ $? -ne 0 ]; then
    echo ""
    echo "Build failed. Trying clean build..."
    $MVN_CMD clean package
    if [ $? -eq 0 ]; then
        echo ""
        echo "Running application..."
        java -jar target/carddemo-poc-1.0.0-SNAPSHOT.jar
    fi
fi
