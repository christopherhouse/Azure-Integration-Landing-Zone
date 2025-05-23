#!/bin/bash

# Azure Integration Landing Zone COBOL Test Runner
# This script compiles and runs all COBOL unit tests

echo "Azure Integration Landing Zone COBOL Test Suite"
echo "==============================================="

# Set up environment
COBOL_DIR="/home/runner/work/Azure-Integration-Landing-Zone/Azure-Integration-Landing-Zone/infra/cobol"
SRC_DIR="$COBOL_DIR/src"
TEST_DIR="$COBOL_DIR/tests"
COPYBOOK_DIR="$COBOL_DIR/copybooks"

# Create output directory for compiled programs
mkdir -p "$COBOL_DIR/bin"

# Track script status
INSTALLATION_SUCCESS=true
COMPILATION_SUCCESS=true
TEST_RUN_SUCCESS=true

echo "Checking for COBOL compiler..."
if ! command -v cobc >/dev/null 2>&1; then
    echo "GnuCOBOL compiler not found. Installing gnucobol..."
    
    # Try to install GnuCOBOL without using sudo first
    if apt-get update && apt-get install -y gnucobol; then
        echo "GnuCOBOL successfully installed without sudo"
    else
        # Try using sudo if direct installation failed
        if sudo apt-get update && sudo apt-get install -y gnucobol; then
            echo "GnuCOBOL successfully installed with sudo"
        else
            echo "Failed to install GnuCOBOL automatically"
            echo "Please install manually: sudo apt-get install gnucobol"
            INSTALLATION_SUCCESS=false
        fi
    fi
    
    # Verify installation succeeded
    if ! command -v cobc >/dev/null 2>&1; then
        echo "GnuCOBOL installation verification failed!"
        INSTALLATION_SUCCESS=false
    else
        echo "GnuCOBOL installation verified successfully"
    fi
else
    echo "GnuCOBOL compiler found"
fi

# Only proceed with compilation if installation succeeded
if [ "$INSTALLATION_SUCCESS" = true ]; then
    echo ""
    echo "Compiling COBOL programs..."
    echo "=========================="
    
    # Compile test programs
    echo "Compiling TESTAUTH..."
    compilation_output=$(cobc -x -I "$COPYBOOK_DIR" -o "$COBOL_DIR/bin/testauth" "$TEST_DIR/TESTAUTH.cbl" 2>&1)
    if [ $? -eq 0 ]; then
        echo "✓ TESTAUTH compiled successfully"
    else
        echo "✗ TESTAUTH compilation failed"
        echo "Error details:"
        echo "$compilation_output"
        COMPILATION_SUCCESS=false
    fi
    
    echo ""
    echo "Compiling TESTSTORAGE..."
    compilation_output=$(cobc -x -I "$COPYBOOK_DIR" -o "$COBOL_DIR/bin/teststorage" "$TEST_DIR/TESTSTORAGE.cbl" 2>&1)
    if [ $? -eq 0 ]; then
        echo "✓ TESTSTORAGE compiled successfully"
    else
        echo "✗ TESTSTORAGE compilation failed"
        echo "Error details:"
        echo "$compilation_output"
        COMPILATION_SUCCESS=false
    fi
    
    # Show syntax help if compilation failed
    if [ "$COMPILATION_SUCCESS" = false ]; then
        echo ""
        echo "Common COBOL Syntax Issues:"
        echo "  - Missing or extra periods"
        echo "  - Incorrect indentation"
        echo "  - Reserved words used as identifiers (like ENVIRONMENT)"
        echo "  - Missing/invalid division or section headers"
        echo "  - Copybooks missing proper termination with newlines"
        echo ""
        echo "To fix these issues, review the error messages and update the corresponding COBOL files."
    fi
    
    # Only run tests if compilation succeeded
    if [ "$COMPILATION_SUCCESS" = true ]; then
        echo ""
        echo "Running tests..."
        echo "=================="
        
        if [ -f "$COBOL_DIR/bin/testauth" ]; then
            echo "Running TESTAUTH..."
            cd "$COBOL_DIR"
            ./bin/testauth
            if [ $? -ne 0 ]; then
                TEST_RUN_SUCCESS=false
                echo "✗ TESTAUTH failed with exit code $?"
            fi
            echo ""
        fi
        
        if [ -f "$COBOL_DIR/bin/teststorage" ]; then
            echo "Running TESTSTORAGE..."
            cd "$COBOL_DIR"
            ./bin/teststorage
            if [ $? -ne 0 ]; then
                TEST_RUN_SUCCESS=false
                echo "✗ TESTSTORAGE failed with exit code $?"
            fi
            echo ""
        fi
    fi
fi

# Summary
echo ""
echo "Test Suite Summary"
echo "================="
echo "GnuCOBOL Installation: $([ "$INSTALLATION_SUCCESS" = true ] && echo "SUCCESS" || echo "FAILED")"
echo "COBOL Compilation: $([ "$COMPILATION_SUCCESS" = true ] && echo "SUCCESS" || echo "FAILED")"

if [ "$COMPILATION_SUCCESS" = true ]; then
    echo "Test Execution: $([ "$TEST_RUN_SUCCESS" = true ] && echo "SUCCESS" || echo "FAILED")"
else
    echo "Test Execution: SKIPPED (compilation failed)"
fi

echo ""
echo "Test run completed."

# Exit with appropriate status code
if [ "$INSTALLATION_SUCCESS" = false ] || [ "$COMPILATION_SUCCESS" = false ] || [ "$TEST_RUN_SUCCESS" = false ]; then
    exit 1
else
    exit 0
fi