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

echo "Compiling COBOL programs..."

# Note: This script assumes GnuCOBOL (formerly OpenCOBOL) is available
# In a real environment, you would need to install a COBOL compiler

echo "Checking for COBOL compiler..."
if command -v cobc >/dev/null 2>&1; then
    echo "GnuCOBOL compiler found"
    
    # Compile test programs
    echo "Compiling test programs..."
    
    cobc -x -I "$COPYBOOK_DIR" -o "$COBOL_DIR/bin/testauth" "$TEST_DIR/TESTAUTH.cbl" 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✓ TESTAUTH compiled successfully"
    else
        echo "✗ TESTAUTH compilation failed"
    fi
    
    cobc -x -I "$COPYBOOK_DIR" -o "$COBOL_DIR/bin/teststorage" "$TEST_DIR/TESTSTORAGE.cbl" 2>/dev/null
    if [ $? -eq 0 ]; then
        echo "✓ TESTSTORAGE compiled successfully"
    else
        echo "✗ TESTSTORAGE compilation failed"
    fi
    
    echo ""
    echo "Running tests..."
    echo "=================="
    
    # Run tests if they compiled successfully
    if [ -f "$COBOL_DIR/bin/testauth" ]; then
        echo "Running TESTAUTH..."
        cd "$COBOL_DIR"
        ./bin/testauth
        echo ""
    fi
    
    if [ -f "$COBOL_DIR/bin/teststorage" ]; then
        echo "Running TESTSTORAGE..."
        cd "$COBOL_DIR"
        ./bin/teststorage
        echo ""
    fi
    
else
    echo "COBOL compiler (cobc) not found."
    echo "To run these tests, install GnuCOBOL:"
    echo "  sudo apt-get install gnucobol"
    echo ""
    echo "Test programs are available for compilation:"
    echo "  - $TEST_DIR/TESTAUTH.cbl"
    echo "  - $TEST_DIR/TESTSTORAGE.cbl"
    echo ""
    echo "Example compilation commands:"
    echo "  cobc -x -I $COPYBOOK_DIR -o testauth $TEST_DIR/TESTAUTH.cbl"
    echo "  cobc -x -I $COPYBOOK_DIR -o teststorage $TEST_DIR/TESTSTORAGE.cbl"
fi

echo "Test run completed."