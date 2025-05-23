#!/bin/bash

# Azure Integration Landing Zone COBOL Build Script
# Compiles all COBOL programs and creates executable binaries

echo "Azure Integration Landing Zone COBOL Build System"
echo "=================================================="

# Set up environment
COBOL_DIR="/home/runner/work/Azure-Integration-Landing-Zone/Azure-Integration-Landing-Zone/infra/cobol"
SRC_DIR="$COBOL_DIR/src"
COPYBOOK_DIR="$COBOL_DIR/copybooks"
BIN_DIR="$COBOL_DIR/bin"

# Create output directory
mkdir -p "$BIN_DIR"

echo "Build environment:"
echo "  Source directory: $SRC_DIR"
echo "  Copybook directory: $COPYBOOK_DIR"
echo "  Binary directory: $BIN_DIR"
echo ""

# Check for COBOL compiler
if command -v cobc >/dev/null 2>&1; then
    echo "✓ GnuCOBOL compiler found: $(cobc --version | head -n1)"
    echo ""
    
    # Compile main programs
    echo "Compiling COBOL programs..."
    echo "=========================="
    
    # Array of programs to compile
    declare -a programs=(
        "AZUREDEPLOY:Main deployment orchestrator"
        "AZUREAUTH:Azure authentication module"
        "STORAGE:Storage Account deployment"
        "KEYVAULT:Key Vault deployment"
        "VIRTUALNET:Virtual Network deployment"
        "LOGANALYTICS:Log Analytics deployment"
        "SERVICEBUS:Service Bus deployment"
        "DATAFACTORY:Data Factory deployment"
    )
    
    compiled_count=0
    failed_count=0
    
    for program_info in "${programs[@]}"; do
        IFS=':' read -r program_name description <<< "$program_info"
        
        echo "Compiling $program_name ($description)..."
        
        if cobc -x -I "$COPYBOOK_DIR" -o "$BIN_DIR/${program_name,,}" "$SRC_DIR/$program_name.cbl" 2>/dev/null; then
            echo "  ✓ $program_name compiled successfully"
            ((compiled_count++))
        else
            echo "  ✗ $program_name compilation failed"
            # Try to get error details
            echo "    Attempting compilation with verbose output:"
            cobc -x -I "$COPYBOOK_DIR" -o "$BIN_DIR/${program_name,,}" "$SRC_DIR/$program_name.cbl"
            ((failed_count++))
        fi
        echo ""
    done
    
    # Compile test programs
    echo "Compiling test programs..."
    echo "========================="
    
    declare -a test_programs=(
        "TESTAUTH:Authentication module tests"
        "TESTSTORAGE:Storage module tests"
        "TESTSERVICEBUS:Service Bus module tests"
        "TESTDATAFACTORY:Data Factory module tests"
    )
    
    for test_info in "${test_programs[@]}"; do
        IFS=':' read -r test_name description <<< "$test_info"
        
        echo "Compiling $test_name ($description)..."
        
        if cobc -x -I "$COPYBOOK_DIR" -o "$BIN_DIR/${test_name,,}" "$COBOL_DIR/tests/$test_name.cbl" 2>/dev/null; then
            echo "  ✓ $test_name compiled successfully"
            ((compiled_count++))
        else
            echo "  ✗ $test_name compilation failed"
            ((failed_count++))
        fi
        echo ""
    done
    
    # Build summary
    echo "Build Summary"
    echo "============="
    echo "Total programs compiled: $compiled_count"
    echo "Failed compilations: $failed_count"
    echo ""
    
    if [ $failed_count -eq 0 ]; then
        echo "🎉 All programs compiled successfully!"
        echo ""
        echo "Available executables in $BIN_DIR:"
        ls -la "$BIN_DIR"
        echo ""
        echo "To run the deployment:"
        echo "  cd $COBOL_DIR"
        echo "  ./bin/azuredeploy"
        echo ""
        echo "To run tests:"
        echo "  cd $COBOL_DIR/tests"
        echo "  ./run-tests.sh"
    else
        echo "⚠️  Some programs failed to compile. Please check the errors above."
        echo ""
        echo "Common issues and solutions:"
        echo "1. Missing copybooks: Ensure AZURECONFIG.cpy and HTTPCLIENT.cpy exist"
        echo "2. Syntax errors: Check COBOL program syntax"
        echo "3. Compiler version: Ensure GnuCOBOL 3.x or later is installed"
    fi
    
else
    echo "❌ COBOL compiler (cobc) not found."
    echo ""
    echo "To install GnuCOBOL:"
    echo "  Ubuntu/Debian: sudo apt-get install gnucobol"
    echo "  CentOS/RHEL:   sudo yum install gnucobol"
    echo "  macOS:         brew install gnu-cobol"
    echo ""
    echo "Alternative: Compile manually with:"
    echo "  cobc -x -I copybooks -o bin/program src/PROGRAM.cbl"
fi

echo ""
echo "Build script completed."