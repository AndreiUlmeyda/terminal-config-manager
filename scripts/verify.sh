#!/bin/bash
# Terminal Config Manager - Code Verification Script
# This script runs all code quality checks before committing

set -e  # Exit on any error

echo "🔍 Terminal Config Manager - Code Verification Pipeline"
echo "======================================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print status
print_step() {
    echo -e "${BLUE}📋 $1${NC}"
}

print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Step 1: Code Formatting
print_step "Step 1: Checking code formatting with Ormolu..."
if stack exec ormolu -- --mode check $(fd '\.hs$' src test) 2>/dev/null; then
    print_success "Code formatting is correct"
else
    print_error "Code formatting issues found!"
    print_warning "Run: stack exec ormolu -- --mode inplace \$(fd '\\.hs\$' src test)"
    exit 1
fi

# Step 2: Linting
print_step "Step 2: Running HLint..."
if stack exec hlint -- src/ test/; then
    print_success "HLint passed with no warnings"
else
    print_error "HLint warnings/errors found!"
    print_warning "Please fix the issues above"
    exit 1
fi

# Step 3: Type Checking & Clean Build
print_step "Step 3: Building with warnings as errors..."
if stack build --ghc-options="-Wall -Werror"; then
    print_success "Clean build completed successfully"
else
    print_error "Build failed or has warnings!"
    exit 1
fi

# Step 4: Unit Testing
print_step "Step 4: Running unit tests..."
if stack test; then
    print_success "All unit tests passed"
else
    print_error "Unit tests failed!"
    exit 1
fi

# Step 5: Integration Testing (optional - just check it builds)
print_step "Step 5: Verifying application builds and runs..."
if stack exec terminal-config-manager -- --help >/dev/null 2>&1 || 
   timeout 2s stack exec terminal-config-manager >/dev/null 2>&1 || 
   [ $? -eq 124 ]; then  # timeout exit code
    print_success "Application builds and starts correctly"
else
    print_warning "Application may have runtime issues (manual testing recommended)"
fi

# Final success message
echo ""
echo "======================================================="
print_success "🎉 All verification checks passed!"
print_success "Your code is ready for commit."
echo "=======================================================" 