#!/bin/bash
# Install pre-commit hook for Terminal Config Manager

echo "Installing pre-commit hook for Terminal Config Manager..."

# Create the pre-commit hook
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
# Terminal Config Manager - Pre-commit Hook
# Automatically runs verification pipeline before each commit

echo "🔍 Running pre-commit verification..."

# Check if we're in the right directory
if [ ! -f "terminal-config-manager.cabal" ]; then
    echo "❌ Error: Not in terminal-config-manager project directory"
    exit 1
fi

# Run the verification script
if [ -x "./scripts/verify.sh" ]; then
    ./scripts/verify.sh
else
    echo "⚠️  Warning: verify.sh script not found or not executable"
    echo "Running manual checks..."
    
    # Fallback to manual checks
    echo "📋 Checking code formatting..."
    if ! stack exec ormolu -- --mode check $(fd '\.hs$' src test 2>/dev/null); then
    echo "❌ Code formatting issues found!"
    echo "💡 Run: stack exec ormolu -- --mode inplace \$(fd '\\.hs\$' src test)"
        exit 1
    fi
    
    echo "📋 Running HLint..."
    if ! stack exec hlint -- src/ test/; then
        echo "❌ HLint warnings found!"
        exit 1
    fi
    
    echo "📋 Building with warnings as errors..."
    if ! stack build --ghc-options="-Wall -Werror"; then
        echo "❌ Build failed or has warnings!"
        exit 1
    fi
    
    echo "📋 Running tests..."
    if ! stack test; then
        echo "❌ Tests failed!"
        exit 1
    fi
fi

echo "✅ Pre-commit verification passed!"
EOF

# Make the hook executable
chmod +x .git/hooks/pre-commit

echo "✅ Pre-commit hook installed successfully!"
echo ""
echo "The hook will now run automatically before each git commit."
echo "To bypass the hook (not recommended), use: git commit --no-verify"
echo ""
echo "You can also run verification manually with: ./scripts/verify.sh" 