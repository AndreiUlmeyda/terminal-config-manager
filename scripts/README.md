# Scripts Directory

This directory contains automation scripts for code quality verification and development workflow.

## Available Scripts

### 🔍 `verify.sh`
**Comprehensive code verification pipeline**

Runs all quality checks before committing:
- **Code formatting** (Ormolu)
- **Linting** (HLint) 
- **Clean build** with warnings as errors
- **Unit tests**
- **Integration testing**

```bash
# Run full verification
./scripts/verify.sh

# Expected output on success:
🔍 Terminal Config Manager - Code Verification Pipeline
=======================================================
📋 Step 1: Checking code formatting with Ormolu...
✅ Code formatting is correct
📋 Step 2: Running HLint...
✅ HLint passed with no warnings
📋 Step 3: Building with warnings as errors...
✅ Clean build completed successfully
📋 Step 4: Running unit tests...
✅ All unit tests passed
📋 Step 5: Verifying application builds and runs...
✅ Application builds and starts correctly

=======================================================
✅ 🎉 All verification checks passed!
✅ Your code is ready for commit.
=======================================================
```

### 🪝 `install-pre-commit-hook.sh` 
**Automatic pre-commit verification**

Installs a Git pre-commit hook that runs the verification pipeline automatically before each commit.

```bash
# Install the pre-commit hook
./scripts/install-pre-commit-hook.sh

# After installation, verification runs automatically:
git commit -m "Your commit message"
# 🔍 Running pre-commit verification...
# ✅ Pre-commit verification passed!
```

**Features:**
- Runs automatically on `git commit`
- Prevents commits that fail verification
- Can be bypassed with `git commit --no-verify` (not recommended)
- Falls back to manual checks if verify.sh is not available

## Development Workflow

### Recommended Daily Workflow

1. **Before starting work:**
   ```bash
   # Install pre-commit hook (one-time setup)
   ./scripts/install-pre-commit-hook.sh
   ```

2. **During development:**
   ```bash
   # Run verification frequently
   ./scripts/verify.sh
   
   # Fix any issues found
   stack exec ormolu -- --mode inplace $(fd -e hs src test)
   stack exec hlint -- src/ test/  # Fix warnings
   ```

3. **Before committing:**
   ```bash
   # Verification runs automatically via pre-commit hook
   git add .
   git commit -m "Your changes"
   ```

### Manual Commands

If you prefer to run individual checks:

```bash
# Format code
stack exec ormolu -- --mode inplace $(fd -e hs src test)

# Check formatting
stack exec ormolu -- --mode check $(fd -e hs src test)

# Run linter
stack exec hlint -- src/ test/

# Build with strict warnings
stack build --ghc-options="-Wall -Werror"

# Run tests
stack test

# Run with coverage
stack test --coverage
```

## CI/CD Integration

For continuous integration, use these commands:

```bash
# Complete build with documentation
stack build --test --haddock --no-haddock-deps

# Formatting check
stack exec ormolu -- --mode check $(fd -e hs)

# Linting
stack exec hlint -- src/ test/

# Tests with coverage
stack test --coverage
```

## Troubleshooting

### Script Won't Run
```bash
# Make sure scripts are executable
chmod +x scripts/*.sh

# Check you're in the project root
ls terminal-config-manager.cabal
```

### Formatting Issues
```bash
# Auto-fix formatting
stack exec ormolu -- --mode inplace $(fd -e hs src test)
```

### HLint Warnings
```bash
# See specific issues
stack exec hlint -- src/ test/

# Ignore specific warnings (use sparingly)
{-# HLINT ignore "Use newtype instead of data" #-}
```

### Build Failures
```bash
# Build without strict warnings first
stack build

# Then identify specific warnings
stack build --ghc-options="-Wall"
```

## Benefits

✅ **Consistent code quality** across all commits  
✅ **Automated verification** prevents broken commits  
✅ **Early problem detection** before code review  
✅ **Standardized formatting** for better readability  
✅ **Zero-warning policy** for cleaner codebase 