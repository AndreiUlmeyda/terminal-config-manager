# .cursor Directory

This directory contains configuration and context files for the [Cursor](https://cursor.sh/) AI-powered code editor. These files help the AI assistant understand the project structure, coding conventions, and provide better suggestions.

## Contents

### Configuration
- **`config.json`** - Cursor-specific settings for AI features, formatting, and linting
- **`rules`** - AI assistant rules and coding conventions for this project

### Documentation  
- **`docs/project-overview.md`** - Comprehensive project architecture and component overview
- **`instructions.md`** - Step-by-step guidance for common development tasks

### Context
- **`context/haskell-patterns.md`** - Common code patterns and examples from this project

## Purpose

The AI assistant uses these files to:
- Understand the project's clean architecture structure
- Follow consistent Haskell coding conventions
- Provide context-aware suggestions that fit the existing codebase
- Help with domain-specific tasks like Brick UI development and YAML parsing
- Maintain type safety and functional programming best practices

## Usage

When working in Cursor:
1. The AI will automatically reference these files for context
2. Ask questions about project structure and the AI will use these docs
3. Request code that follows the patterns and conventions defined here
4. The AI will suggest imports, types, and implementations consistent with the codebase

## Maintenance

Keep these files updated when:
- Adding new architectural layers or components
- Changing coding conventions or patterns
- Introducing new dependencies or technologies
- Discovering new best practices for the project

## Benefits

- **Consistent code style** across all AI-generated suggestions
- **Faster development** with context-aware completions
- **Better architecture adherence** when adding new features
- **Reduced cognitive load** by encoding project knowledge in documentation 