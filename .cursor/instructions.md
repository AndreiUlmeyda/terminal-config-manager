# Development Instructions for Terminal Config Manager

## Adding New Features

### 1. Adding a New Domain Type
When creating new domain concepts:
- Create a newtype wrapper with `Mk` prefix
- Add to appropriate Domain module or create new one
- Export constructor and any necessary accessor functions
- Add `deriving stock (Show, Eq)` unless there's a specific reason not to
- Consider if Generic derivation is needed for JSON parsing

### 2. Adding New Configuration Options
To extend the YAML configuration format:
- Add new field to `ConfigItem` record in `Infrastructure.Config`
- Update the `FromJSON` instance with the new field
- Add corresponding key constant (e.g., `configElementNewField :: Key`)
- Update documentation and examples
- Consider backward compatibility

### 3. Adding New UI Elements
For Brick UI additions:
- Add rendering logic to `UserInterface.Render`
- Add event handling to `UserInterface.Input` if interactive
- Use `Widget WidgetId` type for all widgets
- Follow existing highlighting patterns
- Test with various terminal sizes

### 4. Adding New File Operations
When adding file I/O:
- Use `FsReadIO` monad for file operations
- Add operations to `Infrastructure.FileModification` or `Infrastructure.FsReadIO`
- Handle errors appropriately with descriptive messages
- Consider file permissions and edge cases

## Testing Guidelines

### Unit Tests
- Test pure functions in Domain layer extensively
- Use property-based testing for invariants
- Create test data in `test/data/` directory
- Mock file operations when testing Infrastructure layer

### Integration Tests
- Test full configuration loading pipeline
- Test UI rendering with known inputs
- Verify file modification behavior
- Test error handling with malformed configs

## Debugging

### Common Issues
1. **YAML parsing errors**: Check key names match exactly
2. **File not found**: Verify config file search paths
3. **Pattern matching fails**: Ensure patterns are unique and specific
4. **UI not updating**: Check event handling and state updates

### Debug Techniques
- Use `trace` from `Debug.Trace` for temporary debugging
- Add debug prints in IO operations
- Test UI components in isolation
- Use Stack's ghci for interactive testing

## Performance Considerations

### When to Optimize
- File operations are the main bottleneck
- UI rendering should be responsive
- Configuration parsing happens once at startup
- Memory usage is generally not a concern

### How to Profile
```bash
stack build --profile
stack exec --profile -- terminal-config-manager +RTS -p
```

## Code Review Checklist

### Before Committing
- [ ] All functions have type signatures
- [ ] Exports are minimal and necessary
- [ ] Error handling is appropriate
- [ ] Documentation is updated
- [ ] Tests pass
- [ ] No unused imports or variables
- [ ] Follow consistent naming conventions
- [ ] Architecture layers are respected

### Architecture Validation
- [ ] Domain layer remains pure (no IO)
- [ ] Infrastructure handles all external dependencies
- [ ] UI logic is separate from business logic
- [ ] Application layer orchestrates everything

## Common Refactoring Patterns

### Extract Function
When functions get too large:
```haskell
-- Before
bigFunction :: Input -> Output
bigFunction input = 
  let step1 = ...
      step2 = ...
      step3 = ...
  in step3

-- After  
bigFunction :: Input -> Output
bigFunction input = step3 (step2 (step1 input))
  where
    step1 = ...
    step2 = ...
    step3 = ...
```

### Extract Type
When dealing with complex data:
```haskell
-- Before
processConfig :: Text -> FilePath -> [Text] -> Result

-- After
data ConfigRequest = MkConfigRequest
  { pattern :: Text,
    filePath :: FilePath, 
    values :: [Text]
  }

processConfig :: ConfigRequest -> Result
``` 