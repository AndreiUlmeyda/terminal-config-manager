# Terminal Config Manager - Project Overview

## Purpose
This is a Haskell application that provides a terminal-based interface for managing configuration values across multiple files. Users can quickly switch between predefined values in various config files without manually editing them.

## Architecture

### Clean Architecture Layers
- **Domain**: Core business logic and types
- **Infrastructure**: File I/O, config parsing, external dependencies  
- **UserInterface**: Brick-based TUI rendering and input handling
- **Application**: Main application orchestration

### Key Components

#### Domain Layer (`src/Domain/`)
- `State.hs`: Application state management with `AppState` and `WidgetId`
- `ItemsCursor.hs`: Navigation through config items
- `ValueSelection.hs`: Logic for cycling through possible values
- `FileSynchronization.hs`: Coordination between UI state and file state

#### Infrastructure Layer (`src/Infrastructure/`)
- `Config.hs`: YAML configuration file parsing with Aeson
- `FileModification.hs`: Safe file writing operations
- `FsReadIO.hs`: Restricted file system monad for controlled I/O
- `Errors.hs`: Centralized error message definitions

#### UI Layer (`src/UserInterface/`)
- `Render.hs`: Brick widget rendering with highlighting
- `Input.hs`: Keyboard event handling (arrows, q to quit)
- `Cli.hs`: Command-line interface setup

## Technology Stack
- **Haskell**: Primary language with GHC 9.6.7
- **Stack**: Build system with LTS 22.44 resolver
- **Brick**: Terminal user interface framework
- **Aeson/Yaml**: Configuration file parsing
- **Vty**: Low-level terminal control

## Configuration Format
YAML files with this structure:
```yaml
config_lines_to_manage:
  - title: "Setting Name"
    path: "/path/to/file"
    pattern: "key={{value}}"
    targetValue: "current"
    possibleValues: ["option1", "option2", "option3"]
```

## Development Notes
- Strong type safety with newtype wrappers
- Monadic error handling throughout
- Separation of pure business logic from I/O effects
- Pattern matching for safe value substitution 