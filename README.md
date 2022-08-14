# terminal-config-manager

This is an application to manage selected values scattered over many different
files quickly. A common use case will probably be to quickly switch single values
inside of a set of config files without having to:
- navigate to the file
- open it in a text editor
- search the target value
- modify it
- save the file

In order to know which files to manipulate a few things will need to be specified
beforehand inside of a config file for  each value we want to manage:

- a title
- the absolute path of the target (config) file
- the current value of the config entry in question
- a pattern with which to identify the parts of the target file surrounding the
config entry in question
- a list of possible values for the config entry in question

The application displays one line per managed value, its title and the current
value.

The up/down arrow keys select the value to change, left/right arrows change the
value to the next or previous
possible value.

## Usage
- prepare a config file
- run
    ```
    terminal-config-manager
    ```
- select a config entry using the up/down arrow keys
- change the value using the left/right arrow keys
- hit q to quit

## View the dependency graph
Make sure you've got [graphmod](https://github.com/yav/graphmod)  (generating
the graph) and [xdot](https://github.com/jrfonseca/xdot.py) (viewing the graph)
installed. Then, navigate to the project root directory and run
```
tool/generate-dependency-graph.sh
```
## Known issues
- an [underlying library](https://github.com/judah/terminfo) which
 is tasked with querying information about the terminal the program is run with
 apparently contains [a bug](https://github.com/judah/terminfo/issues/47) which
 can result in the **coloring of the config value** being **wrong**. The value will
 be rendered using the default text color instead of being highlighted. The
 frequency of this issue is **once every few dozens to hundreds of program startups**.
- another library [vty](https://github.com/jtdaugherty/vty) seems to
exhibit an issue where it can throw a segfault when receiving input before being
properly inizialized. This issue should crop up either infrequently or never,
usually only when quickly quitting and restarting the program.

## TODOs
- write state back to config file (on program exit or on each value change)
- find and handle every operation that can fail
- handle the case where the target file value and config value dont match, for
    instance on startup
- handle the case where the pattern does not match anything, for instance when
    the file was modified some other way during runtime
- handle the case where targetValue is not an element of possibleValues
- refactor StateTransitions
- add module documentation
- add help and version commands
- invert the dependency between UI and Application
- break up the Infrastructure/Util, determine which layer it should go (Domain?)
- allow common locations for the config file

## Design considerations, unordered
- domain concepts  line(?) selection, value selection, old file content, new
    file content
- mode = LineSelection | ValueSelection
- LineSelection = NextLine | PreviousLine
- ValueSelection = NextValue | PreviousValue
- DomainFunction :: mode -> OldFile -> NewFile
- ModificationCommand
- Line may be a rendering concept, rather than a domain concept -> item could
    already be better

## Glossary
- **item**: An element of the list inside of the config file. It
    corresponds to one substring inside of one file which you
    want to manage. This also sometimes refers to a line as it
    is rendered on the screen, where you are shown the description and the current value.
- **value**: A part of an item which specifies the current state
    of the substring you want to manage. It is rendered as the
    latter, highlighted part of a line.