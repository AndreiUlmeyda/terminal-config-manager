# terminal-config-manager

## View the dependency graph
Make sure you've got [graphmod](https://github.com/yav/graphmod)  (generating the graph) and [xdot](https://github.com/jrfonseca/xdot.py) (viewing the graph) installed. Then, navigate to the project root directory and run
```
tool/generate-dependency-graph.sh
```

## TODOs
- write state back to config file (on program exit or on each value change)
- find and handle every operation that can fail
- handle the case where the target file value and config value dont match, for instance on startup
- handle the case where the pattern does not match anything, for instance when the file was modified some other way during runtime
- handle the case where targetValue is not an element of possibleValues
- refactor StateTransitions
- add module documentation
- add README (known issues, termCap)
- consider leaving out the Content data type
- add help and version commands
- invert the dependency between UI and Applicatin
- unify app/Main.hs and src/Application/App.hs

## Design considerations, unordered
- domain concepts  line(?) selection, value selection, old file content, new file content
- mode = LineSelection | ValueSelection
- LineSelection = NextLine | PreviousLine
- ValueSelection = NextValue | PreviousValue
- <DomainFunction> :: mode -> OldFile -> NewFile
- ModificationCommand
- Line may be a rendering concept, rather than a domain concept -> item could
    already be better

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