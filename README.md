# terminal-config-manager

# target work flow
commence day job
feel the need to change a config value somewhere on your hard drive
switch to prepared terminal
see one line per config value
see current and possible values for each
cycle through values on key press

# non happy paths
target file missing
target file changed
config entry invalid
no config entries
pattern matches twice

# nice to have
append comment to invalid config entries
allow comments inside of the config file
have the config file be of a structured format

# config entry
consists of
    absolute path of config file
    a way to match the value (line number, regex, ect.)
    current value
    possible values
    title
    description (optional)

