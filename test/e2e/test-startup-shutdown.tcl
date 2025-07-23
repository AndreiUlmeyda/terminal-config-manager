#!/usr/bin/expect -f

# prepare test config file
exec echo "setting=initial" > /tmp/test-target.conf
exec echo "config_lines_to_manage:\n  - title: \"Test Setting\"\n    path: \"/tmp/test-target.conf\"\n    pattern: \"setting={{value}}\"\n    targetValue: \"initial\"\n    possibleValues: \[\"initial\", \"changed\", \"final\"\]" > /tmp/test-config.yaml

# run the app
spawn stack exec terminal-config-manager -- --config /tmp/test-config.yaml

set timeout 5
expect {
    timeout {
        puts "✗ FAIL: No output received within timeout"
        exit 1
    }
    "navigate" {
        puts "✓ Application started and help text visible"
    }
    eof {
        puts "✗ FAIL: Application exited prematurely"
        exit 1
    }
}

# Send quit command
send "q"

# Expect immediate exit (very short timeout - fail if app hangs)
set timeout 2
expect {
    timeout {
        puts "✗ FAIL: Application did not exit within timeout"
        exit 1
    }
    eof {
        puts "✓ SUCCESS: Application exited gracefully"
        exit 0
    }
}