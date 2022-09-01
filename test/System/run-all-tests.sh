#! lib/bats/bin/bats

load 'lib/bats-support/load'
load 'lib/bats-assert/load'
load 'lib/bats-file/load'

# function setup() {
# }

@test "1 The program should run successfully" {
    run ../../bin/terminal-config-manager --help

    assert_success
}