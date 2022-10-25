load 'lib/bats-support/load'
load 'lib/bats-assert/load'
load 'lib/bats-file/load'

@test "given a missing config file, should print an error indicating that" {
    run ~/bin/terminal-config-manager

    assert_output --partial "No config file found at any of the search paths"
}

@test "given " {
    cp ~/test/System/fixture/01/config.yaml .
    cp ~/test/System/fixture/01/target.conf .

    ~/bin/terminal-config-manager || sleep 1; xdotool key q

    # rm ./config.yaml
    # rm ./target.conf

    assert_success
}

# @test "opening and closing immediately should succeed" {
#     run ~/terminal-config-manager

#     sudo ydotool type 'echo Hey guys. This is Austin.'

#     assert_output --partial "Yaml file not found: test/data/config.yaml"
# }


# function setup() {
#     BACKUP_DIRECTORY="$(temp_make)"/
# }

# @test "1 An empty source directory should leave itself and the backup directory unchanged" {
#     run ./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"

#     assert_success
#     # both directories should be empty
#     assert [ -z "$(ls -A "$FTP_DIRECTORY")" ]
#     assert [ -z "$(ls -A "$BACKUP_DIRECTORY")" ]
# }

# @test "2 A single empty directory should not be copied to the backup directory" {
#     mkdir "$FTP_DIRECTORY"/all

#     run ./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"
#     assert_success
#     assert [ -z "$(ls -A "$BACKUP_DIRECTORY")" ]
# }

# @test "3 A single file inside of an operator directory should be copied over creating a directory" {
#     TEST_OPERATOR_DIRECTORY="$FTP_DIRECTORY"/all
#     TEST_BACKUP_DIRECTORY="$BACKUP_DIRECTORY"/all

#     TEST_FILE="$TEST_OPERATOR_DIRECTORY"/somefile
#     BACKUP_FILE="$TEST_BACKUP_DIRECTORY"/somefile

#     mkdir "$TEST_OPERATOR_DIRECTORY"
#     touch "$TEST_FILE"
#     # neither the backup directory nor the file should exist yet
#     assert [ ! -e "$BACKUP_FILE" ]
#     assert [ ! -d "$TEST_BACKUP_DIRECTORY" ]

#     run ./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"

#     assert_success
#     assert [ -e "$BACKUP_FILE" ]
# }

# @test "4 Given two files only the newest one should be copied" {
#     OPERATOR_DIRECTORY="$FTP_DIRECTORY"/all
#     OPERATOR_BACKUP_DIRECTORY="$BACKUP_DIRECTORY"/all
#     TEST_FILE_OLD="$OPERATOR_DIRECTORY"/someOldfile
#     TEST_FILE_NEW="$OPERATOR_DIRECTORY"/someNewfile

#     mkdir "$OPERATOR_DIRECTORY"
#     touch "$TEST_FILE_OLD"
#     sleep .01
#     touch "$TEST_FILE_NEW"

#     run ./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"

#     assert_success
#     assert [ -e "$OPERATOR_BACKUP_DIRECTORY"/someNewfile ]
#     assert [ ! -e "$OPERATOR_BACKUP_DIRECTORY"/someOldfile ]
# }

# @test "5 Searching for new files inside of an operator directory should not be recursive but rather ignore directories further down" {
#     OPERATOR_DIRECTORY="$FTP_DIRECTORY"/all
#     INNER_DIRECTORY="$OPERATOR_DIRECTORY"/to_be
#     INNER_FILE="$INNER_DIRECTORY"/ignored

#     INNER_BACKUP_DIRECTORY="$BACKUP_DIRECTORY"/all/to_be

#     mkdir -p "$INNER_DIRECTORY"
#     touch "$INNER_FILE"

#     run ./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"

#     assert [ ! -d "$INNER_BACKUP_DIRECTORY" ]
# }

# @test "6 The function directoryContentSortedByCreationTime given an empty directory should succeed" {
#     source ./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"

#     assert [ -z "$(ls -A "$FTP_DIRECTORY")" ]
#     directoryContentSortedByCreationTime "$FTP_DIRECTORY"
#     assert_success
# }

# @test "7 The function directoryContentSortedByCreationTime given a single file should return its absolute path" {
#     source ./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"

#     DIRECTORY="$(temp_make)"
#     FILE_NAME="file"
#     ABSOLUTE_PATH="$DIRECTORY"/"$FILE_NAME"
#     touch "$ABSOLUTE_PATH"

#     run directoryContentSortedByCreationTime "$DIRECTORY"
#     assert_output "$ABSOLUTE_PATH"
#     assert_success
# }

# @test "8 A large amount of files located in the source directories should not cause the script to crash" {
#   OPERATOR_DIRECTORY_1="$FTP_DIRECTORY"/all
#   OPERATOR_DIRECTORY_2="$FTP_DIRECTORY"/5vf
#   OPERATOR_BACKUP_DIRECTORY_1="$BACKUP_DIRECTORY"/all
#   OPERATOR_BACKUP_DIRECTORY_2="$BACKUP_DIRECTORY"/5vf

#   mkdir "$OPERATOR_DIRECTORY_1"
#   mkdir "$OPERATOR_DIRECTORY_2"

#   for i in {1..200}; do
#     touch "$OPERATOR_DIRECTORY_1"/"$i"
#     touch "$OPERATOR_DIRECTORY_2"/"$i"
#     sleep .00001
#   done

#   run ./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"

#   assert_success
#   # The files created last SHOULD exist in the backup directory
#   assert [ -e "$OPERATOR_BACKUP_DIRECTORY_1"/200 ]
#   assert [ -e "$OPERATOR_BACKUP_DIRECTORY_2"/200 ]
#   # The files created NEXT TO last should NOT exist in the backup directory
#   assert [ ! -e "$OPERATOR_BACKUP_DIRECTORY_1"/199 ]
#   assert [ ! -e "$OPERATOR_BACKUP_DIRECTORY_2"/199 ]
# }

# @test "9 Input directories without a trailing slash should be rejected with an appropriate error message" {
#   run ./da_ftp_backup_newest_per_operator.sh /a/directory /another/directory

#   assert_failure
#   assert_output "Please make sure both input directories exist and end in a trailing slash."
# }