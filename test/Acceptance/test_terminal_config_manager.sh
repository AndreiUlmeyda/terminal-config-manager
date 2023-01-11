
load 'lib/bats-support/load'
load 'lib/bats-assert/load'
load 'lib/bats-file/load'

# function setup() {
#     # FTP_DIRECTORY="$(temp_make)"/
#     # BACKUP_DIRECTORY="$(temp_make)"/
# }

@test "1 TODO" {
    run ls #./da_ftp_backup_newest_per_operator.sh "$FTP_DIRECTORY" "$BACKUP_DIRECTORY"

    assert_success
}