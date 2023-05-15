/*
 *
 */

// SFTP File Transfer Flags -- (e.g. flags parameter to sftp_open())
#define LIBSSH2_FXF_READ                        0x00000001
#define LIBSSH2_FXF_WRITE                       0x00000002
#define LIBSSH2_FXF_APPEND                      0x00000004
#define LIBSSH2_FXF_CREAT                       0x00000008
#define LIBSSH2_FXF_TRUNC                       0x00000010
#define LIBSSH2_FXF_EXCL                        0x00000020

/* File mode */
/* Read, write, execute/search by owner */
#define LIBSSH2_SFTP_S_IRWXU        0x001C0     /* RWX mask for owner */
#define LIBSSH2_SFTP_S_IRUSR        0x00100     /* R for owner */
#define LIBSSH2_SFTP_S_IWUSR        0x00080     /* W for owner */
#define LIBSSH2_SFTP_S_IXUSR        0x00040     /* X for owner */
/* Read, write, execute/search by group */
#define LIBSSH2_SFTP_S_IRWXG        0x00038     /* RWX mask for group */
#define LIBSSH2_SFTP_S_IRGRP        0x00020     /* R for group */
#define LIBSSH2_SFTP_S_IWGRP        0x00010     /* W for group */
#define LIBSSH2_SFTP_S_IXGRP        0x00008     /* X for group */
/* Read, write, execute/search by others */
#define LIBSSH2_SFTP_S_IRWXO        0x00007     /* RWX mask for other */
#define LIBSSH2_SFTP_S_IROTH        0x00004     /* R for other */
#define LIBSSH2_SFTP_S_IWOTH        0x00002     /* W for other */
#define LIBSSH2_SFTP_S_IXOTH        0x00001     /* X for other */

#define LIBSSH2_SFTP_S_IFDIR        0x04000     /* directory */
