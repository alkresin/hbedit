/*
 * Harbour wrappers for libssh2 - headers
 *
 * Copyright 2023 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "libssh2_config.h"
#ifdef WIN32
#include "libssh2.h"
#include "libssh2_sftp.h"
#else
#include <libssh2.h>
#include <libssh2_sftp.h>
#include <netdb.h>
#endif

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/types.h>
//#ifdef HAVE_STDLIB_H
#include <stdlib.h>
//#endif
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>

#ifdef WIN32
#define __FILESIZE "I64"
#else
#define __FILESIZE "llu"
#endif

typedef struct {

   int sock;
   LIBSSH2_SESSION *session;
   LIBSSH2_CHANNEL *channel;
   LIBSSH2_SFTP *sftp_session;
   int iNonBlocking;
   int iRes, iErr;

} HB_SSH2_SESSION;

typedef struct {
   HB_SSH2_SESSION *pSess;
   LIBSSH2_SFTP_HANDLE *sftp_handle;
} HB_SSH2_SFTP_HANDLE;

int hb_ssh2_WaitSocket( int, LIBSSH2_SESSION * );
HB_SSH2_SESSION * hb_ssh2_Connect( const char *, int, int );
void hb_ssh2_Close( HB_SSH2_SESSION * );
void hb_ssh2_Exit( void );
int hb_ssh2_LoginPass( HB_SSH2_SESSION *, const char *, const char * );
int hb_ssh2_ChannelOpen( HB_SSH2_SESSION * );
void hb_ssh2_ChannelClose( HB_SSH2_SESSION * );
int hb_ssh2_Exec( HB_SSH2_SESSION *, const char * );
char * hb_ssh2_ChannelRead( HB_SSH2_SESSION * );
int hb_ssh2_SftpInit( HB_SSH2_SESSION * );
void hb_ssh2_SftpShutDown( HB_SSH2_SESSION * );
HB_SSH2_SFTP_HANDLE * hb_ssh2_SftpOpenDir( HB_SSH2_SESSION *, const char * );
HB_SSH2_SFTP_HANDLE * hb_ssh2_SftpOpenFile( HB_SSH2_SESSION *, const char *, unsigned long, long );
void hb_ssh2_SftpClose( HB_SSH2_SFTP_HANDLE * );
int hb_ssh2_SftpMkDir( HB_SSH2_SESSION *, const char *, long );
int hb_ssh2_SftpReadDir( HB_SSH2_SFTP_HANDLE *, char *, int, unsigned long *, unsigned long *, unsigned long * );
int hb_ssh2_SftpRead( HB_SSH2_SFTP_HANDLE *, char *, int );
int hb_ssh2_SftpWrite( HB_SSH2_SFTP_HANDLE *, char *, int );
int hb_ssh2_SftpStat( HB_SSH2_SESSION *, char *, int, LIBSSH2_SFTP_ATTRIBUTES * );
