/*
 * Harbour wrappers for libssh2
 *
 * Copyright 2023 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hb_ssh2.h"

#define  BUFFSIZE   8192

static short int iSsh2Init = 0;
static int( *pCallback )( HB_SSH2_SESSION * ) = NULL;


void hb_ssh2_setCallback( int( *fn )( HB_SSH2_SESSION * ) )
{
   pCallback = fn;
}

static unsigned long hb_ssh2_getAddr( const char *szName )
{
   unsigned long ulAddr = inet_addr( szName );

   if( ulAddr == INADDR_NONE )
   {
      struct hostent *Host = gethostbyname( szName );

      if( Host )
         return ( *( unsigned int * ) Host->h_addr_list[0] );
      else
         return INADDR_NONE;
   }

   return ulAddr;
}

int hb_ssh2_WaitSocket( HB_SSH2_SESSION * pSess )
{
   struct timeval timeout;
   int rc;
   fd_set fd;
   fd_set *writefd = NULL;
   fd_set *readfd = NULL;
   int dir;

   timeout.tv_sec = pSess->timeout.tv_sec;
   timeout.tv_usec = pSess->timeout.tv_usec;

   FD_ZERO( &fd );

   FD_SET( pSess->sock, &fd );

   /* now make sure we wait in the correct direction */
   dir = libssh2_session_block_directions( pSess->session );

   if( dir & LIBSSH2_SESSION_BLOCK_INBOUND )
      readfd = &fd;

   if( dir & LIBSSH2_SESSION_BLOCK_OUTBOUND )
      writefd = &fd;

   rc = select( pSess->sock + 1, readfd, writefd, NULL, &(timeout) );

   return rc;
}

HB_SSH2_SESSION *hb_ssh2_Connect( const char *hostname, int iPort, int iNonBlocking )
{

   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) malloc( sizeof( HB_SSH2_SESSION ) );
   unsigned long hostaddr;
   struct sockaddr_in sin;
   int rc;

   memset( pSess, 0, sizeof( HB_SSH2_SESSION ) );

   pSess->timeout.tv_sec = 1;
   pSess->timeout.tv_usec = 0;

   if( !iSsh2Init )
   {
#ifdef WIN32
      WSADATA wsadata;
      rc = WSAStartup( MAKEWORD( 2, 0 ), &wsadata );
      if( rc != 0 )
      {
         pSess->iRes = -99;
         return pSess;
      }
#endif
      rc = libssh2_init( 0 );
      if( rc != 0 )
      {
         pSess->iRes = -1;
         return pSess;
      }
      iSsh2Init = 1;
   }

   hostaddr = hb_ssh2_getAddr( hostname );
   if( hostaddr == INADDR_NONE )
   {
      pSess->iRes = -2;
      return pSess;
   }
   pSess->sock = socket( AF_INET, SOCK_STREAM, 0 );

   sin.sin_family = AF_INET;
   sin.sin_port = htons( iPort );
   sin.sin_addr.s_addr = hostaddr;
   if( connect( pSess->sock, ( struct sockaddr * ) ( &sin ),
               sizeof( struct sockaddr_in ) ) != 0 )
   {
      pSess->iRes = -3;
      return pSess;
   }

   /* Create a session instance */
   pSess->session = libssh2_session_init(  );

   if( !pSess->session )
   {
      pSess->iRes = -4;
      pSess->iErr = libssh2_session_last_errno( pSess->session );
      return pSess;
   }

   libssh2_session_set_blocking( pSess->session, !iNonBlocking );
   pSess->iNonBlocking = iNonBlocking;

   while( ( rc = libssh2_session_handshake( pSess->session,
                     pSess->sock ) ) == LIBSSH2_ERROR_EAGAIN );
   if( rc )
   {
      pSess->iRes = -5;
      pSess->iErr = libssh2_session_last_errno( pSess->session );
      return pSess;
   }

   return pSess;
}

void hb_ssh2_Close( HB_SSH2_SESSION * pSess )
{

   hb_ssh2_SftpShutDown( pSess );
   hb_ssh2_ChannelClose( pSess );

   if( pSess->session )
   {
      libssh2_session_disconnect( pSess->session,
            "Normal Shutdown, Thank you for playing" );
      libssh2_session_free( pSess->session );
      pSess->session = NULL;
   }

   if( pSess->sock )
   {
#ifdef WIN32
      closesocket( pSess->sock );
#else
      close( pSess->sock );
#endif
      pSess->sock = 0;
   }

   free( pSess );

}

void hb_ssh2_Exit( void )
{
   if( iSsh2Init )
      libssh2_exit();
   iSsh2Init = 0;
}

int hb_ssh2_LoginPass( HB_SSH2_SESSION * pSess, const char *pLogin, const char *pPass )
{
   int rc;

   while( ( rc =
               libssh2_userauth_password( pSess->session, pLogin,
                     pPass ) ) == LIBSSH2_ERROR_EAGAIN );
   pSess->iRes = rc;
   pSess->iErr = libssh2_session_last_errno( pSess->session );
   if( rc )
      return 1;
   return 0;
}

int hb_ssh2_ChannelOpen( HB_SSH2_SESSION * pSess )
{
   pSess->iInfo = 1;
   /* Exec non-blocking on the remove host */
   while( ( pSess->channel =
               libssh2_channel_open_session( pSess->session ) ) == NULL &&
         libssh2_session_last_error( pSess->session, NULL, NULL,
               0 ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }
   pSess->iRes = ( pSess->channel == NULL );
   pSess->iErr = libssh2_session_last_errno( pSess->session );
   return pSess->iRes;
}

void hb_ssh2_ChannelClose( HB_SSH2_SESSION * pSess )
{

   if( !pSess->channel )
      return;
   while( libssh2_channel_close( pSess->channel ) == LIBSSH2_ERROR_EAGAIN )
      hb_ssh2_WaitSocket( pSess );
#if 0
   {
      int exitcode = 127;
      char *exitsignal = ( char * ) "none";

      if( rc == 0 )
      {
         exitcode = libssh2_channel_get_exit_status( pSess->channel );
         libssh2_channel_get_exit_signal( pSess->channel, &exitsignal,
               NULL, NULL, NULL, NULL, NULL );
      }

      if( exitsignal )
         fprintf( stderr, "\nGot signal: %s\n", exitsignal );
      else
         fprintf( stderr, "\nEXIT: %d\n", exitcode );
   }
#endif

   libssh2_channel_free( pSess->channel );
   pSess->channel = NULL;
}

int hb_ssh2_Exec( HB_SSH2_SESSION * pSess, const char *commandline )
{
   int rc;

   pSess->iInfo = 2;
   while( ( rc = libssh2_channel_exec( pSess->channel,
                     commandline ) ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }

   pSess->iRes = ( rc != 0 )? -1 : 0;
   if( rc != 0 )
      pSess->iErr = libssh2_session_last_errno( pSess->session );
   return pSess->iRes;
}

char * hb_ssh2_ChannelRead( HB_SSH2_SESSION * pSess )
{

   char buffer[BUFFSIZE], *pOut = NULL;
   int iBytesRead = 0, iBytesReadAll = 0;
   int iOutFirst = 1;
   int rc;

   pSess->iInfo = 3;
   for( ;; )
   {
      do
      {
         rc = libssh2_channel_read( pSess->channel, buffer+iBytesRead, BUFFSIZE-iBytesRead );

         if( rc > 0 )
         {
            iBytesRead += rc;
         }
      }
      while( rc > 0 && iBytesRead < BUFFSIZE );

      if( iOutFirst )
      {
         pOut = (char*) malloc( iBytesRead + 1 );
         memcpy( pOut, buffer, iBytesRead );
         iOutFirst = 0;
      }
      else
      {
         pOut = ( char * ) realloc( pOut, iBytesReadAll + iBytesRead + 1 );
         memcpy( pOut+iBytesReadAll, buffer, iBytesRead );
      }
      iBytesReadAll += iBytesRead;
      pOut[iBytesReadAll] = '\0';
      iBytesRead = 0;

      /* this is due to blocking that would occur otherwise so we loop on
         this condition */
      if( rc == LIBSSH2_ERROR_EAGAIN )
      {
         if( pCallback && !pCallback( pSess ) )
            break;
         hb_ssh2_WaitSocket( pSess );
      }
      else
         break;
   }

   pSess->iRes = ( rc != 0 )? -1 : 0;
   pSess->iErr = libssh2_session_last_errno( pSess->session );
   return pOut;
}

int hb_ssh2_ChannelWrite( HB_SSH2_SESSION * pSess, char *buffer, int iBufferLen )
{
   int rc, iWritten = 0;
   char * ptr = buffer;

   pSess->iInfo = 4;
   do {
       while((rc = libssh2_channel_write( pSess->channel, ptr, iBufferLen )) ==
             LIBSSH2_ERROR_EAGAIN) {
         if( pCallback && !pCallback( pSess ) )
            break;
         hb_ssh2_WaitSocket( pSess );
       }
       if( rc < 0 )
           break;
       else
       {
           iBufferLen -= rc;
           ptr += rc;
           iWritten += rc;
       }
   } while( iBufferLen > 0 );

   pSess->iRes = ( rc < 0 )? -1 : 0;
   return iWritten;

}

int hb_ssh2_ChannelPty( HB_SSH2_SESSION * pSess, const char *pty )
{
   int rc;

   pSess->iInfo = 5;
   while( ( rc = libssh2_channel_request_pty( pSess->channel,
                     pty ) ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }
   pSess->iRes = ( rc != 0 )? -1 : 0;
   if( rc != 0 )
      pSess->iErr = libssh2_session_last_errno( pSess->session );
   return pSess->iRes;
}

int hb_ssh2_ChannelShell( HB_SSH2_SESSION * pSess )
{
   int rc;

   pSess->iInfo = 6;
   while( ( rc = libssh2_channel_shell( pSess->channel ) ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }
   pSess->iRes = ( rc != 0 )? -1 : 0;
   if( rc != 0 )
      pSess->iErr = libssh2_session_last_errno( pSess->session );
   return pSess->iRes;
}

int hb_ssh2_SftpInit( HB_SSH2_SESSION * pSess )
{

   pSess->iInfo = 51;
   while( ( pSess->sftp_session = libssh2_sftp_init( pSess->session ) ) == NULL &&
      libssh2_session_last_error( pSess->session, NULL, NULL, 0 ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }
   pSess->iRes = ( pSess->sftp_session == NULL );
   pSess->iErr = libssh2_sftp_last_error( pSess->sftp_session );
   return pSess->iRes;
}

void hb_ssh2_SftpShutDown( HB_SSH2_SESSION * pSess )
{
   if( pSess->sftp_session )
      libssh2_sftp_shutdown( pSess->sftp_session );
   pSess->sftp_session = NULL;
}

HB_SSH2_SFTP_HANDLE * hb_ssh2_SftpOpenDir( HB_SSH2_SESSION * pSess, const char *sftppath )
{

   HB_SSH2_SFTP_HANDLE * pHandle = NULL;
   LIBSSH2_SFTP_HANDLE *sftp_handle;

   pSess->iInfo = 52;
   while( ( sftp_handle = libssh2_sftp_opendir( pSess->sftp_session, sftppath ) ) == NULL &&
         libssh2_session_last_error( pSess->session, NULL, NULL, 0 ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }

   if( sftp_handle ) {
      pHandle = ( HB_SSH2_SFTP_HANDLE * ) malloc( sizeof( HB_SSH2_SFTP_HANDLE ) );
      pHandle->pSess = pSess;
      pHandle->sftp_handle = sftp_handle;
   }
   pSess->iRes = ( sftp_handle == NULL );
   pSess->iErr = libssh2_sftp_last_error( pSess->sftp_session );
   return pHandle;
}

HB_SSH2_SFTP_HANDLE * hb_ssh2_SftpOpenFile( HB_SSH2_SESSION * pSess, const char *sftppath,
      unsigned long ulFlags, long lMode )
{
   HB_SSH2_SFTP_HANDLE * pHandle = NULL;
   LIBSSH2_SFTP_HANDLE * sftp_handle;

   pSess->iInfo = 53;
   while( ( sftp_handle = libssh2_sftp_open( pSess->sftp_session, sftppath, ulFlags, lMode ) ) == NULL &&
         libssh2_session_last_error( pSess->session, NULL, NULL, 0 ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }

   if( sftp_handle ) {
      pHandle = ( HB_SSH2_SFTP_HANDLE * ) malloc( sizeof( HB_SSH2_SFTP_HANDLE ) );
      pHandle->pSess = pSess;
      pHandle->sftp_handle = sftp_handle;
   }

   pSess->iRes = ( sftp_handle == NULL );
   pSess->iErr = libssh2_sftp_last_error( pSess->sftp_session );
   return pHandle;
}

void hb_ssh2_SftpClose( HB_SSH2_SFTP_HANDLE * pHandle )
{
   if( pHandle->sftp_handle )
      libssh2_sftp_close( pHandle->sftp_handle );
   pHandle->sftp_handle = NULL;
   pHandle->pSess = NULL;
   free( pHandle );
}

int hb_ssh2_SftpMkDir( HB_SSH2_SESSION * pSess, const char *sftppath, long lMode )
{
   int rc;

   pSess->iInfo = 54;
   while( ( rc = libssh2_sftp_mkdir( pSess->sftp_session, sftppath, lMode ) ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }

   return rc;
}

int hb_ssh2_SftpReadDir( HB_SSH2_SFTP_HANDLE * pHandle, char *cName, int iLen,
      unsigned long *pSize, unsigned long *pTime, unsigned long *pAttrs )
{
   LIBSSH2_SFTP_ATTRIBUTES attrs;
   int rc;

   pHandle->pSess->iInfo = 55;
   while( ( rc = libssh2_sftp_readdir( pHandle->sftp_handle, cName, iLen, &attrs ) ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pHandle->pSess ) )
         break;
      hb_ssh2_WaitSocket( pHandle->pSess );
   }

   if( rc )
   {
      *pSize = ( attrs.flags & LIBSSH2_SFTP_ATTR_SIZE ) ? attrs.filesize : 0;
      *pTime = ( attrs.flags & LIBSSH2_SFTP_ATTR_ACMODTIME ) ? attrs.mtime : 0;
      *pAttrs = ( attrs.flags & LIBSSH2_SFTP_ATTR_PERMISSIONS ) ? attrs.permissions : 0;
   }
   pHandle->pSess->iRes = ( rc != 0 )? -1 : 0;
   pHandle->pSess->iErr = libssh2_sftp_last_error( pHandle->pSess->sftp_session );
   return rc;
}

int hb_ssh2_SftpRead( HB_SSH2_SFTP_HANDLE * pHandle, char *buffer, int nBufferLen )
{

   int iBytesRead = 0;
   int rc;

   pHandle->pSess->iInfo = 56;
   do
   {
      rc = libssh2_sftp_read( pHandle->sftp_handle, buffer+iBytesRead, BUFFSIZE-iBytesRead );

      if( rc > 0 )
         iBytesRead += rc;
      if( pHandle->pSess->iNonBlocking )
         if( rc == LIBSSH2_ERROR_EAGAIN )
         {
            if( pCallback && !pCallback( pHandle->pSess ) )
               break;
            hb_ssh2_WaitSocket( pHandle->pSess );
         }
   }
   while( ( pHandle->pSess->iNonBlocking && rc == LIBSSH2_ERROR_EAGAIN ) ||
      ( rc > 0 && iBytesRead < BUFFSIZE ) );

   pHandle->pSess->iRes = ( rc < 0 )? -1 : 0;
   pHandle->pSess->iErr = libssh2_sftp_last_error( pHandle->pSess->sftp_session );

   return iBytesRead;
}

int hb_ssh2_SftpWrite( HB_SSH2_SFTP_HANDLE * pHandle, char *buffer, int nBufferLen )
{

   char * ptr = buffer;
   int rc, iWritten = 0;

   pHandle->pSess->iInfo = 57;
   do
   {
      /* write data in a loop until we block */
      rc = libssh2_sftp_write( pHandle->sftp_handle, ptr, nBufferLen );
      if( pHandle->pSess->iNonBlocking && rc == LIBSSH2_ERROR_EAGAIN )
      {
         if( pCallback && !pCallback( pHandle->pSess ) )
            break;
         hb_ssh2_WaitSocket( pHandle->pSess );
      }
      else if( rc < 0 )
         break;
      else if( rc > 0 )
      {
         ptr += rc;
         nBufferLen -= rc;
         iWritten += rc;
      }
   }
   while( nBufferLen );

   pHandle->pSess->iRes = ( rc < 0 )? -1 : 0;
   return iWritten;
}

int hb_ssh2_SftpStat( HB_SSH2_SESSION * pSess, char *cPath, int iStat_type, LIBSSH2_SFTP_ATTRIBUTES *attrs )
{
   int rc;

   pSess->iInfo = 58;
   while( ( rc = libssh2_sftp_stat_ex( pSess->sftp_session, cPath, strlen( cPath ),
      iStat_type, attrs ) ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pSess ) )
         break;
      hb_ssh2_WaitSocket( pSess );
   }
   pSess->iRes = ( rc < 0 )? -1 : 0;
   return rc;

}

#ifdef _USE_HB

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapifs.h"
#include "hbvm.h"

static PHB_DYNS s_pSymTest = NULL;

HB_FUNC( SSH2_CONNECT )
{
   int iPort = ( hb_pcount(  ) > 1 && HB_ISNUM( 2 ) ) ? hb_parni( 2 ) : 22;
   int iNonBlocking = ( hb_pcount(  ) > 2 && HB_ISLOG( 3 ) ) ? hb_parl( 3 ) : 0;

   if( HB_ISCHAR( 1 ) )
      hb_retptr( ( void * ) hb_ssh2_Connect( hb_parc( 1 ), iPort, iNonBlocking ) );
}

HB_FUNC( SSH2_LASTRES )
{
   hb_retni( ( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) )->iRes );
}

HB_FUNC( SSH2_LASTERR )
{
   hb_retni( ( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) )->iErr );
}

HB_FUNC( SSH2_CLOSE )
{
   hb_ssh2_Close( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) );
}

HB_FUNC( SSH2_EXIT )
{
   hb_ssh2_Exit();
}

HB_FUNC( SSH2_LOGIN )
{
   hb_retl( !hb_ssh2_LoginPass( ( HB_SSH2_SESSION * ) hb_parptr( 1 ),
               hb_parc( 2 ), hb_parc( 3 ) ) );
}

HB_FUNC( SSH2_CHANNEL_OPEN )
{
   hb_retni( hb_ssh2_ChannelOpen( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SSH2_CHANNEL_CLOSE )
{
   hb_ssh2_ChannelClose( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) );
}

HB_FUNC( SSH2_EXEC )
{
   hb_retni( hb_ssh2_Exec( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( SSH2_CHANNEL_READ )
{
   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) hb_parptr( 1 );
   char buffer[BUFFSIZE], *pOut = NULL;
   int iBytesRead = 0, iBytesReadAll = 0;
   int iOutFirst = 1;
   int rc;

   pSess->iInfo = 3;
   for( ;; )
   {
      do
      {
         rc = libssh2_channel_read( pSess->channel, buffer+iBytesRead, BUFFSIZE-iBytesRead );

         if( rc > 0 )
         {
            iBytesRead += rc;
         }
      }
      while( rc > 0 && iBytesRead < BUFFSIZE );

      if( iOutFirst )
      {
         pOut = (char*) hb_xgrab( iBytesRead + 1 );
         memcpy( pOut, buffer, iBytesRead );
         iOutFirst = 0;
      }
      else
      {
         pOut = ( char * ) hb_xrealloc( pOut, iBytesReadAll + iBytesRead + 1 );
         memcpy( pOut+iBytesReadAll, buffer, iBytesRead );
      }
      iBytesReadAll += iBytesRead;
      pOut[iBytesReadAll] = '\0';
      iBytesRead = 0;

      if( rc == LIBSSH2_ERROR_EAGAIN )
      {
         if( pCallback && !pCallback( pSess ) )
            break;
         hb_ssh2_WaitSocket( pSess );
      }
      else
         break;
   }

   if( pOut )
      hb_retclen_buffer( pOut, iBytesReadAll );
   else
      hb_ret();
}

HB_FUNC( SSH2_CHANNEL_READRAW )
{
   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) hb_parptr( 1 );
   char buffer[BUFFSIZE];
   int rc;

   pSess->iInfo = 3;
   rc = libssh2_channel_read( pSess->channel, buffer, BUFFSIZE );
   if( rc <= 0 )
      rc = libssh2_channel_read_stderr( pSess->channel, buffer, BUFFSIZE );

   if( rc == LIBSSH2_ERROR_EAGAIN )
      hb_ssh2_WaitSocket( pSess );

   if( rc > 0 )
      hb_retclen( buffer, rc );
   else if( rc == 0 || rc == LIBSSH2_ERROR_EAGAIN )
      hb_retc( "" );
   else
      hb_ret();
}

HB_FUNC( SSH2_CHANNEL_WRITE )
{
   int iWrite = ( hb_pcount() > 2 && HB_ISNUM(3) )? hb_parni( 3 ) : hb_parclen(2);
   int iBytesWritten;

   iBytesWritten = hb_ssh2_ChannelWrite( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), (char*) hb_parc(2), iWrite );
   hb_retni( iBytesWritten );
}

HB_FUNC( SSH2_CHANNEL_PTY )
{
   hb_retni( hb_ssh2_ChannelPty( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( SSH2_CHANNEL_SHELL )
{
   hb_retni( hb_ssh2_ChannelShell( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SSH2_SFTP_INIT )
{
   hb_retni( hb_ssh2_SftpInit( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SSH2_SFTP_SHUTDOWN )
{
   hb_ssh2_SftpShutDown( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) );
}

HB_FUNC( SSH2_SFTP_OPENDIR )
{
   hb_retptr( hb_ssh2_SftpOpenDir( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( SSH2_SFTP_CLOSE )
{
   hb_ssh2_SftpClose( ( HB_SSH2_SFTP_HANDLE * ) hb_parptr( 1 ) );
}

HB_FUNC( SSH2_SFTP_MKDIR )
{
   long lMode = (hb_pcount()>2 && HB_ISNUM(3))? hb_parnl(3) :
      LIBSSH2_SFTP_S_IRUSR | LIBSSH2_SFTP_S_IWUSR | LIBSSH2_SFTP_S_IXUSR | LIBSSH2_SFTP_S_IRGRP | LIBSSH2_SFTP_S_IROTH;
   hb_retni( hb_ssh2_SftpMkDir( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ), lMode ) );
}

HB_FUNC( SSH2_SFTP_READDIR )
{
   char mem[512];
   unsigned long ulSize;
   unsigned long ulTime;
   unsigned long ulAttrs;
   int rc = hb_ssh2_SftpReadDir( ( HB_SSH2_SFTP_HANDLE * ) hb_parptr( 1 ), mem,
         sizeof( mem ),
         &ulSize, &ulTime, &ulAttrs );

   if( rc > 0 )
   {
      hb_stornl( ulSize, 2 );
      hb_stortdt( ulTime / 86400 + 2440588, ( ulTime % 86400 ) * 1000, 3 );
      hb_stornl( ulAttrs, 4 );
      hb_retc( mem );
   }
   else
      hb_ret(  );
}

HB_FUNC( SSH2_SFTP_OPENFILE )
{
   unsigned long ulFlags = (hb_pcount()>2 && HB_ISNUM(3))? ( unsigned long ) hb_parnl( 3 ) : 0;
   long ulMode = (hb_pcount()>3 && HB_ISNUM(4))? ( unsigned long ) hb_parnl( 4 ) : 0;
   unsigned long ulFlags1 = 0;
   long ulMode1 = 0;
   unsigned long a1[] = { FO_WRITE, FO_CREAT, FO_TRUNC, FO_EXCL };
   unsigned long a2[] = { LIBSSH2_FXF_WRITE, LIBSSH2_FXF_CREAT, LIBSSH2_FXF_TRUNC, LIBSSH2_FXF_EXCL };
   unsigned long a3[] = { HB_FA_RUSR, HB_FA_WUSR, HB_FA_XUSR, HB_FA_RGRP, HB_FA_WGRP, HB_FA_XGRP, HB_FA_ROTH, HB_FA_WOTH, HB_FA_XOTH };
   unsigned long a4[] = { LIBSSH2_SFTP_S_IRUSR, LIBSSH2_SFTP_S_IWUSR, LIBSSH2_SFTP_S_IXUSR, LIBSSH2_SFTP_S_IRGRP,
      LIBSSH2_SFTP_S_IWGRP, LIBSSH2_SFTP_S_IXGRP, LIBSSH2_SFTP_S_IROTH, LIBSSH2_SFTP_S_IWOTH, LIBSSH2_SFTP_S_IXOTH };
   int i;

   for( i = 0; i < 4; i ++ )
      if( (ulFlags & a1[i]) )
         ulFlags1 |= a2[i];
   if( !ulFlags1 )
      ulFlags1 = LIBSSH2_FXF_READ;
   for( i = 0; i < 9; i ++ )
      if( (ulMode & a3[i]) )
         ulMode1 |= a4[i];

   hb_retptr( hb_ssh2_SftpOpenFile( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ),
         ulFlags1, ulMode1 ) );
}

HB_FUNC( SSH2_SFTP_READ )
{
   char buffer[BUFFSIZE];
   int iBytesRead;

   iBytesRead = hb_ssh2_SftpRead( ( HB_SSH2_SFTP_HANDLE * ) hb_parptr( 1 ), buffer, BUFFSIZE );
   if( iBytesRead >= 0 )
      hb_retclen( buffer, iBytesRead );
   else
      hb_ret();
}

HB_FUNC( SSH2_SFTP_READLEN )
{
   int iBytesRead, nToRead = hb_parni( 2 );
   char * buffer = ( char * ) hb_xgrab( nToRead + 1 );

   iBytesRead = hb_ssh2_SftpRead( ( HB_SSH2_SFTP_HANDLE * ) hb_parptr( 1 ), buffer, nToRead );
   if( iBytesRead >= 0 )
      hb_retclen( buffer, iBytesRead );
   else
      hb_retc_null();
   hb_xfree( buffer );
}

HB_FUNC( SSH2_SFTP_READRAW )
{
   HB_SSH2_SFTP_HANDLE * pHandle = ( HB_SSH2_SFTP_HANDLE * ) hb_parptr( 1 );
   char buffer[BUFFSIZE];
   int rc;

   rc = libssh2_sftp_read( pHandle->sftp_handle, buffer, BUFFSIZE );
   if( rc == LIBSSH2_ERROR_EAGAIN )
      hb_ssh2_WaitSocket( pHandle->pSess );
   hb_storni( rc, 2 );
   if( rc > 0 )
      hb_retclen( buffer, rc );
   else
      hb_retc_null();
}

HB_FUNC( SSH2_SFTP_WRITE )
{
   int iWrite = ( hb_pcount() > 2 && HB_ISNUM(3) )? hb_parni( 3 ) : hb_parclen(2);
   int iBytesWritten;

   iBytesWritten = hb_ssh2_SftpWrite( ( HB_SSH2_SFTP_HANDLE * ) hb_parptr( 1 ), (char*) hb_parc(2), iWrite );
   hb_retni( iBytesWritten );
}

HB_FUNC( SSH2_SFTP_SEEK )
{

   libssh2_sftp_seek( ( (HB_SSH2_SFTP_HANDLE *) hb_parptr(1) )->sftp_handle, hb_parni( 2 ) );
}

HB_FUNC( SSH2_SFTP_STAT )
{
   int rc;
   LIBSSH2_SFTP_ATTRIBUTES attrs;

   rc = hb_ssh2_SftpStat( (HB_SSH2_SESSION *) hb_parptr(1), (char *) hb_parc(2),
      LIBSSH2_SFTP_STAT, &attrs );

   if( rc == 0 )
   {
      if( hb_pcount() > 2 )
         hb_stornl( attrs.filesize, 3 );
      if( hb_pcount() > 3 )
         //hb_stornl( attrs.mtime, 4 );
         hb_stortdt( attrs.mtime / 86400 + 2440588, ( attrs.mtime % 86400 ) * 1000, 4 );
      if( hb_pcount() > 4 )
         hb_stornl( attrs.permissions, 5 );
   }
   hb_retni( rc );
}

HB_FUNC( SSH2_SFTP_FSTAT )
{
   HB_SSH2_SFTP_HANDLE * pHandle = (HB_SSH2_SFTP_HANDLE *) hb_parptr(1);
   int rc;
   LIBSSH2_SFTP_ATTRIBUTES attrs;

   //rc = libssh2_sftp_fstat( pHandle->sftp_handle, &attrs );
   pHandle->pSess->iInfo = 58;
   while( ( rc = libssh2_sftp_fstat( pHandle->sftp_handle, &attrs ) ) == LIBSSH2_ERROR_EAGAIN )
   {
      if( pCallback && !pCallback( pHandle->pSess ) )
         break;
      hb_ssh2_WaitSocket( pHandle->pSess );
   }

   if( rc == 0 )
   {
      if( hb_pcount() > 1 )
         hb_stornl( attrs.filesize, 2 );
      if( hb_pcount() > 2 )
         //hb_stornl( attrs.mtime, 4 );
         hb_stortdt( attrs.mtime / 86400 + 2440588, ( attrs.mtime % 86400 ) * 1000, 3 );
      if( hb_pcount() > 3 )
         hb_stornl( attrs.permissions, 4 );
   }
   hb_retni( rc );
}

HB_FUNC( SSH2_SFTP_FILEDELETE )
{
   int rc;

   rc =  libssh2_sftp_unlink( ((HB_SSH2_SESSION *) hb_parptr(1))->sftp_session, hb_parc(2) );
   hb_retni( rc );
}

HB_FUNC( SSH2_SFTP_DIRDELETE )
{
   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) hb_parptr( 1 );
   int rc;

   rc =  libssh2_sftp_rmdir( pSess->sftp_session, hb_parc(2) );
   hb_retni( rc );
}

HB_FUNC( SSH2_SFTP_RENAME )
{
   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) hb_parptr( 1 );
   int rc;

   rc =  libssh2_sftp_rename( pSess->sftp_session, hb_parc(2), hb_parc(3) );
   hb_retni( rc );
}

HB_FUNC( SSH2_VERSION )
{
   hb_retc( libssh2_version( LIBSSH2_VERSION_NUM ) );
}

static int s_cbProc( HB_SSH2_SESSION * pSess )
{

   if( s_pSymTest && hb_dynsymIsFunction( s_pSymTest ) )
   {
      hb_vmPushDynSym( s_pSymTest );
      hb_vmPushNil();   /* places NIL at self */
      hb_vmPushPointer( ( void * )pSess );
      hb_vmDo( 1 );     /* where iArgCount is the number of pushed parameters */
      return hb_parni( -1 );
   }
   else
      return 1;
}

HB_FUNC( SSH2_SETCALLBACK )
{
   if( hb_pcount() > 0 && HB_ISCHAR(1) )
   {
      s_pSymTest = hb_dynsymGetCase( hb_parc(1) );
      hb_ssh2_setCallback( s_cbProc );
   }
   else
   {
      s_pSymTest = NULL;
      hb_ssh2_setCallback( NULL );
   }
}

HB_FUNC( SSH2_GETINFO )
{
   int i = (hb_pcount()==0)? 1 : hb_parni(1);

   if( i == 1 )
      hb_retni( ( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) )->iInfo );
   else
      hb_retnl( ( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) )->lInfo );
}

HB_FUNC( SSH2_TIMEOUT )
{
   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) hb_parptr( 1 );

   pSess->timeout.tv_sec = hb_parni(2);
   pSess->timeout.tv_usec = hb_parni(3);

}

#endif
