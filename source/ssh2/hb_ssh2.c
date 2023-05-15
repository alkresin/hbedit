
#include "hb_ssh2.h"

#define  BUFFSIZE   8192

static short int iSsh2Init = 0;

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

int hb_ssh2_WaitSocket( int socket_fd, LIBSSH2_SESSION * session )
{
   struct timeval timeout;
   int rc;
   fd_set fd;
   fd_set *writefd = NULL;
   fd_set *readfd = NULL;
   int dir;

   timeout.tv_sec = 10;
   timeout.tv_usec = 0;

   FD_ZERO( &fd );

   FD_SET( socket_fd, &fd );

   /* now make sure we wait in the correct direction */
   dir = libssh2_session_block_directions( session );

   if( dir & LIBSSH2_SESSION_BLOCK_INBOUND )
      readfd = &fd;

   if( dir & LIBSSH2_SESSION_BLOCK_OUTBOUND )
      writefd = &fd;

   rc = select( socket_fd + 1, readfd, writefd, NULL, &timeout );

   return rc;
}

HB_SSH2_SESSION *hb_ssh2_Connect( const char *hostname, int iPort, int iNonBlocking )
{

   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) malloc( sizeof( HB_SSH2_SESSION ) );
   unsigned long hostaddr;
   struct sockaddr_in sin;
   int rc;

   memset( pSess, 0, sizeof( HB_SSH2_SESSION ) );

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

   hb_ssh2_SftpClose( pSess );
   hb_ssh2_CloseChannel( pSess );

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

int hb_ssh2_OpenChannel( HB_SSH2_SESSION * pSess )
{
   /* Exec non-blocking on the remove host */
   while( ( pSess->channel =
               libssh2_channel_open_session( pSess->session ) ) == NULL &&
         libssh2_session_last_error( pSess->session, NULL, NULL,
               0 ) == LIBSSH2_ERROR_EAGAIN )
   {
      hb_ssh2_WaitSocket( pSess->sock, pSess->session );
   }
   pSess->iRes = ( pSess->channel == NULL );
   pSess->iErr = libssh2_session_last_errno( pSess->session );
   return pSess->iRes;
}

void hb_ssh2_CloseChannel( HB_SSH2_SESSION * pSess )
{

   if( !pSess->channel )
      return;
   while( libssh2_channel_close( pSess->channel ) == LIBSSH2_ERROR_EAGAIN )
      hb_ssh2_WaitSocket( pSess->sock, pSess->session );

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

   while( ( rc = libssh2_channel_exec( pSess->channel,
                     commandline ) ) == LIBSSH2_ERROR_EAGAIN )
      hb_ssh2_WaitSocket( pSess->sock, pSess->session );
   if( rc != 0 )
   pSess->iRes = ( rc != 0 )? -1 : 0;
   pSess->iErr = libssh2_session_last_errno( pSess->session );
   return pSess->iRes;
}

char * hb_ssh2_ChannelRead( HB_SSH2_SESSION * pSess )
{

   char buffer[BUFFSIZE], *pOut = NULL;
   int iBytesRead = 0, iBytesReadAll = 0;
   int iOutFirst = 1;
   int rc;

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
         hb_ssh2_WaitSocket( pSess->sock, pSess->session );
      }
      else
         break;
   }

   pSess->iRes = ( rc != 0 )? -1 : 0;
   pSess->iErr = libssh2_session_last_errno( pSess->session );
   return pOut;
}

int hb_ssh2_SftpInit( HB_SSH2_SESSION * pSess )
{

   while( ( pSess->sftp_session =
               libssh2_sftp_init( pSess->session ) ) == NULL &&
         libssh2_session_last_error( pSess->session, NULL, NULL,
               0 ) == LIBSSH2_ERROR_EAGAIN )
   {
      hb_ssh2_WaitSocket( pSess->sock, pSess->session );
   }
   pSess->iRes = ( pSess->sftp_session == NULL );
   pSess->iErr = libssh2_sftp_last_error( pSess->sftp_session );
   return pSess->iRes;
}

int hb_ssh2_SftpOpenDir( HB_SSH2_SESSION * pSess, const char *sftppath )
{
   pSess->sftp_handle = libssh2_sftp_opendir( pSess->sftp_session, sftppath );
   pSess->iRes = ( pSess->sftp_handle == NULL );
   pSess->iErr = libssh2_sftp_last_error( pSess->sftp_session );
   return pSess->iRes;
}

void hb_ssh2_SftpClose( HB_SSH2_SESSION * pSess )
{
   if( pSess->sftp_handle )
      libssh2_sftp_close( pSess->sftp_handle );
   pSess->sftp_handle = NULL;
}

int hb_ssh2_SftpOpenFile( HB_SSH2_SESSION * pSess, const char *sftppath,
      unsigned long ulFlags, long lMode )
{
   pSess->sftp_handle =
         libssh2_sftp_open( pSess->sftp_session, sftppath, ulFlags, lMode );
   pSess->iRes = ( pSess->sftp_handle == NULL );
   pSess->iErr = libssh2_sftp_last_error( pSess->sftp_session );
   return pSess->iRes;
}

int hb_ssh2_SftpReadDir( HB_SSH2_SESSION * pSess, char *cName, int iLen,
      unsigned long *pSize, unsigned long *pTime, unsigned long *pAttrs )
{
   LIBSSH2_SFTP_ATTRIBUTES attrs;
   int rc = libssh2_sftp_readdir( pSess->sftp_handle, cName, iLen, &attrs );
   if( rc )
   {
      *pSize = ( attrs.flags & LIBSSH2_SFTP_ATTR_SIZE ) ? attrs.filesize : 0;
      *pTime = ( attrs.flags & LIBSSH2_SFTP_ATTR_ACMODTIME ) ? attrs.mtime : 0;
      *pAttrs = ( attrs.flags & LIBSSH2_SFTP_ATTR_PERMISSIONS ) ? attrs.permissions : 0;
   }
   pSess->iRes = ( rc != 0 )? -1 : 0;
   pSess->iErr = libssh2_sftp_last_error( pSess->sftp_session );
   return rc;
}

int hb_ssh2_SftpRead( HB_SSH2_SESSION * pSess, char *buffer, int nBufferLen )
{

   int iBytesRead = 0;
   int rc;

   do
   {
      rc = libssh2_sftp_read( pSess->sftp_handle, buffer+iBytesRead, BUFFSIZE-iBytesRead );

      if( rc > 0 )
         iBytesRead += rc;
   }
   while( rc > 0 && iBytesRead < BUFFSIZE );

   pSess->iRes = ( rc < 0 )? -1 : 0;
   pSess->iErr = libssh2_sftp_last_error( pSess->sftp_session );

   return iBytesRead;
}

int hb_ssh2_SftpWrite( HB_SSH2_SESSION * pSess, char *buffer, int nBufferLen )
{

   char * ptr = buffer;
   int rc, iWritten = 0;

   do
   {
      /* write data in a loop until we block */
      rc = libssh2_sftp_write( pSess->sftp_handle, ptr, nBufferLen );
      if( rc < 0 )
         break;
      ptr += rc;
      nBufferLen -= rc;
      iWritten += rc;
   }
   while( nBufferLen );

   pSess->iRes = ( rc < 0 )? -1 : 0;
   return iWritten;
}

#ifdef _USE_HB

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicdp.h"

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

HB_FUNC( SSH2_OPENCHANNEL )
{
   hb_retni( hb_ssh2_OpenChannel( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SSH2_CLOSECHANNEL )
{
   hb_ssh2_CloseChannel( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) );
}

HB_FUNC( SSH2_EXEC )
{
   hb_retni( hb_ssh2_Exec( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( SSH2_SFTP_INIT )
{
   hb_retni( hb_ssh2_SftpInit( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) ) );
}

HB_FUNC( SSH2_SFTP_OPENDIR )
{
   hb_retni( hb_ssh2_SftpOpenDir( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( SSH2_SFTP_CLOSE )
{
   hb_ssh2_SftpClose( ( HB_SSH2_SESSION * ) hb_parptr( 1 ) );
}

HB_FUNC( SSH2_SFTP_READDIR )
{
   char mem[512];
   unsigned long ulSize;
   unsigned long ulTime;
   unsigned long ulAttrs;
   int rc = hb_ssh2_SftpReadDir( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), mem,
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
   hb_retni( hb_ssh2_SftpOpenFile( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ),
         ( unsigned long ) hb_parnl( 3 ), hb_parnl( 4 ) ) );
}

HB_FUNC( SSH2_SFTP_EXEC )
{
   hb_retni( hb_ssh2_Exec( ( HB_SSH2_SESSION * ) hb_parptr( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( SSH2_CHANNELREAD )
{
   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) hb_parptr( 1 );
   char buffer[BUFFSIZE], *pOut = NULL;
   int iBytesRead = 0, iBytesReadAll = 0;
   int iOutFirst = 1;
   int rc;

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

      /* this is due to blocking that would occur otherwise so we loop on
         this condition */
      if( rc == LIBSSH2_ERROR_EAGAIN )
      {
         hb_ssh2_WaitSocket( pSess->sock, pSess->session );
      }
      else
         break;
   }

   if( pOut )
      hb_retclen_buffer( pOut, iBytesReadAll );
   else
      hb_ret();

}

HB_FUNC( SSH2_SFTPREAD )
{
   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) hb_parptr( 1 );
   char buffer[BUFFSIZE];
   int iBytesRead;

   iBytesRead = hb_ssh2_SftpRead( pSess, buffer, BUFFSIZE );
   if( iBytesRead >= 0 )
      hb_retclen( buffer, iBytesRead );
   else
      hb_ret();
}

HB_FUNC( SSH2_SFTPWRITE )
{
   HB_SSH2_SESSION *pSess = ( HB_SSH2_SESSION * ) hb_parptr( 1 );
   int iWrite = ( hb_pcount() > 2 && HB_ISNUM(2) )? hb_parni( 3 ) : hb_parclen(2);
   int iBytesWritten;

   iBytesWritten = hb_ssh2_SftpWrite( pSess, (char*) hb_parc(2), iWrite );
   hb_retni( iBytesWritten );
}

#endif
