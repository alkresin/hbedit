/*
 * A set of C functions for a text editor
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#if defined(__linux__) || defined(__unix__)
#include <unistd.h>
#else
#include <io.h>
#endif
#include <stdio.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicdp.h"

static HB_SIZE nLastX, nLastPos;
static char* shm_file_name = "hbedit_cb";

#include "hbapifs.h"
static void _writelog( const char * sFile, int n, const char * s, ... )
{

   if( !sFile )
      return;

   if( n )
   {
      HB_FHANDLE handle;
      if( hb_fsFile( sFile ) )
         handle = hb_fsOpen( sFile, FO_WRITE );
      else
         handle = hb_fsCreate( sFile, 0 );

      hb_fsSeek( handle,0, SEEK_END );
      hb_fsWrite( handle, s, n );
      hb_fsWrite( handle, "\r\n", 2 );
      hb_fsClose( handle );
   }
   else
   {
      FILE * hFile = hb_fopen( sFile, "a" );

      va_list ap;
      if( hFile )
      {
         va_start( ap, s );
         vfprintf( hFile, s, ap );
         va_end( ap );
         fclose( hFile );
      }
   }
}


static int cedi_Utf8CharLen( unsigned char ucChar )
{
   int n;

   if( ucChar >= 0xc0 )
   {
      if( ucChar < 0xe0 )
         n = 2;
      else if( ucChar < 0xf0 )
         n = 3;
      else if( ucChar < 0xf8 )
         n = 4;
   }
   else
      n = 1;
   return n;
}

static HB_WCHAR hb_UTF8StringPeek( const char * pSrc, HB_SIZE nLen, HB_SIZE nPos )
{
   nLastX = 0;

   if( nLen )
   {
      HB_SIZE ul;
      HB_WCHAR wc = 0;
      int n = 0;

      for( ul = 0; ul < nLen && nPos; )
      {
         if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ ul ], &n, &wc ) )
            ++ul;
         if( n == 0 )
            --nPos;
      }

      if( ul < nLen )
      {
         n = 0;
         nLastX = ul;
         do
         {
            if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ ul ], &n, &wc ) )
               ++ul;
            if( n == 0 )
            {
               return wc;
            }
         }
         while( ul < nLen );
      }
   }

   return 0;
}

HB_FUNC( CEDI_PEEK )
{
   int bUtf8 = hb_parl( 1 );
   const char * szString = hb_parc( 2 );
   HB_SIZE nLen = hb_parclen( 2 );
   HB_SIZE nPos = hb_parns( 3 );

   if( bUtf8 )
   {
      HB_SIZE nStart = HB_ISNIL(4)? 0 : hb_parns( 4 ) - 1;
      HB_SIZE nStartPos = HB_ISNIL(5)? 1 : hb_parns( 5 );

      nLastPos = nPos;
      if( nStartPos > nPos )
      {
         nStart = 0; nStartPos = 1;
      }
      szString += nStart;
      if( nLen > nStart && nPos >= nStartPos )
      {
         nLen -= nStart;
         nPos -= nStartPos;
         if( nPos <= nLen )
         {
            char utf8Char[ HB_MAX_CHAR_LEN ];
            int iLen = hb_cdpU16CharToUTF8( utf8Char, hb_UTF8StringPeek( szString, nLen, nPos ) );
            nLastX += nStart;
            hb_stornl( nLastX+1, 4 );
            hb_stornl( nLastPos, 5 );
            hb_retclen( utf8Char, iLen );
            //hb_retnint( hb_UTF8StringPeek( szString, nLen, nPos - 1 ) );
         }
         else
            hb_retc( 0 );
      }
      else
         hb_retc( 0 );
   }
   else
   {
      if( nPos > 0 && nPos <= nLen )
         hb_retclen( szString+nPos-1, 1 );
      else
         hb_retc( 0 );
   }
}

HB_FUNC( CEDI_GETLASTPOS )
{
   hb_retnl( nLastX + 1 );
}

/*
 * cedi_Strncmp( cBuf1, nStart1, cBuf2, nStart2, nLen ) --> lEqual
 */
HB_FUNC( CEDI_STRNCMP )
{

   hb_retl( memcmp( hb_parc(1)+hb_parni(2)-1, hb_parc(3)+hb_parni(4)-1, hb_parni(5) ) == 0 );
}

HB_FUNC( CEDI_SUBSTR )
{
   int bUtf8 = hb_parl( 1 );
   const char * szString = hb_parc( 2 );
   HB_SIZE nLen = hb_parclen( 2 );
   HB_SIZE nFrom = hb_parns( 3 );
   int iPCount = hb_pcount();
   HB_SIZE nCount = (iPCount<4 ||HB_ISNIL(4))? ( HB_ISIZ ) nLen : hb_parns( 4 );

   if( bUtf8 )
   {
      HB_SIZE nStart = HB_ISNIL(5)? 0 : hb_parns( 5 ) - 1;
      HB_SIZE nStartPos = HB_ISNIL(6)? 1 : hb_parns( 6 );
      char * szDest = NULL;
      HB_SIZE nDest = 0;

      if( nStartPos > nFrom )
      {
         nStart = 0; nStartPos = 1;
      }
      szString += nStart;
      nLen -= nStart;
      nFrom -= nStartPos;

      //if( nFrom )
      //   --nFrom;

      if( nLen > ( HB_SIZE ) nFrom && nCount > 0 )
         szDest = hb_cdpUTF8StringSubstr( szString, nLen, nFrom, nCount, &nDest );
      if( szDest )
         hb_retclen_buffer( szDest, nDest );
      else
         hb_retc_null();
   }
   else
   {
      if( nFrom > 0 )
      {
         if( --nFrom > nLen )
            nCount = 0;
      }
      if( nCount > 0 )
      {
         nLen -= nFrom;
         if( nCount > nLen )
            nCount = nLen;
      }
      if( nCount > 0 )
      {
         if( nFrom == 0 && nCount == nLen )
            hb_retclen( szString, nCount );
         else
            hb_retclen( szString + nFrom, nCount );
      }
      else
         hb_retc_null();
   }
}

/*
 * cedi_strpbrk( szFind, szString[, iStartPos][, iEndPos] ) -> iFoundPos
 * Returns a position of one of chars from szFind in szString (from iStartPos till iEndPos ).
 */
HB_FUNC( CEDI_STRPBRK )
{
   const char * szFind = hb_parc( 1 );
   const char * szString = hb_parc( 2 );
   char * p1 = (char*) szString, * p2;
   unsigned long int ulStart, ulEnd;

   if( HB_ISNUM( 3 ) )
   {
      ulStart = hb_parni( 3 ) - 1;
      p1 += ulStart;
   }
   else
      ulStart = 0;
   if( HB_ISNUM( 4 ) )
      ulEnd = hb_parni( 4 ) - 1;
   else
      ulEnd = 0;

   while( *p1 && ( ulEnd==0 || ++ulStart <= ulEnd ) )
   {
      p2 = (char*) szFind;
      while( *p2 )
      {
         if( *p2 == *p1 )
         {
            hb_retni( p1 - (char*)szString + 1 );
            return;
         }
         p2 ++;
      }
      p1 ++;
   }
   hb_retni( -1 );
}

/*
 * cedi_utf8pbrk( szFind, szString[, iStartPos][, iEndPos][, iPos] ) -> iFoundPos
 * Returns a position of one of chars from szFind in utf8 string szString (from iStartPos till iEndPos ).
 */
HB_FUNC( CEDI_UTF8PBRK )
{
   const char * szFind = hb_parc( 1 );
   const char * szString = hb_parc( 2 );
   char * p1 = (char*) szString, * p2;
   unsigned long int ulStart, ulEnd;
   int iPos = ( HB_ISNUM( 5 ) )? hb_parni( 5 ) : 1;
   int iLen;

   if( HB_ISNUM( 3 ) )
   {
      ulStart = hb_parni( 3 ) - 1;
      p1 += ulStart;
   }
   else
      ulStart = 0;
   if( HB_ISNUM( 4 ) )
      ulEnd = hb_parni( 4 ) - 1;
   else
      ulEnd = 0;

   while( *p1 && ( ulEnd==0 || ++ulStart <= ulEnd ) )
   {
      iLen = cedi_Utf8CharLen( *p1 );
      if( iLen > 1 )
      {
         do
         {
            p1 ++;
            if( !(*p1) )
            {
               hb_retni( -2 );
               return;
            }
         }
         while( --iLen );
      }
      else
      {
         p2 = (char*) szFind;
         while( *p2 )
         {
            if( *p2 == *p1 )
            {
               hb_storni( iPos, 5 );
               hb_retni( p1 - (char*)szString + 1 );
               return;
            }
            p2 ++;
         }
         p1 ++;
      };
      iPos ++;
   }
   hb_retni( -1 );
}

HB_FUNC( CEDI_UTF8POS )
{
   const char * szString = hb_parc( 1 );
   int iLen = hb_parni( 2 ) - 1, i = 0;
   int iPos = 0;

   while( szString[i] && i <= iLen )
   {
      i += cedi_Utf8CharLen( szString[i] );
      iPos ++;
   }
   hb_retni( iPos );
}

HB_FUNC( CEDI_STRSKIPCHARS )
{
   const char * szString = hb_parc( 1 );
   int nFrom = hb_parns( 2 ) - 1;
   int bDesc = ( HB_ISNIL( 3 ) ) ? 1 : hb_parl( 3 );

   if( bDesc )
      while( szString[nFrom] && ( szString[nFrom] == ' ' || szString[nFrom] == '\t' ) )
         nFrom ++;
   else
      while( szString[nFrom] && ( szString[nFrom] == ' ' || szString[nFrom] == '\t' ) )
         nFrom --;
   hb_retni( nFrom + 1 );
}

HB_FUNC( CEDI_REDIRON )
{
   int istd = ( HB_ISNIL( 1 ) ) ? 1 : hb_parni( 1 );
   int fd;

   fflush( ( istd == 1 ) ? stdout : stderr );
   fd = dup( fileno( ( istd == 1 ) ? stdout : stderr ) );
   freopen( hb_parc( 2 ), "w", ( istd == 1 ) ? stdout : stderr );
   hb_retni( fd );
}

HB_FUNC( CEDI_REDIROFF )
{
   int istd = ( HB_ISNIL( 1 ) ) ? 1 : hb_parni( 1 );
   int fd;

   fflush( ( istd == 1 ) ? stdout : stderr );

   if( HB_ISNIL( 2 ) )
   {
      fclose( ( istd == 1 ) ? stdout : stderr );
   }
   else
   {
      fd = hb_parni( 2 );
      dup2( fd, fileno( ( istd == 1 ) ? stdout : stderr ) );
      close( fd );
      clearerr( ( istd == 1 ) ? stdout : stderr );
   }
}

#define BUFSIZE  16384

#if defined(__linux__) || defined(__unix__)

#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <string.h>

HB_FUNC( CEDI_SHMREAD )
{
   int shm, iLen;
   char *addr;
   struct stat stat;

   if( (shm = shm_open(shm_file_name, O_RDONLY, 0777)) == -1 ) {
      return;
   }
   fstat( shm, &stat );
   iLen = stat.st_size;
   addr = mmap(0, iLen, PROT_READ, MAP_SHARED, shm, 0);
   if( addr == (char*)-1 ) {
      return;
   }

   hb_retc( addr );

   munmap(addr, iLen);
   close(shm);
   shm_unlink( shm_file_name );
}

HB_FUNC( CEDI_CHOWN )
{
   struct stat info;
   struct passwd *pw;
   //struct group  *gr = getgrgid(info.st_gid);

   stat( hb_parc(1), &info);
   pw = getpwuid( info.st_uid );
   if( pw && pw->pw_name )
      hb_retc_buffer( pw->pw_name );
   else
      hb_retc_null();
}

HB_FUNC( CEDI_CHGRP )
{
   struct stat info;
   struct group *gr;

   stat( hb_parc(1), &info);
   gr = getgrgid(info.st_gid);
   if( gr && gr->gr_name )
      hb_retc_buffer( gr->gr_name );
   else
      hb_retc_null();
}

HB_FUNC( CEDI_RUNBACKGROUNDAPP )
{
   //hb_retl( g_spawn_command_line_async( hb_parc(1), NULL ) );
   char scmd[2048];
   int nLen = hb_parclen( 1 );

   memcpy( scmd, hb_parc(1), nLen );
   scmd[nLen] = ' ';
   scmd[nLen+1] = '&';
   scmd[nLen+2] = '\0';
   system( scmd );
}

HB_FUNC( CEDI_RUNCONSOLEAPP )
{
   /* Ensure that output of command does interfere with stdout */
   fflush( stdin );
   FILE *cmd_file = ( FILE * ) popen( hb_parc( 1 ), "r" );
   FILE *hOut;
   char buf[BUFSIZE], *pOut;
   int bytes_read, read_all = 0, iOutExist = 0, iOutFirst = 1, iExitCode;

   if( !cmd_file )
   {
      hb_retni( -1 );
      return;
   }

   if( !HB_ISNIL( 2 ) )
   {
      hOut = fopen( hb_parc( 2 ), "w" );
      iOutExist = 1;
   }
   else if( HB_ISBYREF( 3 ) )
      iOutExist = 2;

   do
   {
      bytes_read = fread( buf, sizeof( char ), BUFSIZE, cmd_file );
      if( iOutExist == 1 )
         fwrite( buf, 1, bytes_read, hOut );
      else if( iOutExist == 2 )
      {
         read_all += bytes_read;
         if( iOutFirst )
         {
            pOut = (char*) hb_xgrab( bytes_read + 1 );
            memcpy( pOut, buf, bytes_read );
            iOutFirst = 0;
         }
         else
         {
            pOut = ( char * ) hb_xrealloc( pOut, read_all + 1 );
            memcpy( pOut+read_all-bytes_read, buf, bytes_read );
         }
      }
   }
   while( bytes_read == BUFSIZE );

   iExitCode = pclose( cmd_file );
   if( iOutExist == 1 )
      fclose( hOut );
   else if( iOutExist == 2 )
      hb_storclen_buffer( pOut, read_all, 3 );

   hb_retni( iExitCode );
}

HB_FUNC( CEDI_RUNAPP )
{
#ifdef GTHWG
   hb_retl( g_spawn_command_line_async( hb_parc(1), NULL ) );
#else
   hb_retl( 0 );
#endif
}

#define  CMD_ARGS_MAX 32
#define  CMD_LINE_MAX 256

typedef struct {

   pid_t pid;
   int pipe_stdin[2];
   int pipe_stdout[2];
   int pipe_stderr[2];
   fd_set fds;
   char *args[CMD_ARGS_MAX];
   char szCmdLine[CMD_LINE_MAX];
   int iRes;

} PROCESS_HANDLES;

HB_FUNC( CEDI_STARTCONSOLEAPP )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_xgrab( sizeof(PROCESS_HANDLES) );
   pid_t pid;
   int pipe_stdin[2];
   int pipe_stdout[2];
   int pipe_stderr[2];
   char *ptr;
   int iArgs = 0, iCmdLen;
   int iStart;

   memset( pHandles, 0, sizeof( PROCESS_HANDLES ) );

   if( !HB_ISCHAR( 1 ) ) {
       pHandles->iRes = 1; hb_retptr( (void*) pHandles ); return;
   }
   if( (iCmdLen = hb_parclen( 1 )) >= CMD_LINE_MAX ) {
       pHandles->iRes = 2; hb_retptr( (void*) pHandles ); return;
   }

   memcpy( pHandles->szCmdLine, hb_parc(1), iCmdLen );
   pHandles->szCmdLine[iCmdLen] = '\0';
   ptr = pHandles->szCmdLine;
   pHandles->args[0] = ptr;
   iStart = 1;
   while( *ptr ) {
      if( *ptr == ' ' ) {
         *ptr = '\0';
         iStart = 0;
      } else {
         if( !iStart ) {
            iStart = 1;
            if( ++iArgs >= CMD_ARGS_MAX ) {
                pHandles->iRes = 3; hb_retptr( (void*) pHandles ); return;
            }
            pHandles->args[iArgs] = ptr;
         }
         if( *ptr == '\"' ) {
            ptr++;
            while( *ptr && *ptr != '\"' ) ptr++;
            if( *pHandles->args[iArgs] == '\"' ) {
               pHandles->args[iArgs]++;
               *ptr-- = ' ';
            }
            if( ! *ptr )
               break;
         }
      }
      ptr ++;
   }
   pHandles->args[++iArgs] = NULL;

   if( pipe( pipe_stdin ) == -1 || pipe( pipe_stdout ) == -1 || pipe( pipe_stderr ) == -1) {
       pHandles->iRes = 4; hb_retptr( (void*) pHandles ); return;
   }

   pid = fork();
   if( pid == -1 ) {
       pHandles->pid = -1;
       pHandles->iRes = 2; hb_retptr( (void*) pHandles ); return;
   } else if( pid == 0 ) {
       // Child process code
       close( pipe_stdin[1] );
       close( pipe_stdout[0] );
       close( pipe_stderr[0] );
       if( dup2( pipe_stdin[0], STDIN_FILENO ) == -1 ||
           dup2( pipe_stdout[1], STDOUT_FILENO ) == -1 ||
           dup2( pipe_stderr[1], STDERR_FILENO ) == -1 ) {
           exit(EXIT_FAILURE);
       }
       close( pipe_stdin[0] );
       close( pipe_stdout[1] );
       close( pipe_stderr[1] );

       execvp( pHandles->args[0], pHandles->args );
       exit(EXIT_FAILURE);
   } else {
       // Parent process code
       close( pipe_stdin[0] );
       close( pipe_stdout[1] );
       close( pipe_stderr[1] );

       pHandles->pid = pid;
       pHandles->pipe_stdin[1] = pipe_stdin[1];
       pHandles->pipe_stdout[0] = pipe_stdout[0];
       pHandles->pipe_stderr[0] = pipe_stderr[0];

       FD_ZERO( &(pHandles->fds) );
       FD_SET( pHandles->pipe_stdout[0], &(pHandles->fds) );
       FD_SET( pHandles->pipe_stderr[0], &(pHandles->fds) );
   }

   hb_retptr( (void*) pHandles );
}

HB_FUNC( CEDI_RETURNERRCODE )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   hb_retni( pHandles->iRes );
}

HB_FUNC( CEDI_READFROMCONSOLEAPP )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   ssize_t bytes_read;
   char buffer[BUFSIZE];
   struct timeval tv = {0,0};
   int result, status;
   fd_set fds;

   fds = pHandles->fds;
   tv.tv_usec = 1000;

   if( ( result = select( pHandles->pipe_stderr[0]+1, &fds, NULL, NULL, &tv ) ) < 0 ) {
       hb_ret();
       return;
   } else if( result > 0 ) {
      if( FD_ISSET( pHandles->pipe_stdout[0], &fds ) )
         bytes_read = read( pHandles->pipe_stdout[0], buffer, BUFSIZE );
      if( bytes_read < 0 || bytes_read > BUFSIZE )
         bytes_read = 0;
      if( FD_ISSET( pHandles->pipe_stderr[0], &fds ) ) {
         bytes_read += read( pHandles->pipe_stderr[0], buffer+bytes_read, BUFSIZE-bytes_read );
      }
      if( bytes_read > 0 && bytes_read <= BUFSIZE ) {
          buffer[bytes_read] = '\0';
          hb_retclen( buffer, bytes_read );
          return;
      } else if( bytes_read != 0 ) {
          hb_ret();
          return;
      }
   }
   if( ( result = waitpid( pHandles->pid, &status, WNOHANG ) ) == pHandles->pid || result == -1 ) {
      hb_ret();
      return;
   }

   hb_retc( "" );

}

HB_FUNC( CEDI_WRITETOCONSOLEAPP )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );

   write( pHandles->pipe_stdin[1], (const void *) hb_parc(2), hb_parclen(2) );
   fsync( pHandles->pipe_stdin[1] );
   hb_retl( 1 );
}

HB_FUNC( CEDI_ENDCONSOLEAPP )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   int status;

   //waitpid( pHandles->pid, &status, WNOHANG );

   close( pHandles->pipe_stdin[1] );
   close( pHandles->pipe_stdout[0] );
   close( pHandles->pipe_stderr[0] );

   if( pHandles->pid >= 0 ) {
      kill( pHandles->pid, SIGTERM );
      waitpid( pHandles->pid, &status, 0 );
   }

   hb_xfree( pHandles );

}

HB_FUNC( CEDI_WAITPID )
{
   int status;
   hb_retni( waitpid( hb_parni(1), &status, WNOHANG ) );
}

#else

#include <windows.h>
#include <tchar.h>
#define CMDLENGTH  4096

HB_FUNC( CEDI_RUNBACKGROUNDAPP )
{
   STARTUPINFO si;
   PROCESS_INFORMATION pi;
#ifdef UNICODE
   TCHAR wc1[CMDLENGTH];
#endif

   ZeroMemory( &si, sizeof(si) );
   si.cb = sizeof(si);
   si.wShowWindow = SW_HIDE; //SW_SHOW;
   si.dwFlags = STARTF_USESHOWWINDOW;
   ZeroMemory( &pi, sizeof(pi) );

#ifdef UNICODE
   MultiByteToWideChar( GetACP(), 0, hb_parc(1), -1, wc1, CMDLENGTH );
   CreateProcess( NULL,   // No module name (use command line)
       wc1,            // Command line
       NULL,           // Process handle not inheritable
       NULL,           // Thread handle not inheritable
       FALSE,          // Set handle inheritance to FALSE
       CREATE_NO_WINDOW,  // No creation flags
       NULL,           // Use parent's environment block
       NULL,           // Use parent's starting directory
       &si,            // Pointer to STARTUPINFO structure
       &pi );          // Pointer to PROCESS_INFORMATION structure
#else
   CreateProcess( NULL,   // No module name (use command line)
       (LPTSTR)hb_parc(1),  // Command line
       NULL,           // Process handle not inheritable
       NULL,           // Thread handle not inheritable
       FALSE,          // Set handle inheritance to FALSE
       CREATE_NO_WINDOW,  // No creation flags
       NULL,           // Use parent's environment block
       NULL,           // Use parent's starting directory
       &si,            // Pointer to STARTUPINFO structure
       &pi );          // Pointer to PROCESS_INFORMATION structure
#endif
}

HB_FUNC( CEDI_RUNCONSOLEAPP )
{
   SECURITY_ATTRIBUTES sa;
   HANDLE g_hChildStd_OUT_Rd = NULL;
   HANDLE g_hChildStd_OUT_Wr = NULL;
   PROCESS_INFORMATION pi;
   STARTUPINFO si;
   BOOL bSuccess;
   int iOutExist = 0, read_all = 0, iOutFirst = 1;
   DWORD dwRead, dwWritten, dwExitCode;
   CHAR chBuf[BUFSIZE], *pOut;

   HANDLE hOut = NULL;
#ifdef UNICODE
   TCHAR wc1[CMDLENGTH], wc2[CMDLENGTH];
#endif

   sa.nLength = sizeof( SECURITY_ATTRIBUTES );
   sa.bInheritHandle = TRUE;
   sa.lpSecurityDescriptor = NULL;

   // Create a pipe for the child process's STDOUT.
   if( !CreatePipe( &g_hChildStd_OUT_Rd, &g_hChildStd_OUT_Wr, &sa, 32768 ) )
   {
      hb_retni( 1 );
      return;
   }

   // Ensure the read handle to the pipe for STDOUT is not inherited.
   if( !SetHandleInformation( g_hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0 ) )
   {
      hb_retni( 2 );
      return;
   }

   // Set up members of the PROCESS_INFORMATION structure.
   ZeroMemory( &pi, sizeof( PROCESS_INFORMATION ) );

   // Set up members of the STARTUPINFO structure.
   // This structure specifies the STDIN and STDOUT handles for redirection.
   ZeroMemory( &si, sizeof( si ) );
   si.cb = sizeof( si );
   si.wShowWindow = SW_HIDE;
   si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
   si.hStdOutput = g_hChildStd_OUT_Wr;
   si.hStdError = g_hChildStd_OUT_Wr;

#ifdef UNICODE
   MultiByteToWideChar( GetACP(), 0, hb_parc(1), -1, wc1, CMDLENGTH );
   bSuccess = CreateProcess( NULL, wc1, NULL, NULL,
         TRUE, CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi );
#else
   bSuccess = CreateProcess( NULL, ( LPTSTR ) hb_parc( 1 ), NULL, NULL,
         TRUE, CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi );
#endif
   if( !bSuccess )
   {
      hb_retni( 3 );
      return;
   }

   //WaitForSingleObject( pi.hProcess, 8000 ); //INFINITE );
   GetExitCodeProcess( pi.hProcess, &dwExitCode );
   CloseHandle( pi.hProcess );
   CloseHandle( pi.hThread );
   CloseHandle( g_hChildStd_OUT_Wr );

   if( !HB_ISNIL( 2 ) )
   {
#ifdef UNICODE
      MultiByteToWideChar( GetACP(), 0, hb_parc(2), -1, wc2, CMDLENGTH );
      hOut = CreateFile( wc2, GENERIC_WRITE, 0, 0,
            CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
#else
      hOut = CreateFile( ( LPTSTR )hb_parc( 2 ), GENERIC_WRITE, 0, 0,
            CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
#endif
      iOutExist = 1;
   }
   else if( HB_ISBYREF( 3 ) )
      iOutExist = 2;

   while( 1 )
   {
      bSuccess =
            ReadFile( g_hChildStd_OUT_Rd, chBuf, BUFSIZE, &dwRead, NULL );
      if( !bSuccess || dwRead == 0 )
         break;

      if( iOutExist == 1 )
      {
         bSuccess = WriteFile( hOut, chBuf, dwRead, &dwWritten, NULL );
         if( !bSuccess )
            break;
      }
      else if( iOutExist == 2 && dwRead > 0 )
      {
         read_all += (int) dwRead;
         if( iOutFirst )
         {
            pOut = (char*) hb_xgrab( (int)dwRead + 1 );
            memcpy( pOut, chBuf, (int)dwRead );
            iOutFirst = 0;
         }
         else
         {
            pOut = ( char * ) hb_xrealloc( pOut, read_all + 1 );
            memcpy( pOut+read_all-(int)dwRead, chBuf, (int)dwRead );
         }
      }
   }

   if( iOutExist == 1 )
      CloseHandle( hOut );
   else if( iOutExist == 2 )
      if( read_all > 0 )
         hb_storclen_buffer( pOut, read_all, 3 );
      else
         hb_storc( "", 3 );

   CloseHandle( g_hChildStd_OUT_Rd );

   hb_retni( ( int ) dwExitCode );
}

HB_FUNC( CEDI_RUNAPP )
{

   PROCESS_INFORMATION pi;
   STARTUPINFO si;
   BOOL bSuccess;
#ifdef UNICODE
   TCHAR wc1[CMDLENGTH];
#endif

   ZeroMemory( &pi, sizeof( PROCESS_INFORMATION ) );
   ZeroMemory( &si, sizeof( si ) );
   si.cb = sizeof( si );

#ifdef UNICODE
   MultiByteToWideChar( ( (HB_ISLOG(2) && hb_parl(2))? CP_UTF8 : GetACP() ), 0, hb_parc(1), -1, wc1, CMDLENGTH );
   bSuccess = CreateProcess( NULL, wc1, NULL, NULL,
         TRUE, CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi );
#else
   bSuccess = CreateProcess( NULL, ( LPTSTR ) hb_parc( 1 ), NULL, NULL,
         FALSE, 0, NULL, NULL, &si, &pi );
#endif
   if( !bSuccess )
   {
      hb_retl( 0 );
      return;
   }
   CloseHandle( pi.hProcess );
   CloseHandle( pi.hThread );
   hb_retl( 1 );

   //hb_retl( WinExec( hb_parc( 1 ), (HB_ISNIL(2))? SW_SHOW : ( UINT ) hb_parni( 2 ) ) > 31 );
}

typedef struct {

   HANDLE g_hChildStd_IN_Rd;
   HANDLE g_hChildStd_IN_Wr;
   HANDLE g_hChildStd_OUT_Rd;
   HANDLE g_hChildStd_OUT_Wr;
   PROCESS_INFORMATION piProcInfo;
   OVERLAPPED overlapped;
   int iRes;

} PROCESS_HANDLES;

static int CreateChildProcess( PROCESS_HANDLES * pHandles, char * pName, short int uiShow )
{
   STARTUPINFO siStartInfo;
   BOOL bSuccess;
#ifdef UNICODE
   TCHAR wc1[CMDLENGTH];
#endif

   ZeroMemory( &(pHandles->piProcInfo), sizeof( PROCESS_INFORMATION ) );

   ZeroMemory( &siStartInfo, sizeof( STARTUPINFO ) );
   siStartInfo.cb = sizeof( STARTUPINFO );
   siStartInfo.hStdError = pHandles->g_hChildStd_OUT_Wr;
   siStartInfo.hStdOutput = pHandles->g_hChildStd_OUT_Wr;
   siStartInfo.hStdInput = pHandles->g_hChildStd_IN_Rd;
   siStartInfo.wShowWindow = uiShow;
   siStartInfo.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;

#ifdef UNICODE
   MultiByteToWideChar( GetACP(), 0, hb_parc(1), -1, wc1, CMDLENGTH );
   bSuccess = CreateProcess( NULL, wc1, NULL, NULL,
         TRUE, CREATE_NEW_CONSOLE, NULL, NULL, &siStartInfo, &(pHandles->piProcInfo) );
#else
   bSuccess = CreateProcess( NULL, ( LPTSTR ) hb_parc( 1 ), NULL, NULL,
         TRUE, CREATE_NEW_CONSOLE, NULL, NULL, &siStartInfo, &(pHandles->piProcInfo) );
#endif
   if( !bSuccess )
   {
      return 0;
   }
   else
   {
      // Close handles to the stdin and stdout pipes no longer needed by the child process.
      // If they are not explicitly closed, there is no way to recognize that the child process has ended.
      if( pHandles->g_hChildStd_OUT_Wr ) {
         CloseHandle( pHandles->g_hChildStd_OUT_Wr );
         pHandles->g_hChildStd_OUT_Wr = NULL;
      }
      if( pHandles->g_hChildStd_IN_Rd ) {
         CloseHandle( pHandles->g_hChildStd_IN_Rd );
         pHandles->g_hChildStd_IN_Rd = NULL;
      }
   }
   return 1;
}

static int ReadFromPipe( PROCESS_HANDLES * pHandles, CHAR *chBuf, int *iRead )
{
   DWORD dwRead = 0;
   BOOL bSuccess;
   DWORD result;
   int iRes = 1;

   while (1) {
      if (ReadFile(pHandles->g_hChildStd_OUT_Rd, chBuf, BUFSIZE, &dwRead, &(pHandles->overlapped))) {
          break;
      } else if (GetLastError() == ERROR_IO_PENDING) {
          result = WaitForSingleObject(pHandles->overlapped.hEvent, 3000);
          if (result == WAIT_OBJECT_0) {
              if (GetOverlappedResult(pHandles->g_hChildStd_OUT_Rd, &(pHandles->overlapped), &dwRead, FALSE)) {
                  break;
              }
          }
      }
      else
      {
         iRes = 0; break;
      }
   }

   chBuf[dwRead] = '\0';
   *iRead = (int) dwRead;

   return iRes;
}

HB_FUNC( CEDI_STARTCONSOLEAPP )
{
   SECURITY_ATTRIBUTES saAttr;
   DWORD mode = PIPE_READMODE_BYTE | PIPE_WAIT;
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_xgrab( sizeof(PROCESS_HANDLES) );
   short int uiShow = (HB_ISNUM(2))? (short int)hb_parni(2) : 0;

   memset( pHandles, 0, sizeof( PROCESS_HANDLES ) );

// Set the bInheritHandle flag so pipe handles are inherited.
   saAttr.nLength = sizeof( SECURITY_ATTRIBUTES );
   saAttr.bInheritHandle = TRUE;
   saAttr.lpSecurityDescriptor = NULL;

// Create a pipe for the child process's STDOUT.
   if( !CreatePipe( &(pHandles->g_hChildStd_OUT_Rd), &(pHandles->g_hChildStd_OUT_Wr), &saAttr, 0 ) ) {
      pHandles->iRes = 1; hb_retptr( (void*) pHandles ); return;
   }
// Ensure the read handle to the pipe for STDOUT is not inherited.
   if( !SetHandleInformation( pHandles->g_hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0 ) ) {
      pHandles->iRes = 2; hb_retptr( (void*) pHandles ); return;
   }
   if (!SetNamedPipeHandleState(pHandles->g_hChildStd_OUT_Rd, &mode, NULL, NULL)) {
      pHandles->iRes = 3; hb_retptr( (void*) pHandles ); return;
   }

// Create a pipe for the child process's STDIN.
   if( !CreatePipe( &(pHandles->g_hChildStd_IN_Rd), &(pHandles->g_hChildStd_IN_Wr), &saAttr, 0 ) ) {
      pHandles->iRes = 4; hb_retptr( (void*) pHandles ); return;
   }
// Ensure the write handle to the pipe for STDIN is not inherited.
   if( !SetHandleInformation( pHandles->g_hChildStd_IN_Wr, HANDLE_FLAG_INHERIT, 0 ) ) {
      pHandles->iRes = 5; hb_retptr( (void*) pHandles ); return;
   }

   if( !CreateChildProcess( pHandles, (char*) hb_parc(1), uiShow ) ) {
      pHandles->iRes = GetLastError(); hb_retptr( (void*) pHandles ); return;
   }

   pHandles->overlapped.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);

   hb_retptr( (void*) pHandles );

}

HB_FUNC( CEDI_RETURNERRCODE )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   hb_retni( pHandles->iRes );
}

HB_FUNC( CEDI_READFROMCONSOLEAPP )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   CHAR chBuf[BUFSIZE];
   int iRead;
   DWORD dwAvail;

   if( !PeekNamedPipe( pHandles->g_hChildStd_OUT_Rd, NULL, 0, NULL, &dwAvail, NULL ) )
   {
      hb_ret();
   }
   else if( !dwAvail )
   {
      hb_retc( "" );
   }
   else
   {
      if( ReadFromPipe( pHandles, chBuf, &iRead ) )
         hb_retclen( chBuf, iRead );
      else
         hb_ret();

      ResetEvent( pHandles->overlapped.hEvent );
   }
}

HB_FUNC( CEDI_WRITETOCONSOLEAPP )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   DWORD dwWritten;
   BOOL bSuccess;

   bSuccess = WriteFile( pHandles->g_hChildStd_IN_Wr, (char*) hb_parc(2),
      (DWORD) hb_parclen(2), &dwWritten, NULL );

   //_writelog( "_ac.log", 0, "%d write %d %d %s\r\n", bSuccess, hb_parclen(2), (int)dwWritten, (char*) hb_parc(2) );
   hb_retl( bSuccess );
}

HB_FUNC( CEDI_ENDCONSOLEAPP )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   int iNoTerminate = (HB_ISLOG(2) && hb_parl(2))? 1 : 0;

   if( !pHandles )
      return;

   CloseHandle(pHandles->overlapped.hEvent);

   if( pHandles->g_hChildStd_IN_Rd ) {
      CloseHandle( pHandles->g_hChildStd_IN_Rd );
      pHandles->g_hChildStd_IN_Rd = NULL;
   }
   if( pHandles->g_hChildStd_IN_Wr ) {
      CloseHandle( pHandles->g_hChildStd_IN_Wr );
      pHandles->g_hChildStd_IN_Wr = NULL;
   }
   if( pHandles->g_hChildStd_OUT_Rd ) {
      CloseHandle( pHandles->g_hChildStd_OUT_Rd );
      pHandles->g_hChildStd_OUT_Rd = NULL;
   }
   if( pHandles->g_hChildStd_OUT_Wr ) {
      CloseHandle( pHandles->g_hChildStd_OUT_Wr );
      pHandles->g_hChildStd_OUT_Wr = NULL;
   }
   if( pHandles->piProcInfo.hProcess )
   {
      if( !iNoTerminate )
         TerminateProcess( pHandles->piProcInfo.hProcess, 0 );
      CloseHandle( pHandles->piProcInfo.hProcess );
      CloseHandle( pHandles->piProcInfo.hThread );
      pHandles->piProcInfo.hProcess = NULL;
   }
   hb_xfree( pHandles );

}

HB_FUNC( CEDI_GETHWNDVISIBLEBYPID )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   DWORD mypid = pHandles->piProcInfo.dwProcessId;
   DWORD pid;
   HWND h = GetTopWindow( 0 );

   while ( h )
   {
      GetWindowThreadProcessId( h,&pid );
      if( pid == mypid && IsWindowVisible(h) )
      {
         hb_retptr( h );
         return;
      }
      h = GetNextWindow( h , GW_HWNDNEXT );
   }
   hb_ret();
}

HB_FUNC( CEDI_GETHWNDBYPID )
{
   PROCESS_HANDLES * pHandles = (PROCESS_HANDLES *) hb_parptr( 1 );
   DWORD mypid = pHandles->piProcInfo.dwProcessId;
   DWORD pid;
   HWND h = GetTopWindow( 0 );

   while ( h )
   {
      GetWindowThreadProcessId( h,&pid );
      if ( pid == mypid )
      {
         hb_retptr( h );
         return;
      }
      h = GetNextWindow( h , GW_HWNDNEXT );
   }
   hb_ret();
}

HB_FUNC( CEDI_SHOWWINDOW )
{
   ShowWindow( ( HWND ) hb_parptr( 1 ), SW_SHOWNORMAL );
}

HB_FUNC( CEDI_HIDEWINDOW )
{
   ShowWindow( ( HWND ) hb_parptr( 1 ), SW_HIDE );
}

HB_FUNC( CEDI_GETACTIVEWINDOW )
{
   hb_retptr( GetActiveWindow() );
}

HB_FUNC( CEDI_SETACTIVE )
{
   SetActiveWindow( ( HWND ) hb_parptr( 1 ) );
   SetForegroundWindow( ( HWND ) hb_parptr( 1 ) );
}

HB_FUNC( CEDI_SHELLEXECUTE )
{
   int iParams = 0;
#ifdef UNICODE
   TCHAR wc1[CMDLENGTH], wc2[CMDLENGTH];
#endif

   if( hb_pcount() > 1 && HB_ISCHAR(2) )
      iParams = 1;
#ifdef UNICODE
   MultiByteToWideChar( ( (HB_ISLOG(2) && hb_parl(2))? CP_UTF8 : GetACP() ), 0, hb_parc(1), -1, wc1, CMDLENGTH );
   if( iParams )
      MultiByteToWideChar( GetACP(), 0, hb_parc(2), -1, wc2, CMDLENGTH );
   hb_retnl( ( LONG ) ShellExecute( GetActiveWindow(),
      NULL, wc1, (iParams)? wc2:NULL, NULL, SW_SHOWNORMAL ) );
#else
   hb_retnl( ( LONG ) ShellExecute( GetActiveWindow(),
      NULL, hb_parc( 1 ), (iParams)? hb_parc(2):NULL, NULL, SW_SHOWNORMAL ) );
#endif
}

HB_FUNC( CEDI_GETDRIVES )
{
   DWORD dwRes = GetLogicalDrives();
   int i;
   char buf[26], *ptr;

   if( dwRes )
   {
      ptr = buf;
      for( i = 0; i<26; i++ )
         if( (1 << i) & dwRes )
            *ptr++ = (char) i + 65;
      hb_retclen( buf, ptr-buf );
   }
}
/*
  0  The drive type cannot be determined.
  1  The root path is invalid; for example, there is no volume mounted at the specified path.
  2  The drive has removable media; for example, a floppy drive, thumb drive, or flash card reader.
  3  The drive has fixed media; for example, a hard disk drive or flash drive.
  4  The drive is a remote (network) drive.
  5  The drive is a CD-ROM drive.
  6  The drive is a RAM disk.
 */
HB_FUNC( CEDI_GETDRIVETYPE )
{
   int i;

#ifdef UNICODE
   TCHAR wc1[6];
   MultiByteToWideChar( GetACP(), 0, hb_parc(1), -1, wc1, 4 );
   i = (int) GetDriveType( wc1 );
#else
   i = (int) GetDriveType( hb_parc(1) );
#endif
   hb_retni( i );
}
#endif

typedef struct _bitarr_ {
   unsigned char * szArr;
   unsigned int    uiLen;
} bitarr;

bitarr * bitarr_Init( unsigned int uiLen )
{
   bitarr * pArr = (bitarr *) malloc( sizeof( bitarr ) );

   //_writelog( "_ac.log", 0, "malloc %d\r\n", uiLen );
   pArr->uiLen = uiLen/8 + ((uiLen%8 > 0)? 1 : 0);
   pArr->szArr = ( unsigned char * ) malloc( pArr->uiLen );
   memset( pArr->szArr, 0, pArr->uiLen );
   //_writelog( "_ac.log", 0, "malloc-2 %d\r\n", pArr->uiLen );

   return pArr;
}

void bitarr_Release( bitarr * pArr )
{
   free( pArr->szArr );
   free( pArr );
}

/*
  bitarr_Set( bitarr *pArr, unsigned int uiBit, unsigned int uiValue )
  pArr - bit array pointer
  uiBit - Bit number to set or reset, 0...
  uiValue - 1 or 0
 */
void bitarr_Set( bitarr *pArr, unsigned int uiBit, unsigned int uiValue )
{

   if( uiBit > pArr->uiLen * 8 )
   {
      unsigned int uiLenNew = uiBit/8 + ( (uiBit%8 > 0)? 1 : 0 ) + 32;
      unsigned char *ptr, *ptrend;

      //_writelog( "_ac.log", 0, "realloc %d %d  ", pArr->uiLen, uiLenNew );
      pArr->szArr = ( unsigned char * ) realloc( pArr->szArr, uiLenNew );
      //_writelog( "_ac.log", 0, "realloc2 %d  ", uiBit );
      ptr = pArr->szArr + pArr->uiLen;
      ptrend = pArr->szArr + uiLenNew;
      while( ptr < ptrend )
         *ptr++ = '\0';
      pArr->uiLen = uiLenNew;
      //_writelog( "_ac.log", 0, "realloc3\r\n" );
   }

   if( uiValue == 0 )
      *( pArr->szArr + (uiBit/8) ) &= ~( 0x80 >> (uiBit%8) );
   else
      *( pArr->szArr + (uiBit/8) ) |= ( 0x80 >> (uiBit%8) );
   //_writelog( "_ac.log", 0, "%x %x %x\r\n", *pArr->szArr, *(pArr->szArr+1), *(pArr->szArr+2) );
}

/*
  bitarr_Test( bitarr *pArr, unsigned int uiBit )
  pArr - bit array pointer
  uiBit - Bit number to test, 0...
 */
int bitarr_Test( bitarr *pArr, unsigned int uiBit )
{
   if( uiBit > pArr->uiLen * 8 )
      return 0;
   return ( ( *( pArr->szArr + (uiBit/8) ) & ( 0x80 >> (uiBit%8) ) ) != 0 )? 1 : 0;
}

HB_FUNC( BITARR_INIT )
{
   hb_retptr( (void*) bitarr_Init( (unsigned int) hb_parni(1) ) );
}

HB_FUNC( BITARR_RELEASE )
{
   bitarr_Release( (bitarr*) hb_parptr(1) );
}

/*
  bitarr_Set( pArr, nBit, nValue )
  pArr - bit array pointer
  nBit - Bit number to set or reset, 1...
  nValue - 1 or 0
 */
HB_FUNC( BITARR_SET )
{
   unsigned int uiValue = (unsigned int) ( (HB_ISLOG(3))? hb_parl(3) : hb_parni(3) );
   bitarr_Set( (bitarr*) hb_parptr(1), (unsigned int) hb_parni(2)-1, uiValue );
}

/*
  bitarr_Test( pArr, nBit )
  pArr - bit array pointer
  nBit - Bit number to test, 1...
 */
HB_FUNC( BITARR_TEST )
{
   hb_retl( bitarr_Test( (bitarr*) hb_parptr(1), (unsigned int) hb_parni(2)-1 ) );
}

/*
 * cedi_CheckMultiComm( pText, iFrom, iTo, pDop, cQuo, cScomm, cMcomm1, cMcomm2 )
 */
HB_FUNC( CEDI_CHECKMULTICOMM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_ARRAY );
   int iFrom = hb_parni(2), iTo = hb_parni(3), i;
   bitarr *pDop = (bitarr*) hb_parptr(4);
   const char * pLine, *pEnd, *ptr;
   const char * cQuo = hb_parc(5);
   const char * cScomm = hb_parc(6);
   const char * cMcomm1 = hb_parc(7);
   const char * cMcomm2 = hb_parc(8);
   char c;
   int iLenM1 = strlen( cMcomm1 ), iLenM2 = strlen( cMcomm2 ), iLenS = strlen( cScomm );
   int iMulti = ( (iFrom>1 && bitarr_Test(pDop,iFrom-2) == 1)? 1 : 0 );

   //_writelog( "_ac.log", 0, "chk-0 %d %d\r\n", iFrom, iTo );
   for( i=iFrom; i<=iTo; i++ )
   {
      pLine = hb_arrayGetCPtr( pText, i );
      pEnd = pLine + hb_arrayGetCLen( pText, i );
      if( pEnd == pLine )
      {
         bitarr_Set( pDop, i-1, iMulti );
         continue;
      }
      if( iMulti )
      {
         if( ( ptr = strstr( pLine, cMcomm2 ) ) == NULL )
         {
            bitarr_Set( pDop, i-1, 1 );
            continue;
         } else {
            //_writelog( "_ac.log", 0, "chk-2 %d\r\n", i );
            iMulti = 0;
            pLine = ptr + iLenM2;
         }
      }
      while( *pLine && ( *pLine == ' ' || *pLine == '\t' || *pLine == '\x9' ) ) pLine ++;
      while( pLine < pEnd )
      {
         c = *pLine;
         if( strchr( cQuo, c ) && !( pEnd-pLine>1 && *(pLine+1)==c && *(pLine+1)==c ) )
         {
            if( ( ptr = strchr( pLine+1, c ) ) == NULL )
               pLine = pEnd;
            else
               pLine = ptr;
         }
         else if( c == *cScomm && strncmp( pLine, cScomm, iLenS ) == 0 )
         {
            pLine = pEnd;
         }
         else if( c == *cMcomm1 && strncmp( pLine, cMcomm1, iLenM1 ) == 0 )
         {
            if( ( ptr = strstr( pLine+iLenM1, cMcomm2 ) ) == NULL )
            {
               pLine = pEnd;
               iMulti = 1;
               pLine += iLenM1-1;
            }
            else
               pLine == ptr + iLenM1-1;
         }
         pLine ++;
      }
      bitarr_Set( pDop, i-1, iMulti );
   }
   hb_retni(0);
   return;
}

static void sleep_ns( long int milliseconds )
{
#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
   struct timeval tv;
   tv.tv_sec = milliseconds / 1000;
   tv.tv_usec = milliseconds % 1000 * 1000;
   select(0, NULL, NULL, NULL, &tv);
#else
   Sleep( milliseconds );
#endif
}

HB_FUNC( CEDI_SLEEP )
{
   sleep_ns( hb_parni(1) );
}

/*-
 * Copyright (c) 2008 - 2010 CAS Dev Team
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the CAS Dev. Team nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

typedef struct UCSTransMap
{
   /** Source character                  */
   unsigned int sc;
   /** Destination character             */
   unsigned int dc;
} UCSTransMap;

/* Uppercase code point, Lowercase code point */
static UCSTransMap mUCSU2L[] = {
   {0x0041, 0x0061}, {0x0042, 0x0062}, {0x0043, 0x0063}, {0x0044, 0x0064},
   {0x0045, 0x0065}, {0x0046, 0x0066}, {0x0047, 0x0067}, {0x0048, 0x0068},
   {0x0049, 0x0069}, {0x004A, 0x006A}, {0x004B, 0x006B}, {0x004C, 0x006C},
   {0x004D, 0x006D}, {0x004E, 0x006E}, {0x004F, 0x006F}, {0x0050, 0x0070},
   {0x0051, 0x0071}, {0x0052, 0x0072}, {0x0053, 0x0073}, {0x0054, 0x0074},
   {0x0055, 0x0075}, {0x0056, 0x0076}, {0x0057, 0x0077}, {0x0058, 0x0078},
   {0x0059, 0x0079}, {0x005A, 0x007A}, {0x00C0, 0x00E0}, {0x00C1, 0x00E1},
   {0x00C2, 0x00E2}, {0x00C3, 0x00E3}, {0x00C4, 0x00E4}, {0x00C5, 0x00E5},
   {0x00C6, 0x00E6}, {0x00C7, 0x00E7}, {0x00C8, 0x00E8}, {0x00C9, 0x00E9},
   {0x00CA, 0x00EA}, {0x00CB, 0x00EB}, {0x00CC, 0x00EC}, {0x00CD, 0x00ED},
   {0x00CE, 0x00EE}, {0x00CF, 0x00EF}, {0x00D0, 0x00F0}, {0x00D1, 0x00F1},
   {0x00D2, 0x00F2}, {0x00D3, 0x00F3}, {0x00D4, 0x00F4}, {0x00D5, 0x00F5},
   {0x00D6, 0x00F6}, {0x00D8, 0x00F8}, {0x00D9, 0x00F9}, {0x00DA, 0x00FA},
   {0x00DB, 0x00FB}, {0x00DC, 0x00FC}, {0x00DD, 0x00FD}, {0x00DE, 0x00FE},
   {0x0100, 0x0101}, {0x0102, 0x0103}, {0x0104, 0x0105}, {0x0106, 0x0107},
   {0x0108, 0x0109}, {0x010A, 0x010B}, {0x010C, 0x010D}, {0x010E, 0x010F},
   {0x0110, 0x0111}, {0x0112, 0x0113}, {0x0114, 0x0115}, {0x0116, 0x0117},
   {0x0118, 0x0119}, {0x011A, 0x011B}, {0x011C, 0x011D}, {0x011E, 0x011F},
   {0x0120, 0x0121}, {0x0122, 0x0123}, {0x0124, 0x0125}, {0x0126, 0x0127},
   {0x0128, 0x0129}, {0x012A, 0x012B}, {0x012C, 0x012D}, {0x012E, 0x012F},
   {0x0130, 0x0069}, {0x0132, 0x0133}, {0x0134, 0x0135}, {0x0136, 0x0137},
   {0x0139, 0x013A}, {0x013B, 0x013C}, {0x013D, 0x013E}, {0x013F, 0x0140},
   {0x0141, 0x0142}, {0x0143, 0x0144}, {0x0145, 0x0146}, {0x0147, 0x0148},
   {0x014A, 0x014B}, {0x014C, 0x014D}, {0x014E, 0x014F}, {0x0150, 0x0151},
   {0x0152, 0x0153}, {0x0154, 0x0155}, {0x0156, 0x0157}, {0x0158, 0x0159},
   {0x015A, 0x015B}, {0x015C, 0x015D}, {0x015E, 0x015F}, {0x0160, 0x0161},
   {0x0162, 0x0163}, {0x0164, 0x0165}, {0x0166, 0x0167}, {0x0168, 0x0169},
   {0x016A, 0x016B}, {0x016C, 0x016D}, {0x016E, 0x016F}, {0x0170, 0x0171},
   {0x0172, 0x0173}, {0x0174, 0x0175}, {0x0176, 0x0177}, {0x0178, 0x00FF},
   {0x0179, 0x017A}, {0x017B, 0x017C}, {0x017D, 0x017E}, {0x0181, 0x0253},
   {0x0182, 0x0183}, {0x0184, 0x0185}, {0x0186, 0x0254}, {0x0187, 0x0188},
   {0x018A, 0x0257}, {0x018B, 0x018C}, {0x018E, 0x0258}, {0x018F, 0x0259},
   {0x0190, 0x025B}, {0x0191, 0x0192}, {0x0193, 0x0260}, {0x0194, 0x0263},
   {0x0196, 0x0269}, {0x0197, 0x0268}, {0x0198, 0x0199}, {0x019C, 0x026f},
   {0x019D, 0x0272}, {0x019F, 0x0275}, {0x01A0, 0x01A1}, {0x01A2, 0x01A3},
   {0x01A4, 0x01A5}, {0x01A7, 0x01A8}, {0x01A9, 0x0283}, {0x01AC, 0x01AD},
   {0x01AE, 0x0288}, {0x01AF, 0x01B0}, {0x01B1, 0x028A}, {0x01B2, 0x028B},
   {0x01B3, 0x01B4}, {0x01B5, 0x01B6}, {0x01B7, 0x0292}, {0x01B8, 0x01B9},
   {0x01BC, 0x01BD}, {0x01C4, 0x01C6}, {0x01C5, 0x01C6}, {0x01C7, 0x01C9},
   {0x01C8, 0x01C9}, {0x01CA, 0x01CC}, {0x01CB, 0x01CC}, {0x01CD, 0x01CE},
   {0x01CF, 0x01D0}, {0x01D1, 0x01D2}, {0x01D3, 0x01D4}, {0x01D5, 0x01D6},
   {0x01D7, 0x01D8}, {0x01D9, 0x01DA}, {0x01DB, 0x01DC}, {0x01DE, 0x01DF},
   {0x01E0, 0x01E1}, {0x01E2, 0x01E3}, {0x01E4, 0x01E5}, {0x01E6, 0x01E7},
   {0x01E8, 0x01E9}, {0x01EA, 0x01EB}, {0x01EC, 0x01ED}, {0x01EE, 0x01EF},
   {0x01F1, 0x01F3}, {0x01F4, 0x01F5}, {0x01FA, 0x01FB}, {0x01FC, 0x01FD},
   {0x01FE, 0x01FF}, {0x0200, 0x0201}, {0x0202, 0x0203}, {0x0204, 0x0205},
   {0x0206, 0x0207}, {0x0208, 0x0209}, {0x020A, 0x020B}, {0x020C, 0x020D},
   {0x020E, 0x020F}, {0x0210, 0x0211}, {0x0212, 0x0213}, {0x0214, 0x0215},
   {0x0216, 0x0217}, {0x0386, 0x03AC}, {0x0388, 0x03AD}, {0x0389, 0x03AE},
   {0x038A, 0x03AF}, {0x038C, 0x03CC}, {0x038E, 0x03CD}, {0x038F, 0x03CE},
   {0x0391, 0x03B1}, {0x0392, 0x03B2}, {0x0393, 0x03B3}, {0x0394, 0x03B4},
   {0x0395, 0x03B5}, {0x0396, 0x03B6}, {0x0397, 0x03B7}, {0x0398, 0x03B8},
   {0x0399, 0x03B9}, {0x039A, 0x03BA}, {0x039B, 0x03BB}, {0x039C, 0x03BC},
   {0x039D, 0x03BD}, {0x039E, 0x03BE}, {0x039F, 0x03BF}, {0x03A0, 0x03C0},
   {0x03A1, 0x03C1}, {0x03A3, 0x03C3}, {0x03A4, 0x03C4}, {0x03A5, 0x03C5},
   {0x03A6, 0x03C6}, {0x03A7, 0x03C7}, {0x03A8, 0x03C8}, {0x03A9, 0x03C9},
   {0x03AA, 0x03CA}, {0x03AB, 0x03CB}, {0x03E2, 0x03E3}, {0x03E4, 0x03E5},
   {0x03E6, 0x03E7}, {0x03E8, 0x03E9}, {0x03EA, 0x03EB}, {0x03EC, 0x03ED},
   {0x03EE, 0x03EF}, {0x0401, 0x0451}, {0x0402, 0x0452}, {0x0403, 0x0453},
   {0x0404, 0x0454}, {0x0405, 0x0455}, {0x0406, 0x0456}, {0x0407, 0x0457},
   {0x0408, 0x0458}, {0x0409, 0x0459}, {0x040A, 0x045A}, {0x040B, 0x045B},
   {0x040C, 0x045C}, {0x040E, 0x045E}, {0x040F, 0x045F}, {0x0410, 0x0430},
   {0x0411, 0x0431}, {0x0412, 0x0432}, {0x0413, 0x0433}, {0x0414, 0x0434},
   {0x0415, 0x0435}, {0x0416, 0x0436}, {0x0417, 0x0437}, {0x0418, 0x0438},
   {0x0419, 0x0439}, {0x041A, 0x043A}, {0x041B, 0x043B}, {0x041C, 0x043C},
   {0x041D, 0x043D}, {0x041E, 0x043E}, {0x041F, 0x043F}, {0x0420, 0x0440},
   {0x0421, 0x0441}, {0x0422, 0x0442}, {0x0423, 0x0443}, {0x0424, 0x0444},
   {0x0425, 0x0445}, {0x0426, 0x0446}, {0x0427, 0x0447}, {0x0428, 0x0448},
   {0x0429, 0x0449}, {0x042A, 0x044A}, {0x042B, 0x044B}, {0x042C, 0x044C},
   {0x042D, 0x044D}, {0x042E, 0x044E}, {0x042F, 0x044F}, {0x0460, 0x0461},
   {0x0462, 0x0463}, {0x0464, 0x0465}, {0x0466, 0x0467}, {0x0468, 0x0469},
   {0x046A, 0x046B}, {0x046C, 0x046D}, {0x046E, 0x046F}, {0x0470, 0x0471},
   {0x0472, 0x0473}, {0x0474, 0x0475}, {0x0476, 0x0477}, {0x0478, 0x0479},
   {0x047A, 0x047B}, {0x047C, 0x047D}, {0x047E, 0x047F}, {0x0480, 0x0481},
   {0x0490, 0x0491}, {0x0492, 0x0493}, {0x0494, 0x0495}, {0x0496, 0x0497},
   {0x0498, 0x0499}, {0x049A, 0x049B}, {0x049C, 0x049D}, {0x049E, 0x049F},
   {0x04A0, 0x04A1}, {0x04A2, 0x04A3}, {0x04A4, 0x04A5}, {0x04A6, 0x04A7},
   {0x04A8, 0x04A9}, {0x04AA, 0x04AB}, {0x04AC, 0x04AD}, {0x04AE, 0x04AF},
   {0x04B0, 0x04B1}, {0x04B2, 0x04B3}, {0x04B4, 0x04B5}, {0x04B6, 0x04B7},
   {0x04B8, 0x04B9}, {0x04BA, 0x04BB}, {0x04BC, 0x04BD}, {0x04BE, 0x04BF},
   {0x04C1, 0x04C2}, {0x04C3, 0x04C4}, {0x04C7, 0x04C8}, {0x04CB, 0x04CC},
   {0x04D0, 0x04D1}, {0x04D2, 0x04D3}, {0x04D4, 0x04D5}, {0x04D6, 0x04D7},
   {0x04D8, 0x04D9}, {0x04DA, 0x04DB}, {0x04DC, 0x04DD}, {0x04DE, 0x04DF},
   {0x04E0, 0x04E1}, {0x04E2, 0x04E3}, {0x04E4, 0x04E5}, {0x04E6, 0x04E7},
   {0x04E8, 0x04E9}, {0x04EA, 0x04EB}, {0x04EE, 0x04EF}, {0x04F0, 0x04F1},
   {0x04F2, 0x04F3}, {0x04F4, 0x04F5}, {0x04F8, 0x04F9}, {0x0531, 0x0561},
   {0x0532, 0x0562}, {0x0533, 0x0563}, {0x0534, 0x0564}, {0x0535, 0x0565},
   {0x0536, 0x0566}, {0x0537, 0x0567}, {0x0538, 0x0568}, {0x0539, 0x0569},
   {0x053A, 0x056A}, {0x053B, 0x056B}, {0x053C, 0x056C}, {0x053D, 0x056D},
   {0x053E, 0x056E}, {0x053F, 0x056F}, {0x0540, 0x0570}, {0x0541, 0x0571},
   {0x0542, 0x0572}, {0x0543, 0x0573}, {0x0544, 0x0574}, {0x0545, 0x0575},
   {0x0546, 0x0576}, {0x0547, 0x0577}, {0x0548, 0x0578}, {0x0549, 0x0579},
   {0x054A, 0x057A}, {0x054B, 0x057B}, {0x054C, 0x057C}, {0x054D, 0x057D},
   {0x054E, 0x057E}, {0x054F, 0x057F}, {0x0550, 0x0580}, {0x0551, 0x0581},
   {0x0552, 0x0582}, {0x0553, 0x0583}, {0x0554, 0x0584}, {0x0555, 0x0585},
   {0x0556, 0x0586}, {0x10A0, 0x10D0}, {0x10A1, 0x10D1}, {0x10A2, 0x10D2},
   {0x10A3, 0x10D3}, {0x10A4, 0x10D4}, {0x10A5, 0x10D5}, {0x10A6, 0x10D6},
   {0x10A7, 0x10D7}, {0x10A8, 0x10D8}, {0x10A9, 0x10D9}, {0x10AA, 0x10DA},
   {0x10AB, 0x10DB}, {0x10AC, 0x10DC}, {0x10AD, 0x10DD}, {0x10AE, 0x10DE},
   {0x10AF, 0x10DF}, {0x10B0, 0x10E0}, {0x10B1, 0x10E1}, {0x10B2, 0x10E2},
   {0x10B3, 0x10E3}, {0x10B4, 0x10E4}, {0x10B5, 0x10E5}, {0x10B6, 0x10E6},
   {0x10B7, 0x10E7}, {0x10B8, 0x10E8}, {0x10B9, 0x10E9}, {0x10BA, 0x10EA},
   {0x10BB, 0x10EB}, {0x10BC, 0x10EC}, {0x10BD, 0x10ED}, {0x10BE, 0x10EE},
   {0x10BF, 0x10EF}, {0x10C0, 0x10F0}, {0x10C1, 0x10F1}, {0x10C2, 0x10F2},
   {0x10C3, 0x10F3}, {0x10C4, 0x10F4}, {0x10C5, 0x10F5}, {0x1E00, 0x1E01},
   {0x1E02, 0x1E03}, {0x1E04, 0x1E05}, {0x1E06, 0x1E07}, {0x1E08, 0x1E09},
   {0x1E0A, 0x1E0B}, {0x1E0C, 0x1E0D}, {0x1E0E, 0x1E0F}, {0x1E10, 0x1E11},
   {0x1E12, 0x1E13}, {0x1E14, 0x1E15}, {0x1E16, 0x1E17}, {0x1E18, 0x1E19},
   {0x1E1A, 0x1E1B}, {0x1E1C, 0x1E1D}, {0x1E1E, 0x1E1F}, {0x1E20, 0x1E21},
   {0x1E22, 0x1E23}, {0x1E24, 0x1E25}, {0x1E26, 0x1E27}, {0x1E28, 0x1E29},
   {0x1E2A, 0x1E2B}, {0x1E2C, 0x1E2D}, {0x1E2E, 0x1E2F}, {0x1E30, 0x1E31},
   {0x1E32, 0x1E33}, {0x1E34, 0x1E35}, {0x1E36, 0x1E37}, {0x1E38, 0x1E39},
   {0x1E3A, 0x1E3B}, {0x1E3C, 0x1E3D}, {0x1E3E, 0x1E3F}, {0x1E40, 0x1E41},
   {0x1E42, 0x1E43}, {0x1E44, 0x1E45}, {0x1E46, 0x1E47}, {0x1E48, 0x1E49},
   {0x1E4A, 0x1E4B}, {0x1E4C, 0x1E4D}, {0x1E4E, 0x1E4F}, {0x1E50, 0x1E51},
   {0x1E52, 0x1E53}, {0x1E54, 0x1E55}, {0x1E56, 0x1E57}, {0x1E58, 0x1E59},
   {0x1E5A, 0x1E5B}, {0x1E5C, 0x1E5D}, {0x1E5E, 0x1E5F}, {0x1E60, 0x1E61},
   {0x1E62, 0x1E63}, {0x1E64, 0x1E65}, {0x1E66, 0x1E67}, {0x1E68, 0x1E69},
   {0x1E6A, 0x1E6B}, {0x1E6C, 0x1E6D}, {0x1E6E, 0x1E6F}, {0x1E70, 0x1E71},
   {0x1E72, 0x1E73}, {0x1E74, 0x1E75}, {0x1E76, 0x1E77}, {0x1E78, 0x1E79},
   {0x1E7A, 0x1E7B}, {0x1E7C, 0x1E7D}, {0x1E7E, 0x1E7F}, {0x1E80, 0x1E81},
   {0x1E82, 0x1E83}, {0x1E84, 0x1E85}, {0x1E86, 0x1E87}, {0x1E88, 0x1E89},
   {0x1E8A, 0x1E8B}, {0x1E8C, 0x1E8D}, {0x1E8E, 0x1E8F}, {0x1E90, 0x1E91},
   {0x1E92, 0x1E93}, {0x1E94, 0x1E95}, {0x1EA0, 0x1EA1}, {0x1EA2, 0x1EA3},
   {0x1EA4, 0x1EA5}, {0x1EA6, 0x1EA7}, {0x1EA8, 0x1EA9}, {0x1EAA, 0x1EAB},
   {0x1EAC, 0x1EAD}, {0x1EAE, 0x1EAF}, {0x1EB0, 0x1EB1}, {0x1EB2, 0x1EB3},
   {0x1EB4, 0x1EB5}, {0x1EB6, 0x1EB7}, {0x1EB8, 0x1EB9}, {0x1EBA, 0x1EBB},
   {0x1EBC, 0x1EBD}, {0x1EBE, 0x1EBF}, {0x1EC0, 0x1EC1}, {0x1EC2, 0x1EC3},
   {0x1EC4, 0x1EC5}, {0x1EC6, 0x1EC7}, {0x1EC8, 0x1EC9}, {0x1ECA, 0x1ECB},
   {0x1ECC, 0x1ECD}, {0x1ECE, 0x1ECF}, {0x1ED0, 0x1ED1}, {0x1ED2, 0x1ED3},
   {0x1ED4, 0x1ED5}, {0x1ED6, 0x1ED7}, {0x1ED8, 0x1ED9}, {0x1EDA, 0x1EDB},
   {0x1EDC, 0x1EDD}, {0x1EDE, 0x1EDF}, {0x1EE0, 0x1EE1}, {0x1EE2, 0x1EE3},
   {0x1EE4, 0x1EE5}, {0x1EE6, 0x1EE7}, {0x1EE8, 0x1EE9}, {0x1EEA, 0x1EEB},
   {0x1EEC, 0x1EED}, {0x1EEE, 0x1EEF}, {0x1EF0, 0x1EF1}, {0x1EF2, 0x1EF3},
   {0x1EF4, 0x1EF5}, {0x1EF6, 0x1EF7}, {0x1EF8, 0x1EF9}, {0x1F08, 0x1F00},
   {0x1F09, 0x1F01}, {0x1F0A, 0x1F02}, {0x1F0B, 0x1F03}, {0x1F0C, 0x1F04},
   {0x1F0D, 0x1F05}, {0x1F0E, 0x1F06}, {0x1F0F, 0x1F07}, {0x1F18, 0x1F10},
   {0x1F19, 0x1F11}, {0x1F1A, 0x1F12}, {0x1F1B, 0x1F13}, {0x1F1C, 0x1F14},
   {0x1F1D, 0x1F15}, {0x1F28, 0x1F20}, {0x1F29, 0x1F21}, {0x1F2A, 0x1F22},
   {0x1F2B, 0x1F23}, {0x1F2C, 0x1F24}, {0x1F2D, 0x1F25}, {0x1F2E, 0x1F26},
   {0x1F2F, 0x1F27}, {0x1F38, 0x1F30}, {0x1F39, 0x1F31}, {0x1F3A, 0x1F32},
   {0x1F3B, 0x1F33}, {0x1F3C, 0x1F34}, {0x1F3D, 0x1F35}, {0x1F3E, 0x1F36},
   {0x1F3F, 0x1F37}, {0x1F48, 0x1F40}, {0x1F49, 0x1F41}, {0x1F4A, 0x1F42},
   {0x1F4B, 0x1F43}, {0x1F4C, 0x1F44}, {0x1F4D, 0x1F45}, {0x1F59, 0x1F51},
   {0x1F5B, 0x1F53}, {0x1F5D, 0x1F55}, {0x1F5F, 0x1F57}, {0x1F68, 0x1F60},
   {0x1F69, 0x1F61}, {0x1F6A, 0x1F62}, {0x1F6B, 0x1F63}, {0x1F6C, 0x1F64},
   {0x1F6D, 0x1F65}, {0x1F6E, 0x1F66}, {0x1F6F, 0x1F67}, {0x1F88, 0x1F80},
   {0x1F89, 0x1F81}, {0x1F8A, 0x1F82}, {0x1F8B, 0x1F83}, {0x1F8C, 0x1F84},
   {0x1F8D, 0x1F85}, {0x1F8E, 0x1F86}, {0x1F8F, 0x1F87}, {0x1F98, 0x1F90},
   {0x1F99, 0x1F91}, {0x1F9A, 0x1F92}, {0x1F9B, 0x1F93}, {0x1F9C, 0x1F94},
   {0x1F9D, 0x1F95}, {0x1F9E, 0x1F96}, {0x1F9F, 0x1F97}, {0x1FA8, 0x1FA0},
   {0x1FA9, 0x1FA1}, {0x1FAA, 0x1FA2}, {0x1FAB, 0x1FA3}, {0x1FAC, 0x1FA4},
   {0x1FAD, 0x1FA5}, {0x1FAE, 0x1FA6}, {0x1FAF, 0x1FA7}, {0x1FB8, 0x1FB0},
   {0x1FB9, 0x1FB1}, {0x1FD8, 0x1FD0}, {0x1FD9, 0x1FD1}, {0x1FE8, 0x1FE0},
   {0x1FE9, 0x1FE1}, {0x24B6, 0x24D0}, {0x24B7, 0x24D1}, {0x24B8, 0x24D2},
   {0x24B9, 0x24D3}, {0x24BA, 0x24D4}, {0x24BB, 0x24D5}, {0x24BC, 0x24D6},
   {0x24BD, 0x24D7}, {0x24BE, 0x24D8}, {0x24BF, 0x24D9}, {0x24C0, 0x24DA},
   {0x24C1, 0x24DB}, {0x24C2, 0x24DC}, {0x24C3, 0x24DD}, {0x24C4, 0x24DE},
   {0x24C5, 0x24DF}, {0x24C6, 0x24E0}, {0x24C7, 0x24E1}, {0x24C8, 0x24E2},
   {0x24C9, 0x24E3}, {0x24CA, 0x24E4}, {0x24CB, 0x24E5}, {0x24CC, 0x24E6},
   {0x24CD, 0x24E7}, {0x24CE, 0x24E8}, {0x24CF, 0x24E9}, {0xFF21, 0xFF41},
   {0xFF22, 0xFF42}, {0xFF23, 0xFF43}, {0xFF24, 0xFF44}, {0xFF25, 0xFF45},
   {0xFF26, 0xFF46}, {0xFF27, 0xFF47}, {0xFF28, 0xFF48}, {0xFF29, 0xFF49},
   {0xFF2A, 0xFF4A}, {0xFF2B, 0xFF4B}, {0xFF2C, 0xFF4C}, {0xFF2D, 0xFF4D},
   {0xFF2E, 0xFF4E}, {0xFF2F, 0xFF4F}, {0xFF30, 0xFF50}, {0xFF31, 0xFF51},
   {0xFF32, 0xFF52}, {0xFF33, 0xFF53}, {0xFF34, 0xFF54}, {0xFF35, 0xFF55},
   {0xFF36, 0xFF56}, {0xFF37, 0xFF57}, {0xFF38, 0xFF58}, {0xFF39, 0xFF59},
   {0xFF3A, 0xFF5A}
};

/* Lowercase code point, Uppercase code point */
static UCSTransMap mUCSL2U[] = {
   {0x0061, 0x0041}, {0x0062, 0x0042}, {0x0063, 0x0043}, {0x0064, 0x0044},
   {0x0065, 0x0045}, {0x0066, 0x0046}, {0x0067, 0x0047}, {0x0068, 0x0048},
   {0x0069, 0x0049}, {0x006A, 0x004A}, {0x006B, 0x004B}, {0x006C, 0x004C},
   {0x006D, 0x004D}, {0x006E, 0x004E}, {0x006F, 0x004F}, {0x0070, 0x0050},
   {0x0071, 0x0051}, {0x0072, 0x0052}, {0x0073, 0x0053}, {0x0074, 0x0054},
   {0x0075, 0x0055}, {0x0076, 0x0056}, {0x0077, 0x0057}, {0x0078, 0x0058},
   {0x0079, 0x0059}, {0x007A, 0x005A}, {0x00E0, 0x00C0}, {0x00E1, 0x00C1},
   {0x00E2, 0x00C2}, {0x00E3, 0x00C3}, {0x00E4, 0x00C4}, {0x00E5, 0x00C5},
   {0x00E6, 0x00C6}, {0x00E7, 0x00C7}, {0x00E8, 0x00C8}, {0x00E9, 0x00C9},
   {0x00EA, 0x00CA}, {0x00EB, 0x00CB}, {0x00EC, 0x00CC}, {0x00ED, 0x00CD},
   {0x00EE, 0x00CE}, {0x00EF, 0x00CF}, {0x00F0, 0x00D0}, {0x00F1, 0x00D1},
   {0x00F2, 0x00D2}, {0x00F3, 0x00D3}, {0x00F4, 0x00D4}, {0x00F5, 0x00D5},
   {0x00F6, 0x00D6}, {0x00F8, 0x00D8}, {0x00F9, 0x00D9}, {0x00FA, 0x00DA},
   {0x00FB, 0x00DB}, {0x00FC, 0x00DC}, {0x00FD, 0x00DD}, {0x00FE, 0x00DE},
   {0x00FF, 0x0178}, {0x0101, 0x0100}, {0x0103, 0x0102}, {0x0105, 0x0104},
   {0x0107, 0x0106}, {0x0109, 0x0108}, {0x010B, 0x010A}, {0x010D, 0x010C},
   {0x010F, 0x010E}, {0x0111, 0x0110}, {0x0113, 0x0112}, {0x0115, 0x0114},
   {0x0117, 0x0116}, {0x0119, 0x0118}, {0x011B, 0x011A}, {0x011D, 0x011C},
   {0x011F, 0x011E}, {0x0121, 0x0120}, {0x0123, 0x0122}, {0x0125, 0x0124},
   {0x0127, 0x0126}, {0x0129, 0x0128}, {0x012B, 0x012A}, {0x012D, 0x012C},
   {0x012F, 0x012E}, {0x0131, 0x0049}, {0x0133, 0x0132}, {0x0135, 0x0134},
   {0x0137, 0x0136}, {0x013A, 0x0139}, {0x013C, 0x013B}, {0x013E, 0x013D},
   {0x0140, 0x013F}, {0x0142, 0x0141}, {0x0144, 0x0143}, {0x0146, 0x0145},
   {0x0148, 0x0147}, {0x014B, 0x014A}, {0x014D, 0x014C}, {0x014F, 0x014E},
   {0x0151, 0x0150}, {0x0153, 0x0152}, {0x0155, 0x0154}, {0x0157, 0x0156},
   {0x0159, 0x0158}, {0x015B, 0x015A}, {0x015D, 0x015C}, {0x015F, 0x015E},
   {0x0161, 0x0160}, {0x0163, 0x0162}, {0x0165, 0x0164}, {0x0167, 0x0166},
   {0x0169, 0x0168}, {0x016B, 0x016A}, {0x016D, 0x016C}, {0x016F, 0x016E},
   {0x0171, 0x0170}, {0x0173, 0x0172}, {0x0175, 0x0174}, {0x0177, 0x0176},
   {0x017A, 0x0179}, {0x017C, 0x017B}, {0x017E, 0x017D}, {0x0183, 0x0182},
   {0x0185, 0x0184}, {0x0188, 0x0187}, {0x018C, 0x018B}, {0x0192, 0x0191},
   {0x0199, 0x0198}, {0x01A1, 0x01A0}, {0x01A3, 0x01A2}, {0x01A5, 0x01A4},
   {0x01A8, 0x01A7}, {0x01AD, 0x01AC}, {0x01B0, 0x01AF}, {0x01B4, 0x01B3},
   {0x01B6, 0x01B5}, {0x01B9, 0x01B8}, {0x01BD, 0x01BC}, {0x01C6, 0x01C4},
   {0x01C9, 0x01C7}, {0x01CC, 0x01CA}, {0x01CE, 0x01CD}, {0x01D0, 0x01CF},
   {0x01D2, 0x01D1}, {0x01D4, 0x01D3}, {0x01D6, 0x01D5}, {0x01D8, 0x01D7},
   {0x01DA, 0x01D9}, {0x01DC, 0x01DB}, {0x01DF, 0x01DE}, {0x01E1, 0x01E0},
   {0x01E3, 0x01E2}, {0x01E5, 0x01E4}, {0x01E7, 0x01E6}, {0x01E9, 0x01E8},
   {0x01EB, 0x01EA}, {0x01ED, 0x01EC}, {0x01EF, 0x01EE}, {0x01F3, 0x01F1},
   {0x01F5, 0x01F4}, {0x01FB, 0x01FA}, {0x01FD, 0x01FC}, {0x01FF, 0x01FE},
   {0x0201, 0x0200}, {0x0203, 0x0202}, {0x0205, 0x0204}, {0x0207, 0x0206},
   {0x0209, 0x0208}, {0x020B, 0x020A}, {0x020D, 0x020C}, {0x020F, 0x020E},
   {0x0211, 0x0210}, {0x0213, 0x0212}, {0x0215, 0x0214}, {0x0217, 0x0216},
   {0x0253, 0x0181}, {0x0254, 0x0186}, {0x0257, 0x018A}, {0x0258, 0x018E},
   {0x0259, 0x018F}, {0x025B, 0x0190}, {0x0260, 0x0193}, {0x0263, 0x0194},
   {0x0268, 0x0197}, {0x0269, 0x0196}, {0x026F, 0x019C}, {0x0272, 0x019D},
   {0x0275, 0x019F}, {0x0283, 0x01A9}, {0x0288, 0x01AE}, {0x028A, 0x01B1},
   {0x028B, 0x01B2}, {0x0292, 0x01B7}, {0x03AC, 0x0386}, {0x03AD, 0x0388},
   {0x03AE, 0x0389}, {0x03AF, 0x038A}, {0x03B1, 0x0391}, {0x03B2, 0x0392},
   {0x03B3, 0x0393}, {0x03B4, 0x0394}, {0x03B5, 0x0395}, {0x03B6, 0x0396},
   {0x03B7, 0x0397}, {0x03B8, 0x0398}, {0x03B9, 0x0399}, {0x03BA, 0x039A},
   {0x03BB, 0x039B}, {0x03BC, 0x039C}, {0x03BD, 0x039D}, {0x03BE, 0x039E},
   {0x03BF, 0x039F}, {0x03C0, 0x03A0}, {0x03C1, 0x03A1}, {0x03C3, 0x03A3},
   {0x03C4, 0x03A4}, {0x03C5, 0x03A5}, {0x03C6, 0x03A6}, {0x03C7, 0x03A7},
   {0x03C8, 0x03A8}, {0x03C9, 0x03A9}, {0x03CA, 0x03AA}, {0x03CB, 0x03AB},
   {0x03CC, 0x038C}, {0x03CD, 0x038E}, {0x03CE, 0x038F}, {0x03E3, 0x03E2},
   {0x03E5, 0x03E4}, {0x03E7, 0x03E6}, {0x03E9, 0x03E8}, {0x03EB, 0x03EA},
   {0x03ED, 0x03EC}, {0x03EF, 0x03EE}, {0x0430, 0x0410}, {0x0431, 0x0411},
   {0x0432, 0x0412}, {0x0433, 0x0413}, {0x0434, 0x0414}, {0x0435, 0x0415},
   {0x0436, 0x0416}, {0x0437, 0x0417}, {0x0438, 0x0418}, {0x0439, 0x0419},
   {0x043A, 0x041A}, {0x043B, 0x041B}, {0x043C, 0x041C}, {0x043D, 0x041D},
   {0x043E, 0x041E}, {0x043F, 0x041F}, {0x0440, 0x0420}, {0x0441, 0x0421},
   {0x0442, 0x0422}, {0x0443, 0x0423}, {0x0444, 0x0424}, {0x0445, 0x0425},
   {0x0446, 0x0426}, {0x0447, 0x0427}, {0x0448, 0x0428}, {0x0449, 0x0429},
   {0x044A, 0x042A}, {0x044B, 0x042B}, {0x044C, 0x042C}, {0x044D, 0x042D},
   {0x044E, 0x042E}, {0x044F, 0x042F}, {0x0451, 0x0401}, {0x0452, 0x0402},
   {0x0453, 0x0403}, {0x0454, 0x0404}, {0x0455, 0x0405}, {0x0456, 0x0406},
   {0x0457, 0x0407}, {0x0458, 0x0408}, {0x0459, 0x0409}, {0x045A, 0x040A},
   {0x045B, 0x040B}, {0x045C, 0x040C}, {0x045E, 0x040E}, {0x045F, 0x040F},
   {0x0461, 0x0460}, {0x0463, 0x0462}, {0x0465, 0x0464}, {0x0467, 0x0466},
   {0x0469, 0x0468}, {0x046B, 0x046A}, {0x046D, 0x046C}, {0x046F, 0x046E},
   {0x0471, 0x0470}, {0x0473, 0x0472}, {0x0475, 0x0474}, {0x0477, 0x0476},
   {0x0479, 0x0478}, {0x047B, 0x047A}, {0x047D, 0x047C}, {0x047F, 0x047E},
   {0x0481, 0x0480}, {0x0491, 0x0490}, {0x0493, 0x0492}, {0x0495, 0x0494},
   {0x0497, 0x0496}, {0x0499, 0x0498}, {0x049B, 0x049A}, {0x049D, 0x049C},
   {0x049F, 0x049E}, {0x04A1, 0x04A0}, {0x04A3, 0x04A2}, {0x04A5, 0x04A4},
   {0x04A7, 0x04A6}, {0x04A9, 0x04A8}, {0x04AB, 0x04AA}, {0x04AD, 0x04AC},
   {0x04AF, 0x04AE}, {0x04B1, 0x04B0}, {0x04B3, 0x04B2}, {0x04B5, 0x04B4},
   {0x04B7, 0x04B6}, {0x04B9, 0x04B8}, {0x04BB, 0x04BA}, {0x04BD, 0x04BC},
   {0x04BF, 0x04BE}, {0x04C2, 0x04C1}, {0x04C4, 0x04C3}, {0x04C8, 0x04C7},
   {0x04CC, 0x04CB}, {0x04D1, 0x04D0}, {0x04D3, 0x04D2}, {0x04D5, 0x04D4},
   {0x04D7, 0x04D6}, {0x04D9, 0x04D8}, {0x04DB, 0x04DA}, {0x04DD, 0x04DC},
   {0x04DF, 0x04DE}, {0x04E1, 0x04E0}, {0x04E3, 0x04E2}, {0x04E5, 0x04E4},
   {0x04E7, 0x04E6}, {0x04E9, 0x04E8}, {0x04EB, 0x04EA}, {0x04EF, 0x04EE},
   {0x04F1, 0x04F0}, {0x04F3, 0x04F2}, {0x04F5, 0x04F4}, {0x04F9, 0x04F8},
   {0x0561, 0x0531}, {0x0562, 0x0532}, {0x0563, 0x0533}, {0x0564, 0x0534},
   {0x0565, 0x0535}, {0x0566, 0x0536}, {0x0567, 0x0537}, {0x0568, 0x0538},
   {0x0569, 0x0539}, {0x056A, 0x053A}, {0x056B, 0x053B}, {0x056C, 0x053C},
   {0x056D, 0x053D}, {0x056E, 0x053E}, {0x056F, 0x053F}, {0x0570, 0x0540},
   {0x0571, 0x0541}, {0x0572, 0x0542}, {0x0573, 0x0543}, {0x0574, 0x0544},
   {0x0575, 0x0545}, {0x0576, 0x0546}, {0x0577, 0x0547}, {0x0578, 0x0548},
   {0x0579, 0x0549}, {0x057A, 0x054A}, {0x057B, 0x054B}, {0x057C, 0x054C},
   {0x057D, 0x054D}, {0x057E, 0x054E}, {0x057F, 0x054F}, {0x0580, 0x0550},
   {0x0581, 0x0551}, {0x0582, 0x0552}, {0x0583, 0x0553}, {0x0584, 0x0554},
   {0x0585, 0x0555}, {0x0586, 0x0556}, {0x10D0, 0x10A0}, {0x10D1, 0x10A1},
   {0x10D2, 0x10A2}, {0x10D3, 0x10A3}, {0x10D4, 0x10A4}, {0x10D5, 0x10A5},
   {0x10D6, 0x10A6}, {0x10D7, 0x10A7}, {0x10D8, 0x10A8}, {0x10D9, 0x10A9},
   {0x10DA, 0x10AA}, {0x10DB, 0x10AB}, {0x10DC, 0x10AC}, {0x10DD, 0x10AD},
   {0x10DE, 0x10AE}, {0x10DF, 0x10AF}, {0x10E0, 0x10B0}, {0x10E1, 0x10B1},
   {0x10E2, 0x10B2}, {0x10E3, 0x10B3}, {0x10E4, 0x10B4}, {0x10E5, 0x10B5},
   {0x10E6, 0x10B6}, {0x10E7, 0x10B7}, {0x10E8, 0x10B8}, {0x10E9, 0x10B9},
   {0x10EA, 0x10BA}, {0x10EB, 0x10BB}, {0x10EC, 0x10BC}, {0x10ED, 0x10BD},
   {0x10EE, 0x10BE}, {0x10EF, 0x10BF}, {0x10F0, 0x10C0}, {0x10F1, 0x10C1},
   {0x10F2, 0x10C2}, {0x10F3, 0x10C3}, {0x10F4, 0x10C4}, {0x10F5, 0x10C5},
   {0x1E01, 0x1E00}, {0x1E03, 0x1E02}, {0x1E05, 0x1E04}, {0x1E07, 0x1E06},
   {0x1E09, 0x1E08}, {0x1E0B, 0x1E0A}, {0x1E0D, 0x1E0C}, {0x1E0F, 0x1E0E},
   {0x1E11, 0x1E10}, {0x1E13, 0x1E12}, {0x1E15, 0x1E14}, {0x1E17, 0x1E16},
   {0x1E19, 0x1E18}, {0x1E1B, 0x1E1A}, {0x1E1D, 0x1E1C}, {0x1E1F, 0x1E1E},
   {0x1E21, 0x1E20}, {0x1E23, 0x1E22}, {0x1E25, 0x1E24}, {0x1E27, 0x1E26},
   {0x1E29, 0x1E28}, {0x1E2B, 0x1E2A}, {0x1E2D, 0x1E2C}, {0x1E2F, 0x1E2E},
   {0x1E31, 0x1E30}, {0x1E33, 0x1E32}, {0x1E35, 0x1E34}, {0x1E37, 0x1E36},
   {0x1E39, 0x1E38}, {0x1E3B, 0x1E3A}, {0x1E3D, 0x1E3C}, {0x1E3F, 0x1E3E},
   {0x1E41, 0x1E40}, {0x1E43, 0x1E42}, {0x1E45, 0x1E44}, {0x1E47, 0x1E46},
   {0x1E49, 0x1E48}, {0x1E4B, 0x1E4A}, {0x1E4D, 0x1E4C}, {0x1E4F, 0x1E4E},
   {0x1E51, 0x1E50}, {0x1E53, 0x1E52}, {0x1E55, 0x1E54}, {0x1E57, 0x1E56},
   {0x1E59, 0x1E58}, {0x1E5B, 0x1E5A}, {0x1E5D, 0x1E5C}, {0x1E5F, 0x1E5E},
   {0x1E61, 0x1E60}, {0x1E63, 0x1E62}, {0x1E65, 0x1E64}, {0x1E67, 0x1E66},
   {0x1E69, 0x1E68}, {0x1E6B, 0x1E6A}, {0x1E6D, 0x1E6C}, {0x1E6F, 0x1E6E},
   {0x1E71, 0x1E70}, {0x1E73, 0x1E72}, {0x1E75, 0x1E74}, {0x1E77, 0x1E76},
   {0x1E79, 0x1E78}, {0x1E7B, 0x1E7A}, {0x1E7D, 0x1E7C}, {0x1E7F, 0x1E7E},
   {0x1E81, 0x1E80}, {0x1E83, 0x1E82}, {0x1E85, 0x1E84}, {0x1E87, 0x1E86},
   {0x1E89, 0x1E88}, {0x1E8B, 0x1E8A}, {0x1E8D, 0x1E8C}, {0x1E8F, 0x1E8E},
   {0x1E91, 0x1E90}, {0x1E93, 0x1E92}, {0x1E95, 0x1E94}, {0x1EA1, 0x1EA0},
   {0x1EA3, 0x1EA2}, {0x1EA5, 0x1EA4}, {0x1EA7, 0x1EA6}, {0x1EA9, 0x1EA8},
   {0x1EAB, 0x1EAA}, {0x1EAD, 0x1EAC}, {0x1EAF, 0x1EAE}, {0x1EB1, 0x1EB0},
   {0x1EB3, 0x1EB2}, {0x1EB5, 0x1EB4}, {0x1EB7, 0x1EB6}, {0x1EB9, 0x1EB8},
   {0x1EBB, 0x1EBA}, {0x1EBD, 0x1EBC}, {0x1EBF, 0x1EBE}, {0x1EC1, 0x1EC0},
   {0x1EC3, 0x1EC2}, {0x1EC5, 0x1EC4}, {0x1EC7, 0x1EC6}, {0x1EC9, 0x1EC8},
   {0x1ECB, 0x1ECA}, {0x1ECD, 0x1ECC}, {0x1ECF, 0x1ECE}, {0x1ED1, 0x1ED0},
   {0x1ED3, 0x1ED2}, {0x1ED5, 0x1ED4}, {0x1ED7, 0x1ED6}, {0x1ED9, 0x1ED8},
   {0x1EDB, 0x1EDA}, {0x1EDD, 0x1EDC}, {0x1EDF, 0x1EDE}, {0x1EE1, 0x1EE0},
   {0x1EE3, 0x1EE2}, {0x1EE5, 0x1EE4}, {0x1EE7, 0x1EE6}, {0x1EE9, 0x1EE8},
   {0x1EEB, 0x1EEA}, {0x1EED, 0x1EEC}, {0x1EEF, 0x1EEE}, {0x1EF1, 0x1EF0},
   {0x1EF3, 0x1EF2}, {0x1EF5, 0x1EF4}, {0x1EF7, 0x1EF6}, {0x1EF9, 0x1EF8},
   {0x1F00, 0x1F08}, {0x1F01, 0x1F09}, {0x1F02, 0x1F0A}, {0x1F03, 0x1F0B},
   {0x1F04, 0x1F0C}, {0x1F05, 0x1F0D}, {0x1F06, 0x1F0E}, {0x1F07, 0x1F0F},
   {0x1F10, 0x1F18}, {0x1F11, 0x1F19}, {0x1F12, 0x1F1A}, {0x1F13, 0x1F1B},
   {0x1F14, 0x1F1C}, {0x1F15, 0x1F1D}, {0x1F20, 0x1F28}, {0x1F21, 0x1F29},
   {0x1F22, 0x1F2A}, {0x1F23, 0x1F2B}, {0x1F24, 0x1F2C}, {0x1F25, 0x1F2D},
   {0x1F26, 0x1F2E}, {0x1F27, 0x1F2F}, {0x1F30, 0x1F38}, {0x1F31, 0x1F39},
   {0x1F32, 0x1F3A}, {0x1F33, 0x1F3B}, {0x1F34, 0x1F3C}, {0x1F35, 0x1F3D},
   {0x1F36, 0x1F3E}, {0x1F37, 0x1F3F}, {0x1F40, 0x1F48}, {0x1F41, 0x1F49},
   {0x1F42, 0x1F4A}, {0x1F43, 0x1F4B}, {0x1F44, 0x1F4C}, {0x1F45, 0x1F4D},
   {0x1F51, 0x1F59}, {0x1F53, 0x1F5B}, {0x1F55, 0x1F5D}, {0x1F57, 0x1F5F},
   {0x1F60, 0x1F68}, {0x1F61, 0x1F69}, {0x1F62, 0x1F6A}, {0x1F63, 0x1F6B},
   {0x1F64, 0x1F6C}, {0x1F65, 0x1F6D}, {0x1F66, 0x1F6E}, {0x1F67, 0x1F6F},
   {0x1F80, 0x1F88}, {0x1F81, 0x1F89}, {0x1F82, 0x1F8A}, {0x1F83, 0x1F8B},
   {0x1F84, 0x1F8C}, {0x1F85, 0x1F8D}, {0x1F86, 0x1F8E}, {0x1F87, 0x1F8F},
   {0x1F90, 0x1F98}, {0x1F91, 0x1F99}, {0x1F92, 0x1F9A}, {0x1F93, 0x1F9B},
   {0x1F94, 0x1F9C}, {0x1F95, 0x1F9D}, {0x1F96, 0x1F9E}, {0x1F97, 0x1F9F},
   {0x1FA0, 0x1FA8}, {0x1FA1, 0x1FA9}, {0x1FA2, 0x1FAA}, {0x1FA3, 0x1FAB},
   {0x1FA4, 0x1FAC}, {0x1FA5, 0x1FAD}, {0x1FA6, 0x1FAE}, {0x1FA7, 0x1FAF},
   {0x1FB0, 0x1FB8}, {0x1FB1, 0x1FB9}, {0x1FD0, 0x1FD8}, {0x1FD1, 0x1FD9},
   {0x1FE0, 0x1FE8}, {0x1FE1, 0x1FE9}, {0x24D0, 0x24B6}, {0x24D1, 0x24B7},
   {0x24D2, 0x24B8}, {0x24D3, 0x24B9}, {0x24D4, 0x24BA}, {0x24D5, 0x24BB},
   {0x24D6, 0x24BC}, {0x24D7, 0x24BD}, {0x24D8, 0x24BE}, {0x24D9, 0x24BF},
   {0x24DA, 0x24C0}, {0x24DB, 0x24C1}, {0x24DC, 0x24C2}, {0x24DD, 0x24C3},
   {0x24DE, 0x24C4}, {0x24DF, 0x24C5}, {0x24E0, 0x24C6}, {0x24E1, 0x24C7},
   {0x24E2, 0x24C8}, {0x24E3, 0x24C9}, {0x24E4, 0x24CA}, {0x24E5, 0x24CB},
   {0x24E6, 0x24CC}, {0x24E7, 0x24CD}, {0x24E8, 0x24CE}, {0x24E9, 0x24CF},
   {0xFF41, 0xFF21}, {0xFF42, 0xFF22}, {0xFF43, 0xFF23}, {0xFF44, 0xFF24},
   {0xFF45, 0xFF25}, {0xFF46, 0xFF26}, {0xFF47, 0xFF27}, {0xFF48, 0xFF28},
   {0xFF49, 0xFF29}, {0xFF4A, 0xFF2A}, {0xFF4B, 0xFF2B}, {0xFF4C, 0xFF2C},
   {0xFF4D, 0xFF2D}, {0xFF4E, 0xFF2E}, {0xFF4F, 0xFF2F}, {0xFF50, 0xFF30},
   {0xFF51, 0xFF31}, {0xFF52, 0xFF32}, {0xFF53, 0xFF33}, {0xFF54, 0xFF34},
   {0xFF55, 0xFF35}, {0xFF56, 0xFF36}, {0xFF57, 0xFF37}, {0xFF58, 0xFF38},
   {0xFF59, 0xFF39}, {0xFF5A, 0xFF3A}
};

/*
 * Convert an upper-case letter to the corresponding lower-case letter.
 */
static unsigned int wlc( unsigned int iChar )
{
   /* Number of elements */
   int iLBound = 0;
   int iRBound = sizeof( mUCSU2L ) / sizeof( UCSTransMap ) - 1;
   unsigned int iUC;
   int iPos;

   /* Binary search */
   for( ;; )
   {
      /* Calculate new position */
      iPos = ( iRBound + iLBound ) >> 1;
      iUC = mUCSU2L[iPos].sc;

      /* Search in left half */
      if( iUC > iChar )
      {
         iRBound = iPos;
      }
      /* Search in right half */
      else if( iUC < iChar )
      {
         iLBound = iPos;
      }
      /* Found symbol */
      else
      {
         return mUCSU2L[iPos].dc;
      }

      /* Nothing found */
      if( iRBound - iLBound <= 1 )
      {
         if( iChar == mUCSU2L[iRBound].sc )
         {
            return mUCSU2L[iRBound].dc;
         }
         if( iChar == mUCSU2L[iLBound].sc )
         {
            return mUCSU2L[iLBound].dc;
         }

         return iChar;
      }
   }
}

/*
 * Convert an lower-case letter to the corresponding upper-case letter.
 */
static unsigned int wuc( unsigned int iChar )
{
   /* Number of elements */
   int iLBound = 0;
   int iRBound = sizeof( mUCSL2U ) / sizeof( UCSTransMap ) - 1;
   unsigned int iUC;
   int iPos;

   /* Binary search */
   for( ;; )
   {
      /* Calculate new position */
      iPos = ( iRBound + iLBound ) >> 1;
      iUC = mUCSL2U[iPos].sc;

      /* Search in left half */
      if( iUC > iChar )
      {
         iRBound = iPos;
      }
      /* Search in right half */
      else if( iUC < iChar )
      {
         iLBound = iPos;
      }
      /* Found symbol */
      else
      {
         return mUCSL2U[iPos].dc;
      }

      /* Nothing found */
      if( iRBound - iLBound <= 1 )
      {
         if( iChar == mUCSL2U[iRBound].sc )
         {
            return mUCSL2U[iRBound].dc;
         }
         if( iChar == mUCSL2U[iLBound].sc )
         {
            return mUCSL2U[iLBound].dc;
         }

         return iChar;
      }
   }

}

typedef unsigned int ( ( *CaseFn ) ( unsigned int ) );

/*
 * Convert UTF8 character to wide
 */
static int wtoutf8( unsigned int iUCS, char *sUTF8 )
{
   int iPos;
   int iCharLength;
   unsigned char sUTF8Prefix;

   /* ASCII characters. */
   if( ( iUCS & ~0x0000007F ) == 0 )
   {
      /* Modified UTF-8, special case */
      if( iUCS == 0 )
      {
         sUTF8[0] = 0xC0;
         sUTF8[1] = 0x80;
         return 2;
      }

      sUTF8[0] = ( char ) iUCS;
      return 1;
   }
   if( ( iUCS & ~0x000007FF ) == 0 )
   {
      sUTF8Prefix = 0xC0;       // 11000000b
      iCharLength = 2;
   }
   else if( ( iUCS & ~0x0000FFFF ) == 0 )
   {
      sUTF8Prefix = 0xE0;       // 11100000b
      iCharLength = 3;
   }
   else if( ( iUCS & ~0x001FFFFF ) == 0 )
   {
      sUTF8Prefix = 0xF0;       // 11110000b
      iCharLength = 4;
   }
   else if( ( iUCS & ~0x03FFFFFF ) == 0 )
   {
      sUTF8Prefix = 0xF8;       // 11111000b
      iCharLength = 5;
   }
   else if( ( iUCS & ~0x7FFFFFFF ) == 0 )
   {
      sUTF8Prefix = 0xFC;       // 11111100b
      iCharLength = 6;
   }
   /* Incorrect multibyte character */
   else
   {
      return -1;
   }

   /*
    * Convert UCS character to UTF8. Split value in 6-bit chunks and
    * move to UTF8 string
    */
   for( iPos = iCharLength - 1; iPos > 0; --iPos )
   {
      sUTF8[iPos] = ( iUCS & 0x0000003F ) | 0x80;
      iUCS >>= 6;
   }

   /* UTF8 prefix, special case */
   sUTF8[0] = ( iUCS & 0x000000FF ) | sUTF8Prefix;

/* Return size of UTF8 character */
   return iCharLength;
}

/*
 * Convert character to UTF8
 */
static int utf8tow( const char *sUTF8, int iUTF8Length, unsigned int *iUCSResult )
{
   unsigned int iUCS;
   int iPos;
   int iCharLength;
   unsigned char ucPrefix;
   unsigned int uBoundary;

   /* Incorrect multibyte sequence */
   if( iUTF8Length == 0 || sUTF8 == NULL )
   {
      return -1;
   }

   /*
      Determine size of UTF8 character & check "shortest form" of char
    */
   ucPrefix = ( unsigned char ) ( *sUTF8 );

   /* 10000000b  & 00000000b */
   if( ( ucPrefix & 0x80 ) == 0 )
   {
      ucPrefix = 0x7F;
      iCharLength = 1;
      uBoundary = 0;
   }
   /* 11100000b & 11000000b */
   else if( ( ucPrefix & 0xE0 ) == 0xC0 )
   {
      ucPrefix = 0x1F;
      iCharLength = 2;
      uBoundary = 0x80;
   }
   /* 11110000b & 11100000b */
   else if( ( ucPrefix & 0xF0 ) == 0xE0 )
   {
      ucPrefix = 0x0F;
      iCharLength = 3;
      uBoundary = 0x00000800;
   }
   /* 11111000b & 11110000b */
   else if( ( ucPrefix & 0xF8 ) == 0xF0 )
   {
      ucPrefix = 0x07;
      iCharLength = 4;
      uBoundary = 0x00010000;
   }
   /* 11111100b & 11111000b */
   else if( ( ucPrefix & 0xFC ) == 0xF8 )
   {
      ucPrefix = 0x03;
      iCharLength = 5;
      uBoundary = 0x00200000;
   }
   /* 11111110b & 11111100b */
   else if( ( ucPrefix & 0xFE ) == 0xFC )
   {
      ucPrefix = 0x01;
      iCharLength = 6;
      uBoundary = 0x04000000;
   }
   /* Invalid UTF8 sequence */
   else
   {
      return -1;
   }

   /* Invalid UTF8 sequence */
   if( iUTF8Length < iCharLength )
   {
      return -1;
   }

   /* Special case for first character */
   iUCS = ( unsigned char ) ( *sUTF8 ) & ucPrefix;
   ++sUTF8;
   for( iPos = 1; iPos < iCharLength; ++iPos )
   {
      /* Incorect characters in middle of string */
      if( ( ( *sUTF8 ) & 0xC0 ) != 0x80 )
      {
         return -1;
      }

      /* Join value from 6-bit chunks */
      iUCS <<= 6;
      iUCS |= ( *sUTF8 ) & 0x3F;
      ++sUTF8;
   }

   /* Check boundary */
   if( iUCS < uBoundary )
   {
      /* Modified UTF-8, special case */
      if( !( iUCS == 0 && uBoundary == 0x80 ) )
      {
         /* Not a "shortest form" of char */
         return -1;
      }
   }

   *iUCSResult = iUCS;

   return iCharLength;
}

/*
 * Make a UTF8 string upper or lowercase
 */
static int changeutf8case( const char *szSrc, int iSrcLen, char **szDst,
      int *iDstLen, CaseFn pCaseFn )
{
   int iAllocated = iSrcLen;
   char *szTMPDst = ( char * ) malloc( sizeof( char ) * iAllocated );
   int iRealDstLen = 0;
   int iProcessedSrc;
   int iProcessedDst;
   //int iChars = 0;

   for( ;; )
   {
      unsigned int iUCS = 0;
      /* Convert symbol from UTF8 to UCS */
      iProcessedSrc = utf8tow( szSrc, iSrcLen, &iUCS );
      /* Incorrect input character */
      if( iProcessedSrc == -1 )
      {
         free( szTMPDst );
         return -1;
      }

      szSrc += iProcessedSrc;
      iSrcLen -= iProcessedSrc;

      /* Modifies UTF-8. Special case for end-of-string */
      if( iSrcLen == 0 && iUCS == 0 )
      {
         szTMPDst[iRealDstLen] = 0;
         break;
      }

      /* Change case */
      iUCS = ( ( *pCaseFn ) ( iUCS ) );

      /* Convert symbol from UCS to UTF8 */
      iProcessedDst = wtoutf8( iUCS, szTMPDst + iRealDstLen );
      /* Incorrect lowercase character; I hope, this should NEVER happened */
      if( iProcessedDst == -1 )
      {
         free( szTMPDst );
         return -2;
      }
      /* Store processed size */
      iRealDstLen += iProcessedDst;

      /* Check free memory in destination buffer */
      if( iRealDstLen + 6 > iAllocated )
      {
         char *szReallocated;

         /* Reallocate buffer to 1.5 sizes of original */
         iAllocated = iAllocated + iAllocated / 2;
         szReallocated = realloc( szTMPDst, iAllocated * sizeof( char ) );
         /* Cannot reallocate memory */
         if( szReallocated == NULL )
         {
            free( szTMPDst );
            return -3;
         }
         szTMPDst = szReallocated;
      }

      /* Exit condition */
      if( iSrcLen == 0 )
      {
         break;
      }


      //++iChars;
   }

   //*iDstLen = iChars;
   *iDstLen = iRealDstLen;
   *szDst = szTMPDst;
   return 0;
}

/*
 * Make a UTF8 string lowercase
 */
static int utf8lcstr( const char *szSrc, int iSrcLen, char **szDst, int *iDstLen )
{
   return changeutf8case( szSrc, iSrcLen, szDst, iDstLen, wlc );
}

/*
 * Make a UTF8 string uppercase
 */
static int utf8ucstr( const char *szSrc, int iSrcLen, char **szDst, int *iDstLen )
{
   return changeutf8case( szSrc, iSrcLen, szDst, iDstLen, wuc );
}

HB_FUNC( CEDI_UTF8_LOWER )
{
   const char * szSrc = hb_parc(1);
   int iSrcLen = hb_parclen(1);
   char * szDst = NULL;
   int    iDstLen = 0;

   if( utf8lcstr( szSrc, iSrcLen, &szDst, &iDstLen ) != 0 )
      hb_retclen( szSrc, iSrcLen );

   hb_retclen( szDst, iDstLen );
   free( szDst );
}

HB_FUNC( CEDI_UTF8_UPPER )
{
   const char * szSrc = hb_parc(1);
   int iSrcLen = hb_parclen(1);
   char * szDst = NULL;
   int    iDstLen = 0;

   if( utf8ucstr( szSrc, iSrcLen, &szDst, &iDstLen ) != 0 )
      hb_retclen( szSrc, iSrcLen );

   //_writelog( "ac.log", 0, ": %d %d\r\n", iSrcLen, iDstLen );
   hb_retclen( szDst, iDstLen );
   free( szDst );
}