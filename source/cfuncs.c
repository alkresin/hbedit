/*
 * A set of C functions for a text editor
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include <stdio.h>
#include "hbapi.h"

HB_FUNC( CEDI_REDIRON )
{
   int istd = (HB_ISNIL(1))? 1 : hb_parni(1);

   freopen( hb_parc(2), "w", (istd==1)? stdout : stderr );
}

HB_FUNC( CEDI_REDIROFF )
{
   int istd = (HB_ISNIL(1))? 1 : hb_parni(1);
   fclose( (istd==1)? stdout : stderr );
}

#define BUFSIZE  1024

#if defined(__linux__) || defined(__unix__)

HB_FUNC( CEDI_RUNCONSOLEAPP ) 
{ 
    /* Ensure that output of command does interfere with stdout */
    fflush(stdin);
    FILE *cmd_file = (FILE *) popen( hb_parc(1), "r" );
    FILE *hOut;
    char buf[BUFSIZE];
    int bytes_read, iOutExist = 0, iExitCode;

    if( !cmd_file )
    {
        hb_retni( -1 );
        return;
    }

    if( !HB_ISNIL(2) )
    {
       hOut = fopen( hb_parc(2), "w" );
       iOutExist = 1;
    }

    do
    {
        bytes_read = fread( buf, sizeof(char), BUFSIZE, cmd_file );
        if( iOutExist )
           fwrite( buf, 1, bytes_read, hOut );
    } while (bytes_read == BUFSIZE);
 
    iExitCode = pclose(cmd_file);
    if( iOutExist )
       fclose( hOut );

    hb_retni( iExitCode ); 
}

#else

#include <windows.h>

HB_FUNC( CEDI_RUNCONSOLEAPP )
{
   SECURITY_ATTRIBUTES sa; 
   HANDLE g_hChildStd_OUT_Rd = NULL;
   HANDLE g_hChildStd_OUT_Wr = NULL;
   PROCESS_INFORMATION pi;
   STARTUPINFO si;
   BOOL bSuccess;

   DWORD dwRead, dwWritten, dwExitCode;
   CHAR chBuf[BUFSIZE]; 
   HANDLE hOut = NULL;

   sa.nLength = sizeof(SECURITY_ATTRIBUTES); 
   sa.bInheritHandle = TRUE; 
   sa.lpSecurityDescriptor = NULL; 

   // Create a pipe for the child process's STDOUT. 
   if( ! CreatePipe( &g_hChildStd_OUT_Rd, &g_hChildStd_OUT_Wr, &sa, 0 ) )
   {
      hb_retni(1);
      return;
   }

   // Ensure the read handle to the pipe for STDOUT is not inherited.
   if( ! SetHandleInformation( g_hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0 ) )
   {
      hb_retni(2);
      return;
   }

   // Set up members of the PROCESS_INFORMATION structure. 
   ZeroMemory( &pi, sizeof(PROCESS_INFORMATION) );
 
   // Set up members of the STARTUPINFO structure. 
   // This structure specifies the STDIN and STDOUT handles for redirection.
   ZeroMemory( &si, sizeof(si) );
   si.cb = sizeof(si);
   si.wShowWindow = SW_HIDE;
   si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
   si.hStdOutput = g_hChildStd_OUT_Wr;
   si.hStdError = g_hChildStd_OUT_Wr;

   bSuccess = CreateProcess( NULL, (LPTSTR)hb_parc(1), NULL, NULL,
      TRUE, CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi);
   
   if ( ! bSuccess ) 
   {
      hb_retni(3);
      return;
   }

   WaitForSingleObject( pi.hProcess, INFINITE );
   GetExitCodeProcess( pi.hProcess, &dwExitCode );
   CloseHandle( pi.hProcess );
   CloseHandle( pi.hThread );
   CloseHandle( g_hChildStd_OUT_Wr );

   if( !HB_ISNIL(2) )
   {
      hOut = CreateFile( hb_parc(2), GENERIC_WRITE, 0, 0,
             CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
   }
   while( 1 ) 
   { 
      bSuccess = ReadFile( g_hChildStd_OUT_Rd, chBuf, BUFSIZE, &dwRead, NULL );
      if( ! bSuccess || dwRead == 0 ) break; 

      if( !HB_ISNIL(2) )
      {
         bSuccess = WriteFile( hOut, chBuf, dwRead, &dwWritten, NULL );
         if( ! bSuccess ) break; 
      }
   } 

   if( !HB_ISNIL(2) )
      CloseHandle( hOut );
   CloseHandle( g_hChildStd_OUT_Rd );

   hb_retni( (int) dwExitCode);
}
#endif
