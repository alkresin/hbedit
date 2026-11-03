#define NTDDI_VERSION 0x0A000006  // NTDDI_WIN10_RS5 (Windows 10 1809+)
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0A00       // Windows 10

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tmt.h"

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbgtcore.h"
#include "hbapigt.h"

#define CMDLENGTH  4096

typedef HRESULT (WINAPI *PFN_CREATE_PSEUDOCONSOLE)(
    COORD size, HANDLE hInput, HANDLE hOutput, DWORD dwFlags, HPCON* phPC);

typedef void (WINAPI *PFN_RESIZE_PSEUDOCONSOLE)(HPCON hPC, COORD size);

typedef void (WINAPI *PFN_CLOSE_PSEUDOCONSOLE)(HPCON hPC);

/* Структура с указателями на функции */
typedef struct {
    PFN_CREATE_PSEUDOCONSOLE Create;
    PFN_RESIZE_PSEUDOCONSOLE Resize;
    PFN_CLOSE_PSEUDOCONSOLE  Close;
} ConPtyApi;

static ConPtyApi g_conpty = { NULL, NULL, NULL };

static int colors[8] = { 0, 4, 2, 6, 1, 5, 3, 7 };

typedef struct {
    TMT *vt;              /* Экземпляр виртуального терминала libtmt */
    HANDLE hPtyInputWrite;
    HANDLE hPtyOutputRead;
    STARTUPINFOEX siEx;
    PROCESS_INFORMATION pi;
    HPCON hPC;
    HANDLE waitHandles[1];
    int fVgaCell;
    void * pBuffer;
} htty_ctx_t;

static void c_writelog( const char * sFile, int n, const char * sTraceMsg, ... )
{
   FILE *hFile;

   if( sFile == NULL )
   {
      hFile = fopen( "term.log", "a" );
   }
   else
   {
      hFile = fopen( sFile, "a" );
   }

   if( hFile )
   {
      if( n ) {
         fwrite( sTraceMsg, 1, n, hFile );
         fwrite( "\n\r", 1, 2, hFile );
      } else {
         va_list ap;

         va_start( ap, sTraceMsg );
         vfprintf( hFile, sTraceMsg, ap );
         va_end( ap );
      }
      fclose( hFile );
   }
}

static BOOL LoadConPty(void)
{
    HMODULE hKernel32 = GetModuleHandleW(L"kernel32.dll");
    if (!hKernel32) return FALSE;

    g_conpty.Create = (PFN_CREATE_PSEUDOCONSOLE)
        GetProcAddress(hKernel32, "CreatePseudoConsole");
    g_conpty.Resize = (PFN_RESIZE_PSEUDOCONSOLE)
        GetProcAddress(hKernel32, "ResizePseudoConsole");
    g_conpty.Close  = (PFN_CLOSE_PSEUDOCONSOLE)
        GetProcAddress(hKernel32, "ClosePseudoConsole");

    return (g_conpty.Create != NULL);
}

static BOOL IsConPtyAvailable(void)
{
    return g_conpty.Create != NULL;
}

uint16_t utf32_to_utf16( wchar_t cp ) {
    if( cp >= 0x10000 )
        return 63;  // ?

    return (uint16_t) cp;
}

unsigned char utf32_to_char( wchar_t cp ) {
    if( cp >= 256 )
        return 63;  // ?

    return (char) cp;
}

static void htty_render_update(TMT *vt, void *user_data) {
   htty_ctx_t * ctx = (htty_ctx_t *) user_data;
   HB_BYTE * pbyBuffer = ( HB_BYTE * ) ctx->pBuffer;
   const TMTSCREEN *screen = tmt_screen(vt);
   const TMTPOINT *cursor = tmt_cursor(vt);
   TMTATTRS attr;
   int iColorFG, iColorBG;
   bool iAdd = 0;

   //c_writelog( NULL, 0, "render-1 %d %d %d\n", ctx->fVgaCell, screen->nline, screen->ncol );
   for (size_t r = 0; r < screen->nline; r++) {
      for (size_t c = 0; c < screen->ncol; c++) {
         wchar_t ch = screen->lines[r]->chars[c].c;

         attr = screen->lines[r]->chars[c].a;
         iColorFG = attr.fg - 1;
         if( iColorFG < 0 ) {
            iAdd = ( iColorFG == -1 || attr.bold );
            iColorFG = 7;
         }
         else if( iColorFG > 7 )
            iColorFG = 7;
         iColorFG = colors[iColorFG];
         if( iAdd )
            iColorFG += 8;

         iColorBG = attr.bg - 1;
         if( iColorBG < 0 )
            iColorBG = 0;
         else if( iColorBG > 7 )
            iColorBG = 7;
         iColorBG = colors[iColorBG];
         iColorBG <<= 4;

         if( ctx->fVgaCell ) {
            //if( ch != 32 )
            //   c_writelog( NULL, 0, "%lu %u %c\n", ch, utf32_to_char( ch ), utf32_to_char( ch ) );
            *pbyBuffer++ = utf32_to_char( ch );
            *pbyBuffer++ = ( HB_BYTE ) iColorFG + iColorBG;
         } else {
            //if( ch != 32 )
            //   c_writelog( NULL, 0, "%lu %u %c\n", ch, utf32_to_utf16( ch ), utf32_to_utf16( ch ) );
            HB_PUT_LE_UINT16( pbyBuffer, utf32_to_utf16( ch ) );
            pbyBuffer += 2;
            *pbyBuffer++ = ( HB_BYTE ) iColorFG + iColorBG;
            *pbyBuffer++ = 0;
         }
      }
   }
   hb_gtRest( 0, 0, screen->nline-1, screen->ncol-1, ctx->pBuffer );
   hb_gtSetPos( cursor->r, cursor->c );
}

static void tmt_callback(tmt_msg_t msg, TMT *vt, const void *a, void *p) {
    htty_ctx_t *ctx = (htty_ctx_t *)p;
    switch (msg) {
    case TMT_MSG_UPDATE:
        htty_render_update(vt, ctx);
        break;
    case TMT_MSG_MOVED:
       hb_gtSetPos( ((const TMTPOINT *)a)->r, ((const TMTPOINT *)a)->c);
       break;
    case TMT_MSG_BELL:
        //fprintf(stderr, "[htty] BEL\n");
        break;

    default:
        break;
    }
}

static int spawn_in_pty( htty_ctx_t * ctx, char *szCmd, int rows, int cols ) {

   // 1. Создаем пайпы для ConPTY
   HANDLE hPtyInputRead, hPtyInputWrite;
   HANDLE hPtyOutputRead, hPtyOutputWrite;
   TCHAR wc1[CMDLENGTH];

   if (!CreatePipe(&hPtyInputRead, &hPtyInputWrite, NULL, 0) ||
       !CreatePipe(&hPtyOutputRead, &hPtyOutputWrite, NULL, 0)) {
       c_writelog( "pty.err", 0, "Failed to create pipes.\n");
       return 1;
   }

   // 2. Инициализируем Pseudo Console (ConPTY)
   COORD consoleSize = { cols, rows };
   HPCON hPC = NULL;
   HRESULT hr = g_conpty.Create(consoleSize, hPtyInputRead, hPtyOutputWrite, 0, &hPC);
   if (FAILED(hr)) {
       c_writelog( "pty.err", 0, "Failed to create pseudo console. Error: %lx\n", hr);
       return 1;
   }

   // 3. Настраиваем STARTUPINFOEX для передачи ConPTY дочернему процессу
   memset( &(ctx->siEx), 0, sizeof(STARTUPINFOEX) );
   ctx->siEx.StartupInfo.cb = sizeof(STARTUPINFOEX);

   SIZE_T attrListSize = 0;
   InitializeProcThreadAttributeList(NULL, 1, 0, &attrListSize);
   ctx->siEx.lpAttributeList = (PPROC_THREAD_ATTRIBUTE_LIST)malloc(attrListSize);

   if (!InitializeProcThreadAttributeList(ctx->siEx.lpAttributeList, 1, 0, &attrListSize)) {
       c_writelog( "pty.err", 0, "Failed to init attribute list.\n");
       return 1;
   }

   UpdateProcThreadAttribute(ctx->siEx.lpAttributeList, 0,
                             PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
                             hPC, sizeof(hPC), NULL, NULL);

   // 4. Запускаем дочерний процесс (PowerShell)
   MultiByteToWideChar( GetACP(), 0, szCmd, -1, (LPWSTR) wc1, CMDLENGTH );
   BOOL success = CreateProcessW(
       NULL, (LPWSTR) wc1, NULL, NULL, FALSE,
       EXTENDED_STARTUPINFO_PRESENT, // Флаг для использования STARTUPINFOEX
       NULL, NULL, &(ctx->siEx), &(ctx->pi) );

   if (!success) {
       c_writelog( "pty.err", 0, "Failed to create process. Error: %lu\n", GetLastError());
       return 1;
   }

   // Закрываем ненужные концы пайпов в нашем процессе (они теперь у ConPTY)
   CloseHandle( hPtyInputRead );
   CloseHandle( hPtyOutputWrite );

   ctx->hPC = hPC;
   ctx->hPtyInputWrite = hPtyInputWrite;
   ctx->hPtyOutputRead = hPtyOutputRead;
   ctx->waitHandles[0] = hPtyOutputRead;

   return 0;
}

HB_FUNC( PTY_RUN )
{

   char * szCmd = (char*) hb_parc( 1 );
   int nRows = hb_parni( 2 );
   int nCols = hb_parni( 3 );
   htty_ctx_t * ctx;

   if( !IsConPtyAvailable() && !LoadConPty() ) {
       c_writelog( "pty.err", 0, "Failed to load conPTY\n");
       hb_ret();
       return;
   }

   ctx = hb_xgrab( sizeof(htty_ctx_t) );

   ctx->fVgaCell = hb_gt_Base()->fVgaCell;
   ctx->pBuffer = malloc( nRows * nCols * ( (ctx->fVgaCell)? 2:4 ) );
   //c_writelog( NULL, 0, "run-1 %lu %d\n", (long unsigned int)ctx, ctx->fVgaCell );

   ctx->vt = tmt_open( nRows, nCols, tmt_callback, ctx, NULL );
   if( !(ctx->vt) ) {
      hb_xfree( ctx );
      hb_ret();
      return;
   }

   if( spawn_in_pty( ctx, szCmd, nRows, nCols ) ) {
      tmt_close( ctx->vt );
      hb_xfree( ctx );
      hb_ret();
      return;
   }

   hb_retptr( (void*) ctx );

}

HB_FUNC( PTY_CLOSE )
{
   htty_ctx_t * ctx = (htty_ctx_t *) hb_parptr( 1 );

   CloseHandle( ctx->hPtyInputWrite );
   CloseHandle( ctx->hPtyOutputRead );
   CloseHandle( ctx->pi.hProcess );
   CloseHandle( ctx->pi.hThread );

   g_conpty.Close( ctx->hPC );
   DeleteProcThreadAttributeList( ctx->siEx.lpAttributeList );
   free( ctx->siEx.lpAttributeList );

   if( ctx->pBuffer ) {
      free( ctx->pBuffer );
      ctx->pBuffer = NULL;
   }
   tmt_close(ctx->vt);
   hb_xfree( ctx );
}

HB_FUNC( PTY_CHECK )
{

   htty_ctx_t * ctx = (htty_ctx_t *) hb_parptr( 1 );
   char pty_buf[4096];
   //char wstr[4096];
   //size_t char_count;
   DWORD bytesRead = 0;
   DWORD bytesAvailable = 0;

   if( WaitForSingleObject( ctx->pi.hProcess, 0 ) == WAIT_OBJECT_0 ) {
      hb_retni( -1 );
      return;
   }
   DWORD waitResult = WaitForMultipleObjects( 1, ctx->waitHandles, FALSE, 10 );

   if( PeekNamedPipe( ctx->hPtyOutputRead, NULL, 0, NULL, &bytesAvailable, NULL ) ) {
      if( bytesAvailable > 0 ) {
         if( waitResult == WAIT_OBJECT_0 ) {
            // Пришли данные из PTY (вывод консоли)
            if( ReadFile( ctx->hPtyOutputRead, pty_buf, sizeof(pty_buf), &bytesRead, NULL) && bytesRead > 0 ) {
               //c_writelog( "a1.log", bytesRead, pty_buf );
               //char_count = hb_cdpUTF8ToStr( hb_vmCDP(), pty_buf, bytesRead, wstr, 4096 );
               //c_writelog( "a2.log", char_count, wstr );
               //tmt_write( ctx->vt, (const char*)wstr, char_count );
               tmt_write( ctx->vt, pty_buf, bytesRead );
            } else {
               if( GetLastError() == ERROR_BROKEN_PIPE ) {
                  hb_retni( -1 );
                  return;
               }
            }
         }
      }
   } else {
      if( GetLastError() == ERROR_BROKEN_PIPE ) {
         hb_retni( -1 );
         return;
      }
   }
   if (waitResult == WAIT_TIMEOUT || bytesAvailable == 0) {
       Sleep(1);
   }

   hb_retni( 0 );
   return;

}

HB_FUNC( PTY_SENDKEY )
{
   htty_ctx_t * ctx = (htty_ctx_t *) hb_parptr( 1 );
   char * key_buf = (char*) hb_parc( 2 );
   int n = strlen( key_buf );
   DWORD written;

   WriteFile( ctx->hPtyInputWrite, key_buf, n, &written, NULL );
   if( written < 0 )
      hb_retni( -2 );
   else
      hb_retni( 0 );

   return;
}

HB_FUNC( PTY_ISEXISTS )
{
   if( !IsConPtyAvailable() )
      hb_retl( LoadConPty() );
   else
      hb_retl( 1 );
   return;
}