/*
 */

#define NTDDI_VERSION 0x0A000006          // NTDDI_WIN10_RS5 (Windows 10 1809+)
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0A00 // Windows 10

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>

#include <vterm.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbgtcore.h"
#include "hbapigt.h"
#include "inkey.ch"

#define CMDLENGTH  4096

static int colors[8] = { 0, 4, 2, 6, 1, 5, 3, 7 };

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

typedef struct {
    VTerm       *vt;
    VTermScreen *screen;

    HANDLE hPtyInputWrite;
    HANDLE hPtyOutputRead;
    STARTUPINFOEX siEx;
    PROCESS_INFORMATION pi;
    HPCON hPC;
    HANDLE waitHandles[1];

    int          rows, cols;
    int          cursor_row, cursor_col, cursor_visible;
    VTermRect    dirty;
    int          have_dirty;
    int          fVgaCell;
    void *       pBuffer;
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

char utf32_to_char( wchar_t cp ) {
    if( cp >= 256 )
        return 63;  // ?

    return (char) cp;
}

/* ---------- helpers ---------- */

static void dirty_all(htty_ctx_t *ctx) {
    ctx->dirty.start_row = 0;
    ctx->dirty.end_row   = ctx->rows;
    ctx->dirty.start_col = 0;
    ctx->dirty.end_col   = ctx->cols;
    ctx->have_dirty = 1;
}

static void dirty_add(htty_ctx_t *ctx, VTermRect r) {
    if (!ctx->have_dirty) {
        ctx->dirty = r;
        ctx->have_dirty = 1;
        return;
    }
    if (r.start_row < ctx->dirty.start_row) ctx->dirty.start_row = r.start_row;
    if (r.end_row   > ctx->dirty.end_row)   ctx->dirty.end_row   = r.end_row;
    if (r.start_col < ctx->dirty.start_col) ctx->dirty.start_col = r.start_col;
    if (r.end_col   > ctx->dirty.end_col)   ctx->dirty.end_col   = r.end_col;
}

/* ---------- рендер только грязного региона ---------- */
static void htty_render_flush(htty_ctx_t *ctx) {

   PHB_CODEPAGE cdp = hb_vmCDP();

   if (!ctx->have_dirty)
       return;

   VTermRect r = ctx->dirty;
   HB_BYTE * pbyBuffer;
   int iCols = ctx->cols + 1;

   ctx->have_dirty = 0;

   if (r.end_row > ctx->rows) r.end_row = ctx->rows;
   if (r.end_col > ctx->cols) r.end_col = ctx->cols;

   //c_writelog( NULL, 0, "[htty] Render update: from %d/%d to %d/%d max: %d/%d\n",
   //   r.start_row, r.start_col, r.end_row, r.end_col, ctx->rows, ctx->cols );
   for (int row = r.start_row; row < r.end_row; row++) {
       for (int col = r.start_col; col < r.end_col; col++) {
           VTermScreenCell cell;
           VTermPos pos = { .row = row, .col = col };

           if (!vterm_screen_get_cell(ctx->screen, pos, &cell))
               continue;

           uint32_t cp = cell.chars[0];
           /* 0 и (uint32_t)-1 ? лхвост╗ широкого символа */
           if (cp == 0 || cp == (uint32_t)-1)
               cp = ' ';

           int fg_idx = 0;
           int bg_idx = 0;

           // --- Обработка цвета текста (Foreground) ---
           if (VTERM_COLOR_IS_INDEXED(&cell.fg)) {
               // Обращаемся к полю idx структуры indexed
               fg_idx = cell.fg.indexed.idx;
           }
           else if (VTERM_COLOR_IS_DEFAULT_FG(&cell.fg)) {
               // Если цвет "по умолчанию" (стандартный для терминала)
               fg_idx = (cell.attrs.bold) ? 15 : 7;
           }

           // --- Обработка цвета фона (Background) ---
           if (VTERM_COLOR_IS_INDEXED(&cell.bg)) {
               bg_idx = cell.bg.indexed.idx;
           }
           else if (VTERM_COLOR_IS_DEFAULT_BG(&cell.bg)) {
               // Фон по умолчанию
               bg_idx = 0;
           }

           // Если базовый цвет (0..7) используется с атрибутом bold,
           // многие терминалы отображают его как яркий (8..15).
           if (fg_idx >= 0 && fg_idx < 8 && cell.attrs.bold) {
               fg_idx += 8;
           }
           if( fg_idx > 7 ) {
              fg_idx = colors[fg_idx-8] + 8;
           } else
              fg_idx = colors[fg_idx];
           if( bg_idx > 7 ) {
              bg_idx = colors[bg_idx-8];
           } else
              bg_idx = colors[bg_idx];

           if( ctx->fVgaCell ) {
              //c_writelog( NULL, 0, "%d %d: %x %x %x %x\n", row, col, cp, utf32_to_char( cp ), hb_cdpGetChar(cdp,cp), hb_cdpGetU16(cdp,cp) );
              pbyBuffer = ((HB_BYTE *) (ctx->pBuffer)) + (row * iCols + col) * 2;
              //*pbyBuffer++ = utf32_to_char( cp );
              *pbyBuffer++ = hb_cdpGetChar( cdp,cp );
              *pbyBuffer = ( HB_BYTE ) fg_idx + (bg_idx << 4);
           } else {
              pbyBuffer = ((HB_BYTE *) (ctx->pBuffer)) + (row * iCols + col) * 4;
              HB_PUT_LE_UINT16( pbyBuffer, utf32_to_utf16( cp ) );
              pbyBuffer += 2;
              *pbyBuffer++ = ( HB_BYTE ) fg_idx + (bg_idx << 4);
              *pbyBuffer = 0;
           }
           //c_writelog( NULL, 0, "%d %d: %x\n", row, col, cp );
       }
   }
   //c_writelog( NULL, 0, ": %d\n", pbyBuffer - ((HB_BYTE *) (ctx->pBuffer)) );
   hb_gtRest( 0, 0, ctx->rows, ctx->cols, ctx->pBuffer );
}

/* ---------- callbacks ЭКРАНА (не состояния!) ---------- */

static int cb_damage(VTermRect rect, void *user) {
   dirty_add((htty_ctx_t *)user, rect);
   return 1;
}

static int cb_moverect(VTermRect dest, VTermRect src, void *user) {
   htty_ctx_t *ctx = user;
   dirty_add(ctx, dest);
   dirty_add(ctx, src);
   return 1;
}

static int cb_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
   (void)oldpos;
   htty_ctx_t *ctx = user;
   ctx->cursor_row     = pos.row;
   ctx->cursor_col     = pos.col;
   ctx->cursor_visible = visible;
   hb_gtSetPos( pos.row, pos.col );
   //fprintf(stderr, "[htty-1] Cursor moved to (%d, %d)\n", pos.row, pos.col );
   return 1;
}

static int cb_settermprop(VTermProp prop, VTermValue *val, void *user) {
   htty_ctx_t *ctx = user;
   if (prop == VTERM_PROP_CURSORVISIBLE) {
       ctx->cursor_visible = val->boolean;
       hb_gtSetPos( ctx->cursor_row, ctx->cursor_col );
       //fprintf(stderr, "[htty-2] Cursor moved to (%d, %d)\n", ctx->cursor_row, ctx->cursor_col );
   }
   return 1;
}

static int cb_resize(int rows, int cols, void *user) {
   htty_ctx_t *ctx = user;
   ctx->rows = rows;
   ctx->cols = cols;
   dirty_all(ctx);
   return 1;
}

static int cb_bell(void *user) { (void)user; return 1; }

static int cb_sb_pushline(int cols, const VTermScreenCell *cells, void *user) {
   (void)cols; (void)cells; (void)user;
   return 1;
}

static int cb_sb_popline(int cols, VTermScreenCell *cells, void *user) {
   (void)cols; (void)cells; (void)user;
   return 0;
}

static const VTermScreenCallbacks screen_cbs = {
   .damage      = cb_damage,
   .moverect    = cb_moverect,
   .movecursor  = cb_movecursor,
   .settermprop = cb_settermprop,
   .bell        = cb_bell,
   .resize      = cb_resize,
   .sb_pushline = cb_sb_pushline,
   .sb_popline  = cb_sb_popline,
};

/* ---------- ответы терминала обратно в PTY ----------
 * vim шлёт запросы (DA, DSR, DECRQSS...). Без ответов он может
 * подвисать на таймаутах. libvterm формирует ответы сам ? их надо
 * забрать и записать в мастер. */

static void cb_output(const char *s, size_t len, void *user) {
    htty_ctx_t *ctx = user;
    size_t off = 0;
    DWORD written;

    while (off < len) {
        WriteFile( ctx->hPtyInputWrite, s+off, len-off, &written, NULL );
        if (written > 0) { off += (size_t)written; continue; }
        if (written < 0 && errno == EINTR) continue;
        break;
    }
}

/*
 * Запуск шелла (или другой программы) в псевдотерминале.
 * Возвращает мастер-дескриптор PTY или -1 при ошибке.
 */
static int spawn_in_pty( htty_ctx_t * ctx, char *szCmd, int rows, int cols ) {

   // 1. Создаем пайпы для ConPTY
   HANDLE hPtyInputRead, hPtyInputWrite;
   HANDLE hPtyOutputRead, hPtyOutputWrite;
   TCHAR wc1[CMDLENGTH];

   if (!CreatePipe(&hPtyInputRead, &hPtyInputWrite, NULL, 0) ||
       !CreatePipe(&hPtyOutputRead, &hPtyOutputWrite, NULL, 0)) {
       printf("Failed to create pipes.\n");
       return 1;
   }

   // 2. Инициализируем Pseudo Console (ConPTY)
   COORD consoleSize = { 80, 24 };
   HPCON hPC = NULL;
   HRESULT hr = g_conpty.Create(consoleSize, hPtyInputRead, hPtyOutputWrite, 0, &hPC);
   if (FAILED(hr)) {
       printf("Failed to create pseudo console. Error: %lx\n", hr);
       return 1;
   }

   // 3. Настраиваем STARTUPINFOEX для передачи ConPTY дочернему процессу
   memset( &(ctx->siEx), 0, sizeof(STARTUPINFOEX) );
   ctx->siEx.StartupInfo.cb = sizeof(STARTUPINFOEX);

   SIZE_T attrListSize = 0;
   InitializeProcThreadAttributeList(NULL, 1, 0, &attrListSize);
   ctx->siEx.lpAttributeList = (PPROC_THREAD_ATTRIBUTE_LIST)malloc(attrListSize);

   if (!InitializeProcThreadAttributeList(ctx->siEx.lpAttributeList, 1, 0, &attrListSize)) {
       printf("Failed to init attribute list.\n");
       return 1;
   }

   UpdateProcThreadAttribute(ctx->siEx.lpAttributeList, 0,
                             PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
                             hPC, sizeof(hPC), NULL, NULL);

   // 4. Запускаем дочерний процесс (PowerShell)
   MultiByteToWideChar( GetACP(), 0, szCmd, -1, wc1, CMDLENGTH );
   BOOL success = CreateProcessW(
       NULL, wc1, NULL, NULL, FALSE,
       EXTENDED_STARTUPINFO_PRESENT, // Флаг для использования STARTUPINFOEX
       NULL, NULL, &(ctx->siEx), &(ctx->pi) );

   if (!success) {
       printf("Failed to create process. Error: %lu\n", GetLastError());
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
   int i, iLen;
   char *ptr;
   htty_ctx_t * ctx;

   if( !IsConPtyAvailable() && !LoadConPty() ) {
       printf("Failed to load conPTY\n");
       hb_ret();
       return;
   }

   ctx = hb_xgrab( sizeof(htty_ctx_t) );
   ctx->rows = nRows;
   ctx->cols = nCols;
   ctx->cursor_visible = 1;

   ctx->vt = vterm_new( nRows, nCols );
   if (!ctx->vt) {
      hb_ret();
      return;
   }
   vterm_set_utf8(ctx->vt, 1);

   /* ВАЖНО: никаких vterm_state_set_callbacks ? экран владеет state сам */
   ctx->screen = vterm_obtain_screen(ctx->vt);
   vterm_screen_set_callbacks(ctx->screen, &screen_cbs, ctx);
   vterm_screen_enable_altscreen(ctx->screen, 1);
   vterm_screen_reset(ctx->screen, 1);
   /* merge выставляем ПОСЛЕ reset: reset внутри трогает это поле */
   vterm_screen_set_damage_merge(ctx->screen, VTERM_DAMAGE_SCREEN);

   if( spawn_in_pty( ctx, szCmd, nRows, nCols ) ) {
      vterm_free(ctx->vt);
      hb_xfree( ctx );
      hb_ret();
      return;
   }

   vterm_output_set_callback(ctx->vt, cb_output, ctx);

   ctx->fVgaCell = hb_gt_Base()->fVgaCell;
   //c_writelog( NULL, 0, "pty_run.vgacell: %d rows: %d cols: %d\n", (ctx->fVgaCell)? 1:0, nRows, nCols );
   iLen = (nRows+1) * (nCols+1);
   ctx->pBuffer = malloc( iLen * ( (ctx->fVgaCell)? 2:4 ) );
   ptr = (char*) (ctx->pBuffer);
   for( i=0; i<iLen; i++ ) {
      *ptr = (char)32;
      ptr ++;
      if( !ctx->fVgaCell ) {
         *ptr = (char)0;
         ptr ++;
      }
      *ptr = (char)15;
      ptr ++;
      if( !ctx->fVgaCell ) {
        *ptr = (char)0;
         ptr ++;
      }
   }
   //c_writelog( NULL, 0, "buflen: %d  %d\n", iLen * ( (ctx->fVgaCell)? 2:4 ), ptr - ((char *) (ctx->pBuffer)) );

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

   vterm_free(ctx->vt);
   hb_xfree( ctx );
}

HB_FUNC( PTY_CHECK )
{

   htty_ctx_t * ctx = (htty_ctx_t *) hb_parptr( 1 );
   char pty_buf[4096];
   DWORD bytesRead = 0;
   DWORD bytesAvailable = 0;

   if( WaitForSingleObject( ctx->pi.hProcess, 0 ) == WAIT_OBJECT_0 ) {
      hb_retni( -1 );
      return;
   }
   DWORD waitResult = WaitForMultipleObjects( 1, ctx->waitHandles, FALSE, 10 );
   //c_writelog( NULL, "-- 1 --\n" );

   if( PeekNamedPipe( ctx->hPtyOutputRead, NULL, 0, NULL, &bytesAvailable, NULL ) ) {
      if( bytesAvailable > 0 ) {
         if( waitResult == WAIT_OBJECT_0 ) {
            // Пришли данные из PTY (вывод консоли)
            if( ReadFile( ctx->hPtyOutputRead, pty_buf, sizeof(pty_buf), &bytesRead, NULL) && bytesRead > 0 ) {
                size_t consumed = vterm_input_write(ctx->vt, pty_buf, (size_t)bytesRead);
                (void)consumed;
                /* БЕЗ ЭТОГО при DAMAGE_SCREEN не придёт ни одного damage */
                vterm_screen_flush_damage(ctx->screen);
                htty_render_flush( ctx );

                hb_retni( 0 );
                return;
            } else {
               if( GetLastError() == ERROR_BROKEN_PIPE ) {
                  hb_retni( -2 );
                  return;
               }
            }
         }
      }
   } else {
      if( GetLastError() == ERROR_BROKEN_PIPE ) {
         hb_retni( -3 );
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
   int iKey = hb_parni(2);
   char key_buf[16];
   int n = -1;
   DWORD written;

   switch( iKey ) {
      case K_UP:
         n = VTERM_KEY_UP;
         break;
      case K_DOWN:
         n = VTERM_KEY_DOWN;
         break;
      case K_RIGHT:
         n = VTERM_KEY_RIGHT;
         break;
      case K_LEFT:
         n = VTERM_KEY_LEFT;
         break;
      case K_HOME:
         n = VTERM_KEY_HOME;
         break;
      case K_END:
         n = VTERM_KEY_END;
         break;
      case K_PGDN:
         n = VTERM_KEY_PAGEDOWN;
         break;
      case K_PGUP:
         n = VTERM_KEY_PAGEUP;
         break;
      case K_F1:
         n = VTERM_KEY_FUNCTION(1);
         break;
      case K_F2:
         n = VTERM_KEY_FUNCTION(2);
         break;
      case K_F3:
         n = VTERM_KEY_FUNCTION(3);
         break;
      case K_F4:
         n = VTERM_KEY_FUNCTION(4);
         break;
      case K_F5:
         n = VTERM_KEY_FUNCTION(5);
         break;
      case K_F6:
         n = VTERM_KEY_FUNCTION(6);
         break;
      case K_F7:
         n = VTERM_KEY_FUNCTION(7);
         break;
      case K_F8:
         n = VTERM_KEY_FUNCTION(8);
         break;
      case K_F9:
         n = VTERM_KEY_FUNCTION(9);
         break;
      case K_F10:
         n = VTERM_KEY_FUNCTION(10);
         break;
      case K_F11:
         n = VTERM_KEY_FUNCTION(11);
         break;
      case K_F12:
         n = VTERM_KEY_FUNCTION(12);
         break;
   }
   if( n >= 0 ) {
      vterm_keyboard_key( ctx->vt, n, VTERM_MOD_NONE );
      n = vterm_output_read( ctx->vt, key_buf, 16 );
   } else {
      n = hb_cdpU16CharToUTF8( key_buf, ( HB_WCHAR ) iKey );
   }

   //c_writelog( NULL, 0, "sendkey: %s\n", key_buf );
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