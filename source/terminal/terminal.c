/*
 */

#include "vterm.h"

#if defined(__linux__) || defined(__unix__)
   #include <unistd.h>
#else
   #include <io.h>
#endif

#define _XOPEN_SOURCE 700
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <fcntl.h>
#include <errno.h>
#include <pty.h>
#include <poll.h>
#include <sys/wait.h>
#include <termios.h>
#include <locale.h>
#include <wchar.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbgtcore.h"
#include "hbapigt.h"
#include "inkey.ch"

extern int argv_split(const char *s, char ***out_argv);

static int colors[8] = { 0, 4, 2, 6, 1, 5, 3, 7 };

typedef struct {
    VTerm       *vt;
    VTermScreen *screen;
    int          rows, cols;
    int          cursor_row, cursor_col, cursor_visible;
    VTermRect    dirty;
    int          have_dirty;
    int          fVgaCell;
    int          pty_master_fd;  /* Мастер-дескриптор псевдотерминала */
    struct       pollfd fds[1];
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
              pbyBuffer = ((HB_BYTE *) (ctx->pBuffer)) + (row * iCols + col) * 2;
              *pbyBuffer++ = utf32_to_char( cp );
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
    while (off < len) {
        ssize_t w = write(ctx->pty_master_fd, s + off, len - off);
        if (w > 0) { off += (size_t)w; continue; }
        if (w < 0 && errno == EINTR) continue;
        break;
    }
}

/*
 * Запуск шелла (или другой программы) в псевдотерминале.
 * Возвращает мастер-дескриптор PTY или -1 при ошибке.
 */
static int spawn_in_pty( char *const argv[], int rows, int cols ) {
    int master_fd;
    struct winsize ws = {
        .ws_row = (unsigned short)rows,
        .ws_col = (unsigned short)cols,
        .ws_xpixel = 0,
        .ws_ypixel = 0
    };

    pid_t pid = forkpty(&master_fd, NULL, NULL, &ws);
    if (pid < 0) {
        perror("forkpty");
        return -1;
    }

    if (pid == 0) {
        //setenv("TERM", "xterm-256color", 1);
        setenv("TERM", "xterm", 1);
        //c_writelog( NULL, 0, "cmd: %s\n", argv[0] );
        execvp( argv[0], argv );
        perror( "execvp" );
        _exit( 127 );
    }

    /* Родительский процесс: возвращаем мастер-дескриптор. */
    return master_fd;
}

HB_FUNC( PTY_RUN )
{

   char * szCmd = (char*) hb_parc( 1 );
   int nRows = hb_parni( 2 );
   int nCols = hb_parni( 3 );
   int i, iLen;
   char *ptr;
   htty_ctx_t * ctx;

   ctx = hb_xgrab( sizeof(htty_ctx_t) );
   ctx->rows = nRows;
   ctx->cols = nCols;
   ctx->cursor_visible = 1;

   setlocale(LC_ALL, "");

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

   char **argv;
   argv_split( szCmd, &argv );
   ctx->pty_master_fd = spawn_in_pty( argv, nRows, nCols );
   free( argv );
   if (ctx->pty_master_fd < 0) {
      vterm_free(ctx->vt);
      hb_xfree( ctx );
      hb_ret();
      return;
   }

   vterm_output_set_callback(ctx->vt, cb_output, ctx);

   ctx->fds[0].fd = ctx->pty_master_fd;
   ctx->fds[0].events = POLLIN;

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

   if( ctx->pBuffer ) {
      free( ctx->pBuffer );
      ctx->pBuffer = NULL;
   }
   close(ctx->pty_master_fd);
   vterm_free(ctx->vt);
   hb_xfree( ctx );
}

HB_FUNC( PTY_CHECK )
{

   htty_ctx_t * ctx = (htty_ctx_t *) hb_parptr( 1 );
   char pty_buf[4096];

   int poll_ret = poll( ctx->fds, 1, 5 ); /* -1 = ждать бесконечно */
   if( poll_ret < 0 ) {
      if( errno == EINTR ) {
         /* Прервано сигналом — просто повторяем */
         hb_retni( 0 );
         return;
      }
      hb_retni( -1 );
      return;
   }

   /* 1. Данные от vim (PTY) */
   if( ctx->fds[0].revents & POLLIN ) {
       ssize_t n = read( ctx->pty_master_fd, pty_buf, sizeof(pty_buf) );
       if( n > 0 ) {
           size_t consumed = vterm_input_write(ctx->vt, pty_buf, (size_t)n);
           (void)consumed;
           /* БЕЗ ЭТОГО при DAMAGE_SCREEN не придёт ни одного damage */
           vterm_screen_flush_damage(ctx->screen);
           htty_render_flush( ctx );

           hb_retni( 0 );
           return;
       } else if( n == 0 ) {
           hb_retni( -3 );
           return;
       } else if( errno != EINTR && errno != EIO ) {
           hb_retni( -4 );
           return;
       } else if (errno == EIO) {   /* Linux: EIO вместо EOF при закрытии slave */
           hb_retni( -5 );
           return;
       }
   }

   /* Проверка на завершение дочернего процесса (опционально) */
   if( ctx->fds[0].revents & (POLLHUP | POLLERR) ) {
      hb_retni( -2 );
      return;
   }

   hb_retni( 0 );
   return;

}

HB_FUNC( PTY_SENDKEY )
{
   htty_ctx_t * ctx = (htty_ctx_t *) hb_parptr( 1 );
   int iKey = hb_parni(2), iExt = hb_parni(3);
   char key_buf[16];
   int n = -1;

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
      vterm_keyboard_key( ctx->vt, n, (iExt)? VTERM_MOD_CTRL : VTERM_MOD_NONE );
      n = vterm_output_read( ctx->vt, key_buf, 16 );
   } else if( iExt ) {
      vterm_keyboard_unichar( ctx->vt, n, VTERM_MOD_CTRL );
      n = vterm_output_read( ctx->vt, key_buf, 16 );
   } else {
      n = hb_cdpU16CharToUTF8( key_buf, ( HB_WCHAR ) iKey );
   }

   //c_writelog( NULL, 0, "sendkey: %s\n", key_buf );
   ssize_t written = write( ctx->pty_master_fd, key_buf, (size_t)n );
   if (written < 0 && errno != EINTR)
      hb_retni( -2 );
   else
      hb_retni( 0 );

   return;
}

HB_FUNC( PTY_ISEXISTS )
{
   hb_retl( 1 );
   return;
}