/*
 */

#if defined(__linux__) || defined(__unix__)
   #include <unistd.h>
#else
   #include <io.h>
#endif

#define _XOPEN_SOURCE 700
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
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

#include "tmt.h"

static int сolors[8] = { 0, 4, 2, 6, 1, 5, 3, 7 };
typedef struct {
    int fVgaCell;
    TMT *vt;              /* Экземпляр виртуального терминала libtmt */
    int   pty_master_fd;  /* Мастер-дескриптор псевдотерминала */
    struct pollfd fds[1];
    void * pBuffer;
} htty_ctx_t;

/*
void c_writelog( const char * sFile, int n, const char * sTraceMsg, ... )
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
*/

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

// Конвертирует один wchar_t в UTF-8 строку
// Возвращает количество записанных байт (1-4), или 0 при ошибке
size_t wchar_to_utf8(wchar_t wc, char *out) {
    if (wc < 0x80) {
        out[0] = (char)wc;
        return 1;
    } else if (wc < 0x800) {
        out[0] = 0xC0 | (wc >> 6);
        out[1] = 0x80 | (wc & 0x3F);
        return 2;
    } else if (wc < 0x10000) {
        out[0] = 0xE0 | (wc >> 12);
        out[1] = 0x80 | ((wc >> 6) & 0x3F);
        out[2] = 0x80 | (wc & 0x3F);
        return 3;
    } else if (wc <= 0x10FFFF) {
        out[0] = 0xF0 | (wc >> 18);
        out[1] = 0x80 | ((wc >> 12) & 0x3F);
        out[2] = 0x80 | ((wc >> 6) & 0x3F);
        out[3] = 0x80 | (wc & 0x3F);
        return 4;
    }
    return 0; // Невалидный символ
}

/* Вызывается, когда libtmt сообщает об изменении экрана.
 * Здесь вы должны прочитать состояние из vt (tmt_screen, tmt_cursor)
 * и перерисовать соответствующую область в интерфейсе менеджера. */
static void htty_render_update(TMT *vt, void *user_data) {

   htty_ctx_t * ctx = (htty_ctx_t *) user_data;
   HB_BYTE * pbyBuffer = ( HB_BYTE * ) ctx->pBuffer;
   const TMTSCREEN *screen = tmt_screen(vt);
   TMTATTRS attr;
   int iColorFG, iColorBG;
   bool iAdd = 0;

   //fprintf(stderr, "[htty] Render update: %zu rows, %zu cols, cursor at (%zu, %zu), %zu char size\n",
   //        screen->nline, screen->ncol, cursor->r, cursor->c, sizeof(wchar_t) );

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
         iColorFG = сolors[iColorFG];
         if( iAdd )
            iColorFG += 8;

         iColorBG = attr.bg - 1;
         if( iColorBG < 0 )
            iColorBG = 0;
         else if( iColorBG > 7 )
            iColorBG = 7;
         iColorBG = сolors[iColorBG];
         iColorBG <<= 4;

         if( ctx->fVgaCell ) {
            *pbyBuffer++ = utf32_to_char( ch );
            *pbyBuffer++ = ( HB_BYTE ) 15;
         } else {
            HB_PUT_LE_UINT16( pbyBuffer, utf32_to_utf16( ch ) );
            pbyBuffer += 2;
            *pbyBuffer++ = ( HB_BYTE ) iColorFG + iColorBG;
            *pbyBuffer++ = 0;
         }

         //memset( out, 0, 8 );
         //wchar_to_utf8( ch, out );
         //c_writelog( NULL, 0, "%s", out );
      }
      //c_writelog( NULL, 0, "\n" );
   }
   //c_writelog( NULL, 7, "------" );
   hb_gtRest( 0, 0, screen->nline-1, screen->ncol-1, ctx->pBuffer );
}

/* ====================================================================
 * Callback-функция, которую вызывает libtmt.
 * ==================================================================== */
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
        fprintf(stderr, "[htty] BEL\n");
        break;

    default:
        break;
    }
}

/* ====================================================================
 * Запуск шелла (или другой программы) в псевдотерминале.
 * Возвращает мастер-дескриптор PTY или -1 при ошибке.
 * ==================================================================== */
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
        /* Дочерний процесс: здесь мы уже «внутри» PTY.
         * Просто запускаем нужную программу. */
        setenv( "TERM", "ansi", 1 );
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
   htty_ctx_t * ctx;

   ctx = hb_xgrab( sizeof(htty_ctx_t) );

   setlocale(LC_ALL, "");

   char *argv[] = { szCmd, NULL };
   ctx->pty_master_fd = spawn_in_pty( argv, nRows, nCols );
   if( ctx->pty_master_fd < 0 ) {
      tmt_close( ctx->vt );
      hb_xfree( ctx );
      hb_ret();
      return;
   }

   ctx->fds[0].fd = ctx->pty_master_fd;
   ctx->fds[0].events = POLLIN;

   ctx->fVgaCell = hb_gt_Base()->fVgaCell;
   //c_writelog( NULL, 0, "pty_run.vgacell: %d %lu\n", (ctx->fVgaCell)? 1:0, (unsigned long int)ctx );
   ctx->pBuffer = malloc( nRows * nCols * ( (ctx->fVgaCell)? 2:4 ) );

   ctx->vt = tmt_open( nRows, nCols, tmt_callback, ctx, NULL );
   if( !(ctx->vt) ) {
      hb_xfree( ctx );
      hb_ret();
      return;
   }

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
   tmt_close(ctx->vt);
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
       if (n > 0) {
           tmt_write( ctx->vt, pty_buf, (size_t)n );
           hb_retni( 0 );
           return;
       } else if (n == 0) {
           hb_retni( -3 );
           return;
       } else if (errno != EINTR) {
           hb_retni( -4 );
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
   char * key_buf = (char*) hb_parc( 2 );
   int n = strlen( key_buf );

   //c_writelog( NULL, 0, "sendkey: %s\n", key_buf );
   ssize_t written = write( ctx->pty_master_fd, key_buf, (size_t)n );
   if (written < 0 && errno != EINTR)
      hb_retni( -2 );
   else
      hb_retni( 0 );

   return;
}