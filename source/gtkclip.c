#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>

#include <gtk/gtk.h>

static char* shm_file_name = "hbedit_cb";
void t2shm( const char * s )
{
   int shm, iLen = strlen(s)+1;
   char *addr;

   if( (shm = shm_open(shm_file_name, O_CREAT|O_RDWR, 0777)) == -1 ) {
      printf("shm_open");
      return;
   }
   if ( ftruncate( shm, iLen) == -1 ) {
      printf("ftruncate");
      return;
   }
   addr = mmap(0, iLen, PROT_WRITE|PROT_READ, MAP_SHARED, shm, 0);
   if( addr == (char*)-1 ) {
      printf("mmap");
      return;
   }

   memcpy(addr, s, iLen-1);
   addr[iLen] = '\0';

   munmap(addr, iLen);
   close(shm);
}

void t2cb( const char * s )
{
   GtkClipboard* clipboard = gtk_clipboard_get( GDK_SELECTION_CLIPBOARD );
   if( clipboard ) {
      gtk_clipboard_set_text( clipboard, s, -1 );
      gtk_clipboard_store( clipboard );
   }
}

void cb2t( int bMem )
{
   GtkClipboard* clipboard = gtk_clipboard_get( GDK_SELECTION_CLIPBOARD );
   if( clipboard ) {
      if( gtk_clipboard_wait_is_text_available( clipboard ) ) {
         if( bMem )
            t2shm( gtk_clipboard_wait_for_text( clipboard ) );
         else
            printf( gtk_clipboard_wait_for_text( clipboard ) );
      }
   }
}

int main( int argc, char **argv )
{

   gtk_init( 0,0 );
   if( argc > 1 ) {
      if( strcmp(argv[1],"-gm") == 0 )
         cb2t( 1 );
      else if( strcmp(argv[1],"-g") == 0 )
         cb2t( 0 );
      else if( strcmp(argv[1],"-tm") == 0 && gdk_display_get_default() )
         t2shm( "y" );
      else if( strcmp(argv[1],"-t") == 0 && gdk_display_get_default() )
         printf( "y" );
      else if( strcmp(argv[1],"-um") == 0 )
         shm_unlink( shm_file_name );
      else if( argc > 2 && strcmp(argv[1],"-s") == 0 )
         t2cb( argv[2]);
   }
   return 0;
}
