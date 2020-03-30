#include <gtk/gtk.h>

void t2cb( const char * s )
{
   GtkClipboard* clipboard = gtk_clipboard_get( GDK_SELECTION_CLIPBOARD );
   if( clipboard ) {
      gtk_clipboard_set_text( clipboard, s, -1 );
      gtk_clipboard_store( clipboard );
   }
}

void cb2t( void )
{
   GtkClipboard* clipboard = gtk_clipboard_get( GDK_SELECTION_CLIPBOARD );
   if( clipboard ) {
      if( gtk_clipboard_wait_is_text_available( clipboard ) ) {
         printf( gtk_clipboard_wait_for_text( clipboard ) );
      }
      else
         printf( "" );
   }
}

int main( int argc, char **argv )
{

   gtk_init( 0,0 );
   if( argc > 2 && strcmp(argv[1],"-s") == 0 )
      t2cb( argv[2]);
   else if( argc > 1 && strcmp(argv[1],"-g") == 0 )
      cb2t();
   return 0;
}
