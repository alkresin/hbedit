/*
 * HbEdit plugin to show a dbf file on a pane
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

FUNCTION hbc_dbf( cFileName )

   LOCAL oPane := FilePane():PaneCurr()
   LOCAL cColor := "W/B", cp

   SetColor( cColor )
   cp := hb_cdpSelect( "RU866" )
   @ oPane:y1, oPane:x1, oPane:y2, oPane:x2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )


   RETURN Nil