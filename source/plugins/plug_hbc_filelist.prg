/*
 * HbCommander plugin to create a current pane files list
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

FUNCTION plug_hbc_filelist( oPane )

   LOCAL arr := Iif( Empty(oPane:aSelected), oPane:aDir, oPane:aSelected ), i, s := "", oNew

   FOR i := 1 TO Len( arr )
      s += arr[i,1] + Chr(10)
   NEXT

   oNew := mnu_NewBuf( TEdit():aWindows[TEdit():nCurr], "$FileList", s )
   oNew:cp := "UTF8"
   hb_cdpSelect( oNew:cp )
   oNew:lUtf8 := .T.

   RETURN Nil