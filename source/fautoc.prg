/*
 * AutoComplete functions
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"

FUNCTION edi_DoAuC( oEdit, lAuto )

   LOCAL oy, ox
   LOCAL ny := oEdit:nLine, nx1, nx2 := oEdit:nPos
   LOCAL cPrefix
   LOCAL arr, hTrieLang, hTrie
   LOCAL x1, y1, x2, y2, h, w, nSel, nFirst
   LOCAL bufc, cColor, cColorSel
   LOCAL nKeyExt, nKey, lPassKey, lRedraw, lRecalc := .T.

   IF Substr( oEdit:aText[ny], nx2-1, 1 ) == ' '
      RETURN .F.
   ENDIF
   nx1 := edi_PrevWord( oEdit, .T., .F., .F., ny, nx2-1 )
   IF nx2 - nx1 <= 1
      RETURN .T.
   ENDIF

   IF !Empty( oEdit:bAutoC )
      hTrie := Eval( oEdit:bAutoC, oEdit, Substr( oEdit:aText[ny], nx1, nx2-nx1 ) )
   ENDIF
   hTrieLang := Iif( !Empty(oEdit:oHili), hb_hGetDef( oEdit:oHili:hHili, "htrie", Nil ), Nil )
   IF Empty( hTrieLang ) .AND. Empty( oEdit:bAutoC )
      RETURN .F.
   ENDIF

   DO WHILE .T.

      lPassKey := .F.
      IF lRecalc
         oy := Row()
         ox := Col()
         nx2 := oEdit:nPos
         cPrefix := Substr( oEdit:aText[ny], nx1, nx2-nx1 )
         arr := MakeArr( hTrieLang, hTrie, cPrefix )

         bufc := Nil
         IF Empty( arr )
            EXIT
         ELSEIF Len( arr ) == 1
            IF nx2-nx1 == cp_Len( oEdit:lUtf8, arr[1] )
               EXIT
            ELSEIF lAuto
               Replace( oEdit, ny, nx1, nx2, arr[1] )
               EXIT
            ENDIF
         ENDIF
	
         h := Min( Len( arr ),12 ) + 2
         w := 0
         AEval( arr, {|s|w := Max( w, Len(s) )} )
         y1 := Iif( oy < oEdit:y1+h, oy, oy-h+1 )
         x1 := ox
         y2 := y1 + h - 1
         x2 := x1 + w + 2
         nSel := 1
         nFirst := 1
         bufc := SaveScreen( y1, x1, y2, x2 )
         lRedraw := .T.
      ENDIF
      IF lRedraw
         DrawArr( arr, y1, x1, y2, x2, nFirst, nSel, oEdit:cColor, oEdit:cColorSel )
      ENDIF

      lRedraw := lRecalc := .F.
      nKeyExt := Inkey( 0, HB_INKEY_ALL + HB_INKEY_EXT )
      nKey := hb_keyStd(nKeyExt)

      IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE ;
         .OR. nKey == K_LBUTTONUP .OR. nKey == K_RBUTTONUP
         LOOP

      ELSEIF ( nKey >= K_SPACE .AND. nKey <= 255 ) .OR. ( oEdit:lUtf8 .AND. nKey > 3000 )
         RestScreen( y1, x1, y2, x2, bufc )
         oEdit:onKey( nKeyExt )
         lRecalc := .T.

      ELSEIF nKey == K_ESC
         DevPos( oy, ox )
         EXIT

      ELSEIF nKey == K_TAB
         lPassKey := .T.
         DevPos( oy, ox )
         EXIT

      ELSEIF nKey == K_UP
         IF nSel > 1
            nSel --
            lRedraw := .T.
         ELSEIF nFirst > 1
            nFirst --
            lRedraw := .T.
         ENDIF

      ELSEIF nKey == K_DOWN
         IF nSel < h-2
            nSel ++
            lRedraw := .T.
         ELSEIF nFirst + nSel <= Len(arr)
            nFirst ++
            lRedraw := .T.
         ENDIF

      ELSEIF nKey == K_PGUP
         IF nFirst == 1
            nSel := 1
         ENDIF
         nFirst := Max( 1, nFirst-(h-2) )
         lRedraw := .T.

      ELSEIF nKey == K_PGDN
         IF nFirst+h-2 <= Len(arr)-(h-2)+1
            nFirst := nFirst + h - 2
         ELSE
            nSel := h-2
            nFirst := Len(arr) - nSel + 1
         ENDIF
         lRedraw := .T.

      ELSEIF nKey == K_HOME
         nSel := nFirst := 1
         lRedraw := .T.

      ELSEIF nKey == K_END
         nSel := h-2
         nFirst := Len(arr) - nSel + 1
         lRedraw := .T.

      ELSEIF nKey == K_ENTER
         RestScreen( y1, x1, y2, x2, bufc )
         bufc := Nil
         Replace( oEdit, ny, nx1, nx2, arr[nFirst-1+nSel] )
         EXIT

      ENDIF

   ENDDO

   IF !Empty( bufc )
      RestScreen( y1, x1, y2, x2, bufc )
   ENDIF
   IF !Empty( hTrie )
      trie_Close( hTrie )
   ENDIF
   IF lPassKey
      oEdit:onKey( nKeyExt )
   ENDIF

   RETURN .T.

FUNCTION hbc_DoAuC( oHbc, cmd )

   LOCAL oy, ox
   LOCAL cRes := ""
   LOCAL arr, hTrie
   LOCAL x1, y1, x2, y2, h, w, nSel, nFirst
   LOCAL bufc, cColor, cColorSel
   LOCAL nKeyExt, nKey, lRedraw, lRecalc := .T.

   IF Empty( hTrie := FilePane():hCmdTrie )
      RETURN ""
   ENDIF

   x1 := Col() - Len(cmd)
   y2 := ( oy := Row() ) - 1

   DO WHILE .T.

      IF lRecalc
         //oy := Row()
         ox := Col()
         arr := MakeArr( hTrie,, cmd )

         bufc := Nil
         IF Empty( arr )
            cRes := ""
            EXIT
         ENDIF
	
         h := Min( Len( arr ),12 ) + 2
         w := 0
         AEval( arr, {|s|w := Max( w, Len(s) )} )
         //y1 := Iif( oy < oHbc:y1+h, oy, oy-h+1 )
         y1 := Iif( oy < oHbc:y1+h, oy-1, oy-h )
         //x1 := ox
         //y2 := y1 + h - 1
         x2 := x1 + w + 2
         nSel := 1
         nFirst := 1
         bufc := SaveScreen( y1, x1, y2, x2 )
         lRedraw := .T.
      ENDIF
      IF lRedraw
         DrawArr( arr, y1, x1, y2, x2, nFirst, nSel, oHbc:cColor, oHbc:cColorSel )
      ENDIF

      lRedraw := lRecalc := .F.
      nKeyExt := Inkey( 0, HB_INKEY_ALL + HB_INKEY_EXT )
      nKey := hb_keyStd(nKeyExt)

      IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE ;
         .OR. nKey == K_LBUTTONUP .OR. nKey == K_RBUTTONUP
         LOOP

      ELSEIF ( nKey >= K_SPACE .AND. nKey <= 255 ) .OR. ( oHbc:lUtf8 .AND. nKey > 3000 )
         RestScreen( y1, x1, y2, x2, bufc )
         cmd += Chr( nKey )
         DevPos( oy, ox )
         DevOut( Chr(nKey) )
         lRecalc := .T.

      ELSEIF nKey == K_ESC
         //cRes := ""
         //DevPos( oy, ox )
         cRes := cmd
         EXIT

      ELSEIF nKey == K_UP
         IF nSel > 1
            nSel --
            lRedraw := .T.
         ELSEIF nFirst > 1
            nFirst --
            lRedraw := .T.
         ENDIF

      ELSEIF nKey == K_DOWN
         IF nSel < h-2
            nSel ++
            lRedraw := .T.
         ELSEIF nFirst + nSel <= Len(arr)
            nFirst ++
            lRedraw := .T.
         ENDIF

      ELSEIF nKey == K_PGUP
         IF nFirst == 1
            nSel := 1
         ENDIF
         nFirst := Max( 1, nFirst-(h-2) )
         lRedraw := .T.

      ELSEIF nKey == K_PGDN
         IF nFirst+h-2 <= Len(arr)-(h-2)+1
            nFirst := nFirst + h - 2
         ELSE
            nSel := h-2
            nFirst := Len(arr) - nSel + 1
         ENDIF
         lRedraw := .T.

      ELSEIF nKey == K_HOME
         nSel := nFirst := 1
         lRedraw := .T.

      ELSEIF nKey == K_END
         nSel := h-2
         nFirst := Len(arr) - nSel + 1
         lRedraw := .T.

      ELSEIF nKey == K_ENTER
         cRes := arr[nFirst-1+nSel]
         EXIT

      ENDIF

   ENDDO

   IF !Empty( bufc )
      RestScreen( y1, x1, y2, x2, bufc )
   ENDIF

   RETURN cRes

STATIC FUNCTION MakeArr( hTrieLang, hTrie, cPrefix )

   LOCAL arr, cList, arr1

   IF !Empty( hTrieLang )
      IF !Empty( cList := trie_List( hTrieLang, cPrefix ) )
         arr := hb_ATokens( cList, Chr(10) )
      ENDIF
   ENDIF
   IF !Empty( hTrie )
      IF !Empty( cList := trie_List( hTrie, cPrefix ) )
         arr1 := hb_ATokens( cList, Chr(10) )
         IF Empty( arr )
            arr := arr1
         ELSE
            arr := ASize( arr, Len(arr) + Len(arr1) )
            ACopy( arr1, arr,,, Len(arr) - Len(arr1) + 1 )
         ENDIF
      ENDIF
   ENDIF

   RETURN arr

STATIC FUNCTION DrawArr( arr, y1, x1, y2, x2, nFirst, nSel, cColor, cColorSel )

   LOCAL clr := SetColor( cColor ), i, i1 := 1

   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   FOR i := y1+1 TO y2-1
      SetColor( Iif( i1==nSel, cColorSel, cColor ) )
      @ i, x1+1 SAY arr[nFirst-1+i1++]
   NEXT

   SetColor( clr )

   RETURN Nil

STATIC FUNCTION Replace( oEdit, ny, nx1, nx2, cWord )

   oEdit:InsText( ny, nx2, Substr( cWord, nx2-nx1+1 ) )

   RETURN Nil
