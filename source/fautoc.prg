/*
 * AutoComplete functions
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"

FUNCTION edi_DoAuC( oEdit )

   LOCAL oy := Row(), ox := Col()
   LOCAL ny := oEdit:nLine, nx2 := oEdit:nPos
   LOCAL nx1 := edi_PrevWord( oEdit, .T., .F., .T., ny, nx2-1 )
   LOCAL arr
   LOCAL x1, y1, x2, y2, h, w := 0, nSel := 1, nFirst := 1
   LOCAL bufc, cColor, cColorSel
   LOCAL nKeyExt, nKey, lPassKey, lRedraw

   IF nx2 - nx1 <= 1
      RETURN Nil
   ENDIF

   arr := { Dtoc( Date() ), Time() }

   h := Min( Len( arr ),12 ) + 2
   AEval( arr, {|s|w := Max( w, Len(s) )} )
   y1 := Iif( oy < oEdit:y1+h, oy, oy-h+1 )
   x1 := ox
   y2 := y1 + h - 1
   x2 := x1 + w + 2
   bufc := SaveScreen( y1, x1, y2, x2 )
   DrawArr( arr, y1, x1, y2, x2, nFirst, nSel, oEdit:cColor, oEdit:cColorSel )

   DO WHILE .T.

      lPassKey := lRedraw := .F.
      nKeyExt := Inkey( 0, HB_INKEY_ALL + HB_INKEY_EXT )
      nKey := hb_keyStd(nKeyExt)

      IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE ;
         .OR. nKey == K_LBUTTONUP .OR. nKey == K_RBUTTONUP
         LOOP

      ELSEIF nKey == K_ESC
         EXIT

      ELSEIF nKey == K_TAB
         lPassKey := .T.
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

      ELSEIF nKey == K_ENTER
      ENDIF

      IF lRedraw
         DrawArr( arr, y1, x1, y2, x2, nFirst, nSel, oEdit:cColor, oEdit:cColorSel )
      ENDIF

   ENDDO

   RestScreen( y1, x1, y2, x2, bufc )
   DevPos( oy, ox )
   IF lPassKey
      oEdit:onKey( nKeyExt )
   ENDIF

   RETURN Nil

STATIC FUNCTION DrawArr( arr, y1, x1, y2, x2, nFirst, nSel, cColor, cColorSel )

   LOCAL clr := SetColor( cColor ), i, i1 := 1

   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   FOR i := y1+1 TO y2-1
      SetColor( Iif( i1==nSel, cColorSel, cColor ) )
      @ i, x1+1 SAY arr[nFirst-1+i1++]
   NEXT

   SetColor( clr )

   RETURN Nil
