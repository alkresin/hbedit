#include "inkey.ch"
#include "setcurs.ch"

FUNCTION FMenu( obj, aMenu, y1, x1, y2, x2, clrMenu, clrMenuSel, nCurr )

   //LOCAL cScBuf := Savescreen( 0, 0, 24, 79 )
   LOCAL nRow := Row(), nCol := Col(), nr, nc, oldc, xRes := 0
   LOCAL i, nKey, lDo := .T., lSingle := !(Valtype(aMenu[1]) == "A")
   LOCAL nLen, nDop, arr
   LOCAL nFirst := 1, nHeight

   IF y1 == Nil; y1 := 6; ENDIF
   IF x1 == Nil; x1 := 30; ENDIF
   IF clrMenu == Nil; clrMenu := "W+/BG"; ENDIF
   IF clrMenuSel == Nil; clrMenuSel := "GR+/RB"; ENDIF
   oldc := SetColor( clrMenu )

   SetCursor( SC_NONE )
   nLen := Len( aMenu )
   arr := Array( nLen )

   IF x2 == Nil
      x2 := 0
      FOR i := 1 TO nLen
         IF lSingle
            x2 := Max( x2, Len(aMenu[i]) )
         ELSE
            x2 := Max( x2, Len(aMenu[i,1]) + ;
               Iif(Len(aMenu[i])>3.AND.!Empty(aMenu[i,4]), Len(aMenu[i,4])+1,0) )
         ENDIF
      NEXT
      x2 := x1 + x2 + 6
   ENDIF
   IF y2 == Nil
      y2 := Min( MaxRow()-2, nLen + y1 + 1 )
   ENDIF

   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "

   FOR i := 1 TO nLen
      IF lSingle
         arr[i] := PAdr( Iif( i>36, "   ", Iif(i>10, Chr(86+i), Ltrim(Str(i-1)) ) + ": " ) + aMenu[i], x2-x1-3 )
      ELSE
         IF ( nDop := Iif( Len(aMenu[i])>3.AND.!Empty(aMenu[i,4]), Len(aMenu[i,4]), 0 ) ) > 0
            nDop := x2-x1-3 - nDop - Len(aMenu[i,1]) - 3
         ENDIF
         arr[i] := PAdr( Iif( i>36, "   ", Iif(i>10, Chr(86+i), Ltrim(Str(i-1)) ) + ": " ) + ;
            aMenu[i,1] + Iif( nDop>0, Space(nDop)+aMenu[i,4], "" ), x2-x1-3 )
      ENDIF
   NEXT
   MenuRefresh( arr, nFirst, y1, x1, y2 )

   nHeight := y2 -y1 -1
   i := Iif( !Empty(nCurr), nCurr, 1 )
   IF i > nHeight
      nFirst := i
      IF nFirst + nHeight - 1 > nLen
         nFirst -= ( nLen - (nFirst + nHeight - 1) )
      ENDIF
      i := i - nFirst + 1
   ENDIF
   DO WHILE lDo
      @ y1 + i, x1 + 2 SAY arr[i+nFirst-1] COLOR clrMenuSel
      nKey := Inkey( 0, INKEY_ALL )
      @ y1 + i, x1 + 2 SAY arr[i+nFirst-1] COLOR clrMenu
      IF (nKey >= 48 .AND. nKey <= 47 + nLen) ;
            .OR. (nKey >= 97 .AND. nKey <= 86 + nLen) .OR. nKey == K_LBUTTONDOWN .OR. nKey == K_ENTER
         IF nKey == K_LBUTTONDOWN
            nc := MCol()
            nr := MRow()
            IF nr > y1 .AND. nr < y2 .AND. nc > x1 .AND. nc < x2
               i := nr - y1
            ELSE
               EXIT
            ENDIF
         ELSEIF nKey == K_ENTER
         ELSE
            IF nkey <= 57
               i := nKey - 47
            ELSE
               i := nKey - 86
            ENDIF
            IF i > (y2-y1-1)
               IF i - nFirst + 1 > nHeight
                  nFirst := i - nHeight + 1
                  MenuRefresh( arr, nFirst, y1, x1, y2 )
                  i := nHeight
               ELSE
                  i := i - nFirst + 1
               ENDIF
            ENDIF
         ENDIF
         @ y1 + i, x1 + 2 SAY arr[i+nFirst-1] COLOR clrMenuSel
         IF !(Right(arr[i+nFirst-1],1) == ">") .AND. !(nKey == K_ENTER)
            Inkey( 0.3 )
         ENDIF

         IF lSingle .OR. Empty(aMenu[i,2])
            xRes := i + nFirst - 1
         ELSE
            xRes := aMenu[i,2]:exec( obj,aMenu[i+nFirst-1,3] )
         ENDIF
         lDo := .F.

      ELSEIF nKey == K_DOWN
         IF i < nHeight
            i ++
         ELSEIF i + nFirst - 1 < nLen
            nFirst ++
            MenuRefresh( arr, nFirst, y1, x1, y2 )
         ENDIF

      ELSEIF nKey == K_UP
         IF i > 1
            i --
         ELSEIF nFirst > 1
            nFirst --
            MenuRefresh( arr, nFirst, y1, x1, y2 )
         ENDIF

      ELSEIF nKey == K_PGDN
         IF nFirst + nHeight - 1 < nLen
            nFirst := Min( nFirst+(nHeight-1), nLen-(nHeight-1) )
            MenuRefresh( arr, nFirst, y1, x1, y2 )
         ELSE
            i := nLen - nFirst + 1
         ENDIF

      ELSEIF nKey == K_PGUP
         IF nFirst > (nHeight-1)
            nFirst -= (nHeight-1)
            MenuRefresh( arr, nFirst, y1, x1, y2 )
         ELSEIF nFirst > 1
            nFirst := 1
            MenuRefresh( arr, nFirst, y1, x1, y2 )
         ELSE
            i := 1
         ENDIF

      ELSEIF nKey == K_HOME
         i := 1
         IF nFirst > 1
            nFirst := 1
            MenuRefresh( arr, nFirst, y1, x1, y2 )
         ENDIF

      ELSEIF nKey == K_END
         IF nLen > y2-y1-1
            nFirst := nLen - nHeight + 1
            i := nHeight
            MenuRefresh( arr, nFirst, y1, x1, y2 )
         ELSE
            i := nLen
         ENDIF

      ELSEIF nKey == K_ESC .OR. nKey == K_F10
         lDo := .F.
      ENDIF
   ENDDO

   SetColor( oldc )
   //Restscreen( 0, 0, 24, 79, cScBuf )
   IF Valtype( obj ) == "O" .AND. __ObjHasMsg( obj, "LINS" )
      SetCursor( Iif( obj:lIns, SC_NORMAL, SC_SPECIAL1 ) )
   ELSE
      SetCursor( SC_NORMAL )
   ENDIF
   DevPos( nRow, nCol )

   RETURN xRes

STATIC FUNCTION MenuRefresh( arr, nFirst, y1, x1, y2 )

   LOCAL i, nHeight := y2 - y1 - 1

   FOR i := 1 TO nHeight
      DevPos( y1 + i, x1+2 )
      DevOut( arr[i+nFirst-1] )
   NEXT

   RETURN Nil
