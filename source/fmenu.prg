#include "inkey.ch"
#include "setcurs.ch"

STATIC lSea, cSea, aSea

FUNCTION FMenu( obj, aMenu, y1, x1, y2, x2, clrMenu, clrMenuSel, nCurr, lSearch )

   //LOCAL cScBuf := Savescreen( 0, 0, 24, 79 )
   LOCAL lUtf8, nRow := Row(), nCol := Col(), nr, nc, oldc, xRes := 0
   LOCAL i, j, nKey, lDo := .T., lSingle := !(Valtype(aMenu[1]) == "A")
   LOCAL nLen, arr, tmparr
   LOCAL nFirst := 1, nHeight

   IF lSearch == Nil; lSearch := .F.; ENDIF
   lSea := lSearch; cSea := ""

   IF y1 == Nil; y1 := 6; ENDIF
   IF x1 == Nil; x1 := 30; ENDIF
   IF clrMenu == Nil; clrMenu := "W+/BG"; ENDIF
   IF clrMenuSel == Nil; clrMenuSel := "GR+/RB"; ENDIF
   oldc := SetColor( clrMenu )

   IF Valtype( obj ) == "O" .AND. __ObjHasMsg( obj, "LUTF8" )
      lUtf8 := obj:lUtf8
   ELSE
      lUtf8 := .F.
   ENDIF
   SetCursor( SC_NONE )
   nLen := Len( aMenu )

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
   IF lSea
      @ y2, x1+2 SAY "[" + Replicate( " ",x2-x1-6 ) + "]" COLOR clrMenuSel
      SetCursor( SC_NORMAL )
      DevPos( y2, x1+3 )
   ENDIF

   arr := MakeArr( aMenu, x2-x1-3, lUtf8 )
   nLen := Len( arr )

   nHeight := Min( y2 -y1 -1, nLen )
   i := Iif( !Empty(nCurr), nCurr, 1 )
   IF i > nLen
      i := nLen
   ELSEIF i > nHeight
      nFirst := i
      IF nFirst + nHeight - 1 > nLen
         nFirst -= ( (nFirst + nHeight - 1) -nLen )
      ENDIF
      i := i - nFirst + 1
   ENDIF
   MenuRefresh( arr, nFirst, y1, x1, y2, x2 )

   DO WHILE lDo
      SetColor( clrMenuSel )
      @ y1 + i, x1 + 2 SAY arr[i+nFirst-1]
      IF lSea
         Scroll( y2, x1+3, y2, x2-4 )
         DevPos( y2, x1+3 )
         DevOut( cSea )
      ENDIF
      SetColor( clrMenu )
      nKey := Inkey( 0, INKEY_ALL )
      @ y1 + i, x1 + 2 SAY arr[i+nFirst-1]
      IF ( lSea .AND. ( nKey >= K_SPACE .AND. nKey <= 255 ) .OR. ( lUtf8 .AND. nKey > 3000 ) ) ;
            .OR. ( !lSea .AND. ( (nKey >= 48 .AND. nKey <= 47 + nLen) ;
            .OR. (nKey >= 97 .AND. nKey <= 86 + nLen) ) ) .OR. nKey == K_LBUTTONDOWN .OR. nKey == K_ENTER

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
            IF lSea
               IF !Empty( tmparr := MakeArr( aMenu, x2-x1-3, lUtf8, cSea+cp_Chr(lUtf8,nKey) ) )
                  cSea += cp_Chr( lUtf8, nKey )
                  arr := tmparr
                  nLen := Len( arr )
                  i := nFirst := 1
                  nHeight := Min( y2 -y1 -1, nLen )
                  MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
               ENDIF
               LOOP
            ELSE
               IF nkey <= 57
                  i := nKey - 47
               ELSE
                  i := nKey - 86
               ENDIF
               IF i > (y2-y1-1)
                  IF i - nFirst + 1 > nHeight
                     nFirst := i - nHeight + 1
                     MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
                     i := nHeight
                  ELSE
                     i := i - nFirst + 1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         @ y1 + i, x1 + 2 SAY arr[i+nFirst-1] COLOR clrMenuSel
         IF !(Right(arr[i+nFirst-1],1) == ">") .AND. !(nKey == K_ENTER)
            Inkey( 0.3 )
         ENDIF

         IF !Empty( aSea ) .AND. nLen < Len( aMenu )
            i := Ascan( aSea, i )
         ENDIF
         IF lSingle .OR. Empty(aMenu[i,2])
            xRes := i + nFirst - 1
         ELSE
            xRes := aMenu[i,2]:exec( obj,aMenu[i+nFirst-1,3] )
         ENDIF
         lDo := .F.

      ELSEIF nKey == K_BS
         IF lSea .AND. !Empty( cSea )
            cSea := cp_Left( lUtf8, cSea, cp_Len(lUtf8,cSea)-1 )
            IF !Empty( tmparr := MakeArr( aMenu, x2-x1-3, lUtf8, cSea ) )
               arr := tmparr
               nLen := Len( arr )
               i := nFirst := 1
               nHeight := Min( y2 -y1 -1, nLen )
               MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
            ENDIF
            LOOP
         ENDIF

      ELSEIF nKey == K_DOWN
         IF i < nHeight
            i ++
         ELSEIF i + nFirst - 1 < nLen
            nFirst ++
            MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
         ENDIF

      ELSEIF nKey == K_UP
         IF i > 1
            i --
         ELSEIF nFirst > 1
            nFirst --
            MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
         ENDIF

      ELSEIF nKey == K_PGDN
         IF nFirst + nHeight - 1 < nLen
            nFirst := Min( nFirst+(nHeight-1), nLen-(nHeight-1) )
            MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
         ELSE
            i := nLen - nFirst + 1
         ENDIF

      ELSEIF nKey == K_PGUP
         IF nFirst > (nHeight-1)
            nFirst -= (nHeight-1)
            MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
         ELSEIF nFirst > 1
            nFirst := 1
            MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
         ELSE
            i := 1
         ENDIF

      ELSEIF nKey == K_HOME
         i := 1
         IF nFirst > 1
            nFirst := 1
            MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
         ENDIF

      ELSEIF nKey == K_END
         IF nLen > y2-y1-1
            nFirst := nLen - nHeight + 1
            i := nHeight
            MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
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
   aSea := Nil

   RETURN xRes

STATIC FUNCTION MakeArr( aMenu, nSize, lUtf8, cSearch )

   LOCAL i, j, nLen := Len(aMenu), arr, lSingle := !(Valtype(aMenu[1]) == "A"), nLenArr := 0
   LOCAL cs, nDop, l

   IF lSea .AND. !Empty( cSearch )
      IF Empty( aSea )
         aSea := Array( Len( aMenu) )
      ENDIF
      cs := cp_Lower( lUtf8, cSearch )
      FOR i := 1 TO nLen
         IF lSingle .AND. cp_At( lUtf8, cs, cp_Lower( lUtf8, aMenu[i] ) ) > 0
            nLenArr ++
         ELSEIF !lSingle .AND. cp_At( lUtf8, cs, cp_Lower( lUtf8, aMenu[i,1] ) ) > 0
            nLenArr ++
         ENDIF
      NEXT
      IF nLenArr == 0
         RETURN Nil
      ELSE
         arr := Array( nLenArr )
      ENDIF
   ELSE
      arr := Array( nLen )
   ENDIF
   j := 1
   FOR i := 1 TO nLen
      l := .F.
      IF lSingle .AND. ( !lSea .OR. Empty(cs) .OR. cp_At( lUtf8, cs, cp_Lower( lUtf8, aMenu[i] ) ) > 0 )
         arr[j] := PAdr( Iif( i>36.OR.lSea, "   ", ;
            Iif(i>10, Chr(86+i), Ltrim(Str(i-1)) ) + ": " ) + aMenu[i], nSize )
         j ++
         l := .T.
      ELSEIF !lSingle .AND. ( !lSea .OR. Empty(cs) .OR. cp_At( lUtf8, cs, cp_Lower( lUtf8, aMenu[i,1] ) ) > 0 )
         IF ( nDop := Iif( Len(aMenu[i])>3.AND.!Empty(aMenu[i,4]), Len(aMenu[i,4]), 0 ) ) > 0
            nDop := nSize - nDop - Len(aMenu[i,1]) - 3
         ENDIF
         arr[j] := PAdr( Iif( i>36.OR.lSea, "   ", Iif(i>10, Chr(86+i), Ltrim(Str(i-1)) ) + ": " ) + ;
            aMenu[i,1] + Iif( nDop>0, Space(nDop)+aMenu[i,4], "" ), nSize )
         j ++
         l := .T.
      ENDIF
      IF !Empty( aSea )
         aSea[i] := Iif( l, j-1, 0 )
      ENDIF
   NEXT

   RETURN arr

STATIC FUNCTION MenuRefresh( arr, nFirst, y1, x1, y2, x2 )

   LOCAL i, n := y2 -y1 - 1

   FOR i := 1 TO n
      IF i+nFirst-1 > Len( arr )
         Scroll( y1+i, x1+1, y2-1, x2-1 )
         EXIT
      ENDIF
      DevPos( y1 + i, x1+2 )
      DevOut( arr[i+nFirst-1] )
   NEXT

   RETURN Nil
