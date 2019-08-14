#include "inkey.ch"
#include "setcurs.ch"

STATIC lSea, cSea, aSea

FUNCTION FMenu( obj, aMenu, y1, x1, y2, x2, clrMenu, clrMenuSel, nCurr, lSearch, lMulti, bSea )

   LOCAL cScBuf
   LOCAL lUtf8, nRow := Row(), nCol := Col(), nr, nc, oldc, xRes := 0, mRow, mCol
   LOCAL i, j, nKey, nKeyMapped, lDo := .T., lSingle := !(Valtype(aMenu[1]) == "A")
   LOCAL nLen, arr, tmparr
   LOCAL nFirst := 1, nHeight

   IF lMulti == Nil; lMulti := .F.; ENDIF
   IF lSearch == Nil; lSearch := .F.; ENDIF
   lSea := lSearch; cSea := ""

   IF y1 == Nil; y1 := 6; ENDIF
   IF x1 == Nil; x1 := 30; ENDIF
   IF clrMenu == Nil; clrMenu := TEdit():cColorMenu; ENDIF
   IF clrMenuSel == Nil; clrMenuSel := TEdit():cColorMenuSel; ENDIF
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
      x2 := Min( MaxCol()-4, x1 + x2 + 6 )
   ENDIF
   IF y2 == Nil
      y2 := Min( MaxRow()-2, nLen + y1 + 1 )
   ENDIF

   cScBuf := Savescreen( y1, x1, y2, x2 )
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
      nKeyMapped := nKey := Inkey( 0, INKEY_ALL )
      IF nKey == K_MOUSEMOVE .OR. nKey == K_NCMOUSEMOVE
         LOOP
      ENDIF
      @ y1 + i, x1 + 2 SAY arr[i+nFirst-1]
      IF ( lSea .AND. ( ( nKey >= K_SPACE .AND. nKey <= 255 ) .OR. ( lUtf8 .AND. nKey > 3000 ) ) );
            .OR. ( !lSea .AND. ( (nKey >= 48 .AND. nKey <= 57) ;
            .OR. ( (nKeyMapped := edi_MapKey(obj,nKey))>= 97 .AND. nKeyMapped <= 122) ) ) ;
            .OR. nKey == K_LBUTTONDOWN .OR. nKey == K_ENTER .OR. ( lMulti .AND. nKey == K_SPACE )

         IF nKey == K_LBUTTONDOWN
            nc := MCol()
            nr := MRow()
            IF nr > y1 .AND. nr < y2 .AND. nc > x1 .AND. nc < x2
               i := nr - y1
            ELSE
               EXIT
            ENDIF
         ELSEIF nKey == K_ENTER
         ELSEIF lMulti .AND. nKey == K_SPACE
            arr[i+nFirst-1] := Iif(Asc(arr[i+nFirst-1])==32,"x"," ") + cp_Substr( lUtf8, arr[i+nFirst-1], 2 )
            LOOP
         ELSE
            IF lSea
               Scroll( y2, x1+3, y2, x2-4 )
               DevPos( y2, x1+3 )
               DevOut( "Wait..." )
               tmparr := MakeArr( aMenu, x2-x1-3, lUtf8, cSea+cp_Chr(lUtf8,nKey), bSea )
               IF !( tmparr == Nil )
                  cSea += cp_Chr( lUtf8, nKey )
                  IF Valtype( tmparr ) == "A"
                     arr := tmparr
                     nLen := Len( arr )
                     i := nFirst := 1
                     nHeight := Min( y2 -y1 -1, nLen )
                     MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
                  ENDIF
               ENDIF
               LOOP
            ELSE
               j := Ascan( arr, {|s|Asc(s) == nKeyMapped} )
               IF j == 0 .OR. j > nLen
                  LOOP
               ENDIF
               i := j
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
         IF !( Left( arr[i+nFirst-1], 3 ) == "---" )
            IF !(Right(arr[i+nFirst-1],1) == ">") .AND. !(nKey == K_ENTER)
               Inkey( 0.3 )
            ENDIF

            IF lMulti
               xRes := {}
               FOR j := 1 TO Len( arr )
                  IF Asc( arr[j] ) != 32
                     Aadd( xRes, Iif(!Empty(aSea) .AND. nLen < Len(aMenu), Ascan(aSea,j), j) )
                  ENDIF
               NEXT
               IF Empty( xRes )
                  xRes := { Iif(!Empty(aSea) .AND. nLen < Len(aMenu), Ascan(aSea,i), i) + nFirst - 1 }
               ENDIF
            ELSE
               IF !Empty( aSea ) .AND. nLen < Len( aMenu )
                  i := Ascan( aSea, i )
               ENDIF
               IF lSingle .OR. Empty(aMenu[i,2])
                  xRes := i + nFirst - 1
               ELSE
                  DevPos( y1 + i, x2 )
                  xRes := aMenu[i+nFirst-1,2]:exec( obj, Iif( Len(aMenu[i+nFirst-1])>2,aMenu[i+nFirst-1,3],Nil ) )
               ENDIF
            ENDIF
            lDo := .F.
         ENDIF

      ELSEIF nKey == K_BS
         IF lSea .AND. !Empty( cSea )
            cSea := cp_Left( lUtf8, cSea, cp_Len(lUtf8,cSea)-1 )
            IF Valtype( tmparr := MakeArr( aMenu, x2-x1-3, lUtf8, cSea, bSea ) ) == "A"
               arr := tmparr
               nLen := Len( arr )
               i := nFirst := 1
               nHeight := Min( y2 -y1 -1, nLen )
               MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
            ENDIF
            LOOP
         ENDIF

      ELSEIF nKey == K_DOWN .OR. ( nKey == K_MWBACKWARD .AND. ;
         (mRow := MRow()) >= y1 .AND. mRow <= y2 .AND. (mCol := MCol()) >= x1 .AND. mCol <= x2 )
         IF i < nHeight
            i ++
         ELSEIF i + nFirst - 1 < nLen
            nFirst ++
            MenuRefresh( arr, nFirst, y1, x1, y2, x2 )
         ENDIF

      ELSEIF nKey == K_UP .OR. ( nKey == K_MWFORWARD .AND. ;
         (mRow := MRow()) >= y1 .AND. mRow <= y2 .AND. (mCol := MCol()) >= x1 .AND. mCol <= x2 )
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
   Restscreen( y1, x1, y2, x2, cScBuf )
   IF Valtype( obj ) == "O" .AND. __ObjHasMsg( obj, "LINS" )
      SetCursor( Iif( obj:lIns, SC_NORMAL, SC_SPECIAL1 ) )
   ELSE
      SetCursor( SC_NORMAL )
   ENDIF
   DevPos( nRow, nCol )
   aSea := Nil

   RETURN xRes

STATIC FUNCTION MakeArr( aMenu, nSize, lUtf8, cSearch, bSea )

   LOCAL i, nLen := Len(aMenu), arr, lSingle := !(Valtype(aMenu[1]) == "A"), nLenArr := 0
   LOCAL cPrefix, cs, nDop, l, cLine, aSeaTmp

   IF lSea .AND. !Empty( cSearch )
      aSeaTmp := Array( Len( aMenu) )
      IF bSea == Nil
         cs := cSearch
      ELSE
         cs := Eval( bSea, 0, cSearch )
         IF cs == Nil
            RETURN .F.
         ENDIF
      ENDIF
      cs := cp_Lower( lUtf8, cs )
   ENDIF
   arr := Array( nLen )

   FOR i := 1 TO nLen
      l := .F.
      cLine := Iif( lSingle, aMenu[i], aMenu[i,1] )
      IF ( !lSea .OR. Empty(cs) .OR. cp_At( lUtf8, cs, cp_Lower( lUtf8, cLine ) ) > 0 ) .AND. ;
         ( bSea == Nil .OR. Eval( bSea, 1, cSearch, cLine ) )
         nLenArr ++
         IF ( cPrefix := Substr( Iif( lSingle,aMenu[i],aMenu[i,1] ), 2, 2 ) ) == ": " ;
            .OR. cPrefix == "--"
            cPrefix := ""
         ELSE
            cPrefix := Iif( i>36.OR.lSea, "   ", Iif(i>10, Chr(86+i), Ltrim(Str(i-1)) ) + ": " )
         ENDIF
         IF lSingle
            arr[nLenArr] := PAdr( cPrefix + aMenu[i], nSize )
         ELSE
            IF ( nDop := Iif( Len(aMenu[i])>3.AND.!Empty(aMenu[i,4]), Len(aMenu[i,4]), 0 ) ) > 0
               nDop := nSize - nDop - Len(aMenu[i,1]) - 3
            ENDIF
            arr[nLenArr] := PAdr( cPrefix + aMenu[i,1] + Iif( nDop>0, Space(nDop)+aMenu[i,4], "" ), nSize )
         ENDIF
         l := .T.
      ENDIF
      IF !Empty( aSeaTmp )
         aSeaTmp[i] := Iif( l, nLenArr, 0 )
      ENDIF
   NEXT
   IF nLenArr == 0
      RETURN Nil
   ELSEIF nLenArr < nLen
      arr := ASize( arr, nLenArr )
   ENDIF
   aSea := aSeaTmp

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
