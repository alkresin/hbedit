/*
 * A replacement for a GET system, which respects the utf8.
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"
#include "setcurs.ch"

#define G_Y      1
#define G_X      2
#define G_TYPE   3
#define G_VALUE  4
#define G_WIDTH  5
#define G_CLR    6
#define G_CLRSEL 7
#define G_CB     8

#define G_TYPE_STRING  0
#define G_TYPE_CHECK   1
#define G_TYPE_BUTTON  2

#define SHIFT_PRESSED 0x010000
#define CTRL_PRESSED  0x020000

STATIC aClrdef

FUNCTION edi_READ( oEdit, aGets )

   LOCAL nCurr := 1, i, nKeyExt, nKey, lRes := .F., nCol, nRow, nx, x, y, s
   LOCAL clrdef := SetColor(), lUtf8 := oEdit:lUtf8
   
   aClrdef := hb_aTokens( clrdef, ',' )
   FOR i := 1 TO Len( aGets )
      ShowGetItem( aGets[i], .F., lUtf8 )
   NEXT

   ShowGetItem( aGets[1], .T., lUtf8 )

   DO WHILE .T.
      nKeyExt := Inkey( 0, HB_INKEY_ALL + HB_INKEY_EXT )
      nKey := hb_keyStd( nKeyExt )
      nx := Col()
      y := Row()
      x := nx - aGets[nCurr,G_X] + 1

      IF ( nKey >= K_SPACE .AND. nKey <= 255 ) .OR. ( lUtf8 .AND. nKey > 3000 )
         IF aGets[nCurr,G_TYPE] == G_TYPE_STRING
            IF x < aGets[nCurr,G_WIDTH] .AND. cp_Len( lUtf8, aGets[nCurr,G_VALUE] ) < aGets[nCurr,G_WIDTH]
               aGets[nCurr,G_VALUE] := cp_Left( lUtf8,aGets[nCurr,G_VALUE],x-1 ) + ;
                     cp_Chr( lUtf8,nKey ) + cp_Substr( lUtf8,aGets[nCurr,G_VALUE],x )
               DevPos( y, aGets[nCurr,G_X] )
               DevOut( aGets[nCurr,G_VALUE] )
               DevPos( y, ++nx )
            ENDIF
         ELSEIF aGets[nCurr,G_TYPE] == G_TYPE_CHECK
            IF nKey == K_SPACE
               aGets[nCurr,G_VALUE] := !aGets[nCurr,G_VALUE]
               DevPos( y, aGets[nCurr,G_X] )
               DevOut( Iif( aGets[nCurr,G_VALUE], "x"," " ) )
               DevPos( y, aGets[nCurr,G_X] )
            ENDIF
         ELSEIF aGets[nCurr,G_TYPE] == G_TYPE_BUTTON
            IF nKey == K_SPACE
               IF Len(aGets[nCurr]) >= G_CB .AND. !Empty(aGets[nCurr,G_CB])
                  Eval( aGets[nCurr,G_CB] )
               ENDIF
            ENDIF
         ENDIF

      ELSEIF nKey == K_DEL
         IF aGets[nCurr,G_TYPE] == G_TYPE_STRING
            IF x <= cp_Len( lUtf8, aGets[nCurr,G_VALUE] )
               aGets[nCurr,G_VALUE] := cp_Left( lUtf8, aGets[nCurr,G_VALUE], x-1 ) + ;
                  cp_Substr( lUtf8, aGets[nCurr,G_VALUE], x+1 )
               ShowGetItem( aGets[nCurr], .T., lUtf8 )
               DevPos( y, nx )
            ENDIF
         ENDIF

      ELSEIF nKey == K_BS
         IF aGets[nCurr,G_TYPE] == G_TYPE_STRING
            IF x > 1
               aGets[nCurr,G_VALUE] := cp_Left( lUtf8, aGets[nCurr,G_VALUE], x-2 ) + ;
                  cp_Substr( lUtf8, aGets[nCurr,G_VALUE], x )
               ShowGetItem( aGets[nCurr], .T., lUtf8 )
               DevPos( y, --nx )
            ENDIF
         ENDIF

      ELSEIF nKey == K_LEFT
         IF aGets[nCurr,G_TYPE] == G_TYPE_STRING .AND. nx > aGets[nCurr,G_X]
            DevPos( Row(), --nx )
         ENDIF

      ELSEIF nKey == K_RIGHT
         IF aGets[nCurr,G_TYPE] == G_TYPE_STRING .AND. x < aGets[nCurr,G_WIDTH] .AND. ;
               x < cp_Len( lUtf8, aGets[nCurr,G_VALUE] )
            DevPos( Row(), ++nx )
         ENDIF

      ELSEIF nKey == K_UP
         IF nCurr > 1
            ShowGetItem( aGets[nCurr], .F., lUtf8 )
            nCurr --
            ShowGetItem( aGets[nCurr], .T., lUtf8 )
         ENDIF

      ELSEIF nKey == K_DOWN
         IF nCurr < Len( aGets )
            ShowGetItem( aGets[nCurr], .F., lUtf8 )
            nCurr ++
            ShowGetItem( aGets[nCurr], .T., lUtf8 )
         ENDIF

      ELSEIF nKey == K_HOME
         IF aGets[nCurr,G_TYPE] == G_TYPE_STRING
            DevPos( y, nx := aGets[nCurr,G_X] )
         ENDIF

      ELSEIF nKey == K_END
         IF aGets[nCurr,G_TYPE] == G_TYPE_STRING
            DevPos( y, nx := ( aGets[nCurr,G_X] + cp_Len( lUtf8, aGets[nCurr,G_VALUE] ) ) )
         ENDIF

      ELSEIF (hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0 .AND. nKey == 22) .OR. ;
         ( hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0 .AND. nKey == K_INS )
         IF aGets[nCurr,G_TYPE] == G_TYPE_STRING
            s := cb2Text( oEdit )
            aGets[nCurr,G_VALUE] := cp_Left( lUtf8,aGets[nCurr,G_VALUE],x-1 ) + ;
                  s + cp_Substr( lUtf8,aGets[nCurr,G_VALUE],x )
            DevPos( y, aGets[nCurr,G_X] )
            DevOut( aGets[nCurr,G_VALUE] )
            nx += cp_Len( lUtf8, s )
            IF nx > aGets[nCurr,G_X] + aGets[nCurr,G_WIDTH] - 1
               nx := aGets[nCurr,G_X] + aGets[nCurr,G_WIDTH] - 1
            ENDIF
            DevPos( y, nx )
         ENDIF

      ELSEIF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0 .AND. nKey == K_CTRL_DOWN ;
            .AND. hb_keyVal( nKeyExt ) == 14
         IF nCurr < Len( aGets ) .AND. aGets[nCurr+1,G_TYPE] == G_TYPE_BUTTON
            __KeyBoard( Chr(K_DOWN) + Chr(K_SPACE) )
         ENDIF
      
      ELSEIF nKey == K_LBUTTONDOWN
         nCol := MCol()
         nRow := MRow()
         FOR i := 1 TO Len(aGets)
            IF aGets[i,G_Y] == nRow .AND. aGets[i,G_X] <= nCol .AND. aGets[i,G_X]+aGets[i,G_WIDTH] > nCol
               ShowGetItem( aGets[nCurr], .F., lUtf8 )
               nCurr := i
               ShowGetItem( aGets[nCurr], .T., lUtf8 )
               IF aGets[nCurr,G_TYPE] == G_TYPE_CHECK
                  aGets[nCurr,G_VALUE] := !aGets[nCurr,G_VALUE]
                  DevPos( nRow, aGets[nCurr,G_X] )
                  DevOut( Iif( aGets[nCurr,G_VALUE], "x"," " ) )
                  DevPos( nRow, aGets[nCurr,G_X] )
               ELSEIF aGets[nCurr,G_TYPE] == G_TYPE_BUTTON
                  IF Len(aGets[nCurr]) >= G_CB .AND. !Empty(aGets[nCurr,G_CB])
                     Eval( aGets[nCurr,G_CB] )
                  ENDIF
               ENDIF
            ENDIF
         NEXT

      ELSEIF nKey == K_ENTER .OR. nKey == K_PGDN
         lRes := .T.
         EXIT

      ELSEIF nKey == K_ESC
         EXIT

      ENDIF
   ENDDO

   SetColor( clrdef )
   SetCursor( SC_NORMAL )

   RETURN lRes

FUNCTION ShowGetItem( aGet, lSele, lUtf8 )

   LOCAL x
   IF lSele
      SetColor( Iif( Len(aGet) < G_CLRSEL .OR.Empty(aGet[G_CLRSEL]), aClrdef[2], aGet[G_CLRSEL] ) )
   ELSE
      SetColor( Iif( Len(aGet) < G_CLR .OR.Empty(aGet[G_CLR]), aClrdef[5], aGet[G_CLR] ) )
   ENDIF

   Scroll( aGet[G_Y], aGet[G_X], aGet[G_Y], aGet[G_X] + aGet[G_WIDTH] - 1 )

   IF aGet[G_TYPE] == G_TYPE_STRING
      @ aGet[G_Y], aGet[G_X] SAY Iif( aGet[G_WIDTH] >= cp_Len( lUtf8,aGet[G_VALUE] ), ;
         aGet[G_VALUE], cp_Left( lUtf8, aGet[G_VALUE], aGet[G_WIDTH] ) )

   ELSEIF aGet[G_TYPE] == G_TYPE_CHECK
      @ aGet[G_Y], aGet[G_X] SAY Iif(aGet[G_VALUE],"x"," ")

   ELSEIF aGet[G_TYPE] == G_TYPE_BUTTON
      x := aGet[G_X] + Int( (aGet[G_WIDTH] - cp_Len( lUtf8,aGet[G_VALUE] ))/2 )
      @ aGet[G_Y], x SAY aGet[G_VALUE]

   ENDIF

   IF lSele
      DevPos( aGet[G_Y], aGet[G_X] )
      SetCursor( Iif( aGet[G_TYPE] == G_TYPE_BUTTON, SC_NONE, SC_NORMAL ) )
   ENDIF

   RETURN Nil

FUNCTION edi_Alert( oEdit, cText )

   LOCAL nLen := Len( cText ) + 4, cp, x1 := Int((MaxCol()-nLen)/2), oldc
   LOCAL aGets := { {12,Int((MaxCol()-10)/2),2,"[Ok]",10,"W+/N","W+/N",{||__KeyBoard(Chr(K_ENTER))}} }

   oldc := SetColor( "W+/R" )   
   cp := hb_cdpSelect( "RU866" )
   @ 09, x1, 13, x1+nLen BOX "ÚÄ¿³ÙÄÀ³ "
   @ 11, x1 SAY "Ã"
   @ 11, x1+nLen SAY "´"
   @ 11, x1+1 TO 11, x1+nLen-1
   @ 10, x1 + 2 SAY cText
   hb_cdpSelect( cp )
   SetColor( oldc )
   edi_Read( oEdit, aGets )
   
   RETURN Nil

