/*
 * A replacement for a Alert() function.
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"

FUNCTION edi_Alert( cText, cAns1, cAns2, cAns3 )

   LOCAL aText := hb_aTokens( cText, ";" ), i
   LOCAL aGets := { {,,2," Ok ",4,TEdit():cColorWR,TEdit():cColorWB,{||__KeyBoard(Chr(K_ENTER))}} }
   LOCAL nLen := 0, nBtnsLen := 6, cp, x1, y1 := 10, oldc, bufsc

   FOR i := 1 TO Len( aText )
      nLen := Max( nLen, Len( aText[i] ) )
   NEXT
   nLen += 4
   IF cAns1 != Nil
      nBtnsLen := Max( nBtnsLen, Len(cAns1) + 4 )
      aGets[1,4] := " " + cAns1 + " "
   ENDIF
   nBtnsLen := nBtnsLen + Iif( cAns2==Nil,0,Len(cAns2)+4 ) + ;
      Iif( cAns3==Nil,0,Len(cAns3)+2 )
   nLen := Max( nLen, nBtnsLen+4 )

   aGets[1,1] := y1+Len(aText)+2
   aGets[1,2] := Int( (MaxCol()-nBtnsLen)/2 ) + 1
   aGets[1,5] := Len(aGets[1,4])

   IF cAns2 != Nil
      Aadd( aGets, { aGets[1,1], aGets[1,2] + aGets[1,5] + 2, 2, " " + cAns2 + " ", ;
         Len(" " + cAns2 + " "), TEdit():cColorWR,TEdit():cColorWB, {||__KeyBoard(Chr(K_ENTER))} } )
   ENDIF
   IF cAns3 != Nil
      Aadd( aGets, { aGets[1,1], aGets[2,2] + aGets[2,5] + 2, 2, " " + cAns3 + " ", ;
         Len(" " + cAns3 + " "), TEdit():cColorWR,TEdit():cColorWB, {||__KeyBoard(Chr(K_ENTER))} } )
   ENDIF

   x1 := Int( (MaxCol()-nLen)/2 )

   bufsc := Savescreen( y1, x1, y1+Len(aText)+3, x1+nLen )

   oldc := SetColor( TEdit():cColorWR )
   cp := hb_cdpSelect( "RU866" )
   @ y1, x1, y1+Len(aText)+3, x1+nLen BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )
   FOR i := 1 TO Len( aText )
      @ y1+i, x1 + 2 SAY aText[i]
   NEXT
   SetColor( oldc )

   i := edi_Read( aGets )
   Restscreen( y1, x1, y1+Len(aText)+3, x1+nLen, bufsc )

   RETURN i
