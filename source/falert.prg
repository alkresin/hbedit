/*
 * A replacement for a Alert() function.
 * edi_RunPlugin()
 * These functions are placed to separate file for a possibility to replace them
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"
#ifdef __PSEUDOGT
   #include "hwpgt.ch"
#endif

#define G_FLAGS  8
#define G2_PASS  4
#define SHIFT_PRESSED 0x010000

FUNCTION edi_Alert( cText, cAns1, cAns2, cAns3 )

   LOCAL oy := Row(), ox := Col(), lUtf8
   LOCAL aText := hb_aTokens( cText, ";" ), i, n, nPos
   LOCAL aGets := { {,,2," Ok ",4,TEdit():cColorWR,TEdit():cColorWB,{||__KeyBoard(Chr(K_ENTER))}} }
   LOCAL nLen := 0, nBtnsLen := 6, cp, x1, y1 := 10, oldc, bufsc

   lUtf8 := hb_cdpisutf8()
   FOR i := 1 TO Len( aText )
      n := cp_Len( lUtf8, aText[i] )
      IF n > Maxcol() - 8
         n := Maxcol() - 8
         hb_AIns( aText, i+1, cp_Substr( lUtf8,aText[i],n+1 ), .T. )
         aText[i] := cp_Left( lUtf8,aText[i],n )
      ENDIF
      nLen := Max( nLen, n )
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
   DevPos( oy, ox )

   RETURN i

FUNCTION edi_MsgGet( cTitle, y1, x1, x2, lPass, cInitValue, cp )

   LOCAL xRes := "", cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets, cpold
   LOCAL bViewPass := {|aOpt|
      aGets[2,G_FLAGS] := Iif( Empty(aGets[2,G_FLAGS]), "@P", "" )
      aOpt[2,G2_PASS] := !aOpt[2,G2_PASS]
      ShowGetItem( aGets[2], .T., hb_cdpisutf8(),, aOpt[2] )
      __KeyBoard(Chr(K_UP))
      RETURN Nil
   }

   IF Empty( cInitValue ); cInitValue := ""; ENDIF
   IF Empty( lPass ); lPass := .F.; ENDIF
   y1 := Iif( y1 == Nil, Int( MaxRow()/2 ) - 1, y1 )
   x1 := Iif( x1 == Nil, Int( MaxCol()/2 ) - 15, x1 )
   x2 := Iif( x2 == Nil, x1 + 30, x2 )

   aGets := { {y1,x1+4, 11, cTitle}, { y1+1,x1+2, 0, cInitValue, x2-x1-Iif(lPass,6,3),,,Iif(!lPass,Nil,"@P") } }
   IF lPass .AND. Empty(cInitValue)
      AAdd( aGets, {y1+1,x2-3,2,"[v]",3,,, bViewPass} )
   ENDIF

   cBuf := Savescreen( y1, x1, y1 + 2, x2 )
   @ y1, x1, y1 + 2, x2 BOX "ÚÄ¿³ÙÄÀ³ "

   IF !Empty( cp )
      cpOld := hb_cdpSelect( cp )
   ENDIF
   edi_READ( aGets )
   IF LastKey() == 13
      xRes := aGets[2,4]
   ENDIF
   IF !Empty( cp )
      hb_cdpSelect( cpold )
   ENDIF
   SetColor( oldc )
   Restscreen( y1, x1, y1 + 2, x2, cBuf )

   RETURN xRes

FUNCTION edi_MsgGet_ext( cText, y1, x1, y2, x2, cp, lF10, lReadOnly, lTopPane, lNoWrap )

   LOCAL nCurr := TEdit():nCurr, cpOld, cBuff, cRes := ""
   LOCAL oNew, oldc := SetColor( TEdit():cColorSel ), nRow := Row(), nCol := Col(), oldCurs := SetCursor()
   LOCAL lHwg := ( hb_gtversion() == "HWGUI" )
   LOCAL bOnKey := {|o,n|
      LOCAL nKey := hb_keyStd(n), lShift := ( hb_BitAnd( n, SHIFT_PRESSED ) != 0 )
      IF Empty(lF10) .AND. ( ;
         ( nKey == K_ENTER .AND. lHwg .AND. lShift ) .OR. ( nKey == K_DOWN .AND. !lHwg .AND. lShift ) )
         RETURN 0x4100001A
      ELSEIF Empty(lF10) .AND. nKey == K_ENTER .AND. !lShift
         cRes := Trim( oNew:ToString( Chr(10) ) )
         oNew:lClose := .T.
         RestScreen( y1-1, x1-1, y2+3, x2+1, cBuff )
         RETURN -1
      ELSEIF nKey == K_F10
         IF !Empty(lF10)
            IF Empty( lReadOnly )
               cRes := Trim( oNew:ToString( Chr(10) ) )
            ENDIF
            oNew:lClose := .T.
            RestScreen( y1-1, x1-1, y2+3, x2+1, cBuff )
            DevPos( nRow, nCol )
            SetCursor( oldCurs )
         ENDIF
         RETURN -1
      ELSEIF nKey == K_ESC
         //cRes := ""
         oNew:lClose := .T.
         RestScreen( y1-1, x1-1, y2+3, x2+1, cBuff )
         DevPos( nRow, nCol )
         SetCursor( oldCurs )
         RETURN -1
      ENDIF
      RETURN 0
   }

   cBuff := SaveScreen( y1-1, x1-1, y2+3, x2+1 )
   cpOld := hb_cdpSelect( "RU866" )
   @ y1-1, x1-1, y2+3, x2+1 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y2+1, x1-1 SAY "Ã"
   @ y2+1, x2+1 SAY "´"
   @ y2+1, x1 TO y2+1, x2
   hb_cdpSelect( cpOld )
   IF Empty(lF10)
      @ y2+2, x1+2 SAY "Enter - Save and Exit  " + ;
         Iif( lHwg, "Shift-Enter","Shift-Down" ) + " - New Line  ESC - Quit"
   ELSEIF Empty( lReadOnly )
      @ y2+2, x1+2 SAY "F10 - Save and Exit  ESC - Quit"
   ELSE
      @ y2+2, x1+2 SAY "F10, ESC - Close"
   ENDIF
   SetColor( oldc )

   oNew := TEdit():New( cText, "$QUE", y1, x1, y2, x2,, !Empty(lTopPane) )
   oNew:lBuiltIn := .T.
   oNew:lCtrlTab := .F.
   oNew:lWrap := Empty( lNoWrap )
   oNew:nMode := 0
   oNew:lReadOnly := !Empty( lReadOnly )
   oNew:bOnKey := bOnKey
   IF !Empty( cp )
      oNew:cp := cp
      hb_cdpSelect( cp )
      IF cp == "UTF8"
         oNew:lUtf8 := .T.
      ENDIF
   ENDIF
   oNew:Edit()
   TEdit():nCurr := nCurr

   RETURN cRes

FUNCTION edi_RunPlugin( oEdit, aPlugins, xPlugin, aParams )

   LOCAL i, cPlugin, cFullPath

   IF Valtype( xPlugin ) == "N"
      i := xPlugin
   ELSEIF Valtype( xPlugin ) == "C"
      i := Ascan( aPlugins, {|a|a[1]==xPlugin} )
   ENDIF
   IF i > 0
      IF Empty( aPlugins[i,4] )
         cPlugin := aPlugins[i,1]
         IF !Empty( cFullPath := edi_FindPath( "plugins" + hb_ps() + cPlugin ) )
            aPlugins[i,4] := hb_hrbLoad( cFullPath )
            aPlugins[i,5] := cFullPath
         ENDIF
      ENDIF
      IF !Empty( aPlugins[i,4] )
         RETURN hb_hrbDo( aPlugins[i,4], oEdit, hb_fnameDir( aPlugins[i,5] ), aParams )
      ENDIF
   ENDIF

   RETURN .F.