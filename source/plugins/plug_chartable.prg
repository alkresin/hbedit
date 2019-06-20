#define CTRL_PRESSED  0x020000
#define INKEY_ALLEXT   ( 1+2+4+8+16+32+64+128+2048 )

#define K_ESC            27
#define K_UP              5
#define K_DOWN           24
#define K_LEFT           19
#define K_RIGHT           4
#define K_PGUP           18
#define K_PGDN            3
#define K_HOME            1
#define K_END             6
#define K_CTRL_C          3
#define K_CTRL_INS      402
#define K_LBUTTONDOWN  1002
#define K_ENTER          13

#define HB_GTI_CLIPBOARDDATA    15

FUNCTION plug_CharTable( oEdit )

   LOCAL bufc := SaveScreen( 02, 06, 15, 41 ), nRow := Row(), nCol := Col(), oldc := SetColor( oEdit:cColorSel )
   LOCAL nKeyExt, nKey, x1 := 8, y1 := 5, x, y, yf := 0, mCol, mRow
   LOCAL lEnd := .F., c

   hb_cdpSelect( "RU866" )
   @ 02, 06, 15, 41 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 04, 06 SAY "Ã"
   @ 04, 41 SAY "´"
   @ 04, 07 TO 04, 40
   @ 13, 06 SAY "Ã"
   @ 13, 41 SAY "´"
   @ 13, 07 TO 13, 40
   hb_cdpSelect( oEdit:cp )

   @ 03, 08 SAY oEdit:cp
   DrawTable( oEdit:lUtf8, yf, y1, x1 )
   ShowChar( oEdit:lUtf8, 0 )
   DevPos( y := y1, x := x1 )

   DO WHILE !lEnd
      nKeyExt := Inkey( 0, INKEY_ALLEXT )
      nKey := hb_keyStd( nKeyExt )
      IF nKey == K_UP
         IF y > y1
            y --
         ELSEIF yf > 0
            yf --
            DrawTable( oEdit:lUtf8, yf, y1, x1 )
         ENDIF
      ELSEIF nKey == K_DOWN
         IF y < y1 + 7
            y ++
         ELSEIF oEdit:lUtf8 .AND. yf < 2040
            yf ++
            DrawTable( oEdit:lUtf8, yf, y1, x1 )
         ENDIF
      ELSEIF nKey == K_LEFT
         IF x > x1
            x --
         ENDIF
      ELSEIF nKey == K_RIGHT
         IF x < x1 + 31
            x ++
         ENDIF
      ELSEIF nKey == K_PGUP
         IF yf >= 8
            yf -= 8
            DrawTable( oEdit:lUtf8, yf, y1, x1 )
         ENDIF
      ELSEIF nKey == K_PGDN
         IF oEdit:lUtf8 .AND. yf < 2032
            yf += 8
            DrawTable( oEdit:lUtf8, yf, y1, x1 )
         ENDIF
      ELSEIF nKey == K_HOME
         yf := 0
         y := y1
         x := x1
         IF oEdit:lUtf8
            DrawTable( oEdit:lUtf8, yf, y1, x1 )
         ENDIF
      ELSEIF nKey == K_END
         IF oEdit:lUtf8
            yf := 2040
            y := y1 + 7
            x := x1 + 31
            DrawTable( oEdit:lUtf8, yf, y1, x1 )
         ELSE
            y := y1 + 7
            x := x1 + 31
         ENDIF
      ELSEIF nKey == K_LBUTTONDOWN
         mCol := MCol()
         mRow := MRow()
         IF mCol >= x1 .AND. mCol <= x1 + 31 .AND. mRow >= y1 .AND. mRow <= y1 + 7
            y := mRow
            x := mCol
         ENDIF
      ELSEIF (nKey == K_CTRL_INS .OR. nKey == K_CTRL_C) .AND. hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
         hb_gtInfo( HB_GTI_CLIPBOARDDATA, cp_Chr( oEdit:lUtf8, (yf+y-y1) * 32 + x-x1 ) )
      ELSEIF nKey == K_ENTER
         lEnd := .T.
      ELSEIF nKey == K_ESC
         EXIT
      ENDIF
      ShowChar( oEdit:lUtf8, (yf+y-y1) * 32 + x-x1 )
      DevPos( y, x )
   ENDDO

   RestScreen( 02, 06, 15, 41, bufc )
   DevPos( nRow, nCol )
   SetColor( oldc )

   IF lEnd
      c := cp_Chr( oEdit:lUtf8, (yf+y-y1) * 32 + x-x1 )
      oEdit:InsText( oEdit:nLine, oEdit:nPos, c )
   ENDIF

   RETURN Nil

STATIC FUNCTION DrawTable( lUtf8, yf, y1, x1 )

   LOCAL i, j

   Scroll( y1, x1, y1+7, x1+31 )
   FOR i := 0 TO 7
      FOR j := 0 TO 31
         @ y1+i, x1+j SAY cp_Chr( lUtf8, (yf+i) * 32 + j )
      NEXT
   NEXT
   RETURN Nil

STATIC FUNCTION ShowChar( lUtf8,n )

   Scroll( 14, 32, 14, 40 )
   DevPos( 14, 08 )
   DevOut( "Char: " + cp_Chr( lUtf8,n ) + "  Decimal: " + ;
      Iif( n>255, Ltrim(Str(n)), PAdl( Ltrim(Str(n)), 3, '0' ) ) + ;
      " Hex: " + hb_numToHex( n, Iif( n>255, 4, 2 ) ) )

   RETURN Nil
