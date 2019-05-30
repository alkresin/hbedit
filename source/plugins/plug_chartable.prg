#define INKEY_ALL               ( 1+2+4+8+16+32+64+128 )
#define K_ESC       27
#define K_UP         5
#define K_DOWN      24
#define K_LEFT      19
#define K_RIGHT      4
#define K_ENTER     13

FUNCTION plug_CharTable( oEdit )

   LOCAL bufc := SaveScreen( 02, 06, 15, 41 ), nRow := Row(), nCol := Col(), oldc := SetColor( "N/W" )
   LOCAL nKey, i, j, x1 := 8, y1 := 5, x, y
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
   FOR i := 0 TO Iif( oEdit:lUtf8, 3, 7 )
      FOR j := 0 TO 31
         @ y1+i, x1+j SAY Chr( i * 32 + j )
      NEXT
   NEXT
   ShowChar( 0 )
   DevPos( y := y1, x := x1 )
   DO WHILE !lEnd .AND. ( nKey := Inkey( 0, INKEY_ALL ) ) != K_ESC
      IF nKey == K_UP
         IF y > y1
            y --
         ENDIF
      ELSEIF nKey == K_DOWN
         IF y < y1 + 7
            y ++
         ENDIF
      ELSEIF nKey == K_LEFT
         IF x > x1
            x --
         ENDIF
      ELSEIF nKey == K_RIGHT
         IF x < x1 + 31
            x ++
         ENDIF
      ELSEIF nKey == K_ENTER
         lEnd := .T.
      ENDIF
      ShowChar( (y-y1) * 32 + x-x1 )
      DevPos( y, x )
   ENDDO

   RestScreen( 02, 06, 15, 41, bufc )
   DevPos( nRow, nCol )
   SetColor( oldc )

   IF lEnd
      c := Chr( (y-y1) * 32 + x-x1 )
      oEdit:InsText( oEdit:nLine, oEdit:nPos, c )
   ENDIF

   RETURN Nil

STATIC FUNCTION ShowChar( n )

   DevPos( 14, 08 )
   DevOut( "Char: " + Chr( n ) + "  Decimal: " + PAdl( Ltrim(Str(n)), 3, '0' ) + " Hex: " + hb_numToHex( n,2 ) )

   RETURN Nil
