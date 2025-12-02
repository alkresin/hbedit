
#define K_ESC            27
#define K_ENTER          13

#define SC_NONE       0
#define SC_NORMAL     1

#define HB_GTI_CLIPBOARDDATA    15
#define HB_GTI_PALETTE   53

STATIC oEdit, y1t, x1t

FUNCTION plug_Palette( oEd )

   LOCAL bufc, nRow := Row(), nCol := Col(), oldc := SetColor( oEd:cColor )
   LOCAL nKey
   LOCAL lEnd := .F., c

   oEdit := oEd
   y1t := oEdit:y1 + 2
   x1t := oEdit:x1 + 4
   bufc := SaveScreen( y1t, x1t, y1t+9, x1t+58 )

   SetCursor( SC_NONE )
   DrawTable()

   DO WHILE !lEnd
      nKey := Inkey( 0 )
      IF nKey == K_ENTER
         lEnd := .T.
      ELSEIF nKey == K_ESC
         EXIT
      ENDIF
      DevPos( y1t, x1t )
   ENDDO

   RestScreen( y1t, x1t, y1t+9, x1t+58, bufc )
   SetCursor( SC_NORMAL )
   DevPos( nRow, nCol )
   SetColor( oldc )

   oEdit := Nil

   RETURN Nil

STATIC FUNCTION DrawTable()

   LOCAL i, j, aColors := { "N", "B", "G", "BG", "R", "RB", "GR", "W",  "N+", "B+", "G+", "BG+", "R+", "RB+", "GR+", "W+" }

   SetColor( TEdit():cColor )
   //Scroll( y1t, x1t, y1t+9, x1t+58 )
   hb_cdpSelect( "RU866" )
   @ y1t, x1t, y1t+9, x1t+58 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( oEdit:cp )

   @ y1t, x1t + 16 SAY PAdc( TEdit():cCurrPal, 26 )

   FOR i := 1 TO 8
      SetColor( TEdit():cColor )
      @ y1t+1, x1t+(i-1)*7+3 SAY aColors[i]
      SetColor( aColors[1]+"/"+aColors[i] )
      @ y1t+2, x1t+(i-1)*7+1 SAY "      "
      @ y1t+3, x1t+(i-1)*7+1 SAY "      "
      @ y1t+4, x1t+(i-1)*7+1 SAY "      "
   NEXT
   FOR i := 1 TO 16
      SetColor( aColors[i]+"/"+aColors[1] )
      @ y1t+6, x1t+(i-1)*3+5 SAY PAdr( aColors[i],3 )
      SetColor( aColors[i]+"/"+aColors[8] )
      @ y1t+7, x1t+(i-1)*3+5 SAY PAdr( aColors[i], 3 )
   NEXT

   RETURN Nil