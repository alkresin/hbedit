/*
 * HbEdit plugin to show a dbf file on a pane
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4

FUNCTION hbc_dbf( cFileName )

   LOCAL oPane := FilePane():PaneCurr()
   LOCAL cColor := "W/B", cp

   SetColor( cColor )
   cp := hb_cdpSelect( "RU866" )
   @ oPane:y1, oPane:x1, oPane:y2, oPane:x2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )


   RETURN Nil

STATIC FUNCTION _dbf_OnKey( oPane, nKeyExt )

   LOCAL nKey := hb_keyStd( nKeyExt )

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE
      RETURN -1
   ENDIF

   IF nKey == K_LEFT

   ELSEIF nKey == K_RIGHT

   ELSEIF nKey == K_UP

   ELSEIF nKey == K_DOWN
   ENDIF

   RETURN -1

STATIC FUNCTION LineOut()
   RETURN Nil

STATIC FUNCTION FieldOut( numf )
   LOCAL fldtype := dbFieldInfo( 2, numf ), xRez, vartmp, nItem := numf

   xRez := Fieldget( numf )

   DO CASE
   CASE fldtype == "C"
      RETURN cRez

   CASE fldtype $ "NIBYZ842+^"
      RETURN Str( cRez, dbFieldInfo(3, numf), dbFieldInfo(4, numf) )

   CASE fldtype = "D"
      RETURN Dtoc( xRez )

   CASE fldtype = "L"
      RETURN Iif( xRez, "T", "F" )

   CASE fldtype = "M"
      RETURN "  <Memo>  "

   CASE fldtype $ "T=@"
      IF dbFieldInfo( 3, numf ) == 4
         RETURN Transform( xRez, "@T" )
      ELSE
         RETURN hb_ttoc( xRez )
      ENDIF

   ENDCASE

RETURN ''