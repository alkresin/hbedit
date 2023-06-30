
#define  K_ENTER   13
#define  K_ESC     27

FUNCTION plug_hbc_fb2( oPane )

   LOCAL oPaneTo

   oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   oPaneTo:cQVpref := "FB2"

   RETURN Nil

FUNCTION PLUG_HBC_FB2_QVIEW( oPane, aParams )

   LOCAL oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   LOCAL cColor := "W/B", cp

   SetColor( cColor )
   cp := hb_cdpSelect( "RU866" )
   @ oPaneTo:y1, oPaneTo:x1, oPaneTo:y2, oPaneTo:x2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )

   RETURN Nil
