
#define  K_ENTER   13
#define  K_ESC     27

STATIC lGthwg := Nil
STATIC hrbHandle

FUNCTION plug_hbc_img_quick( oPane, cPath )

   LOCAL oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )

   IF lGthwg == Nil
      lGthwg := hb_isFunction( "GTHWG_PAINTCB" ) .AND. File( cPath + "hbc_gthwg_q.hrb" )
   ENDIF
   IF lGthwg
      hrbHandle := hb_hrbLoad( cPath + cLispRun )
      hb_hrbDo( hrbHandle )
   ENDIF
   oPaneTo:cQVpref := "IMG"

   RETURN Nil

FUNCTION PLUG_HBC_IMG_QVIEW( oPane, aParams )

   LOCAL oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )

   RETURN Nil

