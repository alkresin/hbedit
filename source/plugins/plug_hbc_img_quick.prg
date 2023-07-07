
#define  K_ENTER   13
#define  K_ESC     27

STATIC lGthwg := Nil
STATIC hrbHandle

FUNCTION plug_hbc_img_quick( oPane, cPath )

   LOCAL oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   LOCAL cHrb := "hbc_gthwg_q.hrb"

   IF lGthwg == Nil
      lGthwg := hb_isFunction( "GTHWG_PAINTCB" ) .AND. File( cPath + cHrb )
      IF lGthwg
         hrbHandle := hb_hrbLoad( cPath + cHrb )
      ENDIF
   ENDIF
   oPaneTo:cQVpref := "IMG"

   RETURN Nil

FUNCTION PLUG_HBC_IMG_QVIEW( oPane, aParams )

   LOCAL oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   LOCAL cFileName := aParams[1], cp

   IF !lGthwg
      QFileView( cFileName,, oPaneTo:x1, oPaneTo:y1, oPaneTo:x2, oPaneTo:y2 )
      cp := hb_cdpSelect( "RU866" )
      @ oPaneTo:y2-4, oPaneTo:x1+5, oPaneTo:y2-2, oPaneTo:x2-5 BOX "ÚÄ¿³ÙÄÀ³ "
      hb_cdpSelect( cp )
      @ oPaneTo:y2-3, oPaneTo:x1+8 SAY "GTHWG is needed!"
   ELSE
      hb_hrbDo( hrbHandle, oPaneTo, cFileName, .T. )
   ENDIF

   RETURN Nil

FUNCTION PLUG_HBC_IMG_QEND( oPane )

   LOCAL oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )

   IF lGthwg
      hb_hrbDo( hrbHandle, oPaneTo, "", .F. )
   ENDIF

   RETURN Nil
