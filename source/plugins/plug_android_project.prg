/*
 * Android project management
 * HbEdit plugin
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_F9         -8
#define K_F10        -9
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4

STATIC cIniPath
STATIC oAP
STATIC cScreenBuff
STATIC cPrjName
STATIC aMenuMain := { "Create project", "Open project" }

FUNCTION plug_android_project( oEdit, cPath )

   LOCAL i, cName := "$Android_Project"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Android Project" + Iif( Empty(cPrjName), "", ": "+cPrjName ) )
         DevPos( y, o:x2-9 )
         DevOut( "F9-menu" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }

   IF Empty( cIniPath )
      cIniPath := cPath
      //Read_Tetr_Ini( cIniPath + "tetris.ini" )
   ENDIF

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   oAP := mnu_NewBuf( oEdit )
   oAP:cFileName := cName
   oAP:bWriteTopPane := bWPane
   oAP:bOnKey := {|o,n| _AP_OnKey(o,n) }
   oAP:bStartEdit := {|| _AP_Start() }

   RETURN Nil

FUNCTION _AP_Start()

   LOCAL n

   IF Empty( cScreenBuff )
      Scroll( oAP:y1, oAP:x1, oAP:y2, oAP:x2 )
      n := FMenu( oAP, aMenuMain, oAP:y1 + 2, oAP:x1 + 10 )
   ELSE
      RestScreen( oAP:y1, oAP:x1, oAP:y2, oAP:x2, cScreenBuff )
   ENDIF

   RETURN Nil

FUNCTION _AP_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)
   LOCAL aMenu

   IF nKey == K_F9
   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oAP:y1, oAP:x1, oAP:y2, oAP:x2 )
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_ESC .OR. nKey == K_F10
      cScreenBuff := Nil
      //Write_Tetr_Ini()
      mnu_Exit( oEdit )

   ENDIF

   RETURN -1
