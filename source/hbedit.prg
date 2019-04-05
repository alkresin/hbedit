/*
 * Standalone text editor
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

STATIC xKoef := 1, yKoef := 1
STATIC cFontName
STATIC nFontHeight, nFontWidth
STATIC nScreenH := 25, nScreenW := 80

FUNCTION Main( ... )

   LOCAL aParams := hb_aParams(), i, arr
   LOCAL cFileName

   ReadIni( hb_DirBase() + "hbedit.ini" )

   IF nScreenH != 25 .OR. nScreenW != 80
      IF !SetMode( nScreenH, nScreenW )
         nScreenH := 25
         nScreenW := 80
      ENDIF
   ENDIF
#ifdef GTWVT
   ANNOUNCE HB_GTSYS
   REQUEST HB_GT_WVT
   REQUEST HB_GT_WVT_DEFAULT
   #include "hbgtinfo.ch"

   IF Empty( cFontName )
      hb_gtinfo( HB_GTI_FONTNAME, "Lusida console" )
   ELSE
      hb_gtinfo( HB_GTI_FONTNAME, cFontName )
   ENDIF
   IF Empty( nFontHeight )
      hb_gtinfo( HB_GTI_FONTSIZE, Int( ( ( hb_gtinfo( HB_GTI_DESKTOPHEIGHT ) - 64 ) / nScreenW ) / yKoef ) )
   ELSE
      hb_gtinfo( HB_GTI_FONTSIZE, nFontHeight )
   ENDIF
   IF Empty( nFontWidth )
      hb_gtinfo( HB_GTI_FONTWIDTH, Int( ( hb_gtinfo( HB_GTI_DESKTOPWIDTH ) / nScreenH ) / xKoef ) )
   ELSE
      hb_gtinfo( HB_GTI_FONTWIDTH, nFontWidth )
   ENDIF
   hb_gtinfo( HB_GTI_CLOSABLE, .F. )

   arr := hb_gtinfo( HB_GTI_PALETTE )
   arr[2] := 0x800000
   arr[4] := 0x808000
   hb_gtinfo( HB_GTI_PALETTE, arr )
#endif

   FOR i := 1 TO Len( aParams )
      cFileName := aParams[i]
      TEdit():New( Iif(!Empty(cFileName),Memoread(cFileName),""), cFileName, 0, 0, nScreenH-1, nScreenW-1 )
   NEXT

   IF Empty( TEdit():aWindows )
      TEdit():New( "", "", 0, 0, nScreenH-1, nScreenW-1 )
   ENDIF

   TEdit():nCurr := 1
   DO WHILE !Empty( TEdit():aWindows )
      IF TEdit():nCurr > Len(TEdit():aWindows)
         TEdit():nCurr := 1
      ELSEIF TEdit():nCurr <= 0
         TEdit():nCurr := Len(TEdit():aWindows)
      ENDIF
      TEdit():aWindows[TEdit():nCurr]:Edit()
   ENDDO

   RETURN Nil

STATIC FUNCTION ReadIni( cIniName )

   LOCAL hIni := hb_iniRead( cIniName ), aSect, cTmp
   LOCAL cp

   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      IF hb_hHaskey( hIni, "SCREEN" ) .AND. !Empty( aSect := hIni[ "SCREEN" ] )
         hb_hCaseMatch( aSect, .F. )
         IF hb_hHaskey( aSect, "xkoef" ) .AND. !Empty( cTmp := aSect[ "xkoef" ] )
            xKoef := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, "ykoef" ) .AND. !Empty( cTmp := aSect[ "ykoef" ] )
            yKoef := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, "fontname" ) .AND. !Empty( cTmp := aSect[ "fontname" ] )
            cFontName := cTmp
         ENDIF
         IF hb_hHaskey( aSect, "fontheight" ) .AND. !Empty( cTmp := aSect[ "fontheight" ] )
            nFontheight := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, "fontwidth" ) .AND. !Empty( cTmp := aSect[ "fontwidth" ] )
            nFontWidth := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, "screen_height" ) .AND. !Empty( cTmp := aSect[ "screen_height" ] )
            nScreenH := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, "screen_width" ) .AND. !Empty( cTmp := aSect[ "screen_width" ] )
            nScreenW := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, "cp" ) .AND. !Empty( cTmp := aSect[ "cp" ] )
            cp := cTmp
         ENDIF
      ENDIF
      edi_ReadIni( hIni )
   ENDIF

   IF Empty(cp) .OR. Ascan( TEdit():aCpages, cp ) == 0
      cp := "RU866"
   ENDIF
   hb_cdpSelect( cp )

   RETURN Nil
