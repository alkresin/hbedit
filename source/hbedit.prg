/*
 * Standalone text editor
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

STATIC xKoef := 1, yKoef := 1
STATIC cFontName
STATIC nFontHeight, nFontWidth
STATIC nScreenH, nScreenW
STATIC cStartPlugin, lMaximize := .F.
STATIC aFiles := {}

FUNCTION Main( ... )

   LOCAL aParams := hb_aParams(), i, c, arr
   LOCAL cCurrPath := edi_CurrPath(), cIniName
   LOCAL ypos, xpos, nStartLine

   FOR i := 1 TO Len( aParams )
      IF Left( aParams[i],1 ) $ "-/"
         IF ( c := Substr( aParams[i],2,1 ) ) == "f"
            IF Len( aParams[i] ) == 2
               i ++
               cIniName := aParams[i]
            ELSE
               cIniName := Substr( aParams[i],3 )
            ENDIF
            IF Empty( hb_fnameDir( cIniName ) )
               cIniName := cCurrPath + cIniName
            ENDIF

         ELSEIF c == "g"
            nStartLine := Val( Substr( aParams[i],3 ) )

         ELSEIF c == "s" .AND. Substr( aParams[i],3,4 ) == "ize="
            arr := hb_ATokens( Substr( aParams[i],7 ), "," )
            nScreenW := Val( arr[1] )
            nScreenH := Iif( Len(arr)>1, Val( arr[2] ), 25 )

         ELSEIF c == "x" .AND. Substr( aParams[i],3,2 ) == "y="
            arr := hb_ATokens( Substr( aParams[i],5 ), "," )
            xPos := Val( arr[1] )
            yPos := Iif( Len(arr)>1, Val( arr[2] ), 0 )

         ENDIF
      ELSE
         Aadd( aFiles, aParams[i] )
      ENDIF
   NEXT

   IF Empty( cIniName ) .OR. !File( cIniName )
      cIniName := cCurrPath + "hbedit.ini"
      IF !File( cIniName )
         cIniName := hb_DirBase() + "hbedit.ini"
      ENDIF
   ENDIF
   ReadIni( cIniName )

   IF Empty( nScreenH )
      nScreenH := 25
   ENDIF
   IF Empty( nScreenW )
      nScreenW := 80
   ENDIF
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
   IF yPos != Nil
      hb_gtinfo( HB_GTI_SETPOS_XY, xPos, yPos )
   ENDIF
   hb_gtinfo( HB_GTI_CLOSABLE, .F. )

   arr := hb_gtinfo( HB_GTI_PALETTE )
   arr[2] := 0x800000
   arr[4] := 0x808000
   arr[8] := 0xC8C8C8
   hb_gtinfo( HB_GTI_PALETTE, arr )
   IF lMaximize
      hb_gtinfo( HB_GTI_MAXIMIZED, .T. )
   ENDIF
#endif

   IF !Empty( cStartPlugin )
      IF File( cCurrPath + "plugins" + hb_ps() + cStartPlugin )
         hb_hrbRun( cCurrPath + "plugins" + hb_ps() + cStartPlugin )
      ELSEIF File( hb_DirBase() + "plugins" + hb_ps() + cStartPlugin )
         hb_hrbRun( hb_DirBase() + "plugins" + hb_ps() + cStartPlugin )
      ENDIF
   ENDIF

   FOR i := 1 TO Len( aFiles )
      TEdit():New( Iif(!Empty(aFiles[i]),Memoread(aFiles[i]),""), aFiles[i], 0, 0, nScreenH-1, nScreenW-1 )
   NEXT

   IF Empty( TEdit():aWindows )
      TEdit():New( "", "", 0, 0, nScreenH-1, nScreenW-1 )
   ENDIF

   IF nStartLine != Nil
      IF nStartLine < 0; nStartLine := Len(TEdit():aWindows[1]:aText) + nStartLine; ENDIF
      TEdit():aWindows[1]:Goto( nStartLine,,, .T. )
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
   TEdit():onExit()

   RETURN Nil

STATIC FUNCTION ReadIni( cIniName )

   LOCAL hIni := edi_iniRead( cIniName ), aSect, cTmp, arr, i
   LOCAL cp

   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      IF hb_hHaskey( hIni, cTmp := "SCREEN" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         IF hb_hHaskey( aSect, cTmp := "xkoef" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            xKoef := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "ykoef" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            yKoef := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "fontname" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cFontName := cTmp
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "fontheight" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            nFontheight := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "fontwidth" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            nFontWidth := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "screen_height" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            IF Empty( nScreenH )
               nScreenH := Val(cTmp)
            ENDIF
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "screen_width" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            IF Empty( nScreenW )
               nScreenW := Val(cTmp)
            ENDIF
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "cp" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cp := cTmp
         ENDIF
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "START" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         IF hb_hHaskey( aSect, cTmp := "plugin" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cStartPlugin := cTmp
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "maximize" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            lMaximize := ( Lower(cTmp) == "on" )
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "files" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            arr := hb_ATokens( cTmp, "," )
            FOR i := 1 TO Len( arr )
               IF !Empty( arr[i] )
                  AAdd( aFiles, arr[i] )
               ENDIF
            NEXT
         ENDIF
       ENDIF
   ENDIF

   IF Empty(cp) .OR. !hb_cdpExists( cp )
      cp := "RU866"
   ENDIF
   hb_cdpSelect( cp )

   IF !Empty( hIni )
      edi_ReadIni( hIni )
   ENDIF

   RETURN Nil
