/*
 * Android project management
 * HbEdit plugin
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_ENTER      13
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_F9         -8
#define K_F10        -9
#define K_F12       -41
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4

STATIC cIniPath
STATIC oAP
STATIC cProjectsDir
STATIC lIniUpd := .F.
STATIC cScreenBuff
STATIC cPrjName
STATIC cFullProjectName := "", cProjectName

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
      Read_AP_Ini( cIniPath + "ap.ini" )
      IF Empty( cProjectsDir )
         cProjectsDir := AP_DirPrefix() + "Android" + hb_ps() + "Projects" + hb_ps()
         lIniUpd := .T.
      ENDIF
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
      IF AP_Menu()
         DevPos( oAP:y1, oAP:x1+2 )
         DevOut( "Project: " + cFullProjectName )

      ELSE
         //KEYBOARD Chr(K_ESC)
      ENDIF
   ELSE
      RestScreen( oAP:y1, oAP:x1, oAP:y2, oAP:x2, cScreenBuff )
   ENDIF

   RETURN Nil

FUNCTION _AP_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)
   LOCAL n

   IF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oAP:y1, oAP:x1, oAP:y2, oAP:x2 )
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_F12

      RETURN 0

   ELSEIF nKey == K_ESC .OR. nKey == K_F10
      cScreenBuff := Nil
      IF lIniUpd
         Write_AP_Ini()
      ENDIF
      mnu_Exit( oEdit )

   ENDIF

   RETURN -1

STATIC FUNCTION AP_Menu()

   LOCAL n, lRes := .F.
   STATIC aMenuMain := { "Create project with HDroidGUI", "Create project without HDroidGUI", "Open project" }

   IF ( n := FMenu( oAP, aMenuMain, oAP:y1 + 4, oAP:x1 + Int( (oAP:x2-oAP:x1-36)/2 ) ) ) == 1
      lRes := AP_Create( 1 )

   ELSEIF n == 2
      lRes := AP_Create( 2 )

   ELSEIF n == 3
      lRes := AP_Open()

   ENDIF

   RETURN lRes

STATIC FUNCTION AP_Create( nType )

   LOCAL oldc := SetColor( oAP:cColorSel+","+oAP:cColorSel+",,"+oAP:cColorGet+","+oAP:cColorSel )
   LOCAL cBufScr := SaveScreen( oAP:y1, oAP:x1, oAP:y2, oAP:x2 )
   LOCAL aGets := { {06,22,0,cProjectsDir,48,oAP:cColorMenu,oAP:cColorMenu}, ;
      {08,22,0,"",48,oAP:cColorMenu,oAP:cColorMenu}, ;
      {16,25,2,"[Create]",10,oAP:cColorSel,oAP:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {16,58,2,"[Cancel]",10,oAP:cColorSel,oAP:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cSouDir, aSouDir, cNewProjectDir
   LOCAL cBuff, aBuff, cEol, i, j, cLine, nMode, sText, cSouName, cTemp, nPos
   LOCAL lRes := .F.

   hb_cdpSelect( "RU866" )
   @ 04, 20, 17, 72 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 15, 20 SAY "Ã"
   @ 15, 72 SAY "´"
   @ 15, 21 TO 15, 71
   hb_cdpSelect( oAP:cp )

   @ 05,22 SAY "Projects directory"
   @ 07,22 SAY "Full project name (i.e. su.harbour.MyProject)"

   DO WHILE edi_READ( aGets ) > 0

      IF Empty( aGets[1,4] ) .OR. Empty( aGets[2,4] )
         edi_Alert( "Field haven't filled" )
         LOOP
      ENDIF

      IF !( cProjectsDir == Trim( aGets[1,4] ) )
         cProjectsDir := Trim( aGets[1,4] )
         lIniUpd := .T.
      ENDIF
      IF !( Right( cProjectsDir,1 ) $ "/\" )
         cProjectsDir += hb_ps()
         lIniUpd := .T.
      ENDIF

      cFullProjectName := aGets[2,4]
      IF ( nPos := Rat( ".", cFullProjectName ) ) == 0
         edi_Alert( "There must be a full project name,; i.e. su.harbour." + cFullProjectName )
         LOOP
      ENDIF
      aSouDir := hb_ATokens( cFullProjectName, "." )
      cSouDir := StrTran( cFullProjectName, ".", hb_ps() )
      cProjectName := Substr( cFullProjectName, nPos+1 )
      cNewProjectDir := cProjectsDir + cProjectName + hb_ps()

      cTemp := Iif( nType==1,"ap1.pt","ap2.pt" )
      IF Empty( cBuff := MemoRead( cIniPath + cTemp ) )
         edi_Alert( "Can't find pattern file " + cTemp )
         EXIT
      ENDIF
      cEol := Iif( Chr(13) $ cBuff, Chr(13)+Chr(10), Chr(10) )
      aBuff := hb_ATokens( cBuff, cEol )
      nMode := 1
      hb_DirCreate( cNewProjectDir )
      FOR i := 1 TO Len( aBuff )
         cLine := AllTrim( aBuff[i] )
         IF Left( cLine,3 ) == "{{{" .AND. Right( cLine,3 ) == "}}}"
            IF nMode == 2
               // Write file
               nMode := 1
               IF ( nPos := At( "{{1}}", cSouName ) ) > 0
                  cSouName := Left( cSouName, nPos-1 ) + cSouDir + Substr( cSouName, nPos+5 )
               ENDIF
               hb_MemoWrit( cNewProjectDir + cSouName, sText )
            ENDIF
            cLine := Substr( cLine, 4, Len(cLine)-6 )
            IF Right( cLine,1 ) $ "/\"
               // This is a directory name
               IF ( nPos := At( "{{1}}", cLine ) ) > 0
                  cTemp := cNewProjectDir + Left( cLine, nPos-1 ) + hb_ps()
                  FOR j := 1 TO Len( aSouDir )
                     cTemp += aSouDir[j]
                     hb_DirCreate( cTemp )
                     cTemp += hb_ps()
                  NEXT
               ELSE
                  hb_DirCreate( cNewProjectDir + cLine )
               ENDIF
            ELSE
               // This is a file name
               nMode := 2
               cSouName := cLine
               sText := ""
            ENDIF
         ELSEIF nMode == 2
            nPos := 0
            cLine := aBuff[i]
            DO WHILE ( nPos := hb_At( "{{", cLine, nPos ) ) > 0
               IF ( j := Val( Substr( cLine,nPos+2,2 ) ) ) == 1
                  cLine := Left( cLine, nPos-1 ) + cSouDir + Substr( cLine, nPos+5 )
               ELSEIF ( j := Val( Substr( cLine,nPos+2,2 ) ) ) == 2
                  cLine := Left( cLine, nPos-1 ) + cFullProjectName + Substr( cLine, nPos+5 )
               ELSEIF ( j := Val( Substr( cLine,nPos+2,2 ) ) ) == 3
                  cLine := Left( cLine, nPos-1 ) + cProjectName + Substr( cLine, nPos+5 )
               ENDIF
            ENDDO
            sText += cLine + cEol
         ENDIF
      NEXT
      IF nMode == 2
         // Write last file
         IF ( nPos := At( "{{1}}", cSouName ) ) > 0
            cSouName := Left( cSouName, nPos-1 ) + cSouDir + Substr( cSouName, nPos+5 )
         ENDIF
         hb_MemoWrit( cNewProjectDir + cSouName, sText )
      ENDIF
      lRes := .T.
      EXIT
   ENDDO

   SetColor( oldc )
   RestScreen( oAP:y1, oAP:x1, oAP:y2, oAP:x2, cBufScr )
   edi_SetPos( oAP )

   RETURN lRes

STATIC FUNCTION AP_Open()

   LOCAL oldc := SetColor( oAP:cColorSel+","+oAP:cColorSel+",,"+oAP:cColorGet+","+oAP:cColorSel )
   LOCAL cBufScr := SaveScreen( oAP:y1, oAP:x1, oAP:y2, oAP:x2 )
   LOCAL aGets := { {06,22,0,cProjectsDir,48,oAP:cColorMenu,oAP:cColorMenu}, ;
      {09,25,2,"[Ok]",4,oAP:cColorSel,oAP:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {09,58,2,"[Cancel]",10,oAP:cColorSel,oAP:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL arr, i, lRes

   hb_cdpSelect( "RU866" )
   @ 04, 20, 10, 72 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 15, 20 SAY "Ã"
   @ 08, 72 SAY "´"
   @ 08, 21 TO 08, 71
   hb_cdpSelect( oAP:cp )

   @ 05,22 SAY "Projects directory"

   DO WHILE edi_READ( aGets ) > 0

      IF Empty( aGets[1,4] )
         edi_Alert( "Field haven't filled" )
         LOOP
      ENDIF

      IF !( cProjectsDir == Trim( aGets[1,4] ) )
         cProjectsDir := Trim( aGets[1,4] )
         lIniUpd := .T.
      ENDIF
      IF !( Right( cProjectsDir,1 ) $ "/\" )
         cProjectsDir += hb_ps()
         lIniUpd := .T.
      ENDIF

      arr := Directory( cProjectsDir, "D" )
      FOR i := 1 TO Len( arr )
         arr[i] := arr[i,1]
      NEXT
      i := FMenu( oAP, arr, 04, 20, 12, 72 )
      lRes := .T.
      EXIT

   ENDDO

   edi_Alert( "Done!" )
   SetColor( oldc )
   RestScreen( oAP:y1, oAP:x1, oAP:y2, oAP:x2, cBufScr )
   edi_SetPos( oAP )

   RETURN lRes

STATIC FUNCTION AP_DirPrefix()
#ifdef __PLATFORM__UNIX
   LOCAL cPrefix := '/'
#else
   LOCAL cPrefix := hb_curDrive() + ':\'
#endif
   RETURN cPrefix

STATIC FUNCTION Read_AP_Ini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "projects" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cProjectsDir := cTemp
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION Write_AP_Ini()

   LOCAL s := "[MAIN]" + Chr(13)+Chr(10)

   s += "projects=" + cProjectsDir + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "ap.ini", s )

   RETURN Nil
