/*
 * HbEdit plugin - client for https://telegra.ph
 * Based on Telegraph API ( https://telegra.ph/api )
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC      27

STATIC cIniPath
STATIC cIniName := "plug_telegraph.ini"
STATIC _cAccessToken := "access_token"
STATIC cUserKey
STATIC cUrlFunc
STATIC lIsCurl := .F.
STATIC oEd

FUNCTION Plug_Telegraph( oEdit, cPath )

   LOCAL cBuff
   LOCAL nRow := Row(), nCol := Col()
   LOCAL aMenu := { "Get account info", "Create page", "Get page list" }, iChoic

   IF !lIsCurl
      cedi_RunConsoleApp( "curl --version",, @cBuff )
      IF !Empty( cBuff ) .AND. "libcurl" $ cBuff
         lIsCurl := .T.
      ELSE
         edi_Alert( "Curl must be installed to use this plugin; curl executable should be in PATH" )
         DevPos( nRow, nCol )
         RETURN Nil
      ENDIF
   ENDIF
   IF Empty( cIniPath )
      cIniPath := cPath
      _tlph_ReadIni()
      cUrlFunc := Iif( hb_Version(20) .AND. hb_gtVersion()=="HWGUI", 'hwg_ShellExecute("', 'cedi_ShellExecute("' )
   ENDIF
   oEd := oEdit

   IF Empty( cUserKey )
      _tlph_CreateAccount()
      IF Empty( cUserKey )
         RETURN Nil
      ENDIF
   ENDIF

   cBuff := Nil
   iChoic := FMenu( oEdit, aMenu, oEdit:y1+2, oEdit:x1+4 )
   IF iChoic == 1
      _tlph_GetAccountInfo()
   ELSEIF iChoic == 2
      _tlph_CreatePage()
   ELSEIF iChoic == 3
      _tlph_GetPageList()
   ENDIF

   RETURN Nil

STATIC FUNCTION _tlph_CreateAccount()

   LOCAL y1 := 5, x1 := Int(MaxCol()/2)-22, x2 := x1+44
   LOCAL cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL cShortName, cName, cBuff, arr, xTemp, nPos
   LOCAL aGets := { ;
      {y1+1,x1+2, 11, "NickName:"},    ;
      { y1+1,x1+15, 0, "", x2-x1-16 }, ;
      {y1+2,x1+2, 11, "Author name:"}, ;
      { y1+2,x1+15, 0, "", x2-x1-16 }  ;
      }

   cBuf := Savescreen( y1, x1, y1 + 5, x2 )
   @ y1, x1, y1 + 5, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+3, x1 SAY "Ã"
   @ y1+3, x2 SAY "´"
   @ y1+3, x1+1 TO y1+3, x2-1

   edi_READ( aGets )
   Restscreen( y1, x1, y1 + 5, x2, cBuf )
   IF LastKey() == K_ESC
      RETURN .F.
   ELSE
      cShortName := aGets[2,4]
      cName := aGets[4,4]
   ENDIF

   IF !Empty( cShortName ) .AND. !Empty( cName )
      cedi_RunConsoleApp( "curl https://api.telegra.ph/createAccount?short_name=" + cShortName + ;
         "&author_name=" + cName,, @cBuff )
      IF !Empty( cBuff )
         IF ( nPos := At( '{"ok"', cBuff ) ) > 0
            hb_jsonDecode( Substr( cBuff, nPos ), @arr )
            IF Valtype( arr ) == "H" .AND. arr["ok"] .AND. hb_hHaskey( arr, xTemp := "result" ) .AND. ;
               !Empty( xTemp := arr[xTemp] ) .AND. Valtype(xTemp) == "H" .AND. ;
               hb_hHaskey( xTemp, _cAccessToken ) .AND. !Empty( xTemp := xTemp[_cAccessToken] )
               cUserKey := xTemp
               _tlph_WriteIni()
               RETURN Nil
            ENDIF
         ENDIF
      ENDIF
      hb_Memowrit( cIniPath + "plug_telegraph.out", cBuff )
      edi_Alert( "Problems creating account" )
   ENDIF

   RETURN Nil

STATIC FUNCTION _tlph_GetAccountInfo()

   LOCAL cBuff, nPos, arr, xTemp, cr := Chr(10)

   cedi_RunConsoleApp( "curl https://api.telegra.ph/getAccountInfo?" + _cAccessToken + ;
      "=" + cUserKey + '&fields=\[\"short_name\",\"author_name\",\"author_url\",\"auth_url\",\"page_count\"\]',, @cBuff )
   IF !Empty( cBuff )
      IF ( nPos := At( '{"ok"', cBuff ) ) > 0
         hb_jsonDecode( Substr( cBuff, nPos ), @arr )
         IF Valtype( arr ) == "H" .AND. arr["ok"] .AND. hb_hHaskey( arr, xTemp := "result" ) .AND. ;
            !Empty( xTemp := arr[xTemp] ) .AND. Valtype(xTemp) == "H"
            edi_MsgGet_ext( "Short name: " + Iif( hb_hHaskey(xTemp,"short_name"), xTemp["short_name"], "" ) ;
               + cr + "Author name: " + Iif( hb_hHaskey(xTemp,"author_name"), xTemp["author_name"], "" ) ;
               + cr + "Author url: " + Iif( hb_hHaskey(xTemp,"author_url"), xTemp["author_url"], "" ) ;
               + cr + "Auth url: " + Iif( hb_hHaskey(xTemp,"auth_url"), xTemp["auth_url"], "" ) ;
               + cr + "Page count: " + Iif( hb_hHaskey(xTemp,"page_count"), Str(xTemp["page_count"]), "" ) ;
               , oEd:y1+2, oEd:x1+12, oEd:y1+8, oEd:x2-12, "UTF8", .T. )
            RETURN Nil
         ENDIF
      ENDIF
   ENDIF
   hb_Memowrit( cIniPath + "plug_telegraph_1.out", cBuff )
   edi_Alert( "Something goes wrong" )

   RETURN Nil

STATIC FUNCTION _tlph_CreatePage()

   LOCAL cBuff, nPos, arr, xTemp

   cedi_RunConsoleApp( "curl https://api.telegra.ph/getAccountInfo?" + _cAccessToken + ;
      "=" + cUserKey + '&fields=\[\"auth_url\"\]',, @cBuff )
   IF !Empty( cBuff )
      IF ( nPos := At( '{"ok"', cBuff ) ) > 0
         hb_jsonDecode( Substr( cBuff, nPos ), @arr )
         IF Valtype( arr ) == "H" .AND. arr["ok"] .AND. hb_hHaskey( arr, xTemp := "result" ) .AND. ;
            !Empty( xTemp := arr[xTemp] ) .AND. Valtype(xTemp) == "H"
            RETURN &( cUrlFunc + xTemp["auth_url"] + '")' )
         ENDIF
      ENDIF
   ENDIF
   hb_Memowrit( cIniPath + "plug_telegraph_1.out", cBuff )
   edi_Alert( "Something goes wrong" )

   RETURN Nil

STATIC FUNCTION _tlph_GetPageList()

   LOCAL cBuff, nPos, arr, xTemp, i, j, aMenu, cp := hb_cdpSelect(), oNew

   cedi_RunConsoleApp( "curl https://api.telegra.ph/getPageList?" + _cAccessToken + ;
      "=" + cUserKey + '&fields=\[\"auth_url\"\]',, @cBuff )
   IF !Empty( cBuff )
      hb_Memowrit( cIniPath + "plug_telegraph_1.out", cBuff )
      IF ( nPos := At( '{"ok"', cBuff ) ) > 0
         hb_jsonDecode( Substr( cBuff, nPos ), @arr )
         IF Valtype( arr ) == "H" .AND. arr["ok"] .AND. hb_hHaskey( arr, xTemp := "result" ) .AND. ;
            !Empty( xTemp := arr[xTemp] ) .AND. Valtype(xTemp) == "H"
            xTemp := xTemp["pages"]
            IF !Empty( xTemp )
               aMenu := {}
               FOR i := 1 TO Len( xTemp )
                  AAdd( aMenu, "(" + Str(xTemp[i]["views"],8) + ") " + hb_Translate(xTemp[i]["title"],"UTF8",cp) )
               NEXT
               IF ( i := FMenu( oEd, aMenu, oEd:y1+2, oEd:x1+4 ) ) > 0
                  IF ( j := FMenu( oEd, { "Open in browser", "View in editor" }, oEd:y1+2, oEd:x1+4 ) ) == 1
                     RETURN &( cUrlFunc + "https://telegra.ph/" + xTemp[i]["path"] + '")' )
                  ELSEIF j == 2
                     cBuff := arr := Nil
                     cedi_RunConsoleApp( "curl https://api.telegra.ph/getPage/" + xTemp[i]["path"] + ;
                        "?return_content=true",, @cBuff )
                     IF !Empty( cBuff ) .AND. ( nPos := At( '{"ok"', cBuff ) ) > 0
                        hb_jsonDecode( Substr( cBuff, nPos ), @arr )
                        cBuff := hb_jsonEncode( arr, .T. )
                        oNew := mnu_NewBuf( oEd, "$"+xTemp[i]["path"], cBuff )
                        oNew:cp := "UTF8"
                        hb_cdpSelect( oNew:cp )
                        oNew:lUtf8 := .T.
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               edi_Alert( "No pages yet" )
            ENDIF
            RETURN Nil
         ENDIF
      ENDIF
   ENDIF
   edi_Alert( "Something goes wrong" )

   RETURN Nil

STATIC FUNCTION _tlph_WriteIni()

   LOCAL cr := hb_eol()
   LOCAL s := "[MAIN]" + cr + "key=" + cUserKey + cr

   hb_MemoWrit( cIniPath + cIniName, s )

   RETURN Nil

STATIC FUNCTION _tlph_ReadIni()

   LOCAL cIni := cIniPath + cIniName, hIni, aIni, nSect, aSect, cTemp

   IF !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "key" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cUserKey := AllTrim( cTemp )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil