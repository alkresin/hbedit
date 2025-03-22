/*
 * HbEdit plugin - client for https://telegra.ph
 * Based on Telegraph API ( https://telegra.ph/api )
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC      27
#define K_F2       -1
#xtranslate _I( <x,...> ) => TrnMsg( hb_i18n_gettext( <x> ) )

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
   LOCAL aMenu := { "Get account info", "Create page in browser", "Create page in editor", "Get page list" }, iChoic

   IF !lIsCurl
      IF !( lIsCurl := edi_CheckCurl() )
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
      _tlph_CreatePageInBrowser()
   ELSEIF iChoic == 3
      _tlph_CreatePageInEditor()
   ELSEIF iChoic == 4
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
      cBuff := cRun( "curl https://api.telegra.ph/createAccount?short_name=" + cShortName + ;
         "&author_name=" + cName )
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

   cBuff := cRun( "curl https://api.telegra.ph/getAccountInfo?" + _cAccessToken + ;
      "=" + cUserKey + '&fields=\[\"short_name\",\"author_name\",\"author_url\",\"auth_url\",\"page_count\"\]' )
   IF !Empty( cBuff )
      IF ( nPos := At( '{"ok"', cBuff ) ) > 0
         hb_jsonDecode( Substr( cBuff, nPos ), @arr )
         IF Valtype( arr ) == "H" .AND. arr["ok"] .AND. hb_hHaskey( arr, xTemp := "result" ) .AND. ;
            Valtype(xTemp := arr[xTemp]) == "H"
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
   _tlph_Error( cBuff )

   RETURN Nil

STATIC FUNCTION _tlph_CreatePageInBrowser()

   LOCAL cBuff, nPos, arr, xTemp

   cBuff := cRun( "curl https://api.telegra.ph/getAccountInfo?" + _cAccessToken + ;
      "=" + cUserKey + '&fields=\[\"auth_url\"\]' )
   IF !Empty( cBuff )
      IF ( nPos := At( '{"ok"', cBuff ) ) > 0
         hb_jsonDecode( Substr( cBuff, nPos ), @arr )
         IF Valtype( arr ) == "H" .AND. arr["ok"] .AND. hb_hHaskey( arr, xTemp := "result" ) .AND. ;
            !Empty( xTemp := arr[xTemp] ) .AND. Valtype(xTemp) == "H"
            IF hb_hHasKey( xTemp, "auth_url" )
               RETURN &( cUrlFunc + xTemp["auth_url"] + '")' )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   _tlph_Error( cBuff )

   RETURN Nil

STATIC FUNCTION _tlph_CreatePageInEditor()
   LOCAL cBuff, arr, oNew, cr := Chr(10)
   LOCAL bStartEdit := {|o|
      o:hCargo := hb_hash()
      o:hCargo["help"] := "Telegraph plugin hotkeys:" + Chr(10) + ;
         "  F2  - Send page" + Chr(10)
      o:bStartEdit := Nil

      RETURN Nil
   }

  cBuff := '{' + cr + '  "result":' + cr + '  {' + cr + '    "title": "<TITLE>",' + cr + ;
    '    "content":' + cr + '    [' + cr + '      {' + cr + '        "tag": "p",' + cr + ;
        '        "children":' + cr + '        [' + cr + '        "<New line>"' + cr + ;
        '        ]' + cr + '      }' + cr + '    ]' + cr + '  }' + cr + '}'

   oNew := mnu_NewBuf( oEd, "$NewPage", cBuff )
   oNew:cp := "UTF8"
   hb_cdpSelect( oNew:cp )
   oNew:lUtf8 := .T.
   oNew:bStartEdit := bStartEdit
   oNew:bOnKey := {|o,n| _tlph_OnKey(o,n) }

   RETURN Nil

STATIC FUNCTION _tlph_GetPageList()

   LOCAL cBuff, nPos, arr, xTemp, i, j, aMenu, cp := hb_cdpSelect()

   cBuff := cRun( "curl https://api.telegra.ph/getPageList?" + _cAccessToken + ;
      "=" + cUserKey + '&fields=\[\"auth_url\"\]' )
   IF !Empty( cBuff )
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
                     RETURN _tlph_GetPage( xTemp[i]["path"] )
                  ENDIF
               ENDIF
            ELSE
               edi_Alert( "No pages yet" )
            ENDIF
            RETURN Nil
         ENDIF
      ENDIF
   ENDIF
   _tlph_Error( cBuff )

   RETURN Nil

STATIC FUNCTION _tlph_GetPage( cPageName )

   LOCAL cBuff, arr, oNew, nPos
   LOCAL bStartEdit := {|o|
      o:hCargo := hb_hash()
      o:hCargo["help"] := "Telegraph plugin hotkeys:" + Chr(10) + ;
         "  F2  - Send page" + Chr(10)
      o:bStartEdit := Nil

      RETURN Nil
   }

   cBuff := cRun( "curl https://api.telegra.ph/getPage/" + cPageName + ;
      "?return_content=true" )
   IF !Empty( cBuff ) .AND. ( nPos := At( '{"ok"', cBuff ) ) > 0
      hb_jsonDecode( Substr( cBuff, nPos ), @arr )
      IF Valtype( arr ) == "H" .AND. arr["ok"]
         cBuff := hb_jsonEncode( arr, .T. )
         oNew := mnu_NewBuf( oEd, "$"+cPageName, cBuff )
         oNew:cp := "UTF8"
         hb_cdpSelect( oNew:cp )
         oNew:lUtf8 := .T.
         oNew:bStartEdit := bStartEdit
         oNew:bOnKey := {|o,n| _tlph_OnKey(o,n) }
         RETURN Nil
      ENDIF
   ENDIF
   _tlph_Error( cBuff )

   RETURN Nil

STATIC FUNCTION _tlph_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey == K_F2
      _tlph_EditPage( oEdit )
      RETURN -1

   ENDIF

   RETURN 0

STATIC FUNCTION _tlph_EditPage( oEdit )

   LOCAL cBuff, arr, xTemp, hResult, aContent, cr := Chr(10), nPos
   LOCAL cJsonFile := hb_dirTemp() + "hb_tlph.json", lNew := (oEdit:cFilename == "$NewPage")

   hb_jsonDecode( oEdit:ToString(), @arr )
   IF Valtype( arr ) == "H" .AND. hb_hHasKey( arr, xTemp := "result" ) .AND. ;
      Valtype( hResult := arr[xTemp] ) == "H" .AND. hb_hHasKey( hResult,"content" ) .AND. ;
      Valtype( aContent := hResult["content"] ) == "A"

      hb_Memowrit( cJsonFile, '{' + cr + ;
         ' "'+_cAccessToken+'":"'+cUserKey+'",' + cr + ;
         ' "title":"' + hResult["title"] + '",' + cr + ;
         ' "content":' + hb_jsonEncode( aContent ) +',' + cr + ;
         ' "return_content":true' + cr + '}' )

      xTemp := "curl -X POST https://api.telegra.ph/" + Iif(lNew,"createPage/","editPage/") + ;
         Iif( lNew, "", Substr(oEdit:cFileName,2) ) + ;
         ' -H "Content-Type: application/json" -d @"' + cJsonFile + '"'
      //edi_Writelog( xTemp )
      cBuff := cRun( xTemp )
      IF !Empty( cBuff ) .AND. ( nPos := At( '{"ok"', cBuff ) ) > 0
         arr := Nil
         hb_jsonDecode( Substr( cBuff, nPos ), @arr )
         IF Valtype( arr ) == "H" .AND. arr["ok"]
            edi_Alert( "Sent" )
            FErase( cJsonFile )
            RETURN Nil
         ENDIF
      ENDIF
      hb_Memowrit( cIniPath + "plug_telegraph_2.out", cBuff )
      edi_Alert( "Error sending page" )
   ELSE
      edi_Alert( "Wrong json structure" )
   ENDIF

   RETURN Nil

STATIC FUNCTION _tlph_Error( cBuff )

   hb_Memowrit( cIniPath + "plug_telegraph_1.out", cBuff )
   edi_Alert( _I("Something goes wrong...") )

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