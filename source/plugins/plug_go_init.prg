
#define ALT_PRESSED   0x040000
#define K_ALT_I   279
#define K_ALT_R   275
#define K_ALT_F   289
#define K_ALT_L   294

STATIC cIniPath
STATIC nGoInstalled := 0
STATIC oEd

FUNCTION Plug_go_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "Go plugin:  Alt-I Info   Alt-R Run  Alt-F Format  Alt-L �㭪樨" + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab Autocompetion","" ) )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         IF oEdit:hCargo == Nil
            oEdit:hCargo := hb_hash()
         ENDIF
         oEdit:hCargo["help"] := "Go plugin hotkeys:" + Chr(10) + ;
            "  Alt-I - Get info about a function or package under cursor" + Chr(10) + ;
            "  Alt-R - Run code" + Chr(10) + ;
            "  Alt-F - Format code" + Chr(10) + ;
            "  Alt-L - Functions list" + Chr(10) + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab - Autocompetion" + Chr(10),"" )
      ENDIF
      o:bStartEdit := Nil
      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _go_Init_OnKey(o,n)
      IF bOnKeyOrig != Nil .AND. nRes >= 0
         nRes := Eval( bOnKeyOrig, o, Iif( nRes==0, n, nRes ) )
      ENDIF
      RETURN nRes
   }

   oEd := oEdit
   cIniPath := cPath
   oEdit:bAutoC := {|o,s| _go_AutoC(o,s)}

   oEdit:bStartEdit := bStartEdit
   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey

   RETURN Nil

FUNCTION _go_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cPackage, cWord
   LOCAL ny, nx1, nx2, lUtf8 := oEdit:lUtf8

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_I
         ny := oEdit:nLine
         nx2 := oEdit:nPos
         IF ( nx1 := edi_InQuo( oEdit, oEdit:aText[ny], nx2 ) ) > 0
            IF ( nx2 := cp_At( lUtf8, cp_Substr(lUtf8,oEdit:aText[ny],nx1,1), oEdit:aText[ny], nx1+1 ) ) > 0
               nx2 ++
            ELSE
               RETURN -1
            ENDIF
         ELSE
            IF nx2 > 1 .AND. cp_Substr( lUtf8, oEdit:aText[ny], nx2, 1 ) == '.'
               nx2 --
            ENDIF
            IF nx2 == 1 .OR. isAlpha( cp_Substr( lUtf8, oEdit:aText[ny], nx2-1, 1 ) )
               nx1 := edi_PrevWord( oEdit, .F., .F., .F., ny, nx2 )
            ELSE
               nx1 := nx2
            ENDIF
            IF nx1 > 1 .AND. cp_Substr( lUtf8, oEdit:aText[ny], nx1-1, 1 ) == '.'
               nx1 := edi_PrevWord( oEdit, .F., .F., .F., ny, nx1-2 )
            ENDIF
            nx2 := edi_NextWord( oEdit, .F., .F., .F., ny, nx2 )
            IF cp_Substr( lUtf8, oEdit:aText[ny], nx2, 1 ) == '.'
               nx2 := edi_NextWord( oEdit, .F., .F., .F., ny, nx2+1 )
            ENDIF
         ENDIF
         cWord := cp_Substr( lUtf8, oEdit:aText[ny], nx1, nx2-nx1 )
         _go_GetFuncInfo( oEdit, cWord )
         DevPos( nRow, nCol )
         oEdit:TextOut()
         RETURN -1
      ELSEIF nKey == K_ALT_R
         _go_Run()
      ELSEIF nKey == K_ALT_F
         _go_Format()
      ELSEIF nKey == K_ALT_L
         _go_Spis()
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _go_GetFuncInfo( oEdit, cWord )

   LOCAL nx1, cBuff, cFileOut := hb_DirTemp() + "hbedit.out", o, nPos, cAddW := "$FuncInfo"
   LOCAL aImport, cPackage, i

   IF Left( cWord, 1 ) == '"'
      cPackage := Substr( cWord, 2, Len(cWord) - 2 )
      cWord := ""
   ELSEIF ( nx1 := At( '.', cWord ) ) > 0
      cPackage := cp_Left( oEdit:lUtf8, cWord, nx1 - 1 )
      cWord := cp_Substr( oEdit:lUtf8, cWord, nx1 + 1 )
      aImport := _go_KeyWords( oEdit, cPackage, .T. )
      IF ( i := Ascan( aImport, {|a|a[1]==cPackage} ) ) == 0
         edi_Alert( cPackage + " - is not an imported package" )
         RETURN Nil
      ENDIF
      cPackage := aImport[i,2]
   ELSE
      RETURN Nil
   ENDIF

   // edi_Alert( cPackage + " " + cWord )

   FErase( cFileOut )
   cedi_RunConsoleApp( 'go doc ' + cPackage + ' ' + cWord, cFileOut )
   IF Empty( cBuff := MemoRead( cFileOut ) )
      edi_Alert( "Error" )
      RETURN Nil
   ENDIF

   IF ( nPos := Ascan( oEdit:aWindows, {|o|o:cFileName==cAddW} ) ) > 0
      o := oEdit:aWindows[nPos]
      o:SetText( cBuff, cAddW )
      mnu_ToBuf( oEdit, nPos )
   ELSE
      edi_AddWindow( oEdit, cBuff, cAddW, 2, 12, "UTF8" )
   ENDIF

   RETURN Nil

STATIC FUNCTION _go_Run()

   LOCAL cRes, cTempFile, nScreenH, nScreenW, bufsc, cCmd

   IF nGoInstalled == 0
      cedi_RunConsoleApp( 'go version',, @cRes )
      nGoInstalled := Iif( cRes != Nil .AND. "version" $ cRes, 1, -1 )
   ENDIF
   IF nGoInstalled == 1
      cTempFile := hb_DirTemp() + "hb_go_tmp.go"
      hb_MemoWrit( cTempFile, oEd:ToString() )
      cCmd := "go run " + cTempFile
      nScreenH := FilePane():vy2 + 1
      nScreenW := FilePane():vx2 + 1
      bufsc := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )
      hbc_Console( cCmd,, .F. )
      Restscreen( 0, 0, nScreenH-1, nScreenW-1, bufsc )
   ELSE
      edi_Alert( "Go is not installed!" )
   ENDIF

   RETURN Nil

STATIC FUNCTION _go_Format()

   LOCAL cRes

   IF nGoInstalled == 0
      cedi_RunConsoleApp( 'go version',, @cRes )
      nGoInstalled := Iif( cRes != Nil .AND. "version" $ cRes, 1, -1 )
   ENDIF
   IF nGoInstalled == 1
      oEd:Save()
      cedi_RunConsoleApp( "gofmt -w " + oEd:cFileName,, @cRes )
      oEd:SetText( MemoRead( oEd:cFileName ), oEd:cFileName )
      edi_Alert( "Done!" )
   ELSE
      edi_Alert( "Go is not installed!" )
   ENDIF
   RETURN Nil

STATIC FUNCTION _go_Spis()

   LOCAL i, n, arr := oEd:aText, cLine, cfirst, nSkip, arrfnc := {}

   FOR i := 1 TO Len( arr )
      cLine := Lower( Ltrim( arr[i] ) )
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF cfirst == "func"
         Aadd( arrfnc, { cp_Left( oEd:lUtf8,arr[i],64 ), Nil, i } )
      ENDIF
   NEXT
   IF !Empty( arrfnc )
      oEd:TextOut()
      n := oEd:nLine
      FOR i := 1 TO Len( arrfnc )
         IF arrfnc[i,3] > n
            n := i - 1
            EXIT
         ENDIF
      NEXT
      n := Iif( n > Len(arrfnc), Len(arrfnc), Iif( n == 0, 1, n ) )
      IF ( i := FMenu( oEd, arrfnc, 2, 6,,,,, n, (Len(arrfnc)>3) ) ) > 0
         oEd:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _go_AutoC( oEdit, cPrefix )

   LOCAL hTrieLang, hTrie, o := oEdit:oHili
   LOCAL arr, i, nLen, nPrefLen := Len( cPrefix )

   IF Empty( hb_hGetDef( o:hHili, "htrie", Nil ) )
      arr := hb_ATokens( Iif(Empty(o:cKeywords1),"",o:cKeywords1) + " " + ;
         Iif(Empty(o:cKeywords2),"",o:cKeywords2) + " " + Iif(Empty(o:cKeywords3),"",o:cKeywords3) + ;
         " " + Iif(Empty(o:cKeywords4),"",o:cKeywords4), " " )
      hTrieLang := o:hHili["htrie"] := trie_Create( .T. )
      FOR i := 1 TO Len( arr )
         IF Len( arr[i] ) > 3
            trie_Add( hTrieLang, arr[i] )
         ENDIF
      NEXT

   ENDIF

   IF !Empty( arr := _go_KeyWords( oEdit, cPrefix ) )
      FOR i := 1 TO Len( arr )
         IF ( nLen := Len( arr[i] ) ) >= 4 .AND. nLen > nPrefLen
            IF Empty( hTrie )
               hTrie := trie_Create( .T. )
            ENDIF
            trie_Add( hTrie, arr[i] )
         ENDIF
      NEXT
   ENDIF

   RETURN hTrie

STATIC FUNCTION _go_DropComments( cLine, lMultiComm )

   LOCAL nPos := 1, nPos2

   lMultiComm := .F.
   IF ( nPos := hb_At( "//", cLine, nPos ) ) > 0
      cLine := Trim( Left( cLine, nPos-1 ) )
   ENDIF

   nPos := 1
   DO WHILE ( nPos := hb_At( "/*", cLine, nPos ) ) > 0
      lMultiComm := ( ( nPos2 := hb_At( "*/", cLine, nPos+2 ) ) == 0 )
      cLine := Trim( Left( cLine, nPos-1 ) ) + Iif( lMultiComm, "", Substr( cLine, nPos2+2 ) )
   ENDDO

   RETURN cLine

STATIC FUNCTION _go_GetImpNames( cFileName )

   LOCAL arr, cRes := "", cEol := Chr(10), cLine, lMultiComm := .F., nPos, i
   LOCAL nSkip, cfirst

   IF !Empty( arr := MemoRead( cFileName ) )
      IF ( i := At( cEol, arr ) ) > 1 .AND. Substr( arr, i-1, 1 ) == Chr(13)
         cEol := Chr(13) + cEol
      ENDIF
      arr := hb_ATokens( arr, cEol )
      FOR i := 1 TO Len( arr )
         cLine := arr[i]

         IF !Empty( lMultiComm )
            IF ( nPos := At( "*/", cLine ) ) == 0
               LOOP
            ELSE
               lMultiComm := .F.
               cLine := Trim( Substr( cLine, nPos + 2 ) )
            ENDIF
         ENDIF

         cLine := AllTrim( _go_DropComments( cLine, @lMultiComm ) )

         IF Empty( cLine )
            LOOP
         ENDIF

         nSkip := 0
         cfirst := hb_TokenPtr( cLine, @nSkip )
         IF cfirst == "func"
            IF Left( cfirst := hb_TokenPtr( cLine, @nSkip ), 1 ) == '('
               /*
               IF ( nSkip := At( ')', cLine ) ) > 0
                  nSkip ++
                  cfirst := hb_TokenPtr( cLine, @nSkip )
               ELSE
                  LOOP
               ENDIF
               */
               LOOP
            ENDIF
            IF ( nSkip := At( '(', cfirst ) ) > 0
               cfirst := Left( cfirst, nSkip )
            ENDIF
            IF isUpper( cfirst )
               cRes += " " + cfirst
            ENDIF

         ELSEIF cfirst == "type"
            cfirst := hb_TokenPtr( cLine, @nSkip )
            IF isUpper( cfirst )
               cRes += " " + cfirst
            ENDIF

         ENDIF

      NEXT
   ENDIF

   RETURN cRes

STATIC FUNCTION _go_AddFromImp( aImp, aWords, cPrefix )

   LOCAL cRes, cBuff, arr, cFileName := "golang.dat", cEol := Chr(10), i, nSkip, nPrefLen := Len( cPrefix )
   LOCAL lFou := .F., cGoPath, cWord

   IF !Empty( cBuff := MemoRead( cIniPath + cFileName ) )
      IF ( i := At( cEol, cBuff ) ) > 1 .AND. Substr( cBuff, i-1, 1 ) == Chr(13)
         cEol := Chr(13) + cEol
      ENDIF
      arr := hb_ATokens( cBuff, cEol )
      FOR i := 1 TO Len( arr )
         nSkip := 0
         IF aImp[2] == hb_TokenPtr( arr[i], @nSkip )
            cRes := Ltrim( Substr( arr[i], nSkip ) )
            lFou := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

   IF !lFou
      cGoPath := hb_getEnv( "GOROOT" ) + hb_ps() + "src" + hb_ps() + aImp[2]
      IF !hb_DirExists( cGoPath )
         cGoPath := hb_getEnv( "GOPATH" ) + hb_ps() + "src" + hb_ps() + aImp[2]
         IF !hb_DirExists( cGoPath )
            RETURN Nil
         ENDIF
      ENDIF
      arr := Directory( cGoPath + hb_ps() + "*.go" )
      cRes := ""
      FOR i := 1 TO Len( arr )
         IF !( "test" $ arr[i,1] )
            cRes += _go_GetImpNames( cGoPath + hb_ps() + arr[i,1] )
         ENDIF
      NEXT
      IF Empty( cBuff )
         cBuff := ""
      ELSE
         cBuff += cEol
      ENDIF
      cBuff += aImp[2] + cRes
      hb_MemoWrit( cIniPath + cFileName, cBuff )
   ENDIF

   nSkip := 0
   DO WHILE !Empty( cWord := hb_TokenPtr( cRes, @nSkip ) )
      IF nPrefLen > 0 .AND. !( Left( cWord, nPrefLen ) == cPrefix )
         LOOP
      ENDIF
      Aadd( aWords, aImp[1] + '.' + cWord )
   ENDDO

   RETURN Nil

STATIC FUNCTION _go_AddImp( cLine, cWord, nSkip, cPrefix, aWords, aImport )

   LOCAL nPos, arr := { Nil, Nil }

   IF Left( cWord, 1 ) != '"'
      arr[1] := cWord
      cWord := hb_TokenPtr( cLine, @nSkip )
   ENDIF
   IF Left( cWord, 1 ) == '"'
      cWord := Substr( cWord, 2, Len(cWord)-2 )
   ENDIF
   arr[2] := cWord
   IF ( nPos := Rat( "/", cWord ) ) > 0
      cWord := Substr( cWord, nPos + 1 )
   ENDIF
   IF Empty( arr[1] )
      arr[1] := cWord  //Left( cWord, Len(cWord)-1 )
   ENDIF
   IF Left( arr[1], Len(cPrefix) ) == cPrefix
      Aadd( aWords, arr[1] + "." )
      Aadd( aImport, arr )
   ENDIF

   RETURN Nil

STATIC FUNCTION _go_AddVars( oEdit, cLine, cPrefix, nSkip, arr )

   LOCAL cWord, nPos, nPos2, nPrefLen := Len( cPrefix )
   LOCAL b1 := "([{", b2 := ")]}", c1, c2, i, np

   DO WHILE ( nPos := edi_FindChNext( oEdit, cLine, nSkip, "(", "[", "{" ) ) > 0
      np := 0
      nPos2 := nPos
      c1 := cp_Substr( oEdit:lUtf8, cLine, nPos, 1 )
      i := At( c1, b1 )
      c2 := Substr( b2,i,1 )
      DO WHILE ( nPos2 := edi_FindChNext( oEdit, cLine, nPos2, c1, c2 ) ) > 0
         IF cp_Substr( oEdit:lUtf8, cLine, nPos2, 1 ) == c1
            np ++
         ELSEIF np > 0
            np --
         ELSE
            EXIT
         ENDIF
      ENDDO
      cLine := Left( cLine,nPos-1 ) + Iif( nPos2>0, Substr( cLine, nPos2+1 ), "" )
   ENDDO

   cLine := _go_DropComments( cLine )
   DO WHILE !Empty( cWord := AllTrim( hb_TokenPtr( cLine, @nSkip, ',', .T. ) ) )
      IF Left(cWord,nPrefLen) == cPrefix
         IF ( nPos := At( "=", cWord ) ) > 0
            Aadd( arr, Trim( Left( cWord, nPos-1 ) ) )
            EXIT
         ELSEIF ( nPos := At( " ", cWord ) ) > 0
            Aadd( arr, Trim( Left( cWord, nPos-1 ) ) )
         ELSE
            Aadd( arr, cWord )
         ENDIF
      ENDIF
   ENDDO

   RETURN Nil

/*
 * _go_KeyWords( oEdit, cPrefix )
 * Scans the text to find keywords to be included in a list for autocompetion
 */
STATIC FUNCTION _go_KeyWords( oEdit, cPrefix, lImports )

   LOCAL i, nPos, c, aText := oEdit:aText, cLine, cfirst, cSecond, nSkip, aWords := {}
   LOCAL lGlob := .T., nPrefLen := Len( cPrefix ), nLine0, nLineCurr := oEdit:nLine
   LOCAL oHili := oEdit:oHili
   LOCAL aImport := {}, cPref2, lDot := .F.

   IF Empty( lImports ); lImports := .F.; ENDIF
   IF ( nPos := At( '.', cPrefix ) ) != 0
      lDot := .T.
      cPref2 := SubStr( cPrefix, nPos + 1 )
      cPrefix := Left( cPrefix, nPos-1 )
   ENDIF

   oHili:CheckComm()
   FOR i := 1 TO Len( aText )
      IF Empty( cLine := Ltrim( oHili:Getline(i) ) )
         LOOP
      ENDIF
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF lGlob
         IF cfirst == "import" .OR. ( !lImports .AND. cfirst == "const" )
            IF Empty( cSecond := hb_TokenPtr( cLine, @nSkip ) ) .OR. Left( cSecond, 1 ) == '('
               DO WHILE ++i <= Len( aText )
                  IF !Empty( cLine := Alltrim( aText[i] ) ) .AND. Left( cLine,1 ) != '/' .AND. ;
                     oHili:IsComm( i-1 ) != 1
                     IF Left( cLine, 1 ) == ')'
                        EXIT
                     ENDIF
                     nSkip := 0
                     IF cfirst == "import"
                        _go_AddImp( cLine, hb_TokenPtr( cLine, @nSkip ), nSkip, cPrefix, aWords, aImport )
                     ELSEIF Left( cSecond := hb_TokenPtr( cLine, @nSkip ), nPrefLen) == cPrefix
                        Aadd( aWords, cSecond )
                     ENDIF
                     IF Right( cLine, 1 ) == ')'
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               IF cfirst == "import"
                  _go_AddImp( cLine, cSecond, nSkip, cPrefix, aWords, aImport )
               ELSEIF Left( cSecond, nPrefLen) == cPrefix
                  Aadd( aWords, cSecond )
               ENDIF
            ENDIF
            LOOP
         ELSEIF !lImports .AND. cfirst == "var"
            DO WHILE ( c := Right( cLine, 1 ) ) == "," .OR. c == "="
               cLine := Left( cLine, Len(cLine)-1 ) + " " + Alltrim( aText[++i] )
            ENDDO
            _go_AddVars( oEdit, cLine, cPrefix, nSkip, aWords )
         ENDIF
      ENDIF
      IF cfirst == "func"
         IF lImports
            EXIT
         ENDIF
         IF i < nLineCurr
            nLine0 := i
         ENDIF
         lGlob := .F.
         IF !lDot
            IF Left( cSecond := hb_TokenPtr( cLine, @nSkip ), 1 ) == '('
               IF ( nSkip := At( ')', cLine ) ) > 0
                  nSkip ++
                  cSecond := hb_TokenPtr( cLine, @nSkip )
               ELSE
                  LOOP
               ENDIF
            ENDIF
            IF Left( cSecond, nPrefLen) == cPrefix
               IF ( nSkip := At( '(', cLine ) ) > 0
                  cSecond := Left( cSecond, nSkip - 1 )
               ENDIF
               Aadd( aWords, cSecond )
            ENDIF
         ENDIF
         LOOP
      ENDIF
   NEXT

   IF !Empty( nLine0 )
      FOR i := nLine0 TO nLineCurr - 1
         IF Empty( cLine := Alltrim( oHili:Getline(i) ) )
            LOOP
         ENDIF
         nSkip := 0
         IF i == nLine0
         ELSE
            IF ( cfirst := hb_TokenPtr( cLine, @nSkip ) ) == "var"
               DO WHILE ( c := Right( cLine, 1 ) ) == "," .OR. c == "="
                  cLine := Left( cLine, Len(cLine)-1 ) + " " + Alltrim( aText[++i] )
               ENDDO
               _go_AddVars( oEdit, cLine, cPrefix, nSkip, aWords )
            ENDIF
         ENDIF
      NEXT
   ENDIF

   IF lDot
      IF ( i := Ascan( aImport, {|a|a[1]==cPrefix} ) ) > 0
         _go_AddFromImp( aImport[i], aWords, cPref2 )
      ENDIF
   ENDIF

   RETURN Iif( lImports, aImport, aWords )