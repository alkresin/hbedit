
#define ALT_PRESSED   0x040000
#define K_ALT_I   279

STATIC cIniPath

FUNCTION Plug_go_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _go_Init_OnKey(o,n)
      IF bOnKeyOrig != Nil .AND. nRes >= 0
         nRes := Eval( bOnKeyOrig, o, Iif( nRes==0, n, nRes ) )
      ENDIF
      RETURN nRes
   }

   cIniPath := cPath
   oEdit:bAutoC := {|o,s| _go_AutoC(o,s)}

   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey

   RETURN Nil

FUNCTION _go_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cPackage, cWord
   LOCAL ny, nx1, nx2

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_I
         ny := oEdit:nLine
         nx2 := oEdit:nPos
         IF nx2 > 1 .AND. cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx2, 1 ) == '.'
            nx2 --
         ENDIF
         IF nx2 == 1 .OR. isAlpha( cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx2-1, 1 ) )
            nx1 := edi_PrevWord( oEdit, .F., .F., .F., ny, nx2 )
         ELSE
            nx1 := nx2
         ENDIF
         IF nx1 > 1 .AND. cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx1-1, 1 ) == '.'
            nx1 := edi_PrevWord( oEdit, .F., .F., .F., ny, nx1-2 )
         ENDIF
         nx2 := edi_NextWord( oEdit, .F., .F., .F., ny, nx2 )
         IF cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx2, 1 ) == '.'
            nx2 := edi_NextWord( oEdit, .F., .F., .F., ny, nx2+1 )
         ENDIF
         cWord := cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx1, nx2-nx1 )
         _go_GetFuncInfo( oEdit, cWord )
         DevPos( nRow, nCol )
         oEdit:TextOut()
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _go_GetFuncInfo( oEdit, cWord )

   LOCAL nx1, cBuff, cFileOut := hb_DirTemp() + "hbedit.out", o, nPos, cAddW := "$FuncInfo"
   LOCAL aImport, cPackage, i

   IF ( nx1 := At( '.', cWord ) ) > 0
      cPackage := cp_Left( oEdit:lUtf8, cWord, nx1 - 1 )
      cWord := cp_Substr( oEdit:lUtf8, cWord, nx1 + 1 )
      aImport := _go_KeyWords( oEdit, cPackage, .T. )
      IF ( i := Ascan( aImport, {|a|a[1]==cPackage} ) ) == 0
         RETURN Nil
      ENDIF
      cPackage := aImport[i,2]
      // edi_Alert( cPackage + " " + cWord )

      FErase( cFileOut )
      cedi_RunConsoleApp( 'godoc ' + cPackage + ' ' + cWord, cFileOut )
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
   ENDIF

   RETURN Nil

STATIC FUNCTION _go_AutoC( oEdit, cPrefix )

   LOCAL hTrieLang, hTrie, o := oEdit:oHili
   LOCAL arr, i, nPos, nLen, nPrefLen := Len( cPrefix )

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
            //edi_Alert( "Add " + arr[i] )
         ENDIF
      NEXT
      //edi_Alert( cPrefix + " :" + str(Len(arr)) + Str(trie_Count(hTrie,"aDi")) )
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
   LOCAL aDop := Iif( !Empty(oEdit:oHili) .AND. !Empty(oEdit:oHili:aDop), oEdit:oHili:aDop, Nil )
   LOCAL aImport := {}, cPref2, lDot := .F.

   IF Empty( lImports ); lImports := .F.; ENDIF
   IF ( nPos := At( '.', cPrefix ) ) != 0
      lDot := .T.
      cPref2 := SubStr( cPrefix, nPos + 1 )
      cPrefix := Left( cPrefix, nPos-1 )
   ENDIF

   FOR i := 1 TO Len( aText )
      cLine := Ltrim( aText[i] )
      IF i > 1 .AND. !Empty( aDop )
         // Checks if a line is commented with /* */ operators, using a hilight object
         IF aDop[i-1] == 1
            IF ( nPos := At( "*/", cLine ) ) > 0
               cLine := Ltrim( Substr( cLine,nPos+2 ) )
            ELSE
               LOOP
            ENDIF
         ENDIF
      ENDIF
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF lGlob
         IF cfirst == "import" .OR. ( !lImports .AND. cfirst == "const" )
            IF Empty( cSecond := hb_TokenPtr( cLine, @nSkip ) ) .OR. Left( cSecond, 1 ) == '('
               DO WHILE ++i <= Len( aText )
                  IF !Empty( cLine := Alltrim( aText[i] ) ) .AND. Left( cLine,1 ) != '/' .AND. ;
                     aDop[i-1] != 1
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
         cLine := Ltrim( aText[i] )
         IF i > 1 .AND. !Empty( aDop )
            // Checks if a line is commented with /* */ operators, using a hilight object
            IF aDop[i-1] == 1
               IF ( nPos := At( "*/", cLine ) ) > 0
                  cLine := Ltrim( Substr( cLine,nPos+2 ) )
               ELSE
                  LOOP
               ENDIF
            ENDIF
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
