
FUNCTION Plug_go_Init( oEdit, cPath )

   oEdit:bAutoC := {|o,s| _go_AutoC(o,s)}

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

STATIC FUNCTION _go_AddImp( cLine, cWord, nSkip, cPrefix, aWords, aImport )

   LOCAL nPos

   edi_Writelog( cLine )
   Aadd( aImport, { Nil, Nil } )
   IF Left( cWord, 1 ) != '"'
      ATail( aImport )[1] := cWord
      cWord := hb_TokenPtr( cLine, @nSkip )
   ENDIF
   IF Left( cWord, 1 ) == '"'
      cWord := Substr( cWord, 2, Len(cWord)-2 )
   ENDIF
   ATail( aImport )[2] := cWord
   IF ( nPos := Rat( "/", cWord ) ) > 0
      cWord := Substr( cWord, nPos + 1 )
   ENDIF
   IF Empty( ATail( aImport )[1] )
      ATail( aImport )[1] := Left( cWord, Len(cWord)-1 )
   ENDIF
   edi_Writelog( cWord )
   IF Left( cWord, Len(cPrefix) ) == cPrefix
      Aadd( aWords, cWord + "." )
   ENDIF

   RETURN Nil

STATIC FUNCTION _go_AddVars( oEdit, cLine, cPrefix, nSkip, arr )

   LOCAL lComm := .F., cWord, nPos, nPos2, nPrefLen := Len( cPrefix )
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

   DO WHILE !lComm .AND. !Empty( cWord := AllTrim( hb_TokenPtr( cLine, @nSkip, ',', .T. ) ) )
      IF ( nPos := At( "//", cWord ) ) > 0 .OR. ( nPos := At( "/*", cWord ) ) > 0
         cWord := Trim( Left( cWord, nPos - 1 ) )
         lComm := .T.
      ENDIF
      IF Lower( Left(cWord,nPrefLen) ) == cPrefix
         IF ( nPos := At( "=", cWord ) ) > 0
            Aadd( arr, Trim( Left( cWord, nPos-1 ) ) )
            EXIT
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
STATIC FUNCTION _go_KeyWords( oEdit, cPrefix )

   LOCAL i, nPos, c, aText := oEdit:aText, cLine, cfirst, cSecond, nSkip, aWords := {}
   LOCAL lGlob := .T., nPrefLen := Len( cPrefix ), nLine0, nLineCurr := oEdit:nLine
   LOCAL aDop := Iif( !Empty(oEdit:oHili) .AND. !Empty(oEdit:oHili:aDop), oEdit:oHili:aDop, Nil )
   LOCAL aImport := {}

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
         IF cfirst == "import" .OR. cfirst == "const"
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
         ELSEIF cfirst == "var"
            DO WHILE ( c := Right( cLine, 1 ) ) == "," .OR. c == "="
               cLine := Left( cLine, Len(cLine)-1 ) + " " + Alltrim( aText[++i] )
            ENDDO
            _go_AddVars( oEdit, cLine, cPrefix, nSkip, aWords )
         ENDIF
      ENDIF
      IF cfirst == "func"
         IF i < nLineCurr
            nLine0 := i
         ENDIF
         lGlob := .F.
         IF Left( cSecond := hb_TokenPtr( cLine, @nSkip ), 1 ) == '('
            IF ( nSkip := At( ')', cLine ) ) > 0
               nSkip ++
               cSecond := hb_TokenPtr( cLine, @nSkip )
            ELSE
               LOOP
            ENDIF
         ENDIF
         IF Left( cSecond, nPrefLen) == cPrefix
            IF Right( cSecond,1 ) == ')'
               cSecond := Left( cSecond, Len(cSecond) - 1 )
            ENDIF
            Aadd( aWords, cSecond )
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

   RETURN aWords
