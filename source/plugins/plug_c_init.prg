#define ALT_PRESSED   0x040000
#define K_ALT_L            294

STATIC cIniPath
STATIC lClass

FUNCTION Plug_c_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "C/C++ plugin:  Alt-L Functions list" + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab Autocompetion","" ) )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         oEdit:oHili:hHili["help"] := "C/C++ plugin hotkeys:" + Chr(10) + ;
            "  Alt-L  - Functions list" + Chr(10) + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab - Autocompetion" + Chr(10),"" )
      ENDIF
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _c_Init_OnKey(o,n)
      IF bOnKeyOrig != Nil .AND. nRes >= 0
         nRes := Eval( bOnKeyOrig, o, Iif( nRes==0, n, nRes ) )
      ENDIF
      RETURN nRes
   }

   cIniPath := cPath
   oEdit:bStartEdit := bStartEdit
   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey
   oEdit:bAutoC := {|o,s| _c_AutoC(o,s)}

   RETURN Nil

STATIC FUNCTION _c_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cWord

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_L
         _c_Spis( oEdit )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _c_Spis( oEdit )

   LOCAL i, n, arrfnc
   LOCAL oHili := oEdit:oHili
   //LOCAL aDop := Iif( !Empty(oEdit:oHili) .AND. !Empty(oEdit:oHili:aDop), oEdit:oHili:aDop, Nil )

   //IF !Empty( aDop ) .AND. oEdit:oHili:nDopChecked < Len( aDop )
   //   oEdit:oHili:Do( Len( oEdit:aText ) )
   //ENDIF

   lClass := .F.
   oHili:CheckComm()
   IF Empty( arrfnc := _c_Funcs( oEdit, oHili ) )
      edi_Alert( "Nothing found..." )
   ELSE
      oEdit:TextOut()
      n := oEdit:nLine
      FOR i := 1 TO Len( arrfnc )
         IF arrfnc[i,3] > n
            n := i - 1
            EXIT
         ENDIF
      NEXT
      n := Iif( n > Len(arrfnc), Len(arrfnc), Iif( n == 0, 1, n ) )
      IF ( i := FMenu( oEdit, arrfnc, 2, 6,,,,, n, (Len(arrfnc)>3) ) ) > 0
         oEdit:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _c_Funcs( oEdit, oHili, nLineEnd )

   LOCAL i, j, n, arr := oEdit:aText, cLine, nPos, nPos2, c, cLinePrev := "", arrfnc, lList := .F., nLast := 0
   LOCAL lUtf8 := oEdit:lUtf8, cQuotes := ['"], cFind := ['"{}], nLevel := 0

   IF Empty( nLineEnd )
      nLineEnd := Len( arr )
      lList := .T.
      arrfnc := {}
   ENDIF
   FOR i := 1 TO nLineEnd
      cLine := AllTrim( arr[i] )
      IF i > 1 //.AND. !Empty( aDop )
         // Checks if a line is commented with /* */ operators, using a hilight object
         IF oHili:IsComm( i-1 ) == 1 //aDop[i-1] == 1
            IF ( nPos := At( "*/", cLine ) ) > 0
               cLine := Ltrim( Substr( cLine,nPos+2 ) )
            ELSE
               LOOP
            ENDIF
         ENDIF
      ENDIF
      IF Empty( cLine )
         LOOP
      ENDIF

      nPos := 1
      DO WHILE ( nPos := Iif( lUtf8, cedi_utf8pbrk( cFind, cLine, nPos ), cedi_strpbrk( cFind, cLine, nPos ) ) ) != -1
         IF ( c := cp_Substr( lUtf8,cLine,nPos,1 ) ) $ cQuotes
            IF ( nPos := cp_At( lUtf8, c, cLine, nPos + 1 ) ) == 0
               EXIT
            ENDIF

         ELSEIF c == '{'
            IF nLevel == 0 .OR. (lClass .AND. nLevel == 1)
               IF nLevel == 0
                  lClass := .F.
               ENDIF
               IF nPos == 1
                  //edi_writelog( "1: "+cLinePrev )
                  IF !lClass .AND. Left( cLinePrev, 6 ) == "class "
                     //edi_writelog( "class: "+cLinePrev )
                     lClass := .T.
                  ELSEIF lClass .OR. Right( _C_DropComments( oEdit, cLinePrev ), 1 ) == ")"
                     //edi_writelog( "2: "+cLinePrev )
                     IF ( j := _C_AddF( lUtf8, arrfnc, arr, i, cLinePrev ) ) > 0
                        nLast := j
                     ENDIF
                  ENDIF
               ELSE
                  j := nPos
                  DO WHILE ( c := cp_Substr( lUtf8,cLine,--j,1 ) ) == ' ' .AND. j > 1; ENDDO
                  IF c == ')'
                     IF ( j := _C_AddF( lUtf8, arrfnc, arr, i, cLine ) ) > 0
                        nLast := j
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            nLevel ++

         ELSEIF c == '}'
            nLevel --
            IF nLevel == 0
               lClass := .F.
            ENDIF

         ENDIF
         nPos := cp_NextPos( lUtf8, cLine, nPos )
      ENDDO

      IF !Empty( cLine )
         cLinePrev := cLine
      ENDIF
   NEXT

   RETURN Iif( lList, arrfnc, nLast )

STATIC FUNCTION _C_AddF( lUtf8, arrfnc, arr, nLine, cLinePrev )

   LOCAL j

   IF !( '(' $ cLinePrev )
      FOR j := nLine-2 TO 1 STEP -1
         IF '(' $ arr[j]
            cLinePrev := AllTrim( arr[j] )
            nLine := j
            EXIT
         ENDIF
      NEXT
   ENDIF
   IF ( j := At( '(', cLinePrev ) ) > 0
      IF Trim( Left( cLinePrev, j-1 ) ) $ "if|while|for|switch"
         RETURN 0
      ENDIF
      IF arrfnc != Nil
         Aadd( arrfnc, { cp_Left( lUtf8, cLinePrev, 64 ), Nil, nLine } )
      ENDIF
      RETURN nLine
   ENDIF

   RETURN 0

STATIC FUNCTION _C_DropComments( oEdit, cLine )

   LOCAL nPos := 1, nPos2

   DO WHILE ( nPos := hb_At( "//", cLine, nPos ) ) > 0
      IF !edi_InQuo( oEdit, cLine, nPos )
         cLine := Trim( Left( cLine, nPos-1 ) )
         EXIT
      ENDIF
      nPos += 2
   ENDDO

   nPos := 1
   DO WHILE ( nPos := hb_At( "/*", cLine, nPos ) ) > 0
      IF !edi_InQuo( oEdit, cLine, nPos )
         nPos2 := hb_At( "*/", cLine, nPos+2 )
         cLine := Trim( Left( cLine, nPos-1 ) ) + Iif( nPos2==0, "", Substr( cLine, nPos2+2 ) )
      ELSE
         nPos += 2
      ENDIF
   ENDDO

   RETURN cLine

STATIC FUNCTION _c_AutoC( oEdit, cPrefix )

   LOCAL hTrieLang, hTrie, o := oEdit:oHili
   LOCAL arr, i, nPos, nLen, nPrefLen := Len( cPrefix )
   STATIC cUsl := " #include #define #ifdef #ifndef #else #endif"

   IF Empty( hTrieLang := hb_hGetDef( o:hHili, "htrie", Nil ) )
      arr := hb_ATokens( Iif(Empty(o:cKeywords1),"",o:cKeywords1) + " " + ;
         Iif(Empty(o:cKeywords2),"",o:cKeywords2) + " " + Iif(Empty(o:cKeywords3),"",o:cKeywords3) + ;
         " " + Iif(Empty(o:cKeywords4),"",o:cKeywords4) + cUsl, " " )
      hTrieLang := o:hHili["htrie"] := trie_Create( .T. )
      FOR i := 1 TO Len( arr )
         IF Len( arr[i] ) > 3
            trie_Add( hTrieLang, arr[i] )
         ENDIF
      NEXT
   ENDIF

   IF !Empty( arr := _c_KeyWords( oEdit, cPrefix, hTrieLang ) )
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

/*
 * _c_KeyWords( oEdit, cPrefix )
 * Scans the text to find keywords to be included in a list for autocompetion
 */
STATIC FUNCTION _c_KeyWords( oEdit, cPrefix, hTrieLang )

   LOCAL i, nPos, c, aText := oEdit:aText, cLine, cfirst, cSecond, nSkip, aWords := {}
   LOCAL lGlob := .T., nPrefLen := Len( cPrefix ), nLine0, nLineCurr := oEdit:nLine
   LOCAL oHili := oEdit:oHili
   //LOCAL aDop := Iif( !Empty(oEdit:oHili) .AND. !Empty(oEdit:oHili:aDop), oEdit:oHili:aDop, Nil )

   FOR i := 1 TO Len( aText )
      cLine := Ltrim( aText[i] )
      IF i > 1 //.AND. !Empty( aDop )
         // Checks if a line is commented with /* */ operators, using a hilight object
         IF oHili:IsComm( i-1 ) == 1 //aDop[i-1] == 1
            IF ( nPos := At( "*/", cLine ) ) > 0
               cLine := Ltrim( Substr( cLine,nPos+2 ) )
            ELSE
               LOOP
            ENDIF
         ENDIF
      ENDIF
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF i < nLineCurr
         IF cfirst == "#define" .OR. ( cfirst == "#" .AND. (cSecond := hb_TokenPtr( cLine, @nSkip )) == "define" )
            IF Left( cSecond := hb_TokenPtr( cLine, @nSkip ), nPrefLen) == cPrefix
               Aadd( aWords, cSecond )
            ENDIF
            LOOP
         ENDIF
         IF lGlob
         ENDIF
      ENDIF
      // Check for function calls
      nPos := 1
      DO WHILE ( nPos := hb_At( '(', cLine, nPos ) ) > 0
         nSkip := nPos
         DO WHILE nSkip > 1 .AND. ( ( ( c := Substr( cLine, nSkip-1, 1 ) ) >= '0' .AND. c <= '9' ) .OR. ;
            ( c >= 'A' .AND. c <= 'Z' ) .OR. ( c >= 'a' .AND. c <= 'z' ) .OR. c == '_' )
            nSkip --
         ENDDO
         IF nSkip < nPos
            cSecond := Substr( cLine, nSkip, nPos-nSkip+1 )
            IF Left(cSecond,nPrefLen) == cPrefix .AND. hb_Ascan(aWords,cSecond,,,.T.) == 0 ;
               .AND. !trie_Exist( hTrieLang, cSecond )
               Aadd( aWords, cSecond )
            ENDIF
         ENDIF
         nPos ++
      ENDDO
   NEXT

   IF !Empty( nLine0 )
   ENDIF

   RETURN aWords
