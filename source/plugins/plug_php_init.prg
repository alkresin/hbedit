#define ALT_PRESSED   0x040000
#define CTRL_PRESSED  0x020000
#define K_ALT_L    294
#define K_ENTER     13
#define K_ESC       27

STATIC cIniPath

FUNCTION Plug_php_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "Php plugin: Alt-L Functions list " + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab Autocompetion","" ) )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         IF oEdit:hCargo == Nil
            oEdit:hCargo := hb_hash()
         ENDIF
         oEdit:hCargo["help"] := "Php plugin hotkeys:" + Chr(10) + ;
            "  Alt-L  - Functions list" + Chr(10) + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab - Autocompetion" + Chr(10),"" )
      ENDIF
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _php_Init_OnKey(o,n)
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
   oEdit:bAutoC := {|o,s| _php_AutoC(o,s)}

   RETURN Nil

STATIC FUNCTION _php_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cWord

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_L
         _php_Spis( oEdit )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _php_Spis( oEdit )

   LOCAL i, n, arr := oEdit:aText, cLine, cFunc, cfirst, nSkip, arrfnc := {}
   LOCAL oHili := oEdit:oHili
   LOCAL bKeys := {|nKeyExt,nRow|
      LOCAL nn, oNew, s
      IF nKeyExt == 0x41000008  // F8
         s := ""
         FOR nn := 1 TO Len( arrfnc )
            s += arrfnc[nn,1] + Chr(10)
         NEXT
         oNew := mnu_NewBuf( TEdit():aWindows[TEdit():nCurr], "$FuncList", s )
         oNew:cp := oEdit:cp
         oNew:lUtf8 := oEdit:lUtf8
         RETURN .F.
      ENDIF
      RETURN .T.
   }

   oHili:CheckComm()
   FOR i := 1 TO Len( arr )
      IF Empty( cLine := Lower(Ltrim( oHili:Getline(i) )) )
         LOOP
      ENDIF
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF cfirst == "function"
         IF Right( cFunc := cp_Left( oEdit:lUtf8,arr[i],64 ), 1 ) == '{'
            cFunc := Trim( Left( cFunc, Len( cFunc ) - 1 ) )
         ENDIF
         Aadd( arrfnc, { cFunc, Nil, i } )
      ENDIF
   NEXT
   IF !Empty( arrfnc )
      oEdit:TextOut()
      n := oEdit:nLine
      FOR i := 1 TO Len( arrfnc )
         IF arrfnc[i,3] > n
            n := i - 1
            EXIT
         ENDIF
      NEXT
      n := Iif( n > Len(arrfnc), Len(arrfnc), Iif( n == 0, 1, n ) )
      IF ( i := FMenu( oEdit, arrfnc, 2, 6,,,,, n, (Len(arrfnc)>3),,, bKeys, ;
            " Functions list    F8 - Editor" ) ) > 0
         oEdit:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _php_AutoC( oEdit, cPrefix )

   LOCAL hTrieLang, hTrie, o := oEdit:oHili
   LOCAL arr, i, nLen, nPrefLen := Len( cPrefix )

   IF Empty( hTrieLang := hb_hGetDef( o:hHili, "htrie", Nil ) )
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

   IF !Empty( arr := _php_KeyWords( oEdit, cPrefix, hTrieLang ) )
      FOR i := 1 TO Len( arr )
         IF ( nLen := Len( arr[i] ) ) >= 4 .AND. nLen > nPrefLen
            IF Empty( hTrie )
               hTrie := trie_Create( .T. )
            ENDIF
            trie_Add( hTrie, arr[i] )
            //edi_Alert( "Add " + arr[i] )
         ENDIF
      NEXT
   ENDIF

   RETURN hTrie

STATIC FUNCTION _php_KeyWords( oEdit, cPrefix, hTrieLang )

   LOCAL i, nPos, nPos1, c, aText := oEdit:aText, cLine, cSecond, nSkip, aWords := {}
   LOCAL nLen, nPrefLen := Len( cPrefix ), nLineCurr := oEdit:nLine, lFunc := .F.
   LOCAL oHili := oEdit:oHili

   FOR i := Len( aText ) TO 1 STEP -1
      IF Empty( cLine := Ltrim( oHili:Getline(i) ) )
         LOOP
      ENDIF

      IF Left( cLine,6 ) == "define"
         nPos := 7
         DO WHILE hb_bPeek( cLine,nPos ) == 32; nPos ++; ENDDO
         IF hb_bPeek( cLine,nPos ) == 40 // (
            nPos ++
            DO WHILE hb_bPeek( cLine,nPos ) == 32; nPos ++; ENDDO
            IF ( c := hb_bPeek( cLine,nPos ) ) == 39 .OR. c == 34
               nPos ++
               nPos1 := nPos
               nLen := Len( cLine )
               DO WHILE nPos < nLen .AND. hb_bPeek( cLine,nPos ) != c; nPos ++; ENDDO
               IF Left( cSecond := Substr( cLine, nPos1, nPos-nPos1 ), nPrefLen) == cPrefix
                  Aadd( aWords, cSecond )
               ENDIF
            ENDIF
         ENDIF
         LOOP
      ELSEIF i < nLineCurr .AND. !lFunc
         IF Left( cLine,8 ) == "function"
            lFunc := .T.
         ENDIF
         nPos := 1
         DO WHILE ( nPos := hb_At( '$', cLine, nPos ) ) > 0
            nPos1 := nPos
            nPos ++
            nLen := Len( cLine )
            DO WHILE nPos < nLen .AND. ( c := hb_bPeek( cLine,nPos ) ) >= 48 .AND. ;
               (c <= 57 .OR. c >= 65) .AND. (c <= 90 .OR. c >= 95) .AND. c <= 122
               nPos ++
            ENDDO
            IF Left( cSecond := Substr( cLine, nPos1, nPos-nPos1 ), nPrefLen) == cPrefix
               Aadd( aWords, cSecond )
            ENDIF
         ENDDO
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
            cSecond := Substr( cLine, nSkip, nPos-nSkip )
            IF Left(cSecond,nPrefLen) == cPrefix .AND. hb_Ascan(aWords,cSecond,,,.T.) == 0 ;
               .AND. !trie_Exist( hTrieLang, cSecond )
               Aadd( aWords, cSecond + "(" )
            ENDIF
         ENDIF
         nPos ++
      ENDDO
   NEXT

   RETURN aWords