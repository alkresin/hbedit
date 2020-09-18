
FUNCTION Plug_c_Init( oEdit, cPath )

   oEdit:bAutoC := {|o,s| _c_AutoC(o,s)}

   RETURN Nil

STATIC FUNCTION _c_AutoC( oEdit, cPrefix )

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

   IF !Empty( arr := _c_KeyWords( oEdit, cPrefix ) )
      FOR i := 1 TO Len( arr )
         IF ( nLen := Len( arr[i] ) ) >= 4 .AND. nLen > nPrefLen
            IF Empty( hTrie )
               hTrie := trie_Create( .F. )
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
STATIC FUNCTION _c_KeyWords( oEdit, cPrefix )

   LOCAL i, nPos, aText := oEdit:aText, cLine, cfirst, cSecond, nSkip, aWords := {}
   LOCAL lGlob := .T., nPrefLen := Len( cPrefix ), nLineCurr := oEdit:nLine
   LOCAL aDop := Iif( !Empty(oEdit:oHili) .AND. !Empty(oEdit:oHili:aDop), oEdit:oHili:aDop, Nil )

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
   NEXT

   RETURN aWords
