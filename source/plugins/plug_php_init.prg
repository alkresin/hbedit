#define ALT_PRESSED   0x040000
#define CTRL_PRESSED  0x020000
#define K_ALT_D    288
#define K_ALT_I    279
#define K_ALT_L    294
#define K_ENTER     13
#define K_ESC       27
#define K_CTRL_RIGHT 2

STATIC cIniPath
STATIC lIsCurl := .F., cServAddr
STATIC lDescri := .T., lSources := .F., lChglog := .F., lSamples := .F., lRu := .F.
STATIC aHwgFuncs, aHbFuncs
STATIC aHbShort

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
         oEdit:oHili:hHili["help"] := "Php plugin hotkeys:" + Chr(10) + ;
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
   //LOCAL aDop := Iif( !Empty(oEdit:oHili) .AND. !Empty(oEdit:oHili:aDop), oEdit:oHili:aDop, Nil )

   //IF !Empty( aDop ) .AND. oEdit:oHili:nDopChecked < Len( aDop )
   //   oEdit:oHili:Do( Len( oEdit:aText ) )
   //ENDIF
   oHili:CheckComm()
   FOR i := 1 TO Len( arr )
      cLine := Lower( Ltrim( arr[i] ) )
      IF i > 1 //.AND. !Empty( aDop )
         IF oHili:IsComm( i-1 ) == 1 //aDop[i-1] == 1
            IF ( n := At( "*/", cLine ) ) > 0
               cLine := Ltrim( Substr( cLine,n+2 ) )
            ELSE
               LOOP
            ENDIF
         ENDIF
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
      IF ( i := FMenu( oEdit, arrfnc, 2, 6,,,,, n, (Len(arrfnc)>3) ) ) > 0
         oEdit:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _php_AutoC( oEdit, cPrefix )

   LOCAL hTrieLang, hTrie, o := oEdit:oHili
   LOCAL arr, i

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

   RETURN hTrie

