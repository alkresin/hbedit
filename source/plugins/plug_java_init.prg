#define ALT_PRESSED   0x040000
#define K_ALT_L            294
#define K_ALT_R            275

STATIC cIniPath
STATIC cTerm
STATIC cClass := "class "

FUNCTION Plug_java_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "Java plugin:  Alt-L Functions list  Alt-R Run" + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab Autocompetion","" ) )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         oEdit:oHili:hHili["help"] := "Java plugin hotkeys:" + Chr(10) + ;
            "  Alt-L  - Functions list" + Chr(10) + ;
            "  Alt-R  - Run" + Chr(10) + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab - Autocompetion" + Chr(10),"" )
      ENDIF
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _java_Init_OnKey(o,n)
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
   oEdit:bAutoC := {|o,s| _java_AutoC(o,s)}

   RETURN Nil

STATIC FUNCTION _java_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cWord

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_L
         _java_Spis( oEdit )
         RETURN -1
      ELSEIF nKey == K_ALT_R
         _java_Run( oEdit )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _java_Spis( oEdit )

   LOCAL i, n, arrfnc

   oEdit:oHili:CheckComm()
   IF Empty( arrfnc := _java_Funcs( oEdit ) )
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

STATIC FUNCTION _java_Funcs( oEdit )

   LOCAL oHili := oEdit:oHili, i, n, arr := oEdit:aText, nLineEnd := Len( arr ), cLine
   LOCAL nPos, nPos2, c, cLinePrev := "", arrfnc := {}
   LOCAL lUtf8 := oEdit:lUtf8, cQuotes := ['"], cFind := ['"{}], nLevel := 0

   FOR i := 1 TO nLineEnd
      cLine := AllTrim( arr[i] )
      IF i > 1
         // Checks if a line is commented with /* */ operators, using a hilight object
         IF oHili:IsComm( i-1 ) == 1
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
            IF nLevel == 0 .OR. nLevel == 1
               IF nPos == 1
                  _java_AddF( lUtf8, arrfnc, arr, i, cLinePrev, nLevel )
               ELSE
                  _java_AddF( lUtf8, arrfnc, arr, i, cLine, nLevel )
               ENDIF
            ENDIF
            nLevel ++
            //edi_writelog( '+ ' + ltrim(str(nlevel)) + ' ' + cLine )

         ELSEIF c == '}'
            nLevel --
            //edi_writelog( '- ' + ltrim(str(nlevel)) + ' ' + cLine )

         ENDIF
         nPos := cp_NextPos( lUtf8, cLine, nPos )
      ENDDO

      IF !Empty( cLine )
         cLinePrev := cLine
      ENDIF
   NEXT

   RETURN arrfnc

STATIC FUNCTION _java_AddF( lUtf8, arrfnc, arr, nLine, cLinePrev, nLevel )

   LOCAL j

   IF !( '(' $ cLinePrev ) .AND. !( cClass $ cLinePrev )
      FOR j := nLine-2 TO 1 STEP -1
         IF '(' $ arr[j] .OR. cClass $ arr[j]
            cLinePrev := AllTrim( arr[j] )
            nLine := j
            EXIT
         ENDIF
      NEXT
   ENDIF
   IF '(' $ cLinePrev .OR. cClass $ cLinePrev
      IF arrfnc != Nil
         Aadd( arrfnc, { Iif( nLevel>0, "  ", "" ) + cp_Left( lUtf8, cLinePrev, 64 ), Nil, nLine } )
      ENDIF
      RETURN nLine
   ENDIF

   RETURN 0

STATIC FUNCTION _java_Run( oEdit )

   LOCAL cTmpDir := hb_DirTemp(), cTmpJava := cTmpDir + "tmp_hbedit.java", cTmpScr
   LOCAL cFileRes, arr, i, cBuff

   IF Empty( arr := _java_Funcs( oEdit ) ) .OR. ( i := At( cClass, arr[1,1] ) ) == 0
      edi_Alert( "No one class found" )
      RETURN Nil
   ENDIF
   cBuff := hb_TokenPtr( arr[1,1], @i, " " )
   edi_writelog( cBuff )
   RETURN Nil

   hb_MemoWrit( cTmpJava, oEdit:ToString() )

   IF hb_version(20)
      cTmpScr := cTmpDir + "tmp_hbedit.sh"
      hb_MemoWrit( cTmpScr, "#!/bin/bash" + Chr(10) + ;
         "java " + cTmpJava + Chr(10) + "echo ''" + Chr(10) + 'read -n 1 -p "Press any key"' )
      __Run( "chmod a+x " + cTmpScr )
      IF hb_gtVersion() == "HWGUI"
         IF Empty( cTerm )
            arr := { "gnome-terminal", "x-terminal-emulator", "konsole", "xfce4-terminal" }
            cFileRes := cTmpDir + "tmp_hbedit.out"
            FOR i := 1 TO Len( arr )
               cedi_RunConsoleApp( "which " + arr[i], cFileRes )
               IF !Empty( cBuff := MemoRead( cFileRes ) )
                  cBuff := StrTran( cBuff, Chr(10), "" )
                  IF Substr( cBuff, Len(cBuff)-Len(arr[i])+1, Len(arr[i]) ) == arr[i]
                     cTerm := arr[i]
                     EXIT
                  ENDIF
               ENDIF
            NEXT
         ENDIF
         IF Empty( cTerm )
            edi_Alert( "Terminal program not found" )
         ELSE
            __Run( cTerm + " -e " + cTmpScr )
         ENDIF
      ELSE
         CLEAR SCREEN
         Devpos( 0,0 )
         __Run( "./tmp_hbedit.sh" )
      ENDIF
   ELSE
      cTmpScr := cTmpDir + "tmp_hbedit.bat"
      hb_MemoWrit( cTmpScr, + Chr(13) + Chr(10) + ;
         "java " + cTmpJava + Chr(13) + Chr(10) + "pause" )
      __Run( cTmpScr )
   ENDIF

   SetColor( oEdit:cColor )
   oEdit:WriteTopPane()
   oEdit:TextOut()

   edi_Alert( "Done!" )

   RETURN Nil

STATIC FUNCTION _java_AutoC( oEdit, cPrefix )

   LOCAL hTrieLang, hTrie, o := oEdit:oHili
   LOCAL arr, i, nPos, nLen, nPrefLen := Len( cPrefix )

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
   /*
   IF !Empty( arr := _java_KeyWords( oEdit, cPrefix, hTrieLang ) )
      FOR i := 1 TO Len( arr )
         IF ( nLen := Len( arr[i] ) ) >= 4 .AND. nLen > nPrefLen
            IF Empty( hTrie )
               hTrie := trie_Create( .T. )
            ENDIF
            trie_Add( hTrie, arr[i] )
         ENDIF
      NEXT
   ENDIF
   */
   RETURN hTrie

