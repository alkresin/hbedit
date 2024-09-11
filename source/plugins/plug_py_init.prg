#define ALT_PRESSED   0x040000
#define CTRL_PRESSED  0x020000
#define K_ALT_R    275
#define K_ALT_L    294
#define K_ENTER     13
#define K_ESC       27

STATIC cIniPath
STATIC cCompiler
STATIC lUnix := .F.

FUNCTION Plug_py_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "Python plugin: Alt-L Functions list  Alt-R - Run" + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab Autocompetion","" ) )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         IF oEdit:hCargo == Nil
            oEdit:hCargo := hb_hash()
         ENDIF
         oEdit:hCargo["help"] := "Python plugin hotkeys:" + Chr(10) + ;
            "  Alt-L  - Functions list" + Chr(10) + ;
            "  Alt-R  - Run" + Chr(10) + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab - Autocompetion" + Chr(10),"" )
      ENDIF
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _py_Init_OnKey(o,n)
      IF bOnKeyOrig != Nil .AND. nRes >= 0
         nRes := Eval( bOnKeyOrig, o, Iif( nRes==0, n, nRes ) )
      ENDIF
      RETURN nRes
   }

   cIniPath := cPath
   lUnix := hb_version(20)
   oEdit:bStartEdit := bStartEdit
   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey
   oEdit:bAutoC := {|o,s| _py_AutoC(o,s)}

   RETURN Nil

STATIC FUNCTION _py_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cWord

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_L
         _py_Spis( oEdit )
         RETURN -1
      ELSEIF nKey == K_ALT_R
         _py_Run( oEdit )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _py_Spis( oEdit )

   LOCAL i, n, arr := oEdit:aText, cLine, cfirst, nSkip, arrfnc := {}

   FOR i := 1 TO Len( arr )
      cLine := Lower( Ltrim( arr[i] ) )
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF cfirst == "class" .OR. cFirst == "def"
         Aadd( arrfnc, { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i } )
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

STATIC FUNCTION _py_Run( oEdit )

   LOCAL cComp := "python", cComp2, i, aEnv, cCmd, cTempFile, cSep := hb_ps()
   LOCAL bufsc, nScreenH, nScreenW

   IF Empty( cCompiler )
      IF !lUnix
         cComp += ".exe"
      ELSE
         cComp2 := "python3"
      ENDIF
      aEnv := hb_ATokens( GetEnv( "PATH" ), hb_osPathListSeparator() )
      FOR i := 1 TO Len( aEnv )
         IF File( aEnv[i] + cSep + cComp )
            cCompiler := cComp
            EXIT
         ELSEIF !Empty(cComp2) .AND. File( aEnv[i] + cSep + cComp2 )
            cCompiler := cComp2
            EXIT
         ENDIF
      NEXT
   ENDIF

   IF !Empty( cCompiler )
      cTempFile := hb_DirTemp() + "hb_py_tmp.py"
      hb_MemoWrit( cTempFile, oEdit:ToString() )
      cCmd := cCompiler + " " + cTempFile
      nScreenH := FilePane():vy2 + 1
      nScreenW := FilePane():vx2 + 1
      bufsc := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )
      hbc_Console( cCmd,, .F. )
      Restscreen( 0, 0, nScreenH-1, nScreenW-1, bufsc )
   ENDIF

   RETURN Nil

STATIC FUNCTION _py_AutoC( oEdit, cPrefix )

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