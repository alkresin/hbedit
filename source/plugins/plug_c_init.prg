#define ALT_PRESSED   0x040000
#define K_ALT_A            286
#define K_ALT_L            294
#define K_ALT_R            275

#define COMP_ID              1
#define COMP_FAM             2
#define COMP_PATH            3
#define COMP_LIBS            4
#define COMP_ENV             5

STATIC cIniPath
STATIC lClass
STATIC aCompilers := {}, aDopOpt := {}
STATIC isCtags := Nil

FUNCTION Plug_c_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col(), lCpp := ( o:cSyntaxType == "cpp" )
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( Iif( lCpp,"C++","C" ) + " plugin:  Alt-L Functions list  Alt-R Run" + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab Autocompetion","" ) )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         IF oEdit:hCargo == Nil
            oEdit:hCargo := hb_hash()
         ENDIF
         oEdit:hCargo["help"] := Iif( _c_isCtags(), "", "*** Install Ctags for better sources parsing ***" + Chr(10) ) + ;
            Iif( lCpp,"C++","C" ) + " plugin hotkeys:" + Chr(10) + ;
            "  Alt-L  - Functions list" + Chr(10) + ;
            "  Alt-R  - Run" + Chr(10) + ;
            "  Alt-A  - Add to code a standard construction" + Chr(10) + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab - Autocompetion" + Chr(10),"" )
      ENDIF
      oEdit:hCargo["flist"] := Nil
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

   IF Empty( cIniPath )
      cIniPath := cPath
      _c_ReadIni_hwb()
      _c_ReadIni_lc()
   ENDIF
   oEdit:bStartEdit := bStartEdit
   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey
   oEdit:bAutoC := {|o,s| _c_AutoC(o,s)}

   RETURN Nil

STATIC FUNCTION _c_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cWord

   IF oEdit:lUpdated .AND. oEdit:hCargo["flist"] != Nil
      oEdit:hCargo["flist"] := Nil
   ENDIF
   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_L
         _c_Spis( oEdit )
         RETURN -1
      ELSEIF nKey == K_ALT_R
         _c_Run( oEdit )
         RETURN -1
      ELSEIF nKey == K_ALT_A
         _c_AddCode( oEdit )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _c_AddCode( oEdit )

   LOCAL aMenu := { "if() {}", "if() {} else {}", "while() {}", "do {} while()", ;
      "for() {}", "switch ()", "int main(void) {}" }
   LOCAL aCode := { e"if( ) {\r}\n", e"if( ) {\r} else {\r}\n", e"while( ) {\r}\n", e"do {\r} while( );\n", ;
      e"for( int i=1; i<x; i++ ) {\r}\n", e"switch( ) {\r   case :\r      break;\r}\n", "1" }
   LOCAL i, cCode, lCpp := ( oEdit:cSyntaxType == "cpp" )

   IF lCpp
      AAdd( aMenu, "class {}" )
      AAdd( aCode, e"class  {\rpublic:\r};\n" )
   ENDIF
   IF ( i := FMenu( oEdit, aMenu, oEdit:y1+2, oEdit:x1+4 ) ) == 0
      RETURN Nil
   ENDIF
   IF ( cCode := aCode[i] ) == "1"
      cCode := Iif( lCpp, e"#include <bits/stdc++.h>\r#include <iostream>\r", ;
         e"#include <stdio.h>\r" ) + e"\rint main( void ) {\r\r   return 0;\r}\n"
   ENDIF

   oEdit:InsText( oEdit:nLine, oEdit:nPos, StrTran( cCode, Chr(13), Chr(10)+Space(oEdit:nPos-1) ) )
   oEdit:TextOut()

   RETURN Nil

STATIC FUNCTION _c_Run( oEdit )

   LOCAL cSrcName := "hb_c_tmp", cFileOut := "$hb_compile_err", oNew, nRow := Row(), nCol := Col()
   LOCAL cTmpC, cTmpExe, cRes, aOpt, cComp, nCompiler := 0, sDopOpt := "", i, aEnv, aCompEnv

   aOpt := _c_GetParams( oEdit )
   DevPos( nRow, nCol )
   IF Empty( aOpt )
      RETURN Nil
   ENDIF

   cTmpC := hb_dirTemp() + cSrcName + Iif( aOpt[1] == 1, ".c", ".cpp" )
   nCompiler := aOpt[3]

   edi_CloseWindow( cFileOut )
#ifdef __PLATFORM__WINDOWS
   cTmpExe := hb_dirTemp() + cSrcName + ".exe"
#else
   cTmpExe := hb_dirTemp() + cSrcName
#endif
   hb_MemoWrit( cTmpC, oEdit:ToString() )

   IF Empty( nCompiler )
      aEnv := hb_ATokens( GetEnv( "PATH" ), hb_osPathListSeparator() )
      FOR i := 1 TO Len( aEnv )
#ifdef __PLATFORM__WINDOWS
         IF File( aEnv[i] + '\' + "bcc32.exe" )
            AAdd( aCompilers, { "bcc", "bcc", aEnv[i], "", {} } )
            nCompiler := Len( aCompilers )
         ELSEIF File( aEnv[i] + '\' + "gcc.exe" )
            AAdd( aCompilers, { "mingw", "mingw", aEnv[i], "", {} } )
            nCompiler := Len( aCompilers )
         ELSEIF File( aEnv[i] + '\' + "cl.exe" )
            AAdd( aCompilers, { "msvc", "msvc", aEnv[i], "", {} } )
            nCompiler := Len( aCompilers )
         ENDIF
#else
         IF File( aEnv[i] + '/' + "gcc" )
            AAdd( aCompilers, { "gcc", "gcc", aEnv[i], "", {} } )
            nCompiler := Len( aCompilers )
         ENDIF
#endif
      NEXT
   ENDIF
   IF Empty( nCompiler )
      edi_Alert( "No one C compiler found" )
      RETURN Nil
   ENDIF

   IF !Empty( aOpt[4] )
      FOR i := 1 TO Len( aOpt[4] )
         sDopOpt += " " + aDopOpt[ aOpt[4,i],2 ]
      NEXT
   ENDIF
   sDopOpt += " " + aOpt[2]
   FErase( cTmpExe )
   // Compiling
   //edi_Writelog( hb_valtoexp( acompilers ) )
   //edi_Writelog( str( nCompiler ) + " " + aCompilers[nCompiler,COMP_FAM] + " /" + sDopOpt + "/" )
#ifdef __PLATFORM__WINDOWS
   aCompEnv := aCompilers[nCompiler,COMP_ENV]
   IF !Empty( aCompEnv )
      aEnv := Array( Len(aCompEnv),2 )
      FOR i := 1 TO Len( aCompEnv )
         aEnv[i,1] := aCompEnv[i,1]
         aEnv[i,2] := getenv( aEnv[i,1] )
         hb_setenv( aCompEnv[i,1], aCompEnv[i,2] )
      NEXT
   ENDIF
   IF aCompilers[nCompiler,COMP_FAM] == "bcc"
      //edi_Writelog( "bcc32 -e" + cSrcName + " -n" + hb_dirTemp() + " " + cTmpC + sDopOpt )
      cRes := cRun( "bcc32.exe -e" + cSrcName + " -n" + hb_dirTemp() + " " + cTmpC + sDopOpt )
   ELSEIF aCompilers[nCompiler,COMP_FAM] == "mingw"
      sDopOpt += " -luser32 -lwinspool -lcomctl32 -lcomdlg32 -lgdi32 -lole32 -loleaut32 -luuid -lwinmm -lwsock32 -lws2_32 -liphlpapi"
      sDopOpt += " -lstdc++"
      //edi_writelog( "gcc.exe " + cTmpC + " -o" + cTmpExe + sDopOpt )
      cRes := cRun( "gcc.exe " + cTmpC + " -o" + cTmpExe + sDopOpt )
   ELSEIF aCompilers[nCompiler,COMP_FAM] == "msvc"
      cRes := cRun( "cl.exe " + cTmpC + " -o" + cTmpExe + sDopOpt )
   ENDIF
   IF !Empty( aCompEnv )
      FOR i := 1 TO Len( aEnv )
         hb_setenv( aEnv[i,1], aEnv[i,2] )
      NEXT
   ENDIF
#else
   cComp := Iif( hb_fnameExt(cTmpC) == ".cpp", "g++ ", "gcc " )
   cRes := cRun( cComp + cTmpC + " -o" + cTmpExe + sDopOpt + " 2>&1" )
#endif

   IF File( cTmpExe )
      hbc_Console( cTmpExe, .F. )
   ELSEIF !Empty( cRes )
      oNew := edi_AddWindow( oEdit, cRes, cFileOut, 2, 7 )
      oNew:lReadOnly := .T.
   ELSE
      edi_Alert( "Something goes wrong..." )
   ENDIF

   RETURN Nil

STATIC FUNCTION _c_Spis( oEdit )

   LOCAL i, n, arrfnc
   LOCAL oHili := oEdit:oHili

   IF oEdit:hCargo["flist"] == Nil
      IF _c_isCtags()
         arrfnc := _c_Tags( oEdit )
      ELSE
         lClass := .F.
         oHili:CheckComm()
         arrfnc := _c_Funcs( oEdit, oHili )
      ENDIF
      oEdit:hCargo["flist"] := arrfnc
   ELSE
      arrfnc := oEdit:hCargo["flist"]
   ENDIF
   IF Empty( arrfnc )
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

STATIC FUNCTION _c_Tags( oEdit )

   LOCAL arrfnc := {}, cBuff, cFileTmp
   LOCAL nSkip, nSkipLine := 0, cLine, cToken, cType, cNum

   IF oEdit:lUpdated
      cFileTmp := hb_dirTemp() + "hb_c_tmp" + Iif( oEdit:cSyntaxType == "cpp", ".cpp", ".c" )
      hb_MemoWrit( cFileTmp, oEdit:ToString() )
      cBuff := cRun( "ctags -x " + cFileTmp )
   ELSE
      cBuff := cRun( "ctags -x " + oEdit:cFileName )
   ENDIF
   DO WHILE !Empty( cLine := hb_TokenPtr( cBuff, @nSkipLine, Chr(10) ) )
      nSkip := 0
      cToken := hb_TokenPtr( cLine, @nSkip )
      cType := hb_TokenPtr( cLine, @nSkip )
      cNum := hb_TokenPtr( cLine, @nSkip )
      hb_TokenPtr( cLine, @nSkip )
      IF cType == "class" .OR. cType == "function" .OR. cType == "struct"
         AAdd( arrfnc, { Ltrim( Substr(cLine, nSkip) ), Nil, Val(cNum) } )
      ENDIF
   ENDDO
   IF Empty( arrfnc )
      RETURN Nil
   ENDIF

   RETURN ASort( arrfnc,,, {|a1,a2| a1[3]<a2[3] } )

STATIC FUNCTION _c_Funcs( oEdit, oHili, nLineEnd )

   LOCAL i, j, n, arr := oEdit:aText, cLine, nPos, nPos2, c, cLinePrev := "", arrfnc, lList := .F., nLast := 0
   LOCAL lUtf8 := oEdit:lUtf8, cQuotes := ['"], cFind := ['"{}], nLevel := 0

   IF Empty( nLineEnd )
      nLineEnd := Len( arr )
      lList := .T.
      arrfnc := {}
   ENDIF
   FOR i := 1 TO nLineEnd
      IF Empty( cLine := Alltrim( oHili:Getline(i) ) )
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
      IF edi_InQuo( oEdit, cLine, nPos ) == 0
         cLine := Trim( Left( cLine, nPos-1 ) )
         EXIT
      ENDIF
      nPos += 2
   ENDDO

   nPos := 1
   DO WHILE ( nPos := hb_At( "/*", cLine, nPos ) ) > 0
      IF edi_InQuo( oEdit, cLine, nPos ) == 0
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

   FOR i := 1 TO Len( aText )
      IF Empty( cLine := Ltrim( oHili:Getline(i) ) )
         LOOP
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
            cSecond := Substr( cLine, nSkip, nPos-nSkip )
            IF Left(cSecond,nPrefLen) == cPrefix .AND. hb_Ascan(aWords,cSecond,,,.T.) == 0 ;
               .AND. !trie_Exist( hTrieLang, cSecond )
               Aadd( aWords, cSecond + "(" )
            ENDIF
         ENDIF
         nPos ++
      ENDDO
   NEXT

   IF !Empty( nLine0 )
   ENDIF

   RETURN aWords

STATIC FUNCTION _c_isCtags()

   LOCAL cBuff

   IF isCtags == Nil
      isCtags := ( !Empty( cBuff := cRun( "ctags --version" ) ) .AND. "opyri" $ cBuff )
   ENDIF

   RETURN isCtags

STATIC FUNCTION _c_GetParams( oEdit )

   LOCAL cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets, y1, x1, x2, y2, i := 0, j, aOpt := { 1, "", 0, {} }, nGetOpt := 0
   LOCAL lCpp := ( oEdit:cSyntaxType == "cpp" )
   //LOCAL lc := ( Empty( oEdit:cFileName ) .OR. hb_fnameExt( oEdit:cFileName ) == ".c" )

   y1 := Int( MaxRow()/2 ) - 6
   x1 := Int( MaxCol()/2 ) - 16
   x2 := x1 + 32

   aGets := { {y1,x1+4, 11, "Parameters"}, ;
      { y1+1,x1+2, 11, "( ) C" }, { y1+1,x1+3,3,!lCpp,2,,,,"g1" }, ;
      { y1+1,x1+21, 11, "( ) Cpp" }, { y1+1,x1+22,3,lCpp,2,,,,"g1" }, ;
      { y1+2,x1+2, 11, "Options" }, ;
      { y1+3,x1+2, 0, "", x2-x1-4 } }

   IF Len( aCompilers ) > 1
      FOR i := 1 TO Len( aCompilers )
         AAdd( aGets, { y1+3+i,x1+2, 11, "( ) " + aCompilers[i,COMP_ID] } )
         AAdd( aGets, { y1+3+i, x1+3, 3, (i==1), 2,,,, "g2" } )
      NEXT
   ELSEIF Len( aCompilers ) == 1
      aOpt[3] := 1
   ENDIF
   y2 := y1 + 3 + i
   IF Len( aDopOpt ) > 0
      nGetOpt := Len( aGets ) + 1
      FOR i := 1 TO Len( aDopOpt )
         AAdd( aGets, { y2+i, x1+3, 1, .F., 1 } )
         AAdd( aGets, { y2+i,x1+2, 11, "[ ] " + aDopOpt[i,1] } )
      NEXT
      y2 += i
   ENDIF

   cBuf := Savescreen( y1, x1, y2, x2 )
   @ y1, x1, y2, x2 BOX "�Ŀ����� "

   //KEYBOARD Chr( K_DOWN ) + Chr( K_DOWN )
   edi_READ( aGets )
   IF LastKey() == 13
      aOpt[1] := Iif( aGets[3,4], 1, 2 )
      aOpt[2] := AllTrim( aGets[7,4] )
      IF Len( aCompilers ) > 1
         j := 0
         FOR i := 8 TO Len( aGets ) STEP 2
            j ++
            IF aGets[i+1,4]
               aOpt[3] := j
               EXIT
            ENDIF
         NEXT
      ENDIF
      IF Len( aDopOpt ) > 0
         j := 0
         FOR i := nGetOpt TO Len( aGets ) STEP 2
            j ++
            IF aGets[i,4]
               AAdd( aOpt[4], j )
            ENDIF
         NEXT
      ENDIF
   ELSE
      aOpt := Nil
   ENDIF
   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cBuf )

   RETURN aOpt

STATIC FUNCTION _c_ReadIni_hwb()

   LOCAL cPath, hIni, aIni, nSect, aSect, arr, key, cTmp, nPos

   IF File( cPath := ( cIniPath + "hwbuild.ini" ) )
      hIni := edi_IniRead( cPath )
   ENDIF
   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Left( aIni[nSect], 10 ) == "C_COMPILER" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            arr := hb_hKeys( aSect )
            AAdd( aCompilers, { "", "", "", "", {} } )
            FOR EACH key IN arr
               IF key == "id" .AND. !Empty( cTmp := aSect[ key ] )
                  ATail(aCompilers)[COMP_ID] := cTmp
               ELSEIF key == "family" .AND. !Empty( cTmp := aSect[ key ] )
                  ATail(aCompilers)[COMP_FAM] := cTmp
               ELSEIF key == "bin_path" .AND. !Empty( cTmp := aSect[ key ] )
                  ATail(aCompilers)[COMP_PATH] := cTmp
               ELSEIF key == "def_syslibs" .AND. !Empty( cTmp := aSect[ key ] )
                  ATail(aCompilers)[COMP_LIBS] := cTmp
               ELSEIF Left(key,4) == "env_" .AND. !Empty( cTmp := aSect[ key ] )
                  IF ( nPos := At( '=', cTmp ) ) > 0
                     AAdd( ATail(aCompilers)[COMP_ENV], {Left( cTmp,nPos-1 ), Substr( cTmp,nPos+1 )} )
                  ENDIF
               ENDIF
            NEXT
            IF Empty( ATail(aCompilers)[COMP_FAM] )
               ATail(aCompilers)[COMP_FAM] := ATail(aCompilers)[COMP_ID]
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

STATIC FUNCTION _c_ReadIni_lc()

   LOCAL cPath, hIni, aIni, nSect, aSect, arr, key, cTmp, nPos

   IF File( cPath := ( cIniPath + "lang_c.ini" ) )
      hIni := edi_IniRead( cPath )
   ENDIF
   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := hb_hKeys( aSect )
               FOR EACH key IN arr
                  IF Left(key,4) == "opt_" .AND. !Empty( cTmp := aSect[ key ] )
                     IF ( nPos := At( '=', cTmp ) ) > 0
                        AAdd( aDopOpt, {Left( cTmp,nPos-1 ), Substr( cTmp,nPos+1 )} )
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil