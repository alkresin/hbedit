#define ALT_PRESSED   0x040000
#define CTRL_PRESSED  0x020000
#define K_LDBLCLK 1006
#define K_ALT_A    286
#define K_ALT_D    288
#define K_ALT_I    279
#define K_ALT_L    294
#define K_ALT_H    291
#define K_ALT_V    303
#define K_ENTER     13
#define K_ESC       27
#define K_DOWN      24
#define K_CTRL_RIGHT 2

STATIC cIniPath
STATIC lIsCurl := .F., cServAddr
STATIC lDescri := .T., lSources := .F., lChglog := .F., lSamples := .F., lRu := .F.
STATIC aHwgFuncs, aHbFuncs
STATIC aHbShort

FUNCTION Plug_prg_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "Harbour plugin:  Alt-D Dictionary  Alt-I Info" + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab Autocompetion","" ) )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         IF oEdit:hCargo == Nil
            oEdit:hCargo := hb_hash()
         ENDIF
         oEdit:hCargo["help"] := "Harbour plugin hotkeys:" + Chr(10) + ;
            "  Alt-D  - Dictionary (Harbour and HwGUI functions list)" + Chr(10) + ;
            "  Alt-I  - Get info about a function under cursor" + Chr(10) + ;
            "  Alt-L  - Functions list" + Chr(10) + ;
            "  Alt-V  - Current function structure" + Chr(10) + ;
            "  Alt-A  - Add to code a standard construction" + Chr(10) + ;
            "  Alt-H  - Build with hwbc" + Chr(10) + ;
            "  Ctrl-B - Go to a matched keyword (IF...ENDIF, etc.)" + Chr(10) + ;
            Iif( hb_hGetDef(TEdit():options,"autocomplete",.F.),"  Tab - Autocompetion" + Chr(10),"" )
      ENDIF
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _prg_Init_OnKey(o,n)
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
   oEdit:bAutoC := {|o,s| _prg_AutoC(o,s)}

   RETURN Nil

STATIC FUNCTION _prg_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cWord

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_D
         _f_dict( oEdit )
         DevPos( nRow, nCol )
         oEdit:TextOut()
         RETURN -1
      ELSEIF nKey == K_ALT_I
         edi_SelectW( oEdit )
         cWord := cp_Substr( oEdit:lUtf8, oEdit:aText[oEdit:nLine], oEdit:nbx1, oEdit:nbx2-oEdit:nbx1 )
         _GetFuncInfo( oEdit, cWord )
         DevPos( nRow, nCol )
         oEdit:TextOut()
         RETURN -1
      ELSEIF nKey == K_ALT_L
         _prg_Spis( oEdit )
         RETURN -1
      ELSEIF nKey == K_ALT_V
         _prg_FuncStru( oEdit )
         RETURN -1
      ELSEIF nKey == K_ALT_H
         _prg_Init_Build( oEdit )
         RETURN -1
      ELSEIF nKey == K_ALT_A
         _prg_AddCode( oEdit )
         RETURN -1
      ENDIF
   ELSEIF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      IF nKey == K_CTRL_RIGHT .AND. hb_keyVal( nKeyExt ) == 66 // Ctrl-B
         RETURN _prg_GoMatched( oEdit )
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _prg_GetFuncList( oEdit, lFuncStru )

   LOCAL arrfnc := {}, nCurr := oEdit:nLine, aNew, nEnd := Len( oEdit:aText )
   LOCAL i, n, arr := oEdit:aText, cLine, cfirst, cSecond, nSkip, lClassDef := .F.
   LOCAL a4Stru := { "if", "elseif", "else", "endif", "end", "do", "enddo", ;
      "for", "next", "switch", "case", "otherwise", "endswitch", "with", "endwith" }
   LOCAL oHili := oEdit:oHili

   oHili:CheckComm()
   FOR i := 1 TO Len( arr )
      IF Empty( cLine := Lower(Ltrim( oHili:Getline(i) )) )
         LOOP
      ENDIF
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF cfirst == "function" .OR. cfirst == "procedure" .OR. ;
            ( cfirst == "method" .AND. (!lClassDef .OR. " inline " $ cLine .OR. " block " $ cLine )) ;
            .OR. cfirst == "func" .OR. cfirst == "proc" .OR. ( cfirst == "static" .AND. ;
            ( ( cSecond := hb_TokenPtr( cLine, @nSkip ) ) == "function" .OR. ;
            cSecond == "procedure" .OR. cSecond == "func" .OR. cSecond == "proc" ) )
         aNew := { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i }
         IF nCurr < i .AND. lFuncStru
            nEnd := i - 1
            EXIT
         ELSEIF lFuncStru
            IF Empty( arrfnc )
               Aadd( arrfnc, aNew )
            ELSE
               arrfnc[1] := aNew
            ENDIF
         ELSE
            Aadd( arrfnc, aNew )
         ENDIF
      ELSEIF cfirst == "class" .or. ( cfirst == "create" .AND. ;
            ( cSecond := hb_TokenPtr( cLine, @nSkip ) ) == "class" )
         IF cfirst == "create" .OR. ( !( ( cSecond := hb_TokenPtr( cLine, @nSkip ) ) == "var" ) ;
               .AND. !( cSecond == "data" ) )
            lClassDef := .T.
            aNew := { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i }
            IF nCurr < i .AND. lFuncStru
               nEnd := i - 1
               EXIT
            ELSEIF !lFuncStru
               Aadd( arrfnc, aNew )
            ENDIF
         ENDIF
      ELSEIF cfirst == "end" .or. cfirst == "endclass"
         lClassDef := .F.
      ENDIF
   NEXT
   IF lFuncStru .AND. !Empty( arrfnc )
      FOR i := arrfnc[1,3] TO nEnd
         IF Empty( cLine := Lower(Ltrim( oHili:Getline(i) )) )
            LOOP
         ENDIF
         nSkip := 0
         cfirst := hb_TokenPtr( cLine, @nSkip )
         IF hb_Ascan( a4Stru, cfirst,,, .T. ) > 0
            AAdd( arrfnc, { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i } )
         ENDIF
      NEXT
   ENDIF

   RETURN arrfnc

STATIC FUNCTION _prg_Spis( oEdit )

   LOCAL i, n, arrfnc
   LOCAL lSorted := .F., lToLoop := .F.
   LOCAL bKeys := {|nKeyExt,nRow|
      LOCAL nn
      IF nKeyExt == 0x41000009  // F9
         nn := arrfnc[nRow,3]
         IF lSorted
            ASort( arrfnc,,, {|a1,a2| a1[3] < a2[3] } )
         ELSE
            ASort( arrfnc,,, {|a1,a2| a1[1] < a2[1] } )
         ENDIF
         n := Ascan2( arrfnc, nn, 3 )
         lSorted := !lSorted
         lToLoop := .T.
         RETURN .F.
      ENDIF
      RETURN .T.
   }

   arrfnc := _prg_GetFuncList( oEdit, .F. )

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
      DO WHILE .T.
         IF ( i := FMenu( oEdit, arrfnc, 2, 6,,,,, n, (Len(arrfnc)>3),,, bKeys, ;
            " Functions list  F9 - " + Iif(lSorted,"Natural order ","Sort by name ") ) ) > 0 .OR. lToLoop
            IF !lToLoop
               oEdit:Goto( arrfnc[i,3] )
               EXIT
            ENDIF
            lToLoop := .F.
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN Nil

STATIC FUNCTION _prg_FuncStru( oEdit )

   LOCAL i, n, arrfnc, arrcopy, nIndentMin := 999
   LOCAL lToLoop := .F., lTops := .F.
   LOCAL bKeys := {|nKeyExt,nRow|
      LOCAL nn, nInd
      IF nKeyExt == 0x41000009  // F9
         nn := arrfnc[nRow,3]
         IF lTops
            arrfnc := AClone( arrcopy )
         ELSE
            IF Empty( arrcopy )
               arrcopy := AClone( arrfnc )
               FOR i := 2 TO Len(arrfnc)
                  IF (nInd := (Len(arrfnc[i,1]) - Len(Ltrim(arrfnc[i,1])))) < nIndentMin
                     nIndentMin := nInd
                  ENDIF
               NEXT
            ENDIF
            FOR i := Len(arrfnc) TO 1 STEP -1
               IF Len(arrfnc[i,1]) - Len(Ltrim(arrfnc[i,1])) > nIndentMin
                  hb_ADel( arrfnc, i, .T. )
               ENDIF
            NEXT
         ENDIF
         nInd := oEdit:nLine
         FOR i := 1 TO Len(arrfnc)
            IF arrfnc[i,3] > nInd
               n := i - 1
               EXIT
            ENDIF
         NEXT
         lTops := !lTops
         lToLoop := .T.
         RETURN .F.
      ENDIF
      RETURN .T.
   }

   arrfnc := _prg_GetFuncList( oEdit, .T. )

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
      DO WHILE .T.
         IF ( i := FMenu( oEdit, arrfnc, 2, 6,,,,, n, (Len(arrfnc)>3),,, bKeys, ;
            " Function structure  F9 - " + Iif(lTops, "All ", "Tops only ") ) ) > 0 .OR. lToLoop
            IF !lToLoop
               oEdit:Goto( arrfnc[i,3] )
               EXIT
            ENDIF
            lToLoop := .F.
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN Nil

STATIC FUNCTION _prg_AddCode( oEdit )

   LOCAL aMenu := { "IF ... ENDIF", "IF ... ELSE ...", "DO ... ENDDO", ;
      "FOR ... NEXT", "FOR EACH ... NEXT", "SWITCH ... CASE ...", ;
      "FUNC ... RETURN", "STATIC FUNC ... RETURN", "CLASS ... ENDCLASS" }, i
   LOCAL aCode := { e"IF\rENDIF\n", e"IF\rELSE\rENDIF\n", e"DO WHILE\rENDDO\n", ;
      e"FOR i := 1 TO\rNEXT\n", e"FOR EACH x IN\rNEXT\n", e"SWITCH\rCASE\rENDCASE\n", ;
      e"FUNCTION\r   RETURN Nil\n", e"STATIC FUNCTION\r   RETURN Nil\n", e"CLASS\r   DATA\r   METHOD\rENDCLASS" }

   IF ( i := FMenu( oEdit, aMenu, oEdit:y1+2, oEdit:x1+4 ) ) == 0
      RETURN Nil
   ENDIF

   oEdit:InsText( oEdit:nLine, oEdit:nPos, StrTran( aCode[i], Chr(13), Chr(10)+Space(oEdit:nPos-1) ) )
   oEdit:TextOut()

   RETURN Nil

STATIC FUNCTION _prg_Init_Build( oEdit )

   LOCAL cBuff, oNew, i, cDop, cAddW := "$hb_compile_err"
   LOCAL cPathBase := hb_fnameDir( oEdit:cFileName ), cCurrDir := Curdir()

   edi_CloseWindow( cAddW )

   oEdit:Save()
   IF ( cDop := _GetParams() ) == Nil
      RETURN Nil
   ENDIF

   SetColor( oEdit:cColorSel )
   @ 10, Int(MaxCol()/2)-4 SAY " Wait... "
   DirChange( cPathBase )
   cedi_RunConsoleApp( "hwbc " + cDop + " " + oEdit:cFileName,, @cBuff )
   DirChange( cCurrDir )
   SetColor( oEdit:cColor )

   IF Empty( cBuff )
      edi_Alert( "hwbc" + Iif( hb_version(20), "", ".exe" ) + " isn't found" )
   ELSE
      oNew := edi_AddWindow( oEdit, cBuff, cAddW, 2, Int( (oEdit:y2-oEdit:y1)/3 ) )
      oNew:lReadOnly := .T.
      oNew:bOnKey := {|o,n| _prg_ErrWin_OnKey(o,n) }
   ENDIF

   oEdit:TextOut()

   RETURN Nil

STATIC FUNCTION _GetParams()

   LOCAL xRes := "", cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets, y1, x1, x2

   y1 := Int( MaxRow()/2 ) - 1
   x1 := Int( MaxCol()/2 ) - 20
   x2 := x1 + 40

   aGets := { {y1,x1+4, 11, "Parameters"}, ;
      { y1+1,x1+2, 11, "[ ] Short output" }, { y1+1,x1+3, 1, .T., 2 }, ;
      { y1+2,x1+2, 11, "-bcc -mingw -msvc -comp=... -{...}" }, ;
      { y1+3,x1+2, 0, "", x2-x1-4 } }

   cBuf := Savescreen( y1, x1, y1 + 4, x2 )
   @ y1, x1, y1 + 4, x2 BOX "ÚÄ¿³ÙÄÀ³ "

   KEYBOARD Chr( K_DOWN )
   edi_READ( aGets )
   IF LastKey() == 13
      xRes := AllTrim( aGets[5,4] )
      IF aGets[3,4]
         xRes := Iif( Empty(xRes), "-q", xRes + " -q" )
      ENDIF
   ELSE
      xRes := Nil
   ENDIF
   SetColor( oldc )
   Restscreen( y1, x1, y1 + 4, x2, cBuf )

   RETURN xRes

STATIC FUNCTION _prg_ErrWin_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nRow, s, nPos, nLine

   IF nKey == K_ENTER .OR. nKey == K_LDBLCLK
      IF nKey == K_LDBLCLK
         nRow := MRow()
         IF nRow < oEdit:y1 .OR. nRow > oEdit:y2
            RETURN 0
         ENDIF
      ELSE
         nRow := Row()
      ENDIF
      s := Lower( oEdit:aText[ oEdit:RowToLine( nRow ) ] )
      IF ( nPos := At( " error ", s ) ) > 0 .OR. ( nPos := At( " warning ", s ) ) > 0
         s := AllTrim( Left( s, nPos ) )
         IF Right( s, 1 ) == ")" .AND. ( nPos := Rat( "(",s ) ) > 0
            nLine := Val( Substr( s,nPos+1 ) )
            oEdit:oParent:GoTo( nLine, 1,, .T. )
            oEdit:lShow := .F.
            oEdit:nCurr := Ascan( oEdit:aWindows, {|o|o==oEdit:oParent} )
         ENDIF
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _prg_GoMatched( oEdit )

   LOCAL arr1 := { "IF", "WHILE", "DO WHILE", "FOR", "SWITCH" }
   LOCAL arr2 := { "ENDIF", "ENDDO", "ENDDO", "NEXT", "ENDSWITCH" }
   LOCAL i, j, n, s, i1, n1, n2, i2, nLev := 1

   n := n1 := oEdit:nLine
   i := edi_PrevWord( oEdit, .F., .F.,,, oEdit:nPos+1 )
   j := edi_NextWord( oEdit, .F., .T., .F.,, oEdit:nPos-1 )
   s := cp_Substr( oEdit:lUtf8, oEdit:aText[n], i, j-i+1 )
   n2 := n1
   i2 := i1 := oEdit:nPos
   IF ( j := Ascan( arr1, Upper( s ) ) ) > 0
      i := i1 := oEdit:nPos
      DO WHILE oEdit:Search( arr2[j], .F., .T., .T., .F., @n, @i )
         //edi_writelog( "1> "+ltrim(str(n))+" "+ltrim(str(i)) )
         IF edi_InQuo( oEdit, oEdit:aText[n], i ) == 0 .AND. ;
            !_prg_IsCommented( oEdit, n, i )
            nLev --
            //edi_writelog( "1a> "+ltrim(str(nLev)) )
            DO WHILE oEdit:Search( s, .F., .T., .T., .F., @n1, @i1 ) .AND. ;
                  ( n1 < n .OR. (n1 == n) .AND. i1 < i )
               //edi_writelog( "2> "+ltrim(str(n1))+" "+ltrim(str(i1)) )
               IF edi_InQuo( oEdit, oEdit:aText[n1], i1 ) == 0 .AND. ;
                  !_prg_IsCommented( oEdit, n1, i1 )
                  nLev ++
                  //edi_writelog( "2a> "+ltrim(str(nLev)) )
               ENDIF
               i1 ++
               n2 := n1
               i2 := i1
            ENDDO
            n1 := n2
            i1 := i2
            i1 ++
            //edi_writelog( "3> "+ltrim(str(nLev))+" "+ltrim(str(i1)) )
            IF nLev == 0
               oEdit:GoTo( n, i, 0 )
               RETURN -1
            ENDIF
         ENDIF
      ENDDO
      RETURN -1
   ELSEIF ( j := Ascan( arr2, Upper( s ) ) ) > 0
      i --
      i1 := i
      DO WHILE oEdit:Search( arr1[j], .F., .F., .T., .F., @n, @i )
         IF edi_InQuo( oEdit, oEdit:aText[n], i ) == 0 .AND. ;
            !_prg_IsCommented( oEdit, n, i )
            nLev --
            DO WHILE oEdit:Search( s, .F., .F., .T., .F., @n1, @i1 ) .AND. ;
                  ( n1 > n .OR. (n1 == n) .AND. i1 > i )
               IF edi_InQuo( oEdit, oEdit:aText[n1], i1 ) == 0 .AND. ;
                  !_prg_IsCommented( oEdit, n1, i1 )
                  nLev ++
               ENDIF
               i1 --
               n2 := n1
               i2 := i1
            ENDDO
            n1 := n2
            i1 := i2
            i1 --
            IF nLev == 0
               oEdit:GoTo( n, i, 0 )
               RETURN -1
            ENDIF
         ENDIF
      ENDDO
      RETURN -1
   ELSE
      RETURN 0
   ENDIF

   RETURN 0

STATIC FUNCTION _prg_IsCommented( oEdit, nLine, nPos )

   LOCAL nCol := oEdit:PosToCol( nLine, nPos ), s, n
   LOCAL lUtf8 := oEdit:lUtf8, lRes := .F.

   oEdit:lUtf8 := .F.
   s := oEdit:aText[nLine]

   IF nLine > 1
      // Check if a line is commented with /* */ operators, using a hilight object
      IF oEdit:oHili:IsComm( nLine-1 ) == 1
         IF ( n := ( At( "*/", s ) ) ) == 0 .OR. n > nCol
            lRes := .T.
         ENDIF
      ENDIF
   ENDIF

   IF !lRes
      n := nCol
      DO WHILE ( n := hb_Rat( '//', s,, n ) ) > 0
         IF edi_InQuo( oEdit, s, n ) == 0
            lRes := .T.
            EXIT
         ENDIF
         n --
      ENDDO

      IF !lRes
         n := nCol
         DO WHILE ( n := hb_Rat( '/*', s,, n ) ) > 0
            IF edi_InQuo( oEdit, s, n ) == 0 .AND. hb_Rat( '*/', s, n+2, nCol ) == 0
               lRes := .T.
               EXIT
            ENDIF
            n --
         ENDDO
      ENDIF
   ENDIF

   oEdit:lUtf8 := lUtf8

   RETURN lRes

STATIC FUNCTION _f_get_dict( n )

   LOCAL aFiles := { "hb_funcs.txt", "hwg_funcs.txt" }, cEol := Chr(10)
   LOCAL aDicts := { aHbFuncs, aHwgFuncs }, arr := aDicts[n], i, j, nPos

   IF Empty( arr )
      IF !Empty( arr := MemoRead( cIniPath + aFiles[n] ) )
         IF ( j := At( cEol, arr ) ) > 1 .AND. Substr( arr, j-1, 1 ) == Chr(13)
            cEol := Chr(13) + cEol
         ENDIF
         arr := hb_ATokens( arr, cEol )
         IF n == 1
            aHbFuncs := arr
            aHbShort := Array( Len(arr) )
            FOR i := 1 TO Len( arr )
               IF ( nPos := At( '(', arr[i] ) ) > 0
                  aHbShort[i] := Lower( Left( arr[i],nPos-1 ) )
               ELSE
                  aHbShort[i] := arr[i]
               ENDIF
            NEXT
         ELSEIF n == 2
            aHwgFuncs := arr
         ENDIF
      ELSE
         edi_Alert( aFiles[n] + " not found..." )
      ENDIF
   ENDIF

   RETURN arr

STATIC FUNCTION _f_dict( oEdit )

   LOCAL aMenu := { "Harbour functions", "HwGUI functions" }, i, j
   LOCAL arrfuncs, nCol := Col(), nRow := Row()

   i := FMenu( oEdit, aMenu, 2, 6 )
   IF i == 1 .OR. i == 2
      arrfuncs := _f_get_dict( i )
      IF !Empty( arrfuncs )
         IF ( j := FMenu( oEdit, arrfuncs, 2, 6,,,,,, .T. ) ) > 0
            edi_2cb( oEdit,, arrfuncs[j] )
            DevPos( nRow, nCol )
            oEdit:TextOut()
            _GetFuncInfo( oEdit, arrfuncs[j], i )
         ENDIF
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _GetFuncInfo( oEdit, sFunc, nDict )

   LOCAL aGets, nPos, arrf
   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu ), nRes
   LOCAL cFileRes := hb_DirTemp() + "hbedit_curl.out", cBuff, cAddW := "$FuncInfo", o

   IF ( nPos := At( '(', sFunc ) ) > 0
      sFunc := AllTrim( Left( sFunc, nPos-1 ) )
   ENDIF
   IF Empty( sFunc )
      RETURN Nil
   ENDIF

   IF nDict == Nil
      IF Left( sFunc,4 ) == "hwg_"
         nDict := 2
      ELSE
         nDict := 1
         IF Empty( arrf := _f_get_dict( nDict ) )
            RETURN Nil
         ENDIF
         cBuff := Lower( sFunc )
         IF Ascan( aHbShort, {|s|s==cBuff} ) == 0
           edi_Alert( sFunc + " isn't found in a dictionary" )
           RETURN Nil
         ENDIF
      ENDIF
   ENDIF
   aGets := { {10,22,0,Lower( sFunc ),32}, ;
      {11,23,1,lDescri,1}, {11,40,1,lSources,1}, {12,23,1,lChglog,1} }
   IF nDict == 2
      Aadd( aGets, {12,40,1,lSamples,1} )
      Aadd( aGets, {13,23,1,lRu,1} )
   ENDIF
   Aadd( aGets, {15,28,2,"[Info]",4,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}} )
   Aadd( aGets, {15,42,2,"[Cancel]",10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} )

   hb_cdpSelect( "RU866" )
   @ 09, 20, 16, 58 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 14, 20 SAY "Ã"
   @ 14, 58 SAY "´"
   @ 14, 21 TO 14, 57
   hb_cdpSelect( oEdit:cp )

   @ 11, 22 SAY "[ ] Description  [ ] Sources"
   @ 12, 22 SAY "[ ] Changelog"
   IF nDict == 2
      @ 12, 39 SAY "[ ] Samples"
      @ 13, 22 SAY "[ ] Russian language"
   ENDIF

   IF ( nRes := edi_READ( aGets ) ) == 0 .OR. nRes == Len(aGets)
      RETURN Nil
   ENDIF
   sFunc := Lower( aGets[1,4] )

   IF !lIsCurl
      FErase( cFileRes )
      cedi_RunConsoleApp( "curl --version", cFileRes )
      IF !Empty( cBuff := MemoRead( cFileRes ) ) .AND. "libcurl" $ cBuff
         lIsCurl := .T.
      ELSE
         edi_Alert( "Curl must be installed to use this plugin; curl executable should be in PATH" )
         RETURN Nil
      ENDIF
   ENDIF

   @ 10, Int(MaxCol()/2)-4 SAY " Wait... " COLOR oEdit:cColorSel
   IF Empty( cServAddr )
      FErase( cFileRes )
      cedi_RunConsoleApp( "curl www.kresin.ru/php/getad.php -s -o" + cFileRes )
      IF Empty( cBuff := MemoRead( cFileRes ) ) .OR. Left( cBuff, 5 ) != "addr:"
         edi_Alert( "Error" )
         RETURN Nil
      ENDIF
      cServAddr := Substr( cBuff, 6 )
   ENDIF

   sFunc := "?f=" + sFunc + "&" + "o="
   IF ( lDescri := aGets[2,4] )
      sFunc += "d"
   ENDIF
   IF ( lSources := aGets[3,4] )
      sFunc += "s"
   ENDIF
   IF ( lChglog := aGets[4,4] )
      sFunc += "c"
   ENDIF
   IF nDict == 1
      sFunc += "&d=hb0"
   ELSEIF nDict == 2
      IF ( lSamples := aGets[5,4] )
         sFunc += "t"
      ENDIF
      IF ( lRu := aGets[6,4] )
         sFunc += "ru"
      ENDIF
   ENDIF

   FErase( cFileRes )
   cedi_RunConsoleApp( 'curl "' + cServAddr + sFunc + '" -s -o' + cFileRes )
   IF Empty( cBuff := MemoRead( cFileRes ) )
      edi_Alert( "Error" )
      RETURN Nil
   ENDIF

   IF ( nPos := Ascan( oEdit:aWindows, {|o|o:cFileName==cAddW} ) ) > 0
      o := oEdit:aWindows[nPos]
      o:SetText( cBuff, cAddW )
      mnu_ToBuf( oEdit, nPos )
   ELSE
      edi_AddWindow( oEdit, cBuff, cAddW, 2, 10, "UTF8" )
   ENDIF

   RETURN Nil

STATIC FUNCTION _prg_AutoC( oEdit, cPrefix )

   LOCAL hTrieLang, hTrie
   LOCAL arr := { "STATIC", "MEMVAR", "PRIVATE", "PUBLIC", "CONTINUE", "SWITCH", "FUNCTION", ;
      "RETURN", "ENDIF", "ENDDO", "ENDCASE", "ENDSWITCH", "OTHERWISE", "DO WHILE", ;
      "FIELD", "REQUEST", "#define", "#ifdef", "#ifndef", "#else", "#endif", "#include" }
   LOCAL i, nPos, nLen, nPrefLen := Len( cPrefix )

   IF Empty( hTrieLang := hb_hGetDef( oEdit:oHili:hHili, "htrie", Nil ) )
      hTrieLang := oEdit:oHili:hHili["htrie"] := trie_Create( .F. )
      FOR i := 1 TO Len( arr )
         trie_Add( hTrieLang, arr[i] )
      NEXT

      arr := _f_get_dict( 1 )
      FOR i := 1 TO Len( arr )
         IF ( nPos := At( "(", arr[i] ) ) > 0
            trie_Add( hTrieLang, Left( arr[i], nPos ) )
         ENDIF
      NEXT
   ENDIF

   IF !Empty( arr := _prg_KeyWords( oEdit, Lower( cPrefix ), hTrieLang ) )
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

STATIC FUNCTION _prg_AddVars( oEdit, cLine, cPrefix, nSkip, arr, lLocal )

   LOCAL lComm := .F., cWord, nPos, nPos2, nPrefLen := Len( cPrefix )
   LOCAL b1 := "([{", b2 := ")]}", c1, c2, i, np

   IF lLocal
      //edi_Alert( "1>"+cLine )
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
      //edi_Alert( "2>"+cLine )
   ENDIF

   DO WHILE !lComm .AND. !Empty( cWord := AllTrim( hb_TokenPtr( cLine, @nSkip, ',', .T. ) ) )
      IF ( nPos := At( "//", cWord ) ) > 0 .OR. ( nPos := At( "/*", cWord ) ) > 0
         cWord := Trim( Left( cWord, nPos - 1 ) )
         lComm := .T.
      ENDIF
      IF Lower( Left(cWord,nPrefLen) ) == cPrefix
         IF ( nPos := At( ":", cWord ) ) > 0
            cWord := Trim( Left( cWord, nPos-1 ) )
         ENDIF
         Aadd( arr, cWord )
      ENDIF
   ENDDO

   RETURN Nil

/*
 * _prg_KeyWords( oEdit, cPrefix )
 * Scans the text to find keywords to be included in a list for autocompetion
 */
STATIC FUNCTION _prg_KeyWords( oEdit, cPrefix, hTrieLang )

   LOCAL i, nPos, c, aText := oEdit:aText, cLine, cfirst, cSecond, nSkip, aWords := {}
   LOCAL lGlob := .T., lClassDef := .F., nPrefLen := Len( cPrefix ), nLine0, nLineCurr := oEdit:nLine
   LOCAL oHili := oEdit:oHili

   FOR i := 1 TO Len( aText )
      cLine := Ltrim( aText[i] )
      IF i > 1
         // Check if a line is commented with /* */ operators, using a hilight object
         IF oHili:IsComm( i-1 ) == 1
            IF ( nPos := At( "*/", cLine ) ) > 0
               cLine := Ltrim( Substr( cLine,nPos+2 ) )
            ELSE
               LOOP
            ENDIF
         ENDIF
      ENDIF
      nSkip := 0
      cfirst := Lower( hb_TokenPtr( cLine, @nSkip ) )
      IF i < nLineCurr
         IF cfirst == "#define"
            IF Lower( Left( cSecond := hb_TokenPtr( cLine, @nSkip ), nPrefLen) ) == cPrefix
               Aadd( aWords, cSecond )
            ENDIF
            LOOP
         ENDIF
         IF lGlob
            IF ( cfirst == "static" .AND. ;
               !( ( cSecond := Lower( hb_TokenPtr( cLine, nSkip ) ) ) == "function" .OR. ;
               cSecond == "procedure" .OR. cSecond == "func" .OR. cSecond == "proc" ) ) .OR. ;
               cfirst == "memvar" .OR. cfirst == "field"
               DO WHILE Right( cLine, 1 ) == ";"
                  cLine := Left( cLine, Len(cLine)-1 ) + " " + Alltrim( aText[++i] )
               ENDDO
               _prg_AddVars( oEdit, cLine, cPrefix, nSkip, aWords, .F. )
               LOOP
            ENDIF
         ENDIF
      ENDIF
      IF cfirst == "function" .OR. cfirst == "procedure" .OR. ;
            ( cfirst == "method" .AND. (!lClassDef .OR. " inline " $ cLine .OR. " block " $ cLine )) ;
            .OR. cfirst == "func" .OR. cfirst == "proc" .OR. ( cfirst == "static" .AND. ;
            ( ( cSecond := Lower( hb_TokenPtr( cLine, @nSkip ) ) ) == "function" .OR. ;
            cSecond == "procedure" .OR. cSecond == "func" .OR. cSecond == "proc" ) )
         cSecond := hb_TokenPtr( cLine, @nSkip )
         IF i < nLineCurr
            nLine0 := i
         ENDIF
         lGlob := .F.
         IF Lower( Left(cSecond,nPrefLen) ) == cPrefix
            IF ( nPos := At( "(", cSecond ) ) > 0
               cSecond := Left( cSecond, nPos )
            ENDIF
            Aadd( aWords, cSecond )
         ENDIF
      ELSEIF cfirst == "class" .or. ( cfirst == "create" .AND. ;
            ( cSecond := Lower( hb_TokenPtr( cLine, @nSkip ) ) ) == "class" )
         IF cfirst == "create" .OR. ( !( ( cSecond := Lower( hb_TokenPtr( cLine, @nSkip ) ) ) == "var" ) ;
               .AND. !( cSecond == "data" ) )
            lClassDef := .T.
         ENDIF
         lGlob := .F.
      ELSEIF cfirst == "end" .or. cfirst == "endclass"
         lClassDef := .F.
      ELSE
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
               IF Lower( Left(cSecond,nPrefLen) ) == cPrefix .AND. hb_Ascan(aWords,cSecond,,,.T.) == 0 ;
                  .AND. !trie_Exist( hTrieLang, cSecond )
                  Aadd( aWords, cSecond )
               ENDIF
            ENDIF
            nPos ++
         ENDDO
      ENDIF
   NEXT
   IF !Empty( nLine0 )
      FOR i := nLine0 TO nLineCurr - 1
         cLine := Ltrim( aText[i] )
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
         nSkip := 0
         IF i == nLine0
            IF ( nPos := At( "(", cLine ) ) > 0
               nSkip := nPos + 1
               DO WHILE Right( cLine, 1 ) == ";"
                  cLine := Left( cLine, Len(cLine)-1 ) + " " + Ltrim( aText[++i] )
               ENDDO
               IF ( nPos := At( ")", cLine ) ) > 0
                  cLine := Left( cLine, nPos-1 )
               ENDIF
               _prg_AddVars( oEdit, cLine, cPrefix, nSkip, aWords, .F. )
            ENDIF
         ELSE
            cfirst := Lower( hb_TokenPtr( cLine, @nSkip ) )
            IF cfirst == "static" .OR. cfirst == "local" .OR. cfirst == "memvar" .OR. cfirst == "field"
               DO WHILE Right( cLine, 1 ) == ";"
                  cLine := Left( cLine, Len(cLine)-1 ) + " " + Ltrim( aText[++i] )
               ENDDO
               _prg_AddVars( oEdit, cLine, cPrefix, nSkip, aWords, ( cfirst == "local" ) )
               LOOP
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN aWords