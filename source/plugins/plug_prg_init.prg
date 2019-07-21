#define ALT_PRESSED   0x040000
#define K_ALT_D   288
#define K_ALT_I   279
#define K_ENTER    13
#define K_ESC      27

STATIC cIniPath
STATIC lIsCurl := .F., cServAddr
STATIC lDescri := .T., lSources := .F., lChglog := .F., lSamples := .F., lRu := .F.
STATIC aHwgFuncs, aHbFuncs
STATIC aHbShort

FUNCTION Plug_prg_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()

      SetColor( o:cColorPane )
      Scroll( y, o:x1 + 8, y, o:x2 )
      DevPos( y, o:x1 + 8 )
      DevOut( "Harbour plugin:  Alt-D Dictionary  Alt-I Info" )
      SetColor( o:cColor )
      o:bStartEdit := Nil
      DevPos( nRow, nCol )

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

   RETURN Nil

FUNCTION _prg_Init_OnKey( oEdit, nKeyExt )

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
      ENDIF
   ENDIF

   RETURN 0

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
   LOCAL cFileRes := "hbedit_curl.out", cFileOut := "hbedit.out", cBuff, cAddW := "$FuncInfo", o

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
      cedi_RunConsoleApp( "curl www.kresin.ru/php/getad.php -s -o" + cFileRes, cFileOut )
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
   cedi_RunConsoleApp( 'curl "' + cServAddr + sFunc + '" -s -o' + cFileRes, cFileOut )
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
