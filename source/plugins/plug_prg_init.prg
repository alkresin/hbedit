#define CTRL_PRESSED  0x020000
#define K_CTRL_H      8
#define K_CTRL_I      9
#define K_ENTER    13
#define K_ESC      27

STATIC cIniPath
STATIC lIsCurl := .F., cServAddr

FUNCTION Plug_prg_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()

      SetColor( o:cColorPane )
      Scroll( y, o:x1 + 8, y, o:x2 )
      DevPos( y, o:x1 + 8 )
      DevOut( "Harbour plugin:  Ctrl-H Help  Ctrl-I Info" )
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

   IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      IF nKey == K_CTRL_H
         _ctrlh( oEdit )
         DevPos( nRow, nCol )
         oEdit:TextOut()
         RETURN -1
      ELSEIF nKey == K_CTRL_I
         edi_SelectW( oEdit )
         cWord := cp_Substr( oEdit:lUtf8, oEdit:aText[oEdit:nLine], oEdit:nbx1, oEdit:nbx2-oEdit:nbx1 )
         _GetFuncInfo( oEdit, cWord )
         DevPos( nRow, nCol )
         oEdit:TextOut()
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _ctrlh( oEdit )

   LOCAL aMenu := { "HwGUI functions" }, i, cAddW := "$Functions", cFile := "hwg_funcs.txt"
   LOCAL arrfuncs, nCol := Col(), nRow := Row()

   i := FMenu( oEdit, aMenu, 2, 6 )
   IF i == 1
      edi_CloseWindow( cAddW )
      arrfuncs := MemoRead( cIniPath + cFile )
      IF !Empty( arrfuncs )
         IF ( i := FMenu( oEdit, arrfuncs, 2, 6 ) ) > 0
            edi_2cb( oEdit,, arrfuncs[i] )
            DevPos( nRow, nCol )
            oEdit:TextOut()
            _GetFuncInfo( oEdit, arrfuncs[i] )
         ENDIF
      ELSE
         edi_Alert( cFile + " not found..." )
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _GetFuncInfo( oEdit, sFunc )

   LOCAL nPos
   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu ), nRes
   LOCAL aGets := { {10,22,0,"",32}, ;
      {11,23,1,.T.,1}, {11,40,1,.F.,1}, {12,23,1,.F.,1}, {12,40,1,.F.,1}, ;
      {14,28,2,"[Info]",4,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {14,42,2,"[Cancel]",10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cFileRes := "hbedit_curl.out", cFileOut := "hbedit.out", cBuff

   IF ( nPos := At( '(', sFunc ) ) > 0
      sFunc := AllTrim( Left( sFunc, nPos-1 ) )
   ENDIF
   aGets[1,4] := Lower( sFunc )
   IF Empty( sFunc ) .OR. Left( sFunc,4 ) != "hwg_"
      RETURN Nil
   ENDIF

   hb_cdpSelect( "RU866" )
   @ 09, 20, 15, 58 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 13, 20 SAY "Ã"
   @ 13, 58 SAY "´"
   @ 13, 21 TO 13, 57
   hb_cdpSelect( oEdit:cp )

   @ 11, 22 SAY "[ ] Description  [ ] Sources"
   @ 12, 22 SAY "[ ] Changelog    [ ] Samples"

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
      cedi_RunConsoleApp( "curl www.kresin.ru/php/getad.php -o" + cFileRes, cFileOut )
      IF Empty( cBuff := MemoRead( cFileRes ) ) .OR. Left( cBuff, 5 ) != "addr:"
         edi_Alert( "Error" )
         RETURN Nil
      ENDIF
      cServAddr := Substr( cBuff, 6 )
   ENDIF

   sFunc := "?f=" + sFunc + "&o="
   IF aGets[2,4]
      sFunc += "d"
   ENDIF
   IF aGets[3,4]
      sFunc += "s"
   ENDIF
   IF aGets[4,4]
      sFunc += "c"
   ENDIF
   IF aGets[5,4]
      sFunc += "t"
   ENDIF

   FErase( cFileRes )
   cedi_RunConsoleApp( "curl " + cServAddr + sFunc + " -o" + cFileRes, cFileOut )
   IF Empty( cBuff := MemoRead( cFileRes ) )
      edi_Alert( "Error" )
      RETURN Nil
   ENDIF
   edi_Alert( cBuff )

   RETURN Nil
