#define CTRL_PRESSED  0x020000
#define ALT_PRESSED   0x040000
#define K_LBUTTONDOWN 1002
#define K_LDBLCLK     1006
#define K_CTRL_R      18
#define K_CTRL_L      12
#define K_CTRL_F      6
#define K_ENTER       13
#define K_ALT_F      289
#define K_ALT_R      275

#ifdef __PLATFORM__UNIX
STATIC lUnix := .T.
#else
STATIC lUnix := .F.
#endif

FUNCTION Plug_hwprj_Init( oEdit )

   LOCAL bEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "HwBuilder plugin:  Alt-F Files Alt-R Build" )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         IF oEdit:hCargo == Nil
            oEdit:hCargo := hb_hash()
         ENDIF
         oEdit:hCargo["help"] := "HwBuilder plugin hotkeys:" + Chr(10) + ;
            "  Alt-F  - Files list" + Chr(10) + ;
            "  Alt-R  - Build project" + Chr(10)
      ENDIF
      o:bStartEdit := Nil
      RETURN Nil
   }
   LOCAL bOnKeyOrig
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _hwprj_Init_OnKey(o,n)
      IF bOnKeyOrig != Nil .AND. nRes >= 0
         nRes := Eval( bOnKeyOrig, o, Iif( nRes==0, n, nRes ) )
      ENDIF
      RETURN nRes
   }

   oEdit:bStartEdit := bEdit
   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey

   RETURN Nil

FUNCTION _hwprj_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol, nRow

   IF oEdit:lUpdated .AND. oEdit:cargo != Nil
      oEdit:cargo := Nil
   ENDIF
   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_F
         _hwprj_Init_Files( oEdit )
         edi_SetPos( oEdit, oEdit:nLine, oEdit:nPos )
         RETURN -1
      ELSEIF nKey == K_ALT_R
         _hwprj_Init_Build( oEdit )
         edi_SetPos( oEdit, oEdit:nLine, oEdit:nPos )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _hwprj_Init_Files( oEdit )

   LOCAL arr, i, cPathBase := hb_fnameDir( oEdit:cFileName )
   LOCAL cName, aFiles
   LOCAL bSea := {|nop,cSea,cLine|
      LOCAL cBuff, n1, n2, n3, lCase
      IF nop == 0
         IF ( n1 := At( '/', cSea ) ) > 0
            IF Len( cSea ) > n1 .AND. Right( cSea,1 ) == '/'
               RETURN Left( cSea, n1-1 )
            ELSE
               RETURN Nil
            ENDIF
         ELSE
            RETURN cSea
         ENDIF
      ELSE
         IF ( n1 := At( '/', cSea ) ) > 0 .AND. ( n2 := hb_At( '/', cSea, n1+1 ) ) > 0 ;
            .AND. ( n3 := hb_At( '/', cSea, n2+1 ) ) > 0
            lCase := ! ( n3-n2 > 1 .AND. Substr( cSea,n2+1,1 ) == "c" )
            cSea := Substr( cSea, n1+1, n2-n1-1 )
            IF lCase
               cBuff := Memoread( cLine )
            ELSE
               cSea := cp_Lower( oEdit:lUtf8, cSea )
               cBuff := cp_Lower( oEdit:lUtf8, Memoread( cLine ) )
            ENDIF
            IF !( cSea $ cBuff )
               RETURN .F.
            ENDIF
         ENDIF
      ENDIF
      RETURN .T.
   }

   aFiles := _hwprj_Get_Files( oEdit )
   IF !Empty( aFiles )
      IF !Empty( arr := FMenu( oEdit, aFiles,,,,,,,, .T., .T., bSea ) )
         FOR i := 1 TO Len( arr )
            cName := cPathBase + aFiles[arr[i]]
            mnu_NewBuf( oEdit, cName )
         NEXT
      ENDIF
      oEdit:TextOut()
   ENDIF

   RETURN Nil

STATIC FUNCTION _hwprj_Get_Files( oEdit )

   LOCAL i, j, s, cPathBase := hb_fnameDir( oEdit:cFileName ), cCurrDir := Curdir()
   LOCAL cPath, cName, aDir, aFiles, cSep := hb_ps(), cSepWrong := Iif( cSep=='/','\','/' )
   LOCAL cTmp, cTmp2, lYes, l, ap, af, cLine, nPos, cSrcPath := ""

   IF Empty( oEdit:cargo )
      DirChange( cPathBase )
      aFiles := {}
      FOR i := 1 TO Len( oEdit:aText )
         IF !Empty( cLine := AllTrim( StrTran( oEdit:aText[i], Chr(13), "" ) ) ) .AND. !( Left( cLine, 1 ) == "#" )
            DO WHILE Left( cLine,1 ) == '{'
               IF ( nPos := At( "}", cLine ) ) > 0
                  cTmp := AllTrim( Substr( cLine, 2, nPos-2 ) )
                  l := .T.
                  IF Left( cTmp,1 ) == "!"
                     cTmp := LTrim( Substr( cTmp,2 ) )
                     l := .F.
                  ENDIF
                  lYes := ( cTmp == "unix" .AND. lUnix ) .OR. ( cTmp == "win" .AND. !lUnix )
                  IF !l
                     lYes := !lYes
                  ENDIF
                  IF lYes
                     cLine := LTrim( Substr( cLine, nPos + 1 ) )
                  ELSE
                     cLine := ""
                     EXIT
                  ENDIF
               ELSE
                  edi_Alert( "Wrong option: " + cLine )
                  RETURN Nil
               ENDIF
            ENDDO
            IF Empty( cLine )
               LOOP
            ENDIF
            IF ( nPos := At( "=", cLine ) ) > 0
               IF ( cTmp := Lower( Left( cLine, nPos-1 ) ) ) == "srcpath"
                  cSrcPath := _DropSlash( Substr( cLine, nPos + 1 ) ) + hb_ps()
               ENDIF

            ELSEIF Left( cLine,1 ) == '@'

            ELSEIF Left( cLine,1 ) == ':'
            ELSE
               IF ( nPos := At( " ", cLine ) ) > 0
                  cTmp := Left( cLine, nPos - 1 )
                  cTmp2 := AllTrim( Substr( cLine, nPos + 1 ) )
               ELSE
                  cTmp := cLine
                  cTmp2 := Nil
               ENDIF
               IF '*' $ cTmp
                  ap := Nil
                  IF !Empty( cTmp2 ) .AND. ( nPos := At( "-(", cTmp2 ) ) > 0 .AND. ;
                     ( j := hb_At( ")", cTmp2, nPos ) ) > 0
                     ap := hb_aTokens( Substr( cTmp2, nPos+2, j-nPos-2 ), ' ' )
                     cTmp2 := AllTrim( Left( cTmp2, nPos-1 ) + Substr( cTmp2, j+1 ) )
                  ENDIF
                  af := hb_Directory( cSrcPath + cTmp )
                  FOR j := 1 TO Len( af )
                     IF Empty( ap ) .OR. hb_AScan( ap, af[j,1],,, .T. ) == 0
                        AAdd( aFiles, cSrcPath+af[j,1] )
                     ENDIF
                  NEXT
               ELSE
                  AAdd( aFiles, Iif( Empty( cSrcPath ) .AND. Empty( hb_fnameDir(cTmp) ), ;
                     cTmp, cSrcPath + cTmp ) )
               ENDIF

            ENDIF
         ENDIF
      NEXT
      FOR j := 1 TO Len( aFiles )
         IF cSepWrong $ aFiles[j]
            aFiles[j] := StrTran( aFiles[j], cSepWrong, cSep )
         ENDIF
      NEXT
      oEdit:cargo := aFiles
      DirChange( cCurrDir )
   ELSE
      aFiles := oEdit:cargo
   ENDIF

   RETURN aFiles

STATIC FUNCTION _DropSlash( cLine )

   IF Right( cLine,1 ) $ "/\"
      RETURN hb_strShrink( cLine, 1 )
   ENDIF
   RETURN cLine

STATIC FUNCTION _hwprj_Init_Build( oEdit )

   LOCAL cBuff, oNew, i, cDop, cAddW := "$hb_compile_err"
   LOCAL cPathBase := hb_fnameDir( oEdit:cFileName ), cCurrDir := Curdir()

   edi_CloseWindow( "$"+cAddW )

   oEdit:Save()
   cDop := AllTrim( edi_MsgGet( "Parameters" ) )

   SetColor( oEdit:cColorSel )
   @ 10, Int(MaxCol()/2)-4 SAY " Wait... "
   DirChange( cPathBase )
   cedi_RunConsoleApp( "hwbc -q " + cDop + " " + oEdit:cFileName,, @cBuff )
   DirChange( cCurrDir )
   SetColor( oEdit:cColor )

   oNew := edi_AddWindow( oEdit, cBuff, cAddW, 2, 12 )
   oNew:lReadOnly := .T.
   oNew:bOnKey := {|o,n| _hwprj_ErrWin_OnKey(o,n) }

   oEdit:TextOut()

   RETURN Nil

FUNCTION _hwprj_ErrWin_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nRow, sOrig, s, nPos, nPos2, c, nLine, aFiles, oNew, cFileName

   IF nKey == K_ENTER .OR. nKey == K_LDBLCLK
      IF nKey == K_LDBLCLK
         nRow := MRow()
         IF nRow < oEdit:y1 .OR. nRow > oEdit:y2
            RETURN 0
         ENDIF
      ELSE
         nRow := Row()
      ENDIF
      sOrig := oEdit:aText[ oEdit:RowToLine( nRow ) ]
      s := Lower( sOrig )
      IF "error" $ s .OR. "warning" $ s
         nPos2 := Iif( ( nPos := At( ".prg", s ) ) > 0, nPos + 4, ;
            Iif( ( nPos := At( ".c", s ) ) > 0, nPos + 2, 0 ) )
         IF nPos2 > 0
            DO WHILE --nPos > 0 .AND. ( ( (c := Substr( s,nPos,1 )) >= 'a' ) .OR. c $ "0123456789_" )
            ENDDO
            nPos ++
            cFileName := Substr( sOrig, nPos, nPos2 - nPos )
         ENDIF
      ENDIF
      IF !Empty( cFileName )
         aFiles := _hwprj_Get_Files( oEdit:oParent )
         IF ( nPos := Ascan( aFiles, {|cFile|Lower(hb_fnameNameExt(cFile))==cFileName} ) ) > 0
            oNew := mnu_NewBuf( oEdit, hb_fnameDir( oEdit:oParent:cFileName ) + aFiles[nPos] )
            IF oNew != Nil
               oNew:GoTo( nLine, 1,, .T. )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN 0
