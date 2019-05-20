#define CTRL_PRESSED  0x020000
#define K_LBUTTONDOWN 1002
#define K_LDBLCLK     1006
#define K_CTRL_R      18
#define K_CTRL_L      12
#define K_CTRL_F      6
#define K_ENTER       13

FUNCTION Plug_hbp_Init( oEdit )

   LOCAL bEdit := {|o|
      LOCAL y1 := o:aRect[1]
      SetColor( "N/N" )
      Scroll( y1, o:x1, y1, o:x2 )
      SetColor( o:cColorPane )
      DevPos( y1, o:x1 )
      DevOut( PAdr( hb_fnameName( o:cFileName ), 19 ) )
      DevPos( y1, o:x1 + 20 )
      DevOut( "Ctrl-F Files" )
      DevPos( y1, o:x1 + 34 )
      DevOut( "Ctrl-L Build" )
      SetColor( o:cColor )
      RETURN Nil
   }
   LOCAL bEndEdit := {|o|
      LOCAL y1 := o:aRect[1]
      SetColor( o:cColorPane )
      Scroll( y1, o:x1, y1, o:x2 )
      RETURN Nil
   }
   LOCAL bOnKeyOrig
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _hbp_Init_OnKey(o,n)
      IF bOnKeyOrig != Nil .AND. nRes >= 0
         nRes := Eval( bOnKeyOrig, o, Iif( nRes==0, n, nRes ) )
      ENDIF
      RETURN nRes
   }

   oEdit:lTopPane := .F.
   oEdit:y1 := oEdit:aRect[1] + 1
   oEdit:bStartEdit := bEdit
   oEdit:bEndEdit := bEndEdit
   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey

   RETURN Nil

FUNCTION _hbp_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol, nRow

   IF oEdit:lUpdated .AND. oEdit:cargo != Nil
      oEdit:cargo := Nil
   ENDIF
   IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      IF nKey == K_CTRL_F
         _hbp_Init_Files( oEdit )
         edi_SetPos( oEdit, oEdit:nLine, oEdit:nPos )
         RETURN -1
      ELSEIF nKey == K_CTRL_L
         _hbp_Init_Build( oEdit )
         edi_SetPos( oEdit, oEdit:nLine, oEdit:nPos )
         RETURN -1
      ENDIF
   ELSEIF nKey == K_LBUTTONDOWN
      nCol := MCol()
      nRow := MRow()
      IF nRow == oEdit:y1-1 .AND. nCol >= 20 .AND. nCol <= 31
         _hbp_Init_Files( oEdit )
         edi_SetPos( oEdit, oEdit:nLine, oEdit:nPos )
         RETURN -1
      ELSEIF nRow == oEdit:y1-1 .AND. nCol >= 34 .AND. nCol <= 45
         _hbp_Init_Build( oEdit )
         edi_SetPos( oEdit, oEdit:nLine, oEdit:nPos )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _hbp_Init_Files( oEdit )

   LOCAL arr, i, cPathBase := hb_fnameDir( oEdit:cFileName )
   LOCAL cName, aFiles

   aFiles := _hbp_Get_Files( oEdit )
   IF !Empty( aFiles )
      IF !Empty( arr := FMenu( oEdit, aFiles,,,,,,,, .T., .T. ) )
         FOR i := 1 TO Len( arr )
            cName := cPathBase + aFiles[arr[i]]
            mnu_NewBuf( oEdit, cName )
         NEXT
      ENDIF
      oEdit:TextOut()
   ENDIF

   RETURN Nil

STATIC FUNCTION _hbp_Get_Files( oEdit )

   LOCAL i, j, s, cPathBase := hb_fnameDir( oEdit:cFileName )
   LOCAL cPath, cName, aDir, aFiles

   IF Empty( oEdit:cargo )
      aFiles := {}
      FOR i := 1 TO Len( oEdit:aText )
         s := Iif( Asc( oEdit:aText[i] ) == 32, Ltrim(oEdit:aText[i]), oEdit:aText[i] )
         IF !Empty( s ) .AND. !( Left( s,1 ) $ "#-{" )
            s := Trim( s )
            cPath := hb_fnameDir( s )
            cName := hb_fnameNameExt( s )
            IF '*' $ cName
               aDir := Directory( cPathBase+cPath+cName, "HS" )
               FOR j := 1 TO Len( aDir )
                  Aadd( aFiles, cPath + aDir[j,1] )
               NEXT
            ELSE
               Aadd( aFiles, cPath + cName )
            ENDIF
         ENDIF
      NEXT
      oEdit:cargo := aFiles
   ELSE
      aFiles := oEdit:cargo
   ENDIF

   RETURN aFiles

STATIC FUNCTION _hbp_Init_Build( oEdit )

   LOCAL cBuff, oNew, i, cFile := "hb_compile_err.out"

   edi_CloseWindow( "$"+cFile )

   SetColor( "W+/R" )
   @ 10, Int(MaxCol()/2)-4 SAY " Wait... "
   cedi_RunConsoleApp( "hbmk2 " + oEdit:cFileName, cFile )
   cBuff := MemoRead( cFile )
   cFile := "$" + cFile
   SetColor( oEdit:cColor )
   IF Empty( cBuff )
      edi_Alert( "Done" )
   ELSE
      oNew := edi_AddWindow( oEdit, cBuff, cFile, 2, 9 )
      oNew:lReadOnly := .T.
      oNew:bOnKey := {|o,n| _hbp_ErrWin_OnKey(o,n) }
   ENDIF
   oEdit:TextOut()

   RETURN Nil

FUNCTION _hbp_ErrWin_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nRow, s, nPos, nLine, aFiles, oNew

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
            s := hb_fnameNameExt( Trim( Left( s, nPos-1 ) ) )
            aFiles := _hbp_Get_Files( oEdit:oParent )
            IF ( nPos := Ascan( aFiles, {|cFile|Lower(hb_fnameNameExt(cFile))==s} ) ) > 0
               oNew := mnu_NewBuf( oEdit, hb_fnameDir( oEdit:oParent:cFileName ) + aFiles[nPos] )
               IF oNew != Nil
                  oNew:GoTo( nLine, 1,, .T. )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN 0
