#define K_LDBLCLK     1006
#define K_ENTER       13

FUNCTION Plug_Go_Build( oEdit )

   LOCAL i, arr := { "Build current file", "Build project" }
   LOCAL cFileOut := hb_DirTemp() + "hb_compile_err.out", cAddW := "$hb_compile_err", cBuff, oNew

   oEdit:Save()

   IF Empty( i := FMenu( oEdit, arr ) )
      RETURN Nil
   ENDIF

   @ 10, Int(MaxCol()/2)-4 SAY " Wait... " COLOR oEdit:cColorSel

   IF i == 1
      cedi_RunConsoleApp( "go build " + oEdit:cFileName, cFileOut )
   ELSE
      cedi_RunConsoleApp( "go build", cFileOut )
   ENDIF

   cBuff := MemoRead( cFileOut )

   IF Empty( cBuff )
      edi_Alert( "Done" )
   ELSE
      oNew := edi_AddWindow( oEdit, cBuff, cAddW, 2, 9 )
      oNew:lReadOnly := .T.
      oNew:bOnKey := {|o,n| _hbp_ErrWin_OnKey(o,n) }
   ENDIF

   oEdit:TextOut()

   RETURN Nil

FUNCTION _hbp_ErrWin_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nRow, s, nPos1, nPos2, nPos3, oNew, cFile

   IF nKey == K_ENTER .OR. nKey == K_LDBLCLK
      IF nKey == K_LDBLCLK
         nRow := MRow()
         IF nRow < oEdit:y1 .OR. nRow > oEdit:y2
            RETURN 0
         ENDIF
      ELSE
         nRow := Row()
      ENDIF
      s := oEdit:aText[ oEdit:RowToLine( nRow ) ]
      IF ( nPos1 := At( ":", s ) ) > 0 .AND. ( nPos2 := hb_At( ":", s, nPos1+1 ) ) > 0 ;
            .AND. ( nPos3 := hb_At( ":", s, nPos2+1 ) ) > 0
         cFile := hb_fnameNameExt( AllTrim( Left( s, nPos1-1 ) ) )
         oNew := mnu_NewBuf( oEdit, hb_fnameDir( oEdit:oParent:cFileName ) + cFile )
         IF oNew != Nil
            oNew:GoTo( Val(Substr(s,nPos1+1,nPos2-nPos1)), Val(Substr(s,nPos2+1,nPos3-nPos2)),, .T. )
         ENDIF
      ENDIF
   ENDIF

   RETURN 0
