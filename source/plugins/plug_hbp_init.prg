#define CTRL_PRESSED  0x020000
#define K_LBUTTONDOWN 1002
#define K_CTRL_R      18
#define K_CTRL_L      12
#define K_CTRL_F      6

FUNCTION Plug_hbp_Init( oEdit )

   LOCAL bEdit := {|o|
      SetColor( "N/N" )
      Scroll( o:y1, o:x1, o:y1, o:x2 )
      SetColor( o:cColorPane )
      DevPos( o:y1, o:x1 )
      DevOut( PAdr( hb_fnameName( o:cFileName ), 19 ) )
      DevPos( o:y1, o:x1 + 20 )
      DevOut( "Ctrl-F Files" )
      DevPos( o:y1, o:x1 + 34 )
      DevOut( "Ctrl-L Build" )
      o:y1 ++
   }

   oEdit:lTopPane := .F.
   oEdit:bEdit := bEdit
   oEdit:bOnKey := {|o,n| _hbp_Init_OnKey(o,n) }

   RETURN Nil

FUNCTION _hbp_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol, nRow

   IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      IF nKey == K_CTRL_F
         _hbp_Init_Files( oEdit )
         DevPos( oEdit:nRow, oEdit:nCol )
         RETURN -1
      ELSEIF nKey == K_CTRL_L
         _hbp_Init_Build( oEdit )
         DevPos( oEdit:nRow, oEdit:nCol )
         RETURN -1
      ENDIF
   ELSEIF nKey == K_LBUTTONDOWN
      nCol := MCol()
      nRow := MRow()
      IF nRow == oEdit:y1-1 .AND. nCol >= 20 .AND. nCol <= 31
         _hbp_Init_Files( oEdit )
         DevPos( oEdit:nRow, oEdit:nCol )
         RETURN -1
      ELSEIF nRow == oEdit:y1-1 .AND. nCol >= 34 .AND. nCol <= 45
         _hbp_Init_Build( oEdit )
         DevPos( oEdit:nRow, oEdit:nCol )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _hbp_Init_Files( oEdit )

   LOCAL i, j, s, cPathBase := hb_fnameDir( oEdit:cFileName )
   LOCAL cPath, cName, aDir, aFiles := {}

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
   IF !Empty( aFiles )
      IF ( i := FMenu( oEdit, aFiles ) ) > 0
         cName := cPathBase + aFiles[i]
         IF File( cName )
            mnu_NewWin( oEdit, MemoRead(cName), cName )
            RETURN Nil
         ELSE
            edi_Alert( "File not found" )
         ENDIF
      ENDIF
      oEdit:TextOut()
   ENDIF

   RETURN Nil

STATIC FUNCTION _hbp_Init_Build( oEdit )

   LOCAL cBuff

   SetColor( "W+/R" )
   @ 10, Int(MaxCol()/2)-4 SAY " Wait... "
   cedi_RunConsoleApp( "hbmk2 " + oEdit:cFileName, "hb_compile_err.out" )
   cBuff := MemoRead( "hb_compile_err.out" )
   edi_Alert( "Done" )
   SetColor( oEdit:cColor )
   oEdit:TextOut()

   RETURN Nil
