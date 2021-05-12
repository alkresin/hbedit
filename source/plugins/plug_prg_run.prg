#define K_ENTER       13
#define K_LDBLCLK     1006

STATIC cIniPath
STATIC cPathInclude

Function plug_prg_run( oEdit, cPath )

   LOCAL acmd, cHrb, cBuff, cFile := "$hb_compile_err", bOldError, i, ie, oNew
   LOCAL nRow, nCol, cFileRes := hb_DirTemp() + "hb_compile_err.out"

   IF Empty( cIniPath )
      _prg_ReadIni( (cIniPath := cPath) + "lang_prg.ini" )
   ENDIF

   edi_CloseWindow( cFile )

   acmd := { oEdit:ToString(), "harbour", "-n2", "-q", "-w" }
   IF !Empty( cPathInclude )
      AAdd( aCmd, "-I" + cPathInclude )
   ENDIF

   i := cedi_rediron( 1, hb_DirTemp() + "hbcompile.out" )
   ie := cedi_rediron( 2, cFileRes )
   cHrb := hb_compileFromBuf( hb_ArrayToParams( acmd ) )
   cedi_rediroff( 2, ie )
   cedi_rediroff( 1, i )

   cBuff := Memoread( cFileRes )
   IF Empty( cHrb ) .OR. ( !Empty( cBuff ) .AND. " Warning " $ cBuff .AND. ;
         edi_Alert( "There are warnings;Run anyway?","Yes","No" ) == 2 )

      oNew := edi_AddWindow( oEdit, cBuff, cFile, 2, 7 )
      oNew:lReadOnly := .T.
      oNew:bOnKey := {|o,n| _prg_ErrWin_OnKey(o,n) }
   ELSE
      nRow := Row(); nCol := Col()
      CLEAR SCREEN
      bOldError := Errorblock( {|e| MacroError( e ) } )
      BEGIN SEQUENCE
         hb_hrbRun( cHrb )
      END SEQUENCE
      Errorblock( bOldError )
      Inkey(0)
      DevPos( nRow, nCol )
   ENDIF
   SetColor( oEdit:cColor )
   oEdit:WriteTopPane()
   oEdit:TextOut()

   RETURN Nil

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

STATIC FUNCTION _prg_ReadIni( cIni )

   LOCAL hIni, aIni, nSect, aSect, cTemp
   cPathInclude := ""

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "incpath" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cPathInclude := AllTrim( cTemp )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil
