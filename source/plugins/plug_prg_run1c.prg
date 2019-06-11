#define K_ENTER       13
#define K_LDBLCLK     1006

STATIC oConnection, oSession
STATIC cComObject, cServerPath, cBaseName, cLogin, cPass

FUNCTION Plug_prg_Run1c( oEdit )

   LOCAL cText
   LOCAL acmd := Array( 5 ), cHrb, cBuff, cFile := "$hb_compile_err", bOldError, i, oNew
   LOCAL nRow, nCol
   Memvar oConn
   PRIVATE oConn

   IF Empty( cText := oEdit:ToString() )
      RETURN Nil
   ENDIF

   cText := "#xtranslate {<ooo>}:<aaa>([<ppp,...>]) => hb_ExecFromArray( <ooo>,<(aaa)>, {<ppp>})" + ;
      Chr(13)+Chr(10) + "#xcommand TEXT TO VAR <var> => #pragma __stream|<var>:=%s" + ;
      Chr(13)+Chr(10) + "memvar oconn" + ;
      Chr(13)+Chr(10) + cText

   IF Empty( cComObject )
      Read_1c_Ini( edi_FindPath( "plugins" + hb_ps() + "plug_1c.ini" ) )
   ENDIF

   edi_CloseWindow( cFile )

   acmd[1] := cText
   acmd[2] := "harbour"
   acmd[3] := "-n2"
   acmd[4] := "-q"
   acmd[5] := "-w"

   cedi_rediron( 2, "hb_compile_err.out" )
   cHrb := hb_compileFromBuf( hb_ArrayToParams( acmd ) )
   cedi_rediroff( 2 )

   cBuff := Memoread( "hb_compile_err.out" )
   IF Empty( cHrb ) .OR. ( !Empty( cBuff ) .AND. " Warning " $ cBuff .AND. ;
         edi_Alert( "There are warnings;Run anyway?","Yes","No" ) == 2 )

      oNew := edi_AddWindow( oEdit, cBuff, cFile, 2, 7 )
      oNew:lReadOnly := .T.
      oNew:bOnKey := {|o,n| _prg1c_ErrWin_OnKey(o,n) }
   ELSE
      CLEAR SCREEN
      DevPos( 0,0 )
      IF Empty( oConnection )
         _1c_Connect()
      ENDIF
      IF !Empty( oConnection )
         nRow := Row(); nCol := Col()
         oConn := oConnection
         bOldError := Errorblock( {|e| MacroError( e ) } )
         BEGIN SEQUENCE
            hb_hrbRun( cHrb )
         END SEQUENCE
         Errorblock( bOldError )
         ? "Done!. Press any key"
         Inkey(0)
         DevPos( nRow, nCol )
      ENDIF
   ENDIF
   SetColor( oEdit:cColor )
   oEdit:WriteTopPane()
   oEdit:TextOut()

   RETURN Nil

FUNCTION _prg1c_ErrWin_OnKey( oEdit, nKeyExt )

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
            oEdit:oParent:GoTo( Iif(nLine>3,nLine-3,1), 1,, .T. )
            oEdit:lShow := .F.
            oEdit:nCurr := Ascan( oEdit:aWindows, {|o|o==oEdit:oParent} )
         ENDIF
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION _1c_Connect()

   LOCAL cConnString

   IF Empty( oSession )
      ? "Connect to OLE object..."
      oSession := win_OleCreateObject( cComObject )
   ENDIF
   IF !Empty( oSession )
      IF Empty( cPass )
         ACCEPT "Пароль:" TO cPass
      ENDIF
      cConnString := 'Srvr="' + cServerPath + '";Ref="' + cBaseName + ;
         '";Usr="' + cLogin + '";Pwd="' + cPass + '";LicDstr="Y"'
      ? "Connect to " + cBaseName + " as " + cLogin + "..."
      oConnection := oSession:Connect( cConnString )
      ? "Connected!"
   ENDIF

   RETURN Nil

STATIC FUNCTION Read_1c_Ini( cIni )

   LOCAL hIni := edi_iniRead( cIni ), aIni, nSect, cTemp, aSect

   IF !Empty( hIni )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "CONNECT"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "comobject" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cComObject := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "serverpath" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cServerPath := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "basename" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cBaseName := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "user" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cLogin := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "pass" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cPass := cTemp
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil
