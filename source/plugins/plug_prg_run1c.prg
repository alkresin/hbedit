#define K_ENTER       13
#define K_ESC         27
#define K_LDBLCLK     1006

STATIC oConnection, oSession, cIniPath
STATIC cComObject := "", cServerPath := "", cBaseName := "", cLogin := "", cPass := ""

FUNCTION Plug_prg_Run1c( oEdit, cPath )

   LOCAL cText, cFileRes := hb_DirTemp() + "hb_compile_err.out"
   LOCAL acmd := Array( 5 ), cHrb, cBuff, cFile := "$hb_compile_err", bOldError, i, oNew
   LOCAL nRow, nCol
   Memvar oConn
   PRIVATE oConn

   IF Empty( cText := oEdit:ToString() )
      RETURN Nil
   ENDIF

   cText := "#xtranslate ole.<ooo>.<aaa>([<ppp,...>]) => hb_ExecFromArray( <ooo>,<(aaa)>, {<ppp>})"  + ;
      Chr(13)+Chr(10) + "#xcommand TEXT TO VAR <var> => #pragma __stream|<var>:=%s" + ;
      Chr(13)+Chr(10) + "memvar oconn" + ;
      Chr(13)+Chr(10) + cText

   cIniPath := cPath
   IF Empty( cComObject )
      Read_1c_Ini( cPath + "plug_1c.ini" )
   ENDIF

   edi_CloseWindow( cFile )

   acmd[1] := cText
   acmd[2] := "harbour"
   acmd[3] := "-n2"
   acmd[4] := "-q"
   acmd[5] := "-w"

   cedi_rediron( 2, cFileRes )
   cHrb := hb_compileFromBuf( hb_ArrayToParams( acmd ) )
   cedi_rediroff( 2 )

   cBuff := Memoread( cFileRes )
   IF Empty( cHrb ) .OR. ( !Empty( cBuff ) .AND. " Warning " $ cBuff .AND. ;
         edi_Alert( "There are warnings;Run anyway?","Yes","No" ) == 2 )
      oNew := edi_AddWindow( oEdit, cBuff, cFile, 2, 7 )
      oNew:lReadOnly := .T.
      oNew:bOnKey := {|o,n| _prg1c_ErrWin_OnKey(o,n) }
   ELSE
      nRow := Row(); nCol := Col()
      CLEAR SCREEN
      DevPos( 0,0 )
      IF Empty( oConnection )
         IF _GetAccData( oEdit )
            CLEAR SCREEN
            DevPos( 0,0 )
            _1c_Connect()
         ENDIF
      ENDIF
      IF !Empty( oConnection )
         oConn := oConnection
         bOldError := Errorblock( {|e| MacroError( e ) } )
         BEGIN SEQUENCE
            hb_hrbRun( cHrb )
         END SEQUENCE
         Errorblock( bOldError )
         ? "Done!. Press any key"
         Inkey(0)
      ENDIF
      DevPos( nRow, nCol )
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
   IF Empty( oSession )
      ? "Connection failed"
      Inkey(0)
   ELSE
      IF Empty( cPass )
         ACCEPT " à®«ì:" TO cPass
      ENDIF
      cConnString := 'Srvr="' + cServerPath + '";Ref="' + cBaseName + ;
         '";Usr="' + cLogin + '";Pwd="' + cPass + '";LicDstr="Y"'
      ? "Connect to " + cBaseName + " as " + cLogin + "..."
      oConnection := oSession:Connect( cConnString )
      ? "Connected!"
   ENDIF

   RETURN Nil

STATIC FUNCTION _GetAccData( oEdit )

   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu )
   LOCAL aGets := { {10,25,0,Iif(Empty(cComObject),"V83.COMConnector.1",cComObject),36}, ;
      {11,25,0,cServerPath,36 }, {12,25,0,cBaseName,36 }, {13,25,0,cLogin,36 }, {14,25,0,cPass,36 }, ;
      {15,18,1,.F.,1}, ;
      {17,28,2,"[Ok]",4,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {17,42,2,"[Cancel]",10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL nRes, s := "", cEol := Chr(13)+Chr(10)

   hb_cdpSelect( "RU866" )
   @ 09, 15, 18, 66 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 16, 15 SAY "Ã"
   @ 16, 66 SAY "´"
   @ 16, 16 TO 16, 65
   hb_cdpSelect( oEdit:cp )

   @ 10,17 SAY "ComObj: "
   @ 11,17 SAY "Server: "
   @ 12,17 SAY "Base: "
   @ 13,17 SAY "Login: "
   @ 14,17 SAY "Passw: "
   @ 15,17 SAY "[ ] Save"
   SetColor( oEdit:cColorMenu )

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cComObject := Trim( aGets[1,4] )
      cServerPath := Trim( aGets[2,4] )
      cBaseName := Trim( aGets[3,4] )
      cLogin := Trim( aGets[4,4] )
      cPass := Trim( aGets[5,4] )
      IF aGets[6,4]
         IF !Empty( cComObject )
            s += "comobject=" + cComObject + cEol
         ENDIF
         IF !Empty( cServerPath )
            s += "serverpath=" + cServerPath + cEol
         ENDIF
         IF !Empty( cBaseName )
            s += "basename=" + cBaseName + cEol
         ENDIF
         IF !Empty( cLogin )
            s += "user=" + cLogin + cEol
         ENDIF
         IF !Empty( cPass )
            s += "pass=" + cPass + cEol
         ENDIF
         hb_MemoWrit( cIniPath + "plug_1c.ini", s )
      ENDIF
   ENDIF

   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN ( nRes > 0 .AND. nRes < Len(aGets) )

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
