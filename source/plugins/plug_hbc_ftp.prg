
#include "inkey.ch"

STATIC aKeys1 := { K_DOWN, K_UP, K_MWBACKWARD, K_MWFORWARD, K_LEFT, K_RIGHT, ;
   K_PGDN, K_PGUP, K_HOME, K_END, K_TAB, K_CTRL_TAB, K_LBUTTONDOWN, K_RBUTTONDOWN, K_ENTER, ;
   K_INS, K_CTRL_R, K_CTRL_P, K_F9, K_F10, K_ALT_D }
STATIC aMonths := { "jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec" }
STATIC cNotPerm := "Operation not permitted!"

FUNCTION plug_hbc_ftp( oPane, cPlugPath, aParams )

   LOCAL cAddr, cPath, cLogin := "", cPass := "", lSave := .F.
   LOCAL hSocket, nPort

   IF Empty( aParams )
      IF Empty( cAddr := edi_MsgGet( "FTP Address:" ) )
         RETURN .F.
      ELSE
         RETURN oPane:ChangeDir( "ftp:" + cAddr )
      ENDIF
   ENDIF

   cAddr := aParams[1]
   nPort := Iif( Empty(Val(aParams[2])), 21, Val(aParams[2]) )
   cPath := aParams[3]
   cLogin := aParams[4]
   cPass := aParams[5]                    	

   IF Empty( cLogin ) .OR. Empty( cPass )
      IF !hbc_GetLogin( @cLogin, @cPass, @lSave )
         RETURN .F.
      ENDIF
      IF Empty( cLogin )
         cLogin := "anonymous"
      ENDIF
   ENDIF

   hb_inetInit()
   ftplog( "Connect to: " + cAddr + ":" + Ltrim(str(nPort)) )
   IF Empty( hSocket := FtpConnect( cAddr, nPort ) )
      hb_inetCleanup()
      RETURN .F.
   ENDIF

   IF FtpLogin( hSocket, cLogin, cPass )
      FtpSendCmd( hSocket, "TYPE I" )

      oPane:pSess := hSocket
      oPane:bRefresh := {|o|_plug_Refresh(o)}
      oPane:bOnKey := {|o,n|_plug_OnKey(o,n)}

      aParams[4] := cLogin
      aParams[5] := cPass
      aParams[6] := lSave

      RETURN .T.
   ELSE
      edi_Alert( "Can't login" )
   ENDIF

   RETURN .F.

FUNCTION plug_hbc_ftp_close( o )

   hb_inetClose( o:pSess )

   RETURN Nil

FUNCTION plug_hbc_ftp_copy( o, aParams )

   LOCAL cFileName, cFileTo, nPos

   cFileName := aParams[1]
   cFileTo := aParams[2]

   IF aParams[3] .OR. !( cFileTo = "ftp:" )
      edi_Alert( cNotPerm )
      RETURN -1
   ENDIF

   nPos := At( '/', cFileTo )
   IF FtpWriteFile( o:pSess, cFileName, Substr( cFileTo, nPos ) ) <= 0
      RETURN -1
   ENDIF

   RETURN 1

STATIC FUNCTION _plug_Refresh( oPane )

   oPane:aDir := FtpList( oPane:pSess, oPane:cCurrPath )

   RETURN 0

STATIC FUNCTION _plug_OnKey( oPane, nKeyExt )

   LOCAL nKey := hb_keyStd( nKeyExt ), cBuffer, oPaneTo, cFileTo, cName, o

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE
      RETURN -1
   ENDIF
   IF Ascan( aKeys1, nKey ) > 0
      RETURN 0
   ENDIF

   IF nKey == K_F5
      IF 'D' $ oPane:aDir[oPane:nCurrent + oPane:nShift,5]
         edi_Alert( cNotPerm )
         RETURN -1
      ENDIF
      cName := oPane:aDir[oPane:nCurrent + oPane:nShift,1]
      oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
      IF oPaneTo:nPanelMod > 0
         edi_Alert( cNotPerm )
         RETURN -1
      ENDIF

      IF !Empty( cFileTo := FAsk_Copy( "Copy " + NameShortcut( cName, 48 ) + ;
         " to:", oPane:cCurrPath + cName ) )
         cBuffer := FtpReadFile( oPane:pSess, cFileTo )
         hb_MemoWrit( oPaneTo:cCurrPath+cName, cBuffer )
         oPaneTo:Refresh()
         oPaneTo:RedrawAll()
      ENDIF

   ELSEIF nKey == K_F8
      IF 'D' $ oPane:aDir[oPane:nCurrent + oPane:nShift,5]
         edi_Alert( cNotPerm )
         RETURN -1
      ENDIF
      cName := oPane:aDir[oPane:nCurrent + oPane:nShift,1]
      IF edi_Alert( "Really delete " + cName + "?", "No", "Yes" ) == 2
         IF FtpDeleFile( oPane:pSess, oPane:cCurrPath + cName )
            oPane:Refresh()
            oPane:RedrawAll()
         ELSE
            edi_Alert( "Can't delete " + cName )
         ENDIF
      ENDIF
   ELSEIF nKey == K_F4
      IF 'D' $ oPane:aDir[oPane:nCurrent + oPane:nShift,5]
         RETURN 0
      ELSE
         o := TEdit():aWindows[TEdit():nCurr]
         cName := oPane:aDir[oPane:nCurrent + oPane:nShift,1]
         IF !Empty( cBuffer := FtpReadFile( oPane:pSess, oPane:cCurrPath + cName ) )
            mnu_NewBuf( o, cName, cBuffer, ):lReadOnly := .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN -1

STATIC FUNCTION FtpConnect( cAddr, nPort )

   LOCAL hSocket, nErr

   hSocket := hb_inetCreate()
   hb_inetConnect( cAddr, nPort, hSocket )
   IF ( nErr := hb_inetErrorCode( hSocket ) ) != 0
      FtpLog( "Connect failed: " + Ltrim(Str(nErr)) )
      hb_inetClose( hSocket )
      RETURN Nil
   ENDIF
   hb_inetTimeout( hSocket , 3000 )
   FtpGetReply( hSocket )

   RETURN hSocket

STATIC FUNCTION FtpLogin( hSocket, cLogin, cPass )

   LOCAL cRes
   FtpLog( "--- User ---" )
   hb_inetSendAll( hSocket, "USER " + cLogin + Chr(13)+Chr(10) )
   cRes := FtpGetReply( hSocket )
   IF Left( cRes,1 ) > '3'
      RETURN .F.
   ENDIF

   FtpLog( "--- Pass ---" )
   hb_inetSendAll( hSocket, "PASS " + cPass + Chr(13)+Chr(10) )
   cRes := FtpGetReply( hSocket )
   IF Left( cRes,1 ) > '3'
      RETURN .F.
   ENDIF

   RETURN .T.

STATIC FUNCTION FtpSendCmd( hSocket, cmd )

   FtpLog( "--- " + cmd + " ---" )
   hb_inetSendAll( hSocket, cmd + Chr(13)+Chr(10) )
   FtpGetReply( hSocket )

   RETURN Nil

STATIC FUNCTION FtpPASV( hSocket )

   LOCAL cNewIp, cNewPort, cBuf, nPos, aPasv, hSockNew

   FtpLog( "--- Pasv ---" )
   hb_inetSendAll( hSocket, "PASV" + Chr(13)+Chr(10) )

   cBuf := FtpGetReply( hSocket )

   IF ( nPos := At( ',', cBuf ) ) == 0
      FtpLog( "Error1" )
      RETURN Nil
   ENDIF
   nPos --
   DO WHILE IsDigit( Substr( cBuf,nPos,1 ) )
      nPos --
   ENDDO
   cBuf := Substr( cBuf, nPos+1 )
   aPasv := hb_ATokens( cBuf, ',' )
   IF Len( aPasv ) < 6
      FtpLog( "Error2" )
      RETURN Nil
   ENDIF
   cNewIp := Alltrim(aPasv[1])+"."+ALLtrim(aPasv[2])+"."+ALLtrim(aPasv[3])+"."+ALLtrim(aPasv[4])
   cNewPort := Val(aPasv[5])*256 +Val(aPasv[6])

   hSockNew := hb_inetCreate()
   hb_inetConnect( cNewIp, cNewPort, hSockNew )

   RETURN hSockNew

STATIC FUNCTION FtpList( hSocket, cPath )

   LOCAL hSockNew, cBuffer := "", cBuf, nRet
   LOCAL aDir := {}, arr, i, j, cName, nSize, cAttr, dDate, cYear

   IF Empty( hSockNew := FtpPASV( hSocket ) )
      RETURN Nil
   ENDIF

   FtpLog( "--- List ---" )
   hb_inetSendAll( hSocket, "LIST " + cPath + Chr(13)+Chr(10) )

   cBuf := Space( 512 )
   DO WHILE ( nRet := hb_inetRecvAll( hSockNew, @cBuf, 512 ) ) > 0
      cBuffer += Iif( nRet == 512, cBuf, Left( cBuf,nRet ) )
   ENDDO

   hb_inetClose( hSockNew )

   FtpLog( cBuffer )
   FtpGetReply( hSocket )

   arr := hb_ATokens( cBuffer, Chr(10) )
   FOR i := 1 TO Len( arr )
      IF !Empty( arr[i] ) .AND. Len( arr[i] := hb_Atokens( arr[i] ) ) == 9
         cName := Iif( Right(arr[i,9],1) == Chr(13), hb_strShrink(arr[i,9],1), arr[i,9] )
         IF !( cName == "." )
            cAttr := Upper( arr[i,1] )
            nSize := Iif( "D" $ cAttr, 0, Val(arr[i,5]) )
            IF ( j := Ascan( aMonths, Lower(arr[i,6]) ) ) > 0
               cYear := Iif( ':' $ arr[i,8], Str( Year(Date()),4 ), arr[i,8] )
               dDate := Stod( cYear + PAdl( Ltrim(Str(j)),2,'0' ) + PAdl( arr[i,7],2,'0' ) )
            ELSE
               dDate := Stod( "19000101" )
            ENDIF
            AAdd( aDir, { cName, nSize, dDate, "", cAttr } )
            //edi_writelog( cName + " " + Ltrim(Str(nSize)) + " " + arr[i,5] + " " + cAttr )
         ENDIF
      ENDIF
   NEXT

   RETURN aDir

STATIC FUNCTION FtpReadFile( hSocket, cFileName )

   LOCAL hSockNew, cBuffer := "", cBuf, nRet
   LOCAL aDir := {}, arr, i, j, cName, nSize, cAttr, dDate, cYear

   IF Empty( hSockNew := FtpPASV( hSocket ) )
      RETURN Nil
   ENDIF

   hb_inetTimeout( hSockNew , 10000 )

   FtpLog( "--- Retr " + cFileName + " ---" )
   hb_inetSendAll( hSocket, "RETR " + cFileName + Chr(13)+Chr(10) )

   cBuf := Space( 512 )
   DO WHILE ( nRet := hb_inetRecvAll( hSockNew, @cBuf, 512 ) ) > 0
      cBuffer += Iif( nRet == 512, cBuf, Left( cBuf,nRet ) )
   ENDDO

   hb_inetClose( hSockNew )

   //FtpLog( cBuffer )
   FtpGetReply( hSocket )

   RETURN cBuffer

STATIC FUNCTION FtpWriteFile( hSocket, cFileName, cFileTo )

   LOCAL hSockNew, cBuffer := "", cBuf, nRet
   LOCAL aDir := {}, arr, i, j, cName, nSize, cAttr, dDate, cYear

   IF Empty( cBuf := hb_vfLoad( cFileName ) )
      RETURN -1
   ENDIF

   IF Empty( hSockNew := FtpPASV( hSocket ) )
      RETURN Nil
   ENDIF

   FtpLog( "--- Stor " + cFileTo + " ---" )
   IF ( nRet := hb_inetSendAll( hSocket, "STOR " + cFileTo + Chr(13)+Chr(10) ) ) > 0
      nRet :=  hb_inetSendAll( hSockNew, cBuf )
   ENDIF

   hb_inetClose( hSockNew )

   //FtpLog( cBuffer )
   FtpGetReply( hSocket )

   RETURN nRet

STATIC FUNCTION FtpDeleFile( hSocket, cFileName )

   FtpLog( "--- Stor " + cFileName + " ---" )
   IF hb_inetSendAll( hSocket, "DELE " + cFileName + Chr(13)+Chr(10) ) > 0
      IF Left( FtpGetReply( hSocket ),1 ) > '3'
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

STATIC FUNCTION FtpGetReply( hSocket )

   LOCAL cBuffer := "", cBuf, n

   DO WHILE .T.
      IF !Empty( cBuf := hb_inetRecvLine( hSocket ) )
         cBuffer += cBuf
         FtpLog( cBuf )
         IF ( n := Val( Left( cBuf, 1 ) ) ) != 1 .AND. !( Substr(cBuf,4,1) == "-" )
            EXIT
         ENDIF
      ENDIF
   ENDDO

   RETURN cBuffer

STATIC FUNCTION FtpLog( cText )
   //? cText
   edi_Writelog( cText, "_ftp1.log" )

   RETURN Nil
