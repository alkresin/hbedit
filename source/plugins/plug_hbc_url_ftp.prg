
FUNCTION plug_hbc_url_ftp( oPane, cPath, aParams )

   LOCAL cAddr, nPorts, cPath, cLogin := "", cPass := "", lSave := .F.

   IF Empty( aParams )
      IF Empty( cAddr := edi_MsgGet( "FTP Address:" ) )
         RETURN .F.
      ENDIF
   ELSE
      cAddr := aParams[1]
      nPort := Iif( Empty(aParams[2]), 21, Val(aParams[2]) )
      cPath := aParams[3]
      cLogin := aParams[4]
      cPass := aParams[5]
   ENDIF

   IF !hbc_GetLogin( @cLogin, @cPass, @lSave )
      RETURN .F.
   ENDIF

   hb_inetInit()
   IF Empty( hSocket := FtpConnect( cAddr, 21 ) )
      hb_inetCleanup()
      RETURN Nil
   ENDIF

   FtpLogin( hSocket, cLogin, cPass )
   FtpSendCmd( hSocket, "TYPE I" )
   FtpList( hSocket, cPath )

   RETURN .F.

FUNCTION plug_hbc_url_ftp_close( n )

   hb_inetClose( FilePane():aPanes[n]:pSess )

   RETURN Nil

STATIC FUNCTION FtpTest

   LOCAL hSocket, cBuf, cBuffer, nRet, cPass := "", lSave := .F.
   LOCAL cAddr := "ftp.gnu.org", cLogin := "anonymous"

   IF !hbc_GetLogin( @cLogin, @cPass, @lSave )
      RETURN Nil
   ENDIF

   hb_inetInit()
   IF Empty( hSocket := FtpConnect( cAddr, 21 ) )
      hb_inetCleanup()
      RETURN Nil
   ENDIF

   FtpLogin( hSocket, cLogin, cPass )

   FtpSendCmd( hSocket, "TYPE I" )

   FtpList( hSocket, "/" )

   hb_inetClose( hSocket )
   hb_inetCleanup()
   RETURN Nil

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

   FtpLog( "--- User ---" )
   hb_inetSendAll( hSocket, "USER " + cLogin + Chr(13)+Chr(10) )
   FtpGetReply( hSocket )

   FtpLog( "--- Pass ---" )
   hb_inetSendAll( hSocket, "PASS " + cPass + Chr(13)+Chr(10) )
   FtpGetReply( hSocket )

   RETURN Nil

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

   RETURN Nil

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
   ? cText
   edi_Writelog( cText, "_ftp1.log" )

   RETURN Nil
