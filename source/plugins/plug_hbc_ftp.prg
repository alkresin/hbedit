
#include "inkey.ch"
#include "fileio.ch"

#define  READ_BUFF_LEN  4096

STATIC aKeys1 := { K_DOWN, K_UP, K_MWBACKWARD, K_MWFORWARD, K_LEFT, K_RIGHT, ;
   K_PGDN, K_PGUP, K_HOME, K_END, K_TAB, K_CTRL_TAB, K_LBUTTONDOWN, K_RBUTTONDOWN, K_LDBLCLK, ;
   K_ENTER, K_INS, K_CTRL_R, K_CTRL_P, K_CTRL_PGUP, K_F9, K_F10, K_F5, K_F6, K_F7, K_F8, 68, 100 }
STATIC aMonths := { "jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec" }
STATIC cNotPerm := "Operation isn't permitted"

FUNCTION plug_hbc_ftp( oPane, cPlugPath, aParams )

   LOCAL cAddr, cPath, cLogin := "", cPass := "", lSave := .F.
   LOCAL hSocket, nPort
   LOCAL i, aMenu

   IF Empty( aParams )
      aMenu := { "New address" }
      FOR i := 1 TO Len( FilePane():aDefPaths )
         IF FilePane():aDefPaths[i,1] = "ftp:"
            Aadd( aMenu, Substr( FilePane():aDefPaths[i,1],5 ) )
         ENDIF
      NEXT
      NetInfoLoad()
      FOR i := 1 TO Len( FilePane():aNetInfo )
         IF FilePane():aNetInfo[i,1] == "ftp:" .AND. Ascan( aMenu, {|s|s=FilePane():aNetInfo[i,2]} ) == 0
            Aadd( aMenu, FilePane():aNetInfo[i,2] + Iif( Empty(FilePane():aNetInfo[i,3]), "", ;
               ":"+Ltrim(Str(FilePane():aNetInfo[i,3])) ) )
         ENDIF
      NEXT
      IF Len( aMenu ) == 1
         AAdd( aMenu, "ftp.gnu.org" )
      ENDIF
      i := FMenu( oPane, aMenu, oPane:y1+1, oPane:x1+1, oPane:y1+Len(aMenu)+2,, oPane:aClrMenu[1], oPane:aClrMenu[2] )
      IF i == 0
         RETURN .F.
      ELSEIF i == 1
         IF Empty( cAddr := edi_MsgGet( "FTP Address:" ) )
            RETURN .F.
         ELSE
            RETURN oPane:ChangeDir( "ftp:" + cAddr )
         ENDIF
      ELSE
         RETURN oPane:ChangeDir( "ftp:" + aMenu[i] )
      ENDIF
   ELSEIF Empty( aParams[1] )
      RETURN .F.
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

   RETURN 0

FUNCTION plug_hbc_ftp_copyfrom( oPane, aParams )

   LOCAL cFileName, cFileTo, nPos, oPaneTo, cBuffer, i, aDir, nFirst, nSize, dDate

   cFileName := aParams[1]
   cFileTo := aParams[2]
   nFirst := aParams[4]
   nSize := aParams[5]
   dDate := aParams[6]

   IF aParams[3]
      edi_Alert( "From ftp: " + cNotPerm )
      RETURN 2
   ENDIF

   IF hb_vfExists( cFileTo )
      aDir := oPane:aDir[oPane:nCurrent + oPane:nShift]
      hb_vfTimeGet( cFileTo, @i )
      IF !FAsk_Overwrite( nFirst, hb_fnameNameExt(cFileTo), nSize, dDate, hb_vfSize(cFileTo), i )
         RETURN  1
      ENDIF
   ENDIF

   oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   nPos := At( '/', cFileName )
   IF FtpReadFile( oPane:pSess, Substr(cFileName,nPos), cFileTo, nSize )
      IF nFirst == 0
         oPaneTo:Refresh()
         oPaneTo:RedrawAll()
      ENDIF
   ELSE
      RETURN 3
   ENDIF

   RETURN 0

FUNCTION plug_hbc_ftp_copyto( o, aParams )

   LOCAL cFileName, cFileTo, nPos, nFirst, n, aDir, nSize, oPaneCurr

   cFileName := aParams[1]
   cFileTo := aParams[2]
   nFirst := aParams[4]
   nSize := aParams[5]

   //edi_Writelog( cFileName )
   //edi_Writelog( cFileTo )
   IF aParams[3] .OR. !( cFileTo = "ftp:" )
   //IF !( cFileTo = "ftp:" )
      edi_Alert( "To ftp: " + cNotPerm )
      RETURN 2
   ENDIF

/*
   IF aParams[3]
      n := At( '/', cFileTo )
      cFileTo += hb_fnameNameExt( cFileName )
      edi_Writelog( "MKD " + Substr(cFileTo,n) )
      IF !FtpSendCmd( o:pSess, "MKD " + Substr(cFileTo,n) )
         RETURN 2
      ENDIF
      RETURN 0
   ENDIF
*/

   oPaneCurr := Iif( o == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   IF ( n := Ascan2( o:aDir, FTransl( hb_fnameNameExt(cFileName),oPaneCurr:cpPane,o:cpPane ) ) ) > 0
      aDir := o:aDir[n]
      hb_vfTimeGet( cFileName, @n )
      IF !FAsk_Overwrite( nFirst, hb_fnameNameExt(cFileName), hb_vfSize(cFileName), n, aDir[2], aDir[3] )
         RETURN  1
      ENDIF
   ENDIF

   nPos := At( '/', cFileTo )
   IF ( n := FtpWriteFile( o:pSess, cFileName, Substr( cFileTo, nPos ), nSize ) ) <= 0
      RETURN Iif( n == -3, 3, 2 )
   ENDIF
   IF nFirst == 0
      o:Refresh()
      o:RedrawAll()
   ENDIF

   RETURN 0

FUNCTION plug_hbc_ftp_delete( oPane, aParams )

   LOCAL cFileName, nPos

   cFileName := aParams[1]

   IF !( cFileName = "ftp:" )
      edi_Alert( cNotPerm )
      RETURN 2
   ENDIF

   nPos := At( '/', cFileName )
   //IF !FtpDeleFile( oPane:pSess, Substr(cFileName,nPos) )
   IF aParams[2]
      IF !FtpSendCmd( oPane:pSess, "RMD " + Substr(cFileName,nPos) )
         RETURN 2
      ENDIF
   ELSE
      IF !FtpSendCmd( oPane:pSess, "DELE " + Substr(cFileName,nPos) )
         RETURN 2
      ENDIF
   ENDIF
   RETURN 0

FUNCTION plug_hbc_ftp_mkdir( oPane, aParams )

   LOCAL cDirName, nPos

   cDirName := aParams[1]

   IF !( cDirName = "ftp:" )
      edi_Alert( cNotPerm )
      RETURN 2
   ENDIF

   nPos := At( '/', cDirName )
   //edi_Writelog( "MKD " + Substr(cDirName,nPos) )
   IF !FtpSendCmd( oPane:pSess, "MKD " + Substr(cDirName,nPos) )
      RETURN 2
   ENDIF

   RETURN 0

STATIC FUNCTION _plug_Refresh( oPane )

   oPane:aDir := FtpList( oPane:pSess, oPane:cCurrPath )
   IF Empty( oPane:aDir )
      AAdd( oPane:aDir, { "..", 0, Date(), "", "D" } )
   ENDIF

   RETURN 0

STATIC FUNCTION _plug_OnKey( oPane, nKeyExt )

   LOCAL nKey := hb_keyStd( nKeyExt ), cBuffer, cFileTo, cName, o, aDir

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE
      RETURN -1
   ENDIF
   IF Ascan( aKeys1, nKey ) > 0
      RETURN 0
   ENDIF

   IF nKey == K_F4
      aDir := oPane:aDir[oPane:nCurrent + oPane:nShift]
      IF 'D' $ aDir[5]
         RETURN 0
      ELSE
         o := TEdit():aWindows[TEdit():nCurr]
         cName := aDir[1]
         IF !Empty( cBuffer := FtpReadFile( oPane:pSess, oPane:cCurrPath + cName,, aDir[2] ) )
            mnu_NewBuf( o, oPane:cIOpref + oPane:net_cAddress + oPane:net_cPort + ;
            oPane:cCurrPath + cName, cBuffer, ):lReadOnly := .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN -1

STATIC FUNCTION FtpConnect( cAddr, nPort )

   LOCAL hSocket, nErr

   hSocket := hb_inetCreate()
   hb_inetTimeout( hSocket , 3000 )
   hb_inetConnect( cAddr, nPort, hSocket )
   IF ( nErr := hb_inetErrorCode( hSocket ) ) != 0
      edi_Alert( "Connect failed: " + Ltrim(Str(nErr)) )
      FtpLog( "Connect failed: " + Ltrim(Str(nErr)) )
      hb_inetClose( hSocket )
      RETURN Nil
   ENDIF
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
   IF hb_inetSendAll( hSocket, cmd + Chr(13)+Chr(10) ) > 0
      IF Left( FtpGetReply( hSocket ),1 ) > '3'
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

STATIC FUNCTION FtpPASV( hSocket )

   LOCAL cNewIp, cNewPort, cBuf, nPos, aPasv, hSockNew

   FtpLog( "--- Pasv ---" )
   hb_inetSendAll( hSocket, "PASV" + Chr(13)+Chr(10) )

   cBuf := FtpGetReply( hSocket )

   IF ( nPos := At( ',', cBuf ) ) == 0
      FtpLog( "PASV Error1" )
      RETURN Nil
   ENDIF
   nPos --
   DO WHILE IsDigit( Substr( cBuf,nPos,1 ) )
      nPos --
   ENDDO
   cBuf := Substr( cBuf, nPos+1 )
   aPasv := hb_ATokens( cBuf, ',' )
   IF Len( aPasv ) < 6
      FtpLog( "PASV Error2" )
      RETURN Nil
   ENDIF
   cNewIp := Alltrim(aPasv[1])+"."+ALLtrim(aPasv[2])+"."+ALLtrim(aPasv[3])+"."+ALLtrim(aPasv[4])
   cNewPort := Val(aPasv[5])*256 +Val(aPasv[6])

   hSockNew := hb_inetCreate()
   hb_inetConnect( cNewIp, cNewPort, hSockNew )

   RETURN hSockNew

STATIC FUNCTION FtpList( hSocket, cPath )

   LOCAL hSockNew, cBuffer := "", cBuf, nRet, nPos
   LOCAL aDir := {}, arr, arr2, i, j, cName, nSize, cAttr, dDate, cYear

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
      IF !Empty( arr[i] ) .AND. Len( arr2 := hb_Atokens( arr[i] ) ) >= 9
         cName := Iif( Right(arr2[9],1) == Chr(13), hb_strShrink(arr2[9],1), arr2[9] )
         IF !( cName == "." )
            IF Len(arr2) > 9
               nPos := At( arr2[6], arr[i] ) + 1
               nPos := hb_At( arr2[9], arr[i], nPos )
               cName := Substr( arr[i], nPos )
               IF Right( cName,1 ) == Chr(13)
                  cName := hb_strShrink( cName,1 )
               ENDIF
            ENDIF
            cAttr := Upper( arr2[1] )
            nSize := Iif( "D" $ cAttr, 0, Val(arr2[5]) )
            IF ( j := Ascan( aMonths, Lower(arr2[6]) ) ) > 0
               cYear := Iif( ':' $ arr2[8], Str( Year(Date()),4 ), arr2[8] )
               dDate := Stod( cYear + PAdl( Ltrim(Str(j)),2,'0' ) + PAdl( arr2[7],2,'0' ) )
            ELSE
               dDate := Stod( "19000101" )
            ENDIF
            AAdd( aDir, { cName, nSize, dDate, "", cAttr } )
         ENDIF
      ENDIF
   NEXT

   RETURN aDir

STATIC FUNCTION FtpReadFile( hSocket, cFileName, cFileTo, nSize )

   LOCAL hSockNew, cBuffer := "", cBuf, handle, nRet, lToFile := .F., nCopied := 0, lRes := .T.
   LOCAL aWnd, oPaneCurr := FilePane():PaneCurr()

   IF !Empty( cFileTo )
      IF Empty( handle := hb_vfOpen( cFileTo, FO_WRITE+FO_CREAT+FO_TRUNC ) )
         edi_Alert( "Can't create " + cFileTo )
         RETURN .F.
      ENDIF
      lToFile := .T.
   ENDIF

   aWnd := hbc_Wndinit( 05, oPaneCurr:vx1+12, 08, oPaneCurr:vx2-12,, "Copy" )
   hbc_Wndout( aWnd, FTransl( hb_fnameNameExt( cFileName ) ) )
   hbc_Wndout( aWnd, "" )
   hbc_WndProgress( aWnd, 0 )
   IF Empty( hSockNew := FtpPASV( hSocket ) )
      hbc_Wndclose( aWnd )
      RETURN Iif( lToFile, .F., Nil )
   ENDIF

   hb_inetTimeout( hSockNew , 10000 )

   FtpLog( "--- Retr " + cFileName + " ---" )
   hb_inetSendAll( hSocket, "RETR " + cFileName + Chr(13)+Chr(10) )

   cBuf := Space( READ_BUFF_LEN )
   DO WHILE ( nRet := hb_inetRecvAll( hSockNew, @cBuf, READ_BUFF_LEN ) ) > 0
      nCopied += nRet
      IF lToFile
         hb_vfWrite( handle, Iif( nRet == READ_BUFF_LEN, cBuf, Left( cBuf,nRet ) ) )
      ELSE
         cBuffer += Iif( nRet == READ_BUFF_LEN, cBuf, Left( cBuf,nRet ) )
      ENDIF
      IF Inkey() == 27 .AND. !FAsk_Abort( cFileName, nSize, nCopied )
         lRes := .F.
         EXIT
      ENDIF
      hbc_WndProgress( aWnd, nCopied / nSize )
   ENDDO

   IF lToFile
      hb_vfClose( handle )
      IF !lRes
         hb_vfErase( cFileTo )
      ENDIF
   ENDIF

   hb_inetClose( hSockNew )
   FtpGetReply( hSocket )
   hbc_Wndclose( aWnd )

   RETURN Iif( lToFile, lRes, Iif( lRes, cBuffer, Nil ) )

STATIC FUNCTION FtpWriteFile( hSocket, cFileName, cFileTo, nSize )

   LOCAL hSockNew, cBuffer := "", cBuf, handle, nRet, nCopied := 0, lRes := .T.
   LOCAL oPaneCurr := FilePane():PaneCurr(), aWnd

   IF Empty( handle := hb_vfOpen( cFileName, FO_READ ) )
      edi_Alert( "Can't open " + cFileName )
      RETURN -1
   ENDIF

   aWnd := hbc_Wndinit( 05, oPaneCurr:vx1+12, 08, oPaneCurr:vx2-12,, "Copy" )
   hbc_Wndout( aWnd, FTransl( hb_fnameNameExt( cFileName ) ) )
   hbc_Wndout( aWnd, "" )
   hbc_WndProgress( aWnd, 0 )
   IF Empty( hSockNew := FtpPASV( hSocket ) )
      RETURN Nil
   ENDIF

   FtpLog( "--- Stor " + cFileTo + " ---" )
   IF ( nRet := hb_inetSendAll( hSocket, "STOR " + cFileTo + Chr(13)+Chr(10) ) ) > 0
      cBuf := Space( READ_BUFF_LEN )
      DO WHILE ( nRet := hb_vfRead( handle, @cBuf, READ_BUFF_LEN ) ) > 0 .AND. ;
         ( nRet :=  hb_inetSendAll( hSockNew, cBuf, nRet ) ) > 0
         nCopied += nRet
         IF Inkey() == 27 .AND. !FAsk_Abort( cFileName, nSize, nCopied )
            lRes := .F.
            EXIT
         ENDIF
         hbc_WndProgress( aWnd, nCopied / nSize )
      ENDDO
      nRet := Iif( nRet<0, -1, 1 )
   ELSE
      nRet := -1
   ENDIF

   hb_vfClose( handle )
   hb_inetClose( hSockNew )
   IF !lRes
      FtpSendCmd( hSocket, "DELE " + cFileTo )
   ENDIF

   //FtpLog( cBuffer )
   FtpGetReply( hSocket )
   hbc_Wndclose( aWnd )

   RETURN Iif( lRes, nRet, -3 )

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
   //edi_Writelog( cText, "_ftp1.log" )

   RETURN Nil