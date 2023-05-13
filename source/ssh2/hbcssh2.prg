/*
 *
 */

FUNCTION hbc_ssh2_Connect( cAddr, nPort, cLogin, cPass )

   LOCAL pSess

   IF Empty( cLogin )
      IF !GetLogin( @cLogin, @cPass )
         RETURN .F.
      ENDIF
   ENDIF

   pSess := ssh2_Connect( cAddr, nPort )
   IF ssh2_LastErr( pSess ) != 0
      ssh2_Close( pSess )
      RETURN .F.
   ENDIF
   IF !ssh2_Login( pSess, cLogin, cPass )
      ssh2_Close( pSess )
      RETURN .F.
   ENDIF
   IF ssh2_Sftp_Init( pSess ) != 0
      ssh2_Close( pSess )
      RETURN .F.
   ENDIF

   RETURN .T.

FUNCTION hb_ssh2_Directory( pSess, cDirSpec, cAttr )

   LOCAL aDir := {}, cDir := hb_fnameDir( cDirSpec )
   LOCAL cName, nAttr, nSize, dDate, cAttr

   IF ssh2_Sftp_OpenDir( pSess, cDir ) == 0
      DO WHILE !Empty( cName := ssh2_Sftp_ReadDir( pSess, @nSize, @dDate, @nAttr ) )
         cAttr := Iif( hb_BitAnd( nAttr, 0x4000 ) > 0, "D", "" )
         Aadd( aDir, { cName, nSize, dDate, "", cAttr } )
      ENDDO
   ENDIF

   RETURN aDir

STATIC FUNCTION GetLogin( cLogin, cPass )
   RETURN .T.

EXIT PROCEDURE hbcExit

   ssh2_Exit()

   RETURN
