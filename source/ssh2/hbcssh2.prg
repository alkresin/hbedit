/*
 * Ssh2, sftp for HbCommander
 *
 * Copyright 2023 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "fileio.ch"
//#include "hbssh2.ch"

#define  BUFFSIZE  16384
#define  LIBSSH2_SFTP_S_IFDIR        0x04000     /* directory */

FUNCTION hbc_ssh2_Connect( cAddr, nPort, cLogin, cPass, lSave )

   LOCAL pSess

   IF Empty( cLogin ) .OR. Empty( cPass )
      IF !hbc_GetLogin( @cLogin, @cPass, @lSave )
         RETURN .F.
      ENDIF
   ENDIF

   pSess := ssh2_Connect( cAddr, nPort, .T. )
   IF ssh2_LastRes( pSess ) != 0
      _Writelog( "1: " + cAddr + " " + Str(nPort) + " / " + Str(ssh2_LastRes(pSess)) )
      ssh2_Close( pSess )
      RETURN Nil
   ENDIF
   IF !ssh2_Login( pSess, cLogin, cPass )
      _Writelog( "2:" )
      ssh2_Close( pSess )
      RETURN Nil
   ENDIF
   IF ssh2_Sftp_Init( pSess ) != 0
      _Writelog( "3:" )
      ssh2_Close( pSess )
      RETURN Nil
   ENDIF

   RETURN pSess

FUNCTION hbc_ssh2_Directory( pSess, cDirSpec )

   LOCAL pHandle, cDir := StrTran( hb_fnameDir( cDirSpec ), '\', '/' )
   LOCAL cMask := Substr( cDirSpec, Len(cDir)+1 )
   LOCAL aDir := {}, cName, nAttr, nSize, dDate, cAttr

   IF !Empty( pHandle := ssh2_Sftp_OpenDir( pSess, cDir ) )
      DO WHILE !Empty( cName := ssh2_Sftp_ReadDir( pHandle, @nSize, @dDate, @nAttr ) )
         IF Empty( cMask ) .OR. hb_FileMatch( UPPER( cName ), cMask )
            cAttr := Iif( hb_BitAnd( nAttr, LIBSSH2_SFTP_S_IFDIR ) > 0, "D", "" )
            Aadd( aDir, { cName, nSize, dDate, "", cAttr } )
         ENDIF
      ENDDO
      ssh2_sftp_Close( pHandle )
   ELSE
      _Writelog( "Opendir error " + cDir )
   ENDIF

   RETURN aDir

FUNCTION hbc_ssh2_MemoRead( pSess, cFileName )

   LOCAL pHandle, cBuff, cBuffer := ""

   IF '\' $ cFileName
      cFileName := StrTran( cFileName, '\', '/' )
   ENDIF
   IF !Empty( pHandle := ssh2_Sftp_OpenFile( pSess, cFileName ) )
      DO WHILE !Empty( cBuff := ssh2_Sftp_Read( pHandle ) )
         cBuffer += cBuff
      ENDDO
      ssh2_sftp_Close( pHandle )
   ELSE
      _Writelog( "Openfile error " + cFileName )
   ENDIF

   RETURN cBuffer

FUNCTION hbc_ssh2_Download( pSess, cFileName, cFileLocal )

   LOCAL pHandle, handle, cBuff, nSize := 0, nCopied := 0

   CLEAR TYPEAHEAD
   IF '\' $ cFileName
      cFileName := StrTran( cFileName, '\', '/' )
   ENDIF
   IF !Empty( pHandle := ssh2_Sftp_OpenFile( pSess, cFileName ) )
      ssh2_Sftp_FStat( pHandle, @nSize )
      handle := fOpen( cFileLocal, FO_WRITE+FO_CREAT+FO_TRUNC )
      DO WHILE !Empty( cBuff := ssh2_Sftp_Read( pHandle ) )
         fWrite( handle, cBuff )
         nCopied += Len( cBuff )
         IF Inkey() == 27 .AND. !FAsk_Abort( cFileName, nSize, nCopied )
            fClose( handle )
            ssh2_Sftp_Close( pHandle )
            fErase( cFileLocal )
            RETURN -3
         ENDIF
      ENDDO
      fClose( handle )
      ssh2_Sftp_Close( pHandle )
   ELSE
      RETURN -1
   ENDIF

   RETURN 0

FUNCTION hbc_ssh2_Upload( pSess, cFileName, cFileLocal )

   LOCAL pHandle, handle, nBytes, cBuff := Space( BUFFSIZE ), nSize, nCopied := 0

   IF '\' $ cFileName
      cFileName := StrTran( cFileName, '\', '/' )
   ENDIF
   IF !Empty( pHandle := ssh2_Sftp_OpenFile( pSess, cFileName, FO_WRITE + FO_CREAT, ;
      HB_FA_RUSR + HB_FA_WUSR + HB_FA_RGRP + HB_FA_ROTH ) )
      handle := hb_vfOpen( cFileLocal )
      nSize := hb_vfSize( handle )
      DO WHILE ( nBytes := hb_vfRead( handle, @cBuff, BUFFSIZE ) ) > 0
         ssh2_SFtp_Write( pHandle, cBuff, nBytes )
         nCopied += nBytes
         IF Inkey() == 27 .AND. !FAsk_Abort( cFileName, nSize, nCopied )
            hb_vfClose( handle )
            ssh2_Sftp_Close( pHandle )
            ssh2_Sftp_FileDelete( pSess, cFileName )
            RETURN -3
         ENDIF
      ENDDO
      hb_vfClose( handle )
      ssh2_Sftp_Close( pHandle )
   ELSE
      _Writelog( "Openfile error " + cFileName )
      RETURN -1
   ENDIF

   RETURN 0

FUNCTION hbc_ssh2_isFileExists( pSess, cFileName )

   LOCAL nAttr

   IF '\' $ cFileName
      cFileName := StrTran( cFileName, '\', '/' )
   ENDIF
   IF ssh2_Sftp_stat( pSess, cFileName,,, @nAttr ) == 0
      RETURN ( hb_BitAnd( nAttr, LIBSSH2_SFTP_S_IFDIR ) == 0 )
   ENDIF

   RETURN .F.

FUNCTION hbc_ssh2_isDirExists( pSess, cFileName )

   LOCAL nAttr

   IF '\' $ cFileName
      cFileName := StrTran( cFileName, '\', '/' )
   ENDIF
   IF ssh2_Sftp_stat( pSess, cFileName,,, @nAttr ) == 0
      RETURN ( hb_BitAnd( nAttr, LIBSSH2_SFTP_S_IFDIR ) > 0 )
   ENDIF

   RETURN .F.

STATIC FUNCTION _Writelog( cText )
   RETURN edi_Writelog( Dtoc(Date())+" "+Time()+" " + cText, "hbcnet.err" )

EXIT PROCEDURE hbcExit

   ssh2_Exit()

   RETURN
