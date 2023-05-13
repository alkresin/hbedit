/*
 *
 */

FUNCTION hbc_vfOpen( cFileName, cFlags )

   IF cFileName = "sftp:"
      RETURN -1
   ENDIF
   RETURN hb_vfOpen( cFileName, cFlags )

FUNCTION hbc_vfSize( cFileName )

   IF cFileName = "sftp:"
      RETURN -1
   ENDIF
   RETURN hb_vfSize( xFile )

FUNCTION hbc_vfClose( handle )

   IF cFileName = "sftp:"
      RETURN -1
   ENDIF
   RETURN hb_vfClose( handle )

FUNCTION hbc_vfSeek( handle, nShift )
   RETURN hb_vfSeek( handle, nShift )

FUNCTION hbc_vfReadLen( handle, nSize )
   RETURN hb_vfReadLen( handle, nSize )

FUNCTION hbc_vfWrite( handle, cBuff, nToWrite, nTimeOut )
   RETURN hb_vfWrite( handle, cBuff, nToWrite, nTimeOut )

FUNCTION hbc_vfLoad( cFileName, nMaxSize )

   IF cFileName = "sftp:"
      RETURN Nil
   ENDIF
   RETURN hb_vfLoad( cFileName, nMaxSize )

FUNCTION hbc_vfDirectory( cDirSpec, cAttr )

   IF cFileName = "sftp:"
      RETURN hb_ssh2_Directory( cDirSpec, cAttr )
   ENDIF
   RETURN hb_vfDirectory( cDirSpec, cAttr )

FUNCTION hbc_vfTimeGet( cFileName, tsDateTime )

   IF cFileName = "sftp:"
      RETURN .F.
   ENDIF
   RETURN hb_vfTimeGet( cFileName, @tsDateTime )

FUNCTION hbc_vfCopyFile( cFileSrc, cFileDst )

   IF cFileName = "sftp:"
      RETURN -1
   ENDIF
   RETURN hb_vfCopyFile( cFileSrc, cFileDst )

FUNCTION hbc_vfDirExists( cDirName )

   IF cFileName = "sftp:"
      RETURN .F.
   ENDIF
   RETURN hb_vfDirExists( cDirName )

FUNCTION hbc_vfExists( cFileName )

   IF cFileName = "sftp:"
      RETURN .F.
   ENDIF
   RETURN hb_vfExists( cFileName )

FUNCTION hbc_vfRename( cFileSrc, cFileDst )

   IF cFileName = "sftp:"
      RETURN -1
   ENDIF
   RETURN hb_vfRename( cFileSrc, cFileDst )

FUNCTION hbc_vfErase( cFileName )

   IF cFileName = "sftp:"
      RETURN -1
   ENDIF
   RETURN hb_vfErase( cFileName )

FUNCTION hbc_vfDirRemove( cDirName )

   IF cFileName = "sftp:"
      RETURN -1
   ENDIF
   RETURN hb_vfDirRemove( cDirName )

FUNCTION hbc_vfDirMake( cDirName )

   IF cFileName = "sftp:"
      RETURN -1
   ENDIF
   RETURN hb_vfDirMake( cDirName )

