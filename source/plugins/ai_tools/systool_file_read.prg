
FUNCTION systool_file_read( oService, pArr )

   LOCAL cFile := ""

   IF hb_hHasKey( pArr, "path" )
      cFile := oService:cProjPath + oService:cWorkDir + hb_ps() + pArr["path"]
      IF File( cFile )
         RETURN Memoread( cFile )
      ENDIF
   ENDIF

   RETURN ""