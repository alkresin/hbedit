/*
{
  "name": "file_read",
  "description": "Read a specified file",
  "parameters":
  {
    "properties":
    {
      "path": { "type": "string", "description": "Path of the file to read" }
    }
  }
}
*/

FUNCTION tool_file_read( oService, pArr )

   LOCAL cErr := "", cFile := "", cFileName

   IF Valtype( pArr ) == "H" .AND. hb_hHasKey( pArr, "path" )

      cFileName := hb_hGetDef( pArr, "path", "" )
      IF ".." $ cFileName
         oService:AddEvent( "tool_result", {"name"=>"file_read","path"=>cFileName,"status"=>"error"}, ;
            "An attempt to go beyond the working directory" )
         RETURN .F.
      ENDIF

      cFile := oService:cProjPath + oService:cWorkDir + hb_ps() + cFileName
      IF File( cFile )
         oService:AddEvent( "tool_result", {"name"=>"file_read","path"=>cFileName}, Memoread( cFile ) )
      ELSE
         cErr := "File " + cFileName + " is not found"
      ENDIF
   ELSE
      cErr := "Path is not set"
   ENDIF

   IF !Empty( cErr )
      oService:AddEvent( "tool_result", {"name"=>"file_read","status"=>"error"}, cErr )
   ENDIF

   RETURN ""