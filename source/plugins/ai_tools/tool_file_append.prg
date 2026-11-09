/*
{
  "name": "file_append",
  "description": "Add a text to the end of a file",
  "parameters":
  {
    "properties":
    {
      "path": { "type": "string", "description": "Path of the file to append" },
      "content": { "type": "string", "description": "Text to append" }
    }
  }
}
*/

FUNCTION tool_file_append( oService, pArr )

   LOCAL fname, cFile, nHand

   IF Valtype( pArr ) == "H" .AND. hb_hHasKey( pArr, "path" ) .AND. hb_hHasKey( pArr, "content" )
      cFile := hb_hGetDef( pArr, "path", "" )
      IF ".." $ cFile
         oService:AddEvent( "tool_result", {"name"=>"file_append","path"=>cFile,"status"=>"error"}, ;
            "An attempt to go beyond the working directory" )
         RETURN .F.
      ENDIF
      fname := oService:cProjPath + oService:cWorkDir + hb_ps() + cFile

      IF !File( fname )
         nHand := FCreate( fname )
      ELSE
         nHand := FOpen( fname, 1 )
      ENDIF
      FSeek( nHand, 0, 2 )
      FWrite( nHand, Chr(10) + pArr["content"] )
      FClose( nHand )

      oService:AddEvent( "tool_result", {"name"=>"file_append","path"=>cFile}, "The content has been added." )
   ELSE
      oService:AddEvent( "tool_result", {"name"=>"file_append","status"=>"error"}, ;
         "Path or content is not set" )
   ENDIF

   RETURN .T.