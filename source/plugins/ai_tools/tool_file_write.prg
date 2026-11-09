/*
{
  "name": "file_write",
  "description": "Write a text to a file, fully replaces the content of a file. Do not use it, if you need to keep previous file content, use the file_append instead.",
  "parameters":
  {
    "properties":
    {
      "path": { "type": "string", "description": "Path of the file to write" },
      "content": { "type": "string", "description": "Text to write" }
    }
  }
}
*/

FUNCTION tool_file_write( oService, pArr )

   LOCAL cFile

   IF Valtype( pArr ) == "H" .AND. hb_hHasKey( pArr, "path" ) .AND. hb_hHasKey( pArr, "content" )
      cFile := pArr["path"]
      IF ".." $ cFile
         oService:AddEvent( "tool_result", {"name"=>"file_write","path"=>cFile,"status"=>"error"}, ;
            "An attempt to go beyond the working directory" )
         RETURN .F.
      ENDIF

      hb_MemoWrit( oService:cProjPath + oService:cWorkDir + hb_ps() + cFile, pArr["content"] )
      oService:AddEvent( "tool_result", {"name"=>"file_write","path"=>pArr["path"]}, "The file has been written" )
   ELSE
      oService:AddEvent( "tool_result", {"name"=>"file_write","status"=>"error"}, ;
         "Path or content is not set" )
   ENDIF

   RETURN .T.