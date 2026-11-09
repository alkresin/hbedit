/*
{
  "name": "file_edit",
  "description": "Replace an exact string in a file. old_string must be unique unless replace_all is set.",
  "parameters":
  {
    "properties":
    {
      "path": { "type": "string", "description": "Path to the file to edit" },
      "old_string": { "type": "string", "description": "Text to replace" },
      "new_string": { "type": "string", "description": "Replacement text" },
      "replace_all": { "type": "bool", "description": "Replace every occurrence" }
    }
  }
}
*/

FUNCTION tool_file_edit( oService, pArr )

   LOCAL cFile, cOld, cNew, lAll, cBuff

   IF Valtype( pArr ) == "H" .AND. hb_hHasKey( pArr, "path" ) .AND. hb_hHasKey( pArr, "old_string" ) .AND. hb_hHasKey( pArr, "new_string" )
      cFile := pArr["path"]
      IF ".." $ cFile
         oService:AddEvent( "tool_result", {"name"=>"file_edit","path"=>cFile,"status"=>"error"}, ;
            "An attempt to go beyond the working directory" )
         RETURN .F.
      ENDIF

      cOld := pArr["old_string"]
      cNew := pArr["new_string"]
      lAll := hb_hGetDef( pArr, "replace_all", .F. )
      cFile := oService:cProjPath + oService:cWorkDir + hb_ps() + cFile
      IF File( cFile )
         cBuff := Memoread( cFile )
         IF cOld $ cBuff
            cBuff := Strtran( cBuff, cOld, cNew,, Iif( lAll,Nil,1 ) )
            hb_MemoWrit( cFile, cBuff )
            oService:AddEvent( "tool_result", {"name"=>"file_edit","path"=>pArr["path"],"status"=>"Ok"} )
         ELSE
            oService:AddEvent( "tool_result", {"name"=>"file_edit","status"=>"error"}, ;
               "old_string is not found in the file " + pArr["path"] )
         ENDIF
      ELSE
         oService:AddEvent( "tool_result", {"name"=>"file_edit","status"=>"error"}, ;
            "File " + pArr["path"] + " is not found" )
      ENDIF
   ELSE
      oService:AddEvent( "tool_result", {"name"=>"file_edit","status"=>"error"}, ;
         "Necessary parameters are not set" )
   ENDIF

   RETURN .T.