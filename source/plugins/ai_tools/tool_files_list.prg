/*
{
  "name": "files_list",
  "description": "Read the content of a directory",
  "parameters":
  {
    "properties":
    {
      "path": { "type": "string", "description": "Path to the directory. Pass it empty if you need your base directory" }
    }
  }
}
*/

FUNCTION tool_files_list( oService, pArr )

   LOCAL cErr := "", cMask, cDirName

   IF Valtype( pArr ) == "H"
      cDirName := hb_hGetDef( pArr, "path", "" )
      IF ".." $ cDirName
         oService:AddEvent( "tool_result", {"name"=>"file_list","path"=>cDirName,"status"=>"error"}, ;
            "An attempt to go beyond the working directory" )
         RETURN .F.
      ENDIF

      cMask := oService:cProjPath + oService:cWorkDir + hb_ps() + cDirName + Iif( Empty(cDirName),"","/" )
      //edi_writelog( "path="+cDirName + " mask="+cMask )
      IF hb_DirExists( cMask )
         cMask += "*"
         oService:AddEvent( "tool_result", {"name"=>"files_list","path"=>cDirName}, hb_ValToExp(Directory(cMask,"D")) )
      ELSE
         cErr := "Directory " + cDirName + " is not found"
         oService:AddEvent( "tool_result", {"name"=>"files_list","path"=>cDirName,"status"=>"error"}, cErr )
      ENDIF
   ELSE
      oService:AddEvent( "tool_result", {"name"=>"files_list","status"=>"error"}, "Parameters error" )
   ENDIF

   RETURN ""