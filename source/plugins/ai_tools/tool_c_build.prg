/*
{
  "name": "c_build",
  "description": "Build an application from C sources. Returns the result messages. Use it for C only. Do not use it for Harbour, go, etc.",
  "parameters":
  {
    "properties":
    {
      "list": { "type": "string", "description": "list of C source files, divided by space" }
    }
  }
}
*/

FUNCTION tool_c_build( oService, pArr )

   LOCAL cList, cRes, cCurrDir

   IF Valtype( pArr ) == "H" .AND. hb_hHasKey( pArr, "list" )
      cList := pArr["list"]

      cCurrDir := hb_ps() + Curdir()
      DirChange( oService:cProjPath + oService:cWorkDir )
      cedi_RunConsoleApp( "hwbc " + cList,, @cRes )
      DirChange( cCurrDir )

      oService:AddEvent( "tool_result", {"name"=>"c_build","list"=>cList}, cRes )
   ELSE
      oService:AddEvent( "tool_result", {"name"=>"c_build","status"=>"error"}, ;
         "List of sources is not set" )
   ENDIF

   RETURN .T.