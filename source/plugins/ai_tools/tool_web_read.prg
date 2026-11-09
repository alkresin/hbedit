/*
{
  "name": "web_read",
  "description": "Read a specified url in Internet and clean it",
  "parameters":
  {
    "properties":
    {
      "url": { "type": "string", "description": "Url of a web page to read" }
    }
  }
}
*/

FUNCTION tool_web_read( oService, pArr )

   LOCAL cUrl, cHtml

   IF Valtype( pArr ) == "H" .AND. hb_hHasKey( pArr, "url" )
      cUrl := hb_hGetDef( pArr, "url", "" )
      IF !Empty( cHtml := hag_GetHtml( cUrl ) )
         oService:AddEvent( "tool_result", {"name"=>"web_read","url"=>cUrl}, ag_CleanHTML( cHtml ) )
      ELSE
         oService:AddEvent( "tool_result", {"name"=>"web_read","url"=>cUrl,"status"=>"error"}, "Can not get " + cUrl )
      ENDIF
   ELSE
      oService:AddEvent( "tool_result", {"name"=>"web_read","status"=>"error"}, "Url is not set" )
   ENDIF

   RETURN ""