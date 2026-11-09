/*
{
  "name": "get_time",
  "description": "find out the current time and date",
  "parameters": {}
}
*/

FUNCTION tool_get_time( oService, pArr )

   oService:AddEvent( "tool_result", {"name"=>"get_time"} )
   RETURN ""