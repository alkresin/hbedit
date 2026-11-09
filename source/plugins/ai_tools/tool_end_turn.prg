/*
{
  "name": "end_turn",
  "description": "finish the current awakening and fall asleep",
  "parameters": {}
}
*/

FUNCTION tool_end_turn( oService, pArr )

   oService:lFinish := .T.
   RETURN ""