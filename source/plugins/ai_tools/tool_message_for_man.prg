/*
{
  "name": "message_for_man",
  "description": "Send me a message. Use it when you want to say something, make a request, or complain.",
  "parameters":
  {
    "properties":
    {
      "text": { "type": "string", "description": "Text of message" }
    },
    "required": [ "text" ]
  }
}
*/

FUNCTION tool_message_for_man( oService, pArr )

   IF Valtype( pArr ) == "H" .AND. hb_hHasKey( pArr, "text" )
      IF !Empty( oService:pFuncMsg )
         oService:cAskMan := pArr["text"]
      ELSE
         oService:AddEvent( "tool_result", {"name"=>"message_for_man"}, "Сообщение отправлено" )
      ENDIF
   ELSE
      oService:AddEvent( "tool_result", {"name"=>"message_for_man","status"=>"error"}, "Сообщение отсутствует" )
   ENDIF

   RETURN .T.