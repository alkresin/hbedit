FUNCTION Plug_Go_Fmt( oEdit )

   oEdit:Save()
   cedi_RunConsoleApp( "gofmt -w " + oEdit:cFileName )
   oEdit:SetText( MemoRead( oEdit:cFileName ), oEdit:cFileName )
   edi_Alert( "Done!" )

   RETURN Nil