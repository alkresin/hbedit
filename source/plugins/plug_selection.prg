FUNCTION plug_Selection( oEdit, aMenu )

   Aadd( aMenu, {"Test1",@_plug_sele1(),{.F.,8,18}} )
   RETURN Nil

FUNCTION _plug_sele1( oEdit, aParams )

   edi_Alert( "Test1" )
   RETURN Nil
