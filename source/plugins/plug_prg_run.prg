Function plug_prg_run( oEdit )

   LOCAL acmd := Array( 4 ), cHrb, bOldError, i
   
   acmd[1] := oEdit:ToString()
   acmd[2] := "harbour"
   acmd[3] := "-n2"
   acmd[4] := "-q"

   cedi_rediron( 2, "hb_compile_err.out" )
   cHrb := hb_compileFromBuf( hb_ArrayToParams( acmd ) )
   cedi_rediroff( 2 )
   CLEAR SCREEN

   IF !Empty( cHrb )
      bOldError := Errorblock( {|e| MacroError( e ) } )
      BEGIN SEQUENCE
         hb_hrbRun( cHrb )
      END SEQUENCE
      Errorblock( bOldError )
   ELSE
      arr := hb_ATokens( Memoread( "hb_compile_err.out" ), Chr(10) )
      DevPos( 0,0 )
      DevOut( "Compile error." )
      FOR i := 1 TO Min( Len(arr), oEdit:y2-oEdit:y1 )
         DevPos( i,0 )
         DevOut( arr[i] )
      NEXT
      DevPos( oEdit:y2+1,10 )
      DevOut( "Press any key" )
   ENDIF
   Inkey(0)
   SetColor( oEdit:cColor )
   oEdit:WriteTopPane( .T. )
   oEdit:TextOut()

   RETURN Nil
