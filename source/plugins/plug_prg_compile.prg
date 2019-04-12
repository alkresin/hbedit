Function plug_prg_compile( oEdit )

   LOCAL acmd := Array( 4 ), cHrb, i, cName := ""
   
   acmd[1] := oEdit:ToString()
   acmd[2] := "harbour"
   acmd[3] := "-n2"
   acmd[4] := "-q"

   cedi_rediron( 2, "hb_compile_err.out" )
   cHrb := hb_compileFromBuf( hb_ArrayToParams( acmd ) )
   cedi_rediroff( 2 )
   CLEAR SCREEN

   IF !Empty( cHrb )
      SaveHrb( oEdit, cHrb, cName )
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
      Inkey(0)
   ENDIF

   SetColor( oEdit:cColor )
   oEdit:WriteTopPane( .T. )
   oEdit:TextOut()

   RETURN Nil

STATIC FUNCTION SaveHrb( oEdit, cHrb, cName )

   LOCAL aGets := { {11,22,0,"",33,"W+/BG","W+/BG"}, {12,23,1,.F.,1}, ;
      {14,25,2,"[Search]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {14,40,2,"[Cancel]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL lRes, arr, i, s := "", cEol

   hb_cdpSelect( "RU866" )
   @ 09, 20, 15, 60 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 13, 20 SAY "Ã"
   @ 13, 60 SAY "´"
   @ 13, 21 TO 13, 59
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Compiled successfully! Save as ..."
   @ 12,22 SAY "[ ] Add a line to hbedit.ini"

   aGets[1,4] := cName

   lRes := edi_READ( oEdit, aGets )

   IF lRes .AND. !Empty( cName := aGets[1,4] )
      hb_Memowrit( hb_DirBase() + "plugins" + hb_ps() + cName, cHrb )
      IF aGets[2,4]
         arr := hb_aTokens( MemoRead( hb_DirBase() + "hbedit.ini" ), Chr(10) )
         cEol := Iif( Right(arr[1],1) == Chr(13), Chr(13)+Chr(10), Chr(10) )
         FOR i := 1 TO Len( arr )
            s += arr[i] + Chr(10)
            IF Left( arr[i],9 ) == "[PLUGINS]"
               s += "=" + cName + ",,"
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN Nil
