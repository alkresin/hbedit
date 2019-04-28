Function plug_prg_compile( oEdit )

   LOCAL acmd := Array( 4 ), cHrb, i, cName := "", cTemp, nPos
   
   acmd[1] := oEdit:ToString()
   acmd[2] := "harbour"
   acmd[3] := "-n2"
   acmd[4] := "-q"

   cedi_rediron( 2, "hb_compile_err.out" )
   cHrb := hb_compileFromBuf( hb_ArrayToParams( acmd ) )
   cedi_rediroff( 2 )
   CLEAR SCREEN

   FOR i := 1 TO Len( oEdit:aText )
      IF ( cTemp := Lower(Left(oEdit:aText[i],8)) ) == "function" .OR. cTemp == "procedur"
         IF ( nPos := hb_At( " ", oEdit:aText[i], 8 ) ) > 0
            cTemp := Ltrim( Substr( oEdit:aText[i], nPos + 1 ) )
            IF ( nPos := hb_At( "(", cTemp ) ) > 0
               cName := Lower( Left( cTemp, nPos-1 ) ) + ".hrb"
            ELSE
               cName := Lower( cTemp ) + ".hrb"
            ENDIF
            EXIT
         ENDIF
      ENDIF
   NEXT

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
   oEdit:WriteTopPane()
   oEdit:TextOut()

   RETURN Nil

STATIC FUNCTION SaveHrb( oEdit, cHrb, cName )

   LOCAL aGets := { {11,22,0,"",33,"W+/BG","W+/BG"}, ;
      {12,29,0,"",26,"W+/BG","W+/BG"}, {13,23,1,.F.,1}, ;
      {15,25,2,"[Save]",8,"N/W","W+/BG",{||__KeyBoard(Chr(13))}}, ;
      {15,40,2,"[Cancel]",10,"N/W","W+/BG",{||__KeyBoard(Chr(27))}} }
   LOCAL oldc, nRes, arr, i, s := "", cEol, cFull, cTitle

   oldc := SetColor( "N/W,N/W,,,N/W" )
   hb_cdpSelect( "RU866" )
   @ 09, 20, 16, 60 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 14, 20 SAY "Ã"
   @ 14, 60 SAY "´"
   @ 14, 21 TO 14, 59
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Compiled successfully! Save as ..."
   @ 12,22 SAY "Title:"
   @ 13,22 SAY "[ ] Add a line to hbedit.ini"

   aGets[1,4] := cName

   nRes := edi_READ( aGets )
   SetColor( oldc )

   IF nRes > 0 .AND. nRes < Len(aGets) .AND. !Empty( cName := aGets[1,4] )
      cTitle := Iif( Empty(aGets[2,4]), cName, hb_fnameName(aGets[2,4]) )
      cFull := hb_DirBase() + "plugins" + hb_ps() + cName
      IF File( cFull ) .AND. edi_Alert( cName + " already exist. Overwrite it?", "Yes", "No" ) != 1
         RETURN Nil
      ENDIF
      hb_Memowrit( cFull, cHrb )
      IF aGets[3,4] .AND. Ascan( TEdit():aPlugins, {|a|a[1]==cName} ) == 0
         arr := hb_aTokens( MemoRead( hb_DirBase() + "hbedit.ini" ), Chr(10) )
         cEol := Iif( Right(arr[1],1) == Chr(13), Chr(13)+Chr(10), Chr(10) )
         FOR i := 1 TO Len( arr )
            s += arr[i] + Chr(10)
            IF Left( arr[i],9 ) == "[PLUGINS]"
               s += "p" + Ltrim(Str(Len(TEdit():aPlugins)+1)) + "=" + cName + ",," + ;
                  cTitle + cEol
            ENDIF
         NEXT
         hb_MemoWrit( hb_DirBase() + "hbedit.ini", s )
         AAdd( TEdit():aPlugins, { cName, cTitle, "", Nil } )
      ENDIF
   ENDIF

   RETURN Nil
