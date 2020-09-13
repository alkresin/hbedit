FUNCTION plug_prg_Spis( oEdit )

   LOCAL i, n, arr := oEdit:aText, cLine, cfirst, cSecond, nSkip, arrfnc := {}, lClassDef := .F.
   LOCAL aDop := Iif( !Empty(oEdit:oHili) .AND. !Empty(oEdit:oHili:aDop), oEdit:oHili:aDop, Nil )

   FOR i := 1 TO Len( arr )
      cLine := Lower( Ltrim( arr[i] ) )
      IF i > 1 .AND. !Empty( aDop )
         IF aDop[i-1] == 1
            IF ( n := At( "*/", cLine ) ) > 0
               cLine := Ltrim( Substr( cLine,n+2 ) )
            ELSE
               LOOP
            ENDIF
         ENDIF
      ENDIF
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF cfirst == "function" .OR. cfirst == "procedure" .OR. ;
            ( cfirst == "method" .AND. (!lClassDef .OR. " inline " $ cLine .OR. " block " $ cLine )) ;
            .OR. cfirst == "func" .OR. cfirst == "proc" .OR. ( cfirst == "static" .AND. ;
            ( ( cSecond := hb_TokenPtr( cLine, @nSkip ) ) == "function" .OR. ;
            cSecond == "procedure" .OR. cSecond == "func" .OR. cSecond == "proc" ) )
         Aadd( arrfnc, { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i } )
      ELSEIF cfirst == "class" .or. ( cfirst == "create" .AND. ;
            ( cSecond := hb_TokenPtr( cLine, @nSkip ) ) == "class" )
         IF cfirst == "create" .OR. ( !( ( cSecond := hb_TokenPtr( cLine, @nSkip ) ) == "var" ) ;
               .AND. !( cSecond == "data" ) )
            lClassDef := .T.
            Aadd( arrfnc, { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i } )
         ENDIF
      ELSEIF cfirst == "end" .or. cfirst == "endclass"
         lClassDef := .F.
      ENDIF
   NEXT
   IF !Empty( arrfnc )
      oEdit:TextOut()
      n := oEdit:nLine
      FOR i := 1 TO Len( arrfnc )
         IF arrfnc[i,3] > n
            n := i - 1
            EXIT
         ENDIF
      NEXT
      n := Iif( n > Len(arrfnc), Len(arrfnc), Iif( n == 0, 1, n ) )
      IF ( i := FMenu( oEdit, arrfnc, 2, 6,,,,, n, (Len(arrfnc)>3) ) ) > 0
         oEdit:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil
