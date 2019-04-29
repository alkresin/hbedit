
FUNCTION edi_SeleFile( oEdit, cPath, y1, x1, y2, x2 )

   LOCAL aMenu := edi_Directory( cPath ), i, nPos

   DO WHILE .T.
      i := FMenu( oEdit, aMenu, y1, x1, y2, x2,,,, .T. )

      IF i > 0
         IF Empty(aMenu[i,4])
            RETURN cPath + aMenu[i,1]
         ELSE
            IF aMenu[i,1] == ".."
               IF ( nPos := hb_Rat( hb_ps(), cPath,, Len(cPath)-1 ) ) > 0
                  cPath := Left( cPath, nPos )
                  aMenu := edi_Directory( cPath )
               ENDIF
            ELSE
               cPath += aMenu[i,1] + hb_ps()
               aMenu := edi_Directory( cPath )
            ENDIF
         ENDIF
      ELSE
         RETURN Nil
      ENDIF
   ENDDO

   RETURN Nil

FUNCTION edi_Directory( cPath )

   LOCAL aDirTmp := Directory( cPath, "HSD" ), aMenu
   LOCAL i, l1 := .F., l2 := .F., nPos

   FOR i := 1 TO Len( aDirTmp )
      IF Empty( aDirTmp[i] )
         LOOP
      ELSEIF aDirTmp[i,1] == "."
         ADel( aDirTmp, i )
         i --
         l1 := .T.
      ELSEIF "D" $ aDirTmp[i,5]
         IF aDirTmp[i,1] == ".."
            l2 := .T.
         ENDIF
         aDirTmp[i,1] := " " + aDirTmp[i,1]
      ENDIF
   NEXT
   IF l1
      aDirTmp := ASize( aDirTmp, Len(aDirTmp)-1 )
   ENDIF
   IF !l2
      nPos := Len( cPath ) - Iif( Right(cPath,1) $ "\/", 1, 0 )
      IF ( hb_Rat( '/',cPath,nPos ) != 0 .OR. hb_Rat( '\',cPath,nPos ) != 0 ) .AND. Substr(cPath,2,1) != ':'
         Aadd( aDirTmp, Nil )
         AIns( aDirTmp, 1 )
         aDirTmp[1] := { " ..",0,Date(),"","D" }
      ENDIF
   ENDIF
   aDirTmp := ASort( aDirTmp,,, {|z,y|Lower(z[1]) < Lower(y[1])} )
   aMenu := Array( Len( aDirTmp ) )
   FOR i := 1 TO Len( aDirTmp )
      IF "D" $ aDirTmp[i,5] .AND. Left( aDirTmp[i,1],1 ) == " "
         aDirTmp[i,1] := Substr( aDirTmp[i,1],2 )
      ENDIF
      aMenu[i] := { aDirTmp[i,1], Nil, Nil, Iif("D" $ aDirTmp[i,5], "<DIR>", Nil ) }
   NEXT

   RETURN aMenu

FUNCTION edi_IniRead( cFileName )

   LOCAL cText := Memoread( cFileName ), aText, i, s, nPos
   LOCAL hIni, hSect

   IF Empty( cText )
      RETURN Nil
   ENDIF

   aText := hb_aTokens( cText, Chr(10) )
   hIni := hb_Hash()

   FOR i := 1 TO Len( aText )
      s := Iif( Left( aText[i],1 ) == ' ', Ltrim( aText[i] ), aText[i] )
      IF Left( s, 1 ) $ ";#"
         LOOP
      ENDIF
      s := Trim( Iif( Right(s,1)==Chr(13), Left( s,Len(s)-1 ), s ) )
      IF Empty( s )
         LOOP
      ENDIF

      IF Left( s,1 ) == '[' .AND. Right( s,1 ) == ']'
         hSect := hIni[Substr( s,2,Len(s)-2 )] := hb_Hash()
      ELSE
         IF ( nPos := At( '=', s ) ) > 0
            hSect[Trim(Left(s,nPos-1))] := Ltrim( Substr( s,nPos+1 ) )
         ENDIF
      ENDIF
   NEXT

   RETURN hIni

