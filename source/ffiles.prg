
FUNCTION edi_SeleFile( oEdit, cPath, y1, x1, y2, x2 )

   LOCAL aDirTmp := edi_Directory( cPath )
   LOCAL aMenu := Array( Len( aDirTmp ) ), i

   FOR i := 1 TO Len( aMenu )
      aMenu[i] := { aDirTmp[i,1], Nil, Nil, Iif("D" $ aDirTmp[i,5], "<DIR>", Nil ) }
   NEXT

   i := FMenu( oEdit, aMenu, y1, x1, y2, x2 )

   IF i > 0 
      IF Empty(aMenu[i,4])
      ELSE
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION edi_Directory( cPath )

   LOCAL aDirTmp := Directory( cPath, "HSD" )
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
   FOR i := 1 TO Len( aDirTmp )
      IF "D" $ aDirTmp[i,5] .AND. Left( aDirTmp[i,1],1 ) == " "
         aDirTmp[i,1] := Substr( aDirTmp[i,1],2 )
      ENDIF
   NEXT

   RETURN aDirTmp
   
