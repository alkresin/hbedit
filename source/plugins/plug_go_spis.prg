Function plug_go_Spis( oEdit )

   Local i, arr := oEdit:aText, cLine, cfirst, nSkip, arrfnc := {}

   FOR i := 1 TO Len( arr )
      cLine := Lower( Ltrim( arr[i] ) )
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF cfirst == "func"
         Aadd( arrfnc, { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i } )
      ENDIF
   NEXT
   IF !Empty( arrfnc )
      oEdit:TextOut()
      n := oEdit:nRow - oEdit:y1 + oEdit:nyFirst
      FOR i := 1 TO Len( arrfnc )
         IF arrfnc[i,3] > n
            n := i - 1
            EXIT
         ENDIF
      NEXT
      n := Iif( n > Len(arrfnc), Len(arrfnc), Iif( n == 0, 1, n ) )
      IF ( i := FMenu( oEdit, arrfnc, 5, Int((MaxCol()-66)/2),,,,, n ) ) > 0
         oEdit:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil
