Function plug_1c_Spis( oEdit )

   LOCAL i, arr := oEdit:aText, cLine, cfirst, nSkip, arrfnc := {}, n

   FOR i := 1 TO Len( arr )
      cLine := Lower( Ltrim( arr[i] ) )
      nSkip := 0
      cfirst := Lower( hb_TokenPtr( cLine, @nSkip ) )
      IF cfirst == "procedure" .OR. cfirst == "���������" .OR. cfirst == "function" .OR. cfirst == "�������"
         Aadd( arrfnc, { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i } )
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
      IF ( i := FMenu( oEdit, arrfnc, 2, 6,,,,, n, (Len(arrfnc)>36) ) ) > 0
         oEdit:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil
