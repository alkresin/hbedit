Function plug_go_Spis( oEdit )

   Local i, arr := oEdit:aText, cLine, cfirst, nSkip, arrfnc := {}

   FOR i := 1 TO Len( arr )
      cLine := Lower( Ltrim( arr[i] ) )
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF cfirst == "func"
         cFirst := cp_Left( oEdit:lUtf8,arr[i],64 )
         IF Right( cFirst,1 ) == Chr(13)
            cFirst := Left( cFirst, Len(cFirst)-1 )
         ENDIF
         Aadd( arrfnc, { cFirst, Nil, i } )
      ENDIF
   NEXT
   IF !Empty( arrfnc )
      oEdit:TextOut()
      IF ( i := FMenu( oEdit, arrfnc, 5, Int((MaxCol()-66)/2) ) ) > 0
         oEdit:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil
