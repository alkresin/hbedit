Function plug_prg_Spis( oEdit )

   Local i, arr := oEdit:aText, cLine, cfirst, cSecond, nSkip, arrfnc := {}, lClassDef := .F., cItem

   FOR i := 1 TO Len( arr )
      cLine := Lower( Ltrim( arr[i] ) )
      nSkip := 0
      cfirst := hb_TokenPtr( cLine, @nSkip )
      IF cfirst == "function" .OR. cfirst == "procedure" .OR. ;
            ( cfirst == "method" .AND. !lClassDef ) .OR. cfirst == "func" .OR. ;
            cfirst == "proc" .OR. ( cfirst == "static" .AND. ;
            ( ( cSecond := hb_TokenPtr( cLine, @nSkip ) ) == "function" .OR. ;
            cSecond == "procedure" .OR. cSecond == "func" .OR. cSecond == "proc" ) )
         Aadd( arrfnc, { cp_Left( oEdit:lUtf8,arr[i],64 ), Nil, i } )
      ENDIF
      IF cfirst == "class" .or. ( cfirst == "create" .AND. ;
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
      IF ( i := FMenu( oEdit, arrfnc, 5, Int((MaxCol()-66)/2) ) ) > 0
         oEdit:Goto( arrfnc[i,3] )
      ENDIF
   ENDIF

   RETURN Nil

