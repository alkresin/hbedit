Function plug_C_Spis( oEdit )

   LOCAL i, j, n, arr := oEdit:aText, nLen, cLine, nPos, c, cLinePrev := "", arrfnc := {}
   LOCAL lComm := .F., lUtf8 := oEdit:lUtf8, cQuotes := ['"], nLevel := 0

   FOR i := 1 TO Len( arr )
      cLine := AllTrim( arr[i] )
      IF Empty( cLine )
         LOOP
      ENDIF
      nLen := cp_Len( lUtf8, cLine )

      IF !Empty( lComm )
         IF ( nPos := At( "*/", cLine ) ) == 0
            LOOP
         ELSE
            lComm := .F.
            nPos += 2
         ENDIF
      ELSE
         nPos := 1
      ENDIF

      DO WHILE nPos <= nLen
         IF ( c := cp_Substr( lUtf8,cLine,nPos,1 ) ) $ cQuotes
            IF ( nPos := cp_At( lUtf8, c, cLine, nPos + 1 ) ) == 0
               nPos := nLen + 1
            ENDIF

         ELSEIF c == '/'
            IF ( c := cp_Substr( lUtf8, cLine, nPos+1, 1 ) ) == '/'
               nPos := nLen + 1
            ELSEIF c == '*'
               IF ( nPos := cp_At( lUtf8, "*/", cLine, nPos + 2 ) ) == 0
                  nPos := nLen + 1
                  lComm := .T.
               ENDIF
               nPos += 2
            ENDIF

         ELSEIF c == '{'
            IF nLevel == 0
               IF nPos == 1
                  IF !( '(' $ cLinePrev ) .AND. !( Left(cLinePrev,8) == "typedef " )
                     FOR j := i-2 TO 1 STEP -1
                        IF '(' $ arr[j]
                           cLinePrev := AllTrim( arr[j] )
                           EXIT
                        ENDIF
                     NEXT
                  ENDIF
                  Aadd( arrfnc, { cp_Left( lUtf8, cLinePrev, 64 ), Nil, i } )
               ELSE
                  Aadd( arrfnc, { cp_Left( lUtf8, cLine, Min(64,nPos-1) ), Nil, i } )
               ENDIF
            ENDIF
            nLevel ++

         ELSEIF c == '}'
            nLevel --

         ENDIF
         nPos := cp_NextPos( lUtf8, cLine, nPos )
      ENDDO
      cLinePrev := cLine
   NEXT

   IF Empty( arrfnc )
      edi_Alert( "Nothing found..." )
   ELSE
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
