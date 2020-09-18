Function plug_C_Spis( oEdit )

   LOCAL i, j, n, arr := oEdit:aText, nLen, cLine, nPos, nPos2, c, cLinePrev := "", arrfnc := {}
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
            cLine := cp_Substr( lUtf8,cLine,nPos+2 )
            lComm := .F.
         ENDIF
      ENDIF
      nPos := 1

      DO WHILE nPos <= nLen
         IF ( c := cp_Substr( lUtf8,cLine,nPos,1 ) ) $ cQuotes
            IF ( nPos := cp_At( lUtf8, c, cLine, nPos + 1 ) ) == 0
               EXIT
            ENDIF

         ELSEIF c == '/'
            IF ( c := cp_Substr( lUtf8, cLine, nPos+1, 1 ) ) == '/'
               cLine := Trim( cp_Left( lUtf8,cLine,nPos-1 ) )
               EXIT
            ELSEIF c == '*'
               IF ( nPos2 := cp_At( lUtf8, "*/", cLine, nPos + 2 ) ) == 0
                  cLine := Trim( cp_Left( lUtf8,cLine,nPos-1 ) )
                  lComm := .T.
                  EXIT
               ENDIF
               cLine := Trim( cp_Left( lUtf8,cLine,nPos-1 ) + cp_Substr( lUtf8,cLine,nPos2+2 ) )
            ENDIF

         ELSEIF c == '{'
            IF nLevel == 0
               IF nPos == 1
                  IF Right( cLinePrev, 1 ) == ")"
                     _C_AddF( lUtf8, arrfnc, arr, i, cLinePrev )
                  ENDIF
               ELSE
                  j := nPos
                  DO WHILE ( c := cp_Substr( lUtf8,cLine,--j,1 ) ) == ' ' .AND. j > 1; ENDDO
                  IF c == ')'
                     _C_AddF( lUtf8, arrfnc, arr, i, cLine )
                  ENDIF
               ENDIF
            ENDIF
            nLevel ++

         ELSEIF c == '}'
            nLevel --

         ENDIF
         nPos := cp_NextPos( lUtf8, cLine, nPos )
      ENDDO
      IF !Empty( cLine )
         cLinePrev := cLine
      ENDIF
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

STATIC FUNCTION _C_AddF( lUtf8, arrfnc, arr, nLine, cLinePrev )

   LOCAL j

   IF !( '(' $ cLinePrev )
      FOR j := nLine-2 TO 1 STEP -1
         IF '(' $ arr[j]
            cLinePrev := AllTrim( arr[j] )
            EXIT
         ENDIF
      NEXT
   ENDIF
   Aadd( arrfnc, { cp_Left( lUtf8, cLinePrev, 64 ), Nil, nLine } )

   RETURN Nil
