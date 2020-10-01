/*
 * Lisp interpreter
 */

FUNCTION lisp_Dummy()
   RETURN Nil

FUNCTION lisp_Eval( xText )

   LOCAL s, cEol := Chr(10), i, nPos, c, nLevel, cBuff
   LOCAL lIn := .F., lNeedNext
   LOCAL aErrtxt := { "Pair bracket not found", "Wrong char in a line start" }

   IF Valtype( xText ) == "C"
      IF '(' $ xText
         xText := { xText }
      ELSEIF File( xText )
         xText := MemoRead( xText )
         IF ( i := At( cEol, xText ) ) > 1 .AND. Substr( xText, i-1, 1 ) == Chr(13)
            cEol := Chr(13) + cEol
         ENDIF
         xText := hb_ATokens( xText, cEol )
      ENDIF
   ENDIF

   FOR i := 1 TO Len( xText )
      s := xText[i]
      lNeedNext := .F.
      DO WHILE .T.
         IF lNeedNext
            EXIT
         ENDIF
         nPos := cedi_StrSkipChars( s, 1 )
         IF nPos > Len( s ) .OR. ( c := Substr( s, nPos, 1 ) ) == ';'
            lNeedNext := .T.
            LOOP
         ENDIF
         IF lIn
            IF ( nPos := lisp_GetPairBracket( s, nPos, @nLevel ) ) < 0
               IF i == Len( xText )
                  lisp_Error( aErrTxt[1] )
                  RETURN Nil
               ENDIF
               cBuff += AllTrim( s ) + " "
               lNeedNext := .T.
               LOOP
            ENDIF
         ELSE
            IF c == '('
               nLevel := 0
               cBuff := ""
               IF ( nPos := lisp_GetPairBracket( s, nPos, @nLevel ) ) < 0
                  IF i == Len( xText )
                     lisp_Error( aErrTxt[1] )
                     RETURN Nil
                  ENDIF
                  lIn := .T.
                  cBuff += AllTrim( s ) + " "
                  lNeedNext := .T.
                  LOOP
               ENDIF
            ELSE
               lisp_Error( aErrTxt[2] )
               RETURN Nil
            ENDIF
         ENDIF

         lIn := .F.
         cBuff += LTrim( Left( s, nPos ) )
         lisp_EvalLine( cBuff )

         IF Empty ( s := AllTrim( Substr( s, nPos+1 ) ) )
            EXIT
         ENDIF
      ENDDO
   NEXT

   RETURN Nil

STATIC FUNCTION lisp_GetPairBracket( s, n, nLevel )

   LOCAL c

   DO WHILE ( n := cedi_strpbrk( '"()', s, n ) ) > 0
      IF ( c := Substr( s, n, 1 ) ) == '"'
         IF ( n := hb_At( '"', s, n + 1 ) ) == 0
            RETURN -2
         ENDIF
      ELSEIF c == '('
         nLevel ++
      ELSEIF c == ')'
         nLevel --
         IF nLevel == 0
            RETURN n
         ENDIF
      ENDIF
      n ++
   ENDDO

   RETURN -1

STATIC FUNCTION lisp_Error( s )

   ? "Error:", s

   RETURN Nil

FUNCTION lisp_EvalLine( s )

   ? s

   RETURN Nil

FUNCTION lisp_Info()

   RETURN "Not ready yet..."
