/*
 * Lisp interpreter
 */

#define TYPE_ERR   -1
#define TYPE_ATOM   1
#define TYPE_LIST   2
#define TYPE_EXPR   3
#define TYPE_LAMBDA 4

#define ERR_PAIRBRACKET    1
#define ERR_WRONGSTARTCHAR 2
#define ERR_PAIRQUOTE      3
#define ERR_BRA_EXPECTED   4
#define ERR_LIST_EXPECTED  5
#define ERR_LOGIC_EXPECTED 6
#define ERR_WRONG_PARAM_NUMBER 7

STATIC nLispErr := 0
STATIC aErrtxt := { "Pair bracket not found", "Wrong char in a line start", "Pair quote not found", ;
   "Left bracket expected", "List expected", "Logical value expected", "Wrong number of parameters" }

FUNCTION lisp_Run()
   RETURN Nil

FUNCTION lisp_Eval( xText )

   LOCAL s, cEol := Chr(10), i, nPos, c, nLevel, cBuff
   LOCAL lIn := .F., lNeedNext

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
                  lisp_Error( aErrTxt[ERR_PAIRBRACKET] )
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
                     lisp_Error( aErrTxt[ERR_PAIRBRACKET] )
                     RETURN Nil
                  ENDIF
                  lIn := .T.
                  cBuff += AllTrim( s ) + " "
                  lNeedNext := .T.
                  LOOP
               ENDIF
            ELSE
               lisp_Error( aErrTxt[ERR_WRONGSTARTCHAR] )
               RETURN Nil
            ENDIF
         ENDIF

         lIn := .F.
         cBuff += LTrim( Left( s, nPos ) )
         ? lisp_EvalExpr( cBuff )
         lisp_Error()

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

   IF Empty( s ) .AND. nLispErr > 0
      s := aErrtxt[nLispErr]
   ENDIF
   IF !Empty( s )
      ? "Error:", s
   ENDIF

   RETURN Nil

FUNCTION lisp_EvalExpr( s, nType )

   LOCAL nPos, nPos2, cmd, nGetType, nGetType2, cNext, cExpr, lRes
   LOCAL aLambda, i, c

   nLispErr := 0
   nPos := cedi_strSkipChars( s, 2 )
   //edi_Alert( "0> " + s )
   IF Left( s, 1 ) == "'"
      cNext := lisp_GetNextExpr( s, nPos )
      IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
      nType := Iif( Left( cNext,1 ) == '(', TYPE_LIST, TYPE_ATOM )
      //edi_Alert( "0a> " + ltrim(str(ntype))+"/"+ ltrim(str(npos))+"/"+cNext )
      RETURN cNext

   ELSEIF Substr( s, nPos, 1 ) == '('
      cNext := lisp_GetNextExpr( s, nPos )
      IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
      cNext := lisp_EvalExpr( cNext, @nGetType )
      IF nGetType == TYPE_LAMBDA
         cExpr := cNext[1]
         i := 1
         DO WHILE !Empty( c := lisp_GetNextExpr( s, @nPos ) )
            i ++
            IF i > Len( aLambda )
               nLispErr := ERR_WRONG_PARAM_NUMBER; nType := TYPE_ERR;  RETURN Nil
            ENDIF
            c := lisp_EvalExpr( c, @nGetType )
            cExpr := hb_strReplace( cExpr, { ' '+aLambda[i]+' ', '('+aLambda[i]+' ', ;
               ' '+aLambda[i]+')', '('+aLambda[i]+')' }, { ' '+c+' ', '('+c+' ', ;
               ' '+c+')', '('+c+')' } )
         ENDDO
         IF i != Len( aLambda )
            nLispErr := ERR_WRONG_PARAM_NUMBER; nType := TYPE_ERR;  RETURN Nil
         ENDIF
         c := lisp_EvalExpr( c, @nGetType )
         IF Valtype( c ) == "L"
            c := Iif( c, "T", "()" )
         ENDIF
         RETURN c
      ELSE
      ENDIF

   ELSEIF Substr( s, nPos, 1 ) == ')'
      nType := TYPE_ATOM
      RETURN .F.
   ELSE
      nPos2 := cedi_strPBrk( " )", s, nPos+1 )
      cmd := Lower( Substr( s, nPos, nPos2-nPos ) )
      nPos := nPos2
      //edi_Alert( "1> " + ltrim(str(nPos))+"/"+s )
      cNext := lisp_GetNextExpr( s, @nPos )
      //edi_Alert( "2> " + cNext )
      IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF

      SWITCH cmd
      CASE "quote"
         nType := Iif( Left( cNext,1 ) == '(', TYPE_LIST, TYPE_ATOM )
         RETURN cNext

      CASE "atom"
         nType := TYPE_ATOM
         IF Left( cNext,1 ) $ "('"
            lisp_EvalExpr( cNext, @nGetType )
            RETURN ( nGetType == TYPE_ATOM )
         ELSE
            RETURN .T.
         ENDIF

      CASE "car"
         IF Left( cNext,1 ) $ "('"
            cNext := lisp_EvalExpr( cNext, @nGetType )
            IF nGetType == TYPE_LIST
               cNext := lisp_GetNextExpr( cNext, 2 )
               IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
               nType := Iif( Left( cNext,1 ) == '(', TYPE_LIST, TYPE_ATOM )
               RETURN cNext
            ELSE
               nLispErr := ERR_LIST_EXPECTED; nType := TYPE_ERR; RETURN Nil
            ENDIF
         ELSE
            nLispErr := ERR_BRA_EXPECTED; nType := TYPE_ERR;  RETURN Nil
         ENDIF

      CASE "cdr"
         IF Left( cNext,1 ) $ "('"
            cNext := lisp_EvalExpr( cNext, @nGetType )
            IF nGetType == TYPE_LIST
               nPos := 2
               lisp_GetNextExpr( cNext, @nPos )
               IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
               cNext := '(' + Substr( cNext, nPos )
               nPos := cedi_strSkipChars( cNext, 2 )
               IF Substr( cNext, nPos, 1 ) == ')'
                  nType := TYPE_ATOM
                  RETURN .F.
               ELSE
                  nType := TYPE_LIST
                  RETURN cNext
               ENDIF
            ELSE
               nLispErr := ERR_LIST_EXPECTED; nType := TYPE_ERR; RETURN Nil
            ENDIF
         ELSE
            nLispErr := ERR_BRA_EXPECTED; nType := TYPE_ERR; RETURN Nil
         ENDIF

      CASE "cond"
         DO WHILE .T.
            nPos2 := 2
            cExpr := lisp_GetNextExpr( cNext, @nPos2 )
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
            lRes := lisp_EvalExpr( cExpr, @nGetType )
            IF Valtype( lRes ) == "L"
               IF lRes
                  cExpr := lisp_GetNextExpr( cNext, @nPos2 )
                  IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
                  RETURN lisp_EvalExpr( cExpr, @nGetType )
               ENDIF
            ELSE
               nLispErr := ERR_LOGIC_EXPECTED; nType := TYPE_ERR; RETURN Nil
            ENDIF
            IF Empty( cNext := lisp_GetNextExpr( s, @nPos ) )
               EXIT
            ENDIF
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         ENDDO
         nType := TYPE_ATOM
         RETURN .F.

      CASE "cons"
         nType := TYPE_LIST
         IF Left( cNext,1 ) $ "('"
            cNext := lisp_EvalExpr( cNext, @nGetType )
         ENDIF

         cExpr := lisp_GetNextExpr( s, @nPos )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         IF Left( cExpr,1 ) $ "('"
            cExpr := lisp_EvalExpr( cExpr, @nGetType2 )
         ENDIF

         IF nGetType == TYPE_ATOM .AND. Valtype( cNext ) == "L"
            cNext := Iif( cNext, "T", "()" )
         ENDIF
         IF nGetType2 == TYPE_LIST
            RETURN "( " + cNext + Substr( cExpr, 2 )
         ELSE
            IF Valtype( cExpr ) == "L"
               cExpr := Iif( cExpr, "T", "()" )
            ENDIF
            RETURN "( " + cNext + " " + cExpr + ")"
         ENDIF

      CASE "eq"
      CASE "equal"
         IF Left( cNext,1 ) $ "('"
            cNext := lisp_EvalExpr( cNext, @nGetType )
         ENDIF
         cExpr := lisp_GetNextExpr( s, @nPos )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         IF Left( cExpr,1 ) $ "('"
            cExpr := lisp_EvalExpr( cExpr, @nGetType2 )
         ENDIF
         nType := TYPE_ATOM
         IF nGetType != nGetType2 .OR. Valtype( cNext ) != ValType( cExpr )
            RETURN .F.
         ELSE
            RETURN Iif( nGetType == TYPE_LIST, StrTran(cNext," ","") == StrTran(cExpr," ",""), ;
               ( cNext == cExpr ) )
         ENDIF

      CASE "label"
         EXIT

      CASE "lambda"
         IF Left( cNext,1 ) == "("
            aLambda := { Nil }
            nPos2 := 2
            DO WHILE !Empty( c := lisp_GetNextExpr( cNext, @nPos2 ) )
               Aadd( aLambda, c )
            ENDDO
         ELSE
            nLispErr := ERR_BRA_EXPECTED; nType := TYPE_ERR; RETURN Nil
         ENDIF
         nType := TYPE_LAMBDA
         aLambda[1] := lisp_GetNextExpr( s, @nPos )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         RETURN aLambda

      CASE "defun"
         EXIT

      OTHERWISE
      ENDSWITCH
   ENDIF

   RETURN Nil

STATIC FUNCTION lisp_GetNextExpr( s, nPos )

   LOCAL c, nPos2

   nPos := cedi_strSkipChars( s, nPos )
   IF ( c := Substr( s, nPos, 1 ) ) == '('
      IF ( nPos2 := lisp_GetPairBracket( s, nPos, 0 ) ) < 0
         nLispErr := ERR_PAIRBRACKET
         RETURN ""
      ENDIF
      nPos2 ++
      s := Substr( s, nPos, nPos2-nPos )
      nPos := nPos2
      RETURN s

   ELSEIF c == '"'
      IF ( nPos2 := hb_At( '"', s, nPos+1 ) ) == 0
         nLispErr := ERR_PAIRQUOTE
         RETURN ""
      ELSE
         nPos2 ++
         s := Substr( s, nPos, nPos2-nPos )
         nPos := nPos2
         RETURN s
      ENDIF

   ELSEIF c == "'"
      nPos ++
      s := lisp_GetNextExpr( s, @nPos )
      RETURN "'" + s

   ELSEIF c == ')'
      RETURN ""

   ELSE
      nPos2 := cedi_strPBrk( " )", s, nPos+1 )
      IF nPos2 < 0
         s := Substr( s, nPos )
      ELSE
         s := Substr( s, nPos, nPos2-nPos )
      ENDIF
      nPos := nPos2
      RETURN s
   ENDIF

   RETURN ""

