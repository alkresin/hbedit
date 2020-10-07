/*
 * Lisp interpreter
 */

#define TYPE_ERR   -1
#define TYPE_ATOM   1
#define TYPE_LIST   2
#define TYPE_EXPR   3
#define TYPE_LAMBDA 4
#define TYPE_LABEL  5

#define ERR_PAIRBRACKET    1
#define ERR_WRONGSTARTCHAR 2
#define ERR_PAIRQUOTE      3
#define ERR_BRA_EXPECTED   4
#define ERR_LIST_EXPECTED  5
#define ERR_ATOM_EXPECTED  6
#define ERR_LOGIC_EXPECTED 7
#define ERR_LAMBDA_EXPECTED 8
#define ERR_WRONG_PARAM_NUMBER 9
#define ERR_UNKNOWN_FUNCTION 10

STATIC cFalse := "()", cTrue := "t"
STATIC nLispErr := 0
STATIC aErrtxt := { "Pair bracket not found", "Wrong char in a line start", "Pair quote not found", ;
   "Left bracket expected", "List expected", "Atom expected", "Logical value expected", ;
   "Lanbda expected", "Wrong number of parameters", "Unknown function" }
STATIC aLabels := {}

Memvar aDefuns

FUNCTION lisp_Run()
   RETURN Nil

FUNCTION lisp_Eval( xText )

   LOCAL s, cEol := Chr(10), i, nPos, c, nLevel, cBuff
   LOCAL lIn := .F., lNeedNext

   PUBLIC aDefuns := {}

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

   LOCAL nPos, nPos2, cmd, nGetType, nGetType2, cNext, cExpr, cRes
   LOCAL aLambda, cName

   //edi_Writelog( "E>" + s )
   nLispErr := 0
   nPos := cedi_strSkipChars( s, 2 )
   //edi_Alert( "0> " + s )
   IF Left( s, 1 ) == "'"
      cNext := lisp_GetNextExpr( s, nPos )
      IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
      nType := Iif( Left( cNext,1 ) == '(' .AND. !(cNext == cFalse), TYPE_LIST, TYPE_ATOM )
      //edi_Alert( "0a> " + ltrim(str(ntype))+"/"+ ltrim(str(npos))+"/"+cNext )
      RETURN lisp_EvalRet( cNext )

   ELSEIF Substr( s, nPos, 1 ) == '('
      cNext := lisp_GetNextExpr( s, @nPos )
      IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
      cNext := lisp_EvalExpr( cNext, @nGetType )
      IF nGetType == TYPE_LAMBDA .OR. nGetType == TYPE_LABEL
         cName := cNext[2]
         cExpr := lisp_EvalLambda( cNext, s, nPos, @nGetType )
         nType := nGetType
         IF ( nPos := Ascan( aLabels, {|a|a[1]==cName} ) ) > 0
            hb_ADel( aLabels, nPos, .T. )
         ENDIF
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         RETURN lisp_EvalRet( cExpr )
      ELSE
      ENDIF

   ELSEIF Substr( s, nPos, 1 ) == ')'
      nType := TYPE_ATOM
      RETURN lisp_EvalRet( cFalse )
   ELSE
      nPos2 := cedi_strPBrk( " )", s, nPos+1 )
      cmd := Lower( Substr( s, nPos, nPos2-nPos ) )
      nPos := nPos2
      cNext := lisp_GetNextExpr( s, @nPos )
      IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF

      SWITCH cmd
      CASE "quote"
         nType := Iif( Left( cNext,1 ) == '(' .AND. !(cNext == cFalse), TYPE_LIST, TYPE_ATOM )
         RETURN lisp_EvalRet( cNext )

      CASE "atom"
         nType := TYPE_ATOM
         IF Left( cNext,1 ) $ "('"
            lisp_EvalExpr( cNext, @nGetType )
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
            RETURN lisp_EvalRet( Iif( nGetType == TYPE_ATOM, cTrue, cFalse ) )
         ELSE
            RETURN lisp_EvalRet( cTrue )
         ENDIF

      CASE "car"
         IF Left( cNext,1 ) $ "('"
            cNext := lisp_EvalExpr( cNext, @nGetType )
            IF nGetType == TYPE_LIST
               cNext := lisp_GetNextExpr( cNext, 2 )
               IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
               nType := Iif( Left( cNext,1 ) == '(', TYPE_LIST, TYPE_ATOM )
               RETURN lisp_EvalRet( cNext )
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
                  RETURN lisp_EvalRet( cFalse )
               ELSE
                  nType := TYPE_LIST
                  RETURN lisp_EvalRet( cNext )
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
            cRes := lisp_EvalExpr( cExpr, @nGetType )
            IF cRes == cTrue
               cExpr := lisp_GetNextExpr( cNext, @nPos2 )
               IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
               cRes := lisp_EvalRet( lisp_EvalExpr( cExpr, @nGetType ) )
               nType := nGetType
               RETURN cRes
            ELSEIF cRes != cFalse
               nLispErr := ERR_LOGIC_EXPECTED; nType := TYPE_ERR; RETURN Nil
            ENDIF
            IF Empty( cNext := lisp_GetNextExpr( s, @nPos ) )
               EXIT
            ENDIF
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         ENDDO
         nType := TYPE_ATOM
         RETURN lisp_EvalRet( cFalse )

      CASE "cons"
         nType := TYPE_LIST
         IF Left( cNext,1 ) $ "('"
            cNext := lisp_EvalExpr( cNext, @nGetType )
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         ENDIF

         cExpr := lisp_GetNextExpr( s, @nPos )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         IF Left( cExpr,1 ) $ "('"
            cExpr := lisp_EvalExpr( cExpr, @nGetType2 )
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         ENDIF

         IF nGetType2 == TYPE_LIST
            RETURN lisp_EvalRet( "( " + cNext + " " + Ltrim( Substr( cExpr, 2 ) ) )
         ELSE
            RETURN lisp_EvalRet( "( " + Iif( cNext == cFalse, "", cNext ) + " " + Iif( cExpr==cFalse, "", cExpr ) + ")" )
         ENDIF

      CASE "eq"
      CASE "equal"
         IF Left( cNext,1 ) $ "('"
            cNext := lisp_EvalExpr( cNext, @nGetType )
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         ENDIF
         cExpr := lisp_GetNextExpr( s, @nPos )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         IF Left( cExpr,1 ) $ "('"
            cExpr := lisp_EvalExpr( cExpr, @nGetType2 )
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         ENDIF
         //edi_Alert( ltrim(str( ngettype )) +"/"+cNext+"/.../"+ltrim(str( ngettype2 )) +"/"+cExpr+"/" )
         nType := TYPE_ATOM
         IF nGetType != nGetType2
            RETURN lisp_EvalRet( cFalse )
         ELSE
            RETURN lisp_EvalRet( Iif( nGetType == TYPE_LIST, ;
               Iif( StrTran(cNext," ","") == StrTran(cExpr," ",""), cTrue, cFalse ), ;
               Iif( cNext == cExpr, cTrue, cFalse ) ) )
         ENDIF

      CASE "lambda"
         aLambda := lisp_Lambda( s, nPos, cNext )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         nType := TYPE_LAMBDA

         RETURN aLambda

      CASE "label"
         IF Left( cNext,1 ) $ "('"
            nLispErr := ERR_ATOM_EXPECTED; nType := TYPE_ERR; RETURN Nil
         ELSE
            cName := cNext
         ENDIF
         cNext := lisp_GetNextExpr( s, @nPos )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         IF Left( cNext,1 ) == "(" .AND. !Empty( aLambda := lisp_EvalExpr( cNext, @nGetType ) ) ;
            .AND. nGetType == TYPE_LAMBDA
            Aadd( aLabels, { cName, aLambda } )
            aLambda[2] := cName
            nType := TYPE_LABEL
            RETURN aLambda
         ELSE
            nLispErr := ERR_LAMBDA_EXPECTED; nType := TYPE_ERR; RETURN Nil
         ENDIF

      CASE "defun"
         IF Left( cNext,1 ) $ "('"
            nLispErr := ERR_ATOM_EXPECTED; nType := TYPE_ERR; RETURN Nil
         ELSE
            cName := cNext
         ENDIF
         cNext := lisp_GetNextExpr( s, @nPos )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF

         aLambda := lisp_Lambda( s, nPos, cNext, cName )
         IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
         Aadd( aDefuns, { aLambda[2], aLambda } )
         nType := TYPE_ATOM
         RETURN cTrue

      OTHERWISE
         nPos := nPos2
         IF ( nPos2 := Ascan( aLabels, {|a|a[1]==cmd} ) ) > 0
            cExpr := lisp_EvalLambda( aLabels[nPos2,2], s, nPos, @nGetType )
            nType := nGetType
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
            RETURN lisp_EvalRet( cExpr )
         ELSEIF ( nPos2 := Ascan( aDefuns, {|a|a[1]==cmd} ) ) > 0
            cExpr := lisp_EvalLambda( aDefuns[nPos2,2], s, nPos, @nGetType )
            nType := nGetType
            IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
            RETURN lisp_EvalRet( cExpr )
         ELSE
            //edi_Alert( str(len(adefuns)) + "/" + cmd + "/" + aDefuns[1,1] + "/" )
            nLispErr := ERR_UNKNOWN_FUNCTION; nType := TYPE_ERR; RETURN Nil
         ENDIF
      ENDSWITCH
   ENDIF

   RETURN Nil

STATIC FUNCTION lisp_EvalRet( s )

   //edi_Writelog( "R>" + s )

   RETURN s

STATIC FUNCTION lisp_Lambda( s, nPos, cBody, cName )

   LOCAL aLambda, nPos2 := 2, cNewName

   IF Left( cBody,1 ) == "("
      aLambda := { lisp_GetNextExpr( s, @nPos ), cName }
      IF nLispErr > 0; RETURN Nil; ENDIF

      DO WHILE !Empty( cName := lisp_GetNextExpr( cBody, @nPos2 ) )
         cNewName := '&' + cName
         Aadd( aLambda, cNewName )
         aLambda[1] := hb_strReplace( aLambda[1], { ' '+cName+' ', '('+cName+' ', ;
            ' '+cName+')', '('+cName+')' }, { ' '+cNewName+' ', '('+cNewName+' ', ;
            ' '+cNewName+')', '('+cNewName+')' } )
      ENDDO
   ELSE
      nLispErr := ERR_BRA_EXPECTED; RETURN Nil
   ENDIF

   RETURN aLambda

STATIC FUNCTION lisp_EvalLambda( aLambda, s, nPos, nType )

   LOCAL cExpr := aLambda[1], i, cParam, nGetType

   i := 2
   DO WHILE !Empty( cParam := lisp_GetNextExpr( s, @nPos ) )
      i ++
      IF i > Len( aLambda )
         nLispErr := ERR_WRONG_PARAM_NUMBER; RETURN Nil
      ENDIF
      //cParam := lisp_EvalExpr( cParam, @nGetType )
      //edi_Alert( ltrim(str(i))+": "+valtype(aLambda[i])+valtype(cParam) )
      cExpr := hb_strReplace( cExpr, { ' '+aLambda[i]+' ', '('+aLambda[i]+' ', ;
         ' '+aLambda[i]+')', '('+aLambda[i]+')' }, { ' '+cParam+' ', '('+cParam+' ', ;
         ' '+cParam+')', '('+cParam+')' } )
   ENDDO

   IF i != Len( aLambda )
      nLispErr := ERR_WRONG_PARAM_NUMBER; RETURN Nil
   ENDIF

   //edi_Alert( cExpr )
   //edi_Writelog( "1>" + aLambda[1] )
   //edi_Writelog( "2>" + cExpr )
   cExpr := lisp_EvalExpr( cExpr, @nGetType )
   IF nLispErr > 0; nType := TYPE_ERR; RETURN Nil; ENDIF
   nType := nGetType
   //edi_Writelog( "3>" + cExpr )

   RETURN cExpr

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
