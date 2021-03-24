#define K_ESC       27
#define K_F5        -4
#define K_F10       -9

FUNCTION plug_Calculator( oEdit )

   LOCAL oCalc := edi_AddWindow( oEdit, "", "$Calculator", 2, 5 )
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Calculator    F5 - Calculate" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }

   oCalc:nMode := 0
   oCalc:bWriteTopPane := bWPane
   oCalc:bOnKey := {|o,n| _Calcul_OnKey(o,n) }

   RETURN Nil

FUNCTION _Calcul_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)
   LOCAL s, nPos, xRes, bOldError, cType

   IF nKey == K_F5

      s := oEdit:aText[oEdit:nLine]
      IF ( nPos := At( " = ", s ) ) > 0
         s := AllTrim( Left( s, nPos-1 ) )
      ENDIF
      SET DECIMALS TO 8
      bOldError := ErrorBlock( { |e|MacroError( e ) } )
      BEGIN SEQUENCE
         xRes := &s
      RECOVER
         xRes := Nil
      END SEQUENCE
      ErrorBlock( bOldError )

      IF ( cType := Valtype( xRes ) ) == "N"
         oEdit:aText[oEdit:nLine] := s + " = " + Ltrim(Str( xRes )) + " (0x" + hb_NumToHex(xRes) + ")"
      ELSEIF cType == "C"
         oEdit:aText[oEdit:nLine] := s + " = " + xRes
      ELSEIF cType == "D"
         oEdit:aText[oEdit:nLine] := s + " = " + Dtoc(xRes)
      ENDIF
      oEdit:TextOut()

      RETURN -1

   ELSEIF nKey == K_ESC .OR. nKey == K_F10
      oEdit:lUpdated := .F.
      mnu_Exit( oEdit )
      RETURN -1

   ENDIF

   RETURN 0
