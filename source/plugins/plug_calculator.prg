#define K_ESC       27
#define K_F9        -8
#define K_F5        -4

FUNCTION plug_Calculator( oEdit )

   LOCAL oCalc := edi_AddWindow( oEdit, "", "$Calculator", 2, 5 )
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      SetColor( o:cColorPane )
      Scroll( y, o:x1, y, o:x2 )
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Calculator    F9 - Menu  F5 - Calculate" )
      ENDIF
      SetColor( o:cColor )
      DevPos( nRow, nCol )
      RETURN Nil
   }

   oCalc:nMode := 0
   oCalc:bWriteTopPane := bWPane
   oCalc:bOnKey := {|o,n| _Calcul_OnKey(o,n) }

   RETURN Nil

FUNCTION _Calcul_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)
   LOCAL s, nPos, xRes, bOldError

   IF nKey == K_F9

      RETURN -1

   ELSEIF nKey == K_F5

      s := oEdit:aText[oEdit:nLine]
      IF ( nPos := At( "->", s ) ) > 0
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

      IF Valtype( xRes ) == "N"
         oEdit:aText[oEdit:nLine] := s + " -> " + Ltrim(Str( xRes ))
         oEdit:TextOut()
      ENDIF

      RETURN -1

   ELSEIF nKey == K_ESC
      oEdit:lUpdated := .F.
      mnu_Exit( oEdit )
      RETURN -1

   ENDIF

   RETURN 0
