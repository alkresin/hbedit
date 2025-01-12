/*
 * HbEdit plugin - a middleware for python plugins
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

STATIC cIniPath
STATIC oEd
STATIC aPyPlugins := {}
STATIC cCompiler
STATIC nLogLevel := 0
STATIC hExt

FUNCTION plug_toPython( oEdit, cPath, cPyPlugin )

   LOCAL aMenu := {}, i, cExe, xRes

   oEd := oEdit
   cIniPath := cPath

   IF Empty( cCompiler := edi_CheckPython() )
      RETURN Nil
   ENDIF

   cExe := cCompiler + " " + cIniPath + "python" + hb_ps() + cPyPlugin
   IF !Empty( hExt := ecli_Run( cExe, nLogLevel,, "hbedit_py" ) )
      ecli_RunFunc( hExt, "initinfo",{}, .T. )
      IF Empty( xRes :=_topy_CheckAnswer( "Wait for init info" ) )
         edi_Alert( "No initial info" )
         ecli_Close( hExt )
         hExt := Nil
         RETURN Nil
      ENDIF
      DO WHILE !Empty( xRes )
         xRes := _topy_Parse( xRes )
         IF Valtype( xRes ) == "A"
            ecli_RunFunc( hExt, xRes[1], xRes[2], .T. )
            IF ( xRes :=_topy_CheckAnswer() ) == Nil
               EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO

      ecli_Close( hExt )
      hExt := Nil
   ELSE
      edi_Alert( "Failed to start module " + str( nLogLevel ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION _topy_CheckAnswer( cTitle, cText )

   LOCAL sAns, nSec := Seconds(), arr

   IF cTitle == Nil; cTitle := "Wait"; ENDIF
   IF cText == Nil; cText := "Press ESC to exit"; ELSEIF cText == "clock"; cText := ""; ENDIF

   DO WHILE ( sAns := ecli_CheckAnswer( hExt ) ) == Nil
      IF Inkey( 0.02 ) == 27
         EXIT
      ENDIF
      IF Seconds() - nSec > 1
         IF Empty(arr)
            arr := hbc_Wndinit( 8, 28, 10, 52,, cTitle )
            hbc_Wndout( arr, cText )
         ENDIF
         IF Empty( cText )
            hbc_Wndout( arr, Time(), .T. )
         ENDIF
      ENDIF
   ENDDO
   IF !Empty(arr)
      hbc_Wndclose( arr )
   ENDIF

   RETURN sAns

/*
 */
STATIC FUNCTION _topy_Parse( xMsg )

   LOCAL cCmd, nPos, arr, cFunc, cMetka, i, oPane

   IF Valtype( xMsg ) == "C"
      IF Left( xMsg,1 ) == ":"
         IF ( cCmd := Substr( xMsg,2,4 ) ) == "end;"
            IF !Empty( xMsg := Substr( xMsg,6 ) )
               edi_Alert( xMsg )
            ENDIF
            RETURN .F.
         ELSEIF cCmd == "get("
            IF ( nPos := hb_At( ")", xMsg, 5 ) ) > 0
               arr := hb_ATokens( Substr( xMsg, 6, nPos-6 ) )
               cFunc := arr[1]
               cMetka := Iif( Len(arr)>1, arr[2], Nil )
               oPane := FilePane():PaneCurr()
               arr := hb_ATokens( Substr( xMsg, nPos+1 ) )
               FOR i := 1 TO Len( arr )
                  IF arr[i] == "dir"
                     arr[i] := oPane:cCurrPath
                  ELSEIF arr[i] == "file"
                     arr[i] := oPane:aDir[oPane:nCurrent+oPane:nShift,1]
                  ENDIF
               NEXT
               IF cMetka != Nil
                  hb_AIns( arr, 1, cMetka, .T. )
               ENDIF
               RETURN { cFunc, arr }
            ELSE
            ENDIF
         ENDIF
      ELSE
         edi_Alert( xMsg )
         RETURN .F.
      ENDIF
   ELSEIF Valtype( xMsg ) == "A"
      edi_Alert( "Arr: " + Str(Len(arr)) )
   ENDIF

   RETURN .T.