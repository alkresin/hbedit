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
STATIC lEscPressed := .F.

FUNCTION plug_toPython( oEdit, cPath, cPyPlugin )

   LOCAL aMenu := {}, i, cExe, xRes

   oEd := oEdit
   cIniPath := cPath

   IF !_topy_CheckPython()
      RETURN Nil
   ENDIF

   cExe := cCompiler + " " + cIniPath + "python" + hb_ps() + cPyPlugin
   IF !Empty( hExt := ecli_Run( cExe, nLogLevel,, "hbedit_py" ) )
      ecli_RunFunc( hExt, "initinfo",{}, .T. )
      IF Empty( xRes :=_topy_CheckAnswer() )
         edi_Alert( "No initial info" )
         ecli_Close( hExt )
         hExt := Nil
         RETURN Nil
      ENDIF
      DO WHILE !Empty( xRes )
         xRes := _topy_Parse( xRes )
         IF Valtype( xRes ) == "A"
            ecli_RunFunc( hExt, xRes[1], xRes[2], .T. )
            IF Empty( xRes :=_topy_CheckAnswer() )
               IF lEscPressed
                  EXIT
               ELSE
                  edi_Alert( "Bad answer..." )
               ENDIF
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

STATIC FUNCTION _topy_CheckAnswer()

   LOCAL sAns, nSec := Seconds(), arr

   lEscPressed := .F.
   DO WHILE ( sAns := ecli_CheckAnswer( hExt ) ) == Nil
      IF Inkey( 0.02 ) == 27
         lEscPressed := .T.
         EXIT
      ENDIF
      IF Empty(arr) .AND. Seconds() - nSec > 1
         arr := hbc_Wndinit( 8, 30, 10, 50,, "Wait" )
         hbc_Wndout( arr, "Press ESC to exit" )
      ENDIF
   ENDDO
   IF !Empty(arr)
      hbc_Wndclose( arr )
   ENDIF

   RETURN sAns

STATIC FUNCTION _topy_CheckPython()

   LOCAL cRes

   cedi_RunConsoleApp( 'python --version',, @cRes )
   IF !Empty( cRes )
      cCompiler := "python"
   ELSE
      cedi_RunConsoleApp( 'python3 --version',, @cRes )
      IF !Empty( cRes )
         cCompiler := "python3"
      ELSE
         edi_Alert( "You need to install Python to use this plugun" )
         RETURN .F.
      ENDIF
   ENDIF
   RETURN .T.

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