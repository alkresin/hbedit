/*
 * HbEdit plugin - list of games
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

STATIC cIniPath
STATIC oEd
STATIC aGames := {}

FUNCTION plug_games( oEdit, cPath )

   LOCAL aMenu := {}, i

   oEd := oEdit
   IF Empty( cIniPath )
      _gm_ReadIni( (cIniPath := cPath) + "games.ini" )
   ENDIF

   IF Empty( aGames )
      edi_Alert( "games.ini is absent or empty"  )
      RETURN Nil
   ENDIF

   FOR i := 1 TO Len( aGames )
      AAdd( aMenu, aGames[i,2] )
   NEXT

   IF ( i := FMenu( oEd, aMenu, oEd:y1+2, oEd:x1+4 ) ) == 0
      RETURN Nil
   ENDIF

   edi_RunPlugin( oEdit, aGames, i )

   RETURN Nil

STATIC FUNCTION _gm_ReadIni( cIni )

   LOCAL hIni, aIni, nSect, aSect, i, arr, aTemp
   LOCAL cdpCurr := hb_CdpSelect()

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := ASort( hb_hKeys( aSect ) )
               FOR i := 1 TO Len( arr )
                  aTemp := hb_ATokens( aSect[ arr[i] ], ";" )
                  IF !( cdpCurr == "UTF8" )
                     aTemp[2] := hb_Utf8ToStr( aTemp[2], cdpCurr )
                  ENDIF
                  IF File( cIniPath + aTemp[1] )
                     AAdd( aTemp, "" )
                     AAdd( aTemp, Nil )
                     AAdd( aTemp, Nil )
                     Aadd( aGames, aTemp )
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil