
#define K_ESC    27

STATIC cPath_mpv, aHis := {}, lHisUpd := .F.

FUNCTION plug_mpv_ytdlp( oEdit, cPath )

   LOCAL cIniName := "mpv_ytdlp.ini", aMenu := { "New address" }, i, cUrl

   IF Empty( cPath_mpv )
      _mpv_ytdlp_rdini( cPath + cIniName )
   ENDIF

   FOR i := 1 TO Len( aHis )
      Aadd( aMenu, aHis[i,1] )
   NEXT

   IF ( i := FMenu( oEdit, aMenu ) ) == 1
      IF !Empty( cUrl := _mpv_ytdlp_GetAddr() )
         cedi_RunApp( cPath_mpv + " " + cUrl )
      ENDIF
   ELSEIF i > 1
      cedi_RunApp( cPath_mpv + " " + aHis[i-1,2] )
   ENDIF

   IF lHisUpd
      _mpv_ytdlp_wrini( cPath + cIniName )
   ENDIF

   RETURN Nil

STATIC FUNCTION _mpv_ytdlp_GetAddr()

   LOCAL y1 := 5, x1 := Int(MaxCol()/2)-30, x2 := x1+60
   LOCAL cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets := { ;
      {y1+1,x1+2, 11, _I("Title:")}, ;
      { y1+1,x1+10, 0, "", x2-x1-12 }, ;
      {y1+2,x1+2, 11, _I("Url:")}, ;
      { y1+2,x1+10, 0, "", x2-x1-12 }, ;
      {y1+4,x1+3, 1, .F., 1 }, {y1+4,x1+2, 11, "[ ] " + _I("Save")} ;
      }

   cBuf := Savescreen( y1, x1, y1 + 5, x2 )
   @ y1, x1, y1 + 5, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+3, x1 SAY "Ã"
   @ y1+3, x2 SAY "´"
   @ y1+3, x1+1 TO y1+3, x2-1

   edi_READ( aGets )
   Restscreen( y1, x1, y1 + 5, x2, cBuf )
   IF LastKey() == K_ESC
      RETURN Nil
   ELSEIF aGets[5,4]
      Aadd( aHis, {aGets[2,4], aGets[4,4]} )
      lHisUpd := .T.
   ENDIF

   RETURN aGets[4,4]

STATIC FUNCTION _mpv_ytdlp_rdini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect, arr, i

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "path" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cPath_mpv := cTemp
               ENDIF
            ENDIF
         ELSEIF Upper(aIni[nSect]) == "HIS"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := ASort( hb_hKeys( aSect ) )
               FOR i := 1 TO Len( arr )
                  Aadd( aHis,  hb_ATokens( aSect[ arr[i] ], "," ) )
               NEXT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

STATIC FUNCTION _mpv_ytdlp_wrini( cIni )

   LOCAL s := "[MAIN]" + Chr(10) + "path=" + cPath_mpv + Chr(10) + Chr(10) + "[HIS]" + Chr(10)
   LOCAL i

   FOR i := 1 TO Len( aHis )
      s += ""
   NEXT

   hb_MemoWrit( cIni, s )

   RETURN Nil
