
#define K_ESC    27
#define K_DEL     7

STATIC cPath_mpv := "", cProxy := "", aHis := {}, lHisUpd := .F.

FUNCTION plug_mpv_ytdlp( oEdit, cPath )

   LOCAL cIniName := "mpv_ytdlp.ini", aMenu := { "New address" }, i, cUrl, lRes := .T.
   LOCAL bKeys := {|nKeyExt,nLine|
      LOCAL nKey := hb_keyStd( nKeyExt )
      IF nKey == K_DEL .AND. nLine > 1 .AND. ;
         edi_Alert( aMenu[nLine] + ';' + "really delete?", "Yes", "No" ) == 1
         aMenu[nLine] := "---"
         aHis[nLine-1,1] := "---"
         lHisUpd := .T.
         RETURN .T.
      ENDIF
      RETURN Nil
   }

   IF Empty( cPath_mpv )
      _mpv_ytdlp_rdini( cPath + cIniName )
   ENDIF

   IF Empty( cPath_mpv )
      cPath_mpv := Iif( hb_Version( 20 ), "/usr/bin/mpv", "mpv" )
      edi_Alert( "Mpv path isn't set,;will use default value." )
   ENDIF

   FOR i := 1 TO Len( aHis )
      Aadd( aMenu, aHis[i,1] )
   NEXT

   IF ( i := FMenu( oEdit, aMenu,,,,,,,, .T.,,, bKeys ) ) == 1
      IF !Empty( cUrl := _mpv_ytdlp_GetAddr() )
         IF !( lRes := cedi_RunApp( cPath_mpv + ;
            Iif( Empty(cProxy), "", " --http-proxy " + cProxy ) + " " + cUrl ) )
            lHisUpd := .F.
         ENDIF
      ENDIF
   ELSEIF i > 1
      lRes := cedi_RunApp( cPath_mpv + ;
         Iif( Empty(cProxy), "", " --http-proxy " + cProxy ) + " " + aHis[i-1,2] )
   ENDIF

   IF !lRes
      edi_Alert( "Can't execute mpv" )
   ENDIF

   IF lHisUpd
      _mpv_ytdlp_wrini( cPath + cIniName )
   ENDIF

   RETURN Nil

STATIC FUNCTION _mpv_ytdlp_GetAddr()

   LOCAL y1 := 5, x1 := Int(MaxCol()/2)-30, x2 := x1+60
   LOCAL cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets := { ;
      {y1+1,x1+2, 11, "Title:"}, ;
      { y1+1,x1+10, 0, "", x2-x1-12 }, ;
      {y1+2,x1+2, 11, "Url:"}, ;
      { y1+2,x1+10, 0, "", x2-x1-12 }, ;
      {y1+4,x1+3, 1, .F., 1 }, {y1+4,x1+2, 11, "[ ] " + "Save"} ;
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
   LOCAL cdpCurr := hb_CdpSelect()

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "path" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cPath_mpv := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "proxy" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cProxy := cTemp
               ENDIF
            ENDIF
         ELSEIF Upper(aIni[nSect]) == "HIS"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := ASort( hb_hKeys( aSect ) )
               FOR i := 1 TO Len( arr )
                  Aadd( aHis,  hb_ATokens( aSect[ arr[i] ], ";" ) )
                  IF ! (cdpCurr == "UTF8" )
                     aHis[i,1] := hb_Utf8ToStr( aHis[i,1], cdpCurr )
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

STATIC FUNCTION _mpv_ytdlp_wrini( cIni )

   LOCAL s := "[MAIN]" + Chr(10) + "path=" + cPath_mpv + ;
      Iif( Empty(cProxy), "", Chr(10) + "proxy=" + cProxy ) + ;
      Chr(10) + Chr(10) + "[HIS]" + Chr(10)
   LOCAL i, cdpCurr := hb_CdpSelect()

   FOR i := Len( aHis ) TO 1 STEP -1
      IF aHis[i,1] == "---"
         hb_ADel( aHis, i, .T. )
      ENDIF
   NEXT

   FOR i := 1 TO Len( aHis )
      s += PAdl( Ltrim(Str(i,4)), 4, '0' ) + "=" + ;
         Iif( cdpCurr == "UTF8", aHis[i,1], hb_strToUtf8( aHis[i,1], cdpCurr ) ) + ;
         ";" + aHis[i,2] + Chr(10)
   NEXT

   hb_MemoWrit( cIni, s )

   RETURN Nil