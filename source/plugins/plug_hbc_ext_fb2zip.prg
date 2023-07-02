
#define  K_ENTER   13
#define  K_ESC     27

#xtranslate _I( <x,...> ) => hb_i18n_gettext( <x> )

STATIC cUnzBuff, nPosStart, nPosEnd, nArrLen, nRealLen, arr, lUtf8, nWidth
STATIC nLevel

FUNCTION plug_hbc_ext_fb2zip( oEdit, cPath, aParams )

   LOCAL oPane := aParams[1], cFile := aParams[2], hUnzip, nSize, nPos, nPos1
   LOCAL n, aLevels := Array( 5 ), nStartBak, nEndBak
   LOCAL oNew, lErr, lRes

   IF !Empty( hUnzip := hb_unzipOpen( cFile ) )
      hb_unzipFileFirst( hUnzip )
      hb_unzipFileInfo( hUnzip,,,,,,, @nSize, )
      IF hb_unzipFileOpen( hUnzip, Nil ) == 0
         cUnzBuff := Space( nSize )
         nSize := hb_unzipFileRead( hUnzip, @cUnzBuff )
         hb_unzipFileClose( hUnzip )
      ENDIF
      hb_unzipClose( hUnzip )
   ENDIF

   IF Empty( nSize )
      RETURN Nil
   ENDIF

   IF ( nPosStart := hb_At( "<body", cUnzBuff, 1, nSize ) ) == 0
      RETURN Nil
   ENDIF
   nPosStart += 6
   IF ( nPosEnd := hb_At( "</body", cUnzBuff, nPosStart, nSize ) ) == 0
      RETURN Nil
   ENDIF

   nLevel := 0
   nWidth := oPane:vx2 - oPane:vx1 - 1
   lUtf8 := ( fb2_enc( cUnzBuff ) == "utf-8" )
   nArrLen := Int( (nPosEnd-nPosStart) / nWidth ) * Iif( lUtf8, 2, 1 )
   arr := Array( nArrLen )
   nRealLen := 0

   fb2_gettitle()
   lErr := .F.
   DO WHILE ( nPosStart := hb_At( "<", cUnzBuff, nPosStart, nPosEnd ) ) > 0
      n := hb_bpeek( cUnzBuff, ++nPosStart )
      SWITCH n
      CASE 115     // s
         IF Substr( cUnzBuff, nPosStart, 7 ) == "section"
            fb2_getsection()
         ENDIF
         EXIT
      CASE 101     // e
         IF Substr( cUnzBuff, nPosStart, 8 ) == "epigraph"
            fb2_getepi( "</epigraph>" )
         ENDIF
         EXIT
      CASE 99      // c
         IF Substr( cUnzBuff, nPosStart, 8 ) == "cite"
            fb2_getepi( "</cite>" )
         ENDIF
         EXIT
      CASE 112     // p
         fb2_getp()
         EXIT
      CASE 47      // /
         IF hb_bpeek( cUnzBuff, ++nPosStart ) == 115 .AND. ;
            Substr( cUnzBuff, nPosStart, 7 ) == "section"
            nLevel --
         ENDIF
      END

   ENDDO

   IF lErr .OR. nRealLen == 0
      edi_Alert( _I("Something goes wrong...") )
      IF nRealLen == 0
         RETURN Nil
      ENDIF
   ENDIF
   arr := ASize( arr, nRealLen )
   oNew := mnu_NewBuf( TEdit():aWindows[TEdit():nCurr] )
   oNew:aText := arr
   oNew:lReadOnly := .T.
   IF lUtf8
      oNew:cp := "UTF8"
      oNew:lUtf8 := .T.
   ENDIF
   hb_cdpSelect( oNew:cp )

   RETURN Nil

STATIC FUNCTION fb2_gettitle()

   LOCAL cTemp, nPos

   nPosStart := hb_At( "<", cUnzBuff, nPosStart, nPosEnd )
   IF Substr( cUnzBuff, nPosStart+1, 5 ) == "title"
      nPosStart ++
      fb2_add( Replicate( '-', nLevel*2 ) )
      nPos := hb_At( "</title", cUnzBuff, nPosStart, nPosEnd )
      cTemp := fb2_strip( Substr( cUnzBuff, nPosStart+6, nPos-nPosStart-6 ) )
      nPosStart := nPos + 1
      fb2_add_stripped( cTemp )
   ENDIF

   Return Nil

STATIC FUNCTION fb2_add_stripped( sBuff, sLeft )

   LOCAL nPos, c10 := Chr(10)

   DO WHILE ( nPos := At( c10, sBuff ) ) > 0
      fb2_add( Left( sBuff, nPos-1 ), sLeft )
      sBuff := Substr( sBuff, nPos + 1 )
   ENDDO
   fb2_add( sBuff, sLeft )

   RETURN Nil

STATIC FUNCTION fb2_getp()

   LOCAL n, nBak := nPosStart

   IF ( n := hb_bpeek( cUnzBuff, ++nPosStart ) ) == 62 .OR. n == 32  // >
      IF ( nPosStart := hb_At( ">", cUnzBuff, nPosStart, nPosEnd ) ) == 0
         nPosStart := nBak
         RETURN .F.
      ENDIF
      nPosStart ++
      IF ( n := hb_At( "</p", cUnzBuff, nPosStart, nPosEnd ) ) == 0
         nPosStart := nBak
         RETURN .F.
      ENDIF
      fb2_add( "" )
      fb2_add( "  " + fb2_strip( Substr( cUnzBuff, nPosStart, n-nPosStart ) ) )
      nPosStart := n + 4

   ELSEIF n == 111 .AND. hb_bpeek( cUnzBuff, ++nPosStart ) == 101
      IF ( nPosStart := hb_At( ">", cUnzBuff, nPosStart, nPosEnd ) ) == 0
         nPosStart := nBak
         RETURN .F.
      ENDIF
      nPosStart ++
      IF ( n := hb_At( "</poem", cUnzBuff, nPosStart, nPosEnd ) ) == 0
         RETURN .F.
      ENDIF
      fb2_add( "" )
      fb2_add_stripped( fb2_strip( Substr( cUnzBuff, nPosStart, n-nPosStart ) ) )
   ENDIF

   Return .T.

STATIC FUNCTION fb2_getepi( cEnd )

   LOCAL nPos, nBak := nPosStart

   IF ( nPosStart := hb_At( ">", cUnzBuff, nPosStart, nPosEnd ) ) == 0
      nPosStart := nBak
      RETURN .F.
   ENDIF
   nPosStart ++
   IF ( nPos := hb_At( cEnd, cUnzBuff, nPosStart, nPosEnd ) ) == 0
      RETURN .F.
   ENDIF

   fb2_add_stripped( fb2_strip( Substr( cUnzBuff, nPosStart, nPos-nPosStart ) ), ;
      Iif( cEnd == "</epigraph>", "~   ","= " ) )
   nPosStart := nPos + 4

   Return .T.

STATIC FUNCTION fb2_getsection()

   LOCAL n, nPos

   fb2_add( "" )
   nLevel ++
   fb2_gettitle()

   Return .T.

STATIC FUNCTION fb2_strip( cBuff )

   LOCAL nPos1, nPos2

   IF Chr(10) $ cBuff .OR. Chr(13) $ cBuff
      cBuff := hb_strReplace( cBuff, {Chr(10),Chr(13)} )
   ENDIF
   edi_writelog( "st-1>" + cBuff )
   IF "</p>" $ cBuff .OR. "</v>" $ cBuff
      cBuff := hb_strReplace( cBuff, {"</p>","</v>","</text-author>","<v>"}, {Chr(10),Chr(10),Chr(10),"    "} )
   ENDIF
   edi_writelog( "st-2>" + cBuff )
   DO WHILE ( nPos1 := At( "<", cBuff ) ) > 0
      IF ( nPos2 := hb_At( ">",cBuff, nPos1 ) ) > 0
         cBuff := Left( cBuff, nPos1-1 ) + Substr( cBuff, nPos2+1 )
      ELSE
         EXIT
      ENDIF
   ENDDO
   edi_writelog( "st-3>" + cBuff )

   RETURN cBuff

STATIC FUNCTION fb2_add( sBuff, sLeft )

   LOCAL i

   IF !Empty( sLeft )
      sBuff := sLeft + sBuff
   ENDIF
   arr[++nRealLen] := sBuff
   IF nRealLen == nArrLen
      nArrLen += 100
      arr := ASize( arr, nArrLen )
   ENDIF

   DO WHILE cp_Len( lUtf8, arr[nRealLen] ) > nWidth
      IF cp_Substr( lUtf8, arr[nRealLen], nWidth+1, 1 ) == ' '
         arr[nRealLen+1] := cp_Substr( lUtf8, arr[nRealLen], nWidth+1 )
         IF !Empty( sLeft )
            arr[nRealLen+1] := sLeft + arr[nRealLen+1]
         ENDIF
         arr[nRealLen] := cp_Left( lUtf8, arr[nRealLen], nWidth )
         nRealLen ++
      ELSE
         i := nWidth + 1
         DO WHILE --i > 1 .AND. !( cp_Substr( lUtf8, arr[nRealLen],i,1 ) ) $ " .,!?-"; ENDDO
         IF i == 1
            i := nWidth
         ENDIF
         arr[nRealLen+1] := cp_Substr( lUtf8, arr[nRealLen], i+1 )
         IF !Empty( sLeft )
            arr[nRealLen+1] := sLeft + arr[nRealLen+1]
         ENDIF
         arr[nRealLen] := cp_Left( lUtf8, arr[nRealLen], i )
         nRealLen ++
      ENDIF
      IF nRealLen == nArrLen
         nArrLen += 100
         arr := ASize( arr, nArrLen )
      ENDIF
   ENDDO

   RETURN Nil

STATIC FUNCTION fb2_enc( sBuff )

   LOCAL nPos1, nPos2, cTemp

   IF ( nPos1 := At( "<?xml", sBuff ) ) != 0 .AND. ( nPos2 := hb_At( "?>", sBuff, nPos1 ) ) != 0
      cTemp := SubStr( sBuff, nPos1, nPos2 - nPos1 )
      IF ( nPos1 := At( "encoding=", cTemp ) ) != 0
         cTemp := SubStr( cTemp, nPos1 + 10 )
         IF ( nPos1 := At( '"', cTemp ) ) != 0
            RETURN Lower( Left( cTemp,nPos1 - 1 ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN ""
