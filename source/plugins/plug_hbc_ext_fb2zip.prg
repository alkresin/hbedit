
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
   /*
   DO WHILE ( nPosStart := hb_At( "section", cUnzBuff, nPosStart, nPosEnd ) ) > 0
      IF ( n := hb_bpeek( cUnzBuff, nPosStart-1 ) ) == 60  // <
         IF ++nLevel > Len( aLevels ) .OR. ;
            ( nPosStart := hb_At( ">", cUnzBuff, nPosStart, nPosEnd ) ) == 0
            lErr := .T.
            EXIT
         ENDIF
         //nPosStart ++
         aLevels[nLevel] := nPosStart
      ELSEIF n == 47 .AND. hb_bpeek( cUnzBuff, nPosStart-2 ) == 60   // </
         nStartBak := nPosStart; nEndBak := nPosEnd
         nPosEnd := nPosStart-3; nPosStart := aLevels[nLevel]
         lRes := fb2_getsection()
         nPosStart := nStartBak + 7; nPosEnd := nEndBak
         IF --nLevel < 0 .OR. !lRes
            lErr := .T.
            EXIT
         ENDIF
         //nPosStart ++
      ENDIF
   ENDDO
   */

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

   LOCAL cTemp, nPos, c10 := Chr(10)

   nPosStart := hb_At( "<", cUnzBuff, nPosStart, nPosEnd )
   nPosStart ++
   IF Substr( cUnzBuff, nPosStart, 5 ) == "title"
      fb2_add( Replicate( '-', nLevel*2 ) )
      nPos := hb_At( "</title", cUnzBuff, nPosStart, nPosEnd )
      cTemp := fb2_strip( Substr( cUnzBuff, nPosStart+6, nPos-nPosStart-6 ) )
      nPosStart := nPos + 1
      DO WHILE ( nPos := At( c10, cTemp ) ) > 0
         fb2_add( Left( cTemp, nPos-1 ) )
         cTemp := Substr( cTemp, nPos + 1 )
      ENDDO
      fb2_add( cTemp )
   ENDIF

   Return Nil

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
      fb2_add( fb2_strip( Substr( cUnzBuff, nPosStart, n-nPosStart ) ) )
      nPosStart := n + 4

   ELSEIF n == 111 .AND. hb_bpeek( cUnzBuff, ++nPosStart ) == 101
      //fb2_getpoem()
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

   fb2_add( fb2_strip( Substr( cUnzBuff, nPosStart, nPos-nPosStart ) ) )
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
   IF "</p>" $ cBuff
      cBuff := hb_strReplace( cBuff, {"</p>","</text-author>"}, Chr(10) )
   ENDIF
   //cBuff := StrTran( cBuff, "</p>", Chr(10) )
   DO WHILE ( nPos1 := At( "<", cBuff ) ) > 0
      IF ( nPos2 := hb_At( ">",cBuff, nPos1 ) ) > 0
         cBuff := Left( cBuff, nPos1-1 ) + Substr( cBuff, nPos2+1 )
      ELSE
         EXIT
      ENDIF
   ENDDO

   RETURN cBuff

STATIC FUNCTION fb2_add( sBuff )

   LOCAL i

   arr[++nRealLen] := sBuff
   IF nRealLen == nArrLen
      nArrLen += 100
      arr := ASize( arr, nArrLen )
   ENDIF

   DO WHILE cp_Len( lUtf8, arr[nRealLen] ) > nWidth
      IF cp_Substr( lUtf8, arr[nRealLen], nWidth+1, 1 ) == ' '
         arr[nRealLen+1] := cp_Substr( lUtf8, arr[nRealLen], nWidth+1 )
         arr[nRealLen] := cp_Left( lUtf8, arr[nRealLen], nWidth )
         nRealLen ++
      ELSE
         i := nWidth + 1
         DO WHILE --i > 1 .AND. !( cp_Substr( lUtf8, arr[nRealLen],i,1 ) ) $ " .,!?-"; ENDDO
         IF i == 1
            i := nWidth
         ENDIF
         arr[nRealLen+1] := cp_Substr( lUtf8, arr[nRealLen], i+1 )
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
