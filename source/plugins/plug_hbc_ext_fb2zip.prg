
#define ALT_PRESSED   0x040000
#define CTRL_PRESSED  0x020000
#define K_ALT_D    288
#define K_ALT_I    279
#define K_ALT_L    294

#define  K_ENTER   13
#define  K_ESC     27

#xtranslate _I( <x,...> ) => hb_i18n_gettext( <x> )

STATIC cUnzBuff, nPosStart, nPosEnd, nArrLen, nRealLen, arr, lUtf8, nWidth
STATIC nLevel
STATIC aContent

FUNCTION plug_hbc_ext_fb2zip( oEdit, cPath, aParams )

   LOCAL oPane := aParams[1], cFile := aParams[2], hUnzip, nSize, nPos, nPos1
   LOCAL n, aLevels := Array( 5 ), nStartBak, nEndBak
   LOCAL oNew, lErr, lRes, nSec
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col(), h
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "Fb2 plugin:  Alt-L Table of contents  Alt-I Info" )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         IF o:hCargo == Nil
            o:hCargo := hb_hash()
         ENDIF
         o:hCargo["help"] := "Fb2 plugin hotkeys:" + Chr(10) + ;
            "  Alt-L  - Functions list" + Chr(10)
      ENDIF
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _fb2zip_OnKey(o,n)
      RETURN nRes
   }

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
   nArrLen := Int( (nPosEnd-nPosStart) / nWidth ) * Iif( lUtf8, 3, 2 )
   arr := Array( nArrLen )
   nRealLen := 0
   aContent := {}

   nSec := Seconds()
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
   //edi_Alert( Str(Seconds()-nSec) )

   IF lErr .OR. nRealLen == 0
      edi_Alert( _I("Something goes wrong...") )
      IF nRealLen == 0
         RETURN Nil
      ENDIF
   ENDIF
   arr := ASize( arr, nRealLen )
   oNew := mnu_NewBuf( TEdit():aWindows[TEdit():nCurr] )
   oNew:cFileName := hb_fnameName( hb_fnameName( cFile ) )
   oNew:aText := arr
   oNew:lReadOnly := .T.
   IF lUtf8
      oNew:cp := "UTF8"
      oNew:lUtf8 := .T.
   ENDIF
   hb_cdpSelect( oNew:cp )
   oNew:bStartEdit := bStartEdit
   oNew:bOnKey := bOnKey
   oNew:cargo := { aContent }

   RETURN Nil

STATIC FUNCTION fb2_gettitle()

   LOCAL cTemp, nPos, nLine

   nPosStart := hb_At( "<", cUnzBuff, nPosStart, nPosEnd )
   IF Substr( cUnzBuff, nPosStart+1, 5 ) == "title"
      nPosStart ++
      nLine := nRealLen + 1
      nPos := hb_At( "</title", cUnzBuff, nPosStart, nPosEnd )
      cTemp := fb2_strip( Substr( cUnzBuff, nPosStart+6, nPos-nPosStart-6 ) )
      nPosStart := nPos + 1
      fb2_add_stripped( cTemp )
      Aadd( aContent, { Space(nLevel*2)+(arr[nLine] :=LTrim(arr[nLine])), Nil, nLine } )
      arr[nLine] := Replicate( ':', nLevel*2 ) + " " + arr[nLine]
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
   STATIC lPoem := .F.

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
      IF lPoem
         lPoem := .F.
      ELSE
         fb2_add( "" )
      ENDIF
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
      lPoem := .T.
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
   IF "&#160;" $ cBuff
      cBuff := strTran( cBuff, "&#160;", Chr(160) )
   ENDIF
   IF "</p>" $ cBuff .OR. "</v>" $ cBuff
      cBuff := hb_strReplace( cBuff, {"</p>","</v>","</text-author>","<v>"}, {Chr(10),Chr(10),Chr(10),"    "} )
   ENDIF
   DO WHILE ( nPos1 := At( "<", cBuff ) ) > 0
      IF ( nPos2 := hb_At( ">",cBuff, nPos1 ) ) > 0
         cBuff := Left( cBuff, nPos1-1 ) + Substr( cBuff, nPos2+1 )
      ELSE
         EXIT
      ENDIF
   ENDDO

   RETURN cBuff
/*
STATIC FUNCTION fb2_add( sBuff, sLeft )

   LOCAL i, nLen := cp_Len( lUtf8, sBuff ), nCurrPos := 0

   DO WHILE nLen - nCurrPos > nWidth
      IF ++nRealLen == nArrLen
         nArrLen += 100
         arr := ASize( arr, nArrLen )
      ENDIF
      IF cp_Peek( lUtf8, sBuff, nCurrPos+nWidth+1 ) == 32
         arr[nRealLen] := cp_Substr( lUtf8, sBuff, nCurrPos+1, nWidth )
         IF !Empty( sLeft )
            arr[nRealLen] := sLeft + arr[nRealLen]
         ENDIF
         nCurrPos += nWidth
      ELSE
         i := nCurrPos + nWidth + 1
         DO WHILE --i > nCurrPos+1 .AND. !( cp_Substr( lUtf8, sBuff,i,1 ) ) $ " .,!?-"; ENDDO
         IF i == nCurrPos+1
            i := nCurrPos+nWidth
         ENDIF
         arr[nRealLen] := cp_Substr( lUtf8, sBuff, nCurrPos+1, i-nCurrPos )
         nCurrPos := i
         IF !Empty( sLeft )
            arr[nRealLen] := sLeft + arr[nRealLen]
         ENDIF
      ENDIF
   ENDDO
   IF nCurrPos < nLen .OR. nCurrPos == 0
      IF ++nRealLen == nArrLen
         nArrLen += 100
         arr := ASize( arr, nArrLen )
      ENDIF
      arr[nRealLen] := cp_Substr( lUtf8, sBuff, nCurrPos+1 )
      IF !Empty( sLeft )
         arr[nRealLen] := sLeft + arr[nRealLen]
      ENDIF
   ENDIF

   RETURN Nil
*/

STATIC FUNCTION fb2_add( sBuff, sLeft )

   LOCAL i, nFirst := nReallen + 1
   STATIC cChars := " .,!?-"

   arr[++nRealLen] := sBuff
   IF nRealLen == nArrLen
      nArrLen += 100
      arr := ASize( arr, nArrLen )
   ENDIF

   DO WHILE cp_Len( lUtf8, arr[nRealLen] ) > nWidth
      //IF cp_Substr( lUtf8, arr[nRealLen], nWidth+1, 1 ) == ' '
      IF cp_Peek( lUtf8, arr[nRealLen], nWidth+1 ) == 32
         arr[nRealLen+1] := cp_Substr( lUtf8, arr[nRealLen], nWidth+1 )
         IF !Empty( sLeft )
            arr[nRealLen+1] := sLeft + arr[nRealLen+1]
         ENDIF
         arr[nRealLen] := cp_Left( lUtf8, arr[nRealLen], nWidth )
         nRealLen ++
      ELSE
         i := nWidth + 1
         DO WHILE --i > 1 .AND. !( cp_Substr( lUtf8, arr[nRealLen],i,1 ) ) $ cChars; ENDDO
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
   IF !Empty( sLeft )
      arr[nFirst] := sLeft + arr[nFirst]
   ENDIF

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

STATIC FUNCTION _fb2zip_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), n, i, arr

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_L
         n := oEdit:nLine
         arr := oEdit:cargo[1]
         FOR i := 1 TO Len( arr )
            IF arr[i,3] > n
               n := i - 1
               EXIT
            ENDIF
         NEXT
         n := Iif( n > Len(arr), Len(arr), Iif( n == 0, 1, n ) )
         IF ( i := FMenu( oEdit, arr, 2, 6,,,,, n, (Len(arr)>3) ) ) > 0
            oEdit:Goto( arr[i,3] )
         ENDIF
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0
