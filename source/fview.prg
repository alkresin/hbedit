#include "inkey.ch"
#include "fileio.ch"
#include "hbc.ch"
#ifdef __PSEUDOGT
   #include "hwpgt.ch"
#endif

#define RDBUFFERSIZE  16384
#define FILEINFO_LEN     16
#define HEXBYTES         16

#xtranslate _I( <x,...> ) => hb_i18n_gettext( <x> )

REQUEST HB_CODEPAGE_RU866
REQUEST HB_CODEPAGE_RU1251
REQUEST HB_CODEPAGE_UTF8

MEMVAR lShowCR, nCodePage, lHexMode, cBuffer, nShiftBuffer, lEdited, nBytesLine, nAddrDigits

STATIC aCPages := { "RU866", "RU1251" }

FUNCTION FileView( cFileName, x1, y1, x2, y2, cColor, cColorSel )

   LOCAL handle := hb_vfOpen( cFileName, FO_READWRITE+FO_SHARED )
   LOCAL cScBuf, nFirst := 1, nFileLen, nSize, nShiftBuf := 0
   LOCAL nKey, lDraw := .T.
   LOCAL arr := {}, arrtmp, i, nLenATmp, nSizeATmp, cp
   LOCAL nHexOff := 0, nHexOffMax, nHeight, nCursY := 1, nCursX := 1
   PRIVATE lShowCR := .F., nCodePage := 1, lHexMode := .F., cBuffer, nShiftBuffer := 0, lEdited := .F., ;
           nBytesLine, nAddrDigits

   IF Empty( handle )
      edi_Alert( _I("Can't open") + " " + cFileName )
      RETURN .F.
   ENDIF

   nFileLen := hb_vfSize( cFileName )
   nLenATmp := (y2-y1-1) * 2
   arrtmp := Array( nLenATmp )
   nSize  := ReadBufNext( handle, arr, nShiftBuf, RDBUFFERSIZE, x2-x1-1, (RDBUFFERSIZE>=nFileLen) )

   IF cColor == Nil; cColor := "W/B"; ENDIF
   IF Empty( cColorSel )
      i := At( "/", cColor )
      cColorSel := Substr( cColor, i+1 ) + "/" + Left( cColor, i-1 )
   ENDIF

   cScBuf := Savescreen( y1, x1, y2, x2 )
   SetColor( cColor )
   cp := hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )
   @ y1, x1 + 2 SAY NameShortcut( cFileName, x2-x1-3-FILEINFO_LEN )
   @ y1, x2 - 16 SAY "F1-Help"
   @ y1, x2 - 8 SAY aCPages[nCodePage]

   nHeight     := y2 - y1 - 1
   nAddrDigits := Max( 8, _HexDigits( Max( 0, nFileLen - 1 ) ) )
   nBytesLine  := HexBytesPerLine( x1, x2 )
   nHexOffMax  := Int( Max( 0, (nFileLen - (nFileLen % nBytesLine)) - (nHeight-1) * nBytesLine ) )

   DO WHILE .T.
      IF lDraw
         IF lHexMode
            HexDraw( handle, nHexOff, x1, y1, x2, y2 )
            SetCursClr( nCursY, nCursX, cColorSel, y1+1 )
         ELSE
            Draw( arr, nFirst, x1, y1, x2, y2 )
         ENDIF
         lDraw := .F.
      ENDIF

      IF ( nKey := Inkey( 0 ) ) == K_F10 .OR. nKey == K_ESC
         EXIT
      ELSEIF nKey == K_F4
         lHexMode := !lHexMode
         @ y1+1, x1+1 CLEAR TO  y2-1, x2-1
         IF lHexMode
            nHexOff := nShiftBuf
            FOR i := 2 TO nFirst
               nHexOff += Len( arr[i-1] )
            NEXT
         ENDIF
         lDraw := .T.
      ELSEIF nKey == K_F8
         nCodePage := Iif( nCodePage == 1, 2, 1 )
         @ y1, x2 - 8 SAY PAdr( aCPages[nCodePage], 6 )
         lDraw := .T.
         LOOP
      ELSEIF nKey == K_F1
         edi_Alert( Iif( lHexMode, "Enter - Edit byte", "F2 - Show CR On/Off" ) + ;
         ";F4 - Hex mode On/Off;F8 - Change codepage;Esc - Exit" )
      ENDIF

      IF lHexMode
         IF nKey == K_DOWN
            IF nCursY < nHeight
               SetCursClr( nCursY, nCursX, cColor, y1+1 )
               nCursY ++
               SetCursClr( nCursY, nCursX, cColorSel, y1+1 )
               LOOP
            ELSE
               nHexOff += nBytesLine
            ENDIF
         ELSEIF nKey == K_UP
            IF nCursY > 1
               SetCursClr( nCursY, nCursX, cColor, y1+1 )
               nCursY --
               SetCursClr( nCursY, nCursX, cColorSel, y1+1 )
            ELSE
               nHexOff -= nBytesLine
            ENDIF
         ELSEIF nKey == K_PGDN
            nHexOff += nBytesLine * nHeight
         ELSEIF nKey == K_PGUP
            nHexOff -= nBytesLine * nHeight
         ELSEIF nKey == K_HOME
            nHexOff := 0
         ELSEIF nKey == K_END
            nHexOff := nHexOffMax
         ELSEIF nKey == K_RIGHT
            IF nCursX < nBytesLine
               SetCursClr( nCursY, nCursX, cColor, y1+1 )
               nCursX ++
               SetCursClr( nCursY, nCursX, cColorSel, y1+1 )
            ENDIF
            LOOP
         ELSEIF nKey == K_LEFT
            IF nCursX > 1
               SetCursClr( nCursY, nCursX, cColor, y1+1 )
               nCursX --
               SetCursClr( nCursY, nCursX, cColorSel, y1+1 )
            ENDIF
            LOOP
         ELSEIF nKey == K_ENTER
            IF EditByte( handle, nCursY, nCursX, nHexOff )
               lEdited := .T.
               HexDraw( handle, nHexOff, x1, y1, x2, y2 )
               SetCursClr( nCursY, nCursX, cColorSel, y1+1 )
            ENDIF
         ELSE
            LOOP
         ENDIF
         IF nHexOff > nHexOffMax
            nHexOff := nHexOffMax
         ENDIF
         IF nHexOff < 0
            nHexOff := 0
         ENDIF
         lDraw := .T.
      ELSE
         IF nKey == K_F2
            lShowCR := !lShowCR
            lDraw := .T.
         ELSEIF nKey == K_DOWN .OR. nKey == K_PGDN
            IF nKey == K_DOWN .AND. nFirst + (y2-y1-1) - 1 < Len( arr )
               nFirst ++

            ELSEIF nKey == K_PGDN .AND. nFirst + (y2-y1-2) <= Len( arr ) .AND. ;
                  ( (nFirst + (y2-y1-2) + (y2-y1-2) <= Len( arr )) .OR. (nShiftBuf + nSize >= nFileLen) )
               nFirst += (y2-y1-2)

            ELSEIF nShiftBuf + nSize < nFileLen
               nShiftBuf += nSize
               nSize := RDBUFFERSIZE
               nFirst := nFirst + Iif( nKey == K_DOWN, 1, y2-y1-2 ) - (Len(arr)-nLenATmp)

               ACopy( arr, arrtmp, Len(arr)-nLenATmp+1, nLenATmp )
               arr := AClone( arrtmp )
               nSizeATmp := 0
               FOR i := 1 TO nLenATmp
                  nSizeATmp += Len( arr[i] )
               NEXT

               nSize := ReadBufNext( handle, arr, nShiftBuf, nSize, x2-x1-1, ((nShiftBuf+nSize)>=nFileLen) )
               nShiftBuffer := nShiftBuf
               nShiftBuf -= nSizeATmp
               nSize += nSizeATmp
               //nFirst := Iif( nKey == K_DOWN, (y2-y1-1) + 2, nLenATmp + 1 )
            ENDIF
            lDraw := .T.

         ELSEIF nKey == K_UP .OR. nKey == K_PGUP
            IF nFirst > 1
               nFirst := Iif( nKey == K_UP, nFirst-1, Max( nFirst-(y2-y1-2), 1 ) )

            ELSEIF nShiftBuf > 0
               nSize := Min( RDBUFFERSIZE, nShiftBuf )
               nShiftBuf -= nSize
               arr := ASize( arr, nLenATmp )
               i := ReadBufPrev( handle, arr, nShiftBuf, nSize, x2-x1-1, (nShiftBuf==0) )
               nShiftBuffer := nShiftBuf
               IF i != nSize
                  nShiftBuf -= ( nSize-i )
               ENDIF
               nSize := i
               nFirst := Iif( nKey == K_UP, Len( arr ) - nLenATmp, ;
                     Max( Len( arr ) - nLenATmp - (y2-y1-2), 1 ) )
            ENDIF
            lDraw := .T.

         ELSEIF nKey == K_HOME
            IF nShiftBuf > 0
               arr := {}
               nShiftBuffer := nShiftBuf := 0
               nSize := RDBUFFERSIZE
               nSize := ReadBufNext( handle, arr, nShiftBuf, nSize, x2-x1-1, .F. )
            ENDIF
            nFirst := 1
            lDraw := .T.

         ELSEIF nKey == K_END
            IF nShiftBuf + nSize < nFileLen
               arr := {}
               nSize := RDBUFFERSIZE
               nShiftBuf := nFileLen - nSize + 1
               nSize := ReadBufPrev( handle, arr, nShiftBuf, nSize, x2-x1-1, .F. )
               nShiftBuffer := nShiftBuf
               nShiftBuf := nFileLen - nSize + 1
            ENDIF
            nFirst := Max( Len( arr ) - (y2-y1-2), 1 )
            lDraw := .T.
         ENDIF
      ENDIF
   ENDDO

   Restscreen( y1, x1, y2, x2, cScBuf )

   hb_vfClose( handle )
   RETURN .T.

FUNCTION QFileView( cFileName, cBuff, x1, y1, x2, y2, cColor, cPage, lWords )

   LOCAL handle, nSize := (x2-x1-1) * (y2-y1-1) * 2, s, cp
   LOCAL nRow := y1+1, nCol := x1+1, nPos := 1, c, nPos1, c0 := Chr(0), c10 := Chr(10)
   LOCAL cSeps := " .,;!?-"

   lWords := !Empty( lWords )
   IF cBuff == Nil
      IF hb_vfExists( cFileName )
         handle := hb_vfOpen( cFileName, FO_READ+FO_SHARED )
         IF Empty( handle )
            edi_Alert( _I("Can't open") + " " + cFileName )
            RETURN .F.
         ENDIF
         cBuff := hb_vfReadLen( handle, nSize )
         hb_vfClose( handle )
      ELSEIF hb_vfDirexists( cFileName )
         cBuff := "<Directory>"
      ENDIF
   ENDIF

   IF cColor == Nil; cColor := "W/B"; ENDIF
   SetColor( cColor )
   cp := hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )
   @ y1, x1 + 2 SAY NameShortcut( hb_fnameNameExt(cFileName), x2-x1-3-FILEINFO_LEN )

   IF !Empty( cPage ) .AND. !( cPage == cp )
      hb_cdpSelect( cPage )
   ENDIF

   DispBegin()
   s := ""
   DO WHILE nRow < y2
      c := Substr( cBuff, nPos, 1 )
      nPos ++
      nCol ++
      IF c == c0
         c := ' '
      ELSEIF c == c10
         c := ""
         nCol := x2
      ENDIF

      s += c
      IF nCol == x2
         IF lWords .AND. !(c == '') .AND. !(c $ cSeps) .AND. !(Substr(cBuff,nPos,1) == ' ')
            nPos1 := Len( s )
            DO WHILE nPos1 > 0
               IF Substr(s,nPos1,1) $ cSeps
                  EXIT
               ENDIF
               nPos1 --
            ENDDO
            IF nPos1 > 1
               nPos -= ( Len(s)-nPos1 )
               s := Left( s, nPos1 )
            ENDIF
         ENDIF
         @ nRow, x1 + 1 SAY s
         s := ""
         nRow ++
         nCol := x1 + 1
      ENDIF
   ENDDO
   DispEnd()

   hb_cdpSelect( cp )

   RETURN Nil

STATIC FUNCTION HexBytesPerLine( x1, x2 )

   LOCAL nBytes := Int( ( x2 - x1 - 1 - nAddrDigits - 5 ) / 4 )

   IF nBytes < 1
      nBytes := 1
   ELSEIF nBytes > HEXBYTES
      nBytes := HEXBYTES
   ENDIF

   RETURN nBytes

STATIC FUNCTION _HexDigits( n )

   LOCAL nDigits := 1

   n := Int( n )
   DO WHILE n >= 16
      n := Int( n / 16 )
      nDigits ++
   ENDDO

   RETURN nDigits

STATIC FUNCTION HexDraw( handle, nOffset, x1, y1, x2, y2 )

   LOCAL i, j, nHeight := y2 - y1 - 1, nSize := nBytesLine * nHeight
   LOCAL cBuff, cBuf, cAddr, cHex, cAsc, cByte, nAbs, nLen, nVal
   LOCAL nMid := Iif( nBytesLine == HEXBYTES, Int( HEXBYTES/2 ), HEXBYTES * 2 )

   IF !lEdited .AND. nOffset >= nShiftBuffer .AND. nOffset + nSize < nShiftBuffer + RDBUFFERSIZE
      cBuff := Substr( cBuffer, nOffset - nShiftBuffer + 1, nSize )
   ELSE
      hb_vfSeek( handle, nOffset )
      cBuff := hb_vfReadLen( handle, nSize )
   ENDIF

   FOR i := 1 TO nHeight

      nAbs := nOffset + ( i - 1 ) * nBytesLine
      cBuf := Substr( cBuff, nAbs-nOffset+1, nBytesLine )
      nLen := Len( cBuf )

      IF nLen == 0
         @ y1 + i, x1 + 1 CLEAR TO y1 + i, x2 - 1
      ELSE
         cAddr := hb_NumToHex( nAbs, nAddrDigits ) + ": "
         cHex  := ""
         cAsc  := ""
         FOR j := 1 TO nBytesLine
            IF j <= nLen
               cByte := Substr( cBuf, j, 1 )
               nVal  := Asc( cByte )
               cHex += hb_NumToHex( nVal, 2 ) + " "
               cAsc += Iif( nVal >= 32 .AND. nVal < 127, cByte, "." )
            ELSE
               cHex += "   "
               cAsc += " "
            ENDIF
            IF j == nMid
               cHex += "| "
            ENDIF
         NEXT
         IF nCodePage != 1
            cAsc := hb_Translate( cAsc, aCPages[nCodePage], aCPages[1] )
         ENDIF
         @ y1 + i, x1 + 1 SAY cAddr + cHex + " " + cAsc
      ENDIF
   NEXT

   DevPos( y1 + 1, x1 + 1 )

   RETURN Nil

STATIC FUNCTION _ChangeClr( y1, x1, cColor )

   LOCAL s := Savescreen( y1, x1, y1, x1 ), c

   IF Len( s ) == 2
      c := Left( s,1 )
   ELSE
      c := hb_Translate( cedi_Wchar_to_utf8( hb_bPeek(s,1) + hb_bPeek(s,2) * 256 ), ;
        "UTF8", Iif( nCodePage == 1, "RU866", "RU1251" ) )
   ENDIF

   @ y1, x1 SAY c COLOR cColor

   RETURN Nil

STATIC FUNCTION SetCursClr( nCursY, nCursX, cColor, y1 )

   LOCAL x1 := nAddrDigits + 2, nY1, nX1

   nY1 := y1 + nCursY - 1
   nX1 := x1 + (nCursX-1) * 3 + Iif( nBytesLine == HEXBYTES .AND. nCursX > HEXBYTES/2, 3, 1 )

   _ChangeClr( nY1, nX1, cColor )
   _ChangeClr( nY1, nX1+1, cColor )

   nX1 := x1 + nBytesLine * 3 + nCursX + Iif( nBytesLine == HEXBYTES, 3, 1 )
   _ChangeClr( nY1, nX1, cColor )

   RETURN Nil

STATIC FUNCTION _getByte( cInitValue )

   LOCAL clr1 := TEdit():cColorSel, clr2 := TEdit():cColorMenu
   LOCAL xRes := "", cBuf, oldc := SetColor( clr1 + "," + clr2 )
   LOCAL x1 := Int( MaxCol()/2 ) - 12, x2 := x1 + 24, y1 := Int( MaxRow()/2 ) - 1, y2 := y1 + 7
   LOCAL aGets, cpold, nRes, cByte

   aGets := { {y1+1,x1+2, 11, _I("Edit byte:")}, { y1+1,x1+13, 0, cInitValue, 2,,,"XX" }, ;
   {y1+3,x1+2, 11, _I("File will be updated"),,clr1,clr2}, ;
   {y1+4,x1+2, 11, _I("immedeately!"),,clr1,clr2}, ;
   {y1+6,x1+2,2,_I("[Save]"),10,clr1,clr2,{||__KeyBoard(Chr(K_ENTER))}}, ;
   {y1+6,x2-10,2,_I("[Cancel]"),10,clr1,clr2,{||__KeyBoard(Chr(K_ESC))}} }

   cBuf := Savescreen( y1, x1, y2, x2 )
   cpOld := hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+2, x1 SAY "Ã"
   @ y1+2, x2 SAY "´"
   @ y1+2, x1+1 TO y1+2, x2-1
   @ y1+5, x1 SAY "Ã"
   @ y1+5, x2 SAY "´"
   @ y1+5, x1+1 TO y1+5, x2-1
   hb_cdpSelect( cpold )

   DO WHILE .T.
      __keyboard( Chr(K_INS) )
      IF ( nRes := edi_READ( aGets ) ) <= 0
         EXIT
      ENDIF
      cByte := aGets[2,4]
      IF hb_HexToNum( cByte ) > 0 .OR. cByte == "00"
         EXIT
      ENDIF
   ENDDO
   IF nRes > 0
      xRes := cByte
   ENDIF

   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cBuf )

   RETURN xRes

STATIC FUNCTION EditByte( handle, nCursY, nCursX, nOffset )

   LOCAL nBytePos := nOffset + ( nCursY-1 ) * nBytesLine + nCursX - 1
   LOCAL cByte, cByteHex, cByteNew, nByte

   hb_vfSeek( handle, nBytePos )
   cByte := hb_vfReadLen( handle, 1 )
   cByteHex := hb_NumToHex( Asc(cByte), 2 )

   cByteNew := Trim( _getByte( cByteHex ) )
   IF !Empty( cByteNew )
      nByte := hb_HexToNum( cByteNew )

      hb_vfSeek( handle, nBytePos )
      hb_vfWrite( handle, Chr(nByte), 1 )
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION Draw( arr, nFirst, x1, y1, x2, y2 )

   LOCAL i, nHeight := y2 - y1 - 1, n, cTemp

   FOR i := 1 TO nHeight

      n := nFirst + i - 1
      IF n > Len( arr )
         @ y1 + i, x1 + 1 CLEAR TO y1 + i, x2 - 1
      ELSE
         cTemp := StrTran( arr[n], Chr(0), ' ' )
         IF !lShowCR
            cTemp := StrTran( StrTran( cTemp, Chr(10), ' ' ), Chr(13), ' ' )
         ENDIF
         IF nCodePage != 1
            cTemp := hb_Translate( cTemp, aCPages[nCodePage], aCPages[1] )
         ENDIF
         @ y1 + i, x1 + 1 SAY cTemp
         IF Len( cTemp ) < ( x2-x1-1 )
            @ y1 + i, x1 + 1 + Len(cTemp) SAY Space( x2-x1-1 - Len(cTemp) )
         ENDIF
      ENDIF
   NEXT
   DevPos( y1 + 1, x1 + 1 )

   RETURN Nil

STATIC FUNCTION ReadBufNext( handle, arr, nShift, nSize, nWidth, lLast )

   LOCAL nPos := 1, nLen, nPos2

   hb_vfSeek( handle, nShift )
   cBuffer := hb_vfReadLen( handle, nSize )
   nSize := Len( cBuffer )
   //edi_Writelog( "Bufnext1: " + ltrim(str(nShift)) + " " + ltrim(str(nSize)) + " " + ltrim(str(len(arr))) )
   DO WHILE .T.
      nLen := Min( nWidth, nSize-nPos+1 )
      nPos2 := hb_At( Chr(10), cBuffer, nPos, nPos + nLen - 1 )
      IF nPos2 == 0    // CR is absent in a current line
         IF nSize-nPos < nWidth   // A last line in a buffer
            IF lLast .AND. nLen > 0
               Aadd( arr, Substr( cBuffer, nPos, nLen ) )
            ELSE
               nSize := nPos - 1
            ENDIF
            EXIT
         ENDIF
      ELSE             // CR presents in a current line
         nLen := nPos2 - nPos + 1
      ENDIF
      Aadd( arr, Substr( cBuffer, nPos, nLen ) )
      nPos += nLen
   ENDDO
   //edi_Writelog( "Bufnext2: " + ltrim(str(len(arr))) )

   RETURN nSize

STATIC FUNCTION ReadBufPrev( handle, arr, nShift, nSize, nWidth, lFirst )

   LOCAL nPos, nLen, nPos2

   hb_vfSeek( handle, nShift )
   cBuffer := hb_vfReadLen( handle, nSize )
   nPos := nSize := Len( cBuffer )

   DO WHILE .T.
      nLen := Min( nWidth, nPos )
      nPos2 := hb_RAt( Chr(10), cBuffer, nPos - nLen + 1, nPos - 1 )
      IF nPos2 == 0    // CR is absent in a current line
         IF nPos < nWidth   // A first line in a buffer
            IF lFirst .AND. nLen > 0
               Aadd( arr, Nil )
               AIns( arr, 1 )
               arr[1] := Substr( cBuffer, 1, nLen )
            ELSE
               nSize -= (nPos - 1)
            ENDIF
            EXIT
         ENDIF
      ELSE             // CR presents in a current line
         nLen := nPos - nPos2
      ENDIF
      Aadd( arr, Nil )
      AIns( arr, 1 )
      arr[1] := Substr( cBuffer, nPos-nLen+1, nLen )
      nPos -= nLen
   ENDDO

   RETURN nSize