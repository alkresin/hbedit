/*
 * File viewer, based on Harbour File IO API
 *
 * Copyright 2016 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"
#include "fileio.ch"

#define RDBUFFERSIZE  16384
#define MAXLEN       100000
#define FILEINFO_LEN      8

REQUEST HB_CODEPAGE_RU866
REQUEST HB_CODEPAGE_RU1251
REQUEST HB_CODEPAGE_RUKOI8

Memvar lShowCR, nCodePage

STATIC aCPages := { "RU866", "RU1251", "RUKOI8" }

FUNCTION FileView( cFileName, x1, y1, x2, y2, cColor )

   LOCAL handle := hb_vfOpen( cFileName, FO_READ+FO_SHARED )
   LOCAL cScBuf, nFirst := 1, nFileLen, nShiftBuf := 0, nSize
   LOCAL nKey, lDraw := .T.
   LOCAL arr := {}, arrtmp, i, nLenATmp, nSizeATmp, cp
   PRIVATE lShowCR := .F., nCodePage := 1

   IF Empty( handle )
      RETURN .F.
   ENDIF

   nFileLen := hb_vfSize( handle )
   nLenATmp := (y2-y1-1) * 2
   arrtmp := Iif( nFileLen > MAXLEN, Array( nLenATmp ), Nil )
   nSize := Iif( nFileLen > MAXLEN, RDBUFFERSIZE, nFileLen )
   nSize := ReadBufNext( handle, arr, nShiftBuf, nSize, x2-x1-1, ((nShiftBuf+nSize)>=nFileLen) )

   IF cColor == Nil; cColor := "W/B"; ENDIF

   cScBuf := Savescreen( y1, x1, y2, x2 )
   SetColor( cColor )
   cp := hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )
   @ y1, x1 + 2 SAY NameShortcut( cFileName, x2-x1-3-FILEINFO_LEN )
   @ y1, x2 - 8 SAY aCPages[nCodePage]

   DO WHILE .T.
      IF lDraw
         Draw( arr, nFirst, x1, y1, x2, y2 )
         lDraw := .F.
      ENDIF
      IF ( nKey := Inkey( 0 ) ) == K_F10 .OR. nKey == K_ESC
         EXIT

      ELSEIF nKey == K_DOWN .OR. nKey == K_PGDN
         IF nKey == K_DOWN .AND. nFirst + (y2-y1-1) - 1 < Len( arr )
            nFirst ++

         ELSEIF nKey == K_PGDN .AND. nFirst + (y2-y1-2) <= Len( arr ) .AND. ;
               ( (nFirst + (y2-y1-2) + (y2-y1-2) <= Len( arr )) .OR. (nShiftBuf + nSize >= nFileLen) .OR. (nFileLen <= MAXLEN) )
            nFirst += (y2-y1-2)

         ELSEIF nShiftBuf + nSize < nFileLen .AND. nFileLen > MAXLEN
            nShiftBuf += nSize
            nSize := RDBUFFERSIZE
            ACopy( arr, arrtmp, Len(arr)-nLenATmp+1, nLenATmp )
            nSizeATmp := 0
            FOR i := 1 TO nLenATmp
               nSizeATmp += Len( arrtmp[i] )
            NEXT
            arr := arrtmp
            nSize := ReadBufNext( handle, arr, nShiftBuf, nSize, x2-x1-1, ((nShiftBuf+nSize)>=nFileLen) )
            nShiftBuf -= nSizeATmp
            nSize += nSizeATmp
            nFirst := Iif( nKey == K_DOWN, (y2-y1-1) + 2, nLenATmp + 1 )
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
            nShiftBuf := 0
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
            nShiftBuf := nFileLen - nSize + 1
         ENDIF
         nFirst := Max( Len( arr ) - (y2-y1-2), 1 )
         lDraw := .T.

      ELSEIF nKey == K_F2
         lShowCR := !lShowCR
         lDraw := .T.

      ELSEIF nKey == K_F8
         nCodePage := Iif( nCodePage==1, 2, 1 )
         @ y1, x2 - 8 SAY PAdr( aCPages[nCodePage],6 )
         lDraw := .T.
      ENDIF
   ENDDO

   Restscreen( y1, x1, y2, x2, cScBuf )

   hb_vfClose( handle )
   RETURN .T.

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
         IF Len( arr[n] ) < ( x2-x1-1 )
            @ y1 + i, x1 + 1 + Len(arr[n]) SAY Space( x2-x1-1 - Len(arr[n]) )
         ENDIF
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION ReadBufNext( handle, arr, nShift, nSize, nWidth, lLast )

   LOCAL cBuffer, nPos := 1, nLen, nPos2

   hb_vfSeek( handle, nShift )
   cBuffer := hb_vfReadLen( handle, nSize )
   nSize := Len( cBuffer )
   DO WHILE .T.
      nLen := Min( nWidth, nSize-nPos )
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

   RETURN nSize

STATIC FUNCTION ReadBufPrev( handle, arr, nShift, nSize, nWidth, lFirst )

   LOCAL cBuffer, nPos, nLen, nPos2

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
