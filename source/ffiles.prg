/*
 * Few functions, related to file system.
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "fileio.ch"
#include "fedit.ch"

FUNCTION edi_SeleFile( oEdit, cPath, y1, x1, y2, x2, cMask )

   LOCAL aMenu := edi_Directory( cPath, cMask ), i, nPos, arr
   LOCAL bSea := {|nop,cSea,cLine|
      LOCAL cBuff, n1, n2, n3, lCase
      IF nop == 0
         IF ( n1 := At( '/', cSea ) ) > 0
            IF Len( cSea ) > n1 .AND. Right( cSea,1 ) == '/'
               RETURN Left( cSea, n1-1 )
            ELSE
               RETURN Nil
            ENDIF
         ELSE
            RETURN cSea
         ENDIF
      ELSE
         IF ( n1 := At( '/', cSea ) ) > 0 .AND. ( n2 := hb_At( '/', cSea, n1+1 ) ) > 0 ;
            .AND. ( n3 := hb_At( '/', cSea, n2+1 ) ) > 0
            lCase := ! ( n3-n2 > 1 .AND. Substr( cSea,n2+1,1 ) == "c" )
            cSea := Substr( cSea, n1+1, n2-n1-1 )
            IF lCase
               cBuff := Memoread( cPath + cLine )
            ELSE
               cSea := cp_Lower( oEdit:lUtf8, cSea )
               cBuff := cp_Lower( oEdit:lUtf8, Memoread( cPath + cLine ) )
            ENDIF
            IF !( cSea $ cBuff )
               RETURN .F.
            ENDIF
         ENDIF
      ENDIF
      RETURN .T.
   }

   DO WHILE .T.
      aMenu := hb_AIns( aMenu, 1, { "---" + PadC(NameShortcut(cPath,x2-x1-9),x2-x1-9) + "---","","","",""}, .T. )
      arr := FMenu( oEdit, aMenu, y1, x1, y2, x2,,,, .T., .T., bSea )

      IF !Empty( arr )
         IF Len( arr ) > 1 .OR. Empty( aMenu[arr[1],4] )
            FOR i := Len( arr ) TO 1 STEP -1
               IF Empty( aMenu[arr[i],4] )
                  arr[i] := cPath + aMenu[arr[i],1]
               ELSE
                  hb_ADel( arr, i, .T. )
               ENDIF
            NEXT
            RETURN arr
         ELSE
            i := arr[1]
            IF aMenu[i,1] == ".."
               IF ( nPos := hb_Rat( hb_ps(), cPath,, Len(cPath)-1 ) ) > 0
                  cPath := Left( cPath, nPos )
                  aMenu := edi_Directory( cPath, cMask )
               ENDIF
            ELSE
               cPath += aMenu[i,1] + hb_ps()
               aMenu := edi_Directory( cPath, cMask )
            ENDIF
         ENDIF
      ELSE
         RETURN Nil
      ENDIF
   ENDDO

   RETURN Nil

FUNCTION edi_Directory( cPath, cMask )

   LOCAL aDirTmp := Directory( cPath, "HSD" ), aMenu
   LOCAL i, j, l1 := .F., l2 := .F., nPos

   FOR i := 1 TO Len( aDirTmp )
      IF Empty( aDirTmp[i] )
         LOOP
      ELSEIF aDirTmp[i,1] == "."
         ADel( aDirTmp, i )
         i --
         l1 := .T.
      ELSEIF "D" $ aDirTmp[i,5]
         IF aDirTmp[i,1] == ".."
            l2 := .T.
         ENDIF
         aDirTmp[i,1] := " " + aDirTmp[i,1]
      ENDIF
   NEXT
   IF l1
      aDirTmp := ASize( aDirTmp, Len(aDirTmp)-1 )
   ENDIF
   IF !l2
      nPos := Len( cPath ) - Iif( Right(cPath,1) $ "\/", 1, 0 )
      IF ( hb_Rat( '/',cPath,nPos ) != 0 .OR. hb_Rat( '\',cPath,nPos ) != 0 ) .AND. Substr(cPath,2,1) != ':'
         Aadd( aDirTmp, Nil )
         AIns( aDirTmp, 1 )
         aDirTmp[1] := { " ..",0,Date(),"","D" }
      ENDIF
   ENDIF
   aDirTmp := ASort( aDirTmp,,, {|z,y|Lower(z[1]) < Lower(y[1])} )
   aMenu := Array( Len( aDirTmp ) )
   j := 0
   FOR i := 1 TO Len( aDirTmp )
      IF "D" $ aDirTmp[i,5]
         IF Left( aDirTmp[i,1],1 ) == " "
            aDirTmp[i,1] := Substr( aDirTmp[i,1],2 )
         ENDIF
      ELSEIF !Empty( cMask )
         IF !hb_filematch( aDirTmp[i,1], cMask )
            LOOP
         ENDIF
      ENDIF
      j ++
      aMenu[j] := { aDirTmp[i,1], Nil, Nil, Iif("D" $ aDirTmp[i,5], "<DIR>", Nil ) }
   NEXT
   IF j < Len( aMenu )
      aMenu := ASize( aMenu, j )
   ENDIF

   RETURN aMenu

FUNCTION edi_IniRead( cFileName )

   LOCAL cText := Memoread( cFileName ), aText, i, s, nPos
   LOCAL hIni, hSect

   IF Empty( cText )
      RETURN Nil
   ENDIF

   aText := hb_aTokens( cText, Chr(10) )
   hIni := hb_Hash()

   FOR i := 1 TO Len( aText )
      s := Iif( Left( aText[i],1 ) == ' ', Ltrim( aText[i] ), aText[i] )
      IF Left( s, 1 ) $ ";#"
         LOOP
      ENDIF
      s := Trim( Iif( Right(s,1)==Chr(13), Left( s,Len(s)-1 ), s ) )
      IF Empty( s )
         LOOP
      ENDIF

      IF Left( s,1 ) == '[' .AND. Right( s,1 ) == ']'
         hSect := hIni[Substr( s,2,Len(s)-2 )] := hb_Hash()
      ELSEIF !( hSect == Nil )
         IF ( nPos := At( '=', s ) ) > 0
            hSect[Trim(Left(s,nPos-1))] := Ltrim( Substr( s,nPos+1 ) )
         ENDIF
      ENDIF
   NEXT

   RETURN hIni

FUNCTION edi_CurrPath()

   LOCAL cPrefix

#ifdef __PLATFORM__UNIX
   cPrefix := '/'
#else
   cPrefix := hb_curDrive() + ':\'
#endif

   RETURN cPrefix + CurDir() + hb_ps()

FUNCTION edi_FindPath( cFile )

   LOCAL cFullPath

#ifdef __PLATFORM__UNIX
   IF File( cFullPath := ( edi_CurrPath() + cFile ) ) .OR. ;
      File( cFullPath := ( getenv("HOME") + "/hbedit/" + cFile ) ) .OR. ;
      File( cFullPath := ( hb_DirBase() + cFile ) )
#else
   IF File( cFullPath := ( edi_CurrPath() + cFile ) ) .OR. ;
      File( cFullPath := ( hb_DirBase() + cFile ) )
#endif
      RETURN cFullPath
   ENDIF

   RETURN Nil

#define  BUFFSIZE  32768

FUNCTION edi_CopyFile( cFileSrc, cFileDst, aWnd )

   LOCAL phDst, phSrc, nBytes, cBuff := Space( BUFFSIZE ), nSize, nCopied := 0

   //IF '\' $ cFileName
   //   cFileName := StrTran( cFileName, '\', '/' )
   //ENDIF
   IF !Empty( phDst := hb_vfOpen( cFileDst, FO_WRITE+FO_CREAT+FO_TRUNC ) )
      IF !Empty( phSrc := hb_vfOpen( cFileSrc, FO_READ ) )
         nSize := hb_vfSize( phSrc )
#ifndef _NO_HBC
         hbc_WndProgress( aWnd, 0 )
#endif
         DO WHILE ( nBytes := hb_vfRead( phSrc, @cBuff, BUFFSIZE ) ) > 0
            hb_vfWrite( phDst, cBuff, nBytes )
            nCopied += nBytes
#ifndef _NO_HBC
            IF Inkey() == 27 .AND. !FAsk_Abort( cFileSrc, nSize, nCopied )
               hb_vfClose( phSrc )
               hb_vfClose( phDst )
               hb_vfErase( cFileDst )
               RETURN -3
            ENDIF
            hbc_WndProgress( aWnd, nCopied / nSize )
#endif
         ENDDO
         hb_vfClose( phSrc )
         hb_vfClose( phDst )
      ELSE
         hb_vfClose( phDst )
         hb_vfErase( cFileDst )
         edi_Alert( "Can't open " + cFileSrc )
      ENDIF
   ELSE
      edi_Alert( "Can't open " + cFileDst )
   ENDIF

   RETURN 0

FUNCTION edi_WriteLog( cText, fname )

   LOCAL nHand

   fname := hb_DirBase() + IIf( fname == Nil, "a.log", fname )
   IF ! File( fname )
      nHand := FCreate( fname )
   ELSE
      nHand := FOpen( fname, 1 )
   ENDIF
   FSeek( nHand, 0, 2 )
   FWrite( nHand, cText + Chr( 10 ) )
   FClose( nHand )

   RETURN Nil