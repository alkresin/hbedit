
#define  K_ENTER   13
#define  K_ESC     27

FUNCTION plug_hbc_fb2_quick( oPane )

   LOCAL oPaneTo

   oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   oPaneTo:cQVpref := "FB2"

   RETURN Nil

FUNCTION PLUG_HBC_FB2_QVIEW( oPane, aParams )

   LOCAL oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   LOCAL cColor := "W/B", cp
   LOCAL hUnzip, cFileName, nSize := 64000, cUnzBuff, nLen := 0, cBuff := ""
   LOCAL cEnc, nPos, nPos1, nPos2, cTemp

   cFileName := aParams[1]

   SetColor( cColor )
   cp := hb_cdpSelect( "RU866" )
   @ oPaneTo:y1, oPaneTo:x1, oPaneTo:y2, oPaneTo:x2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )


   IF !Empty( hUnzip := hb_unzipOpen( cFileName ) )
      hb_unzipFileFirst( hUnzip )
      //hb_unzipFileInfo( hUnzip,,,,,,, @nSize, )
      IF hb_unzipFileOpen( hUnzip, Nil ) == 0
         cUnzBuff := Space( nSize )
         nLen := hb_unzipFileRead( hUnzip, @cUnzBuff )
         hb_unzipFileClose( hUnzip )
      ENDIF
      hb_unzipClose( hUnzip )
   ENDIF
   IF nLen == 0
      QFileView( cFileName, "Can't read file", oPaneTo:x1, oPaneTo:y1, oPaneTo:x2, oPaneTo:y2 )
      RETURN Nil
   ENDIF

   cEnc := fb2_enc( cUnzBuff )
   IF ( nPos1 := At( "<title-info", cUnzBuff ) ) != 0
      IF ( nPos2 := hb_At( "</title-info", cUnzBuff, nPos1 ) ) != 0
         cUnzBuff := SubStr( cUnzBuff, nPos1, nPos2 - nPos1 )

         // Author
         cTemp := ""
         nPos := 1
         DO WHILE ( nPos1 := hb_At( "<author", cUnzBuff, nPos ) ) != 0 .AND. ( nPos2 := hb_At( "</author", cUnzBuff, nPos1 ) ) != 0
            IF !Empty( cTemp )
               cTemp += ", "
            ENDIF
            cBuff := AllTrim( SubStr( cUnzBuff, nPos1 + 8, nPos2 - nPos1 - 8 ) )
            nPos := nPos2 + 8
            IF ( nPos1 := At( "<first-name", cBuff ) ) != 0 .AND. ( nPos2 := hb_At( "</first-name", cBuff, nPos1 ) ) != 0
               cTemp += AllTrim( SubStr( cBuff, nPos1 + 12, nPos2 - nPos1 - 12 ) ) + " "
            ENDIF
            IF ( nPos1 := At( "<middle-name", cBuff ) ) != 0 .AND. ( nPos2 := hb_At( "</middle-name", cBuff, nPos1 ) ) != 0
               cTemp += AllTrim( SubStr( cBuff, nPos1 + 13, nPos2 - nPos1 - 13 ) ) + " "
            ENDIF
            IF ( nPos1 := At( "<last-name", cBuff ) ) != 0 .AND. ( nPos2 := hb_At( "</last-name", cBuff, nPos1 ) ) != 0
               cTemp += AllTrim( SubStr( cBuff, nPos1 + 11, nPos2 - nPos1 - 11 ) ) + " "
            ENDIF
            cTemp := Trim( cTemp )
         ENDDO
         IF Empty( cTemp )
            cBuff := ""
         ELSE
            cBuff := Trim( cTemp ) + hb_eol()
         ENDIF

         // Book title
         IF ( nPos1 := At( "<book-title", cUnzBuff ) ) != 0 .AND. ( nPos2 := hb_At( "</book-title", cUnzBuff, nPos1 ) ) != 0
            cTemp := AllTrim( SubStr( cUnzBuff, nPos1 + 12, nPos2 - nPos1 - 12 ) )
            cBuff += Trim( cTemp ) + hb_eol()
         ENDIF

         // Annotation
         IF ( nPos1 := At( "<annotation", cUnzBuff ) ) != 0 .AND. ( nPos2 := hb_At( "</annotation", cUnzBuff, nPos1 ) ) != 0
            cTemp := AllTrim( SubStr( cUnzBuff, nPos1 + 12, nPos2 - nPos1 - 12 ) )
            cBuff += hb_eol() + Trim( cTemp )
            cBuff := StrTran( cBuff, "</p>", hb_eol() )
            DO WHILE ( nPos1 := At( "<", cBuff ) ) > 0
               IF ( nPos2 := hb_At( ">",cBuff, nPos1 ) ) > 0
                  cBuff := Left( cBuff, nPos1-1 ) + Substr( cBuff, nPos2+1 )
               ELSE
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF cEnc == "utf-8"
            cBuff := StrTran( cBuff, 'Â«', '"' )
            cBuff := StrTran( cBuff, 'Â»', '"' )
            cBuff := StrTran( cBuff, 'â€”', '-' )
            //hb_strReplace( cBuff, {'«','»','â€”'}, {'"','"','-'} )
            cBuff := hb_utf8ToStr( cBuff, "RU866" )
         ELSEIF cEnc == "windows-1251"
            cBuff := hb_Translate( cBuff, "RU1251", "RU866" )
         ENDIF
      ENDIF
   ENDIF

   QFileView( cFileName, cBuff, oPaneTo:x1, oPaneTo:y1, oPaneTo:x2, oPaneTo:y2,, "RU866", .T. )

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
