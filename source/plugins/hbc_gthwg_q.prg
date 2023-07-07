/*
 */

#define HB_GTI_SCREENWIDTH      1
#define HB_GTI_SCREENHEIGHT     2
#define HB_GTI_WINHANDLE        69

STATIC cImgFile, hImage, aImgSize

FUNCTION hbc_gthwg_q( oPaneTo, cFileName, lDo )

   LOCAL cp

   IF !lDo
      //edi_Writelog( "qend " + Iif( oPaneTo == FilePane():aPanes[1], "1", "2" ) )
      cImgFile := Nil
      IF !Empty( hImage )
         hwg_Deleteobject( hImage )
         hImage := Nil
      ENDIF
      oPaneTo:bPaint := Nil
      RETURN Nil
   ENDIF

   //edi_Writelog( "qstart " + Iif( oPaneTo == FilePane():aPanes[1], "1", "2" ) )
   IF !Empty( cImgFile ) .AND. !( cImgFile == cFileName )
      cImgFile := Nil
      IF !Empty( hImage )
         hwg_Deleteobject( hImage )
         hImage := Nil
      ENDIF
   ENDIF

   IF Empty( cImgFile )
      cImgFile := cFileName
      hImage := hwg_OpenImage( cImgFile )
      aImgSize := hwg_Getbitmapsize( hImage )
   ENDIF

   cp := hb_cdpSelect( "RU866" )
   @ oPaneTo:y1, oPaneTo:x1, oPaneTo:y2, oPaneTo:x2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( cp )

   oPaneTo:bPaint := {|o,h|gthwg_qView(o,h)}
   hwg_Invalidaterect( hb_gtinfo(HB_GTI_WINHANDLE), 0 )

   RETURN Nil

STATIC FUNCTION gthwg_qView( oPane, hDC )

   LOCAL xKoef, yKoef, nWidthMax, nHeightMax, nWidth, nHeight

   IF !Empty( hImage )
      xKoef := hb_gtinfo( HB_GTI_SCREENWIDTH ) / MaxCol()
      yKoef := hb_gtinfo( HB_GTI_SCREENHEIGHT ) / MaxRow()
      nWidthMax := Int( ( oPane:x2 - oPane:x1 - 4 ) * xKoef )
      nHeightMax := Int( ( oPane:y2 - oPane:y1 - 3 ) * yKoef )
      IF aImgSize[1] <= nWidthMax .AND. aImgSize[2] <= nHeightMax
         hwg_Drawbitmap( hDC, hImage,, Int((oPane:x1+2) * xKoef), Int((oPane:y1+2) * yKoef) )
      ELSE
         //edi_Writelog( " : " + ltrim(str(nWidthMax)) + "/" + ltrim(str(nHeightMax)) )
         IF aImgSize[1] > nWidthMax
            nWidth := nWidthMax
            nHeight := Int( aImgSize[2] * (nWidthMax/aImgSize[1]) )
            //edi_Writelog( "1: " + ltrim(str(nWidth)) + "/" + ltrim(str(nHeight)) )
            IF nHeight > nHeightMax
               nWidth := Int( nWidth * ( nHeightMax/nHeight ) )
               nHeight := nHeightMax
               //edi_Writelog( "2: " + ltrim(str(nWidth)) + "/" + ltrim(str(nHeight)) )
            ENDIF
         ELSE
            nHeight := nHeightMax
            nWidth := Int( aImgSize[1] * (nHeightMax/aImgSize[2]) )
            //edi_Writelog( "3: " + ltrim(str(nWidth)) + "/" + ltrim(str(nHeight)) )
            IF nWidth > nWidthMax
               nHeight := Int( nHeight * ( nWidthMax/nWidth) )
               nWidth := nWidthMax
               //edi_Writelog( "4: " + ltrim(str(nWidth)) + "/" + ltrim(str(nHeight)) )
            ENDIF
         ENDIF
         hwg_Drawbitmap( hDC, hImage,, Int((oPane:x1+2) * xKoef), Int((oPane:y1+2) * yKoef), nWidth, nHeight )
      ENDIF
   ENDIF

   RETURN Nil
