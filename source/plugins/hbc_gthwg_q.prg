/*
 */

#define HB_GTI_SCREENWIDTH      1
#define HB_GTI_SCREENHEIGHT     2
#define HB_GTI_WINHANDLE       69

#define IMAGE_ICON              1
#define LR_LOADFROMFILE        16
#define LR_DEFAULTSIZE         64
#define LR_SHARED           32768

STATIC cImgFile, nImgType, hImage, aImgSize, oPane

FUNCTION hbc_gthwg_q( oPaneTo, cFileName, cDo )

   LOCAL cp

   oPane := oPaneTo
   IF cDo == "qend"
      //edi_Writelog( "qend " + Iif( oPaneTo == FilePane():aPanes[1], "1", "2" ) )
      cImgFile := Nil
      IF !Empty( hImage )
         hwg_Deleteobject( hImage )
         hImage := Nil
      ENDIF
      gthwg_paint_SetCallback()
      //oPaneTo:bPaint := Nil
      RETURN Nil
   ELSEIF cDo == "qstart"
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
         IF Lower( hb_fnameExt( cFileName ) ) == ".ico"
#ifdef __PLATFORM__WINDOWS
            hImage := hwg_Loadimage( 0, cFileName, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE + LR_LOADFROMFILE + LR_SHARED )
#endif
            aImgSize := { 128, 128 }
            nImgType := 2
         ELSE
            hImage := hwg_OpenImage( cImgFile )
            aImgSize := hwg_Getbitmapsize( hImage )
            nImgType := 1
         ENDIF
      ENDIF

      cp := hb_cdpSelect( "RU866" )
      @ oPaneTo:y1, oPaneTo:x1, oPaneTo:y2, oPaneTo:x2 BOX "ÚÄ¿³ÙÄÀ³ "
      hb_cdpSelect( cp )

      //oPaneTo:bPaint := {|o,h|gthwg_qView(o,h)}
      //gthwg_paint_SetCallback( "GTHWG_PAINTCB" )
      gthwg_paint_SetCallback( "GTHWG_QVIEW" )
      hwg_Invalidaterect( hb_gtinfo(HB_GTI_WINHANDLE), 0 )

   ENDIF

   RETURN Nil

FUNCTION gthwg_qView( hDC )

   LOCAL xKoef, yKoef, nWidthMax, nHeightMax, nWidth, nHeight

   IF !Empty( hImage )
      xKoef := hb_gtinfo( HB_GTI_SCREENWIDTH ) / MaxCol()
      yKoef := hb_gtinfo( HB_GTI_SCREENHEIGHT ) / MaxRow()
      nWidthMax := Int( ( oPane:x2 - oPane:x1 - 4 ) * xKoef )
      nHeightMax := Int( ( oPane:y2 - oPane:y1 - 3 ) * yKoef )
      IF aImgSize[1] <= nWidthMax .AND. aImgSize[2] <= nHeightMax
         IF nImgType == 1
            hwg_Drawbitmap( hDC, hImage,, Int((oPane:x1+2) * xKoef), Int((oPane:y1+2) * yKoef) )
         ELSEIF nImgType == 2
#ifdef __PLATFORM__WINDOWS
            hwg_Drawicon( hDC, hImage, Int((oPane:x1+2) * xKoef), Int((oPane:y1+2) * yKoef) )
#endif
         ENDIF
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
