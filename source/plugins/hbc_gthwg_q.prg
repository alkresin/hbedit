/*
 */

#define HB_GTI_SCREENWIDTH      1
#define HB_GTI_SCREENHEIGHT     2
#define HB_GTI_WINHANDLE       69

#define IMAGE_ICON              1
#define LR_LOADFROMFILE        16
#define LR_DEFAULTSIZE         64
#define LR_SHARED           32768

STATIC cImgFile, nImgType, hImage, aImgSize, oPane, handle

FUNCTION hbc_gthwg_q( oPaneTo, cFileName, cDo, xDopInfo )

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
            IF Empty( hImage )
               hImage := hwg_GdiplusOpenimage( cFileName )
            ENDIF
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

   ELSEIF cDo == "dlg"
      ImgViewDlg( cFileName, xDopInfo )
   ENDIF

   RETURN Nil

STATIC FUNCTION ImgViewDlg( cFileName, cImageBuff )

   LOCAL oDlg, oPanel, nWidth, nHeight, aBmpSize, nWidthMax, nHeightMax

   IF !Empty( cImageBuff )
      handle := hwg_Openimage( cImageBuff, .T. )
      IF Empty( handle )
         cFileName := hb_DirTemp() + hb_fnameNameExt( cFileName )
         hb_Memowrit( cFileName, cImageBuff )
         handle := hwg_GdiplusOpenimage( cFileName )
         //edi_alert( cFileName + ": " + Iif( empty(handle),"F","T" ) )
         FErase( cFileName )
      ENDIF
   ELSE
      IF Empty( handle := hwg_Openimage( cFileName ) )
         handle := hwg_GdiplusOpenimage( cFileName )
      ENDIF
      //edi_Alert( "Image absent" )
      //RETURN Nil
   ENDIF
   IF Empty( handle )
      edi_Alert( "Can't display image" )
      RETURN Nil
   ENDIF

   aBmpSize  := hwg_Getbitmapsize( handle )
   nWidthMax := hwg_GetDesktopWidth() - 50
   nHeightMax := hwg_GetDesktopHeight() - 50
   IF aBmpSize[1] <= nWidthMax .AND. aBmpSize[2] <= nHeightMax
      nWidth := aBmpSize[1]
      nHeight := aBmpSize[2]
   ELSE
      IF aBmpSize[1] > nWidthMax
         nWidth := nWidthMax
         nHeight := Int( aBmpSize[2] * (nWidthMax/aBmpSize[1]) )
         IF nHeight > nHeightMax
            nWidth := Int( nWidth * ( nHeightMax/nHeight ) )
            nHeight := nHeightMax
         ENDIF
      ELSE
         nHeight := nHeightMax
         nWidth := Int( aBmpSize[1] * (nHeightMax/aBmpSize[2]) )
         IF nWidth > nWidthMax
            nHeight := Int( nHeight * ( nWidthMax/nWidth) )
            nWidth := nWidthMax
         ENDIF
      ENDIF
   ENDIF

   oDlg := HDialog():New( 11,,0,0,nWidth,nHeight,hb_fnameName( cFileName ),,,{||hwg_Deleteobject(handle),.T.},,,,,,.F.,,,.F.,,,.F.,,.F. )
   oPanel := HPanel():New(,,13,0,0,nWidth,nHeight,,,{||PPanel(oPanel)},, )
   oPanel:Anchor := 1 + 2 + 8 + 4
   oDlg:Activate( .T., .F., .F., .F., )


   RETURN Nil

STATIC FUNCTION PPanel( oPanel )
   LOCAL pps, hDC, aCoors

   pps := hwg_Definepaintstru()
   hDC := hwg_Beginpaint( oPanel:handle, pps )
   aCoors := hwg_Getclientrect( oPanel:handle )

   hwg_Drawbitmap( hDC, handle,, 0, 0, aCoors[3]-4, aCoors[4]-4 )

   hwg_Endpaint( oPanel:handle, pps )

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
