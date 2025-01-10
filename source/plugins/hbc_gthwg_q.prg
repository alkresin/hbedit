/*
 * HbEdit plugin to show an image on a pane or in a separate dialog window,
 *    it may be called from other plugins
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define HB_GTI_SCREENWIDTH      1
#define HB_GTI_SCREENHEIGHT     2
#define HB_GTI_WINHANDLE       69

#define IMAGE_ICON              1
#define LR_LOADFROMFILE        16
#define LR_DEFAULTSIZE         64
#define LR_SHARED           32768

#define CLR_BLACK           0
#define CLR_DGRAY1   0x222222
#define CLR_DGRAY2   0x555555
#define CLR_DGRAY3   0x888888
#define CLR_WHITE    0xFFFFFF

#define SB_LINEUP           0
#define SB_LINEDOWN         1
#define SCROLLVRANGE       20
#define SCROLLHRANGE       20

#define WM_MOUSEMOVE          512
#define WM_LBUTTONDOWN        513
#define WM_LBUTTONUP          514
#define WM_ERASEBKGND          20

#define K_PGUP                 18
#define K_PGDN                  3

STATIC nImgType, hImage, aImgSize, oPane
STATIC lUnix, nScreenW, nScreenH, nVert, nHorz
STATIC cFunc := "HWG_GDIPLUSOPENIMAGE"

FUNCTION hbc_gthwg_q( oPaneTo, cFileName, cDo, xDopInfo )

   LOCAL cImgFile
   LOCAL cp

   oPane := oPaneTo
   IF cDo == "qend"
      cImgFile := Nil
      IF !Empty( hImage )
         hwg_Deleteobject( hImage )
         hImage := Nil
      ENDIF
      gthwg_paint_SetCallback()
      RETURN Nil
   ELSEIF cDo == "qstart"
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
               IF hb_isFunction( cFunc )
                  hImage := Eval( &( "{|s|" + cFunc + "(s)}" ), cFileName )
               ENDIF
            ENDIF
            aImgSize := hwg_Getbitmapsize( hImage )
            nImgType := 1
         ENDIF
      ENDIF

      cp := hb_cdpSelect( "RU866" )
      @ oPaneTo:y1, oPaneTo:x1, oPaneTo:y2, oPaneTo:x2 BOX "ÚÄ¿³ÙÄÀ³ "
      hb_cdpSelect( cp )

      gthwg_paint_SetCallback( "GTHWG_QVIEW" )
      hwg_Invalidaterect( hb_gtinfo(HB_GTI_WINHANDLE), 0 )

   ELSEIF cDo == "dlg"
      ImgViewDlg( cFileName, xDopInfo )
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
         @ oPane:y1+1, oPane:x1+2 SAY PAdr( Ltrim(Str(aImgSize[1])) + "x" + Ltrim(Str(aImgSize[2])), 20 )
         IF nImgType == 1
            hwg_Drawbitmap( hDC, hImage,, Int((oPane:x1+2) * xKoef), Int((oPane:y1+2) * yKoef) )
         ELSEIF nImgType == 2
#ifdef __PLATFORM__WINDOWS
            hwg_Drawicon( hDC, hImage, Int((oPane:x1+2) * xKoef), Int((oPane:y1+2) * yKoef) )
#endif
         ENDIF
      ELSE
         IF aImgSize[1] > nWidthMax
            nWidth := nWidthMax
            nHeight := Int( aImgSize[2] * (nWidthMax/aImgSize[1]) )
            IF nHeight > nHeightMax
               nWidth := Int( nWidth * ( nHeightMax/nHeight ) )
               nHeight := nHeightMax
            ENDIF
         ELSE
            nHeight := nHeightMax
            nWidth := Int( aImgSize[1] * (nHeightMax/aImgSize[2]) )
            IF nWidth > nWidthMax
               nHeight := Int( nHeight * ( nWidthMax/nWidth) )
               nWidth := nWidthMax
            ENDIF
         ENDIF
         hwg_Drawbitmap( hDC, hImage,, Int((oPane:x1+2) * xKoef), Int((oPane:y1+2) * yKoef), nWidth, nHeight )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION ImgViewDlg( cFileName, xDopInfo )

   LOCAL oDlg, oBoard, oPaneDrawn, nWidth, nHeight, aBmpSize, handle
   LOCAL lFull := ( Valtype(xDopInfo) == "L" .AND. xDopInfo )
   LOCAL oWnd := HWindow():Getmain(), aCoors := hwg_Getclientrect( oWnd:handle ), arrSize
   LOCAL aCorners := { 4,4,4,4 }
   LOCAL aStyles := { HStyle():New( { CLR_DGRAY2 }, 1, aCorners ), ;
      HStyle():New( { CLR_WHITE }, 2, aCorners ), ;
      HStyle():New( { CLR_DGRAY3 }, 1, aCorners ) }

   lUnix := hb_Version(20)
   nScreenW := hwg_getDesktopWidth() - 20
   nScreenH := hwg_getDesktopHeight() - 50
   nVert := ( oWnd:nHeight - aCoors[4] )
   nHorz := ( oWnd:nWidth - aCoors[3] )

   IF Valtype( xDopInfo ) == "C"
      handle := hwg_Openimage( xDopInfo, .T. )
      IF Empty( handle )
         IF hb_isFunction( cFunc )
            cFileName := hb_DirTemp() + hb_fnameNameExt( cFileName )
            hb_Memowrit( cFileName, xDopInfo )
            handle := Eval( &( "{|s|" + cFunc + "(s)}" ), cFileName )
            FErase( cFileName )
         ENDIF
      ENDIF
   ELSE
      IF Empty( handle := hwg_Openimage( cFileName ) )
         IF hb_isFunction( cFunc )
            handle := Eval( &( "{|s|" + cFunc + "(s)}" ), cFileName )
         ENDIF
      ENDIF
   ENDIF
   IF Empty( handle )
      edi_Alert( "Can't display image" )
      RETURN Nil
   ENDIF

   aBmpSize  := hwg_Getbitmapsize( handle )
   arrSize := SetSize( aBmpSize[1], aBmpSize[2] )
   nWidth := arrSize[1]
   nHeight := arrSize[2]

   oDlg := HDialog():New( 11,,0,0,nWidth,nHeight,"",,, ;
      {||hwg_Deleteobject(oDlg:cargo["h"]),.T.},,,,,,.F.,,,.F.,,,.F.,,.F. )

   oBoard := HBoard():New( ,, 0, 0, nWidth, nHeight,,, {|o,x,y|o:Move(,,x,y-4)}, {|o,h|FPaint(o,h)} )
   hwg_SetCtrlName( oBoard, "OBOARD" )
   oBoard:bOther := {|o,m,wp,lp|BoardProc( o, m, wp, lp ) }

   oPaneDrawn := HDrawn():New( oBoard, 4, 4, 95, Iif(lFull,130,100), CLR_WHITE, CLR_BLACK,, ;
       ,,,, {|o,n|o:lHide:=(n==0),o:Refresh(),-1} )
   oPaneDrawn:lHide := .T.

   HDrawn():New( oPaneDrawn, 16, 12, 30, 30, CLR_WHITE, CLR_DGRAY1, aStyles, ;
       '+',,, {|| Zoom(oDlg,1) }, )
   HDrawn():New( oPaneDrawn, 56, 12, 30, 30, CLR_WHITE, CLR_DGRAY1, aStyles, ;
       '-',,, {|| Zoom(oDlg,-1) }, )
   HDrawn():New( oPaneDrawn, 16, 52, 30, 30, CLR_WHITE, CLR_DGRAY1, aStyles, ;
       '=',,, {|| Zoom(oDlg,0) }, )
   IF lFull
      HDrawn():New( oPaneDrawn, 16, 92, 30, 30, CLR_WHITE, CLR_DGRAY1, aStyles, ;
          '<=',,, {|| FNext( oDlg,.F. ) }, )
      HDrawn():New( oPaneDrawn, 56, 92, 30, 30, CLR_WHITE, CLR_DGRAY1, aStyles, ;
          '=>',,, {|| FNext(oDlg,.T.) }, )
      hwg_SetDlgKey( oDlg, 0, Iif( lUnix, 0xFF56, 0x22 ), {||FNext(oDlg,.T.)} )
      hwg_SetDlgKey( oDlg, 0, Iif( lUnix, 0xFF55, 0x21 ), {||FNext(oDlg,.F.)} )
   ENDIF

   hwg_SetDlgKey( oDlg, 0, Iif( lUnix, 0xFF1B, 27 ), {||oDlg:Close()} )
   hwg_SetDlgKey( oDlg, 0, Iif( lUnix, 0xFF54, 0x28 ), {||VScroll(oDlg,SB_LINEDOWN)} )
   hwg_SetDlgKey( oDlg, 0, Iif( lUnix, 0xFF52, 0x26 ), {||VScroll(oDlg,SB_LINEUP)} )
   hwg_SetDlgKey( oDlg, 0, Iif( lUnix, 0xFF53, 0x27 ), {||HScroll(oDlg,SB_LINEDOWN)} )
   hwg_SetDlgKey( oDlg, 0, Iif( lUnix, 0xFF51, 0x25 ), {||HScroll(oDlg,SB_LINEUP)} )

   oDlg:Activate( .T., .F., .F., .F., {|| FDlgInit(oDlg,cFileName,handle,arrSize[3])} )

   RETURN Nil

STATIC FUNCTION FDlgInit( oDlg, cFileName, handle, nKoef )

   LOCAL aBmpSize  := hwg_Getbitmapsize( handle )

   IF Empty( oDlg:cargo )
      oDlg:cargo := hb_Hash()
      oDlg:cargo["dir"] := cFileName
   ENDIF

   oDlg:cargo["h"] := handle
   oDlg:cargo["imgname"] := hb_fnameNameExt( cFileName )
   oDlg:cargo["offseth"] := 0
   oDlg:cargo["offsetv"] := 0
   oDlg:cargo["scrollh"] := .F.
   oDlg:cargo["scrollv"] := .F.
   oDlg:cargo["width"] := aBmpSize[1]
   oDlg:cargo["height"] := aBmpSize[2]
   oDlg:cargo["koef"] := nKoef

   oDlg:SetTitle( oDlg:cargo["imgname"] + "  (" + Str(nKoef*100,3,0) + "%  " + ;
      Ltrim(Str(aBmpSize[1])) + "x" + Ltrim(Str(aBmpSize[2])) + " )" )

   RETURN .T.

STATIC FUNCTION FPaint( o, hDC )

   LOCAL oDlg := o:oParent, handle := oDlg:cargo["h"], nKoef := oDlg:cargo["koef"]

   IF !Empty( handle )
      hwg_Drawbitmap( hDC, handle,, oDlg:cargo["offseth"], ;
         oDlg:cargo["offsetv"], oDlg:cargo["width"] * nKoef, oDlg:cargo["height"] * nKoef )
   ENDIF

   RETURN -1

STATIC FUNCTION BoardProc( oBoard, msg, wParam, lParam )

   LOCAL oDlg := oBoard:oParent, nPosX, nPosY, o
   LOCAL lScrollV, lScrollH, nOffsetV, nOffsetH, nImgWidth, nImgHeight, nKoef
   STATIC lDown := .F., nOldX, nOldY

   IF msg == WM_MOUSEMOVE
      IF !Empty( oDlg:cargo )
         lScrollV := oDlg:cargo["scrollv"]; lScrollH := oDlg:cargo["scrollh"]
         nOffsetV := oDlg:cargo["offsetv"]; nOffsetH := oDlg:cargo["offseth"]
         nImgWidth := oDlg:cargo["width"];  nImgHeight := oDlg:cargo["height"]; nKoef := oDlg:cargo["koef"]
         nPosX := hwg_Loword( lParam )
         nPosY := hwg_Hiword( lParam )
         IF lDown .AND. (lScrollV .OR. lScrollH) .AND. ( Abs(nPosX-nOldX) > 5 .OR. Abs(nPosY-nOldY) > 5 )
            IF lScrollV
               IF ( nOffsetV := ( nOffsetV + nPosY - nOldY ) ) > 0
                  nOffsetV := 0
               ELSEIF nOffsetV < - ( nImgHeight * nKoef - oBoard:nHeight )
                  nOffsetV := - ( nImgHeight * nKoef - oBoard:nHeight )
               ENDIF
            ENDIF
            IF lScrollH
               IF ( nOffsetH := ( nOffsetH + nPosX - nOldX ) ) > 0
                  nOffsetH := 0
               ELSEIF nOffsetH < - ( nImgWidth * nKoef - oBoard:nWidth )
                  nOffsetH := - ( nImgWidth * nKoef - oBoard:nWidth )
               ENDIF
            ENDIF
            nOldY := nPosY
            nOldX := nPosX
            oDlg:cargo["offsetv"] := nOffsetV
            oDlg:cargo["offseth"] := nOffsetH
            hwg_Invalidaterect( oBoard:handle, 0 )
         ENDIF
      ENDIF

   ELSEIF msg == WM_LBUTTONDOWN
      nPosX := hwg_Loword( lParam )
      nPosY := hwg_Hiword( lParam )
      lDown := .T.
      nOldX := nPosX
      nOldY := nPosY

   ELSEIF msg == WM_LBUTTONUP
      lDown := .F.

   ELSEIF msg == WM_ERASEBKGND
      RETURN 1

   ENDIF

   Return -1

STATIC FUNCTION Vscroll( oDlg, nScrollCode )

   LOCAL oBoard := oDlg:oBoard, stepV, nStepV

   IF !oDlg:cargo["scrollv"]
      RETURN Nil
   ENDIF

   stepV := Round( ( oDlg:cargo["height"] * oDlg:cargo["koef"] - oBoard:nHeight ) / SCROLLVRANGE, 0 )
   nStepV := Int( Abs( oDlg:cargo["offsetv"] / stepV) )
   IF nScrollCode == SB_LINEDOWN
      IF nStepV < SCROLLVRANGE
         nStepV ++
         oDlg:cargo["offsetv"] := - nStepV * stepV
      ENDIF
   ELSEIF nScrollCode == SB_LINEUP
      IF nStepV > 0
         nStepV --
         oDlg:cargo["offsetv"] := - nStepV * stepV
      ENDIF
   ENDIF
   hwg_Invalidaterect( oBoard:handle, 0 )

   RETURN Nil

STATIC FUNCTION Hscroll( oDlg, nScrollCode )

   LOCAL oBoard := oDlg:oBoard, stepH, nStepH

   IF !oDlg:cargo["scrollh"]
      RETURN Nil
   ENDIF

   stepH := Round( (oDlg:cargo["width"] * oDlg:cargo["koef"] - oBoard:nWidth) / SCROLLVRANGE, 0 )
   nStepH := Int( Abs( oDlg:cargo["offseth"] / stepH ) )
   IF nScrollCode == SB_LINEDOWN
      IF nStepH < SCROLLHRANGE
         nStepH ++
         oDlg:cargo["offseth"] := - nStepH * stepH
      ENDIF
   ELSEIF nScrollCode == SB_LINEUP
      IF nStepH > 0
         nStepH --
         oDlg:cargo["offseth"] := - nStepH * stepH
      ENDIF
   ENDIF
   hwg_Invalidaterect( oBoard:handle, 0 )

   RETURN Nil

STATIC FUNCTION Zoom( oDlg, nOp )

   LOCAL oBoard := oDlg:oBoard, nWidth, nHeight, arrSize
   LOCAL nKoef, nOffsetH := 0, nOffsetV := 0
   LOCAL nImgWidth := oDlg:cargo["width"], nImgHeight := oDlg:cargo["height"]

   IF oDlg:cargo["h"] == Nil
      RETURN Nil
   ENDIF

   nKoef := Round( oDlg:cargo["koef"], 1 )
   IF nOp < 0 .AND. nKoef > 0.11
      nKoef -= 0.1
   ELSEIF nOp > 0
      nKoef += 0.1
   ELSEIF nOp == 0
      nKoef := 1
   ENDIF

   oDlg:cargo["koef"] := nKoef
   nOffsetH := nOffsetV := 0

   arrSize := SetSize( Round( nImgWidth * nKoef, 0 ), Round( nImgHeight * nKoef, 0 ), .T. )
   nWidth := arrSize[1]
   nHeight := arrSize[2]
   oDlg:cargo["scrollh"] := ( nImgWidth * nKoef - nWidth > 3 )
   oDlg:cargo["scrollv"] := ( nImgHeight * nKoef - nHeight > 3 )

   oDlg:SetTitle( oDlg:cargo["imgname"] + "  (" + Str(nKoef*100,3,0) + "%)" )
   hwg_Invalidaterect( oBoard:handle, 0 )
   IF lUnix .AND. ( oBoard:nWidth > nWidth .OR. oBoard:nHeight > nHeight )
      oBoard:Move( ,, nWidth-4, nHeight-4 )
   ENDIF
   oDlg:Move( ,, nWidth, nHeight )
   IF oDlg:nLeft + nWidth > nScreenW
      oDlg:Center()
   ENDIF

   RETURN Nil

STATIC FUNCTION FNewFile( oDlg, cFileName )

   LOCAL handle, aBmpSize, arrSize, nWidth, nHeight, nKoef
   LOCAL oBoard := oDlg:oBoard

   IF Empty( handle := hwg_Openimage( cFileName ) )
      IF hb_isFunction( cFunc )
         handle := Eval( &( "{|s|" + cFunc + "(s)}" ), cFileName )
      ENDIF
   ENDIF
   IF Empty( handle )
      hwg_MsgStop( "Can't display " + cFileName )
      RETURN Nil
   ENDIF

   hwg_Deleteobject( oDlg:cargo["h"] )
   aBmpSize  := hwg_Getbitmapsize( handle )
   arrSize := SetSize( aBmpSize[1], aBmpSize[2] )
   nWidth := arrSize[1]
   nHeight := arrSize[2]
   FDlgInit( oDlg, cFileName, handle, arrSize[3] )

   IF lUnix .AND. ( oBoard:nWidth > nWidth .OR. oBoard:nHeight > nHeight )
      oBoard:Move( ,, nWidth-4, nHeight-4 )
   ENDIF
   oDlg:Move( ,, nWidth, nHeight )
   hwg_Invalidaterect( oBoard:handle, 0 )

   RETURN Nil

STATIC FUNCTION FNext( oDlg, lNext )

   LOCAL aDir, i, cTmp, cExtImg := ".bmp.jpg.gif.png.tif", nCurr

   IF Valtype( oDlg:cargo["dir"] ) == "C"
      aDir := hb_vfDirectory( hb_fnameDir( oDlg:cargo["dir"] ) + "*.*" )
      cTmp := hb_fnameNameExt( oDlg:cargo["dir"] )
      oDlg:cargo["dir"] := aDir := ASort( aDir,,, {|z,y|Lower(z[1]) < Lower(y[1])} )
      nCurr := Ascan( aDir, {|a|a[1] == cTmp} )
   ELSE
      aDir := oDlg:cargo["dir"]
      nCurr := oDlg:cargo["currpos"]
   ENDIF

   IF lNext
      FOR i := nCurr + 1 TO Len( aDir )
         IF Lower( hb_fnameExt( aDir[i,1] ) ) $ cExtImg
            FNewFile( oDlg, aDir[i,1] )
            nCurr := i
            EXIT
         ENDIF
      NEXT
   ELSE
      FOR i := nCurr - 1 TO 1 STEP -1
         IF Lower( hb_fnameExt( aDir[i,1] ) ) $ cExtImg
            FNewFile( oDlg, aDir[i,1] )
            nCurr := i
            EXIT
         ENDIF
      NEXT
   ENDIF
   oDlg:cargo["currpos"] := nCurr

   RETURN Nil

STATIC FUNCTION SetSize( nImgWidth, nImgHeight, l )

   LOCAL nWidth, nHeight, nWidthMax := nScreenW - nHorz, nHeightMax := nScreenH - nVert, nKoef

   IF l == Nil; l := .F.; ENDIF

   IF nImgWidth <= nWidthMax .AND. nImgHeight <= nHeightMax
      nWidth := nImgWidth
      nHeight := nImgHeight
      nKoef := 1
   ELSE
      IF nImgWidth > nWidthMax
         nWidth := nWidthMax
         IF l
            nHeight := Iif( nImgHeight > nHeightMax, nHeightMax, nImgHeight )
         ELSE
            nHeight := Int( nImgHeight * (nWidthMax/nImgWidth) )
            IF nHeight > nHeightMax
               nWidth := Int( nWidth * ( nHeightMax/nHeight ) )
               nHeight := nHeightMax
            ENDIF
         ENDIF
      ELSE
         nHeight := nHeightMax
         IF l
            nWidth := Iif( nImgWidth > nWidthMax, nWidthMax, nImgWidth )
         ELSE
            nWidth := Int( nImgWidth * (nHeightMax/nImgHeight) )
            IF nWidth > nWidthMax
               nHeight := Int( nHeight * ( nWidthMax/nWidth) )
               nWidth := nWidthMax
            ENDIF
         ENDIF
      ENDIF
      nKoef := nWidth / nImgWidth
   ENDIF

   nWidth += nHorz
   nHeight += nVert

   RETURN { nWidth, nHeight, nKoef }