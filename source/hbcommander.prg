/*
 * File manager, based on Harbour File IO API
 *
 * Copyright 2016-2023 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "fileio.ch"
#include "setcurs.ch"

#ifdef __PLATFORM__UNIX
   #define  LASTROW  23
#else
   #define  LASTROW  24
#endif

#include "hbgtinfo.ch"

#define SHIFT_PRESSED 0x010000
#define CTRL_PRESSED  0x020000
#define ALT_PRESSED   0x040000
#define KP_PRESSED    0x080000

#define HB_ZIP_OPEN_ADDINZIP   2

#define ADIR_POS      6
#define AZF_POS       6

REQUEST HB_CODEPAGE_RU866
REQUEST HB_CODEPAGE_RU1251
REQUEST HB_CODEPAGE_RUKOI8
REQUEST HB_CODEPAGE_FR850
REQUEST HB_CODEPAGE_FRWIN
REQUEST HB_CODEPAGE_FRISO
REQUEST HB_CODEPAGE_UTF8

REQUEST NETIO_PROCEXISTS, NETIO_PROCEXEC, NETIO_FUNCEXEC

STATIC oHbc
STATIC lGuiVer := .F.
STATIC cNetPort := "2941"
STATIC cFontName
STATIC nFontHeight, nFontWidth
STATIC nScreenH := 25, nScreenW := 80
STATIC cFileOut, cOutBuff
STATIC oPaneCurr, oPaneTo
STATIC lCase_Sea := .F., lRegex_Sea := .F.
#ifdef __PLATFORM__UNIX
STATIC aExtExe := { ".sh" }
#else
STATIC aExtExe := { ".exe", ".com", ".bat" }
#endif
STATIC aExtZip := { ".zip", ".rar", ".7z", ".lha", ".arj", ".gz" }

MEMVAR GETLIST

#ifdef _HBC_MAIN
FUNCTION Main( cDir1, cDir2 )
   LOCAL aPanes, arr

   SET CURSOR OFF
   SET SCORE OFF
   SET DATE FORMAT "dd.mm.yy"

   //SetBlink( .F. )
   aPanes := ReadIni( hb_DirBase() + "hbc.ini" )

#ifdef GTWVT
   ANNOUNCE HB_GTSYS
   REQUEST HB_GT_WVT
   REQUEST HB_GT_WVT_DEFAULT
   lGuiVer := .T.
#endif

#ifdef GTHWG
   REQUEST HB_GT_HWGUI
   REQUEST HB_GT_HWGUI_DEFAULT

   gthwg_CreateMainWindow( "HbCommander" )
   lGuiVer := .T.
#endif

   IF Empty( nScreenH )
      nScreenH := 25
   ENDIF
   IF Empty( nScreenW )
      nScreenW := 80
   ENDIF
   IF nScreenH != 25 .OR. nScreenW != 80
      IF !SetMode( nScreenH, nScreenW )
         nScreenH := 25
         nScreenW := 80
      ENDIF
   ENDIF

   IF Empty( cFontName )
      hb_gtinfo( HB_GTI_FONTNAME, "Lusida console" )
   ELSE
      hb_gtinfo( HB_GTI_FONTNAME, cFontName )
   ENDIF
   IF Empty( nFontWidth )
      hb_gtinfo( HB_GTI_FONTWIDTH, Int( ( hb_gtinfo( HB_GTI_DESKTOPWIDTH ) / nScreenH ) ) )
   ELSE
      hb_gtinfo( HB_GTI_FONTWIDTH, nFontWidth )
   ENDIF
   IF Empty( nFontHeight )
      hb_gtinfo( HB_GTI_FONTSIZE, Int( ( ( hb_gtinfo( HB_GTI_DESKTOPHEIGHT ) - 64 ) / nScreenW ) ) )
   ELSE
      hb_gtinfo( HB_GTI_FONTSIZE, nFontHeight )
   ENDIF
   hb_gtinfo( HB_GTI_CLOSABLE, .F. )
   arr := hb_gtinfo( HB_GTI_PALETTE )
   arr[2] := 0x800000
   arr[4] := 0x808000
   hb_gtinfo( HB_GTI_PALETTE, arr )

   SetPanes( aPanes, cDir1, cDir2 )
   Hbc()

#ifdef GTHWG
   gthwg_CloseWindow()
#endif

   RETURN Nil
#endif

FUNCTION Hbc( oEdit )

   LOCAL aPanes
   LOCAL i, cName := "$HbCommander"
   LOCAL bTextOut := {||
      RETURN Nil
   }

   IF ( i := Ascan( TEdit():aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN TEdit():aWindows[i]
   ENDIF

   IF !FilePane():lReadIni
#if defined (GTHWG) || defined (GTWVT)
      lGuiVer := .T.
#endif

      IF !Empty( oEdit )
         FilePane():vy2 := TEdit():aRectFull[3]; nScreenH := FilePane():vy2 + 1
         FilePane():vx2 := TEdit():aRectFull[4]; nScreenW := FilePane():vx2 + 1
      ENDIF

      oHbc := mnu_NewBuf( oEdit )
      oHbc:cFileName := cName
      oHbc:lTopPane := .F.
      oHbc:bOnKey := {|o,n| _Hbc_OnKey(o,n) }
      oHbc:bStartEdit := {|| _Hbc_Start() }
      oHbc:bEndEdit := {|| hb_gtinfo( HB_GTI_WINTITLE, "HbEdit" ) }
      oHbc:bTextOut := bTextOut
      oHbc:lIns := Nil

      aPanes := ReadIni( hb_DirBase() + "hbc.ini" )
      edi_SetPalette( oHbc, "default" )
      hb_cdpSelect( oHbc:cp := FilePane():cp )
      SetPanes( aPanes )
      cFileOut := hb_DirTemp() + "hbc_cons.out"
      //TEdit():cLauncher := "Panel"
   ENDIF

   /*
   DO WHILE oPaneCurr:Activate() != 0
      oPaneCurr:DrawCell( ,.F. )
      oPaneCurr := Iif( oPaneCurr == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   ENDDO
   */

   RETURN Nil

STATIC FUNCTION _Hbc_Start()

   hb_gtinfo( HB_GTI_WINTITLE, "HbCommander" )
   SetCursor( SC_NONE )
   DirChange( oPaneCurr:cCurrPath )
   FilePane():RedrawAll()

   RETURN Nil

STATIC FUNCTION _Hbc_OnKey( oEdit_Hbc, nKeyExt )

   LOCAL nKey, cPath, nPos, lRedraw, cExt, cExtFull, cTemp, o, nRow, nCol, i, aDir
   LOCAL bufsc

   nKey := hb_keyStd( nKeyExt )

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE
      RETURN -1
   ENDIF

   IF oPaneCurr:nCurrent == 0 .AND. !( nKey == K_ALT_D .OR. nKey == K_CTRL_TAB .OR. nKey == K_ALT_TAB .OR. ;
      nKey == K_F1 .OR. nKey == K_F9 .OR. nKey == K_TAB .OR. nKey == K_CTRL_PGUP .OR. nKey == K_F10 )
      RETURN -1
   ENDIF

   aDir := Iif( Empty(oPaneCurr:aDir), {}, oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift] )
   IF nKey == K_F9
     IF !oPaneCurr:PaneMenu()
        CmdHisSave()
        mnu_Exit( oEdit_Hbc )
     ENDIF

   ELSEIF nKey == K_F10
      CmdHisSave()
      mnu_Exit( oEdit_Hbc )

   ELSEIF nKey == K_F5
      IF Empty( oPaneCurr:aSelected )
         hbc_FCopyFile()
      ELSE
         hbc_FCopySele()
      ENDIF

   ELSEIF nKey == K_F6
      hbc_FRename()

   ELSEIF nKey == K_F7
      hbc_FMakeDir()

   ELSEIF nKey == K_F8
      IF Empty( oPaneCurr:aSelected )
         hbc_FDelete()
      ELSE
         hbc_FDeleteSele()
      ENDIF

   ELSEIF nKey == K_F11
      Plugins( oPaneCurr )

   ELSEIF nKey == K_F12
      mnu_Buffers( oHbc, {oPaneCurr:y1+1,oPaneCurr:x1+1} )

   ELSEIF nKey == K_F1
      mnu_Help( oHbc, edi_FindPath( "hbc.help" ) )
      edi_SetPalette( oHbc, oHbc:cPalette )
      hb_CdpSelect( FilePane():cp )
      FilePane():RedrawAll()

   ELSEIF nKey == K_F2
      oPaneCurr:ContextMenu()

   ELSEIF nKey == K_DOWN .OR. ( nKey == K_MWBACKWARD .AND. ;
         (nRow := MRow()) > oPaneCurr:y1 .AND. nRow < oPaneCurr:y2 .AND. ;
         (nCol := MCol()) > oPaneCurr:x1 .AND. nCol < oPaneCurr:x2 )
     oPaneCurr:DrawCell( ,.F. )
     IF ++ oPaneCurr:nCurrent > oPaneCurr:nCells
        IF oPaneCurr:nCurrent + oPaneCurr:nShift <= Len( oPaneCurr:aDir )
           oPaneCurr:nShift ++
           oPaneCurr:Draw()
           oPaneCurr:DrawHead( .T. )
        ENDIF
        oPaneCurr:nCurrent --
     ELSEIF oPaneCurr:nCurrent > Len( oPaneCurr:aDir )
        oPaneCurr:nCurrent --
     ENDIF
     oPaneCurr:DrawCell( ,.T. )

   ELSEIF nKey == K_UP .OR. ( nKey == K_MWFORWARD .AND. ;
         (nRow := MRow()) > oPaneCurr:y1 .AND. nRow < oPaneCurr:y2 .AND. ;
         (nCol := MCol()) > oPaneCurr:x1 .AND. nCol < oPaneCurr:x2 )
     oPaneCurr:DrawCell( ,.F. )
     IF -- oPaneCurr:nCurrent < 1
        oPaneCurr:nCurrent ++
        IF oPaneCurr:nShift > 0
           oPaneCurr:nShift --
           oPaneCurr:Draw()
           oPaneCurr:DrawHead( .T. )
        ENDIF
     ENDIF
     oPaneCurr:DrawCell( ,.T. )

   ELSEIF nKey == K_HOME  // K_CTRL_A
     IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
        SetFileAttrs( oPaneCurr )
     ELSE
        oPaneCurr:DrawCell( ,.F. )
        oPaneCurr:nCurrent := 1
        lRedraw := ( oPaneCurr:nShift > 0 )
        oPaneCurr:nShift := 0
        IF lRedraw
           oPaneCurr:Draw()
           oPaneCurr:DrawHead( .T. )
        ENDIF
        oPaneCurr:DrawCell( ,.T. )
     ENDIF
   ELSEIF nKey == K_END
     oPaneCurr:DrawCell( ,.F. )
     IF Len( oPaneCurr:aDir ) > oPaneCurr:nCells
        oPaneCurr:nCurrent := oPaneCurr:nCells
        oPaneCurr:nShift := Len( oPaneCurr:aDir ) - oPaneCurr:nCells
        oPaneCurr:Draw()
        oPaneCurr:DrawHead( .T. )
     ELSE
        oPaneCurr:nCurrent := Len( oPaneCurr:aDir )
     ENDIF
     oPaneCurr:DrawCell( ,.T. )

   ELSEIF nKey == K_LEFT
     IF oPaneCurr:nCurrent > oPaneCurr:nRows
         oPaneCurr:DrawCell( ,.F. )
         oPaneCurr:nCurrent -= oPaneCurr:nRows
         oPaneCurr:DrawCell( ,.T. )
      ENDIF

   ELSEIF nKey == K_RIGHT
      IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
         RETURN -1
      ELSE
         IF oPaneCurr:nCurrent + oPaneCurr:nRows < oPaneCurr:nCells .AND. ;
            oPaneCurr:nCurrent + oPaneCurr:nRows < Len( oPaneCurr:aDir )
            oPaneCurr:DrawCell( ,.F. )
            oPaneCurr:nCurrent += oPaneCurr:nRows
            oPaneCurr:DrawCell( ,.T. )
         ENDIF
      ENDIF

   ELSEIF nKey == K_PGDN
      oPaneCurr:DrawCell( ,.F. )
      lRedraw := .F.
      IF oPaneCurr:nCurrent + oPaneCurr:nShift + oPaneCurr:nCells - 1 <= Len( oPaneCurr:aDir )
         oPaneCurr:nShift += ( oPaneCurr:nCells - 1 )
         lRedraw := .T.
      ELSE
         oPaneCurr:nCurrent := Len( oPaneCurr:aDir ) - oPaneCurr:nShift
         IF oPaneCurr:nCurrent > oPaneCurr:nCells
            oPaneCurr:nShift := oPaneCurr:nCurrent - oPaneCurr:nCells
            oPaneCurr:nCurrent := oPaneCurr:nCells
            lRedraw := .T.
         ENDIF
      ENDIF
      IF lRedraw
         oPaneCurr:Draw()
         oPaneCurr:DrawHead( .T. )
      ENDIF
      oPaneCurr:DrawCell( ,.T. )

   ELSEIF nKey == K_PGUP
      oPaneCurr:DrawCell( ,.F. )
      lRedraw := .F.
      IF oPaneCurr:nShift == 0
         oPaneCurr:nCurrent := 1
      ELSEIF oPaneCurr:nCurrent + oPaneCurr:nShift - oPaneCurr:nCells + 1 >= 1
         oPaneCurr:nShift -= ( oPaneCurr:nCells -1 )
         IF oPaneCurr:nShift < 0
            oPaneCurr:nCurrent := -oPaneCurr:nShift
            oPaneCurr:nShift := 0
         ENDIF
         lRedraw := .T.
      ELSE
         oPaneCurr:nCurrent := 1
         oPaneCurr:nShift := 0
         lRedraw := .T.
      ENDIF
     IF lRedraw
        oPaneCurr:Draw()
        oPaneCurr:DrawHead( .T. )
     ENDIF
     oPaneCurr:DrawCell( ,.T. )

   ELSEIF nKey == K_TAB
      oPaneCurr:DrawCell( ,.F. )
      oPaneCurr:DrawHead( .F. )
      oPaneTo := oPaneCurr
      oPaneCurr := Iif( oPaneCurr == FilePane():aPanes[1], FilePane():aPanes[2], ;
         FilePane():aPanes[1] )
      oPaneCurr:DrawCell( ,.T. )
      oPaneCurr:DrawHead( .T. )
      DirChange( oPaneCurr:cCurrPath )
      RETURN -1

   ELSEIF nKey == K_ENTER .OR. nKey == K_CTRL_PGUP .OR. (nKey == K_LDBLCLK .AND. MRow()>0)
      IF nKey == K_CTRL_PGUP
         oPaneCurr:DrawCell( ,.F. )
         oPaneCurr:nCurrent := 1
         oPaneCurr:nShift := 0
         aDir := Iif( Empty(oPaneCurr:aDir), {}, oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift] )
         IF !Empty( oPaneCurr:aDir ) .AND. !( oPaneCurr:aDir[1,1] == ".." )
            oPaneCurr:Draw()
            oPaneCurr:DrawCell( ,.T. )
            oPaneCurr:DrawHead( .T. )
            RETURN -1
         ENDIF
      ENDIF
      IF Empty( oPaneCurr:aDir ) .OR. 'D' $ aDir[5]
         IF Empty( oPaneCurr:aDir ) .OR. aDir[1] == ".."
            IF oPaneCurr:nPanelMod > 0
               IF oPaneCurr:nPanelMod == 1 .OR. Empty( cTemp := aDir[6] )
                  oPaneCurr:cIOpref := oPaneCurr:cIOpref_bak
                  oPaneCurr:net_cAddress := oPaneCurr:net_cAddress_bak
                  IF oPaneCurr:nPanelMod == 2
                     hb_unzipClose( oPaneCurr:hUnzip )
                     oPaneCurr:hUnzip := Nil
                  ENDIF
                  oPaneCurr:nPanelMod := 0
                  oPaneCurr:Refresh()
               ELSE
                  IF ( nPos := hb_Rat( '/', cTemp,, Len(cTemp)-1 ) ) == 0
                     cTemp := ""
                  ELSE
                     cTemp := Left( cTemp,nPos )
                  ENDIF
                  zipDirRefresh( oPaneCurr, cTemp )
                  //edi_Alert( ltrim(str(len(oPaneCurr:adir))) + " " + ltrim(str(len(cTemp))) + ":" + cTemp )
               ENDIF
               oPaneCurr:nCurrent := 1
               oPaneCurr:Draw()
               oPaneCurr:DrawCell( ,.T.)
               oPaneCurr:DrawHead( .T. )
               RETURN -1
            ELSE
               cPath := oPaneCurr:cCurrPath
               nPos := Len( cPath ) - Iif( Right(cPath,1) $ "\/", 1, 0 )
               DO WHILE nPos > 0 .AND. !( Substr(cPath,nPos,1) $ "\/" )
                  nPos --
               ENDDO
               IF nPos > 0
                  cTemp := Substr( cPath, nPos+1, Len(cPath)-nPos-1 )
                  cPath := Left( cPath, nPos )
               ELSE
                  IF Empty( oPaneCurr:aDir )
                     oPaneCurr:nCurrent := 0
                  ENDIF
                  oPaneCurr:Draw()
                  oPaneCurr:DrawCell( ,.T. )
                  oPaneCurr:DrawHead( .T. )
                  RETURN -1
               ENDIF
            ENDIF
         ELSEIF oPaneCurr:nPanelMod == 2
            zipDirRefresh( oPaneCurr, oPaneCurr:aZipFull[aDir[ADIR_POS],1] )
         ELSE
            cPath := oPaneCurr:cCurrPath + Iif(Right(oPaneCurr:cCurrPath,1) $ "\/", "", hb_ps() ) + oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1] + hb_ps()
         ENDIF
         oPaneCurr:nCurrent := Iif( Empty( oPaneCurr:aDir ), 0, 1 )
         oPaneCurr:nShift := 0
         IF oPaneCurr:nPanelMod < 2
            oPaneCurr:SetDir( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + cPath )
            oPaneCurr:nCurrent := Iif( Empty( oPaneCurr:aDir ), 0, 1 )
            IF !Empty( cTemp ) .AND. ( nPos := Ascan( oPaneCurr:aDir, {|a|a[1]==cTemp} ) ) > 0
               IF nPos <= oPaneCurr:nCells
                  oPaneCurr:nCurrent := nPos
               ELSE
                  oPaneCurr:nShift := nPos - 1
               ENDIF
            ENDIF
         ENDIF
         oPaneCurr:Draw()
         oPaneCurr:DrawCell( ,.T. )
         oPaneCurr:DrawHead( .T. )
      ELSE
         IF Empty( oPaneCurr:cIOpref )
            cExt := Lower( hb_fnameExt( aDir[1] ) )
            cExtFull := Lower( Substr( GetFullExt( aDir[1] ), 2 ) )
            IF ( nPos := Ascan( oPaneCurr:aExtEnter, {|a|a[1] == cExtFull .or. '/'+cExtFull+'/' $ a[1]} ) ) == 0
               cTemp := Substr( cExt,2 )
               nPos := Ascan( oPaneCurr:aExtEnter, {|a|a[1] == cTemp .or. '/'+cTemp+'/' $ a[1]} )
            ENDIF
            cTemp := oPaneCurr:cCurrPath + aDir[1]
            IF ' ' $ cTemp
               cTemp := '"' + cTemp + '"'
            ENDIF
            IF nPos > 0
               cedi_RunApp( oPaneCurr:aExtEnter[nPos,2] + " " + cTemp )
#ifdef __PLATFORM__WINDOWS
            ELSEIF cExt == ".bat"
               hbc_Console( cTemp )
            ELSEIF cExt == ".exe"
              cedi_RunApp( cTemp )
#endif
#ifdef __PLATFORM__UNIX
            ELSEIF cExt == ".sh"
               hbc_Console( cTemp )
#endif

            ELSEIF Ascan( aExtZip, {|s| s==cExt} ) > 0 .AND. hbc_FReadArh()
            ELSE
#ifdef __PLATFORM__UNIX
#ifdef GTHWG
               hwg_shellExecute( "file://" + cTemp )
#endif
#else
#ifdef GTHWG
               hwg_shellExecute( cTemp )
#else
               cedi_shellExecute( cTemp )
#endif
#endif
            ENDIF
         ENDIF
      ENDIF

   ELSEIF nKey == K_F3 .OR. nKey == K_CTRL_F3
      IF 'D' $ aDir[5]
         RETURN -1
      ENDIF
      nPos := 0
      cTemp := hb_DirTemp() + "hbc_view.tmp"
      IF nKey == K_CTRL_F3
         IF Empty( oPaneCurr:cIOpref )
            cExt := Lower( Substr( hb_fnameExt( aDir[1] ),2 ) )
            cExtFull := Lower( Substr( GetFullExt( aDir[1] ), 2 ) )
            IF ( nPos := Ascan( oPaneCurr:aExtView, {|a|a[1] == cExtFull .or. '/'+cExtFull+'/' $ a[1]} ) ) > 0
               cedi_RunApp( oPaneCurr:aExtView[nPos,2] + " " + oPaneCurr:cCurrPath + aDir[1] )
            ELSEIF ( nPos := Ascan( oPaneCurr:aExtView, {|a|a[1] == cExt .or. '/'+cExt+'/' $ a[1]} ) ) > 0
               cedi_RunApp( oPaneCurr:aExtView[nPos,2] + " " + oPaneCurr:cCurrPath + aDir[1] )
            ELSEIF ( nPos := Ascan( oPaneCurr:aExtView, {|a|a[1] == "..."} ) ) > 0
               cedi_RunApp( oPaneCurr:aExtView[nPos,2] + " " + oPaneCurr:cCurrPath + aDir[1] )
            ENDIF
         ENDIF
      ENDIF
      IF nPos == 0
         IF oPaneCurr:nPanelMod == 2
            IF hb_unzipFileGoto( oPaneCurr:hUnzip, oPaneCurr:aZipFull[aDir[ADIR_POS],AZF_POS] ) == 0 ;
               .AND. hb_unzipExtractCurrentFile( oPaneCurr:hUnzip, cTemp ) == 0
               FileView( cTemp, oPaneCurr:vx1, oPaneCurr:vy1, oPaneCurr:vx2, oPaneCurr:vy2 )
               FErase( cTemp )
            ENDIF
         ELSE
            FileView( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
               oPaneCurr:cCurrPath + aDir[1], oPaneCurr:vx1, oPaneCurr:vy1, oPaneCurr:vx2, oPaneCurr:vy2 )
         ENDIF
#ifdef __PLATFORM__UNIX
         oPaneCurr:RedrawAll()
#endif
      ENDIF

   ELSEIF nKey == K_F4 .OR. nKey == K_CTRL_F4
      IF 'D' $ aDir[5]
         RETURN 0
      ENDIF
      nPos := 0
      IF nKey == K_CTRL_F4
         IF Empty( oPaneCurr:cIOpref )
            cTemp := Lower( Substr( GetFullExt( aDir[1] ), 2 ) )
            IF ( nPos := Ascan( oPaneCurr:aExtEdit, {|a|a[1] == cTemp .or. '/'+cTemp+'/' $ a[1]} ) ) > 0
               cedi_RunApp( oPaneCurr:aExtEdit[nPos,2] + " " + oPaneCurr:cCurrPath + aDir[1] )
            ENDIF
         ENDIF
      ENDIF
      IF nPos == 0
         cTemp := oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
            oPaneCurr:cCurrPath + aDir[1]
         IF Empty( oPaneCurr:cIOpref )
            mnu_NewBuf( oHbc, cTemp )
         ELSEIF oPaneCurr:cIOpref == "net:"
            mnu_NewBuf( oHbc, cTemp, hb_vfLoad(cTemp), @vfWrit_Net() )
         ELSEIF oPaneCurr:cIOpref == "zip:"
            i := hb_unzipFileGoto( oPaneCurr:hUnzip, oPaneCurr:aZipFull[aDir[ADIR_POS],AZF_POS] )
            IF i == 0 .AND. hb_unzipFileOpen( oPaneCurr:hUnzip ) == 0
               cTemp := Space( aDir[2] )
               hb_unzipFileRead( oPaneCurr:hUnzip, @cTemp )
               hb_unzipFileClose( oPaneCurr:hUnzip )
               mnu_NewBuf( oHbc, "zip|"+oPaneCurr:net_cAddress+"|"+aDir[1], cTemp )
            ELSE
               edi_Alert( "Something goes wrong..." )
            ENDIF
         ENDIF
      ENDIF

   ELSEIF nKey == K_SH_F4
      RETURN 0

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_ALT_TAB
      RETURN 0

   ELSEIF nKey == K_LBUTTONDOWN .OR. nKey == K_RBUTTONDOWN
      nRow := MRow(); nCol := MCol()
      o := Iif( nCol <= FilePane():aPanes[1]:x2, FilePane():aPanes[1], FilePane():aPanes[2] )
      IF nRow == 0
         IF nKey == K_LBUTTONDOWN
            IF nCol >= FilePane():vx2 - 5
               mnu_Buffers( oHbc, {oPaneCurr:y1+1,oPaneCurr:x1+1} )
               RETURN -1
            ELSE
               IF !o:PaneMenu()
                  CmdHisSave()
                  mnu_Exit( oEdit_Hbc )
               ENDIF
            ENDIF
         ELSE
            oPaneCurr:ContextMenu()
            RETURN -1
         ENDIF
      ENDIF
      IF oPaneCurr:nCurrent > 0 .AND. nRow > 0 .AND. nRow <= o:nRows
         oPaneCurr:DrawCell( ,.F. )
         IF o:nDispMode == 1 .OR. (o:nDispMode == 2 .AND. nCol <= o:nWidth)
            o:nCurrent := nRow
         ELSE
            o:nCurrent := nRow + o:nRows
         ENDIF
         IF o:nCurrent + o:nShift > Len( o:aDir )
            o:nCurrent := Len( o:aDir ) - o:nShift
         ENDIF
         IF nKey == K_RBUTTONDOWN
            IF 'D' $ o:aDir[o:nCurrent + o:nShift,5]
               RETURN -1
            ENDIF
            IF ( nPos := Ascan( o:aSelected, o:nCurrent+o:nShift ) ) == 0
               Aadd( o:aSelected, o:nCurrent+o:nShift )
            ELSE
               o:aSelected := hb_ADel( o:aSelected, nPos, .T. )
            ENDIF
         ENDIF
         IF oPaneCurr == o
            oPaneCurr:DrawCell( ,.T. )
         ENDIF
      ENDIF
      IF !( oPaneCurr == o )
         oPaneCurr:DrawCell( ,.F. )
         oPaneCurr:DrawHead( .F. )
         oPaneTo := oPaneCurr
         oPaneCurr := o
         oPaneCurr:DrawCell( ,.T. )
         oPaneCurr:DrawHead( .T. )
         DirChange( oPaneCurr:cCurrPath )
         //RETURN 1
      ENDIF

   ELSEIF nKey == K_INS
      IF 'D' $ aDir[5]
         RETURN -1
      ENDIF
      IF ( nPos := Ascan( oPaneCurr:aSelected, oPaneCurr:nCurrent+oPaneCurr:nShift ) ) == 0
         Aadd( oPaneCurr:aSelected, oPaneCurr:nCurrent+oPaneCurr:nShift )
      ELSE
         oPaneCurr:aSelected := hb_ADel( oPaneCurr:aSelected, nPos, .T. )
      ENDIF
      KEYBOARD Chr( K_DOWN )

   ELSEIF nKey == K_CTRL_O
      hbc_Console()

/*
   ELSEIF nKey == K_ALT_1
      oPaneCurr:ChangeMode( 1 )
   ELSEIF nKey == K_ALT_2
      oPaneCurr:ChangeMode( 2 )
*/
   ELSEIF nKey == K_ALT_D
      oPaneCurr:ChangeDir()
   ELSEIF nKey == K_ALT_F1
      IF !Empty( cTemp := hbc_SelePath( FilePane():vy1-1, FilePane():aPanes[1]:x1-1 ) )
         FilePane():aPanes[1]:ChangeDir( cTemp )
      ENDIF
   ELSEIF nKey == K_ALT_F2
      IF !Empty( cTemp := hbc_SelePath( FilePane():vy1-1, FilePane():aPanes[2]:x1-1 ) )
         FilePane():aPanes[2]:ChangeDir( cTemp )
      ENDIF
   ELSEIF nKey == K_ALT_F7
      hbc_Search()
   ELSEIF nKey == K_ALT_F12
      AppList( oPaneCurr )
   ELSEIF nKey == K_SH_F1
      hbc_Zip()
   ELSEIF nKey == K_SH_F2
      cExt := Lower( hb_fnameExt( aDir[1] )   )
      IF Ascan( aExtZip, {|s| s==cExt} ) > 0
         hbc_Unzip()
      ENDIF
   ELSEIF nKey == 43 //.AND. hb_BitAnd( nKeyExt, KP_PRESSED ) != 0
      hbc_Search( .T. )
   ELSEIF nKey == 61 //.OR. (nKey >= 65 .AND. nKey <= 90) .OR. (nKey >= 97 .AND. nKey <= 122)
      KEYBOARD Chr(nKey)
      hbc_Console()
   ELSE
      IF !Empty( FilePane():aDefPaths )
         FOR i := 1 TO Len( FilePane():aDefPaths )
            IF FilePane():aDefPaths[i,3] == nKeyExt
               oPaneCurr:ChangeDir( FilePane():aDefPaths[i,1] )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN -1

STATIC FUNCTION ReadIni( cIniName )

   LOCAL hIni := edi_iniRead( cIniName ), aSect, arr, i, cTmp, s, nPos
   LOCAL aPanes := { Nil, Nil }, cp, lPalette := .F.

   FilePane():lReadIni := .T.
   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
#ifdef _HBC_MAIN
      IF hb_hHaskey( hIni, cTmp := "SCREEN" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         IF hb_hHaskey( aSect, "fontname" ) .AND. !Empty( cTmp := aSect[ "fontname" ] )
            cFontName := cTmp
         ENDIF
         IF hb_hHaskey( aSect, "fontheight" ) .AND. !Empty( cTmp := aSect[ "fontheight" ] )
            nFontheight := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, "fontwidth" ) .AND. !Empty( cTmp := aSect[ "fontwidth" ] )
            nFontWidth := Val(cTmp)
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "screen_height" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            //IF Empty( nScreenH )
               nScreenH := Val(cTmp)
            //ENDIF
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "screen_width" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            //IF Empty( nScreenW )
               nScreenW := Val(cTmp)
            //ENDIF
         ENDIF
      ENDIF
#endif
      IF hb_hHaskey( hIni, cTmp := "OPTIONS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         IF hb_hHaskey( aSect, cTmp := "cp" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cp := cTmp
         ENDIF
         IF hb_hHaskey( aSect, cTemp := "palette" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
            edi_SetPalette( oHbc, cTemp )
            lPalette := .T.
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "context_menu_plugin" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            FilePane():xContextMenu := cTmp
         ENDIF
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "COLORS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FOR i := 1 TO Len( arr )
            IF !Empty( cTemp := aSect[ arr[i] ] )
               IF ( n := Ascan( aHiliOpt, arr[i] ) ) > 0
                  FilePane():aHiliAttrs[n] := cTemp
               ELSEIF arr[i] == "colorbox"
                  FilePane():cClrBox := cTemp
               ELSEIF arr[i] == "colordir"
                  FilePane():cClrDir := cTemp
               ELSEIF arr[i] == "colorfile"
                  FilePane():cClrFil := cTemp
               ELSEIF arr[i] == "colorfileexe"
                  FilePane():cClrExe := cTemp
               ELSEIF arr[i] == "colorfilezip"
                  FilePane():cClrZip := cTemp
               ELSEIF arr[i] == "colorfilehidden"
                  FilePane():cClrHid := cTemp
               ELSEIF arr[i] == "colorcurr"
                  FilePane():cClrCurr := cTemp
               ELSEIF arr[i] == "colorsel"
                  FilePane():cClrSel := cTemp
               ELSEIF arr[i] == "colorselcurr"
                  FilePane():cClrSelCurr := cTemp
               ELSEIF arr[i] == "colormenuf"
                  FilePane():aClrMenu[1] := cTemp
               ELSEIF arr[i] == "colormenub"
                  FilePane():aClrMenu[2] := cTemp
               ENDIF
            ENDIF
         NEXT
      ENDIF

      IF hb_hHaskey( hIni, cTmp := "PANE1" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         aPanes[1] := { Nil, Nil }
         hb_hCaseMatch( aSect, .F. )
         IF hb_hHaskey( aSect, "path" ) .AND. !Empty( cTmp := aSect[ "path" ] )
            aPanes[1,1] := cTmp
         ENDIF
         IF hb_hHaskey( aSect, "mode" ) .AND. !Empty( cTmp := aSect[ "mode" ] )
            aPanes[1,2] := Val(cTmp)
         ENDIF
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "PANE2" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         aPanes[2] := { Nil, Nil }
         hb_hCaseMatch( aSect, .F. )
         IF hb_hHaskey( aSect, "path" ) .AND. !Empty( cTmp := aSect[ "path" ] )
            aPanes[2,1] := cTmp
         ENDIF
         IF hb_hHaskey( aSect, "mode" ) .AND. !Empty( cTmp := aSect[ "mode" ] )
            aPanes[2,2] := Val(cTmp)
         ENDIF
      ENDIF

      IF hb_hHaskey( hIni, cTmp := "PATHS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FilePane():aDefPaths := Array( Len( arr ) )
         FOR i := 1 TO Len( arr )
            IF !Empty( cTmp := aSect[ arr[i] ] )
               IF ( nPos := At( ',', cTmp ) ) > 0
                  s := Substr( cTmp, nPos+1 )
                  cTmp := Left( cTmp, nPos-1 )
               ELSE
                  s := ""
               ENDIF
               FilePane():aDefPaths[i] := { cTmp, s, Iif( !Empty(s), edi_KeyCToN(s), Nil ) }
            ENDIF
         NEXT
      ENDIF

      IF hb_hHaskey( hIni, cTmp := "EXTVIEW" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FilePane():aExtView := Array( Len( arr ) )
         FOR i := 1 TO Len( arr )
            FilePane():aExtView[i] := { Lower( arr[i] ), aSect[ arr[i] ] }
         NEXT
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "EXTEDIT" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FilePane():aExtEdit := Array( Len( arr ) )
         FOR i := 1 TO Len( arr )
            FilePane():aExtEdit[i] := { Lower( arr[i] ), aSect[ arr[i] ] }
         NEXT
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "EXTENTER" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FilePane():aExtEnter := Array( Len( arr ) )
         FOR i := 1 TO Len( arr )
            FilePane():aExtEnter[i] := { Lower( arr[i] ), aSect[ arr[i] ] }
         NEXT
      ENDIF

      IF hb_hHaskey( hIni, cTmp := "PLUGINS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FOR i := 1 TO Len( arr )
            s := aSect[ arr[i] ]
            IF ( n := At( ",", s ) ) > 0
               cTemp := AllTrim( Left( s,n-1 ) )
               IF !Empty( edi_FindPath( "plugins" + hb_ps() + cTemp ) )
                  s := Substr( s, n+1 )
                  IF ( n := At( ",", s ) ) > 0
                     Aadd( FilePane():aPlugins, { cTemp, Substr( s, n+1 ), AllTrim(Left( s,n-1 )), Nil, Nil } )
                  ENDIF
               ENDIF
            ENDIF
         NEXT
      ENDIF

      IF hb_hHaskey( hIni, cTmp := "APPLIST" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         arr := hb_hKeys( aSect )
         FilePane():aAppList := Array( Len( arr ) )
         FOR i := 1 TO Len( arr )
            s := aSect[ arr[i] ]
            IF ( n := At( ",", s ) ) > 0
               cTemp := AllTrim( Left( s,n-1 ) )
               FilePane():aAppList[i] := { cTemp, Substr( s, n+1 ) }
            ENDIF
         NEXT
      ENDIF
   ENDIF

   IF Empty(cp) //.OR. Ascan( TEdit():aCpages, cp ) == 0
      cp := "RU866"
   ENDIF
   hb_cdpSelect( FilePane():cp := cp )
   IF Lower( cp ) == "utf8"
      FilePane():lUtf8 := .T.
   ENDIF
   IF !lPalette
      edi_SetPalette( oHbc, "default" )
   ENDIF

   RETURN aPanes

STATIC FUNCTION SetPanes( aPanes, cDir1, cDir2 )

   LOCAL cPrefix, nMode1, nMode2

#ifdef __PLATFORM__UNIX
   cPrefix := '/'
#else
   cPrefix := hb_curDrive() + ':\'
#endif

   IF Empty( cDir1 )
      IF !Empty( aPanes[1] ) .AND. !Empty( aPanes[1,1] )
         cDir1 := aPanes[1,1]
         IF !Empty( aPanes[1,2] )
            nMode1 := aPanes[1,2]
         ENDIF
      ELSE
         cDir1 := cPrefix + CurDir()
      ENDIF
   ENDIF
   IF Empty( cDir2 )
      IF !Empty( aPanes[2] ) .AND. !Empty( aPanes[2,1] )
         cDir2 := aPanes[2,1]
         IF !Empty( aPanes[2,2] )
            nMode2 := aPanes[2,2]
         ENDIF
      ELSE
         cDir2 := cPrefix + CurDir()
      ENDIF
   ENDIF

   FilePane():New( 0, 0, Int(nScreenW/2)-1, nScreenH-1, nMode1, cDir1 )
   FilePane():New( Int(nScreenW/2), 0, nScreenW-1, nScreenH-1, nMode2, cDir2 )
   oPaneCurr := FilePane():aPanes[1]
   oPaneTo   := FilePane():aPanes[2]

   RETURN Nil

CLASS FilePane

   CLASS VAR aPanes SHARED INIT {}
   CLASS VAR aDefPaths SHARED
   CLASS VAR aExtView, aExtEdit, aExtEnter SHARED
   CLASS VAR aCmdHis   SHARED
   CLASS VAR lCmdHis   SHARED INIT .F.
   CLASS VAR vx1 SHARED  INIT 0
   CLASS VAR vy1 SHARED  INIT 0
   CLASS VAR vx2 SHARED  INIT nScreenW-1
   CLASS VAR vy2 SHARED  INIT nScreenH-1
   CLASS VAR cClrBox  SHARED   INIT "+W/B"
   CLASS VAR cClrDir  SHARED   INIT "+W/B"
   CLASS VAR cClrFil  SHARED   INIT "BG+/B"
   CLASS VAR cClrCurr SHARED   INIT "N/BG"
   CLASS VAR cClrSel  SHARED   INIT "+GR/B"
   CLASS VAR cClrSelCurr  SHARED   INIT "+GR/BG"
   CLASS VAR aClrMenu SHARED   INIT { "W/B","N/BG" }
   CLASS VAR cClrExe  SHARED   INIT "+G/B"
   CLASS VAR cClrZip  SHARED   INIT "+RB/B"
   CLASS VAR cClrHid  SHARED   INIT "W/B"
   CLASS VAR lUtf8    SHARED   INIT .F.
   CLASS VAR cp       SHARED
   CLASS VAR aPlugins SHARED   INIT {}
   CLASS VAR aAppList SHARED   INIT {}
   CLASS VAR lReadIni SHARED   INIT .F.
   CLASS VAR cConsOut SHARED   INIT ""
   CLASS VAR nConsMax SHARED   INIT 20000
   CLASS VAR xContextMenu SHARED

   DATA cIOpref       INIT ""
   DATA net_cAddress  INIT ""
   DATA net_cPort     INIT ""
   DATA cIOpref_bak   INIT ""
   DATA net_cAddress_bak INIT ""
   DATA zip_cCurrDir
   DATA x1, y1, x2, y2
   DATA lViewStatus   INIT .T.

   DATA nDispMode     INIT 1
   DATA nShift        INIT 0
   DATA nCells, nRows, nWidth
   DATA nPanelMod     INIT 0
   DATA hUnzip

   DATA cCurrPath

   DATA nCurrent     INIT 1

   DATA cPath
   DATA aDir
   DATA aZipFull
   DATA aSelected     INIT {}

   METHOD New( x1, y1, x2, y2, nMode, cPath )
   METHOD ChangeMode( nMode )
   METHOD ChangeDir()
   METHOD ParsePath( cPath )
   METHOD SetDir( cPath )
   METHOD Refresh()
   METHOD Draw()
   METHOD DrawCell( nCell, lSel )
   METHOD DrawHead( lSel )
   METHOD PaneMenu()
   METHOD ContextMenu()
   METHOD RedrawAll()

ENDCLASS

METHOD New( x1, y1, x2, y2, nMode, cPath ) CLASS FilePane

   Aadd( ::aPanes, Self )

   ::x1 := x1
   ::y1 := y1
   ::x2 := x2
   ::y2 := y2
   IF !Empty( nMode )
      ::nDispMode := nMode
   ENDIF

   ::SetDir( cPath )
   ::nCurrent := Iif( Empty( ::aDir ), 0, 1 )
   //::Draw()
   //::DrawHead( .F. )

   RETURN Self

METHOD ChangeMode( nMode ) CLASS FilePane

   IF ::nDispMode != nMode
      ::nDispMode := nMode
      ::Draw()
      ::DrawCell( ,.T. )
   ENDIF

   RETURN Nil

METHOD ChangeDir( cNewPath ) CLASS FilePane

   LOCAL cBuf
   LOCAL aGets := { { ::y1+7,::x1+10, 0, "", 26 } }
   LOCAL nRes

   IF Empty( cNewPath )
      cNewPath := Space( 200 )
      cBuf := Savescreen( ::y1 + 6, ::x1 + 8, ::y1 + 8, ::x1 + 36 )
      Set COLOR TO +GR/B,N/BG
      @ ::y1 + 6, ::x1 + 8, ::y1 + 8, ::x1 + 36 BOX "ÚÄ¿³ÙÄÀ³ "
      @ ::y1 + 6, ::x1 + 12 SAY " Set new path:"
      KEYBOARD Chr(K_ENTER)

      DO WHILE .T.
         IF ( nRes := edi_READ( aGets, hb_Hash( 0x440003ee, Chr(K_ENTER)) ) ) > 0
            cNewPath := aGets[1,4]
         ENDIF
         IF LastKey() == 13 .AND. Empty( cNewPath )
            IF !Empty( cNewPath := hbc_SelePath( ::y1 + 7, ::x1 + 10 ) )
               EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
      Restscreen( ::y1 + 6, ::x1 + 8, ::y1 + 8, ::x1 + 36, cBuf )
      IF LastKey() == 27
         cNewPath := ""
      ENDIF
   ENDIF

   IF !Empty( cNewPath )
      ::SetDir( Trim(cNewPath) )
      ::nCurrent := Iif( Empty( ::aDir ), 0, 1 )
      ::nShift := 0
      ::Draw()
      IF Self == oPaneCurr
         ::DrawCell( ,.T. )
         ::DrawHead( .T. )
      ELSE
         ::DrawHead( .F. )
      ENDIF
   ENDIF

   RETURN Nil

METHOD ParsePath( cPath ) CLASS FilePane

   LOCAL nPos1, nPos2, cCurrPath, cPref, cAddr, cPort, cPass, c, l, i

   IF Lower( Left( cPath,4 ) ) == "net:"
      cPref := "net:"
      IF ( nPos1 := hb_At( '<', cPath, 5 ) ) > 0 .AND. ;
         ( nPos2 := hb_At( '>', cPath, nPos1+1 ) ) > 0
         cPass := Substr( cPath, nPos1+1, nPos2-nPos1-1 )
         cPath := Left( cPath, nPos1-1 ) + Substr( cPath, nPos2+1 )
      ENDIF
      IF ( nPos1 := hb_At( ':', cPath, 5 ) ) > 0
         cAddr := Substr( cPath, 5, nPos1-5 )
         IF ( nPos2 := hb_At( ':', cPath, nPos1+1 ) ) > 0
            cPort := Substr( cPath, nPos1+1, nPos2-nPos1-1 )
            cCurrPath := Substr( cPath, nPos2 + 1 )
         ELSE
            cPort := cNetPort
            cCurrPath := Substr( cPath, nPos1 + 1 )
         ENDIF
      ELSEIF ( nPos1 := hb_At( '\', cPath, 5 ) ) > 0 .OR. ( nPos1 := hb_At( '/', cPath, 5 ) ) > 0
         cAddr := Substr( cPath, 5, nPos1-5 )
         cPort := cNetPort
         cCurrPath := Substr( cPath, nPos1 )
      ELSE
         nPos1 := 4
         nPos2 := 0
         l := .T.
         DO WHILE ++nPos1 <= Len(cPath)
            IF ( c := Substr( cPath, nPos1, 1 ) ) == '.'
               nPos2 ++
            ELSEIF !isDigit( c )
               l := .F.
               EXIT
            ENDIF
         ENDDO
         IF l .AND. nPos2 == 3
            cAddr := Substr( cPath, 5 )
            cPort := cNetPort
            cCurrPath := ""
         ELSE
            ::cCurrPath := Substr( cPath, 5 )
            RETURN .T.
         ENDIF
      ENDIF
      cAddr += ':'
      cPort += ':'
      l := .F.
      FOR i := 1 TO Len( FilePane():aPanes )
         IF ( FilePane():aPanes[i]:net_cAddress == cAddr ) .AND. ( FilePane():aPanes[i]:net_cPort == cPort )
            l := .T.
            EXIT
         ENDIF
      NEXT
      IF !l
         IF Empty( cPass )
            cPass := edi_MsgGet( "Password", ::y1+5, ::x1+10, ::x1+30 )
         ENDIF
         IF netio_Connect( Left(cAddr,Len(cAddr)-1), Left(cPort,Len(cPort)-1), 2000, cPass )
            l := .T.
         ELSE
            RETURN .F.
         ENDIF
      ENDIF
      IF l
         ::cIOpref := cPref
         ::net_cAddress := cAddr
         ::net_cPort := cPort
         ::cCurrPath := cCurrPath
         ::cIOpref_bak := ::net_cAddress_bak := ""
      ENDIF
   ELSE
      IF !Empty( ::cIOpref )
         cPref := ::cIOpref
         cAddr := ::net_cAddress
         cPort := ::net_cPort
         ::cIOpref := ::net_cAddress := ::net_cPort := ::cIOpref_bak := ::net_cAddress_bak := ""
         l := .F.
         FOR i := 1 TO Len( FilePane():aPanes )
            IF ( FilePane():aPanes[i]:cIOpref == cPref ) .AND. ( FilePane():aPanes[i]:net_cAddress == cAddr ) .AND. ( FilePane():aPanes[i]:net_cPort == cPort )
               l := .T.
               EXIT
            ENDIF
         NEXT
         IF !l
            netio_DisConnect( Left(cAddr,Len(cAddr)-1), Left(cPort,Len(cPort)-1) )
         ENDIF
      ENDIF
      ::cCurrPath := cPath
   ENDIF
   IF !( Right( ::cCurrPath,1 ) $ "\/" )
      ::cCurrPath += hb_ps()
   ENDIF

   RETURN .T.

METHOD SetDir( cPath ) CLASS FilePane

   ::aSelected := {}
   IF Empty( cPath )
      ::aDir := {}
      RETURN Nil
   ENDIF

   IF ::nPanelMod == 2
      hb_unzipClose( ::hUnzip )
      ::hUnzip := Nil
   ENDIF
   ::nPanelMod := 0

   ::ParsePath( cPath )
   ::Refresh()
   IF Empty( ::cIOpref )
      DirChange( ::cCurrPath )
   ENDIF

   RETURN Nil

METHOD Refresh() CLASS FilePane

   LOCAL aDirTmp, i, l1 := .F., l2 := .F., nPos
   LOCAL cPath := ::cCurrPath

   IF ::nPanelMod > 0
      RETURN .F.
   ENDIF
   aDirTmp := hb_vfDirectory( ::cIOpref + ::net_cAddress + ::net_cPort + cPath, "HSD" )
   IF Empty( aDirTmp )
      ::aDir := {}
      RETURN .F.
   ENDIF

   FOR i := 1 TO Len( aDirTmp )
      IF Empty( aDirTmp[i] )
         LOOP
      ELSEIF aDirTmp[i,1] == "."
         ADel( aDirTmp, i )
         i --
         l1 := .T.
      ELSEIF "D" $ aDirTmp[i,5]
         IF aDirTmp[i,1] == ".."
            aDirTmp[i,1] := " .."
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
         aDirTmp[1] := { "  ..",0,Date(),"","D" }
      ENDIF
   ENDIF
   aDirTmp := ASort( aDirTmp,,, {|z,y|Lower(z[1]) < Lower(y[1])} )
   FOR i := 1 TO Len( aDirTmp )
      IF "D" $ aDirTmp[i,5]
         IF aDirTmp[i,1] == "  .."
            aDirTmp[i,1] := ".."
         ELSEIF Left( aDirTmp[i,1],1 ) == " "
            aDirTmp[i,1] := Substr( aDirTmp[i,1],2 )
         ENDIF
      ENDIF
   NEXT

   ::aDir := aDirTmp

   RETURN .T.

METHOD Draw() CLASS FilePane

   LOCAL i, cTemp

   SetColor( ::cClrBox )
   @ ::y1, ::x1, ::y2, ::x2 BOX "ÚÄ¿³ÙÄÀ³ "

   ::nRows := ::y2 - ::y1 - Iif( ::lViewStatus, 3, 1 )
   IF ::nDispMode == 1
      ::nCells := ::nRows
      ::nWidth := ::x2 - ::x1 - 1
   ELSEIF ::nDispMode == 2
      ::nCells := ::nRows * 2
      ::nWidth := Int( (::x2 - ::x1) / 2 ) - 1
      @ ::y1 + 1, ::x1 + ::nWidth + 1 TO ::y1 + ::nRows, ::x1 + ::nWidth + 1
   ENDIF
   IF ::lViewStatus
      @ ::y2 - 2, ::x1 + 1 TO ::y2 - 2, ::x2 - 1
   ENDIF

   FOR i := 1 TO ::nCells
      IF i + ::nShift > Len( ::aDir )
         EXIT
      ENDIF
      ::DrawCell( i, .F. )
   NEXT
   IF Len(FilePane():aPanes) > 1 .AND. Self == FilePane():aPanes[2] .AND. !Empty( TEdit():aWindows )
      cTemp := Ltrim( Str(Len(TEdit():aWindows)) )
      @ ::y1, ::x2-Len(cTemp)-2 SAY "[" + cTemp + "]" COLOR ::cClrSelCurr
   ENDIF

   RETURN Nil

METHOD DrawCell( nCell, lCurr ) CLASS FilePane

   LOCAL arr, nRow, x1 := ::x1 + 1, cText, nWidth, cDop, lSel
   LOCAL cDate, dDate, cSize, cClrFil := ::cClrFil, cExt

   IF ::nCurrent == 0
      @ ::y2 - 1, ::x1 + 1 SAY "Not available"
      RETURN Nil
   ENDIF

   nRow := nCell := Iif( nCell==Nil,::nCurrent,nCell )
   arr := ::aDir[nCell+::nShift]
   lSel := ( Ascan( ::aSelected, nCell+::nShift ) > 0 )

   IF ::nDispMode == 2 .AND. nRow > ::nRows
      x1 += ( ::nWidth+1 )
      IF ( nRow := (nRow % ::nRows) ) == 0
         nRow := ::nRows
      ENDIF
   ENDIF
   cText := Trim( arr[1] )
   cExt := hb_fnameExt( cText )
   IF 'H' $ arr[5]
      cClrFil := ::cClrHid
   ELSEIF 'D' $ arr[5]
      cClrFil := ::cClrDir
   ELSE
      IF Ascan( aExtExe, {|s| s==cExt} ) > 0
         cClrFil := ::cClrExe
      ELSEIF Ascan( aExtZip, {|s| s==cExt} ) > 0
         cClrFil := ::cClrZip
      ENDIF
   ENDIF
   SetColor( Iif( lCurr, Iif( lSel, ::cClrSelCurr, ::cClrCurr ), ;
      Iif( lSel, ::cClrSel, cClrFil ) ) )
   IF Len( cText ) > ::nWidth
      cText := Left( cText, ::nWidth-1 ) + '>'
   ENDIF
   @ ::y1 + nRow, x1 SAY PAdr( cText, ::nWidth )

   SetColor( ::cClrFil )
   IF ::lViewStatus .AND. lCurr
      IF Empty( arr[3] )
         IF !hb_fGetDateTime( ::cCurrPath + arr[1], @dDate )
            arr[3] := Stod( "19171107" )
            arr[2] := -1
         ELSE
            arr[3] := dDate
            arr[2] := hb_fSize( ::cCurrPath + arr[1] )
         ENDIF
      ENDIF
      cDop := Iif( 'D' $ arr[5], "<dir>", Ltrim(Str(arr[2])) ) + " " + hb_Dtoc(arr[3]) + " " + Left(arr[4],5)
      nWidth := ::x2 - ::x1 - 3 - Len(cDop)
      cText := NameShortcut( Trim( ::aDir[nCell+::nShift,1] ), nWidth, "~" )
      @ ::y2 - 1, ::x1 + 1 SAY cText
      @ ::y2 - 1, ::x1 + 1 + Len(cText) SAY Space( ::x2 - ::x1 - 1 - Len(cText) )
      @ ::y2 - 1, ::x2 - Len(cDop) SAY cDop
      x1 := Int( (::x2-::x1-15)/2 )
      IF Empty( ::aSelected )
         @ ::y2 - 2, x1, ::y2 - 2, x1 + 15 BOX "ÚÄ¿³ÙÄÀ³ " COLOR ::cClrBox
      ELSE
         @ ::y2 - 2, x1 SAY "Selected: " + PAdl(Ltrim(Str(Len(::aSelected))),4) COLOR ::cClrSel
      ENDIF
   ENDIF

   RETURN Nil

METHOD DrawHead( lCurr ) CLASS FilePane

   LOCAL cPath := ::cCurrPath

   cPath := ::cIOpref + ::net_cAddress + cPath
   SetColor( Iif( lCurr, ::cClrCurr, ::cClrFil ) )
   IF ::nPanelMod == 0
      @ ::y1, ::x1 + Int((::x2-::x1-1)/2) - Int( Len(cPath)/2 ) SAY NameShortcut( cPath, ::x2-::x1-3 )
   ELSEIF ::nPanelMod == 1
      cPath := "Search results"
      @ ::y1, ::x1 + Int((::x2-::x1-1)/2) - Int( Len(cPath)/2 ) SAY cPath
   ELSEIF ::nPanelMod == 2
      cPath := ::net_cAddress + ":" + ::zip_cCurrDir
      @ ::y1, ::x1 + Int((::x2-::x1-1)/2) - Int( Len(cPath)/2 ) SAY NameShortcut( cPath, ::x2-::x1-3 )
   ENDIF

   RETURN Nil

METHOD PaneMenu() CLASS FilePane

   LOCAL cBuf, nChoic := 1, cTemp, bufsc, o
   LOCAL cSep := "---"
   LOCAL aMenu := { {"Pane mode",,}, {"Change dir",,,"Alt-D"}, {"File edit history",,}, {"Find file",,,"Alt-F7"}, ;
      {"Plugins",,,"F11"}, {"Apps",,,"Alt-F12"}, {"Buffers",,,"F12"}, ;
      {cSep,,}, {"Edit hbc.ini",,}, {cSep,,}, {"Exit",,} }
   LOCAL aMenu1 := { "Mode 1 " + Iif(::nDispMode==1,"x"," "), ;
      "Mode 2 " + Iif(::nDispMode==2,"x"," ") }

   IF !Empty( FilePane():cConsOut )
      aMenu := hb_AIns( aMenu, Len(aMenu)-3, {"Stdout window",,}, .T. )
   ENDIF
   nChoic := FMenu( oHbc, aMenu, ::y1+1, ::x1+1, ::y1+Len(aMenu)+2, ::x1+25, ::aClrMenu[1], ::aClrMenu[2] )
   IF nChoic == 1
      nChoic := FMenu( oHbc, aMenu1, ::y1+2, ::x1+14, ::y1+Len(aMenu1)+3, ::x1+38, ::aClrMenu[1], ::aClrMenu[2] )
      IF nChoic == 1
         ::ChangeMode( 1 )
      ELSEIF nChoic == 2
         ::ChangeMode( 2 )
      ENDIF

   ELSEIF nChoic == 2
      /*
      IF !Empty( cTemp := hbc_SelePath( ::y1-1, ::x1-1 ) )
         ::ChangeDir( cTemp )
      ENDIF
      */
      ::ChangeDir()
   ELSEIF nChoic == 3
      hbc_Dirlist()
   ELSEIF nChoic == 4
      hbc_Search()
   ELSEIF nChoic == 5
      Plugins( Self )
   ELSEIF nChoic == 6
      AppList( Self )
   ELSEIF nChoic == 7
      mnu_Buffers( oHbc, {oPaneCurr:y1+1,oPaneCurr:x1+1} )
   ELSEIF !Empty( FilePane():cConsOut ) .AND. nChoic == Len( aMenu ) - 4
      ShowStdout()
   ELSEIF nChoic == Len( aMenu ) - 2
      mnu_NewBuf( oHbc, hb_DirBase() + "hbc.ini" )

   ELSEIF nChoic == Len( aMenu )
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD ContextMenu() CLASS FilePane

   LOCAL aMenu := {}, nChoic, xPlugin, cFullPath, bMenu

   IF !( 'D' $ oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,5] )
      Aadd( aMenu, { "Copy",,1,"F5" } )
      Aadd( aMenu, { "Rename",,2,"F6" } )
      Aadd( aMenu, { "Delete",,3,"F8" } )
   ENDIF

   IF !Empty( xPlugin := FilePane():xContextMenu )
      IF Valtype( xPlugin ) == "C" .AND. ;
         !Empty( cFullPath := edi_FindPath( "plugins" + hb_ps() + xPlugin ) )
         xPlugin := FilePane():xContextMenu := { cFullPath, hb_hrbLoad( cFullPath ) }
      ENDIF
      IF Valtype( xPlugin ) == "A" .AND. !Empty( xPlugin[2] )
         bMenu := hb_hrbDo( xPlugin[2], aMenu, Self, hb_fnameDir( xPlugin[1] ) )
      ENDIF
   ENDIF

   IF Len( aMenu ) == 0
      RETURN Nil
   ENDIF

   nChoic := FMenu( oHbc, aMenu, ::y1+3, ::x1+10,,, ::aClrMenu[1], ::aClrMenu[2] )
   IF nChoic == 0
      RETURN Nil
   ELSEIF aMenu[nChoic,3] == 1
      IF Empty( oPaneCurr:aSelected )
         hbc_FCopyFile()
      ELSE
         hbc_FCopySele()
      ENDIF
   ELSEIF aMenu[nChoic,3] == 2
      hbc_FRename()
   ELSEIF aMenu[nChoic,3] == 3
      IF Empty( oPaneCurr:aSelected )
         hbc_FDelete()
      ELSE
         hbc_FDeleteSele()
      ENDIF
   ELSEIF !Empty( bMenu )
      Eval( bMenu, aMenu[nChoic,3] )
   ENDIF

   RETURN Nil

METHOD RedrawAll() CLASS FilePane

   ::aPanes[1]:Draw()
   ::aPanes[2]:Draw()
   oPaneCurr:DrawCell( ,.T. )
   ::aPanes[1]:DrawHead( ::aPanes[1] == oPaneCurr )
   ::aPanes[2]:DrawHead( ::aPanes[2] == oPaneCurr )

   RETURN Nil

STATIC FUNCTION FAsk_Overwrite( n, cFile )

   LOCAL nRes
   LOCAL y1 := 6, x1 := Int( (FilePane():vx2-FilePane():vx1-50)/2 ), y2 := y1+5, x2 := x1+50
   LOCAL cBuf, oldc := SetColor( TEdit():cColorWR + "," + TEdit():cColorWR )
   LOCAL aGets := { ;
      {y1+1,x1+2, 11, cFile + " exists already! Owerwrite it?"}, ;
      {y1+2,x1+3, 1, .F., 1, TEdit():cColorWR,TEdit():cColorWB }, {y1+2,x1+2, 11, "[ ] Don's ask anymore"}, ;
      {y1+4,x1+16, 2, "[Yes]", 5,TEdit():cColorWR,TEdit():cColorWB,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+4,x1+28, 2,"[No]", 4,TEdit():cColorWR,TEdit():cColorWB,{||__KeyBoard(Chr(K_ESC))}} }
   STATIC lNoAsk := .F., lRes := .F.

   IF n == 0  // One time
      aGets[2,3] := aGets[3,3] := -1
   ELSEIF n == 1  // First time
      lNoAsk := .F.
   ELSEIF lNoAsk
      RETURN lRes
   ENDIF

   cBuf := Savescreen( y1, x1, y2, x2 )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+3, x1 SAY "Ã"
   @ y1+3, x2 SAY "´"
   @ y1+3, x1+1 TO y1+3, x2-1

   lRes := ( ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets) )
   lNoAsk := aGets[2,4]

   Restscreen( y1, x1, y2, x2, cBuf )
   SetColor( oldc )

   RETURN lRes

STATIC FUNCTION FCopy( aDir, cFileTo, nFirst )

   LOCAL i, cTemp, cIOpref, handle, lRes := .T., hZip

   IF hb_vfExists( cFileTo ) .AND. !FAsk_Overwrite( nFirst, hb_fnameNameExt(cFileTo) )
      RETURN .F.
   ENDIF

   IF nFirst == 0
      @ 08, 28 SAY PAdc( "Wait", 28 )
   ENDIF

   IF oPaneCurr:nPanelMod == 2
      i := hb_unzipFileGoto( oPaneCurr:hUnzip, oPaneCurr:aZipFull[aDir[ADIR_POS],AZF_POS] )
      IF i == 0
         cIOpref := Left( cFileTo, 4 )
         IF cIOpref == "net:" .AND. hb_unzipFileOpen( oPaneCurr:hUnzip ) == 0
            cTemp := Space( aDir[2] )
            hb_unzipFileRead( oPaneCurr:hUnzip, @cTemp )
            hb_unzipFileClose( oPaneCurr:hUnzip )
            handle := hb_vfOpen( cFileTo, FO_WRITE+FO_CREAT+FO_TRUNC )
            hb_vfWrite( handle, cTemp )
            hb_vfClose( handle )
         ELSEIF cIOpref != "net:" .AND. hb_unzipExtractCurrentFile( oPaneCurr:hUnzip, cFileTo ) == 0
         ELSE
            edi_Alert( "Something goes wrong..." )
            RETURN .F.
         ENDIF
      ENDIF
   ELSEIF Left( cFileTo,4 ) == "zip:"
      IF ( i := hb_At( ':', cFileTo, 5 ) ) == 0 .OR. Substr( cFileTo, 5, i-4 ) != oPaneTo:net_cAddress
         lRes := .F.
      ENDIF
      IF !Empty( hZip := hb_zipOpen( oPaneTo:cCurrPath + oPaneTo:net_cAddress, HB_ZIP_OPEN_ADDINZIP ) )
         IF hb_zipStoreFile( hZip, oPaneCurr:cCurrPath + aDir[1], Substr( cFileTo, i+1 ) ) != 0
            lRes := .F.
         ENDIF
         hb_zipClose( hZip )
      ELSE
         lRes := .F.
      ENDIF
   ELSE
      IF hb_vfCopyFile( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
            oPaneCurr:cCurrPath + aDir[1], cFileTo ) != 0
         lRes := .F.
      ENDIF
   ENDIF
   IF !lRes
      edi_Alert( "Error copying " + aDir[1] )
   ENDIF

   RETURN lRes

STATIC FUNCTION hbc_FCopyFile()

   LOCAL cFileTo, aDir := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift]
   LOCAL cScBuf, oldc, nRes, cTemp, lDir, cInitDir, nStart := 0, aWnd
   LOCAL cFileName := aDir[1]
   LOCAL aGets := { ;
      {06,12,11,"Copy " + NameShortcut( cFileName, 48 ) + " to:"}, ;
      {07,12,0,"",56,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {09,25,2,"[Copy]",6,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {09,50,2,"[Cancel]",10,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL bCopy := {|s,arr|
      LOCAL nLen := Len(oPaneCurr:cIOpref+oPaneCurr:net_cAddress+oPaneCurr:net_cPort+oPaneCurr:cCurrPath) + 1
      IF "D" $ arr[5]
         IF !( s == "." .OR. s == ".." )
            //edi_writelog( "dir:  " + cFileTo + Substr( s,nLen ) )
            IF hb_vfDirMake( cFileTo + Substr( s,nLen ) ) != 0
               RETURN .F.
            ENDIF
         ENDIF
      ELSE
         nStart ++
         //edi_writelog( "from: " + Substr( s,nLen ) )
         //edi_writelog( "to:   " + cFileTo + Substr( s,nLen ) )
         WndOut( aWnd, Substr( s,nLen ) )
         RETURN FCopy( {Substr( s,nLen )}, cFileTo + Substr( s,nLen ), nStart )
      ENDIF
      RETURN .T.
   }

   lDir := ( 'D' $ aDir[5] )

   cScBuf := Savescreen( 05, 10, 10, 70 )
   oldc := SetColor( oHbc:cColorSel+","+oHbc:cColorSel+",,"+oHbc:cColorGet+","+oHbc:cColorSel )
   hb_cdpSelect( "RU866" )
   @ 05, 10, 10, 70 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 08, 10 SAY "Ã"
   @ 08, 70 SAY "´"
   @ 08, 11 TO 08, 69
   hb_cdpSelect( oHbc:cp )

   IF oPaneTo:nPanelMod == 2
      aGets[2,4] := oPaneTo:cIOpref + oPaneTo:net_cAddress + ":" + oPaneTo:zip_cCurrDir + cFileName
   ELSE
      aGets[2,4] := oPaneTo:cIOpref + oPaneTo:net_cAddress + oPaneTo:net_cPort + oPaneTo:cCurrPath + ;
         Iif( lDir, "", cFileName )
   ENDIF

   nRes := edi_READ( aGets )
   Restscreen( 05, 10, 10, 70, cScBuf )

   IF nRes > 0 .AND. nRes < Len(aGets)
      cFileTo := AllTrim( aGets[2,4] )
      IF ( cTemp := Left( cFileTo,4 ) ) == "sea:" .OR. ( cTemp == "zip:" .AND. lDir )
         RETURN edi_Alert( "Operation isn't permitted" )
      ENDIF

      IF lDir
         IF !( Right(cFileTo,1) $ "/\" )
            cFileTo += hb_ps()
         ENDIF
         cInitDir := oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
            oPaneCurr:cCurrPath + aDir[1] + hb_ps()
         IF hb_vfDirMake( cFileTo  + aDir[1] ) != 0
            RETURN edi_Alert( "Error copying " + aDir[1] )
         ENDIF
         aWnd := WndInit( 05, 20, 12, 60 )
         DirEval( cInitDir, "*", .T., bCopy, .T. )
         WndClose( aWnd, "Done, " + Ltrim(Str(nStart)) + " files copied." )
         oPaneTo:Refresh()
         oPaneCurr:Refresh()
         oPaneCurr:RedrawAll()
      ELSE
         IF FCopy( aDir, cFileTo, 0 )
            IF Left( cFileTo,4 ) == "zip:"
               // Refresh zip panel
               hb_unzipClose( oPaneTo:hUnzip )
               oPaneTo:aZipFull := zipRead( oPaneTo:hUnzip := ;
                  hb_unzipOpen(oPaneTo:cCurrPath+oPaneTo:net_cAddress) )
               zipDirRefresh( oPaneTo, oPaneTo:zip_cCurrDir )
            ELSE
               oPaneTo:Refresh()
            ENDIF
            oPaneCurr:Refresh()
            oPaneCurr:RedrawAll()
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_FCopySele()

   LOCAL cFileName, cFileTo, i, aWnd, nSch := 0, aDir

   IF oPaneTo:nPanelMod > 0
      edi_Alert( "Operation isn't permitted" )
      RETURN Nil
   ENDIF

   IF edi_Alert( "Copy " + Ltrim(Str(Len(oPaneCurr:aSelected))) + " files?", "Yes", "No" ) == 1
      aWnd := WndInit( 05, 20, 12, 60 )

      FOR i := 1 TO Len( oPaneCurr:aSelected )
         aDir := oPaneCurr:aDir[oPaneCurr:aSelected[i]]
         cFileName := aDir[1]
         cFileTo := oPaneTo:cIOpref + oPaneTo:net_cAddress + oPaneTo:net_cPort + oPaneTo:cCurrPath + cFileName

         WndOut( aWnd, cFileName )
         IF FCopy( aDir, cFileTo, i )
            nSch ++
         ENDIF
         IF Inkey() == 27
            EXIT
         ENDIF
      NEXT

      WndClose( aWnd, "Done, " + Ltrim(Str(nSch)) + " files copied." )
      oPaneCurr:aSelected := {}
      IF nSch > 0
         oPaneTo:Refresh()
      ENDIF
      oPaneCurr:RedrawAll()
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_FRename()

   LOCAL cFileName, cBuf, cNewName

   cFileName := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
   IF 'D' $ oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,5]
      RETURN Nil
   ENDIF
   IF oPaneCurr:nPanelMod > 0
      edi_Alert( "Operation isn't permitted" )
      RETURN Nil
   ENDIF

   cBuf := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )

   Set COLOR TO N/W
   @ 05, 10, 10, 70 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 06, 12 SAY "Rename " + NameShortcut( cFileName, 46 ) + " to:"
   Set COLOR TO N/BG,B/BG
   @ 09, 24 SAY "[Enter - Ok]  [ESC - Cancel]"
   cNewName := PAdr( cFileName, 200 )
   @ 7, 12 GET cNewName PICTURE "@S56"
   READ
   IF LastKey() != 27 .AND. !Empty( cNewName )
      cNewName := Trim( cNewName )
      IF hb_vfExists( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cNewName )
         edi_Alert( "Such a file exists already!" )
         Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )
         RETURN Nil
      ENDIF
      IF hb_vfRename( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cFileName, oPaneCurr:cCurrPath + cNewName ) == 0
         Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )
         oPaneCurr:Refresh()
         oPaneCurr:Draw()
         oPaneCurr:DrawCell( ,.T.)
      ELSE
         edi_Alert( "Error renaming " + cFileName )
         Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )
      ENDIF
      RETURN Nil
   ENDIF

   Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )

   RETURN Nil

STATIC FUNCTION hbc_FDelete()

   LOCAL cFileName, lDir, lRes := .F.
   LOCAL cInitDir, aDirs, i, aWnd, nStart := 0
   LOCAL bDel := {|s,arr|
      LOCAL nLen := Len(oPaneCurr:cIOpref+oPaneCurr:net_cAddress+oPaneCurr:net_cPort+oPaneCurr:cCurrPath) + 1
      IF "D" $ arr[5]
         IF !( s == "." .OR. s == ".." )
            Aadd( aDirs, s )
         ENDIF
      ELSE
         nStart ++
         WndOut( aWnd, Substr( s,nLen ) )
         IF hb_vfErase( s ) != 0
            lRes := .F.
            edi_Alert( "Error deleting " + Substr( s,nLen ) )
            RETURN .F.
         ENDIF
      ENDIF
      RETURN .T.
   }

   IF oPaneCurr:nPanelMod > 0
      RETURN edi_Alert( "Operation isn't permitted" )
   ENDIF

   cFileName := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
   lDir := ('D' $ oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,5])

   IF edi_Alert( "Really delete " + cFileName + "?", "No", "Yes" ) == 2
      IF lDir
         lRes := .T.
         aWnd := WndInit( 05, 20, 12, 60 )
         cInitDir := oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cFileName
         aDirs := { cInitDir }
         DirEval( cInitDir, "*", .T., bDel, .T. )
         aDirs := ASort( aDirs,,, {|s1,s2|Len(s1)>Len(s2)} )
         FOR i := 1 TO Len(aDirs)
            IF hb_vfDirRemove( aDirs[i] ) != 0
               lRes := .F.
               edi_Alert( "Error deleting " + aDirs[i] )
               EXIT
            ENDIF
         NEXT
         WndClose( aWnd, "Done, " + Ltrim(Str(nStart)) + " files deleted" )
      ELSE
         IF hb_vfErase( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cFileName ) == 0
            lRes := .T.
         ENDIF
      ENDIF
      IF lRes
         oPaneCurr:Refresh()
         IF oPaneCurr:nCurrent + oPaneCurr:nShift > Len( oPaneCurr:aDir )
            IF oPaneCurr:nCurrent == 1
               oPaneCurr:nShift --
            ELSE
               oPaneCurr:nCurrent --
            ENDIF
         ENDIF
         oPaneCurr:Draw()
         oPaneCurr:DrawCell( ,.T.)
         oPaneCurr:DrawHead( .T. )
      ELSE
         edi_Alert( "Error deleting " + cFileName )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_FDeleteSele()

   LOCAL cFileName, i

   IF oPaneCurr:nPanelMod > 0
      RETURN edi_Alert( "Operation isn't permitted" )
   ENDIF

   IF edi_Alert( "Really delete " + Ltrim(Str(Len(oPaneCurr:aSelected))) + " files?", "No", "Yes" ) == 2
      FOR i := 1 TO Len( oPaneCurr:aSelected )
         cFileName := oPaneCurr:aDir[oPaneCurr:aSelected[i],1]
         IF hb_vfErase( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cFileName ) == 0
         ELSE
         ENDIF
      NEXT
      oPaneCurr:aSelected := {}
      oPaneCurr:Refresh()
      oPaneCurr:nCurrent := 1
      oPaneCurr:nShift := 0
      oPaneCurr:Draw()
      oPaneCurr:DrawCell( ,.T.)
      oPaneCurr:DrawHead( .T. )
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_FMakeDir()

   LOCAL cBuf, cNewName

   IF oPaneCurr:nPanelMod > 0
      edi_Alert( "Operation isn't permitted" )
      RETURN Nil
   ENDIF

   cBuf := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )

   Set COLOR TO N/W
   @ 05, 10, 10, 70 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 06, 12 SAY "Create new directory"
   Set COLOR TO N/BG,B/BG
   @ 09, 24 SAY "[Enter - Ok]  [ESC - Cancel]"
   cNewName := Space( 200 )
   @ 7, 12 GET cNewName PICTURE "@S56"
   READ
   IF LastKey() != 27 .AND. !Empty( cNewName )
      IF hb_vfDirMake( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + Trim(cNewName) ) == 0
         Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )
         oPaneCurr:Refresh()
         oPaneCurr:Draw()
         oPaneCurr:DrawCell( ,.T.)
      ELSE
         Alert( "Error creaing " + cNewName )
         Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )
      ENDIF
      RETURN Nil
   ENDIF

   Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )

   RETURN Nil

STATIC FUNCTION hbc_SelePath( y1, x1 )

   LOCAL i, nHeight, nWidth := 16, cRes := "", aPaths := Array( Len(FilePane():aDefPaths) )
   LOCAL nwMax := FilePane():vx2 - 1 - x1

   IF !Empty( FilePane():aDefPaths )
      nHeight := Min( Len(aPaths),20 )
      FOR i := 1 TO Len(aPaths)
         aPaths[i] := { FilePane():aDefPaths[i,1],,, FilePane():aDefPaths[i,2] }
         nWidth := Min( Max( Len(aPaths[i,1])+5, nWidth ), nwMax )
      NEXT
      i := FMenu( oHbc, aPaths, y1+2, x1+2, y1+3+nHeight, x1+3+nWidth, FilePane():aClrMenu[1], FilePane():aClrMenu[2] )
      IF i > 0
         cRes := PAdr( aPaths[i,1], 200 )
      ENDIF
   ENDIF

   RETURN cRes

STATIC FUNCTION hbc_FReadArh()

   LOCAL cFileName := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
   LOCAL cExt := Lower( hb_fnameExt( cFileName ) )

   IF 'D' $ oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,5]
      RETURN Nil
   ENDIF

   IF cExt == ".zip"
      oPaneCurr:hUnzip := hb_unzipOpen( cFileName )
      IF !Empty( oPaneCurr:hUnzip )
         oPaneCurr:aZipFull := zipRead( oPaneCurr:hUnzip )
         zipDirRefresh( oPaneCurr, "" )
         oPaneCurr:nPanelMod := 2
         oPaneCurr:cIOpref_bak := oPaneCurr:cIOpref
         oPaneCurr:cIOpref := "zip:"
         oPaneCurr:net_cAddress_bak := oPaneCurr:net_cAddress
         oPaneCurr:net_cAddress := cFileName
         oPaneCurr:nCurrent := 1
         oPaneCurr:nShift := 0
         oPaneCurr:Draw()
         oPaneCurr:DrawCell( ,.T. )
         oPaneCurr:DrawHead( .T. )
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION hbc_Dirlist()

   LOCAL i, aMenu := {}, cDir

   FOR i := 1 TO Len( TEdit():aEditHis )
      cDir := NameShortcut(hb_Translate(hb_fnameDir(TEdit():aEditHis[i,1]),"UTF8"), 48,'~' )
      IF Ascan( aMenu, {|a|a[1]==cDir} ) == 0
         AAdd( aMenu, { cDir,Nil,i} )
      ENDIF
   NEXT

   IF !Empty( aMenu )
      i := FMenu( oHbc, aMenu, oPaneCurr:y1+1, oPaneCurr:x1+1,,, FilePane():aClrMenu[1], FilePane():aClrMenu[2] )
      IF i > 0
         oPaneCurr:ChangeDir( hb_fnameDir( TEdit():aEditHis[aMenu[i,3],1] ) )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION zipRead( hUnzip )

   LOCAL nErr, aDir, cFile, nSize, dDate, cTime, lCrypted

   aDir := { { "..","","","","D" } }
   nErr := hb_unzipFileFirst( hUnzip )
   DO WHILE nErr == 0
      hb_unzipFileInfo( hUnzip, @cFile, @dDate, @cTime, , , , @nSize, @lCrypted )
      Aadd( aDir, { cFile, nSize, dDate, cTime, Iif(Right(cFile,1)=='/',"D",""), ;
         hb_unzipFilePos(hUnzip) } )
      nErr := hb_unzipFileNext( hUnzip )
   ENDDO

   RETURN aDir

STATIC FUNCTION zipDirRefresh( oPane, cDir )

   LOCAL i, aDir := { { "..","","","","D",cDir } }, aFull := oPane:aZipFull, cName

   FOR i := 1 TO Len( aFull )
      IF Right( cName := aFull[i,1],1 ) == '/'
         cName := hb_strShrink( cName,1 )
      ENDIF
      IF hb_fnameDir( cName ) == cDir .AND. !( ".." $ cName )
         AAdd( aDir, { hb_fnameNameExt(cName), aFull[i,2], aFull[i,3], aFull[i,4], ;
            Iif(Right(aFull[i,1],1)=='/',"D",""), i } )
      ENDIF
   NEXT

   oPane:zip_cCurrDir := cDir
   oPane:aDir := aDir

   RETURN Nil

STATIC FUNCTION DirEval( cInitDir, cMask, lRecur, bCode, lEvalDir )

   LOCAL i, res, nCount := 0, arlen, aFiles, nPos1 := 1, nPos2, cMsk, lDo := .T.

   IF !( Right( cInitDir,1 ) $ "/\" ); cInitDir += hb_ps(); ENDIF
   cMask := Iif( cMask==Nil, "*.*", Upper(cMask) )

   DO WHILE lDo
      IF ( nPos2 := hb_At( ";", cMask, nPos1 ) ) > 0
         cMsk := Substr( cMask, nPos1, nPos2-nPos1 )
         nPos1 := nPos2 + 1
      ELSE
         cMsk := Substr( cMask, nPos1 )
         lDo := .F.
      ENDIF
      aFiles := hb_vfDirectory( cInitDir + "*.*", "HSD" )
      arlen := Len( aFiles )
      FOR i := 1 TO arlen
         IF "D" $ aFiles[ i,5 ]
            IF "." != aFiles[ i,1 ] .AND. ".." != aFiles[ i,1 ] .AND. lRecur
               IF !Empty( lEvalDir )
                  nCount ++
                  res := Eval( bCode, cInitDir + aFiles[i,1], aFiles[i] )
                  IF ValType( res ) == "L" .AND. !res
                     Return nCount
                  ENDIF
               ENDIF
               nCount += DirEval( cInitDir + aFiles[i,1] + hb_OsPathSeparator(), cMsk, .T., bCode, lEvalDir )
            ENDIF
         ELSEIF hb_FileMatch( UPPER( aFiles[ i,1 ] ), cMsk )
            nCount ++
            IF bCode != Nil
               res := Eval( bCode, cInitDir + aFiles[i,1], aFiles[i] )
               IF ValType( res ) == "L" .AND. !res
                  Return nCount
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDDO

   RETURN nCount

STATIC FUNCTION hbc_Search( lSele )

   LOCAL cScBuf, oldc, nRes, i
   LOCAL aGets := { ;
      {09,28,0,"*.*",33,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {10,28,0,"",33,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {10,62,2,"[^]",3,oHbc:cColorSel,oHbc:cColorMenu,{||mnu_SeaHist(oHbc,aGets[2])}}, ;
      {12,18,1,.F.,1}, ;
      {12,38,1,.F.,1}, ;
      {13,18,1,.F.,1}, ;
      {13,38,1,Empty(lSele),1}, ;
      {12,58,1,!Empty(lSele),1}, ;
      {15,25,2,"[Search]",10,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {15,40,2,"[Cancel]",10,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cSearch, lCase, lWord, lRegex, lRecu, lSelect
   LOCAL cs_utf8, cCmd, cRes, aRes, aDir := { { "..","","","","D" } }, lFound := .F., n, cPath

   IF oPaneCurr:nPanelMod > 0
      RETURN edi_Alert( "Operation isn't permitted" )
   ENDIF

   cScBuf := Savescreen( 08, 15, 16, 70 )
   oldc := SetColor( oHbc:cColorSel+","+oHbc:cColorSel+",,"+oHbc:cColorGet+","+oHbc:cColorSel )
   hb_cdpSelect( "RU866" )
   @ 08, 15, 16, 70 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 14, 15 SAY "Ã"
   @ 14, 70 SAY "´"
   @ 14, 16 TO 14, 69
   hb_cdpSelect( oHbc:cp )

   @ 09, 17 SAY "File mask"
   @ 10, 17 SAY "Search for"
   @ 12, 17 SAY "[ ] Case sensitive"
   @ 12, 37 SAY "[ ] Regular expr."
   @ 13, 17 SAY "[ ] Whole word"
   @ 13, 37 SAY "[ ] Recursive"
   @ 12, 57 SAY "[ ] Select"

   IF Empty( lSele ) .AND. !Empty( TEdit():aSeaHis )
      aGets[2,4] := hb_Translate( TEdit():aSeaHis[1], "UTF8" )
      aGets[4,4] := lCase_Sea
      aGets[5,4] := lRegex_Sea
   ENDIF

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cSearch := aGets[2,4]
      lCase := aGets[4,4]
      lRegex := aGets[5,4]
      lWord := aGets[6,4]
      lRecu := aGets[7,4]
      lSelect := Iif( lRecu, .F., aGets[8,4] )

      aDir := Iif( lSelect, {}, { { "..","","","","D" } } )
      IF Empty( cSearch )
         cPath := oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath
         IF lSelect
            DirEval( cPath, aGets[1,4], lRecu, ;
               {|s|Aadd( aDir, Ascan2(oPaneCurr:aDir,hb_fnameNameExt(s)) )} )
         ELSE
            DirEval( cPath, aGets[1,4], lRecu, ;
               {|s|Aadd( aDir,{ Substr(s,Len(oPaneCurr:cCurrPath)+1),"","","","" })} )
         ENDIF
      ELSEIF oPaneCurr:cIOpref == "net:"
         edi_Alert( "Operation isn't permitted" )
      ELSE
         IF lRecu
            cCmd := 'grep ' + Iif(!lCase,'-i ','') + Iif(lWord,'-w ','') + Iif(lRegex,'-P ','') + ;
               '-R -l --include "' + aGets[1,4] + '" "' + cSearch + '"'
         ELSE
            cCmd := 'grep ' + Iif(!lCase,'-i ','') + Iif(lWord,'-w ','') + Iif(lRegex,'-P ','') + ;
               '-l ' + '"' + cSearch + '" ' + aGets[1,4]
         ENDIF
         FErase( cFileOut )
         cedi_RunConsoleApp( cCmd, cFileOut )
         IF !Empty( cRes := MemoRead( cFileOut ) )
            aRes := hb_ATokens( cRes, Iif( Chr(13) $ cRes, Chr(13)+Chr(10), Chr(10) ) )
            FOR i := 1 TO Len( aRes )
               IF !Empty( aRes[i] )
                  IF lSelect
                     IF ( n := Ascan2( oPaneCurr:aDir,aRes[i] ) ) > 0
                        Aadd( aDir, n )
                     ENDIF
                  ELSE
                     Aadd( aDir, { aRes[i],"","","","" } )
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ENDIF
      IF !lSelect .AND. Len( aDir ) > 1
         oPaneCurr:nPanelMod := 1
         oPaneCurr:aDir := aDir

         IF !Empty( cSearch )
            cs_utf8 := hb_Translate( cSearch,, "UTF8" )
            IF ( i := Ascan( TEdit():aSeaHis, {|cs|cs==cs_utf8} ) ) > 0
               ADel( TEdit():aSeaHis, i )
               hb_AIns( TEdit():aSeaHis, 1, cs_utf8, .F. )
            ELSE
               hb_AIns( TEdit():aSeaHis, 1, cs_utf8, Len(TEdit():aSeaHis)<hb_hGetDef(TEdit():options,"seahismax",10) )
            ENDIF
         ENDIF

         lFound := .T.
      ELSEIF lSelect .AND. Len( aDir ) > 0
         oPaneCurr:aSelected := aDir
         lFound := .T.
      ENDIF
      oPaneCurr:nCurrent := 1
   ENDIF

   Restscreen( 08, 15, 16, 70, cScBuf )
   SetColor( oldc )
   IF lFound
      IF !lSelect
         oPaneCurr:cIOpref_bak := oPaneCurr:cIOpref
         oPaneCurr:net_cAddress_bak := oPaneCurr:net_cAddress
         oPaneCurr:cIOpref := "sea:"
      ENDIF
      oPaneCurr:Draw()
      oPaneCurr:DrawCell( ,.T. )
      oPaneCurr:DrawHead( .T. )
   ELSEIF LastKey() != K_ESC
      edi_Alert( "Nothing found" )
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_Zip()

   LOCAL aDir := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift]
   LOCAL aGets := { ;
      {06,12,11,"Create archive"}, ;
      {07,12,0,"",56,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {09,25,2,"[Ok]",4,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {09,50,2,"[Cancel]",10,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cScBuf, oldc, nRes
   LOCAL cPath, hZip, cFile, i, aDirToZip, aWnd, nSch := 0, cFileInZip, aDirs := {}, arr, j, cTemp

   IF oPaneCurr:nPanelMod == 2 .OR. Left( oPaneCurr:cIOpref,4 ) == "net:"
      RETURN edi_Alert( "Operation isn't permitted" )
   ENDIF

   oldc := SetColor( oHbc:cColorSel+","+oHbc:cColorSel+",,"+oHbc:cColorGet+","+oHbc:cColorSel )
   cScBuf := Savescreen( 05, 10, 10, 70 )
   hb_cdpSelect( "RU866" )
   @ 05, 10, 10, 70 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 08, 10 SAY "Ã"
   @ 08, 70 SAY "´"
   @ 08, 11 TO 08, 69
   hb_cdpSelect( oHbc:cp )

   IF !Empty( oPaneCurr:aSelected )
      aGets[2,4] := hb_fnameName( hb_strShrink(oPaneCurr:cCurrPath,1) ) + ".zip"
   ELSE
      aGets[2,4] := aDir[1] + ".zip"
   ENDIF

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets) .AND. !Empty(aGets[2,4])
      hZip := hb_zipOpen( aGets[2,4] )

      Restscreen( 05, 10, 10, 70, cScBuf )
      IF !Empty( oPaneCurr:aSelected )
         aWnd := WndInit( 05, 20, 16, 60 )
         FOR i := 1 TO Len( oPaneCurr:aSelected )
            cFile := oPaneCurr:aDir[oPaneCurr:aSelected[i],1]
            WndOut( aWnd, cFile )
            IF hb_zipStoreFile( hZip, cFile, cFile ) == 0
               nSch++
            ENDIF
         NEXT
         WndClose( aWnd, "Done, " + Ltrim(Str(nSch)) + " files archived" )
         oPaneCurr:aSelected := {}
      ELSEIF 'D' $ aDir[5]
         aWnd := WndInit( 05, 20, 16, 60 )
         aDirToZip := ASort( hb_DirScan( aDir[1], "*", "HSD" ),,, {|a1,a2|a1[1]<a2[1]} )
         cFile := aDir[1] + hb_ps()
         hb_zipStoreFile( hZip, cFile, cFile )
         FOR i := 1 TO Len( aDirToZip )
            cFileInZip := cFile + aDirToZip[i,1]
            IF "D" $ aDirToZip[i,5] .AND. !( (cTemp := Right(cFileInZip,2)) == ".." ) .AND. ;
               !( cTemp == hb_ps()+"." ) .AND. !( aDirToZip[i,1] == "." )
               IF Ascan( aDirs, {|s|s==cFileInZip} ) == 0
                  Aadd( aDirs, cFileInZip )
                  cFileInZip += hb_ps()
                  hb_zipStoreFile( hZip, cFileInZip, cFileInZip )
               ENDIF
            ENDIF
         NEXT
         FOR i := 1 TO Len( aDirToZip )
            IF !( "D" $ aDirToZip[i,5] )
               cFileInZip := cFile + aDirToZip[i,1]
               WndOut( aWnd, cFileInZip )
               IF hb_zipStoreFile( hZip, cFileInZip, cFileInZip ) == 0
                  nSch++
               ENDIF
            ENDIF
         NEXT
         WndClose( aWnd, "Done, " + Ltrim(Str(nSch)) + " files archived" )
      ELSE
         hb_zipStoreFile( hZip, aDir[1], aDir[1] )
      ENDIF

      hb_zipClose( hZip )
      oPaneCurr:Refresh()
      oPaneCurr:RedrawAll()
   ELSE
      Restscreen( 05, 10, 10, 70, cScBuf )
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_Unzip()

   LOCAL cFileName := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
   LOCAL cExt := Lower( hb_fnameExt( cFileName ) )
   LOCAL nErr, hUnzip, cFile, lCrypted, aWnd, nSch := 0
   LOCAL aGets := { ;
      {06,12,11,"Extract files to"}, ;
      {07,12,0,"",56,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {09,25,2,"[Ok]",4,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {09,50,2,"[Cancel]",10,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cScBuf, oldc, nRes, cPath, nFirst := 0

   IF cExt == ".zip"

      oldc := SetColor( oHbc:cColorSel+","+oHbc:cColorSel+",,"+oHbc:cColorGet+","+oHbc:cColorSel )
      cScBuf := Savescreen( 05, 10, 10, 70 )
      hb_cdpSelect( "RU866" )
      @ 05, 10, 10, 70 BOX "ÚÄ¿³ÙÄÀ³ "
      @ 08, 10 SAY "Ã"
      @ 08, 70 SAY "´"
      @ 08, 11 TO 08, 69
      hb_cdpSelect( oHbc:cp )

      aGets[2,4] := Iif( Empty(oPaneTo:cIOpref), oPaneTo:cCurrPath, "" )

      IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)

         Restscreen( 05, 10, 10, 70, cScBuf )
         cPath := aGets[2,4]
         IF !Empty(cPath) .AND. !( Right( cPath,1 ) $ "/\" )
            cPath += hb_ps()
         ENDIF

         hUnzip := hb_unzipOpen( cFileName )
         IF ! Empty( hUnzip )
            aWnd := WndInit( 05, 20, 16, 60 )
            nErr := hb_unzipFileFirst( hUnzip )
            DO WHILE nErr == 0
               hb_unzipFileInfo( hUnzip, @cFile,,,,,,, @lCrypted )
               WndOut( aWnd, cFile )
               IF Right( cFile,1 ) $ "/\"
                  IF !hb_vfDirExists( cPath+cFile )
                     hb_vfDirMake( cPath+cFile )
                  ENDIF
               ELSE
                  IF !hb_vfExists( cPath+cFile ) .OR. FAsk_Overwrite( ++nFirst, hb_fnameNameExt(cFile) )
                     IF hb_unzipExtractCurrentFile( hUnzip, cPath+cFile ) == 0
                        nSch++
                     ENDIF
                  ENDIF
               ENDIF
               nErr := hb_unzipFileNext( hUnzip )
            ENDDO
         ENDIF
         hb_unzipClose( hUnzip )
         WndClose( aWnd, "Done, " + Ltrim(Str(nSch)) + " files unzipped" )
         IF nSch > 0
            oPaneCurr:Refresh()
            oPaneCurr:RedrawAll()
         ENDIF
      ELSE
         Restscreen( 05, 10, 10, 70, cScBuf )
      ENDIF
      SetColor( oldc )

   ENDIF


   RETURN Nil

STATIC FUNCTION GetFullExt( cName )

   LOCAL cExt := "", cTemp

   DO WHILE !Empty( cTemp := hb_fnameExt( cName ) )
      cExt := cTemp + cExt
      cName := hb_fnameName( cName )
   ENDDO

   RETURN cExt

FUNCTION vfWrit_Net( cFileName, cText )

   LOCAL handle := hb_vfOpen( cFileName, FO_WRITE+FO_CREAT+FO_TRUNC )

   hb_vfWrite( handle, cText )
   hb_vfClose( handle )

   RETURN Nil

FUNCTION vfWrit_Zip( cFileName, cText )

   edi_Alert( "Operation isn't permitted" )

   RETURN Nil

STATIC FUNCTION Plugins( oPane )

   LOCAL aMenu := {}, i

   FOR i := 1 TO Len( FilePane():aPlugins )
      IF Empty( FilePane():aPlugins[i,3] )
         AAdd( aMenu, { FilePane():aPlugins[i,2], Nil, i } )
      ENDIF
   NEXT
   IF !Empty( aMenu )
      IF ( i := FMenu( oHbc, aMenu, oPane:y1+1, oPane:x1+1,,, FilePane():aClrMenu[1], FilePane():aClrMenu[2] ) ) > 0
         i := aMenu[i,3]
         edi_RunPlugin( oPane, FilePane():aPlugins, i )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION AppList( oPane )

   LOCAL aMenu := {}, i

   FOR i := 1 TO Len( FilePane():aAppList )
      AAdd( aMenu, FilePane():aAppList[i,2] )
   NEXT
   IF !Empty( aMenu )
      IF ( i := FMenu( oHbc, aMenu, oPane:y1+1, oPane:x1+1,,, FilePane():aClrMenu[1], FilePane():aClrMenu[2] ) ) > 0
         cedi_RunApp( FilePane():aAppList[i,1] )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION SetFileAttrs( oPane )

   LOCAL x1 := oPane:x1+4, y1 := oPane:y1 + 3
   LOCAL cScBuf, oldc, cp
   LOCAL aGets := { ;
      { y1+2,x1+3, 1, .F., 1 }, ;
      { y1+3,x1+3, 1, .F., 1 }, ;
      { y1+4,x1+3, 1, .F., 1 }, ;
      { y1+5,x1+3, 1, .F., 1 }, ;
      { y1+6,x1+3, 1, .F., 1 }, ;
      { y1+7,x1+3, 1, .F., 1 }, ;
      { y1+8,x1+3, 1, .F., 1 }, ;
      { y1+9,x1+3, 1, .F., 1 }, ;
      { y1+10,x1+3, 1, .F., 1 }, ;
      { y1+11,x1+3, 1, .F., 1 }, ;
      { y1+12,x1+3, 1, .F., 1 } }

#ifdef __PLATFORM__UNIX
LOCAL aDefs := { HB_FA_SUID, HB_FA_SGID, HB_FA_SVTX, ;
   HB_FA_RUSR, HB_FA_WUSR, HB_FA_XUSR, ;
   HB_FA_RGRP, HB_FA_WGRP, HB_FA_XGRP, ;
   HB_FA_ROTH, HB_FA_WOTH, HB_FA_XOTH }
#else
LOCAL aDefs := { HB_FA_READONLY, HB_FA_HIDDEN, HB_FA_SYSTEM, HB_FA_ARCHIVE, HB_FA_COMPRESSED, ;
   HB_FA_ENCRYPTED, HB_FA_NOTINDEXED, HB_FA_SPARSE, HB_FA_REPARSE, HB_FA_TEMPORARY, HB_FA_OFFLINE }
#endif
   LOCAL arr := oPane:aDir[oPane:nCurrent + oPane:nShift], arrD[Len(aDefs)], i
   LOCAL nAttr, nAttrNew

   IF !hb_fGetAttr( arr[1], @nAttr )
      edi_Alert( "Can't read attributes" )
      RETURN Nil
   ENDIF
   nAttrNew := nAttr

#ifdef __PLATFORM__UNIX
   Aadd( aGets, { y1+13,x1+3, 1, .F., 1 } )
#endif
   Aadd( aGets, { y1+17,x1+7, 2, "[Ok]", 6, "N/W","N/W",{||__KeyBoard(Chr(K_ENTER))} } )
   Aadd( aGets, { y1+17,x1+17, 2, "[Cancel]", 10, "N/W","N/W",{||__KeyBoard(Chr(K_ESC))} } )

   cScBuf := Savescreen( y1, x1, y1+18, x1+34 )
   oldc := SetColor( "N/W"+","+"N/W"+",,"+"N+/W"+","+"N/W" )

   cp := hb_cdpSelect( "RU866" )
   @ y1, x1, y1+18, x1+34 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+16, x1 SAY "Ã"
   @ y1+16, x1+34 SAY "´"
   @ y1+16, x1+1 TO y1+16, x1+33
   hb_cdpSelect( cp )

   @ y1,x1+10 SAY " Attributes "
   @ y1+1,x1+2 SAY PAdc( "for " + arr[1], 28 )
#ifdef __PLATFORM__UNIX
   @ y1+2, x1+2 SAY "[ ] Set user ID on execution"
   @ y1+3, x1+2 SAY "[ ] Set group ID on execution"
   @ y1+4, x1+2 SAY "[ ] Sticky bit"
   @ y1+5, x1+2 SAY "[ ] Read by owner"
   @ y1+6, x1+2 SAY "[ ] Write by owner"
   @ y1+7, x1+2 SAY "[ ] Execute/search by owner"
   @ y1+8, x1+2 SAY "[ ] Read by group"
   @ y1+9, x1+2 SAY "[ ] Write by group"
   @ y1+10, x1+2 SAY "[ ] Execute/search by group"
   @ y1+11, x1+2 SAY "[ ] Read by others"
   @ y1+12, x1+2 SAY "[ ] Write by others"
   @ y1+13, x1+2 SAY "[ ] Execute/search by others"
   @ y1+15, x1+4 SAY "Owner: " + cedi_chown( arr[1] ) + "/" + cedi_chgrp( arr[1] )
#else
   @ y1+2, x1+2 SAY "[ ] Readonly"
   @ y1+3, x1+2 SAY "[ ] Hidden"
   @ y1+4, x1+2 SAY "[ ] System"
   @ y1+5, x1+2 SAY "[ ] Archive"
   @ y1+6, x1+2 SAY "[ ] Compressed"
   @ y1+7, x1+2 SAY "[ ] Encrypted"
   @ y1+8, x1+2 SAY "[ ] Not indexed"
   @ y1+9, x1+2 SAY "[ ] Sparse"
   @ y1+10, x1+2 SAY "[ ] Reparse (Link)"
   @ y1+11, x1+2 SAY "[ ] Temporary"
   @ y1+12, x1+2 SAY "[ ] Offline"
#endif

   FOR i := 1 TO Len( aDefs )
      arrD[i] := aGets[i,4] := ( hb_bitAnd(nAttr,aDefs[i]) > 0 )
   NEXT
   IF edi_READ( aGets ) > 0
      FOR i := 1 TO Len( aDefs )
         IF arrD[i] != aGets[i,4]
            IF aGets[i,4]
               nAttrNew := hb_BitOr( nAttrNew, aDefs[i] )
            ELSE
               nAttrNew := hb_BitAnd( nAttrNew, hb_BitNot( aDefs[i] ) )
            ENDIF
         ENDIF
      NEXT
      edi_Alert( str( nAttr ) + " " + str( nAttrNew ) )
      IF nAttr != nAttrNew
         IF !hb_fSetAttr( arr[1], nAttrNew )
            edi_Alert( "Can't set attributes" )
         ENDIF
      ENDIF
   ENDIF

   SetColor( oldc )
   Restscreen( y1, x1, y1+18, x1+34, cScBuf )

   RETURN Nil

STATIC FUNCTION ShowStdout()

   LOCAL cName := "$Stdout", i, oNew

   IF ( i := Ascan( TEdit():aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oHbc, i )
      RETURN Nil
      //RETURN TEdit():aWindows[i]
   ENDIF

   oNew := TEdit():New( FilePane():cConsOut, cName, oHbc:aRectFull[1], oHbc:aRectFull[2], oHbc:aRectFull[3], oHbc:aRectFull[4] )
   oHbc:lShow := .F.
   TEdit():nCurr := Len( TEdit():aWindows )
   oNew:lReadOnly := .T.
   //KEYBOARD Chr( K_PGDN )
   edi_Move( oNew, 71 )

   RETURN Nil

FUNCTION hbc_Console( xCommand )

   LOCAL bufsc, clr, i, nHis := 0, cCommand := "", nCommand := 0, s
   LOCAL xRes, bOldError
   LOCAL pApp, nKeyExt, nKey, cmd, nSecInit, hWnd
   LOCAL bKeys := {|nKey|
      IF nKey == K_DOWN
         IF nHis <= Len( FilePane():aCmdHis )
            nHis ++
            RETURN Iif( nHis <= Len( FilePane():aCmdHis ), FilePane():aCmdHis[nHis], "" )
         ENDIF
      ELSEIF nKey == K_UP
         IF nHis > 1
            nHis --
            RETURN FilePane():aCmdHis[nHis]
         ENDIF
      ELSEIF nKey == K_CTRL_O
         KEYBOARD Chr(K_ENTER)
         RETURN "exit"
      ENDIF
      RETURN Nil
   }

   bufsc := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )
   clr := SetColor( "+W/N" )

   SET CURSOR ON

   IF Empty( cOutBuff )
      CLEAR SCREEN
   ELSE
      RestScreen( 0, 0, nScreenH-1, nScreenW-1, cOutBuff )
   ENDIF

   IF Valtype( FilePane():aCmdHis ) != "A"
      CmdHisLoad()
   ENDIF

   DO WHILE .T.
      nHis := Len( FilePane():aCmdHis ) + 1
      ?
      @ Maxrow(), 0 CLEAR TO Maxrow(), MaxCol()
      DevPos( Maxrow(), 0 )
      SetColor( "+W/N" )
      IF Valtype( xCommand ) == "C"
         cCommand := xCommand
         xCommand := Nil
      ELSEIF Valtype( xCommand ) == "A"
         cCommand := Iif( nCommand < Len( xCommand ), xCommand[++nCommand], "" )
      ENDIF
      IF !Empty( cCommand )
         KEYBOARD Chr( K_ENTER )
      ENDIF
      cCommand := GetLine( ">", cCommand, bKeys )
      IF !Empty( cCommand )
         DO WHILE ( i := At( '%', cCommand ) ) > 0
            s := Substr( cCommand,i+1,1 )
            IF s == 'p'
               s := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
            ELSEIF s == 'f'
               s := oPaneCurr:cCurrPath + oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
            ELSEIF s == 'n'
               s := hb_fnameName( oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1] )
            ELSEIF s == 'd'
               s := oPaneCurr:cCurrPath
            ELSEIF s == 'm'
               s := edi_MsgGet( "Input text", oPaneCurr:y1+10, oPaneCurr:x1+6, oPaneCurr:x2-6 )
            ELSEIF s == '/'
               s := hb_ps()
            ELSEIF s == '%'
            ELSE
               s := ""
            ENDIF
            IF !Empty( s )
               cCommand := Left( cCommand,i-1 ) + s + Substr( cCommand,i+2 )
            ENDIF
         ENDDO
         @ Maxrow(), 0 CLEAR TO Maxrow(), MaxCol()
         DevPos( Maxrow(), 0 )
         ?? cCommand
         IF cCommand == "exit"
            EXIT
         ELSEIF Left( cCommand,1 ) == "="
            bOldError := Errorblock( {|e| MacroError( e ) } )
            BEGIN SEQUENCE
               xRes := &( Substr( cCommand,2 ) )
            END SEQUENCE
            Errorblock( bOldError )
            SetColor( "W/N" )
            ? hb_ValToExp( xRes )
         ELSE
            FErase( cFileOut )
#ifdef __PLATFORM__UNIX
            cCommand += " 2>&1"
            cedi_RunConsoleApp( cCommand, cFileOut )
            IF !Empty( xRes := MemoRead( cFileOut ) )
               IF Chr(9) $ xRes
                  xRes := StrTran( xRes, Chr(9), Space(8) )
               ENDIF
               SetColor( "W/N" )
               FilePane():cConsOut += Chr(13)+Chr(10) + "> " + cCommand + Chr(13)+Chr(10) + xRes
               IF Len( FilePane():cConsOut ) > FilePane():nConsMax
                  i := hb_At( Chr(10), FilePane():cConsOut, Len(FilePane():cConsOut)-FilePane():nConsMax )
                  FilePane():cConsOut := Substr( FilePane():cConsOut, i + 1 )
               ENDIF
               ? xRes
            ENDIF
#else
            cmd := ""
            pApp := cedi_StartConsoleApp( cCommand )
            IF ( xRes :=  cedi_ReturnErrCode( pApp ) ) > 0
               ? "Error starting app ", xRes
               cedi_EndConsoleApp( pApp )
               cCommand := ""
               LOOP
            ENDIF
            FilePane():cConsOut += Chr(13)+Chr(10) + "> " + cCommand + Chr(13)+Chr(10)
            DevPos( Maxrow(), nColInit := 0 )
            ?
            nSecInit := Seconds()
            DO WHILE ( xRes := cedi_ReadFromConsoleApp(pApp) ) != Nil
               IF !Empty( xRes )
                  IF Chr(9) $ xRes
                     xRes := StrTran( xRes, Chr(9), Space(8) )
                  ENDIF
                  SetColor( "W/N" )
                  FilePane():cConsOut += xRes
                  IF Len( FilePane():cConsOut ) > FilePane():nConsMax
                     i := hb_At( Chr(10), FilePane():cConsOut, Len(FilePane():cConsOut)-FilePane():nConsMax )
                     FilePane():cConsOut := Substr( FilePane():cConsOut, i + 1 )
                  ENDIF
                  ?? xRes
                  nColInit := Col()
                  cmd := ""
                  nSecInit := 0
               ENDIF
               nKeyExt := Inkey( 0.05, INKEY_KEYBOARD + HB_INKEY_EXT )
               IF nKeyExt == 0
                  IF nSecInit > 0 .AND. Seconds() - nSecInit > 0.3
                     nSecInit := 0
                     IF !Empty( hWnd := cedi_GETHWNDBYPID( pApp ) )
                        //IF ( i := edi_Alert( "Show app window?", "Yes", "No", "Close it!" ) ) == 1
                           cedi_ShowWindow( hWnd )
                           cedi_EndConsoleApp( pApp, .T. )
                           pApp := Nil
                           EXIT
                        //ELSEIF i == 3
                        //   EXIT
                        //ENDIF
                     ENDIF
                  ENDIF
                  LOOP
               ELSEIF (nKey := hb_keyStd( nKeyExt )) == K_ESC .OR. ;
                     ( hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0 .AND. nKey == K_CTRL_C )
                  EXIT
               ELSEIF hb_keyStd( nKeyExt ) == K_ENTER
                  IF !cedi_WriteToConsoleApp( pApp, cmd+Chr(13)+Chr(10) )
                     ? "Pipe write error"
                  ENDIF
               ELSE
                  cmd := ProcessKey( nColInit, cmd, nKeyExt )
               ENDIF
            ENDDO
            cedi_EndConsoleApp( pApp )
#endif
         ENDIF
         IF ( i := Ascan( FilePane():aCmdHis, {|s|s == cCommand} ) ) > 0
            FilePane():aCmdHis := hb_ADel( FilePane():aCmdHis, i, .T. )
         ENDIF
         Aadd( FilePane():aCmdHis, cCommand )
         FilePane():lCmdHis := .T.
         cCommand := ""
      ENDIF
   ENDDO
   cOutBuff := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )
   SetColor( clr )
   RestScreen( 0, 0, nScreenH-1, nScreenW-1, bufsc )
   SET CURSOR OFF
   oPaneCurr:Refresh()
   FilePane():RedrawAll()

   RETURN Nil

STATIC FUNCTION GetLine( cMsg, cRes, bKeys )

   LOCAL nRow := Row(), nPos := Len(cRes) + 1, nColInit, nKeyExt, nKey

   DevOut( Iif( cMsg==Nil, "", cMsg ) )
   nColInit := Col()
   IF cRes == Nil
      cRes := ""
   ELSEIF !Empty( cRes )
      DevOut( cRes )
   ENDIF
   DO WHILE .T.
      nKeyExt := Inkey( 0, INKEY_KEYBOARD + HB_INKEY_EXT )
      IF (nKey := hb_keyStd( nKeyExt )) == K_ENTER
         RETURN cRes
      ELSEIF nKey == K_ESC
         RETURN ""
      ELSE
         cRes := ProcessKey( nColInit, cRes, nKeyExt, bKeys )
      ENDIF
   ENDDO

   RETURN cRes

STATIC FUNCTION ProcessKey( nColInit, cRes, nKeyExt, bKeys )

   LOCAL nRow := Row(), lChg, cTemp, nResLen := Len( cRes ), nPos := Col() - nColInit + 1
   LOCAL nKey := hb_keyStd( nKeyExt )
   STATIC lIns := .T.

   lChg := .F.
   IF nKey >= 32 .AND. nKey <= 250
      cRes := Left( cRes, nPos-1 ) + Chr(nKey) + Substr( cRes, Iif(lIns,nPos,nPos+1) )
      nPos ++
      lChg := .T.
   ELSEIF nKey == K_DEL
      IF nPos <= Len( cRes )
         cRes := Left( cRes, nPos-1 ) + Substr( cRes, nPos+1 )
         lChg := .T.
      ENDIF
   ELSEIF nKey == K_BS
      IF nPos > 1
         cRes := Left( cRes, nPos-2 ) + Substr( cRes, nPos )
         nPos --
         lChg := .T.
      ENDIF
   ELSEIF nKey == K_RIGHT
      IF nPos <= Len( cRes )
         nPos ++
      ENDIF
   ELSEIF nKey == K_LEFT
      IF nPos > 1
         nPos --
      ENDIF
   ELSEIF nKey == K_HOME
      nPos := 1
   ELSEIF nKey == K_END
      nPos := Len( cRes ) + 1
   ELSEIF nKey == K_ENTER
      RETURN cRes
   ELSEIF (hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0 .AND. nKey == 22) .OR. ;
      ( hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0 .AND. nKey == K_INS )
      cTemp := s_cb2t()
      cRes := Left( cRes, nPos-1 ) + cTemp + Substr( cRes, Iif(lIns,nPos,nPos+1) )
      nPos := nPos + Len( cTemp )
      lChg := .T.
   ELSEIF nKey == K_INS
      lIns := !lIns
   ELSEIF nKey == K_ESC
      cRes := ""
      RETURN cRes
   ELSEIF !Empty( bKeys ) .AND. Valtype( cTemp := Eval( bKeys,nKey ) ) == "C"
      cRes := cTemp
      nPos := Len( cRes ) + 1
      lChg := .T.
   ENDIF
   IF lChg
      DevPos( nRow, nColInit )
      DevOut( cRes )
      IF nResLen > Len( cRes )
         DevOut( Space(nResLen - Len( cRes )) )
      ENDIF
   ENDIF
   DevPos( nRow, nColInit + nPos - 1 )

   RETURN cRes

STATIC FUNCTION CmdHisLoad()

   LOCAL arr := hb_ATokens( Memoread( hb_DirBase() + "hbc.his" ), Chr(10) ), i

   FOR i := Len(arr) TO 1 STEP -1
      IF Empty( arr[i] )
         hb_ADel( arr, i, .T. )
      ELSEIF Right( arr[i],1 ) == Chr(13)
         arr[i] := hb_strShrink( arr[i], 1 )
      ENDIF
   NEXT

   FilePane():aCmdHis := arr

   RETURN Nil

STATIC FUNCTION CmdHisSave()

   LOCAL i, s := "", nCou := 0, nLen

   IF !Empty( FilePane():aCmdHis ) .AND. FilePane():lCmdHis
      nLen := Len(FilePane():aCmdHis)
      FOR i := Max( 1,nLen-200 ) TO nLen
         IF Len( FilePane():aCmdHis[i] ) > 2
            s += FilePane():aCmdHis[i] + Chr(10)
         ENDIF
      NEXT
      hb_MemoWrit( hb_DirBase() + "hbc.his", s )
      FilePane():lCmdHis := .F.
   ENDIF

   RETURN Nil

STATIC FUNCTION WndInit( y1, x1, y2, x2, clr )

   LOCAL cBuf := Savescreen( y1, x1, y2, x2 )

   IF Empty( clr )
      clr := "N/W"
   ENDIF
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ " COLOR (clr)

   RETURN { y1, x1, y2, x2, clr, cBuf }

STATIC FUNCTION WndOut( arr, cText )

   LOCAL clr := SetColor( arr[5] )

   Scroll( arr[1]+1, arr[2]+1, arr[3]-1, arr[4]-1, 1 )
   @ arr[3]-1, arr[2]+2 SAY cText
   SetColor( clr )

   RETURN Nil

STATIC FUNCTION WndClose( arr, cText )

   LOCAL clr := SetColor( arr[5] )

   Scroll( arr[1]+1, arr[2]+1, arr[3]-1, arr[4]-1, 1 )
   @ arr[3]-1, arr[2]+2 SAY cText COLOR (arr[5])

   Inkey( 0 )
   SetColor( clr )
   Restscreen( arr[1], arr[2], arr[3], arr[4], arr[6] )

   RETURN Nil

FUNCTION Ascan2( arr, xItem )
   RETURN Ascan( arr, {|a|a[1]==xItem} )
/*
#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapiitm.h"
#if defined (HB_OS_UNIX)
#include <gtk/gtk.h>
HB_FUNC( FRUNAPP )
{
   g_spawn_command_line_async( hb_parc(1), NULL );
}
#else
#include <windows.h>
HB_FUNC( FRUNAPP )
{
   WinExec( hb_parc( 1 ), SW_SHOW );
}
#endif

#pragma ENDDUMP
*/
