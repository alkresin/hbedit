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
#include "hbgtinfo.ch"

#include "hbc.ch"
#ifdef __PSEUDOGT
   #include "hwpgt.ch"
#endif

REQUEST HB_CODEPAGE_RU866
REQUEST HB_CODEPAGE_RU1251
REQUEST HB_CODEPAGE_RUKOI8
REQUEST HB_CODEPAGE_FR850
REQUEST HB_CODEPAGE_FRWIN
REQUEST HB_CODEPAGE_FRISO
REQUEST HB_CODEPAGE_UTF8

REQUEST NETIO_PROCEXISTS, NETIO_PROCEXEC, NETIO_FUNCEXEC
#ifdef GTHWG
REQUEST GTHWG_PAINT_SETCALLBACK
#endif

STATIC oHbc
STATIC lGuiVer := .F.
STATIC nScreenH := 25, nScreenW := 80
STATIC cOutBuff
STATIC oPaneCurr, oPaneTo
STATIC lCase_Sea := .F., lRegex_Sea := .F.
#ifdef __PLATFORM__UNIX
STATIC cTermApp := "gnome-terminal -x"
STATIC aExtExe := { ".sh" }
#else
STATIC aExtExe := { ".exe", ".com", ".bat" }
#endif
STATIC aExtZip := { ".zip", ".rar", ".7z", ".lha", ".arj", ".gz" }
#ifdef _USE_SSH2
STATIC aRemote := { "net:", "sftp:" }
STATIC aRemotePorts := { "2941", "22" }
#else
STATIC aRemote := { "net:" }
STATIC aRemotePorts := { "2941" }
#endif
STATIC aCpInUse := { "RU866", "RU1251", "UTF8" }
STATIC aUtf8Auto := { "sftp:", "ftp:" }
STATIC cNotPerm := "Operation isn't permitted"

#xtranslate _I( <x,...> ) => hb_i18n_gettext( <x> )

MEMVAR GETLIST

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

   IF Empty( FilePane():cp )
#if defined (GTHWG) || defined (GTWVT)
      lGuiVer := .T.
#endif

      oHbc := mnu_NewBuf( oEdit )
      FilePane():vy1 := TEdit():aRectFull[1]
      FilePane():vx1 := TEdit():aRectFull[2]
      FilePane():vy2 := TEdit():aRectFull[3]
      FilePane():vx2 := TEdit():aRectFull[4]
      nScreenH := FilePane():vy2 + 1
      nScreenW := FilePane():vx2 + 1

      oHbc:cFileName := cName
      oHbc:lTopPane := .F.
      oHbc:bOnKey := {|o,n| _Hbc_OnKey(o,n) }
      oHbc:bStartEdit := {|| _Hbc_Start() }
      oHbc:bEndEdit := {|| hb_gtinfo( HB_GTI_WINTITLE, "HbEdit" ) }
      oHbc:bTextOut := bTextOut
      oHbc:lIns := Nil

      aPanes := ReadIni( hb_DirBase() + "hbc.ini" )
      hb_cdpSelect( oHbc:cp := FilePane():cp )
      oHbc:lUtf8 := ( Lower(oHbc:cp) == "utf8" )
      Set( _SET_DATEFORMAT, FilePane():cDateFormat )
      SetPanes( aPanes )
   ENDIF

   RETURN Nil

STATIC FUNCTION _Hbc_Start()

   hb_gtinfo( HB_GTI_WINTITLE, "HbCommander" )
   SetCursor( SC_NONE )
   DirChange( oPaneCurr:cCurrPath )
   FilePane():RedrawAll()
   IF FilePane():lConsMode
      KEYBOARD Chr( K_CTRL_O )
   ENDIF

   RETURN Nil

STATIC FUNCTION _Hbc_OnKey( oEdit_Hbc, nKeyExt )

   LOCAL nKey, cPath, nPos, lRedraw, cExt, cExtFull, cTemp, o, nRow, nCol, i, aDir
   LOCAL bufsc

   IF !Empty( oPaneCurr:bOnKey )
      i := Eval( oPaneCurr:bOnKey, oPaneCurr, nKeyExt )
      IF i == - 1
         RETURN -1
      ELSEIF i > 0
         nKeyExt := i
      ENDIF
   ENDIF

   nKey := hb_keyStd( nKeyExt )

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE .OR. nKey == K_LBUTTONUP
      RETURN -1
   ENDIF

   IF oPaneCurr:nCurrent == 0 .AND. !( (nKey >= 65 .AND. nKey <= 122) .OR. nKey == K_CTRL_TAB .OR. nKey == K_ALT_TAB .OR. ;
      nKey == K_F1 .OR. nKey == K_F9 .OR. nKey == K_TAB .OR. nKey == K_CTRL_PGUP .OR. nKey == K_F10 )
      RETURN -1
   ENDIF
   IF oPaneTo:nPanelMod == 3 .AND. !( nKey == K_LEFT .OR. nKey == K_RIGHT .OR. nKey == K_UP ;
      .OR. nKey == K_DOWN .OR. nKey == K_HOME .OR. nKey == K_END .OR. nKey == K_UP .OR. nKey == K_PGDN )
      IF !( nKey == K_LBUTTONDOWN .AND. (nRow := MRow()) > 0 .AND. (nCol := MCol()) < oPaneCurr:x2 ;
         .AND. nCol > oPaneCurr:x1 )
         oPaneTo:nPanelMod := oPaneTo:nPanelMod_bak
         IF !Empty( oPaneTo:cQVpref )
            PlugFunc( oPaneCurr, oPaneTo:cQVpref, "QEND" )
            oPaneTo:cQVpref := ""
         ENDIF
         oPaneTo:RedrawAll()
         IF nKey == K_CTRL_Q
            RETURN -1
         ENDIF
      ENDIF
   ENDIF

   aDir := Iif( Empty(oPaneCurr:aDir).OR.oPaneCurr:nCurrent==0, {}, oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift] )
   IF nKey == K_F9
     IF !oPaneCurr:PaneMenu()
        mnu_Exit( oEdit_Hbc )
     ENDIF

   ELSEIF nKey == K_F10
      mnu_Exit( oEdit_Hbc )

   ELSEIF nKey == K_F5
      IF Empty( oPaneCurr:aSelected )
         hbc_FCopyFile( oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift] )
      ELSE
         hbc_FCopySele()
      ENDIF

   ELSEIF nKey == K_F6
      IF Empty( oPaneCurr:aSelected )
         hbc_FRename( .F. )
      ELSE
         hbc_FRenameSele()
      ENDIF

   ELSEIF nKey == K_SH_F6
      hbc_FRename( .T. )

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

   ELSEIF nKey == K_SH_F11
      mnu_Plugins( oHbc )

   ELSEIF nKey == K_F12
      mnu_Buffers( oHbc, {oPaneCurr:y1+1,oPaneCurr:x1+1} )

   ELSEIF nKey == K_F1
      mnu_Help( oHbc, edi_FindPath( "hbc.help" ), Iif( FilePane():lConsMode,":: :",Nil ) )
      edi_SetPalette( oHbc, oHbc:cPalette )
      hb_CdpSelect( FilePane():cp )
      IF FilePane():lConsMode
         KEYBOARD Chr( K_CTRL_O )
      ELSE
         FilePane():RedrawAll()
      ENDIF

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
     IF oPaneCurr:nDispMode == 2 .AND. oPaneCurr:nCurrent > oPaneCurr:nRows
         oPaneCurr:DrawCell( ,.F. )
         oPaneCurr:nCurrent -= oPaneCurr:nRows
         oPaneCurr:DrawCell( ,.T. )
      ENDIF

   ELSEIF nKey == K_RIGHT
      IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
         RETURN -1
      ELSEIF oPaneCurr:nDispMode == 2
         oPaneCurr:DrawCell( ,.F. )
         IF oPaneCurr:nCurrent + oPaneCurr:nRows <= oPaneCurr:nCells .AND. ;
            oPaneCurr:nShift + oPaneCurr:nCurrent + oPaneCurr:nRows <= Len( oPaneCurr:aDir )
            oPaneCurr:nCurrent += oPaneCurr:nRows
         ELSE
            oPaneCurr:nCurrent := Len( oPaneCurr:aDir ) - oPaneCurr:nShift
         ENDIF
         oPaneCurr:DrawCell( ,.T. )
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

   ELSEIF nKey == K_PGUP  .AND. hb_BitAnd( nKeyExt, CTRL_PRESSED ) == 0
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
                  oPaneCurr:bOnKey := oPaneCurr:bDraw := oPaneCurr:bDrawCell := oPaneCurr:bDrawHead := oPaneCurr:bRefresh := Nil
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
            cPath := oPaneCurr:cCurrPath + Iif(Right(oPaneCurr:cCurrPath,1) $ "\/", "", hb_ps() ) + oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1] //+ hb_ps()
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
            cTemp := Substr( cExt,2 )
            IF ','+cTemp+',' $ oPaneCurr:cDocHis
               AddDocHis( 1, oPaneCurr:cCurrPath + aDir[1], oPaneCurr:cpPane, .F. )
            ELSEIF ','+cTemp+',' $ oPaneCurr:cDocHis2
               AddDocHis( 2, oPaneCurr:cCurrPath + aDir[1], oPaneCurr:cpPane, .F. )
            ELSEIF ','+cTemp+',' $ oPaneCurr:cDocHis3
               AddDocHis( 3, oPaneCurr:cCurrPath + aDir[1], oPaneCurr:cpPane, .F. )
            ENDIF
            IF ( nPos := Ascan( oPaneCurr:aExtEnter, {|a|a[1] == cExtFull .or. '/'+cExtFull+'/' $ a[1]} ) ) == 0
               nPos := Ascan( oPaneCurr:aExtEnter, {|a|a[1] == cTemp .or. '/'+cTemp+'/' $ a[1]} )
            ENDIF
            cTemp := oPaneCurr:cCurrPath + aDir[1]
            IF ' ' $ cTemp
               cTemp := '"' + cTemp + '"'
            ENDIF
            IF nPos > 0
               IF Left( oPaneCurr:aExtEnter[nPos,2], 1 ) == '@'
                  hbc_Console( Substr(oPaneCurr:aExtEnter[nPos,2],2) + " " + cTemp, .T. )
               ELSE
#ifdef __PLATFORM__WINDOWS
                  IF !( oPaneCurr:cpPane == "UTF8" )
                     cTemp := hb_strToUtf8( cTemp, oPaneCurr:cpPane )
                  ENDIF
#endif
                  cedi_RunApp( oPaneCurr:aExtEnter[nPos,2] + " " + cTemp, .T. )
               ENDIF
#ifdef __PLATFORM__WINDOWS
            ELSEIF cExt == ".bat"
               IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0
                  cedi_ShellExecute( cTemp )
               ELSE
                  hbc_Console( cTemp )
               ENDIF
            ELSEIF cExt == ".exe"
               cedi_ShellExecute( cTemp )
#endif
#ifdef __PLATFORM__UNIX
            ELSEIF cExt == ".sh" .OR. 'X' $ aDir[5]
               IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0
                  cedi_RunApp( cTermApp + " " + cTemp )
               ELSE
                  hbc_Console( cTemp )
               ENDIF
#endif

            ELSEIF Ascan( aExtZip, {|s| s==cExt} ) > 0 .AND. hbc_FReadArh()
            ELSE
#ifdef __PLATFORM__UNIX
#ifdef GTHWG
               hwg_shellExecute( "file://" + cTemp )
#endif
#else
#ifdef __PLATFORM__WINDOWS
               IF !( oPaneCurr:cpPane == "UTF8" )
                  cTemp := hb_strToUtf8( cTemp, oPaneCurr:cpPane )
               ENDIF
#endif
               cedi_shellExecute( cTemp, .T. )
#endif
            ENDIF
         ENDIF
      ENDIF

   ELSEIF nKey == K_SPACE
      cExtFull := Lower( Strtran( GetFullExt( aDir[1] ), '.', '' ) )
      IF ( i := Ascan2( FilePane():aPlugins, "plug_hbc_ext_"+cExtFull+".hrb" ) ) > 0
         edi_RunPlugin( oPaneCurr, FilePane():aPlugins, i, {oPaneCurr,oPaneCurr:cCurrPath+aDir[1]} )
      ELSEIF ( i := Ascan2( FilePane():aPlugins, "plug_hbc_ext_all.hrb" ) ) > 0
         edi_RunPlugin( oPaneCurr, FilePane():aPlugins, i, {oPaneCurr,oPaneCurr:cCurrPath+aDir[1]} )
      ENDIF

   ELSEIF nKey == K_CTRL_Q
      IF oPaneCurr:nPanelMod == 2
         RETURN -1
      ENDIF
      oPaneTo:nPanelMod_bak := oPaneTo:nPanelMod
      oPaneTo:nPanelMod := 3
      oPaneTo:cQVpref := ""

   ELSEIF nKey == K_F3 .OR. nKey == K_CTRL_F3
      IF 'D' $ aDir[5]
         hbc_FCalcSize()
         RETURN -1
      ENDIF
      nPos := 0
      IF nKey == K_CTRL_F3
         IF Empty( oPaneCurr:cIOpref ) .OR. oPaneCurr:nPanelMod == 1
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
            cTemp := hb_DirTemp() + "hbc_view.tmp"
            IF hb_unzipFileGoto( oPaneCurr:hUnzip, oPaneCurr:aZipFull[aDir[ADIR_POS],AZF_POS] ) == 0 ;
               .AND. hb_unzipExtractCurrentFile( oPaneCurr:hUnzip, cTemp ) == 0
               FileView( cTemp, oPaneCurr:vx1, oPaneCurr:vy1, oPaneCurr:vx2, oPaneCurr:vy2 )
               FErase( cTemp )
            ENDIF
         ELSE
            FileView( Iif( oPaneCurr:nPanelMod==1, oPaneCurr:cIOpref_bak + oPaneCurr:net_cAddress_bak, ;
               oPaneCurr:cIOpref + oPaneCurr:net_cAddress ) + oPaneCurr:net_cPort + ;
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
         IF Empty( oPaneCurr:cIOpref ) .OR. oPaneCurr:nPanelMod == 1
            cTemp := Lower( Substr( GetFullExt( aDir[1] ), 2 ) )
            IF ( nPos := Ascan( oPaneCurr:aExtEdit, {|a|a[1] == cTemp .or. '/'+cTemp+'/' $ a[1]} ) ) > 0
               cedi_RunApp( oPaneCurr:aExtEdit[nPos,2] + " " + oPaneCurr:cCurrPath + aDir[1] )
            ENDIF
         ENDIF
      ENDIF
      IF nPos == 0
         cTemp := Iif( oPaneCurr:nPanelMod==1, oPaneCurr:cIOpref_bak + oPaneCurr:net_cAddress_bak, ;
            oPaneCurr:cIOpref + oPaneCurr:net_cAddress ) + oPaneCurr:net_cPort + ;
            oPaneCurr:cCurrPath + aDir[1]
         IF Empty( oPaneCurr:cIOpref ) .OR. oPaneCurr:nPanelMod == 1
            mnu_NewBuf( oHbc, cTemp )
         ELSEIF Ascan( aRemote, oPaneCurr:cIOpref ) > 0
            //oPaneCurr:cIOpref == "net:" .OR. oPaneCurr:cIOpref == "sftp:"
            mnu_NewBuf( oHbc, cTemp, hb_vfLoad(cTemp), @vfWrit_Net() )
         ELSEIF oPaneCurr:cIOpref == "zip:"
            i := hb_unzipFileGoto( oPaneCurr:hUnzip, oPaneCurr:aZipFull[aDir[ADIR_POS],AZF_POS] )
            IF i == 0 .AND. hb_unzipFileOpen( oPaneCurr:hUnzip ) == 0
               cTemp := Space( aDir[2] )
               hb_unzipFileRead( oPaneCurr:hUnzip, @cTemp )
               hb_unzipFileClose( oPaneCurr:hUnzip )
               mnu_NewBuf( oHbc, "zip|"+oPaneCurr:net_cAddress+"|"+aDir[1], cTemp )
            ELSE
               edi_Alert( _I("Something goes wrong...") )
            ENDIF
         ENDIF
      ENDIF

   ELSEIF nKey == K_SH_F4
      RETURN 0

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_ALT_TAB
      IF Len( TEdit():aWindows ) == 1
         RETURN edi_KeyCToN( "Shift-F4" )
      ELSE
         RETURN 0
      ENDIF

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
         IF o:nDispMode != 2 .OR. (o:nDispMode == 2 .AND. nCol <= o:nWidth)
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
   ELSEIF nKey == K_CTRL_INS .AND. hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      cTemp := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
      IF cTemp == ".."
         cTemp := oPaneCurr:cCurrPath
      ENDIF
      s_t2cb( oHbc, cTemp )
   ELSEIF nKey == K_CTRL_ENTER .AND. hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      FilePane():cConsCmd += oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
      hbc_Console()
   ELSEIF nKey == K_CTRL_O
      hbc_Console()
   ELSEIF nKey == K_CTRL_W
      ShowStdout()
   ELSEIF nKey == K_CTRL_P
      hbc_PaneOpt()
   //ELSEIF nKey == K_ALT_D
   //   oPaneCurr:ChangeDir()
   ELSEIF nKey == K_CTRL_F1
      IF !Empty( cTemp := hbc_SelePath( FilePane():vy1-1, FilePane():aPanes[1]:x1-1 ) )
         FilePane():aPanes[1]:ChangeDir( cTemp )
      ENDIF
   ELSEIF nKey == K_CTRL_F2
      IF !Empty( cTemp := hbc_SelePath( FilePane():vy1-1, FilePane():aPanes[2]:x1-1 ) )
         FilePane():aPanes[2]:ChangeDir( cTemp )
      ENDIF
   ELSEIF nKey == K_CTRL_F7
      hbc_Search()
   ELSEIF nKey == K_CTRL_F12
      AppList( oPaneCurr )
   ELSEIF nKey == K_SH_F1
      hbc_Zip()
   ELSEIF nKey == K_CTRL_F8
      hbc_CmdHis()
   ELSEIF nKey == K_SH_F2
      cExt := Lower( hb_fnameExt( aDir[1] )   )
      IF Ascan( aExtZip, {|s| s==cExt} ) > 0
         hbc_Unzip()
      ENDIF
   ELSEIF nKey == K_CTRL_R .AND. hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      oPaneCurr:Refresh()
      IF oPaneCurr:nCurrent + oPaneCurr:nShift > Len( oPaneCurr:aDir )
         oPaneCurr:nShift := Max( 0, Len( oPaneCurr:aDir ) ) - oPaneCurr:nCells
         oPaneCurr:nCurrent := Len( oPaneCurr:aDir ) - oPaneCurr:nShift
      ENDIF
      oPaneCurr:RedrawAll()
   ELSEIF nKey == 43     // +
      hbc_Search( .T. )
   ELSEIF nKey == 61     // =
      KEYBOARD Chr(nKey)
      hbc_Console()
   ELSEIF nKey == 109    // m
      i := hbc_Wndinit( 2, 4, 6, 40,, _I("Set Bookmark") )
      hbc_Wndout( i, "a" + Iif( hb_hHaskey(oHbc:hBookMarks,97), ;
         " "+NameShortcut( (aDir := oHbc:hBookMarks[97])[1]+aDir[4],35,'~' ), "") )
      hbc_Wndout( i, "s" + Iif( hb_hHaskey(oHbc:hBookMarks,115), ;
         " "+NameShortcut( (aDir := oHbc:hBookMarks[115])[1]+aDir[4],35,'~' ), "") )
      hbc_Wndout( i, "d" + Iif( hb_hHaskey(oHbc:hBookMarks,100), ;
         " "+NameShortcut( (aDir := oHbc:hBookMarks[100])[1]+aDir[4],35,'~' ), "") )
      nKey := Inkey(0)
      hbc_Wndclose( i )
      IF Chr( nKey ) $ "asd"
         oHbc:hBookMarks[nKey] := { oPaneCurr:cIOpref, oPaneCurr:net_cAddress, ;
            oPaneCurr:net_cPort, oPaneCurr:cCurrPath }
      ENDIF
   ELSEIF nKey == 39     // '
      i := hbc_Wndinit( 2, 4, 6, 40,, _I("Go to Bookmark") )
      hbc_Wndout( i, "a" + Iif( hb_hHaskey(oHbc:hBookMarks,97), ;
         " "+NameShortcut( (aDir := oHbc:hBookMarks[97])[1]+aDir[4],35,'~' ), "") )
      hbc_Wndout( i, "s" + Iif( hb_hHaskey(oHbc:hBookMarks,115), ;
         " "+NameShortcut( (aDir := oHbc:hBookMarks[115])[1]+aDir[4],35,'~' ), "") )
      hbc_Wndout( i, "d" + Iif( hb_hHaskey(oHbc:hBookMarks,100), ;
         " "+NameShortcut( (aDir := oHbc:hBookMarks[100])[1]+aDir[4],35,'~' ), "") )
      nKey := Inkey(0)
      hbc_Wndclose( i )
      IF Chr( nKey ) $ "asd" .AND. hb_hHaskey( oHbc:hBookMarks, nKey )
         aDir := oHbc:hBookMarks[nKey]
         oPaneCurr:ChangeDir( aDir[1]+aDir[2]+aDir[3]+aDir[4] )
      ENDIF
   ELSE
      nKey := edi_MapKey( oHbc, nKey )
      IF nKey == 68 .OR. nKey == 100  // D d
         oPaneCurr:ChangeDir()
      ELSEIF nKey == 72 .OR. nKey == 104  // H h
         hbc_HistMnu()
      ELSE
         IF !Empty( FilePane():aDefPaths )
            FOR i := 1 TO Len( FilePane():aDefPaths )
               IF FilePane():aDefPaths[i,3] == nKeyExt
                  oPaneCurr:ChangeDir( FilePane():aDefPaths[i,1] )
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   IF oPaneTo:nPanelMod == 3
      cTemp := oPaneTo:cQVpref
      oPaneTo:cQVpref := ""
      PlugFunc( oPaneCurr, cTemp, "QEND" )
      aDir := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift]
      cExtFull := Lower( Substr( GetFullExt( aDir[1] ), 2 ) )
      IF ( nPos := Ascan( oPaneCurr:aQView, {|a|a[1] == cExtFull .or. '/'+cExtFull+'/' $ a[1]} ) ) > 0
         edi_RunPlugin( oPaneCurr, FilePane():aPlugins, oPaneCurr:aQView[nPos,2] )
      ENDIF
      IF Empty( oPaneTo:cQVpref )
         QFileView( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
            oPaneCurr:cCurrPath + aDir[1], ;
            , oPaneTo:x1, oPaneTo:y1, oPaneTo:x2, oPaneTo:y2 )
      ELSE
         PlugFunc( oPaneCurr, oPaneTo:cQVpref, "QVIEW", { oPaneCurr:cCurrPath + oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift][1] } )
      ENDIF
   ENDIF

   IF FilePane():nLastKey != 0
      hb_KeyPut( FilePane():nLastKey )
      FilePane():nLastKey := 0
   ENDIF

   RETURN -1

STATIC FUNCTION ReadIni( cIniName )

   LOCAL hIni := edi_iniRead( cIniName ), aSect, arr, i, cTmp, s, nPos, n
   LOCAL aPanes := { Nil, Nil }, cp, lPalette := .F.

   FilePane():hMisc := hb_Hash()
   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      IF hb_hHaskey( hIni, cTmp := "OPTIONS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         IF hb_hHaskey( aSect, cTmp := "cp" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cp := Upper( cTmp )
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "palette" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            edi_SetPalette( oHbc, cTmp )
            lPalette := .T.
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "context_menu_plugin" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            FilePane():xContextMenu := cTmp
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "consauto" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            FilePane():lConsAuto := ( Lower(cTmp) == "on" )
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "stdout_size" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            FilePane():nConsMax := Val( cTmp )
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "cpinuse" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            aCpInUse := hb_Atokens( cTmp, ',' )
            FOR i := Len( aCpInUse ) TO 1 STEP -1
               IF i > 5 .OR. Empty( aCpInUse[i] ) .OR. !hb_cdpExists( aCpInUse[i] := Upper(aCpInUse[i]) )
                  aCpInUse := hb_ADel( aCpInUse, i, .T. )
               ENDIF
            NEXT
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "utf8auto" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            aUtf8Auto := hb_Atokens( cTmp, ',' )
            FOR i := Len( aUtf8Auto ) TO 1 STEP -1
               IF Empty( aUtf8Auto[i] )
                  aUtf8Auto := hb_ADel( aUtf8Auto, i, .T. )
               ENDIF
            NEXT
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "dochis" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            FilePane():cDocHis := "," + cTmp + ","
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "dochis-2" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            FilePane():cDocHis2 := "," + cTmp + ","
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "dochis-3" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            FilePane():cDocHis3 := "," + cTmp + ","
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "docmax" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            FilePane():nDocMax := Val( cTmp )
         ENDIF
#ifdef __PLATFORM__UNIX
         IF hb_hHaskey( aSect, cTmp := "termapp" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cTermApp := cTmp
         ENDIF
#endif
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "COLORS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FOR i := 1 TO Len( arr )
            IF !Empty( cTmp := aSect[ arr[i] ] )
               IF arr[i] == "colorbox"
                  FilePane():cClrBox := cTmp
               ELSEIF arr[i] == "colordir"
                  FilePane():cClrDir := cTmp
               ELSEIF arr[i] == "colorfile"
                  FilePane():cClrFil := cTmp
               ELSEIF arr[i] == "colorfileexe"
                  FilePane():cClrExe := cTmp
               ELSEIF arr[i] == "colorfilezip"
                  FilePane():cClrZip := cTmp
               ELSEIF arr[i] == "colorfilehidden"
                  FilePane():cClrHid := cTmp
               ELSEIF arr[i] == "colorcurr"
                  FilePane():cClrCurr := cTmp
               ELSEIF arr[i] == "colorsel"
                  FilePane():cClrSel := cTmp
               ELSEIF arr[i] == "colorselcurr"
                  FilePane():cClrSelCurr := cTmp
               ELSEIF arr[i] == "colormenuf"
                  FilePane():aClrMenu[1] := cTmp
               ELSEIF arr[i] == "colormenub"
                  FilePane():aClrMenu[2] := cTmp
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
      IF hb_hHaskey( hIni, cTmp := "QUICKVIEW" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FilePane():aQView := Array( Len( arr ) )
         FOR i := 1 TO Len( arr )
            cTmp := aSect[ arr[i] ]
            IF !Empty( edi_FindPath( "plugins" + hb_ps() + cTmp ) )
               Aadd( FilePane():aPlugins, { cTmp, "", "qv", Nil, Nil } )
               FilePane():aQView[i] := { Lower( arr[i] ), Len( FilePane():aPlugins ) }
            ENDIF
         NEXT
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "PLUGINS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FOR i := 1 TO Len( arr )
            s := aSect[ arr[i] ]
            IF ( n := At( ",", s ) ) > 0
               cTmp := AllTrim( Left( s,n-1 ) )
               IF !Empty( edi_FindPath( "plugins" + hb_ps() + cTmp ) )
                  s := Substr( s, n+1 )
                  IF ( n := At( ",", s ) ) > 0
                     Aadd( FilePane():aPlugins, { cTmp, Substr( s, n+1 ), AllTrim(Left( s,n-1 )), Nil, Nil } )
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
               cTmp := AllTrim( Left( s,n-1 ) )
               s := Substr( s, n+1 )
            ELSE
               cTmp := s
               s := hb_fnameName( s )
            ENDIF
            FilePane():aAppList[i] := { cTmp, s }
         NEXT
      ENDIF

      IF hb_hHaskey( hIni, cTmp := "MISC" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         arr := hb_hKeys( aSect )
         FOR i := 1 TO Len( arr )
            FilePane():hMisc[Lower(arr[i])] := aSect[ arr[i] ]
         NEXT
      ENDIF
   ENDIF

   cTmp := hb_DirBase()
#ifdef __PLATFORM__UNIX
   IF hb_dirExists( s := ( hb_getenv( "HOME" ) + "/hbedit" ) )
      cTmp := s + "/"
   ENDIF
#endif
   FilePane():aCmdHis := {}
   FilePane():aDocHis := { {}, {}, {} }
   IF FilePane():hCmdTrie == Nil
      FilePane():hCmdTrie := trie_Create( .F. )
   ENDIF
   hIni := edi_iniRead( cTmp + "hbc.his" )
   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      IF hb_hHaskey( hIni, cTmp := "COMMANDS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         arr := ASort( hb_hKeys( aSect ) )
         FOR i := 1 TO Len(arr)
            arr[i] := aSect[ arr[i] ]
         NEXT
         FilePane():aCmdHis := arr
         FOR i := 1 TO Len( arr )
            trie_Add( FilePane():hCmdTrie, arr[i] )
         NEXT
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "DOCUMENTS" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         FilePane():aDocHis[1] := CrDocHis( aSect )
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "DOCUMENTS-2" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         FilePane():aDocHis[2] := CrDocHis( aSect )
      ENDIF
      IF hb_hHaskey( hIni, cTmp := "DOCUMENTS-3" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         FilePane():aDocHis[3] := CrDocHis( aSect )
      ENDIF
   ENDIF

   IF Empty( cp )
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

STATIC FUNCTION CrDocHis( aSect )

   LOCAL i, arr

   arr := ASort( hb_hKeys( aSect ) )
   FOR i := 1 TO Len(arr)
      arr[i] := hb_ATokens( aSect[ arr[i] ], ';' )
   NEXT

   RETURN arr

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

   FilePane():New( Filepane():vx1, Filepane():vy1, Int(nScreenW/2)-1, nScreenH-1, nMode1, cDir1 )
   FilePane():New( Int(nScreenW/2), Filepane():vy1, nScreenW-1, nScreenH-1, nMode2, cDir2 )
   oPaneCurr := FilePane():aPanes[1]
   oPaneTo   := FilePane():aPanes[2]

   RETURN Nil

CLASS FilePane

   CLASS VAR aPanes SHARED INIT {}
   CLASS VAR aDefPaths SHARED
   CLASS VAR aExtView, aExtEdit, aExtEnter, aQView SHARED
   CLASS VAR aCmdHis   SHARED
   CLASS VAR lCmdHis   SHARED INIT .F.
   CLASS VAR hCmdTrie  SHARED INIT Nil
   CLASS VAR aDocHis   SHARED
   CLASS VAR lDocHis   SHARED INIT .F.
   CLASS VAR cDocHis   SHARED INIT ",odt,doc,docx,pdf,djvu,"
   CLASS VAR cDocHis2, cDocHis3   SHARED
   CLASS VAR nDocMax   SHARED INIT 50
   CLASS VAR aNetInfo  SHARED
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
   CLASS VAR lConsAuto SHARED   INIT .F.
   CLASS VAR cConsOut SHARED   INIT ""
   CLASS VAR nConsMax SHARED   INIT 40000
   CLASS VAR xContextMenu SHARED
   CLASS VAR lConsMode SHARED INIT .F.
   CLASS VAR nLastKey SHARED INIT 0
   CLASS VAR cConsCmd SHARED INIT ""
   CLASS VAR cDateFormat SHARED INIT "dd.mm.yy"
   CLASS VAR hMisc SHARED

   DATA cIOpref       INIT ""
   DATA net_cAddress  INIT ""
   DATA net_cPort     INIT ""
   DATA cIOpref_bak   INIT ""
   DATA net_cAddress_bak INIT ""
   DATA zip_cCurrDir
   DATA x1, y1, x2, y2
   DATA lViewStatus   INIT .T.

   DATA cpPane
   DATA nDispMode     INIT 1
   DATA nSortMode     INIT 1
   DATA nShift        INIT 0
   DATA nCells
   DATA nRows, nWidth
   DATA nPanelMod     INIT 0      // 0 - default, 1 - Search, 2 - Zip, 3 - QView
   DATA nPanelMod_bak
   DATA hUnzip
   DATA pSess
   DATA cCurrPath

   DATA nCurrent     INIT 1

   DATA cPath
   DATA aDir
   DATA aZipFull
   DATA aSelected     INIT {}

   DATA cQVpref

   DATA bOnKey, bDraw, bDrawCell, bDrawHead, bRefresh

   METHOD New( x1, y1, x2, y2, nMode, cPath )
   METHOD ChangeMode( nMode, nSort, nCP )
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
   METHOD onExit()

ENDCLASS

METHOD New( x1, y1, x2, y2, nMode, cPath ) CLASS FilePane

   Aadd( ::aPanes, Self )

   ::x1 := x1
   ::y1 := y1
   ::x2 := x2
   ::y2 := y2
   ::cpPane := ::cp
   IF !Empty( nMode )
      ::nDispMode := nMode
   ENDIF

   ::SetDir( cPath )
   ::nCurrent := Iif( Empty( ::aDir ), 0, 1 )

   RETURN Self

METHOD ChangeMode( nMode, nSort, nCP ) CLASS FilePane

   LOCAL lUpd := .F.

   IF nMode != Nil
      IF ::nDispMode != nMode
         ::nDispMode := nMode
         lUpd := .T.
      ENDIF
   ENDIF
   IF nSort != Nil
      IF ::nSortMode != nSort
         ::nSortMode := nSort
         ::Refresh( .T. )
         lUpd := .T.
      ENDIF
   ENDIF
   IF nCP != Nil
      IF !( ::cpPane == aCpInUse[nCP] )
         ::cpPane := aCpInUse[nCP]
         lUpd := .T.
      ENDIF
   ENDIF
   IF lUpd
      ::Draw()
      ::DrawCell( ,.T. )
   ENDIF

   RETURN Nil

METHOD ChangeDir( cNewPath ) CLASS FilePane

   LOCAL cBuf
   LOCAL aGets := { { ::y1+3,::x1+4, 0, "", 26 } }
   LOCAL nRes

   IF Empty( cNewPath )
      cNewPath := Space( 200 )
      cBuf := Savescreen( ::y1 + 2, ::x1 + 2, ::y1 + 4, ::x1 + 36 )
      Set COLOR TO +GR/B,N/BG
      @ ::y1 + 2, ::x1 + 2, ::y1 + 4, ::x1 + 36 BOX "ÚÄ¿³ÙÄÀ³ "
      @ ::y1 + 2, ::x1 + 6 SAY " " + _I("Set new path")
      KEYBOARD Chr(K_ENTER)

      DO WHILE .T.
         IF ( nRes := edi_READ( aGets, hb_Hash( 0x440003ee, Chr(K_ENTER)) ) ) > 0
            cNewPath := aGets[1,4]
         ENDIF
         IF LastKey() == 13 .AND. Empty( cNewPath )
            IF !Empty( cNewPath := hbc_SelePath( ::y1 + 3, ::x1 + 4 ) )
               EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
      Restscreen( ::y1 + 2, ::x1 + 2, ::y1 + 4, ::x1 + 36, cBuf )
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

   LOCAL nNet, nPos, nPos1, nPos2, cCurrPath, cPref, cAddr, cPort, cDefPort, cLogin := "", cPass := ""
   LOCAL c, l, i, nNetFou := 0, lPassEmpty, lSave := .F., aParam, nPlug

   IF ( nNet := Ascan( aRemote, {|s| cPath = s } ) ) > 0 .OR. ;
      ( nPos := At( ':', cPath ) ) > 2 .AND. ;
      ( nPlug := Ascan2( FilePane():aPlugins, "plug_hbc_"+Left(cPath,nPos-1)+".hrb" ) ) > 0
      IF nNet > 0
         cPref := aRemote[nNet]
         nPos := Len(cPref) + 1
         cDefPort := aRemotePorts[nNet]
      ELSE
         cPref := Left( cPath, nPos )
         nPos ++
         cDefPort := ""
      ENDIF
      IF ( nPos1 := hb_At( ':', cPath, nPos ) ) > 0
         // net:10.8.0.1:...
         cAddr := Substr( cPath, nPos, nPos1-nPos )   // cAddr = 10.8.0.1
         IF ( nPos2 := hb_At( ':', cPath, nPos1+1 ) ) > 0
            // net:10.8.0.1:2941:...
            cPort := Substr( cPath, nPos1+1, nPos2-nPos1-1 )
            cCurrPath := Substr( cPath, nPos2 + 1 )
         ELSEIF IsDigit( Substr( cPath, nPos1+1, 1 ) )
            // net:10.8.0.1:2941...
            IF ( nPos2 := cedi_Strpbrk( "/\", cPath, nPos1 ) ) > 0
               // net:10.8.0.1:2941/...
               cPort := Ltrim( Str( Val(Substr(cPath,nPos1+1)) ) )
               cCurrPath := Substr( cPath, nPos2 )
            ELSE
               // net:10.8.0.1:100abc
               cPort := cDefPort
               cCurrPath := Substr( cPath, nPos1+1 )
            ENDIF
         ELSE
            // net:10.8.0.1:...
            cPort := cDefPort
            cCurrPath := Substr( cPath, nPos1+1 )
         ENDIF
      ELSEIF ( nPos1 := cedi_Strpbrk( "/\", cPath, nPos ) ) > 0
         // net:10.8.0.1/Directory
         cAddr := Substr( cPath, nPos, nPos1-nPos )
         cPort := cDefPort
         cCurrPath := Substr( cPath, nPos1 )
      ELSE
         // net:10.8.0.1 or net:Directory
         cPort := cDefPort
         cAddr := Substr( cPath, nPos )
         cCurrPath := ""
      ENDIF
      NetInfoLoad()
      FOR i := 1 TO Len( FilePane():aNetInfo )
         IF FilePane():aNetInfo[i,1] == cPref .AND. FilePane():aNetInfo[i,2] == cAddr .AND. ;
            FilePane():aNetInfo[i,3] == cPort
            cPass := hb_MD5Decrypt( hb_base64decode(FilePane():aNetInfo[i,4]),"hbedit" )
#ifdef _USE_SSH2
            cLogin := Iif( Len(FilePane():aNetInfo[i]) > 4, ;
               hb_MD5Decrypt( hb_base64decode(FilePane():aNetInfo[i,5]),"hbedit" ), "" )
#endif
            nNetFou := i
            EXIT
         ENDIF
      NEXT
      lPassEmpty := Empty( cPass )
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
         IF cPref == "net:"
            IF Empty( cPass )
               cPass := edi_MsgGet( _I("Password"), ::y1+5, ::x1+10, ::x1+30, .T. )
            ENDIF
            l := netio_Connect( hb_strShrink(cAddr,1), hb_strShrink(cPort,1), 2000, cPass )
#ifdef _USE_SSH2
         ELSEIF cPref == "sftp:"
            ::pSess := hbc_ssh2_Connect( hb_strShrink(cAddr,1), Val(cPort), @cLogin, @cPass, @lSave )
            l := !Empty( ::pSess )
            IF !l .OR. !lSave
               cPass := ""
            ENDIF
#endif
         ELSE
            aParam := { hb_strShrink(cAddr,1), cPort, cCurrPath, cLogin, cPass, lSave }
            l := edi_RunPlugin( Self, FilePane():aPlugins, nPlug, aParam )
            cLogin := aParam[4]
            IF l .AND. aParam[6]
               cPass := aParam[5]
            ENDIF
         ENDIF
         IF l
            IF nNetFou == 0
#ifdef _USE_SSH2
               Aadd( FilePane():aNetInfo, {cPref, hb_strShrink(cAddr,1), hb_strShrink(cPort,1), ;
                  hb_base64encode(hb_MD5Encrypt(cPass,"hbedit")), ;
                  hb_base64encode(hb_MD5Encrypt(cLogin,"hbedit"))} )
#else
               Aadd( FilePane():aNetInfo, {cPref, hb_strShrink(cAddr,1), hb_strShrink(cPort,1), ;
                  hb_base64encode(hb_MD5Encrypt(cPass,"hbedit"))} )
#endif
               NetInfoSave()
            ELSEIF lPassEmpty .AND. !Empty( cPass )
               FilePane():aNetInfo[nNetFou,4] := hb_base64encode(hb_MD5Encrypt(cPass,"hbedit"))
               NetInfoSave()
            ENDIF
            IF Ascan( aUtf8Auto, cPref ) > 0
               ::cpPane := "UTF8"
            ENDIF
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
      ::cIOpref := ::net_cAddress := ::net_cPort := ::cIOpref_bak := ::net_cAddress_bak := ""
#ifndef __PLATFORM__UNIX
      IF !( ':' $ cPath )
         cPath := hb_CurDrive() + ":" + cPath
      ENDIF
#endif
      ::cCurrPath := cPath
   ENDIF
   IF !( Right( ::cCurrPath,1 ) $ "\/" )
      IF Empty( ::cIOpref )
         ::cCurrPath += hb_ps()
      ELSE
         ::cCurrPath += Iif( '\' $ ::cCurrPath, '\', '/' )
      ENDIF
   ENDIF

   RETURN .T.

METHOD SetDir( cPath ) CLASS FilePane

   LOCAL aOldPath := { ::cIOpref, ::net_cAddress, ::net_cPort, ::pSess, ;
      ::bOnKey, ::bDraw, ::bDrawCell, ::bDrawHead, ::bRefresh }
   LOCAL cProc, l, i, pSess

   ::aSelected := {}
   IF Empty( cPath )
      ::aDir := {}
      RETURN Nil
   ENDIF

   IF !( cPath = hb_strShrink( ::cIOpref + ::net_cAddress,1 ) )
      ::bOnKey := ::bDraw := ::bDrawCell := ::bDrawHead := ::bRefresh := Nil
      ::cpPane := ::cp
   ENDIF

   IF ::nPanelMod == 2
      hb_unzipClose( ::hUnzip )
      ::hUnzip := Nil
   ENDIF
   ::nPanelMod := 0

   IF ::ParsePath( cPath )

      IF !Empty( aOldPath[1] )
         l := .F.
         FOR i := 1 TO Len( FilePane():aPanes )
            IF ( FilePane():aPanes[i]:cIOpref == aOldPath[1] ) .AND. ;
               ( FilePane():aPanes[i]:net_cAddress == aOldPath[2] ) .AND. ;
               ( FilePane():aPanes[i]:net_cPort == aOldPath[3] )
               l := .T.
               EXIT
            ENDIF
         NEXT
         IF !l
            pSess := ::pSess
            ::pSess := aOldPath[4]
            IF aOldPath[1] == "net:"
               netio_DisConnect( hb_strShrink( aOldPath[2],1 ), hb_strShrink( aOldPath[3],1 ) )
#ifdef _USE_SSH2
            ELSEIF aOldPath[1] == "sftp:"
               IF !Empty( ::pSess )
                  ssh2_Close( ::pSess )
               ENDIF
#endif
           ELSE
              PlugFunc( Self, aOldPath[1], "CLOSE" )
           ENDIF
           IF ::pSess == pSess
              ::pSess := Nil
           ELSE
              ::pSess := pSess
           ENDIF
         ENDIF
      ENDIF

      ::Refresh()
      IF Empty( ::cIOpref )
         DirChange( ::cCurrPath )
      ENDIF
   ELSE
      ::bOnKey := aOldPath[5]; ::bDraw := aOldPath[6]; ::bDrawCell := aOldPath[7]
      ::bDrawHead := aOldPath[8]; ::bRefresh := aOldPath[9]
   ENDIF

   RETURN Nil

METHOD Refresh( lResort ) CLASS FilePane

   LOCAL aDirTmp, i, l1 := .F., l2 := .F., nPos
   LOCAL cPath := ::cCurrPath

   IF ::nPanelMod == 3
      RETURN .T.
   ENDIF

   IF !Empty( ::bRefresh )
      IF Eval( ::bRefresh, Self ) == -1
         RETURN .F.
      ELSE
         lResort := .T.
      ENDIF
   ENDIF

   IF ::nPanelMod > 0 .AND. Empty( lResort )
      RETURN .F.
   ENDIF
   IF !Empty( lResort )
      aDirTmp := ::aDir
   ELSE
      aDirTmp := hb_vfDirectory( ::cIOpref + ::net_cAddress + ::net_cPort + cPath, "HSD" )
   ENDIF
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
            IF ::nSortMode == 2
               aDirTmp[i,3] += 100000
            ENDIF
         ENDIF
         IF ::nSortMode == 1
            aDirTmp[i,1] := " " + aDirTmp[i,1]
         ELSEIF ::nSortMode == 2
            aDirTmp[i,3] += 365000
         ENDIF
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
         aDirTmp[1] := { "  ..",0,Iif(::nSortMode == 2,Date()+365000+100000,Date()),"","D" }
      ENDIF
   ENDIF
   IF ::nSortMode == 1
      aDirTmp := ASort( aDirTmp,,, {|z,y|Lower(z[1]) < Lower(y[1])} )
   ELSEIF ::nSortMode == 2
      aDirTmp := ASort( aDirTmp,,, {|z,y|z[3] > y[3]} )
   ENDIF
   FOR i := 1 TO Len( aDirTmp )
      IF "D" $ aDirTmp[i,5]
         IF aDirTmp[i,1] == "  .."
            aDirTmp[i,1] := ".."
         ELSEIF Left( aDirTmp[i,1],1 ) == " "
            aDirTmp[i,1] := Substr( aDirTmp[i,1],2 )
         ENDIF
         IF ::nSortMode == 2
            aDirTmp[i,3] -= Iif( aDirTmp[i,1] = "..", 365000+100000, 365000 )
         ENDIF
      ENDIF
   NEXT

   ::aDir := aDirTmp

   RETURN .T.

METHOD Draw() CLASS FilePane

   LOCAL i, cTemp

   IF ::nPanelMod == 3
      RETURN Nil
   ENDIF

   IF !Empty( ::bDraw )
      RETURN Eval( ::bDraw, Self )
   ENDIF

   SetColor( ::cClrBox )
   @ ::y1, ::x1, ::y2, ::x2 BOX "ÚÄ¿³ÙÄÀ³ "

   ::nRows := ::y2 - ::y1 - Iif( ::lViewStatus, 3, 1 )
   IF ::nDispMode == 1 .OR. ::nDispMode == 3 .OR. ::nDispMode == 4
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
      cTemp := Iif( (i:=Len(TEdit():aWindows))==1, " ", Ltrim( Str(i-1) ) )
      @ ::y1, ::x2-Len(cTemp)-2 SAY "[" + cTemp + "]" COLOR ::cClrSelCurr
   ENDIF

   RETURN Nil

METHOD DrawCell( nCell, lCurr ) CLASS FilePane

   LOCAL arr, nRow, x1 := ::x1 + 1, cText, nWidth, cDop, lSel, nLen
   LOCAL cDate, dDate, cSize, cClrFil := ::cClrFil, cExt, lUtf8, nAttr := 0

   IF ::nPanelMod == 3
      RETURN Nil
   ENDIF
   IF !Empty( ::bDrawCell )
      RETURN Eval( ::bDrawCell, Self, nCell, lCurr )
   ENDIF

   IF ::nCurrent == 0
      @ ::y2 - 1, ::x1 + 1 SAY _I("Not available")
      RETURN Nil
   ENDIF

   IF !( ::cp == ::cpPane )
      hb_cdpSelect( ::cpPane )
   ENDIF
   lUtf8 := ( ::cpPane == "UTF8" )
   nRow := nCell := Iif( nCell==Nil,::nCurrent,nCell )
   arr := ::aDir[nCell+::nShift]
   lSel := ( Ascan( ::aSelected, nCell+::nShift ) > 0 )

   nWidth := ::nWidth
   IF ::nDispMode == 2 .AND. nRow > ::nRows
      x1 += ( ::nWidth+1 )
      IF ( nRow := (nRow % ::nRows) ) == 0
         nRow := ::nRows
      ENDIF
   ELSEIF ::nDispMode == 3
      nWidth -= Len(::cDateFormat) - 2
   ELSEIF ::nDispMode == 4
      nWidth -= Len(::cDateFormat) - 8
   ENDIF
   cText := Trim( arr[1] )
   cExt := hb_fnameExt( cText )
   IF 'H' $ arr[5]
      cClrFil := ::cClrHid
   ELSEIF 'D' $ arr[5]
      cClrFil := ::cClrDir
   ELSE
      IF Ascan( aExtZip, {|s| s==cExt} ) > 0
         cClrFil := ::cClrZip
      ELSE
#ifdef __PLATFORM__UNIX
         IF IsFileExec( arr )
            cClrFil := ::cClrExe
         ENDIF
#else
         IF Ascan( aExtExe, {|s| s==cExt} ) > 0
            cClrFil := ::cClrExe
         ENDIF
#endif
      ENDIF
   ENDIF
   SetColor( Iif( lCurr, Iif( lSel, ::cClrSelCurr, ::cClrCurr ), ;
      Iif( lSel, ::cClrSel, cClrFil ) ) )
   IF ( nLen := cp_Len( lUtf8, cText ) ) > nWidth
      cText := cp_Left( lUtf8, cText, nWidth-1 ) + '>'
   ENDIF
   @ ::y1 + nRow, x1 SAY cText
   IF nLen < nWidth
      @ ::y1 + nRow, x1+nLen SAY Space( nWidth-nLen )
   ENDIF
   IF ::nDispMode == 3 .OR. ::nDispMode == 4
      IF ::nDispMode == 4
         IF 'D' $ arr[5] .AND. arr[2] == 0
            cSize := " <dir>"
         ELSE
            cSize := Iif( arr[2]<=999999, PAdl(Ltrim(Str(arr[2])),6), ;
               Iif( arr[2]<10238976,PAdl(Left(Ltrim(Str(Round(arr[2]/1024,2))),5)+"K",6), ;
               Iif( arr[2]<10484711424, PAdl(Left(Ltrim(Str(Round(arr[2]/1048576,2))),5)+"M",6), ;
               PAdl(Left(Ltrim(Str(Round(arr[2]/1073741824,2))),5)+"G",6) ) ) )
         ENDIF
         @ ::y1 + nRow, ::x2 - Len(FilePane():cDateFormat) - 7 SAY cSize
      ENDIF
      @ ::y1 + nRow, ::x2 - Len(FilePane():cDateFormat) SAY hb_Dtoc( arr[3] )
   ENDIF

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
      cDop := Iif( 'D' $ arr[5] .AND. arr[2]==0, "<dir>", Ltrim(Str(arr[2])) ) + " " + hb_Dtoc(arr[3]) + " " + Left(arr[4],5)
      nWidth := ::x2 - ::x1 - 3 - Len(cDop)
      cText := NameShortcut( Trim( ::aDir[nCell+::nShift,1] ), nWidth, "~", lUtf8 )
      @ ::y2 - 1, ::x1 + 1 SAY cText
      @ ::y2 - 1, ::x1 + 1 + Len(cText) SAY Space( ::x2 - ::x1 - 1 - Len(cText) )
      @ ::y2 - 1, ::x2 - Len(cDop) SAY cDop
      x1 := ::x1 + Int( (::x2-::x1-15)/2 )
      IF Empty( ::aSelected )
         @ ::y2 - 2, x1, ::y2 - 2, x1 + 15 BOX "ÚÄ¿³ÙÄÀ³ " COLOR ::cClrBox
      ELSE
         @ ::y2 - 2, x1 SAY _I("Selected") + ": " + PAdl(Ltrim(Str(Len(::aSelected))),4) COLOR ::cClrSel
      ENDIF
   ENDIF
   IF !( ::cp == ::cpPane )
      hb_cdpSelect( ::cp )
   ENDIF

   RETURN Nil

METHOD DrawHead( lCurr ) CLASS FilePane

   LOCAL cPath

   IF ::nPanelMod == 3
      RETURN Nil
   ENDIF

   IF !Empty( ::bDrawHead )
      RETURN Eval( ::bDrawHead, Self )
   ENDIF

   IF !( ::cp == ::cpPane )
      hb_cdpSelect( ::cpPane )
   ENDIF
   SetColor( Iif( lCurr, ::cClrCurr, ::cClrFil ) )
   IF ::nPanelMod == 0
      cPath := NameShortcut( ::cIOpref + ::net_cAddress + ::cCurrPath, ::x2-::x1-6, '~', oHbc:lUtf8 )
      @ ::y1, ::x1 + Int((::x2-::x1-1)/2) - Int( Len(cPath)/2 ) SAY cPath
   ELSEIF ::nPanelMod == 1
      cPath := _I("Search results")
      @ ::y1, ::x1 + Int((::x2-::x1-1)/2) - Int( Len(cPath)/2 ) SAY cPath
   ELSEIF ::nPanelMod == 2
      cPath := ::net_cAddress + ":" + ::zip_cCurrDir
      @ ::y1, ::x1 + Int((::x2-::x1-1)/2) - Int( Len(cPath)/2 ) SAY NameShortcut( cPath, ::x2-::x1-3,, oHbc:lUtf8 )
   ENDIF
   IF !( ::cp == ::cpPane )
      hb_cdpSelect( ::cp )
   ENDIF

   RETURN Nil

METHOD PaneMenu() CLASS FilePane

   LOCAL cBuf, nChoic := 1, cTemp, bufsc, o
   LOCAL cSep := "---"
   LOCAL aMenu := { {_I("Pane mode"),,,"Ctrl-P"}, {_I("Change dir"),,,"D"}, ;
      {_I("History"),,,"H"}, {_I("Find file"),,,"Ctrl-F7"}, ;
      {_I("Plugins"),,,"F11"}, {_I("Apps"),,,"Ctrl-F12"}, {_I("Buffers"),,,"F12"}, {_I("Refresh"),,,"Ctrl-R"}, ;
      {_I("Console"),,,"Ctrl-O"}, {cSep,,}, {_I("Edit")+ " hbc.ini",,}, {cSep,,}, {_I("Exit"),,} }

   IF !Empty( FilePane():cConsOut )
      aMenu := hb_AIns( aMenu, Len(aMenu)-3, {_I("Stdout window"),,,"Ctrl-W"}, .T. )
   ENDIF
   nChoic := FMenu( oHbc, aMenu, ::y1+1, ::x1+1, ::y1+Len(aMenu)+2,, ::aClrMenu[1], ::aClrMenu[2] )
   IF nChoic == 1
      hbc_PaneOpt()
   ELSEIF nChoic == 2
      ::ChangeDir()
   ELSEIF nChoic == 3
      hbc_HistMnu()
   ELSEIF nChoic == 4
      hbc_Search()
   ELSEIF nChoic == 5
      Plugins( Self )
   ELSEIF nChoic == 6
      AppList( Self )
   ELSEIF nChoic == 7
      mnu_Buffers( oHbc, {::y1+1,::x1+1} )
   ELSEIF nChoic == 8
      ::Refresh()
      IF ::nCurrent + ::nShift > Len( ::aDir )
         ::nShift := Max( 0, Len( ::aDir ) ) - ::nCells
         ::nCurrent := Len( ::aDir ) - ::nShift
      ENDIF
      ::RedrawAll()
   ELSEIF nChoic == 9
      hbc_Console()
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
   LOCAL aDir

   aDir := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift]
   IF Empty( aDir )
      RETURN Nil
   ENDIF

   IF !( aDir[1] == ".." )
      IF oPaneTo:nPanelMod != 1
         Aadd( aMenu, { _I("Copy"),,1,"F5" } )
      ENDIF
      IF oPaneCurr:nPanelMod == 0 .AND. oPaneTo:nPanelMod != 1
         Aadd( aMenu, { _I("Rename"),,2,"F6" } )
      ENDIF
      IF oPaneCurr:nPanelMod == 0
         Aadd( aMenu, { _I("Delete"),,3,"F8" } )
      ENDIF
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
         hbc_FCopyFile( aDir )
      ELSE
         hbc_FCopySele()
      ENDIF
   ELSEIF aMenu[nChoic,3] == 2
      IF Empty( oPaneCurr:aSelected )
         hbc_FRename( .F. )
      ELSE
         hbc_FRenameSele()
      ENDIF
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

METHOD onExit() CLASS FilePane

   LOCAL cHisDir := hb_DirBase(), s := "", i, j, nLen
#ifdef __PLATFORM__UNIX
   LOCAL sLine
#endif

   IF FilePane():lCmdHis .OR. FilePane():lDocHis
      IF !Empty( FilePane():aCmdHis )
         s += "[COMMANDS]" + Chr(13) + Chr(10)
         nLen := Len(FilePane():aCmdHis)
         FOR i := Max( 1,nLen-200 ) TO nLen
            s += "c" + PAdl(Ltrim(Str(i)),3,'0') + "=" + FilePane():aCmdHis[i] + Chr(13) + Chr(10)
         NEXT
      ENDIF
      FOR j := 1 TO 3
         IF !Empty( FilePane():aDocHis[j] )
            s += Chr(13) + Chr(10) + "[DOCUMENTS" + Iif( j>1, "-" + Str(j,1), "" ) + "]" + Chr(13) + Chr(10)
            nLen := Len(FilePane():aDocHis[j])
            FOR i := 1 TO Min( nLen,FilePane():nDocMax )
               s += "d" + PAdl(Ltrim(Str(i)),3,'0') + "=" + FilePane():aDocHis[j,i,1] + ";" + ;
                  FilePane():aDocHis[j,i,2] + Chr(13) + Chr(10)
            NEXT
         ENDIF
      NEXT
   ENDIF
   IF !Empty( s )
#ifdef __PLATFORM__UNIX
      IF hb_dirExists( sLine := ( hb_getenv( "HOME" ) + "/hbedit" ) )
         cHisDir := sLine + "/"
      ENDIF
#endif
      hb_MemoWrit( cHisDir + "hbc.his", s )

      FilePane():lCmdHis := FilePane():lDocHis := .F.
      IF !( FilePane():hCmdTrie == Nil )
         trie_Close( FilePane():hCmdTrie )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION FAsk_Abort( cFile, nSize, nCopied )

   LOCAL y1 := 6, x1 := Int( (FilePane():vx2-FilePane():vx1-50)/2 ), y2 := y1+9, x2 := x1+50, nRes
   LOCAL cBuf, oldc := SetColor( TEdit():cColorWR + "," + TEdit():cColorWR )
   LOCAL aGets := { ;
      {y1+1,x1+2, 11, hb_fnameNameExt(cFile) }, ;
      {y1+2,x1+2, 11, Ltrim(Str(Int(nCopied*100/nSize))) + "% " + _I("copied")}, ;
      {y1+8,x1+16, 2, _I("[Continue]"), 10,TEdit():cColorWR,TEdit():cColorWB,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+8,x1+28, 2,_I("[Abort]"), 7,TEdit():cColorWR,TEdit():cColorWB,{||__KeyBoard(Chr(K_ESC))}} }

   cBuf := Savescreen( y1, x1, y2, x2 )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+3, x1 SAY "Ã"
   @ y1+3, x2 SAY "´"
   @ y1+3, x1+1 TO y1+3, x2-1

   nRes := edi_READ( aGets )

   Restscreen( y1, x1, y2, x2, cBuf )
   SetColor( oldc )

   RETURN ( nRes > 0 .AND. nRes < Len(aGets) )

FUNCTION FAsk_Overwrite( n, cFile, nSouSize, dSouDate, nDestSize, dDestDate )

   LOCAL nRes
   LOCAL y1 := 6, x1 := Int( (FilePane():vx2-FilePane():vx1-50)/2 ), y2 := y1+9, x2 := x1+50
   LOCAL cBuf, oldc := SetColor( TEdit():cColorWR + "," + TEdit():cColorWR )
   LOCAL aGets := { ;
      {y1+1,x1+2, 11, cFile + " " + _I("exists already! Overwrite it?")}, ;
      {y1+3,x1+2, 11, _I("New")+":"+PAdl(Ltrim(Str(nSouSize)),12)+" "+hb_ttoc(dSouDate)}, ;
      {y1+4,x1+2, 11, _I("Existing")+":"+PAdl(Ltrim(Str(nDestSize)),10)+" "+hb_ttoc(dDestDate)}, ;
      {y1+6,x1+3, 1, .F., 1, TEdit():cColorWR,TEdit():cColorWB }, {y1+6,x1+2, 11, "[ ] "+_I("Don's ask anymore")}, ;
      {y1+8,x1+16, 2, _I("[Yes]"), 5,TEdit():cColorWR,TEdit():cColorWB,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+8,x1+28, 2,_I("[No]"), 4,TEdit():cColorWR,TEdit():cColorWB,{||__KeyBoard(Chr(K_ESC))}} }
   STATIC lNoAsk := .F., lRes := .F.

   IF n == 0  // One time
      aGets[4,3] := aGets[5,3] := -1
   ELSEIF n == 1  // First time
      lNoAsk := .F.
   ELSEIF lNoAsk
      RETURN lRes
   ENDIF

   cBuf := Savescreen( y1, x1, y2, x2 )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+2, x1 SAY "Ã"
   @ y1+2, x2 SAY "´"
   @ y1+2, x1+1 TO y1+2, x2-1
   @ y1+5, x1 SAY "Ã"
   @ y1+5, x2 SAY "´"
   @ y1+5, x1+1 TO y1+5, x2-1
   @ y1+7, x1 SAY "Ã"
   @ y1+7, x2 SAY "´"
   @ y1+7, x1+1 TO y1+7, x2-1

   lRes := ( ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets) )
   lNoAsk := aGets[4,4]

   Restscreen( y1, x1, y2, x2, cBuf )
   SetColor( oldc )

   RETURN lRes

FUNCTION FAsk_Copy( cTitle, cRes )

   LOCAL cScBuf, oldc, nRes
   LOCAL aGets := { ;
      {06,12,11,cTitle}, ;
      {07,12,0,cRes,56,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {09,25,2,_I("[Ok]"),4,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {09,50,2,_I("[Cancel]"),10,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }

   cScBuf := Savescreen( 05, 10, 10, 70 )
   oldc := SetColor( oHbc:cColorSel+","+oHbc:cColorSel+",,"+oHbc:cColorGet+","+oHbc:cColorSel )
   hb_cdpSelect( "RU866" )
   @ 05, 10, 10, 70 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 08, 10 SAY "Ã"
   @ 08, 70 SAY "´"
   @ 08, 11 TO 08, 69
   hb_cdpSelect( oHbc:cp )

   nRes := edi_READ( aGets )
   SetColor( oldc )
   Restscreen( 05, 10, 10, 70, cScBuf )

   RETURN Iif( nRes > 0 .AND. nRes < Len(aGets), AllTrim(aGets[2,4]), Nil )

FUNCTION FTransl( cName, cpFrom, cpTo )

   IF cpFrom == Nil
      cpFrom := oPaneCurr:cpPane
      cpTo := oPaneCurr:cp
   ENDIF
   RETURN Iif( cpFrom == cpTo, cName, hb_Translate( cName, cpFrom, cpTo ) )

/*
 *  FCopy() returns 0 if it is Ok, 1 - if !overwrite, 2 - if error
 */
STATIC FUNCTION FCopy( aDir, cFileTo, nFirst )

   LOCAL i, cTemp, cIOpref, handle, nRes := 0, hZip

   IF hb_vfExists( cFileTo )
      hb_vfTimeGet( cFileTo, @i )
      IF !FAsk_Overwrite( nFirst, FTransl(hb_fnameNameExt(cFileTo)), aDir[2], aDir[3], hb_vfSize(cFileTo), i )
         RETURN 1
      ENDIF
   ENDIF

   //edi_Writelog( "1: " + aDir[1] )
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
            edi_Alert( _I("Something goes wrong...") )
            RETURN 2
         ENDIF
      ENDIF
   ELSEIF Left( cFileTo,4 ) == "zip:"
      IF ( i := hb_At( ':', cFileTo, 5 ) ) == 0 .OR. Substr( cFileTo, 5, i-4 ) != oPaneTo:net_cAddress
         nRes := 2
      ENDIF
      IF !Empty( hZip := hb_zipOpen( oPaneTo:cCurrPath + oPaneTo:net_cAddress, HB_ZIP_OPEN_ADDINZIP ) )
         IF hb_zipStoreFile( hZip, oPaneCurr:cCurrPath + aDir[1], Substr( cFileTo, i+1 ) ) != 0
            nRes := 2
         ENDIF
         hb_zipClose( hZip )
      ELSE
         nRes := 2
      ENDIF
   ELSEIF oPaneCurr:nPanelMod == 1
      IF ( nRes := hb_vfCopyFile( oPaneCurr:cIOpref_bak + oPaneCurr:net_cAddress_bak + ;
            oPaneCurr:cCurrPath + aDir[1], cFileTo ) ) != 0
         nRes := Iif( nRes == -3, 3, 2 )
      ENDIF
   ELSE
      //edi_Writelog( "2: " + oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + aDir[1] )
      IF ( nRes := hb_vfCopyFile( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
            oPaneCurr:cCurrPath + aDir[1], cFileTo ) ) != 0
         //edi_Writelog( "Err" )
         nRes := Iif( nRes == -3, 3, 2 )
      ENDIF
   ENDIF
   IF nRes == 2
      edi_Alert( _I("Error copying") + " " + aDir[1] )
   ENDIF

   RETURN nRes

STATIC FUNCTION hbc_FCopyFile( aDir, cFileTo, nStart )

   LOCAL lSilent, l1 := .F., cTemp, lDir, nCopied := 0, aWnd
   LOCAL cFileName := aDir[1], lRes := .F., nRes
   LOCAL bCopy := {|s,arr|
      LOCAL nLen := Len(oPaneCurr:cIOpref+oPaneCurr:net_cAddress+oPaneCurr:net_cPort+oPaneCurr:cCurrPath) + 1
      LOCAL nRes
      IF "D" $ arr[5]
         IF !( s == "." .OR. s == ".." )
            IF !hb_vfDirExists( cFileTo + Substr(s,nLen) ) .AND. hb_vfDirMake( cFileTo + Substr(s,nLen) ) != 0
               RETURN ( lRes := .F. )
            ENDIF
         ENDIF
      ELSE
         nStart ++
         IF ( nRes := FCopy( {Substr(s,nLen),arr[2],arr[3]}, cFileTo + ;
            FTransl( Substr(s,nLen),oPaneCurr:cpPane,oPaneTo:cpPane ), nStart ) ) == 0
            nCopied ++
            hbc_Wndout( aWnd, FTransl( Substr( s,nLen ) ) )
         ELSEIF nRes == 1
            l1 := .T.
         ENDIF
         RETURN ( lRes := (nRes<2) )
      ENDIF
      RETURN .T.
   }

   IF oPaneTo:nPanelMod == 1
      edi_Alert( _I(cNotPerm) )
      RETURN 2
   ENDIF

   lDir := ( 'D' $ aDir[5] )
   lSilent := !Empty( cFileTo )
   IF Empty(nStart); nStart := 0; ENDIF

   IF !lSilent
      IF oPaneTo:nPanelMod == 2
         cFileTo := oPaneTo:cIOpref + oPaneTo:net_cAddress + ":" + oPaneTo:zip_cCurrDir + FTransl(cFileName)
      ELSEIF oPaneCurr:nPanelMod == 1
         cFileTo := oPaneTo:cIOpref + oPaneTo:net_cAddress + oPaneTo:net_cPort + oPaneTo:cCurrPath + ;
            hb_fnameNameExt( FTransl(cFileName) )
      ELSE
         cFileTo := oPaneTo:cIOpref + oPaneTo:net_cAddress + oPaneTo:net_cPort + oPaneTo:cCurrPath + ;
            Iif( lDir, "", FTransl(cFileName) )
      ENDIF
   ENDIF
   IF lSilent .OR. !Empty( cFileTo := FAsk_Copy( ;
      _I("Copy") + " " + NameShortcut( FTransl(cFileName), 48,, oHbc:lUtf8 ) + " " + _I("to"), cFileTo ) )
      IF ( cTemp := Left( cFileTo,4 ) ) == "sea:" .OR. ( cTemp == "zip:" .AND. lDir )
         edi_Alert( _I(cNotPerm) )
         RETURN 2
      ENDIF
      IF !lSilent
         cFileTo := FTransl( cFileTo, oPaneCurr:cp, oPaneCurr:cpPane )
      ENDIF
      cFileTo := FTransl( cFileTo, oPaneCurr:cpPane, oPaneTo:cpPane )
      IF !Empty( oPaneCurr:cIOpref ) .AND. ;
         ( nRes := PlugFunc( oPaneCurr, oPaneCurr:cIOpref, "COPYFROM", ;
         {oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
         oPaneCurr:cCurrPath + cFileName, cFileTo, lDir, nStart, aDir[2],aDir[3]} ) ) != Nil
         RETURN nRes
      ENDIF
      IF !Empty( oPaneTo:cIOpref ) .AND. ;
         ( nRes := PlugFunc( oPaneTo, oPaneTo:cIOpref, "COPYTO", ;
         {oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
         oPaneCurr:cCurrPath + cFileName, cFileTo, lDir, nStart, aDir[2]} ) ) != Nil
         RETURN nRes
      ENDIF
      IF lDir
         IF !( Right(cFileTo,1) $ "/\" )
            cFileTo += hb_ps()
         ENDIF
         IF !hb_vfDirExists( cFileTo  + cFileName ) .AND. hb_vfDirMake( cFileTo  + cFileName ) != 0
            edi_Alert( _I("Error creating")+" " + cFileName )
            RETURN 2
         ENDIF
         aWnd := hbc_Wndinit( 05, 20, 12, 60,, _I("Copy") )
         lRes := .T.
         DirEval( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
            oPaneCurr:cCurrPath + cFileName + hb_ps(), "*", .T., bCopy, .T. )
         IF lSilent
            KEYBOARD Chr(K_SPACE)
         ENDIF
         hbc_Wndclose( aWnd, _I("Done")+", " + Ltrim(Str(nCopied)) + " "+_I("files copied") )
         IF !lSilent
            oPaneTo:Refresh()
            oPaneCurr:Refresh()
            oPaneCurr:RedrawAll()
         ENDIF
      ELSE
         IF !lSilent .AND. nStart == 0
            aWnd := hbc_Wndinit( 05, Int(MaxCol()/2-10), 06, Int(MaxCol()/2+10),, _I("Wait") )
         ENDIF
         IF ( nRes := FCopy( aDir, cFileTo, nStart ) ) < 2
            lRes := .T.
            IF Left( cFileTo,4 ) == "zip:"
               // Refresh zip panel
               hb_unzipClose( oPaneTo:hUnzip )
               oPaneTo:aZipFull := zipRead( oPaneTo:hUnzip := ;
                  hb_unzipOpen(oPaneTo:cCurrPath+oPaneTo:net_cAddress) )
               zipDirRefresh( oPaneTo, oPaneTo:zip_cCurrDir )
            ELSEIF !lSilent
               oPaneTo:Refresh()
            ENDIF
            IF !lSilent
               oPaneCurr:Refresh()
               oPaneCurr:RedrawAll()
            ENDIF
         ELSE
            IF !lSilent .AND. nStart == 0
               hbc_Wndclose( aWnd )
            ENDIF
            IF nRes == 3
               RETURN 3
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN Iif( !lRes, 2, Iif( l1, 1, 0 ) )

STATIC FUNCTION hbc_FCopySele()

   LOCAL cFileName, cDirTo, cFileTo, i, aWnd, nSch := 0, aDir, nRes

   IF oPaneTo:nPanelMod == 1
      RETURN edi_Alert( _I(cNotPerm) )
   ENDIF

   cDirTo := oPaneTo:cIOpref + oPaneTo:net_cAddress + oPaneTo:net_cPort + oPaneTo:cCurrPath
   IF !Empty( cDirTo := FAsk_Copy( ;
      _I("Copy seleted files to"), cDirTo ) )
      aWnd := hbc_Wndinit( 05, 20, 12, 60,, _I("Copy") )

      FOR i := 1 TO Len( oPaneCurr:aSelected )
         aDir := oPaneCurr:aDir[oPaneCurr:aSelected[i]]
         cFileName := aDir[1]
         cFileTo := cDirTo + cFileName

         hbc_Wndout( aWnd, FTransl( cFileName ) )
         IF ( nRes := hbc_FCopyFile( aDir, cFileTo, i ) ) == 0
            nSch ++
         ENDIF
         IF nRes == 3 .OR. Inkey() == 27
            EXIT
         ENDIF
      NEXT

      hbc_Wndclose( aWnd, Iif( nRes==3, _I("Aborted")+", ",_I("Done")+", " ) + Ltrim(Str(nSch)) + " " + _I("files copied") )
      oPaneCurr:aSelected := {}
      IF nSch > 0
         oPaneTo:Refresh()
      ENDIF
      oPaneCurr:RedrawAll()
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_FRename( lRename )

   LOCAL aDir := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift]
   LOCAL cFileName := aDir[1], i
   LOCAL cNewName, lDir, lCopyDel := .F., lRes := .F.

   lDir := ( 'D' $ aDir[5] )
   IF oPaneCurr:nPanelMod > 0
      RETURN edi_Alert( _I(cNotPerm) )
   ENDIF
   IF oPaneTo:nPanelMod > 0
      lRename := .T.
   ENDIF

   cNewName := Iif( Empty(lRename), oPaneTo:cIOpref + oPaneTo:net_cAddress + ;
      oPaneTo:net_cPort + oPaneTo:cCurrPath + Iif(lDir,"",cFileName), cFileName )

   IF !Empty( cNewName := FAsk_Copy( ;
      _I("Rename") + " " + NameShortcut( FTransl(cFileName), 48,, oHbc:lUtf8 ) + " " + _I("to"), FTransl(cNewName) ) )
      cNewName := FTransl( cNewName, oPaneCurr:cp, oPaneCurr:cpPane )
      IF ':' $ cNewName .OR. '\' $ cNewName .OR. '/' $ cNewName
         lRename := .F.
         IF !Empty( oPaneCurr:cIOpref ) .OR. At( ':', cNewName ) > 2
            lCopyDel := .T.
         ENDIF
         IF lDir .AND. !( Right(cNewName,1) $ "/\" )
            cNewName += hb_ps()
         ENDIF
      ELSE
         cNewName := oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cNewName
         lRename := .T.
      ENDIF
      IF lRename .AND. ( ( lDir .AND. hb_vfDirExists( cNewName ) ) .OR. ;
         ( !lDir .AND. hb_vfExists( cNewName ) ) )
         edi_Alert( _I("Such a file exists already!") )
         RETURN Nil
      ENDIF
      IF !lCopyDel
         IF !lRename .AND. hb_vfExists( cNewName )
            hb_vfTimeGet( cNewName, @i )
            IF !FAsk_Overwrite( 0, FTransl(hb_fnameNameExt(cNewName)), aDir[2], aDir[3], hb_vfSize(cNewName), i )
               RETURN Nil
            ENDIF
         ENDIF
         IF hb_vfRename( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
            oPaneCurr:cCurrPath + cFileName, cNewName+Iif(lDir.AND.!lRename,cFileName,"") ) == 0
            lRes := .T.
         ENDIF
      ENDIF
      IF !lRes .AND. ( lCopyDel .OR. !lRename )
         IF hbc_FCopyFile( aDir, cNewName ) == 0
            lRes := hbc_FDelete( .T. )
         ENDIF
      ENDIF
      oPaneCurr:Refresh()
      IF !lRename
         oPaneTo:Refresh()
      ENDIF
      IF oPaneCurr:nCurrent + oPaneCurr:nShift > Len( oPaneCurr:aDir )
         IF oPaneCurr:nCurrent == 1
            oPaneCurr:nShift --
         ELSE
            oPaneCurr:nCurrent --
         ENDIF
      ENDIF
      oPaneCurr:RedrawAll()
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_FRenameSele()

   LOCAL aWnd, i, aDir, cFileName, nSch := 0, nRes
   LOCAL cMoveTo := oPaneTo:cIOpref + oPaneTo:net_cAddress + oPaneTo:net_cPort + oPaneTo:cCurrPath, cFileTo

   IF oPaneCurr:nPanelMod > 0
      RETURN edi_Alert( _I(cNotPerm) )
   ENDIF

   IF !Empty( cMoveTo := FAsk_Copy( ;
      _I("Move selected files to"), cMoveTo ) )
      aWnd := hbc_Wndinit( 05, 20, 12, 60,, _I("Move") )

      FOR i := 1 TO Len( oPaneCurr:aSelected )
         aDir := oPaneCurr:aDir[oPaneCurr:aSelected[i]]
         cFileName := aDir[1]
         cFileTo := cMoveTo + cFileName

         hbc_Wndout( aWnd, FTransl( cFileName ) )

         IF ( nRes := hbc_FCopyFile( aDir, cFileTo, i ) ) == 0
            IF hbc_FDelete( .T., cFileName, .F. )
               nSch ++
            ENDIF
         ENDIF
         IF nRes == 3 .OR. Inkey() == 27
            EXIT
         ENDIF
      NEXT

      hbc_Wndclose( aWnd, Iif( nRes==3, _I("Aborted")+", ",_I("Done")+", " ) + ;
         Ltrim(Str(nSch)) + " " +_I("files moved") )
      oPaneCurr:aSelected := {}
      IF nSch > 0
         oPaneCurr:Refresh()
         IF oPaneCurr:nCurrent + oPaneCurr:nShift > Len( oPaneCurr:aDir )
            oPaneCurr:nCurrent := Iif( Empty(oPaneCurr:aDir), 0, 1 )
            oPaneCurr:nShift := 0
         ENDIF
         oPaneTo:Refresh()
      ENDIF
      oPaneCurr:RedrawAll()
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_FDelete( lSilent, cFileName, lDir )

   LOCAL lRes := .F., nRes
   LOCAL cInitDir, aDirs, i, aWnd, nStart := 0
   LOCAL bDel := {|s,arr|
      LOCAL nLen := Len(oPaneCurr:cIOpref+oPaneCurr:net_cAddress+oPaneCurr:net_cPort+oPaneCurr:cCurrPath) + 1
      IF "D" $ arr[5]
         IF !( s == "." .OR. s == ".." )
            Aadd( aDirs, s )
         ENDIF
      ELSE
         nStart ++
         hbc_Wndout( aWnd, FTransl( Substr( s,nLen ) ) )
         IF hb_vfErase( s ) != 0
            lRes := .F.
            edi_Alert( _I("Error deleting") + " " + Substr( s,nLen ) )
            RETURN .F.
         ENDIF
      ENDIF
      RETURN .T.
   }

   IF oPaneCurr:nPanelMod > 0
      RETURN edi_Alert( _I(cNotPerm) )
   ENDIF

   IF Empty( cFileName )
      cFileName := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
      lDir := ('D' $ oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,5])
   ENDIF
   lSilent := !Empty( lSilent )

   IF lSilent .OR. edi_Alert( _I("Really delete")+" " + FTransl(cFileName) + "?", _I("Yes"), _I("No") ) == 1
      IF !Empty( oPaneCurr:cIOpref ) .AND. ;
         ( nRes := PlugFunc( oPaneCurr, oPaneCurr:cIOpref, "DELETE", {oPaneCurr:cIOpref + ;
         oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cFileName, lDir} ) ) != Nil
         lRes := ( nRes == 0 )
      ENDIF
      //edi_Alert( valtype(nRes) )
      IF nRes == Nil
         IF lDir
            lRes := .T.
            aWnd := hbc_Wndinit( 05, 20, 12, 60,, _I("Delete") )
            cInitDir := oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cFileName
            aDirs := { cInitDir }
            DirEval( cInitDir, "*", .T., bDel, .T. )
            aDirs := ASort( aDirs,,, {|s1,s2|Len(s1)>Len(s2)} )
            FOR i := 1 TO Len(aDirs)
               IF hb_vfDirRemove( aDirs[i] ) != 0
                  lRes := .F.
                  edi_Alert( _I("Error deleting") + " " + aDirs[i] )
                  EXIT
               ENDIF
            NEXT
            IF lSilent
               KEYBOARD Chr(K_SPACE)
            ENDIF
            hbc_Wndclose( aWnd, _I("Done") + ", " + Ltrim(Str(nStart)) + " " + _I("files deleted") )
         ELSE
            IF hb_vfErase( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + cFileName ) == 0
               lRes := .T.
            ENDIF
         ENDIF
      ENDIF
      IF lRes
         IF !lSilent
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
         ENDIF
      ELSE
         edi_Alert( _I("Error deleting") + " " + cFileName )
      ENDIF
   ENDIF

   RETURN lRes

STATIC FUNCTION hbc_FDeleteSele()

   LOCAL cFileName, i

   IF oPaneCurr:nPanelMod > 0
      RETURN edi_Alert( _I(cNotPerm) )
   ENDIF

   IF edi_Alert( _I("Really delete") + " " + Ltrim(Str(Len(oPaneCurr:aSelected))) + ;
      " " + _I("files")+"?", _I("Yes"), _I("No") ) == 1
      FOR i := 1 TO Len( oPaneCurr:aSelected )
         cFileName := oPaneCurr:aDir[oPaneCurr:aSelected[i],1]
         hbc_FDelete( .T., cFileName, .F. )
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

   LOCAL cBuf, cNewName, nRes

   IF oPaneCurr:nPanelMod > 0
      edi_Alert( _I(cNotPerm) )
      RETURN Nil
   ENDIF

   cBuf := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )

   Set COLOR TO N/W
   @ 05, 10, 10, 70 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 06, 12 SAY _I("Create new directory")
   Set COLOR TO N/BG,B/BG
   @ 09, 24 SAY "[Enter - Ok]  [ESC - Cancel]"
   cNewName := Space( 200 )
   @ 7, 12 GET cNewName PICTURE "@S56"
   READ
   Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )
   IF LastKey() != 27 .AND. !Empty( cNewName := AllTrim(cNewName) )
      IF !Empty( oPaneCurr:cIOpref )
         nRes := PlugFunc( oPaneCurr, oPaneCurr:cIOpref, "MKDIR", ;
         {oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
         oPaneCurr:cCurrPath + cNewName} )
      ENDIF
      IF nRes == 0 .OR. ( nRes == Nil .AND. hb_vfDirMake( oPaneCurr:cIOpref + ;
         oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath + Trim(cNewName) ) == 0 )
         oPaneCurr:Refresh()
         oPaneCurr:Draw()
         oPaneCurr:DrawCell( ,.T.)
      ELSE
         edi_Alert( _I("Error creating") + " " + cNewName )
      ENDIF
      RETURN Nil
   ENDIF

   Restscreen( 0, 0, nScreenH-1, nScreenW-1, cBuf )

   RETURN Nil

STATIC FUNCTION hbc_FCalcSize()

   LOCAL nSize := 0

   DirEval( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
      oPaneCurr:cCurrPath + oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1], ;
      "*", .T., {|s,a|nSize := nSize + a[2]} )
   oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,2] := nSize
   oPaneCurr:DrawCell( ,.T. )

   RETURN Nil

STATIC FUNCTION hbc_SelePath( y1, x1 )

   LOCAL i, nHeight, nWidth := 16, cRes := "", aPaths, nLen
   LOCAL nwMax := FilePane():vx2 - 1 - x1, l := Empty(oPaneTo:cIOpref) .AND. !(oPaneCurr:cCurrPath == oPaneTo:cCurrPath)
   LOCAL cDrives := "", j, iType

#ifndef __PLATFORM__UNIX
   LOCAL aDriveTypes := { "", "", "Removable", "Fixed", "Remote", "Cdrom", "Ram" }
   cDrives := cedi_GetDrives()
#endif

   IF !Empty( FilePane():aDefPaths ) .OR. !Empty( cDrives ) .OR. l
      j := Iif( Empty(cDrives), 0, Len( cDrives ) + 1 )
      aPaths := Array( nLen := ( Len(FilePane():aDefPaths) + Iif(l,1,0) + Iif(j>0,j,0) ) )
      nHeight := Min( nLen,22 )
      FOR i := 1 TO nLen
         IF l .AND. i == nLen
            aPaths[i] := { oPaneTo:cCurrPath,,, }
         ELSEIF i < j
#ifndef __PLATFORM__UNIX
            iType := cedi_GetDriveType( Substr(cDrives,i,1)+":" )
            aPaths[i] := { Substr(cDrives,i,1)+":",,, Iif( iType+1>Len(aDriveTypes),"",aDriveTypes[iType+1] ) }
#endif
         ELSEIF i == j
            aPaths[i] := { "---",,, }
         ELSE
            aPaths[i] := { FilePane():aDefPaths[i-j,1],,, FilePane():aDefPaths[i-j,2] }
         ENDIF
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
#ifdef __PLATFORM__UNIX
      cDir := NameShortcut(hb_Translate(hb_fnameDir(TEdit():aEditHis[i,1]),"UTF8"), 48,'~',oHbc:lUtf8 )
      IF Ascan( aMenu, {|a|a[1]==cDir} ) == 0
         AAdd( aMenu, { cDir,Nil,i} )
      ENDIF
#else
      cDir := Lower( NameShortcut(hb_Translate(hb_fnameDir(TEdit():aEditHis[i,1]),"UTF8"), 48,'~',oHbc:lUtf8 ) )
      IF '/' $ cDir
         cDir := StrTran( cDir, '/', '\' )
      ENDIF
      IF Ascan( aMenu, {|a|Lower(a[1])==cDir} ) == 0
         AAdd( aMenu, { cDir,Nil,i} )
      ENDIF
#endif
   NEXT

   IF !Empty( aMenu )
      i := FMenu( oHbc, aMenu, oPaneCurr:y1+1, oPaneCurr:x1+1,,, ;
         FilePane():aClrMenu[1], FilePane():aClrMenu[2],, .T. )
      IF i > 0
         oPaneCurr:ChangeDir( hb_fnameDir( TEdit():aEditHis[aMenu[i,3],1] ) )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_Doclist( n )

   LOCAL i, aMenu, cpOld, cFile, cNewDir
   LOCAL bKeys := {|nKeyExt,nLine|
      LOCAL nKey := hb_keyStd( nKeyExt )
      IF nKey == K_F2
         cNewDir := hb_fnameDir( Filepane():aDocHis[n,nLine,2] )
         RETURN .F.
      ENDIF
      RETURN Nil
   }
   STATIC lChecked := .F.

   IF !lChecked
      FOR i := 1 TO Len( Filepane():aDocHis[n] )
         cFile := Iif( Filepane():aDocHis[n,i,1] == "UTF8", Filepane():aDocHis[n,i,2], ;
            hb_Utf8ToStr( Filepane():aDocHis[n,i,2], Filepane():aDocHis[n,i,1] ) )
         cpOld := hb_cdpSelect( Filepane():aDocHis[n,i,1] )
         IF !File( cFile )
            hb_ADel( Filepane():aDocHis[n], i, .T. )
            i --
         ENDIF
         hb_cdpSelect( cpOld )
      NEXT
      lChecked := .T.
   ENDIF

   aMenu := Array( Len(Filepane():aDocHis[n]) )
   FOR i := 1 TO Len( Filepane():aDocHis[n] )
      aMenu[i] := NameShortcut( hb_fnameNameExt(Filepane():aDocHis[n,i,2]), 48,'~', .T. )
   NEXT

   IF !Empty( aMenu )
      cpOld := hb_cdpSelect( "UTF8" )
      i := FMenu( oHbc, aMenu, oPaneCurr:y1+1, oPaneCurr:x1+1,,, ;
         FilePane():aClrMenu[1], FilePane():aClrMenu[2],, .T.,,, bKeys )
      hb_cdpSelect( cpOld )
      IF !Empty( cNewDir )
         oPaneCurr:ChangeDir( cNewDir )
      ELSEIF i > 0
#ifdef __PLATFORM__UNIX
#ifdef GTHWG
         hwg_shellExecute( "file://" + Filepane():aDocHis[n,i,2] )
#endif
#else
         cedi_shellExecute( Filepane():aDocHis[n,i,2], .T. )
#endif
         AddDocHis( n, Filepane():aDocHis[n,i,2], Filepane():aDocHis[n,i,1], .T. )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_CmdHis()

   LOCAL i

   IF !Empty( FilePane():aCmdHis )
      KEYBOARD Chr(K_END)
      i := FMenu( oHbc, FilePane():aCmdHis, oPaneCurr:vy1+2, oPaneCurr:vx1+10, ;
         oPaneCurr:vy2-2, oPaneCurr:vx2-10, oPaneCurr:aClrMenu[1], oPaneCurr:aClrMenu[2] )
      IF i > 0
         hbc_Console( FilePane():aCmdHis[i] )
      ENDIF
   ENDIF
   RETURN Nil

STATIC FUNCTION hbc_PaneOpt()

   LOCAL cm := _I("Mode")
   LOCAL aMenu1 := { cm + " 1 " + Iif(oPaneCurr:nDispMode==1,"x"," "), ;
      cm + " 2 " + Iif(oPaneCurr:nDispMode==2,"x"," "), cm + " 3 " + Iif(oPaneCurr:nDispMode==3,"x"," "), ;
      cm + " 4 " + Iif(oPaneCurr:nDispMode==4,"x"," "), ;
      "---", _I("Sort by name")+" " + Iif(oPaneCurr:nSortMode==1,"x"," "), ;
      _I("Sort by date")+" " + Iif(oPaneCurr:nSortMode==2,"x"," "), "---" }
   LOCAL nChoic

   FOR nChoic := 1 TO Len( aCpInUse )
      AAdd( aMenu1, aCpInUse[nChoic] + Iif( aCpInUse[nChoic]==oPaneCurr:cpPane, "   x", "" ) )
   NEXT
   nChoic := FMenu( oHbc, aMenu1, oPaneCurr:y1+2, oPaneCurr:x1+14, ;
      oPaneCurr:y1+Len(aMenu1)+3, oPaneCurr:x1+38, oPaneCurr:aClrMenu[1], oPaneCurr:aClrMenu[2] )
   IF nChoic > 0
      oPaneCurr:ChangeMode( Iif(nChoic<=4,nChoic,Nil), Iif(nChoic>4.AND.nChoic<8,nChoic-5,Nil), ;
         Iif(nChoic>=9,nChoic-8,Nil) )
   ENDIF

   RETURN .T.

STATIC FUNCTION hbc_HistMnu()

   LOCAL aMenu := { {_I("Editing"),,}, {_I("Commands"),,,"Ctrl-F8"}  }, nChoic, i

   FOR i := 1 TO 3
      IF !Empty( Filepane():aDocHis[i] )
         AAdd( aMenu, { _I("Documents")+Iif(i==1,"","-"+Str(i,1)),,i } )
      ENDIF
   NEXT

   nChoic := FMenu( oHbc, aMenu, oPaneCurr:y1+2, oPaneCurr:x1+14, ;
      oPaneCurr:y1+Len(aMenu)+3,, oPaneCurr:aClrMenu[1], oPaneCurr:aClrMenu[2] )
   IF nChoic == 1
      hbc_Dirlist()
   ELSEIF nChoic == 2
      hbc_CmdHis()
   ELSEIF nChoic == 3
      hbc_Doclist(1)
   ELSEIF nChoic == 4 .OR. nChoic == 5
      hbc_Doclist( aMenu[nChoic,3] )
   ENDIF

   RETURN .T.

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

FUNCTION DirEval( cInitDir, cMask, lRecur, bCode, lEvalDir )

   LOCAL i, res, nCount := 0, arlen, aFiles, nPos1 := 1, nPos2, cMsk, lDo := .T.

   IF !( Right( cInitDir,1 ) $ "/\" ); cInitDir += hb_ps(); ENDIF
   cMask := Iif( cMask==Nil, "*", Upper(cMask) )

   DO WHILE lDo
      IF ( nPos2 := hb_At( ";", cMask, nPos1 ) ) > 0
         cMsk := Substr( cMask, nPos1, nPos2-nPos1 )
         nPos1 := nPos2 + 1
      ELSE
         cMsk := Substr( cMask, nPos1 )
         lDo := .F.
      ENDIF
      aFiles := hb_vfDirectory( cInitDir + "*", "HSD" )
      //edi_writelog( cInitDir + "*" + " msk:" + cMask )
      arlen := Len( aFiles )
      FOR i := 1 TO arlen
         IF "D" $ aFiles[ i,5 ] .AND. !("L" $ aFiles[ i,5 ])
            IF "." != aFiles[ i,1 ] .AND. ".." != aFiles[ i,1 ] .AND. lRecur
               IF !Empty( lEvalDir )
                  nCount ++
                  res := Eval( bCode, cInitDir + aFiles[i,1], aFiles[i] )
                  IF ValType( res ) == "L" .AND. !res
                     Return nCount
                  ENDIF
               ENDIF
               nCount += DirEval( cInitDir + aFiles[i,1] + hb_OsPathSeparator(), cMask, .T., bCode, lEvalDir )
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
      {08,30,0,"*.*",33,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {09,30,0,"",33,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {09,65,2,"[^]",3,oHbc:cColorSel,oHbc:cColorMenu,{||mnu_SeaHist(oHbc,aGets[2])}}, ;
      {11,18,1,.F.,1}, ;
      {11,40,1,.F.,1}, ;
      {12,18,1,.F.,1}, ;
      {12,40,1,Empty(lSele),1}, ;
      {11,58,1,!Empty(lSele),1}, ;
      {13,18,1,.F.,1}, ;
      {13,30,0,hb_dtoc(Date(),"dd.mm.yyyy"),10,oHbc:cColorMenu,oHbc:cColorMenu,"99.99.9999"}, ;
      {13,42,0,"00:00",5,oHbc:cColorMenu,oHbc:cColorMenu,"99:99"}, ;
      {15,25,2,_I("[Search]"),,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {15,50,2,_I("[Cancel]"),,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cSearch, lCase, lWord, lRegex, lRecu, lSelect, dDateS, d, d1, cTimeS
   LOCAL cs_utf8, cCmd, cRes, aRes, aDir := { { "..","","","","D" } }, lFound := .F., n, cPath
   LOCAL b1 := {|s|
      IF !aGets[9,4] .OR. ( hb_vfTimeGet( s, @d ) .AND. ( ( d1 := hb_ttod(d) ) > dDateS .OR. ;
         ( d1 == dDateS .AND. Substr( hb_ttos(d),9,4 ) >= cTimeS ) ) )
         Aadd( aDir, Ascan2( oPaneCurr:aDir,hb_fnameNameExt(s) ) )
      ENDIF
      RETURN .T.
   }
   LOCAL b2 := {|s|
      IF !aGets[9,4] .OR. ( hb_vfTimeGet( s, @d ) .AND. ( ( d1 := hb_ttod(d) ) > dDateS .OR. ;
         ( d1 == dDateS .AND. Substr( hb_ttos(d),9,4 ) >= cTimeS ) ) )
         Aadd( aDir,{ Substr( s,Len(oPaneCurr:cCurrPath)+1 ),"","","","" } )
      ENDIF
      //edi_Writelog( s + " " + Dtos(hb_ttod(d)), Dtos(dDateS) )
      RETURN .T.
   }

   IF oPaneCurr:nPanelMod > 0
      RETURN edi_Alert( _I(cNotPerm) )
   ENDIF

   cScBuf := Savescreen( 07, 15, 16, 70 )
   oldc := SetColor( oHbc:cColorSel+","+oHbc:cColorSel+",,"+oHbc:cColorGet+","+oHbc:cColorSel )
   hb_cdpSelect( "RU866" )
   @ 07, 15, 16, 70 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 14, 15 SAY "Ã"
   @ 14, 70 SAY "´"
   @ 14, 16 TO 14, 69
   hb_cdpSelect( oHbc:cp )

   @ 08, 17 SAY _I("File mask")
   @ 09, 17 SAY _I("Search for")
   @ 11, 17 SAY "[ ] " + _I("Case sensitive")
   @ 11, 39 SAY "[ ] " + _I("Regular expr.")
   @ 12, 17 SAY "[ ] " + _I("Whole word")
   @ 12, 39 SAY "[ ] " + _I("Recursive")
   @ 11, 57 SAY "[ ] " + _I("Select")
   @ 13, 17 SAY "[ ] " + _I("Date") + " >="
   //@ 13, 39 SAY "(yyyymmdd)"
   //@ 13, 56 SAY "(hh:mm)"

   IF Empty( lSele ) .AND. !Empty( TEdit():aSeaHis )
      aGets[2,4] := hb_Translate( TEdit():aSeaHis[1], "UTF8" )
      aGets[4,4] := lCase_Sea
      aGets[5,4] := lRegex_Sea
   ENDIF

   DO WHILE ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cSearch := aGets[2,4]
      lCase := aGets[4,4]
      lRegex := aGets[5,4]
      lWord := aGets[6,4]
      lRecu := aGets[7,4]
      lSelect := Iif( lRecu, .F., aGets[8,4] )
      IF aGets[9,4]
         dDateS := Stod( Substr(aGets[10,4],7,4) + Substr(aGets[10,4],4,2) + Left(aGets[10,4],2) )
         IF Empty( dDateS )
            edi_Alert( _I("Wrong date") )
            LOOP
         ENDIF
         cTimeS := Left( aGets[11,4],2 ) + Substr( aGets[11,4],4,2 )
      ENDIF
      aDir := Iif( lSelect, {}, { { "..","","","","D" } } )
      IF Empty( cSearch )
         cPath := oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + oPaneCurr:cCurrPath
         IF lSelect
            DirEval( cPath, aGets[1,4], lRecu, b1 )
         ELSE
            DirEval( cPath, aGets[1,4], lRecu, b2 )
         ENDIF
      ELSEIF oPaneCurr:cIOpref == "net:"
         edi_Alert( _I(cNotPerm) )
      ELSE
         IF lRecu
            cCmd := 'grep ' + Iif(!lCase,'-i ','') + Iif(lWord,'-w ','') + Iif(lRegex,'-P ','') + ;
               '-R -l --include "' + aGets[1,4] + '" "' + cSearch + '"'
         ELSE
            cCmd := 'grep ' + Iif(!lCase,'-i ','') + Iif(lWord,'-w ','') + Iif(lRegex,'-P ','') + ;
               '-l ' + '"' + cSearch + '" ' + aGets[1,4]
         ENDIF
         cedi_RunConsoleApp( cCmd,, @cRes )
         IF !Empty( cRes )
            aRes := hb_ATokens( cRes, Iif( Chr(13) $ cRes, Chr(13)+Chr(10), Chr(10) ) )
            FOR i := 1 TO Len( aRes )
               IF !Empty( aRes[i] )
                  IF !( !aGets[9,4] .OR. ( hb_vfTimeGet( aRes[i], @d ) .AND. ( ( d1 := hb_ttod(d) ) > dDateS .OR. ;
                     ( d1 == dDateS .AND. Substr( hb_ttos(d), 9,4 ) >= cTimeS ) ) ) )
                     LOOP
                  ENDIF
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
      EXIT
   ENDDO

   Restscreen( 07, 15, 16, 70, cScBuf )
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
      edi_Alert( _I("Nothing found") )
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_Zip()

   LOCAL aDir := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift]
   LOCAL aGets := { ;
      {06,12,11,_I("Create archive")}, ;
      {07,12,0,"",56,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {09,25,2,_I("[Ok]"),,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {09,50,2,_I("[Cancel]"),,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cScBuf, oldc, nRes
   LOCAL cPath, hZip, cFile, i, aDirToZip, aWnd, nSch := 0, cFileInZip, aDirs := {}, arr, j, cTemp

   IF oPaneCurr:nPanelMod == 2 .OR. Left( oPaneCurr:cIOpref,4 ) == "net:"
      RETURN edi_Alert( _I(cNotPerm) )
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
         aWnd := hbc_Wndinit( 05, 20, 16, 60,, "Zip" )
         FOR i := 1 TO Len( oPaneCurr:aSelected )
            cFile := oPaneCurr:aDir[oPaneCurr:aSelected[i],1]
            hbc_Wndout( aWnd, cFile )
            IF hb_zipStoreFile( hZip, cFile, cFile ) == 0
               nSch++
            ENDIF
         NEXT
         hbc_Wndclose( aWnd, _I("Done")+", " + Ltrim(Str(nSch)) + " "+_I("files archived") )
         oPaneCurr:aSelected := {}
      ELSEIF 'D' $ aDir[5]
         aWnd := hbc_Wndinit( 05, 20, 16, 60,, "Zip" )
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
               hbc_Wndout( aWnd, cFileInZip )
               IF hb_zipStoreFile( hZip, cFileInZip, cFileInZip ) == 0
                  nSch++
               ENDIF
            ENDIF
         NEXT
         hbc_Wndclose( aWnd, _I("Done")+", " + Ltrim(Str(nSch)) + " "+_I("files archived") )
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
   LOCAL nErr, hUnzip, cFile, dDate, cTime, nSize, lCrypted, dd, aWnd, nSch := 0
   LOCAL aGets := { ;
      {06,12,11,_I("Extract files to")}, ;
      {07,12,0,"",56,oHbc:cColorMenu,oHbc:cColorMenu}, ;
      {09,25,2,_I("[Ok]"),4,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {09,50,2,_I("[Cancel]"),10,oHbc:cColorSel,oHbc:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
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
            aWnd := hbc_Wndinit( 05, 20, 16, 60,, "Unzip" )
            nErr := hb_unzipFileFirst( hUnzip )
            DO WHILE nErr == 0
               hb_unzipFileInfo( hUnzip, @cFile, @dDate, @cTime, , , , @nSize, @lCrypted )
               hbc_Wndout( aWnd, cFile )
               IF Right( cFile,1 ) $ "/\"
                  IF !hb_vfDirExists( cPath+cFile )
                     hb_vfDirMake( cPath+cFile )
                  ENDIF
               ELSE
                  IF !hb_vfExists( cPath+cFile ) .OR. ( Valtype( hb_vfTimeGet(cPath+cFile,@dd) ) == 'L';
                     .AND. FAsk_Overwrite( ++nFirst, hb_fnameNameExt(cFile), nSize, ;
                        hb_DToT(dDate,cTime), hb_vfSize(cPath+cFile), dd ) )
                     IF hb_unzipExtractCurrentFile( hUnzip, cPath+cFile ) == 0
                        nSch++
                     ENDIF
                  ENDIF
               ENDIF
               nErr := hb_unzipFileNext( hUnzip )
            ENDDO
         ENDIF
         hb_unzipClose( hUnzip )
         hbc_Wndclose( aWnd, _I("Done")+", " + Ltrim(Str(nSch)) + " " + _I("files unzipped") )
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

   LOCAL cExt, cTemp

   cExt := hb_fnameExt( cName )
   cName := hb_fnameName( cName )
   cExt := hb_fnameExt( cName ) + cExt
/*
   DO WHILE !Empty( cTemp := hb_fnameExt( cName ) )
      cExt := cTemp + cExt
      cName := hb_fnameName( cName )
   ENDDO
*/
   RETURN cExt

FUNCTION vfWrit_Net( cFileName, cText )

   LOCAL handle := hb_vfOpen( cFileName, FO_WRITE+FO_CREAT+FO_TRUNC )

   hb_vfWrite( handle, cText )
   hb_vfClose( handle )

   RETURN Nil

FUNCTION vfWrit_Zip( cFileName, cText )

   edi_Alert( _I(cNotPerm) )

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
#ifdef __PLATFORM__UNIX
         cedi_RunApp( FilePane():aAppList[i,1] )
#else
         cedi_shellExecute( FilePane():aAppList[i,1] )
#endif
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
   LOCAL nAttr, nAttrNew, t

   IF !hb_vfAttrGet( Iif( oPaneCurr:cIOpref == "sea:", "", ;
      oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort ) + ;
      oPaneCurr:cCurrPath + arr[1], @nAttr )
      edi_Alert( _I("Can't read attributes") )
      RETURN Nil
   ENDIF
   nAttrNew := nAttr
   hb_vfTimeGet( Iif( oPaneCurr:cIOpref == "sea:", "", ;
      oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort ) + ;
      oPaneCurr:cCurrPath + arr[1], @t )

#ifdef __PLATFORM__UNIX
   Aadd( aGets, { y1+13,x1+3, 1, .F., 1 } )
#endif
   Aadd( aGets, { y1+18,x1+7, 2, _I("[Ok]"),, "N/W","N/W",{||__KeyBoard(Chr(K_ENTER))} } )
   Aadd( aGets, { y1+18,x1+17, 2, _I("[Cancel]"),, "N/W","N/W",{||__KeyBoard(Chr(K_ESC))} } )

   cScBuf := Savescreen( y1, x1, y1+19, x1+34 )
   oldc := SetColor( "N/W"+","+"N/W"+",,"+"N+/W"+","+"N/W" )

   cp := hb_cdpSelect( "RU866" )
   @ y1,x1, y1+19, x1+34 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+17, x1 SAY "Ã"
   @ y1+17, x1+34 SAY "´"
   @ y1+17, x1+1 TO y1+17, x1+33
   hb_cdpSelect( cp )

   @ y1,x1+10 SAY " " + _I("Attributes") + " "
   @ y1+1,x1+2 SAY PAdc( _I("for") + " " + arr[1], 28 )
#ifdef __PLATFORM__UNIX
   @ y1+2, x1+2 SAY "[ ] " + _I("Set user ID on execution")
   @ y1+3, x1+2 SAY "[ ] " + _I("Set group ID on execution")
   @ y1+4, x1+2 SAY "[ ] " + _I("Sticky bit")
   @ y1+5, x1+2 SAY "[ ] " + _I("Read by owner")
   @ y1+6, x1+2 SAY "[ ] " + _I("Write by owner")
   @ y1+7, x1+2 SAY "[ ] " + _I("Execute/search by owner")
   @ y1+8, x1+2 SAY "[ ] " + _I("Read by group")
   @ y1+9, x1+2 SAY "[ ] " + _I("Write by group")
   @ y1+10, x1+2 SAY "[ ] " + _I("Execute/search by group")
   @ y1+11, x1+2 SAY "[ ] " + _I("Read by others")
   @ y1+12, x1+2 SAY "[ ] " + _I("Write by others")
   @ y1+13, x1+2 SAY "[ ] " + _I("Execute/search by others")
   @ y1+15, x1+4 SAY _I("Owner") + ": " + cedi_chown( arr[1] ) + "/" + cedi_chgrp( arr[1] )
#else
   @ y1+2, x1+2 SAY "[ ] " + _I("Readonly")
   @ y1+3, x1+2 SAY "[ ] " + _I("Hidden")
   @ y1+4, x1+2 SAY "[ ] " + _I("System")
   @ y1+5, x1+2 SAY "[ ] " + _I("Archive")
   @ y1+6, x1+2 SAY "[ ] " + _I("Compressed")
   @ y1+7, x1+2 SAY "[ ] " + _I("Encrypted")
   @ y1+8, x1+2 SAY "[ ] " + _I("Not indexed")
   @ y1+9, x1+2 SAY "[ ] " + _I("Sparse")
   @ y1+10, x1+2 SAY "[ ] " + _I("Reparse (Link)")
   @ y1+11, x1+2 SAY "[ ] " + _I("Temporary")
   @ y1+12, x1+2 SAY "[ ] " + _I("Offline")
#endif

   FOR i := 1 TO Len( aDefs )
      arrD[i] := aGets[i,4] := ( hb_bitAnd(nAttr,aDefs[i]) > 0 )
   NEXT
   IF !Empty( t )
      @ y1+16, x1+2 SAY hb_ttoc( t )
   ENDIF
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
            edi_Alert( _I("Can't set attributes") )
         ENDIF
      ENDIF
   ENDIF

   SetColor( oldc )
   Restscreen( y1, x1, y1+19, x1+34, cScBuf )

   RETURN Nil

STATIC FUNCTION ShowStdout()

   LOCAL cName := "$Stdout", i, oNew

   IF ( i := Ascan( TEdit():aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oHbc, i )
      RETURN Nil
   ENDIF

   oNew := TEdit():New( FilePane():cConsOut, cName, oHbc:aRectFull[1], oHbc:aRectFull[2], oHbc:aRectFull[3], oHbc:aRectFull[4] )
   oHbc:lShow := .F.
   TEdit():nCurr := Len( TEdit():aWindows )
   //oNew:lReadOnly := .T.
   edi_Move( oNew, 71 )

   RETURN Nil

STATIC FUNCTION hbc_Cons_Auto( cmd )

   LOCAL cTmp
#ifdef __PLATFORM__UNIX
   LOCAL b := {|a|
      RETURN IsFileExec( a )
   }
#else
   LOCAL b := {|a|
      RETURN ( hb_Ascan( aExtExe,hb_fnameExt(a[1]),,,.T. ) > 0 )
   }
#endif
   IF Left( cmd,2 ) == "./" .AND. !( ' ' $ cmd ) .AND. ;
      !( cTmp := hbc_DoAuC( oHbc, cmd, oPaneCurr:aDir, b )) == cmd
      RETURN "./" + cTmp
   ELSE
      IF ( cTmp := hbc_DoAuC( oHbc, cmd ) ) == cmd
         IF ' ' $ cmd .AND. !(Right(cmd,1)==' ') .AND. !( cTmp := hbc_DoAuC( oHbc, cmd, oPaneCurr:aDir )) == cmd
            RETURN cTmp
         ENDIF
      ELSE
         RETURN cTmp
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION hbc_Cons_Menu( cmd )

   LOCAL cSep := "---"
   LOCAL aMenu := { {_I("Commands history"),,,"Ctrl-F8"}, {_I("Stdout window"),,,"Ctrl-W"}, ;
      {_I("Set autocompletion") + " "+Iif(FilePane():lConsAuto,"Off","On"),,}, {cSep,,}, ;
      {_I("Close"),,,"Ctrl-O,Esc"} }
   LOCAL n, nChoic, lShell := .F.

#ifdef _USE_SSH2
   IF oPaneCurr:cIOpref == "sftp:"
      aMenu := hb_AIns( aMenu, Len(aMenu)-1, {_I("Shell"),,}, .T. )
      lShell := .T.
   ENDIF
#endif
   nChoic := FMenu( oHbc, aMenu, oPaneCurr:y1+5, Int(MaxCol()/2-16),,, ;
      oPaneCurr:aClrMenu[1], oPaneCurr:aClrMenu[2] )
   IF nChoic == 1
      IF !Empty( FilePane():aCmdHis )
         KEYBOARD Chr(K_END)
         n := FMenu( oHbc, FilePane():aCmdHis, oPaneCurr:vy1+2, oPaneCurr:vx1+10, ;
            oPaneCurr:vy2-2, oPaneCurr:vx2-10, oPaneCurr:aClrMenu[1], oPaneCurr:aClrMenu[2] )
         IF n > 0
            RETURN FilePane():aCmdHis[n]
         ENDIF
      ENDIF
   ELSEIF nChoic == 2
      KEYBOARD Chr(K_CTRL_W)
   ELSEIF nChoic == 3
      FilePane():lConsAuto := !FilePane():lConsAuto
#ifdef _USE_SSH2
   ELSEIF lShell .AND. nChoic == 4
      KEYBOARD Chr(K_ENTER)
      RETURN "shell"
#endif
   ELSEIF nChoic == Len( aMenu )
      FilePane():nLastKey := 0
      FilePane():cConsCmd := cmd
      KEYBOARD Chr(K_ENTER)
      RETURN "exit"
   ENDIF

   RETURN Nil

FUNCTION hbc_Console( xCommand, lSetOnly )

   LOCAL bufsc, clr, i, nHis := 0, cCommand := "", nCommand := 0, s
   LOCAL xRes, bOldError
   LOCAL bKeys := {|nKeyExt,cmd,nColInit|
      LOCAL nKey := hb_keyStd( nKeyExt ), cTmp, n
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
         FilePane():nLastKey := 0
         FilePane():cConsCmd := cmd
         KEYBOARD Chr(K_ENTER)
         RETURN "exit"
      ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_CTRL_TAB
         FilePane():nLastKey := nKeyExt
         FilePane():cConsCmd := cmd
         KEYBOARD Chr(K_ENTER)
         RETURN "exit"
      ELSEIF nKey == K_CTRL_W
         FilePane():nLastKey := K_CTRL_W
         FilePane():cConsCmd := cmd
         KEYBOARD Chr(K_ENTER)
         RETURN "exit"
      ELSEIF nKey == K_F1
         FilePane():nLastKey := K_F1
         FilePane():cConsCmd := cmd
         KEYBOARD Chr(K_ENTER)
         RETURN "exit"
      ELSEIF nKey == K_TAB
         RETURN hbc_Cons_Auto( cmd )
      ELSEIF nKey == K_CTRL_F8
         IF !Empty( FilePane():aCmdHis )
            KEYBOARD Chr(K_END)
            n := FMenu( oHbc, FilePane():aCmdHis, oPaneCurr:vy1+2, oPaneCurr:vx1+10, ;
               oPaneCurr:vy2-2, oPaneCurr:vx2-10, oPaneCurr:aClrMenu[1], oPaneCurr:aClrMenu[2] )
            IF n > 0
               RETURN FilePane():aCmdHis[n]
            ENDIF
         ENDIF
      ELSEIF nKey == K_F9 .OR. nKey == K_RBUTTONDOWN
         RETURN hbc_Cons_Menu( cmd )

      ELSEIF nKey >= 32 .AND. nKey <= 250 .AND. FilePane():lConsAuto .AND. ;
         ( n := Len(cmd) ) > 1 .AND. n + nColInit - 1 == Col()
         DevPos( Row(), nColInit )
         DevOut( cmd )
         RETURN hbc_Cons_Auto( cmd )
      ENDIF
      RETURN Nil
   }

   FilePane():lConsMode := .T.
   bufsc := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )
   clr := SetColor( "+W/N" )

   SET CURSOR ON

   IF Empty( cOutBuff )
      CLEAR SCREEN
      @ Int(MaxRow()/2-1), Int(MaxCol()/2-10) SAY "F1 - " + _I("Help")
      @ Int(MaxRow()/2), Int(MaxCol()/2-10) SAY "F9,Right Click - " +_I("menu")
   ELSE
      RestScreen( 0, 0, nScreenH-1, nScreenW-1, cOutBuff )
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
         IF Empty( lSetOnly )
            KEYBOARD Chr( K_ENTER )
         ENDIF
      ELSEIF !Empty( FilePane():cConsCmd )
         cCommand := FilePane():cConsCmd
      ENDIF
      cCommand := GetLine( Iif( oPaneCurr:nPanelMod>0,oPaneCurr:cIOpref+">", ;
          Iif( !Empty(oPaneCurr:cIOpref), oPaneCurr:cIOpref,NameShortcut(Curdir(),28,'~' ) ) + ">" ), cCommand, bKeys )
      IF !Empty( cCommand )
         IF cCommand == "exit"
            IF FilePane():nLastKey == 0
               FilePane():lConsMode := .F.
            ENDIF
            EXIT
         ENDIF
         IF ( i := Ascan( FilePane():aCmdHis, {|s|s == cCommand} ) ) > 0
            FilePane():aCmdHis := hb_ADel( FilePane():aCmdHis, i, .T. )
         ELSE
            trie_Add( FilePane():hCmdTrie, cCommand )
         ENDIF
         Aadd( FilePane():aCmdHis, cCommand )
         FilePane():lCmdHis := .T.
         IF Left( cCommand,3 ) == "cd "
            DirChange( AllTrim( Substr(cCommand,4) ) )
            cCommand := ""
            LOOP
         ENDIF
         i := 0
         DO WHILE ( i := hb_At( '%', cCommand,i+1 ) ) > 0
            s := Substr( cCommand,i+1,1 )
            IF s == 'p'
               s := oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
            ELSEIF s == 'f'
               s := oPaneCurr:cCurrPath + oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1]
            ELSEIF s == 'n'
               s := hb_fnameName( oPaneCurr:aDir[oPaneCurr:nCurrent + oPaneCurr:nShift,1] )
            ELSEIF s == 'd'
               s := oPaneCurr:cCurrPath
            ELSEIF s == 'o'
               s := oPaneTo:cCurrPath
            ELSEIF s == 'm'
               s := edi_MsgGet( _I("Input text"), oPaneCurr:y1+10, oPaneCurr:x1+6, oPaneCurr:x2-6 )
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
         IF Left( cCommand,1 ) == "="
            bOldError := Errorblock( {|e| MacroError( e ) } )
            BEGIN SEQUENCE
               xRes := &( Substr( cCommand,2 ) )
            END SEQUENCE
            Errorblock( bOldError )
            SetColor( "W/N" )
            ? hb_ValToExp( xRes )
            Add2Consout( cCommand + Chr(13)+Chr(10) )
            Add2Consout( hb_ValToExp( xRes ) )
         ELSEIF oPaneCurr:nPanelMod > 0
            edi_Alert( "Pane is in "+Iif(oPaneCurr:nPanelMod==1,"search","zip") + " mode" )
         ELSEIF oPaneCurr:cIOpref == "net:"
            edi_Alert( _I("Can't run commands in net environment") )
#ifdef _USE_SSH2
         ELSEIF oPaneCurr:cIOpref == "sftp:"
            IF !Empty( oPaneCurr:pSess )
               ssh2_Channel_Open( oPaneCurr:pSess )
               IF ssh2_LastRes( oPaneCurr:pSess ) == 0
                  Cons_ssh2_My( oPaneCurr:pSess, cCommand )
               ELSE
                  ? "OpenChannel failed"
               ENDIF
               ssh2_Channel_Close( oPaneCurr:pSess )
            ENDIF
#endif
         ELSE
            Cons_My( cCommand )
         ENDIF
         cCommand := ""
      ELSEIF Lastkey() == K_ESC
         FilePane():lConsMode := .F.
         FilePane():cConsCmd := ""
         EXIT
      ENDIF
   ENDDO
   cOutBuff := Savescreen( 0, 0, nScreenH-1, nScreenW-1 )
   SetColor( clr )
   RestScreen( 0, 0, nScreenH-1, nScreenW-1, bufsc )
   SET CURSOR OFF
   IF Empty( oPaneCurr:cIOpref )
      s := hb_ps() + Curdir() + hb_ps()
      IF !( s == oPaneCurr:cCurrPath )
         oPaneCurr:Setdir( s )
      ELSE
         oPaneCurr:Refresh()
         IF oPaneCurr:nCurrent + oPaneCurr:nShift > Len( oPaneCurr:aDir )
            oPaneCurr:nCurrent := Iif( Empty(oPaneCurr:aDir), 0, 1 )
            oPaneCurr:nShift := 0
         ENDIF
      ENDIF
   ENDIF
   FilePane():RedrawAll()

   RETURN Nil

STATIC FUNCTION GetLine( cMsg, cRes, bKeys )

   LOCAL nRow := Row(), nColInit, nCol, nKeyExt, nKey, clr, s

   DevOut( Iif( cMsg==Nil, "", cMsg ) )
   nColInit := Col()
   IF cRes == Nil
      cRes := ""
   ELSEIF !Empty( cRes )
      DevOut( cRes )
   ENDIF
   DO WHILE .T.
      nKeyExt := Inkey( 0, HB_INKEY_ALL + HB_INKEY_EXT )
      IF nKeyExt == Nil
         RETURN Nil
      ENDIF
      IF ((nKey := hb_keyStd( nKeyExt )) >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE
         LOOP
      ENDIF
      IF nKey == K_ENTER
         RETURN cRes
      ELSEIF nKey == K_ESC
         IF Empty( cRes )
            RETURN ""
         ELSE
            DevPos( nRow, nColInit )
            DevOut( Space( Len(cRes) ) )
            cRes := ""
            DevPos( nRow, nColInit )
         ENDIF
      ELSE
         IF nKey == K_CTRL_R .OR. ;
            ( MaxCol() - nColInit < cp_Len( oHbc:lUtf8, cRes ) .AND. nKey >= 32 .AND. nKey <= 250 )
            nCol := Col()
            clr := SetColor()
            s := edi_MsgGet_ext( cRes, 4, 4, 8, MaxCol()-4, oHbc:cp )
            SetColor( clr )
            DevPos( nRow, nCol )
            SetCursor( SC_NORMAL )
            IF !Empty( s )
               RETURN s
            ENDIF
         ELSE
            cRes := ProcessKey( nColInit, cRes, nKeyExt, bKeys )
         ENDIF
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
   ENDIF
   IF !Empty( bKeys ) .AND. Valtype( cTemp := Eval( bKeys,nKeyExt,cRes,nColInit ) ) == "C"
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

/*
STATIC FUNCTION Cons_Hrb( cCommand )

   LOCAL cmd := "", xRes, i, nColInit, nKeyExt, nKey
   LOCAL oCons := RCons():New( cCommand )

   IF oCons:hProcess < 0
      ? "Error starting app"
      RETURN Nil
   ENDIF
   FilePane():cConsOut += Chr(13)+Chr(10) + "> " + cCommand + Chr(13)+Chr(10)
   DevPos( Maxrow(), nColInit := 0 )
   ?
   DO WHILE ( xRes := oCons:Read() ) != Nil
      IF !Empty( xRes )
         IF Chr(9) $ xRes
            xRes := StrTran( xRes, Chr(9), Space(8) )
         ENDIF
         SetColor( "W/N" )
         Add2Consout( xRes )
         ?? xRes
         nColInit := Col()
         cmd := ""
      ENDIF
      nKeyExt := Inkey( 0.05, INKEY_KEYBOARD + HB_INKEY_EXT )
      IF nKeyExt == 0
         LOOP
      ELSEIF (nKey := hb_keyStd( nKeyExt )) == K_ESC .OR. ;
            ( hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0 .AND. nKey == K_CTRL_C )
         EXIT
      ELSEIF hb_keyStd( nKeyExt ) == K_ENTER
         IF !oCons:Write( cmd + hb_Eol() )
            ? "Pipe write error"
         ENDIF
      ELSE
         cmd := ProcessKey( nColInit, cmd, nKeyExt )
      ENDIF
   ENDDO
   oCons:End()

   RETURN Nil
*/

STATIC FUNCTION Add2Consout( cText )

   LOCAL i

   FilePane():cConsOut += cText
   IF Len( FilePane():cConsOut ) > FilePane():nConsMax
      i := hb_At( Chr(10), FilePane():cConsOut, Len(FilePane():cConsOut)-FilePane():nConsMax )
      FilePane():cConsOut := Substr( FilePane():cConsOut, i + 1 )
   ENDIF

   RETURN Nil

STATIC FUNCTION Cons_My( cCommand )

   LOCAL cmd := "", xRes, i, nColInit, nKeyExt, nKey
   LOCAL nRow, nCol, clr, s, arr
   LOCAL pApp := cedi_StartConsoleApp( cCommand ), nSecInit, hWnd

   IF ( xRes :=  cedi_ReturnErrCode( pApp ) ) > 0
      ? _I("Error starting app"), xRes
      cedi_EndConsoleApp( pApp )
      RETURN Nil
   ENDIF
   Add2Consout( Chr(13)+Chr(10) + "> " + cCommand + Chr(13)+Chr(10) )
   DevPos( Maxrow(), nColInit := 0 )
   ?
   nSecInit := Seconds()
   DO WHILE ( xRes := cedi_ReadFromConsoleApp(pApp) ) != Nil
      //IF !Empty( xRes )
      IF !Empty( xRes := removeEscapeCodes( xRes ) )
         IF Chr(9) $ xRes
            xRes := StrTran( xRes, Chr(9), Space(8) )
         ENDIF
         SetColor( "W/N" )
         Add2Consout( xRes )
         ?? xRes
         nColInit := Col()
         cmd := ""
         nSecInit := 0
      ENDIF
      nKeyExt := Inkey( 0.05, INKEY_KEYBOARD + HB_INKEY_EXT )
      IF nKeyExt == Nil
         RETURN Nil
      ENDIF
      IF nKeyExt == 0
#ifndef __PLATFORM__UNIX
         IF nSecInit > 0 .AND. Seconds() - nSecInit > 0.3
            nSecInit := 0
            IF !Empty( hWnd := cedi_GETHWNDBYPID( pApp ) )
               IF ( edi_Alert( _I("Application has a window"), _I("Show it"), _I("Ignore") ) ) == 1
                  cedi_ShowWindow( hWnd )
                  cedi_EndConsoleApp( pApp, .T. )
                  pApp := Nil
                  EXIT
               ENDIF
            ENDIF
         ENDIF
#endif
         LOOP
      ELSEIF (nKey := hb_keyStd( nKeyExt )) == K_ESC .OR. ;
            ( hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0 .AND. nKey == K_CTRL_C )
         EXIT
      ELSEIF hb_keyStd( nKeyExt ) == K_ENTER
         Add2Consout( cmd+hb_eol() )
         ?
         IF !cedi_WriteToConsoleApp( pApp, cmd+hb_eol() )
            ?? "Pipe write error"
         ENDIF
      ELSE
         IF nKey == K_CTRL_R .OR. ;
            ( MaxCol() - nColInit < cp_Len( oHbc:lUtf8, cmd ) .AND. nKey >= 32 .AND. nKey <= 250 )
            nCol := Col(); nRow := Row()
            clr := SetColor()
            s := edi_MsgGet_ext( cmd, 4, 4, 8, MaxCol()-4, oHbc:cp )
            SetColor( clr )
            DevPos( nRow, nCol )
            SetCursor( SC_NORMAL )
            IF !Empty( s )
               Add2Consout( s + hb_eol() )
               DevPos( nRow, nColInit )
               IF Chr(10) $ s
                  arr := hb_ATokens( s, Chr(10) )
                  DevOut( arr[1] )
                  FOR i := 2 TO Len( arr )
                     ? arr[i]
                  NEXT
               ELSE
                  DevOut( s )
               ENDIF
               ?
               IF !cedi_WriteToConsoleApp( pApp, s+hb_eol() )
                  ?? "Pipe write error"
               ENDIF
            ENDIF
         ELSE
            cmd := ProcessKey( nColInit, cmd, nKeyExt )
         ENDIF
         //cedi_WriteToConsoleApp( pApp, Chr(nKey) )
      ENDIF
   ENDDO
   cedi_EndConsoleApp( pApp )

   RETURN Nil

#ifdef _USE_SSH2
STATIC FUNCTION Cons_ssh2_My( pSess, cCommand )

   LOCAL xRes, nKeyExt, nKey, l10 := .F.

   ssh2_Channel_Pty( pSess, "xterm" )
   IF cCommand == "shell"
      ssh2_Channel_Shell( pSess )
   ELSE
      ssh2_Exec( pSess, "cd " + oPaneCurr:cCurrPath + "; " + cCommand )
   ENDIF
   IF ssh2_LastRes( pSess ) != 0
      ? "Exec failed"
      RETURN Nil
   ENDIF

   ? "> " + cCommand
   ?
   Add2Consout( Chr(13)+Chr(10) + "> " + cCommand + Chr(13)+Chr(10) )
   DO WHILE ( xRes := ssh2_Channel_ReadRaw( pSess ) ) != Nil
      IF !Empty( xRes )
         IF !Empty( xRes := removeEscapeCodes( xRes ) )
            SetColor( "W/N" )
            IF l10
               IF Asc( xRes ) == 10
                  xRes := Substr( xRes, 2 )
               ENDIF
               l10 := .F.
            ENDIF
            ?? xRes
            Add2Consout( xRes )
         ENDIF
      ENDIF
      nKeyExt := Inkey( 0.05, INKEY_KEYBOARD + HB_INKEY_EXT )
      nKey := hb_keyStd( nKeyExt )
      IF nKeyExt == 0
         LOOP
      ELSEIF nKey == K_ESC
         EXIT
      ELSEIF nKey == K_ENTER
         ssh2_Channel_Write( pSess, Chr(10) )
         l10 := .T.
         ?
      ELSEIF ( nKey >= 32 .AND. nKey <= 250 ) .OR. nKey == K_BS
         ssh2_Channel_Write( pSess, Chr(nKey) )
      ENDIF
   ENDDO
   /*
   IF !Empty( xRes := ssh2_Channel_Read( pSess ) )
      ? xRes
      Add2Consout( xRes )
   ENDIF
   */

   RETURN Nil
#endif

STATIC FUNCTION removeEscapeCodes( cText, lProcess )

   LOCAL nPos, nPos2 := 1, n, nLen
   STATIC cEsc := e"\x1b"

   IF !( cEsc $ cText )
      RETURN cText
   ENDIF
   IF lProcess == Nil; lProcess := .F.; ENDIF

   //edi_Writelog( cText )
   DO WHILE ( nPos := hb_At( cEsc, cText, nPos2 ) ) > 0
      nLen := Len( cText )
      nPos2 := nPos
      DO WHILE .T.
         IF hb_bPeek( cText, nPos2+1 ) == 93  // ]
            DO WHILE ++nPos2 <= nLen .AND. ( n := hb_bPeek( cText, nPos2 ) ) != 7 .AND. ;
               !(n == 83.AND.hb_bPeek(cText,nPos2+1) == 84); ENDDO
            IF nPos2 <= nLen
               //edi_Writelog( Substr( cText, nPos2 ) )
               IF n == 83
                  nPos2 ++
               ENDIF
            ENDIF
            //edi_Writelog( Substr( cText, nPos2 ) )
         ELSE
            DO WHILE ++nPos2 <= nLen .AND. ;
               ( n := hb_bPeek( cText, nPos2 ) ) < 65 .OR. ( n > 90 .AND. n < 97 ) .OR. n > 122; ENDDO
         ENDIF
         IF lProcess
            processEscapeCode( Substr( cText, nPos+1, nPos2-nPos-1 ) )
         ENDIF
         IF ++nPos2 <= nLen .OR. hb_bPeek( cText, nPos2 ) != 27
            EXIT
         ENDIF
      ENDDO
      cText := Left( cText, nPos - 1 ) + Substr( cText, nPos2 )
      nPos2 := nPos
   ENDDO

   //edi_Writelog( cText )
   RETURN cText

STATIC FUNCTION processEscapeCode( cCode )

   RETURN Nil

FUNCTION hbc_GetLogin( cLogin, cPass, lSave )
   LOCAL y1 := 5, x1 := Int(MaxCol()/2)-15, x2 := x1+30
   LOCAL cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets := { ;
      {y1+1,x1+2, 11, _I("Login:")}, ;
      { y1+1,x1+10, 0, cLogin, x2-x1-12 }, ;
      {y1+2,x1+2, 11, _I("Passw:")}, ;
      { y1+2,x1+10, 0, cPass, x2-x1-12,,,"@P" }, ;
      {y1+4,x1+3, 1, .F., 1 }, {y1+4,x1+2, 11, "[ ] " + _I("Save password")} ;
      }

   cBuf := Savescreen( y1, x1, y1 + 5, x2 )
   @ y1, x1, y1 + 5, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+3, x1 SAY "Ã"
   @ y1+3, x2 SAY "´"
   @ y1+3, x1+1 TO y1+3, x2-1

   edi_READ( aGets )
   Restscreen( y1, x1, y1 + 5, x2, cBuf )
   IF LastKey() == K_ESC
      RETURN .F.
   ELSE
      cLogin := aGets[2,4]
      cPass  := aGets[4,4]
      lSave  := aGets[5,4]
   ENDIF

   RETURN .T.

FUNCTION NetInfoLoad()

   LOCAL arr := hb_ATokens( Memoread( hb_DirBase() + "hbc.net" ), Chr(10) ), i

   IF !Empty( FilePane():aNetInfo )
      RETURN Nil
   ENDIF
   FOR i := Len(arr) TO 1 STEP -1
      IF Empty( arr[i] )
         hb_ADel( arr, i, .T. )
      ELSE
         arr[i] := hb_ATokens( ;
            Iif(Right( arr[i],1 ) == Chr(13), hb_strShrink( arr[i], 1 ), arr[i] ), ',' )
      ENDIF
   NEXT

   FilePane():aNetInfo := arr

   RETURN Nil

STATIC FUNCTION NetInfoSave()

   LOCAL i, j, s := "", nLen, s1

   IF !Empty( FilePane():aNetInfo )
      nLen := Len(FilePane():aNetInfo)
      FOR i := 1 TO nLen
         s1 := ""
         FOR j := 1 TO Len(FilePane():aNetInfo[i])
            s1 += Iif( j==1, "", ',' ) + FilePane():aNetInfo[i,j]
         NEXT
         s += s1 + Chr(10)
      NEXT
      hb_MemoWrit( hb_DirBase() + "hbc.net", s )
   ENDIF

   RETURN Nil

STATIC FUNCTION AddDocHis( n, cDocName, cp, lNoTrans )

   LOCAL i

   IF !lNoTrans .AND. !( cp == "UTF8" )
      cDocName := hb_strToUtf8( cDocName, cp )
   ENDIF
   IF ( i := Ascan2( Filepane():aDocHis[n], cDocName, 2 ) ) > 0
      IF i > 1
         ADel( Filepane():aDocHis[n], i )
         hb_AIns( Filepane():aDocHis[n], 1, { cp, cDocName }, .F. )
      ENDIF
   ELSE
      hb_AIns( Filepane():aDocHis[n], 1, { cp, cDocName }, Len(Filepane():aDocHis[n])<Filepane():nDocMax )
   ENDIF
   FilePane():lDocHis := .T.

   RETURN Nil

STATIC FUNCTION PlugFunc( oPane, cIOpref, cName, aParams )

   LOCAL cFunc := "PLUG_HBC_" + ;
      Iif( Right(cIOpref,1)==':',hb_strShrink(cIOpref,1),cIOpref ) + "_" + cName

   //edi_Alert( cFunc )
   IF hb_isFunction( cFunc )
      RETURN Eval( &( "{|o,a|" + cFunc + "(o,a)}" ), oPane, aParams )
   ENDIF
   RETURN Nil

#ifdef __PLATFORM__UNIX
STATIC FUNCTION IsFileExec( arr )

   LOCAL nAttr

   IF 'D' $ arr[5]
      RETURN .F.
   ENDIF
   IF !( 'R' $ arr[5] )
      arr[5] += "R"
      IF hb_vfAttrGet( oPaneCurr:cIOpref + oPaneCurr:net_cAddress + oPaneCurr:net_cPort + ;
         oPaneCurr:cCurrPath + arr[1], @nAttr )
         IF hb_bitAnd( nAttr,HB_FA_XUSR+HB_FA_XGRP+HB_FA_XOTH ) > 0
            arr[5] += "X"
         ENDIF
      ENDIF
   ENDIF

   RETURN ( 'X' $ arr[5] )
#endif

FUNCTION hbc_Wndinit( y1, x1, y2, x2, clr, cTitle )

   LOCAL cBuf := Savescreen( y1, x1, y2, x2 )

   IF Empty( clr )
      clr := "N/W"
   ENDIF
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ " COLOR (clr)
   IF !Empty( cTitle )
      @ y1, x1+2 SAY cTitle COLOR (clr)
   ENDIF

   RETURN { y1, x1, y2, x2, clr, cBuf }

FUNCTION hbc_Wndout( arr, cText )

   LOCAL clr := SetColor( arr[5] )

   Scroll( arr[1]+1, arr[2]+1, arr[3]-1, arr[4]-1, 1 )
   @ arr[3]-1, arr[2]+2 SAY NameShortcut( cText, arr[4]-arr[2]-1, "~", oHbc:lUtf8 )
   SetColor( clr )

   RETURN Nil

FUNCTION hbc_Wndclose( arr, cText )

   LOCAL clr

   IF cText != Nil
      clr := SetColor( arr[5] )
      Scroll( arr[1]+1, arr[2]+1, arr[3]-1, arr[4]-1, 1 )
      @ arr[3]-1, arr[2]+2 SAY cText COLOR (arr[5])
      Inkey( 0 )
      SetColor( clr )
   ENDIF
   Restscreen( arr[1], arr[2], arr[3], arr[4], arr[6] )

   RETURN Nil

FUNCTION Ascan2( arr, xItem, n )
   IF Empty( n )
      n := 1
   ENDIF
   RETURN Ascan( arr, {|a|a[n]==xItem} )

/*
#define BUFFER_SIZE  1024

CLASS RCons

   DATA   hProcess
   DATA   hStdIn, hStdOut, hStdErr

   METHOD New( cCmd )
   METHOD Read()
   METHOD Write( cText )
   METHOD End()

ENDCLASS

METHOD New( cCmd ) CLASS RCons

   LOCAL hStdIn, hStdOut, hStdErr

   ::hProcess = hb_processOpen( cCmd, @hStdIn, @hStdOut, @hStdErr )

   ::hStdIn = hStdIn
   ::hStdOut = hStdOut
   ::hStdErr = hStdErr

   RETURN Self

METHOD Read() CLASS RCons

   LOCAL nRead
   LOCAL cBuffer := Space( BUFFER_SIZE )

   IF ( nRead := hb_PRead( ::hStdOut, @cBuffer, BUFFER_SIZE, 10 ) ) == 0
      nRead := hb_PRead( ::hStdErr, @cBuffer, BUFFER_SIZE, 10 )
   ENDIF

   RETURN Iif( nRead > 0, Left( cBuffer, nRead ), Iif( nRead < 0, Nil, "" ) )

METHOD Write( cText ) CLASS RCons

   RETURN ( FWrite( ::hStdIn, cText ) > 0 )

METHOD End() CLASS RCons

   FClose( ::hStdIn )
   FClose( ::hStdOut )
   FClose( ::hStdErr )
#ifdef __PLATFORM__UNIX
   cedi_waitpid( ::hProcess )
#endif
   RETURN hb_processClose( ::hProcess )
*/