/*
 * Text editor
 *
 * Copyright 2019-2023 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"

#include "fedit.ch"
#ifdef __PSEUDOGT
   #include "hwpgt.ch"
#endif

#define SHIFT_PRESSED 0x010000
#define CTRL_PRESSED  0x020000
#define ALT_PRESSED   0x040000
#define MAX_CBOARDS         29
#define MAX_EDIT_CBOARDS    10
#define CBOARD_MINUS        28
#define CBOARD_SYS          29

#define UNDO_LINE1      1
#define UNDO_POS1       2
#define UNDO_LINE2      3
#define UNDO_POS2       4
#define UNDO_OPER       5
#define UNDO_TEXT       6

#define UNDO_OP_INS     1
#define UNDO_OP_OVER    2
#define UNDO_OP_DEL     3
#define UNDO_OP_SHIFT   4
#define UNDO_OP_START   5
#define UNDO_OP_END     6

#define UNDO_INC       12

#xtranslate _I( <x,...> ) => TrnMsg( hb_i18n_gettext( <x> ) )

STATIC aKeysMove := { K_UP, K_DOWN, K_LEFT, K_RIGHT, K_HOME, K_END, K_PGDN, K_PGUP, K_CTRL_PGUP, K_CTRL_PGDN }
STATIC aAltKeysMove := { K_ALT_UP, K_ALT_DOWN, K_ALT_LEFT, K_ALT_RIGHT, K_ALT_HOME, K_ALT_END, K_ALT_PGDN, K_ALT_PGUP }
STATIC cKeysMove := "hjklwWeEbBG0$^"
STATIC hKeyMap := Nil

STATIC aLangExten := {}
STATIC cLangMapCP, aLangMapUpper, aLangMapLower
STATIC nLastReg := 1
STATIC aLangs
STATIC hPalettes
STATIC lCase_Sea := .F., lWord_Sea := .F., lRegex_Sea := .F.
STATIC cDopMode := ""
STATIC cLastDir := ""
STATIC aMacro
STATIC cTab := e"\x9", cTabStr
STATIC nLastMacro
STATIC aLastSeleOper
STATIC aLastOper, lLastOper_Ended := .T., lDoLastOper := .F., lAddLast
STATIC nLastKey := 0, nLastSec := 0, nAutoDelay := 0
// STATIC hIdle

CLASS TEdit

   CLASS VAR aCPages    SHARED  INIT {}
   CLASS VAR aWindows   SHARED  INIT {}          // An array with all TEdit objects
   CLASS VAR nCurr      SHARED                   // A currently processed TEdit object number
   CLASS VAR nCurrPrev  SHARED
   CLASS VAR cpInit     SHARED
   CLASS VAR cLauncher  SHARED  INIT ""
   CLASS VAR lReadIni   SHARED  INIT .F.         // If ini file have been read already
   CLASS VAR options    SHARED  INIT { => }
   CLASS VAR aCmdHis    SHARED  INIT {}
   CLASS VAR aSeaHis    SHARED  INIT {}
   CLASS VAR aReplHis   SHARED  INIT {}
   CLASS VAR aEditHis   SHARED  INIT {}
   CLASS VAR aCBoards   SHARED                   // An array for clipboard buffers
   CLASS VAR aHiliAttrs SHARED  INIT { "W+/B", "W+/B", "W+/B", "W+/B", "W+/B", "GR+/B", "W/B", "W/B", "W/B", "W/B" }
   CLASS VAR aPlugins   SHARED  INIT {}
   CLASS VAR nDefMode   SHARED  INIT 0           // A start mode ( -1 - Edit only, 0 - Edit, 1- Vim )
   CLASS VAR lCmdMode   SHARED  INIT .F.
   CLASS VAR cDefPal    SHARED  INIT "default"
   CLASS VAR cCurrPal   SHARED
   CLASS VAR cColor     SHARED  INIT "BG+/B"
   CLASS VAR cColorSel  SHARED  INIT "N/W"
   CLASS VAR cColorPane SHARED  INIT "N/BG"
   CLASS VAR cColorBra  SHARED  INIT "R+/B"
   CLASS VAR cColorMenu SHARED  INIT "W+/BG"
   CLASS VAR cColorMenuSel SHARED  INIT "GR+/RB"
   CLASS VAR cColorWB   SHARED  INIT "W+/N"
   CLASS VAR cColorWR   SHARED  INIT "W+/R"
   CLASS VAR cColorGet  SHARED  INIT "N+/BG"
   CLASS VAR nTabLen    SHARED  INIT 4
   CLASS VAR aRectFull  SHARED
   CLASS VAR bNew       SHARED
   CLASS VAR hMacros    SHARED
   CLASS VAR hSelePlug  SHARED
   CLASS VAR cLangCP    SHARED
   CLASS VAR hMisc      SHARED
#ifdef __PLATFORM__UNIX
#ifndef GTHWG
   CLASS VAR cClipCmd   SHARED
#endif
#endif

   DATA   aRect       INIT { 0,0,24,79 }
   DATA   y1, x1, y2, x2
   DATA   cFileName   INIT ""
   DATA   dDateMod, cTimeMod
   DATA   cp
   DATA   cPalette
   DATA   nxFirst, nyFirst, nxOfLine
   DATA   aText
   DATA   lWrap       INIT .F.
   DATA   nMode                       // Current mode ( 0 - Edit, 1 - Vim, Cmd)
   DATA   nDopMode    INIT 0          // A state in a Vim mode after pressing some keys
                                      // (m, ', 0..9, ...) when other keys are expected
   DATA   cSyntaxType

   DATA   oParent                     // A TEdit object of a parent edit window
   DATA   aUndo       INIT {}
   DATA   nUndo       INIT 0

   DATA   lTopPane    INIT .T.
   DATA   nTopName    INIT 36

   DATA   lBuiltIn    INIT .F.
   DATA   lCtrlTab    INIT .T.
   DATA   lReadOnly   INIT .F.
   DATA   lUtf8       INIT .F.
   DATA   lUpdated    INIT .F.
   DATA   lIns        INIT .T.
   DATA   lShiftKey   INIT .F.

   //DATA   lTabs       INIT .F.

   DATA   nPos, nLine
   DATA   nPosBack    INIT 1
   DATA   nLineBack   INIT 1
   DATA   lF3         INIT .F.
   DATA   nSeleMode   INIT  0
   DATA   nby1        INIT -1
   DATA   nby2        INIT -1
   DATA   nbx1, nbx2
   DATA   lTextOut    INIT .F.

   DATA   lShow
   DATA   lClose      INIT .F.
   DATA   cEol
   DATA   lBom        INIT .F.

   DATA   funSave
   DATA   bStartEdit, bEndEdit
   DATA   bOnKey, bWriteTopPane, bTextOut
   DATA   bAutoC
   DATA   oHili
   DATA   hBookMarks
   DATA   npy1, npx1, npy2, npx2
   DATA   cargo, hCargo

   METHOD New( cText, cFileName, y1, x1, y2, x2, cColor, lTopPane )
   METHOD SetText( cText, cFileName )
   METHOD Edit()
   METHOD TextOut( n1, n2 )
   METHOD LineOut( nLine )
   METHOD onKey( nKeyExt )
   METHOD WriteTopPane( lClear )
   METHOD RowToLine( nRow )
   METHOD ColToPos( nRow, nCol )
   METHOD LineToRow( nLine, nPos )
   METHOD PosToCol( nLine, nPos )
   METHOD Search( cSea, lCase, lNext, lWord, lRegex, ny, nx, lInc )
   METHOD GoTo( ny, nx, nSele, lNoGo )
   METHOD ToString( cEol, cp )
   METHOD Save( cFileName )
   METHOD InsText( nLine, nPos, cText, lOver, lChgPos, lNoUndo )
   METHOD DelText( nLine1, nPos1, nLine2, nPos2, lNoUndo )
   METHOD Undo( nLine1, nPos1, nLine2, nPos2, nOper, cText )
   METHOD Highlighter( oHili )
   METHOD OnExit()

ENDCLASS

METHOD New( cText, cFileName, y1, x1, y2, x2, cColor, lTopPane ) CLASS TEdit

   LOCAL i, cTmp

   IF !::lReadIni
      edi_ReadIni( edi_FindPath( "hbedit.ini" ) )
   ENDIF
   IF Empty( ::aRectFull )
#ifndef __BUILT_IN
      IF !::lBuiltIn .AND. !Empty( ::cLangCP ) .AND. !Empty( cTmp := edi_FindPath( "hbedit_"+Lower(::cLangCP)+".hbl" ) )
         cTmp := hb_MemoRead( cTmp )
         IF hb_i18n_Check( cTmp )
            hb_i18n_Set( hb_i18n_RestoreTable(cTmp) )
         ENDIF
      ENDIF
#endif
      ::aRectFull := { 0, 0, MaxRow(), MaxCol() }
      IF y1 != Nil; ::aRectFull[1] := y1; ENDIF
      IF x1 != Nil; ::aRectFull[2] := x1; ENDIF
      IF y2 != Nil; ::aRectFull[3] := y2; ENDIF
      IF x2 != Nil; ::aRectFull[4] := x2; ENDIF
   ENDIF
   ::y1 := ::aRect[1] := Iif( y1==Nil, ::aRectFull[1], y1 )
   ::x1 := ::aRect[2] := Iif( x1==Nil, ::aRectFull[2], x1 )
   ::y2 := ::aRect[3] := Iif( y2==Nil, ::aRectFull[3], y2 )
   ::x2 := ::aRect[4] := Iif( x2==Nil, ::aRectFull[4], x2 )
   ::cColor := Iif( Empty(cColor), ::cColor, cColor )
   ::nxFirst := ::nyFirst := ::nxOfLine := 1
   ::npy1 := ::npx1 := ::npy2 := ::npx2 := 0
   IF Valtype( lTopPane ) == "L" .AND. !lTopPane
      ::lTopPane := .F.
   ENDIF

   IF ::lTopPane
      ::nTopName := Max( ::x2 - ::x1 - Iif(::x2-::x1>54,44,37), 0 )
      ::y1 ++
   ENDIF
   ::nLine := ::nPos := 1

   ::nMode := Iif( ::nDefMode<0, 0, ::nDefMode )
   ::cp := ::cpInit

   ::SetText( cText, cFileName )

   ::hBookMarks := hb_Hash()

   Aadd( ::aWindows, Self )

   IF !Empty( ::bNew )
      Eval( ::bNew, Self )
   ENDIF

   RETURN Self

METHOD SetText( cText, cFileName ) CLASS TEdit

   LOCAL i, j, arr, cFile_utf8, cExt, cFullPath, cBom := e"\xef\xbb\xbf"
   LOCAL nEol := hb_hGetDef( TEdit():options,"eol", 0 )
   LOCAL lT2Sp := hb_hGetDef( TEdit():options,"tabtospaces", .F. )
   LOCAL dDateMod, cTimeMod

   IF !Empty( cFileName )
      IF Left( cFileName,1 ) == "$"
         ::cFileName := cFileName
      ELSE
         IF Empty( hb_fnameDir( cFileName ) )
#ifdef __PLATFORM__UNIX
            cFileName := '/' + Curdir() + '/' + cFileName
#else
            cFileName := hb_curDrive() + ":\" + Curdir() + '\' + cFileName
#endif
         ENDIF
#ifdef __PLATFORM__UNIX
         IF !Empty( cExt := cedi_RealPath( cFileName ) )
            cFileName := cExt
         ENDIF
#endif
         ::cFileName := cFileName
         cFile_utf8 := hb_Translate( cFileName,, "UTF8" )
#ifdef __PLATFORM__UNIX
         i := Ascan( TEdit():aEditHis, {|a|a[1]==cFile_utf8} )
#else
         cExt := cp_Lower( .T., cFile_utf8 )
         i := Ascan( TEdit():aEditHis, {|a|cp_Lower(.T.,a[1])==cExt} )
#endif
         IF i > 0
            arr := TEdit():aEditHis[i]
            ADel( TEdit():aEditHis, i )
            hb_AIns( TEdit():aEditHis, 1, arr, .F. )
            hb_cdpSelect( ::cp := arr[2] )
            ::nLine := arr[3]
            ::nPos  := arr[4]
         ELSE
            hb_AIns( TEdit():aEditHis, 1, {cFile_utf8,::cp,1,1}, Len(TEdit():aEditHis)<hb_hGetDef(TEdit():options,"edithismax",10) )
         ENDIF
      ENDIF
      IF !Empty( ::cFileName ) .AND. Left( ::cFileName,1 ) != "$" .AND. ;
         hb_fGetDateTime( ::cFileName, @dDateMod, @cTimeMod )
         ::dDateMod := dDateMod
         ::cTimeMod := cTimeMod
      ENDIF
   ENDIF

   ::lUtf8 := ( Lower(::cp) == "utf8" )
   IF Empty( cText )
      ::aText := { "" }
      ::cEol := Iif( nEol == 1, Chr(10), Chr(13) + Chr(10) )
   ELSE
      ::aText := hb_ATokens( cText, Chr(10) )
      IF nEol == 0
         ::cEol := Iif( Right( ::aText[1],1 ) == Chr(13), Chr(13) + Chr(10), Chr(10) )
      ELSE
         ::cEol := Iif( nEol == 1, Chr(10), Chr(13) + Chr(10) )
      ENDIF
      IF Left( ::aText[1], 3 ) == cBom
         hb_cdpSelect( ::cp := "UTF8" )
         ::lUtf8 := .T.
         ::aText[1] := Substr( ::aText[1], 4 )
         ::lBom := .T.
      ENDIF
      FOR i := 1 TO Len( ::aText )
         IF lT2Sp .AND. cTab $ ::aText[i]
            ::aText[i] := Strtran( ::aText[i], cTab, cTabStr )
            //::lTabs := .T.
         ENDIF
         IF Right( ::aText[i],1 ) == Chr(13)
            ::aText[i] := Left( ::aText[i], Len( ::aText[i])-1 )
         ELSEIF Left( ::aText[i],1 ) == Chr(13)
            ::aText[i] := Substr( ::aText[i], 2 )
         ENDIF
      NEXT
   ENDIF

   ::aUndo := {}
   ::nUndo := 0

   IF hb_hGetDef( TEdit():options, "syntax", .F. ) .AND. !Empty( cFileName )
      cExt := Lower( hb_fnameExt(cFileName) )
      FOR i := 1 TO Len(aLangExten)
         IF ( j := At( cExt, aLangExten[i,2] ) ) > 0 .AND. !isAlpha( Substr( aLangExten[i,2],j+Len(cExt),1 ) ) ;
            .AND. hb_hHaskey(aLangs,aLangExten[i,1])
            mnu_SyntaxOn( Self, aLangExten[i,1] )
            EXIT
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

METHOD Edit( lShowOnly ) CLASS TEdit

   LOCAL i, nKeyExt, cFile_utf8, n

   IF !Empty( ::oParent ) .AND. Len( ::aWindows ) > ::nCurrPrev .AND. ;
      ( ::aWindows[::nCurrPrev]:oParent == ::oParent )
      ::oParent:Edit( .T. )
   ENDIF
   hb_cdpSelect( ::cp )
   ::nCurr := Ascan( ::aWindows, {|o|o==Self} )

   FOR i := Len( ::aWindows ) TO 1 STEP -1
      // Draw the child window, if found.
      IF !Empty( ::aWindows[i]:oParent ) .AND. ::aWindows[i]:oParent == Self
         IF !( ::aWindows[i]:cPalette == ::cCurrPal )
            edi_SetPalette( ::aWindows[i], Iif( Empty( ::aWindows[i]:cPalette ), ::cDefPal, ::aWindows[i]:cPalette ) )
         ENDIF
         ::aWindows[i]:WriteTopPane( .T. )
         ::aWindows[i]:TextOut()
      ENDIF
   NEXT

   IF ( ::cCurrPal == Nil .OR. !( ::cPalette == ::cCurrPal ) ) .AND. ;
      ( !Empty(::cPalette) .OR. !Empty(::cDefPal) )
      edi_SetPalette( Self, Iif( Empty( ::cPalette ), ::cDefPal, ::cPalette ) )
   ENDIF
   SetCursor( SC_NONE )
   SetColor( ::cColor )
   Scroll( ::y1, ::x1, ::y2, ::x2 )

   IF ::nLine < 1 .OR. ::nLine > Len( ::aText )
      ::nyFirst := ::nxFirst := ::nxOfLine := ::nLine := ::nPos := 1
   ENDIF

   ::WriteTopPane()

   ::GoTo( ::nLine, ::nPos )
   IF Len( ::aText ) > 1 .OR. !Empty( ::aText[1] )
      ::TextOut()
   ENDIF

   IF !Empty( ::bStartEdit )
      Eval( ::bStartEdit, Self )
   ENDIF

   IF !Empty( lShowOnly )
      RETURN Nil
   ENDIF
   ::nCurrPrev := ::nCurr

   //::nPosBack := ::nPos
   //::nLineBack := ::nLine
   ::lShow := .T.
   DO WHILE ::lShow .AND. !::lClose
      SetCursor( Iif( ::lIns==Nil, SC_NONE, Iif( ::lIns, SC_NORMAL, SC_SPECIAL1 ) ) )
      IF nAutoDelay > 0
         DO WHILE ( nKeyExt := Inkey( 0.5, HB_INKEY_ALL + HB_INKEY_EXT ) ) == 0
            FCheckAutoc()
         ENDDO
      ELSE
         nKeyExt := Inkey( 0, HB_INKEY_ALL + HB_INKEY_EXT )
      ENDIF
      IF nKeyExt == Nil
         ::lShow := .F.
         ::lClose := .T.
         EXIT
      ENDIF
      IF !Empty( hKeyMap ) .AND. !Empty( i := hb_hGetDef( hKeyMap, nKeyExt, 0 ) )
         //edi_Alert( hb_NumToHex(nkeyext) + " " + valtype(i) )
         IF Valtype( i ) == "N"
            nKeyExt := i
         ELSEIF Valtype( i ) == "A"
            IF i[1] == 0
               edi_Alert( "1" )
               FOR n := 2 TO Len( i ) - 1
                  ::onKey( i[n] )
               NEXT
               nKeyExt := i[n]
            ELSE
               FOR n := 1 TO Len( i )
                  IF Empty( ::aPlugins[i[n],3] ) .OR. ::aPlugins[i[n],3] == ::cSyntaxType
                     //edi_Alert( "2a " + Iif(Empty( ::aPlugins[i[n],3] ),"T ","F ") + ::cSyntaxType)
                     i := i[n]
                     EXIT
                  ENDIF
               NEXT
               IF Valtype( i ) == "N"
                  SetCursor( SC_NONE )
                  edi_RunPlugin( Self, ::aPlugins, i )
                  LOOP
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      SetCursor( SC_NONE )
      ::onKey( nKeyExt )
   ENDDO

   hb_cdpSelect( ::cpInit )
   IF !Empty( ::cFileName )
      cFile_utf8 := hb_Translate( ::cFileName,, "UTF8" )
      IF ( i := Ascan( TEdit():aEditHis, {|a|a[1]==cFile_utf8} ) ) > 0
         TEdit():aEditHis[i,2] := ::cp
         TEdit():aEditHis[i,3] := ::nLine
         TEdit():aEditHis[i,4] := ::nPos
      ENDIF
   ENDIF

   ::WriteTopPane( .T. )
   IF !Empty( ::bEndEdit )
      Eval( ::bEndEdit, Self )
   ENDIF
   IF ::lClose
      IF Ascan( ::aWindows, {|o|o:oParent==Self} ) == 0
         edi_CloseWindow( Self )
         IF ::nCurr > 1
            ::nCurr --
         ENDIF
      ELSEIF edi_Alert( _I("There are child windows.;Close anyway?"), _I("Yes"), _I("No") ) == 1
         IF ( n := edi_WindowUpdated( Self ) ) > 0
            IF ( i := edi_Alert( _I("Some files are updated"), _I("Cancel"), _I("GoTo"), _I("Close anyway") ) ) <= 1
               ::lClose := .F.
               RETURN Nil
            ELSEIF i == 2
               ::nCurr := i
               RETURN Nil
            ENDIF
         ENDIF
         edi_CloseWindow( Self )
         IF ::nCurr > 1
            ::nCurr --
         ENDIF
      ELSE
         ::lClose := .F.
      ENDIF
   ENDIF

   RETURN Nil

METHOD TextOut( n1, n2 ) CLASS TEdit

   LOCAL i, nCol := Col(), nRow := Row()

   IF !Empty( ::bTextOut )
      RETURN Eval( ::bTextOut, Self )
   ENDIF
   IF n1 == Nil; n1 := 1; ENDIF
   IF n2 == Nil; n2 := ::y2 -::y1 + 1; ENDIF

   hb_cdpSelect( ::cp )
   FOR i := n1 TO n2
      ::LineOut( i )
   NEXT
   DevPos( nRow, nCol )

   RETURN Nil

METHOD LineOut( nLine, lInTextOut ) CLASS TEdit

   LOCAL n := nLine + ::nyFirst - 1, y := ::y1 + nLine - 1, nWidth := ::x2 - ::x1 + 1, s, nLen
   LOCAL lSel := .F., nby1, nby2, nbx1, nbx2, nx1, nx2, lTabs := .F., nf := ::nxFirst, nf1
   LOCAL aStru, i, aClrs
   LOCAL nxPosFirst := 1, nxPosLast := nf + nWidth, nDop

   IF ::lWrap
      n := edi_CalcWrapped( Self, y,, @nxPosFirst, .F. )
      nf := nxPosFirst
      nxPosLast := nxPosFirst + nWidth - 1
   ENDIF
   IF n <= Len( ::aText )

      IF ::nby1 >= 0 .AND. ::nby2 >= 0
         IF ::nby1 < ::nby2 .OR. ( ::nby1 == ::nby2 .AND. ::nbx1 < ::nbx2 )
            nby1 := ::nby1; nbx1 := ::nbx1; nby2 := ::nby2; nbx2 := ::nbx2
         ELSE
            nby1 := ::nby2; nbx1 := ::nbx2; nby2 := ::nby1; nbx2 := ::nbx1
         ENDIF
         IF ::nSeleMode == 2
            IF n != nby1
               nbx1 := ::ColToPos( ::LineToRow(n), ::PosToCol( nby1,nbx1 ) )
            ENDIF
            IF n != nby2
               nbx2 := ::ColToPos( ::LineToRow(n), ::PosToCol( nby2,nbx2 ) )
            ENDIF
            IF nbx1 > nbx2
               i := nbx1; nbx1 := nbx2; nbx2 := i
            ENDIF
         ENDIF
         lSel := ( n >= nby1 .AND. n <= nby2 ) .AND. !( nby1 == nby2 .AND. nbx1 == nbx2 )
      ENDIF

      DevPos( y, ::x1 )
      IF ::lWrap
         s := cp_Substr( ::lUtf8, ::aText[n], nxPosFirst, nWidth )
         nLen := cp_Len( ::lUtf8, s )
      ELSE
         IF nf == 1 .AND. !::lUtf8 .AND. ( nLen := Len(::aText[n]) ) < nWidth
            s := ::aText[n]
         ELSE
            nxPosFirst := edi_Col2Pos( Self, n, nf )
            s := cp_Substr( ::lUtf8, ::aText[n], nxPosFirst, nWidth )
            nLen := cp_Len( ::lUtf8, s )
         ENDIF
      ENDIF
      DispBegin()
      SetColor( ::cColor )
      IF nLen > 0
         IF ::lWrap
            DevOut( s )
         ELSE
            i := 1
            IF ::nxFirst > 1 .AND. nxPosFirst < ::nxFirst
               nf := edi_ExpandTabs( Self, cp_Left(::lUtf8,::aText[n],nxPosFirst), 1, .T. )
               lTabs := .T.
               DevPos( y, i := (::x1 + nf - ::nxFirst) )
            ENDIF
            IF cTab $ s
               lTabs := .T.
               DevOut( cp_Left( ::lUtf8, edi_ExpandTabs( Self, s, nf ), nWidth-i+1 ) )
            ELSE
               DevOut( s )
            ENDIF
            IF lTabs
               nxPosLast := edi_Col2Pos( Self, n, nf + nWidth )
            ENDIF
         ENDIF
         nLen := Col() - ::x1

         IF !Empty( ::oHili ) .AND. hb_hGetDef( TEdit():options, "syntax", .F. )
            nDop := ::oHili:IsComm( n )
            ::oHili:Do( n )
            IF nDop != ::oHili:IsComm( n )
               ::lTextOut := .T.
            ENDIF
            aStru := ::oHili:aLineStru
            IF ::oHili:nItems > 0
               aClrs := ::oHili:hHili["colors"]
               FOR i := 1 TO ::oHili:nItems
                  IF aStru[i,1] < nxPosLast
                     IF aStru[i,2] >= nxPosFirst .AND. aStru[i,3] > 0
                        nx1 := Max( nxPosFirst, aStru[i,1] )
                        nx2 := Min( aStru[i,2], nxPosLast - 1 )
                        SetColor( Iif( !Empty(aClrs[aStru[i,3]]), aClrs[aStru[i,3]], ::aHiliAttrs[aStru[i,3]] ) )
                        IF lTabs
                           nf1 := edi_ExpandTabs( Self, cp_Left( ::lUtf8, s, nx1 - nxPosFirst ), nf, .T. )
                           DevPos( y, ::x1 + nf + nf1 - ::nxFirst )
                           DevOut( edi_ExpandTabs( Self, ;
                              cp_Substr( ::lUtf8, s, nx1-nxPosFirst+1, nx2-nx1+1 ), nf + nf1 ) )
                        ELSE
                           DevPos( y, nx1 - nf + ::x1 )
                           DevOut( cp_Substr( ::lUtf8, s, nx1-nf+1, nx2-nx1+1 ) )
                        ENDIF
                     ENDIF
                  ELSE
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDIF
         SetColor( ::cColor )
         IF lSel
            nbx1 := Iif( n > nby1 .AND. ::nSeleMode != 2, 1, nbx1 )
            nbx2 := Iif( n < nby2 .AND. ::nSeleMode != 2, cp_Len(::lUtf8,::aText[n])+1, nbx2 )
            IF nbx1 < nxPosLast .AND. nbx2 > nxPosFirst
               nbx1 := Max( nbx1, nxPosFirst )
               nbx2 := Min( nbx2, nxPosLast - 1 )
               SetColor( ::cColorSel )
               IF lTabs
                  nf1 := edi_ExpandTabs( Self, cp_Left( ::lUtf8, s, nbx1 - nxPosFirst ), nf, .T. )
                  DevPos( y, ::x1 + nf + nf1 - ::nxFirst )
                  DevOut( edi_ExpandTabs( Self, ;
                     cp_Substr( ::lUtf8, s, nbx1-nxPosFirst+1, nbx2-nbx1 ), nf + nf1 ) )
               ELSE
                  DevPos( y, nbx1 - nf + ::x1 )
                  DevOut( cp_Substr( ::lUtf8, s, nbx1-nf+1, nbx2-nbx1 ) )
               ENDIF
            ENDIF
            SetColor( Iif( n < nby2 .AND. ::nSeleMode != 2, ::cColorSel, ::cColor ) )
         ENDIF
      ELSEIF lSel .AND. n < nby2 .AND. ::nSeleMode != 2
         SetColor( ::cColorSel )
      ENDIF
      IF nLen < nWidth
         Scroll( y, ::x1 + nLen, y, ::x2 )
      ENDIF
      SetColor( ::cColor )
      DispEnd()
   ELSE
      Scroll( y, ::x1, y, ::x2 )
   ENDIF
   RETURN Nil

METHOD onKey( nKeyExt ) CLASS TEdit

   LOCAL nKey := hb_keyStd(nKeyExt), i, j, n, x, s, nCol := Col(), nRow := Row()
   LOCAL lShift, lCtrl := .F., lNoDeselect := .F., lSkip := .F.

   lAddLast := .F.
   n := ::nLine
   ::lTextOut := .F.

   IF !Empty( ::bOnKey )
      i := Eval( ::bOnKey, Self, nKeyExt )
      IF i == - 1
         nLastSec := 0
         RETURN Nil
      ELSEIF i > 0
         nKeyExt := i
         nKey := hb_keyStd(nKeyExt)
      ENDIF
   ENDIF

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE ;
      .OR. nKey == K_LBUTTONUP .OR. nKey == K_RBUTTONUP
      RETURN Nil
   ENDIF

   IF ::npy1 > 0
      // drop highlighting of matched parenthesis
      DevPos( ::LineToRow(::npy1), ::PosToCol(::npy1,::npx1) )
      DevOut( cp_Substr( ::lUtf8, ::aText[::npy1], ::npx1, 1 ) )
      IF ::npy2 >= ::nyFirst .AND. ::npy2 < ::nyFirst + ::y2 - ::y1 .AND. ;
            ::npx2 >= ::nxFirst .AND. ::npx2 < ::nxFirst + ::x2 - ::x1
         DevPos( ::LineToRow(::npy2), ::PosToCol(::npy1,::npx2) )
         DevOut( cp_Substr( ::lUtf8, ::aText[::npy2], ::npx2, 1 ) )
      ENDIF
      DevPos( ::LineToRow(::npy1), ::PosToCol(::npy1,::npx1) )
      ::npy1 := ::npx1 := ::npy2 := ::npx2 := 0
   ENDIF

   IF ::nDopMode == 113  // q - macro recording
      IF Len( cDopMode ) == 1
         nKey := edi_MapKey( Self, nKey )
         IF (nKey >= 48 .AND. nKey <= 57) .OR. (nKey >= 97 .AND. nKey <= 122)
            cDopMode := "Rec " + Chr( nKey )
            aMacro := {}
         ELSE
            ::nDopMode := 0
            cDopMode := ""
         ENDIF
         lSkip := .T.
      ELSE
         IF nKey == 113
            ::hMacros[Asc(Right(cDopMode,1))] := aMacro
            ::nDopMode := 0
            cDopMode := ""
            lSkip := .T.
         ELSE
            Aadd( aMacro, nKeyExt )
         ENDIF
      ENDIF
   ELSEIF ::nDopMode > 0
      IF nKey == K_ESC
         ::nDopMode := 0
      ELSE
         nKey := edi_MapKey( Self, nKey )
         SWITCH ::nDopMode
         CASE 109     // m
            edi_BookMarks( Self, nKey, .T. )
            ::nDopMode := 0
            EXIT
         CASE 39  // '
            IF nKey == 46   // .
               ::GoTo( ::aUndo[::nUndo][UNDO_LINE2], ::aUndo[::nUndo][UNDO_POS2] )
            ELSE
               edi_BookMarks( Self, nKey, .F. )
            ENDIF
            ::nDopMode := 0
            EXIT
         CASE 34  // "
            ::nDopMode := 0
            cDopMode := ""
            IF (nKey >= 97 .AND. nKey <= 122) .OR. nKey == 42 .OR. nKey == 45
               // a...z, -
               nLastReg := nKey
               nLastKey := nKeyExt
               RETURN Nil
            ENDIF
            EXIT
         CASE 102 // f
            IF ( i := cp_At( ::lUtf8, cp_Chr(::lUtf8,nKey), ::aText[n], ::nPos+1 ) ) > 0
               ::GoTo( n, i )
            ENDIF
            ::nDopMode := 0
            EXIT
         CASE 70  // F
            IF ( i := cp_Rat( ::lUtf8, cp_Chr(::lUtf8,nKey), ::aText[n],,::nPos-1 ) ) > 0
               ::GoTo( n, i )
            ENDIF
            ::nDopMode := 0
            EXIT
         CASE 49  // 1
            cDopMode += Chr( nKey )
            IF nKey == 103    // g
               ::nDopMode := 103
            ELSEIF nKey == 121   // y
               ::nDopMode := 121
            ELSEIF nKey == 64    // @
               ::nDopMode := 64
            ELSEIF Chr(nKey) $ cKeysMove .AND. nKey != 48
               edi_Move( Self, nKey, Val( cDopMode ) )
               nKey := K_RIGHT
               ::nDopMode := 0
            ELSEIF nKey == 120   // x
               FOR i := Val( cDopMode ) TO 1 STEP -1
                  ::DelText( n, ::nPos, n, ::nPos )
               NEXT
               ::nDopMode := 0
            ELSEIF nKey == 99 .OR. nKey == 100 .OR. nKey == 60 .OR. nKey == 62  // c, d, <, >
               ::nDopMode := nKey
            ELSEIF !( nKey >= 48 .AND. nKey <= 57 )
               ::nDopMode := 0
            ENDIF
            EXIT
         CASE 99   // c
         CASE 100  // d
            IF nKey == 100    // d
               IF ::nDopMode == 100
                  x := Iif( IsDigit( cDopMode ), Val( cDopMode ), 1 )
                  FOR i := 1 TO x
                     IF n > 0 .AND. n <= Len( ::aText )
                        ::DelText( n, 0, n+1, 0 )
                     ENDIF
                  NEXT
                  edi_SetLastOper( cDopMode+Chr(nKey), .T. )
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 99    // c
               IF ::nDopMode == 99
                  x := Iif( IsDigit( cDopMode ), Val( cDopMode ), 1 )
                  FOR i := 1 TO x
                     IF n > 0 .AND. n <= Len( ::aText )
                        ::DelText( n, 1, n, cp_Len(::lUtf8,::aText[n]) )
                     ENDIF
                  NEXT
                  edi_ChgMode( Self, 0 )
                  edi_SetLastOper( cDopMode+Chr(nKey) )
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 105    // i
               cDopMode += Chr( nKey )

            ELSEIF (nKey == 98 .OR. nKey == 66) .AND. !(cDopMode $ "di;ci")    // b, B
               edi_SetLastOper( cDopMode+Chr(nKey), (::nDopMode == 100) )
               IF IsDigit( cDopMode )
                  x := Val( cDopMode )
                  i := Iif( ( i := At( 'd',cDopMode ) ) == 0, At( 'á',cDopMode ), i )
                  cDopMode := Substr( cDopMode, i )
               ELSE
                  x := 1
               ENDIF
               IF cDopMode $ "cd"
                  mnu_F3( Self )
                  FOR i := 1 TO x
                     edi_PrevWord( Self, (nKey == 66) )
                     ::nby2 := ::nLine
                     ::nbx2 := ::nPos
                     cbDele( Self )
                  NEXT
                  IF ::nDopMode == 99
                     edi_ChgMode( Self, 0 )
                  ENDIF
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 101 .OR. nKey == 69   // e, E
               edi_SetLastOper( cDopMode+Chr(nKey), (::nDopMode == 100) )
               IF IsDigit( cDopMode )
                  x := Val( cDopMode )
                  i := Iif( ( i := At( 'd',cDopMode ) ) == 0, At( 'á',cDopMode ), i )
                  cDopMode := Substr( cDopMode, i )
               ELSE
                  x := 1
               ENDIF
               IF cDopMode $ "cd"
                  mnu_F3( Self )
                  FOR i := 1 TO x
                     edi_NextWord( Self, (nKey == 69), .T. )
                     edi_GoRight( Self )
                     ::nby2 := ::nLine
                     ::nbx2 := ::nPos
                     cbDele( Self )
                  NEXT
                  IF ::nDopMode == 99
                     edi_ChgMode( Self, 0 )
                  ENDIF
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 119 .OR. nKey == 87   // w, W
               edi_SetLastOper( cDopMode+Chr(nKey), (::nDopMode == 100) )
               IF IsDigit( cDopMode )
                  x := Val( cDopMode )
                  i := Iif( ( i := At( 'd',cDopMode ) ) == 0, At( 'á',cDopMode ), i )
                  cDopMode := Substr( cDopMode, i )
               ELSE
                  x := 1
               ENDIF
               IF cDopMode $ "cd"
                  mnu_F3( Self )
                  FOR i := 1 TO x
                     edi_NextWord( Self, (nKey == 87) )
                     ::nby2 := ::nLine
                     ::nbx2 := ::nPos
                     cbDele( Self )
                     ::lF3 := .F.
                  NEXT
                  IF ::nDopMode == 99
                     edi_ChgMode( Self, 0 )
                  ENDIF
               ELSEIF cDopMode $ "di;ci"
                  edi_SelectW( Self, (nKey == 87) )
                  cbDele( Self )
                  ::lF3 := .F.
                  IF ::nDopMode == 99
                     edi_ChgMode( Self, 0 )
                  ENDIF
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 34 .OR. nKey == 39  // ",'
               IF cDopMode $ "di;ci"
                  edi_SetLastOper( cDopMode+Chr(nKey), (::nDopMode == 100) )
                  IF ( j := hb_At( Chr(nKey), ::aText[n], ::nPos+1 ) ) > 0
                     IF ( i := hb_Rat( Chr(nKey), ::aText[n],, ::nPos ) ) > 0
                        ::nPos := i + 1
                        ::DelText( ::nLine, i+1, ::nLine, j-1 )
                        nKey := K_RIGHT
                     ELSEIF ( i := hb_At( Chr(nKey), ::aText[n], j+1 ) ) > 0
                        ::nPos := j + 1
                        ::DelText( ::nLine, j+1, ::nLine, i-1 )
                        nKey := K_RIGHT
                     ENDIF
                  ENDIF
                  IF nKey == K_RIGHT
                     IF ::nDopMode == 99
                        edi_ChgMode( Self, 0 )
                     ENDIF
                  ENDIF
               ENDIF
               ::nDopMode := 0
            ELSEIF Chr(nKey) $ "bB()[]{}"
               IF !Empty( x := edi_Bracket( Self, .T., .F., ;
                     Iif(nKey==91.OR.nKey==93, ']', Iif(nKey==66.OR.nKey==123.OR.nKey==125,'}',')')) ) )
                  edi_SetLastOper( cDopMode+Chr(nKey), (::nDopMode == 100) )
                  i := ::nLine; j := ::nPos
                  ::nLine := Iif( Valtype( x ) == "A", x[1], ::nLine )
                  ::nPos := Iif( Valtype( x ) == "A", x[2], x )
                  IF !Empty( s := edi_Bracket( Self, .T., .T. ) )
                     i := Iif( Valtype( x ) == "A", x[1], ::nLine )
                     j := Iif( Valtype( x ) == "A", x[2], x ) + 1
                     x := Iif( Valtype( s ) == "A", s[1], ::nLine )
                     s := Iif( Valtype( s ) == "A", s[2], s ) - 1
                     ::DelText( i, j, x, s )
                     IF ::nDopMode == 99
                        edi_ChgMode( Self, 0 )
                     ENDIF
                  ELSE
                     ::nLine := i; ::nPos := j
                  ENDIF
               ENDIF
               ::nDopMode := 0
            ELSE
               ::nDopMode := 0
            ENDIF
            EXIT
         CASE 121  // y
            IF Left( cDopMode, 2 ) != "yi"
               IF nKey == 121
                  ::nby1 := ::nLine; ::nbx1 := 1
                  ::nby2 := Iif( (i := Val(cDopMode)) > 0, ::nLine + i - 1, ::nLine )
                  ::nbx2 := cp_Len( ::lUtf8, ::aText[::nby2] ) + 1
                  edi_2cb( Self )
                  ::nby1 := ::nby2 := -1
                  ::nDopMode := 0
               ELSEIF nKey == 105  // i
                  cDopMode += Chr( nKey )
               ELSE
                  ::nDopMode := 0
               ENDIF
               EXIT
            ENDIF
         CASE 118  // v
            IF nKey == 87 .OR. nKey == 119  // w, W
               edi_SelectW( Self, (nKey == 87) )
               IF "yi" $ cDopMode
                  edi_2cb( Self, nLastReg )
                  ::nby1 := ::nby2 := -1
               ELSE
                  nKey := K_RIGHT
                  lNoDeselect := .T.
               ENDIF
            ELSEIF nKey == 34 .OR. nKey == 39 // ",'
               IF ( j := hb_At( Chr(nKey), ::aText[n], ::nPos+1 ) ) > 0
                  IF ( i := hb_Rat( Chr(nKey), ::aText[n],, ::nPos ) ) > 0
                     ::nby1 := ::nby2 := ::nLine
                     ::nbx1 := i + 1; ::nbx2 := ::nPos := j
                     nKey := K_RIGHT
                  ELSEIF ( i := hb_At( Chr(nKey), ::aText[n], j+1 ) ) > 0
                     ::nby1 := ::nby2 := ::nLine
                     ::nbx1 := j+1; ::nbx2 := ::nPos := i
                     nKey := K_RIGHT
                  ENDIF
               ENDIF
               IF nKey == K_RIGHT
                  IF "yi" $ cDopMode
                     edi_2cb( Self, nLastReg )
                     ::nby1 := ::nby2 := -1
                  ELSE
                     edi_SetPos( Self )
                     lNoDeselect := .T.
                  ENDIF
               ENDIF
            ELSEIF Chr(nKey) $ "bB()[]{}"
               IF !Empty( x := edi_Bracket( Self, .T., .F., ;
                     Iif(nKey==91.OR.nKey==93, ']', ;
                     Iif(nKey==66.OR.nKey==123.OR.nKey==125,'}',')')) ) )
                  i := ::nLine; j := ::nPos
                  ::nLine := Iif( Valtype( x ) == "A", x[1], ::nLine )
                  ::nPos := Iif( Valtype( x ) == "A", x[2], x )
                  IF !Empty( s := edi_Bracket( Self, .T., .T. ) )
                     ::nby1 := Iif( Valtype( x ) == "A", x[1], ::nLine )
                     ::nbx1 := Iif( Valtype( x ) == "A", x[2], x ) + 1
                     ::nby2 := Iif( Valtype( x ) == "A", s[1], ::nLine )
                     ::nbx2 := Iif( Valtype( x ) == "A", s[2], s )
                     IF "yi" $ cDopMode
                        edi_2cb( Self, nLastReg )
                        ::nby1 := ::nby2 := -1
                     ELSE
                        nKey := K_RIGHT
                        ::nLine := ::nby2; ::nPos := ::nbx2
                        lNoDeselect := .T.
                     ENDIF
                  ELSE
                     ::nLine := i; ::nPos := j
                  ENDIF
               ENDIF
            ELSEIF nKey == 115 // s
               ::nby1 := ::nby2 := ::nLine
               ::nbx1 := Iif( ( i := cp_Rat( ::lUtf8, '.', ::aText[::nLine],, ::nPos-1 ) ) > 0, i+1, 1 )
               ::nbx2 := Iif( ( i := cp_At( ::lUtf8, '.', ::aText[::nLine], ::nbx1+1 ) ) > 0, i, ;
                  cp_Len(::lUtf8, ::aText[::nLine] ) ) + 1
               IF "yi" $ cDopMode
                  edi_2cb( Self, nLastReg )
                  ::nby1 := ::nby2 := -1
               ELSE
                  nKey := K_RIGHT
                  ::GoTo( , ::nbx2 )
                  lNoDeselect := .T.
               ENDIF
            ENDIF
            ::nDopMode := 0
            EXIT
         CASE 114  // r
            IF ::nby1 >= 0 .AND. ::nby2 >= 0
               edi_FillSelected( Self, cp_Chr(::lUtf8,hb_keyStd(nKeyExt)) )
            ELSE
               ::InsText( n, ::nPos, cp_Chr(::lUtf8,hb_keyStd(nKeyExt)), .T. )
            ENDIF
            edi_SetLastOper( cDopMode+Chr(nKey), .T. )
            ::nDopMode := 0
            EXIT
         CASE 103  // g
            IF nKey == 103    // g
               IF Val( cDopMode ) > 0
                  ::Goto( Val( cDopMode ) )
                  ::nDopMode := 0
               ELSE
                  ::Goto( 1 )
               ENDIF
            ELSEIF nKey == 105    // i
               IF ::nUndo > 0
                  ::GoTo( ::aUndo[::nUndo][UNDO_LINE2], ::aUndo[::nUndo][UNDO_POS2] )
               ENDIF
            ENDIF
            ::nDopMode := 0
            EXIT
         CASE 60  // <
         CASE 62  // >
            IF nKey == ::nDopMode
               edi_Indent( Self, (nKey==62), Val(cDopMode) )
            ENDIF
            ::nDopMode := 0
            EXIT
         CASE 64   // @ - Macro playing
            ::nDopMode := 0
            IF ( n := Val( cDopMode ) ) == 0
               n := 1
            ENDIF
            IF nKey == 64 .AND. !Empty( nLastMacro )
               nKey := nLastMacro
            ENDIF
            IF hb_hHaskey( ::hMacros, nKey )
               nLastMacro := nKey
               x := ::hMacros[nKey]
               FOR j := 1 TO n
                  FOR i := 1 TO Len( x )
                     ::onKey( x[i] )
                  NEXT
               NEXT
            ENDIF
            EXIT
         CASE 90   // Z
            IF nKey == 90      // Z
               FOR i := Len( ::aWindows ) TO 1 STEP -1
                  IF ::aWindows[i]:lUpdated .AND. Empty(::aWindows[i]:cFileName)
                     edi_Alert( _I("Set file name") )
                     ::lShow := .F.
                     ::nCurr := i
                     nLastKey := nKeyExt
                     RETURN Nil
                  ENDIF
               NEXT
               FOR i := Len( ::aWindows ) TO 1 STEP -1
                  IF !( ::aWindows[i] == Self )
                     IF ::aWindows[i]:lUpdated
                        ::aWindows[i]:Save()
                     ENDIF
                     hb_ADel( ::aWindows, i, .T. )
                  ENDIF
               NEXT
               ::Save()
               mnu_Exit( Self )
            ELSEIF nKey == 81  // Q
               FOR i := Len( ::aWindows ) TO 1 STEP -1
                  IF !( ::aWindows[i] == Self )
                     hb_ADel( ::aWindows, i, .T. )
                  ENDIF
               NEXT
               ::lUpdated := ::lShow := .F.
               ::lClose := .T.
            ENDIF
            ::nDopMode := 0
            EXIT
         CASE K_CTRL_W
            IF nKey == 119   // w
               mnu_Windows( Self,, 1 )
            ELSEIF nKey == 115   // s  split window horizontally
               mnu_Windows( Self,, 4 )
            ELSEIF nKey == 118   // v  split window vertically
               mnu_Windows( Self,, 5 )
            ELSEIF nKey == 99    // c
               mnu_Windows( Self,, 3 )
            ELSEIF nKey == 111   // o
               mnu_Windows( Self,, 2 )
            ELSEIF nKey == 43    // +
            ELSEIF nKey == 45    // -
            ELSEIF nKey == 60    // <
            ELSEIF nKey == 62    // >
            ENDIF
            ::nDopMode := 0
            EXIT
         END
      ENDIF
      IF ::nDopMode == 0
         cDopMode := ""
      ENDIF
      lSkip := .T.
   ENDIF

   IF !lSkip
      lShift := ( hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0 .AND. ( ;
         Ascan( aKeysMove, nkey ) != 0 .OR. Ascan( aAltKeysMove, nkey ) != 0 ) )
      IF lShift
         IF ( i := Ascan( aAltKeysMove, nkey ) ) > 0
            nKey := aKeysMove[i]
         ENDIF
         IF !::lShiftKey
            ::nby1 := ::nLine
            ::nbx1 := ::nPos
            ::nSeleMode := Iif( hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0, 2, 0 )
            ::lShiftKey := .T.
         ENDIF
      ELSEIF !( ::lShiftKey .AND. nKey == 111 )
         ::lShiftKey := .F.
      ENDIF
      IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
         SWITCH nKey
         CASE K_ALT_F7
            mnu_SeaNext( Self, .F. )
            EXIT
         CASE K_ALT_F8
            mnu_GoTo( Self )
            ::lTextOut := .T.
            EXIT
         CASE K_ALT_B
            ::GoTo( ::nLineBack, ::nPosBack )
            EXIT
         CASE K_ALT_M
            ::nDopMode := 109
            cDopMode := "m"
            EXIT
         CASE K_ALT_QUOTE
            ::nDopMode := 39
            cDopMode := "'"
            EXIT
         CASE K_ALT_SC
            ::nDopMode := 34
            cDopMode := '"'
            EXIT
        CASE K_ALT_BS
            ::Undo()
            EXIT
        CASE K_ALT_3
            edi_SeaWord( Self, .T. )
            EXIT
        CASE K_ALT_8
            edi_SeaWord( Self, .F. )
            EXIT
        END
      ENDIF
      IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0

         lCtrl := .T.
         SWITCH nKey
         CASE K_CTRL_INS
         CASE 3                           // Ctrl-Ins or Ctrl-c
            edi_2cb( Self )
            nKey := K_CTRL_INS
            lNoDeselect := .T.
            EXIT
         CASE 22                          // Ctrl-v
            IF ::nMode == 1
               mnu_F3( Self, 2 )
               nKey := K_RIGHT
            ELSEIF !::lReadOnly
               cb2Text( Self, nLastReg )
            ENDIF
            EXIT
         CASE K_CTRL_P
            cb2Text( Self )
            EXIT
         CASE K_CTRL_X
            edi_2cb( Self )
            cbDele( Self )
            EXIT
         CASE K_CTRL_Q
            IF hb_keyVal( nKeyExt ) == 81 .AND. ::nDefMode >= 0
               edi_ChgMode( Self )
            ENDIF
            EXIT
         CASE K_CTRL_Y
            IF !::lReadOnly .AND. ::nMode == 0 .AND. n > 0 .AND. n <= Len( ::aText )
               ::DelText( n, 1, n+1, 1 )
            ENDIF
            EXIT
         CASE K_CTRL_A
            IF !::lF3
               ::nby1 := ::nbx1 := 1
               ::nby2 := Len( ::aText )
               ::nbx2 := cp_Len( ::lUtf8, ::aText[Len(::aText)] )
               ::nSeleMode := 0
               ::lF3 := .T.
            ENDIF
            EXIT
         CASE K_CTRL_TAB
            IF ::lCtrlTab
               IF Len( ::aWindows ) == 1
#ifndef _NO_HBC
                  Hbc( Self )
#endif
               ELSE
                  ::lShow := .F.
                  ::nCurr ++
               ENDIF
            ENDIF
            lNoDeselect := .T.
            EXIT
#ifndef _NO_HBC
         CASE K_CTRL_O
            FilePane():lConsMode := .T.
            Hbc( Self )
            EXIT
#endif
         CASE K_CTRL_PGUP
         CASE K_CTRL_HOME
            ::nPosBack := ::nPos
            ::nLineBack := ::nLine
            ::lTextOut := (::nyFirst>1 .OR. ::nxFirst>1)
            ::nxFirst := ::nyFirst := ::nxOfLine := 1
            edi_SetPos( Self, 1, 1 )
            EXIT
         CASE K_CTRL_PGDN
         CASE K_CTRL_END
            IF hb_keyVal( nKeyExt ) == 87  // Ctrl-W
#ifndef __BUILT_IN
               IF !::lBuiltIn
                  ::nDopMode := nKey
                  cDopMode := "W"
               ENDIF
#endif
            ELSE
               edi_Move( Self, 71 )
            ENDIF
            EXIT
         CASE K_CTRL_RIGHT
            IF hb_keyVal( nKeyExt ) == 66 // Ctrl-B
               IF ::nMode == 1
                  edi_Move( Self, K_CTRL_B )
               ELSE
                  edi_Bracket( Self )
               ENDIF
            ELSEIF hb_keyVal( nKeyExt ) == 16
               edi_NextWord( Self, .F. )
            ENDIF
            EXIT
         CASE K_CTRL_LEFT
            IF hb_keyVal( nKeyExt ) == 90  // Ctrl-Z
               ::Undo()
            ELSEIF hb_keyVal( nKeyExt ) == 15
               edi_PrevWord( Self, .F. )
            ENDIF
            EXIT
         CASE K_CTRL_F
            IF ::nMode == 1
               edi_Move( Self, K_CTRL_F )
            ENDIF
            EXIT
         CASE K_CTRL_F3
            mnu_F3( Self, 2 )
            nKey := K_RIGHT
            EXIT
#ifndef __BUILT_IN
         CASE K_CTRL_F4
            IF !::lBuiltIn
               mnu_OpenFile( Self )
               ::lTextOut := .T.
            ENDIF
            EXIT
#endif
         CASE K_CTRL_F7
            mnu_SeaAndRepl( Self )
            ::lTextOut := .T.
            EXIT
         CASE K_CTRL_BS
            IF ::nUndo > 0 .AND. ::aUndo[::nUndo,UNDO_OPER] == UNDO_OP_INS
               ::nby1 := ::aUndo[::nUndo,UNDO_LINE1]
               ::nbx1 := ::aUndo[::nUndo,UNDO_POS1]
               ::nby2 := ::aUndo[::nUndo,UNDO_LINE2]
               ::nbx2 := ::aUndo[::nUndo,UNDO_POS2]+1
               ::lF3 := .T.
               lNoDeselect := .T.
               ::lTextOut := .T.
            ENDIF
            EXIT
        END
      ELSE
         IF ( nKey >= K_SPACE .AND. nKey <= 255 ) .OR. ( ::lUtf8 .AND. nKey > 3000 )
            IF ::nby1 >= 0 .AND. ::nby2 >= 0
               // Selection Mode
               nKey := edi_MapKey( Self, nKey )
               IF Chr(nKey) $ cKeysMove
                  edi_Move( Self, nKey )
                  nKey := K_RIGHT
               ELSE
                  SWITCH nKey
                  CASE 85   // U  Convert to upper case
                     edi_ConvertCase( Self, .T. )
                     lNoDeselect := .T.
                     EXIT
                  CASE 117   // u Convert to lower case
                     edi_ConvertCase( Self, .F. )
                     lNoDeselect := .T.
                     EXIT
                  CASE 99    // c Deletes selection and switch to Edit mode
                     IF ::nMode == 1
                        cbDele( Self )
                        edi_ChgMode( Self, 0 )
                     ENDIF
                     EXIT
                  CASE 100   // d Deletes selection
                     cbDele( Self )
                     EXIT
                  CASE 121   // y Copy to clipboard
                     edi_2cb( Self, nLastReg )
                     EXIT
                  CASE 62    // > Shift lines right
                     edi_Indent( Self, .T.,, .T. )
                     lNoDeselect := .T.
                     EXIT
                  CASE 60    // > Shift lines left
                     edi_Indent( Self, .F.,, .T. )
                     lNoDeselect := .T.
                     EXIT
                  CASE 34    // "
                     ::nDopMode := 34
                     cDopMode := '"'
                     nKey := K_RIGHT
                     lNoDeselect := .T.
                     EXIT
                  CASE 105   // i
                     ::nDopMode := 118
                     cDopMode := "vi"
                     nKey := K_RIGHT
                     lNoDeselect := .T.
                     EXIT
                  CASE 114   // r
                     ::nDopMode := 114
                     cDopMode := "r"
                     nKey := K_RIGHT
                     lNoDeselect := .T.
                     EXIT
                  CASE 111   // o
                     IF ::nLine == ::nby2 .AND. ::nPos == ::nbx2
                        ::GoTo( ::nby1, ::nbx1 )
                     ELSE
                        ::GoTo( ::nby2, ::nbx2 )
                     ENDIF
                     x := ::nby1; ::nby1 := ::nby2; ::nby2 := x
                     x := ::nbx1; ::nbx1 := ::nbx2; ::nbx2 := x
                     nKey := K_RIGHT
                     lNoDeselect := .T.
                     EXIT
                  CASE 109   // m
                     mnu_Sele( Self )
                     lNoDeselect := .T.
                     EXIT
                  CASE 46    // .
                     IF !Empty( aLastSeleOper )
                        aLastSeleOper[1]:exec( Self, aLastSeleOper[2] )
                     ENDIF
                     lNoDeselect := .T.
                     EXIT
                  CASE K_ESC
                     nKey := 0
                     EXIT
                  END
               ENDIF
            ELSEIF ::nMode == 1
               nKey := edi_MapKey( Self, nKey )
               IF Chr(nKey) $ cKeysMove
                  edi_Move( Self, nKey )
                  nKey := K_RIGHT
               ELSEIF nKey >= 49 .AND. nKey <= 57  // 1...9
                  ::nDopMode := 49
                  cDopMode := Chr( nKey )
               ELSE
                  SWITCH nKey
                  CASE 118   // v Start selection
                     mnu_F3( Self )
                     nKey := K_RIGHT
                     EXIT
                  CASE 86    // V Start selection
                     mnu_F3( Self, 1 )
                     nKey := K_RIGHT
                     EXIT
                  CASE 117   // u Undo
                     ::Undo()
                     EXIT
                  CASE 112   // p Insert clipboard after current column
                     IF !::lReadOnly
                        edi_SetPos( Self, ::nLine, ++::nPos )
                        cb2Text( Self, nLastReg )
                     ENDIF
                     EXIT
                  CASE 80    // P Insert clipboard
                     IF !::lReadOnly
                        cb2Text( Self, nLastReg )
                     ENDIF
                     EXIT
                  CASE 105   // i - to edit mode
                     edi_ChgMode( Self, 0 )
                     ::lIns := .T.
                     edi_SetLastOper( Chr(nKey) )
                     EXIT
                  CASE 73    // I - to edit mode
                     edi_Move( Self, 94 )
                     edi_ChgMode( Self, 0 )
                     ::lIns := .T.
                     edi_SetLastOper( Chr(nKey) )
                     EXIT
                  CASE 97    // a - to edit mode
                     edi_GoRight( Self )
                     edi_ChgMode( Self, 0 )
                     ::lIns := .T.
                     edi_SetLastOper( Chr(nKey) )
                     EXIT
                  CASE 65    // A - to edit mode
                     edi_ChgMode( Self, 0 )
                     edi_GoEnd( Self )
                     ::lIns := .T.
                     edi_SetLastOper( Chr(nKey) )
                     EXIT
                  CASE 82    // R - to edit mode
                     edi_ChgMode( Self, 0 )
                     ::lIns := .F.
                     edi_SetLastOper( Chr(nKey) )
                     EXIT
                  CASE 111   // o Insert line after current
                     ::InsText( n, cp_Len(::lUtf8,::aText[n])+1, Chr(10), .F. )
                     edi_ChgMode( Self, 0 )
                     ::lIns := .T.
                     edi_SetLastOper( Chr(nKey) )
                     EXIT
                  CASE 126   // ~ Invert case
                     x := cedi_Peek( ::lUtf8, ::aText[n], ::nPos )
                     IF ( s := cp_Upper( ::lUtf8, x ) ) != x .OR. ;
                           ( s := cp_Lower( ::lUtf8, x ) ) != x
                        ::InsText( n, ::nPos, s, .T. )
                     ENDIF
                     edi_SetLastOper( Chr(nKey), .T. )
                     EXIT
                  CASE 102   // f - find next char
                  CASE 70    // F - find previous char
                  CASE 109   // m - set bookmark
                  CASE 39    // ' - goto bookmark
                  CASE 99    // c - delete and edit
                  CASE 100   // d - delete
                  CASE 103   // g
                  CASE 113   // q - record macro
                  CASE 64    // w - play macro
                  CASE 114   // r - replace one char under cursor
                  CASE 90    // Z
                  CASE 60    // <
                  CASE 62    // >
                  CASE 34    // "
                  CASE 121   // y
                     ::nDopMode := nKey
                     cDopMode := Chr( nKey )
                     EXIT
                  CASE 120   // x - delete a char
                     ::DelText( n, ::nPos, n, ::nPos )
                     edi_SetLastOper( Chr(nKey) )
                     EXIT
                  CASE 37    // %  Go to matching parentheses
                     edi_Bracket( Self )
                     EXIT
                  CASE 72    // H
                     edi_SetPos( Self, ::nyFirst, ::nxFirst )
                     edi_Move( Self, 94 )
                     EXIT
                  CASE 77    // M
                     edi_SetPos( Self, Min( Len(::aText), ::nyFirst + Int((::y2-::y1)/2) ), ::nxFirst )
                     edi_Move( Self, 94 )
                     EXIT
                  CASE 76    // L
                     edi_SetPos( Self, Min( Len(::aText), ::nyFirst + ::y2-::y1 ), ::nxFirst )
                     edi_Move( Self, 94 )
                     EXIT
                  CASE 42    // *
                     edi_SeaWord( Self, .F. )
                     EXIT
                  CASE 35    // #
                     edi_SeaWord( Self, .T. )
                     EXIT
#ifndef _NO_CMDLINE
                  CASE 47    // /
                     IF ::lCmdMode
                        ::nMode := 2
                        ::WriteTopPane( 1 )
                        __KeyBoard( "/" )
                        mnu_CmdLine( Self )
                     ENDIF
                     EXIT
#endif
                  CASE 58    // :
                     IF ::nDefMode >= 0 .AND. ::lCmdMode
                        edi_ChgMode( Self, 2 )
                     ENDIF
                     EXIT
                  CASE 46    // .
                     edi_DoLastOper( Self )
                     EXIT
                  END
               ENDIF
            ELSE
               ::InsText( n, ::nPos, cp_Chr(::lUtf8,nKey), !::lIns )
               edi_SetLastOper( nKeyExt )
            ENDIF

         ELSE
            SWITCH nKey
            CASE K_ENTER
               IF !::lReadOnly .AND. ::nMode == 0
                  s := ""
                  IF hb_hGetDef( TEdit():options, "autoindent", .F. )
                     i := 0
                     DO WHILE ( x := cp_Substr( ::lUtf8, ::aText[n], ++i, 1 )  ) == " " .OR. x == cTab
                        s += x
                     ENDDO
                     IF !(s == "") .AND. ::nPos <= Len( s )
                        s := Left( s, ::nPos - 1 )
                     ENDIF
                  ENDIF
                  ::InsText( n, ::nPos, Chr(10) + s, .F. )
               ENDIF
               EXIT
            CASE K_DEL
               IF !::lReadOnly
                  IF ::nby1 >= 0 .AND. ::nby2 >= 0
                     IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0
                        edi_2cb( Self )
                     ENDIF
                     cbDele( Self )
                  ELSE
                     ::DelText( n, ::nPos, n, ::nPos )
                  ENDIF
               ENDIF
               EXIT
            CASE K_BS
               IF !::lReadOnly .AND. ::nMode == 0
                  IF ::nby1 >= 0 .AND. ::nby2 >= 0
                     IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0
                        edi_2cb( Self )
                     ENDIF
                     cbDele( Self )
                  ELSE
                     IF ::nPos == 1
                        IF n > 1
                           edi_GoUp( Self )
                           edi_GoEnd( Self )
                           ::DelText( n-1, ::nPos, n-1, ::nPos )
                        ENDIF
                     ELSEIF ::nPos <= cp_Len( ::lUtf8, ::aText[n] ) + 1
                        ::DelText( n, ::nPos-1, n, ::nPos-1 )
                     ELSE
                        edi_GoLeft( Self )
                     ENDIF
                  ENDIF
               ENDIF
               EXIT
            CASE K_TAB
               IF ::nMode == 0 .AND. !::lReadOnly .AND. ::lIns
                  IF hb_hGetDef( TEdit():options,"autocomplete", .F. ) .AND. hb_keyStd(nLastKey) != K_TAB
                     IF ::nPos > 1 .AND. cp_Substr( ::lUtf8, ::aText[n], ::nPos, 1 ) <= ' ' .AND. ;
                        cp_Substr( ::lUtf8, ::aText[n], ::nPos-1, 1 ) >= ' '
                        nLastSec := Seconds()
                        nLastKey := nKeyExt
                        IF edi_DoAuC( Self, .T. )
                           EXIT
                        ENDIF
                     ENDIF
                  ENDIF
                  IF hb_hGetDef( TEdit():options,"tabtospaces", .F. )
                     ::InsText( n, ::nPos, cTabStr, .F. )
                  ELSE
                     ::InsText( n, ::nPos, cTab, .F. )
                  ENDIF
               ENDIF
               EXIT
            CASE K_SH_TAB
               IF ::lCtrlTab
                  IF Len( ::aWindows ) == 1
#ifndef _NO_HBC
                     Hbc( Self )
#endif
                  ELSE
                     ::lShow := .F.
                     ::nCurr --
                  ENDIF
               ENDIF
               lNoDeselect := .T.
               EXIT
            CASE K_INS
               IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0
                  IF !::lReadOnly
                     cb2Text( Self, nLastReg)
                  ENDIF
               ELSE
                  ::lIns := !::lIns
               ENDIF
               EXIT
            CASE K_MWFORWARD
            CASE K_UP
               IF nKey == K_UP .OR. ( (nRow := MRow()) >= ::y1 .AND. ;
                  nRow <= ::y2 .AND. (nCol := MCol()) >= ::x1 .AND. nCol <= ::x2 )
                  edi_GoUp( Self )
               ENDIF
               EXIT
            CASE K_MWBACKWARD
            CASE K_DOWN
               IF nKey == K_DOWN .OR. ( (nRow := MRow()) >= ::y1 .AND. ;
                  nRow <= ::y2 .AND. (nCol := MCol()) >= ::x1 .AND. nCol <= ::x2 )
                  edi_GoDown( Self )
               ENDIF
               EXIT
            CASE K_LEFT
               IF ::nby1 >= 0 .AND. ::nby2 >= 0 .AND. !::lF3 .AND. !lShift
                  IF (::nLine != ::nby1 .OR. ::nPos != ::nbx1) .AND. ;
                     (::nby1 < ::nby2 .OR. ( ::nby1 == ::nby2 .AND. ::nbx1 < ::nbx2 ))
                     ::GoTo( ::nby1, ::nbx1 )
                  ENDIF
               ELSE
                  edi_GoLeft( Self )
               ENDIF
               EXIT
            CASE K_RIGHT
               IF ::nby1 >= 0 .AND. ::nby2 >= 0 .AND. !::lF3 .AND. !lShift
                  IF (::nLine != ::nby1 .OR. ::nPos != ::nbx1) .AND. ;
                     !(::nby1 < ::nby2 .OR. ( ::nby1 == ::nby2 .AND. ::nbx1 < ::nbx2 ))
                     ::GoTo( ::nby1, ::nbx1 )
                  ENDIF
               ELSE
                  edi_GoRight( Self )
               ENDIF
               EXIT
            CASE K_HOME
               edi_Move( Self, 48 )
               EXIT
            CASE K_END
               edi_GoEnd( Self )
               EXIT
            CASE K_PGUP
               edi_Move( Self, K_CTRL_B )
               EXIT
            CASE K_PGDN
               edi_Move( Self, K_CTRL_F )
               EXIT
            CASE K_LDBLCLK
            CASE K_LBUTTONDOWN
               IF ::nDopMode == 0
                  nCol := MCol()
                  nRow := MRow()
                  IF ::lTopPane .AND. nRow == ::y1-1 .AND. nCol < 8
                     IF ::nby1 >= 0 .AND. ::nby2 >= 0
                        mnu_Sele( Self )
                     ELSEIF !::lBuiltIn
#ifndef __BUILT_IN
                        mnu_Main( Self )
#endif
                     ENDIF
                     lNoDeselect := .T.
                     ::lTextOut := .T.
                  ELSEIF nRow >= ::y1 .AND. nRow <= ::y2 .AND. nCol >= ::x1 .AND. nCol <= ::x2
                     IF ::RowToLine(nRow) > Len(::aText)
                        nRow := Len(::aText) - ::nyFirst + ::y1
                     ENDIF
                     edi_SetPos( Self, ::RowToLine(nRow), ::ColToPos(nRow,nCol) )
                     IF ::nLine >= ::nby1 .AND. ::nLine <= ::nby2 .AND. ::nPos >= ::nbx1 ;
                        .AND. ::nPos < ::nbx2 .AND. Seconds()-nLastSec < 0.6
                        edi_SelectW( Self, .T. )
                        ::lF3 := .T.
                        nKey := K_RIGHT
                        lNoDeselect := .T.
                     ELSEIF nKey == K_LDBLCLK
                        edi_SelectW( Self, .F. )
                        ::lF3 := .T.
                        nKey := K_RIGHT
                        lNoDeselect := .T.
                     ENDIF
                  ELSEIF nRow >= ::aRectFull[1] .AND. nRow <= ::aRectFull[3] .AND. nCol >= ::aRectFull[2] .AND. nCol <= ::aRectFull[4]
                     IF ( x := edi_FindWindow( Self,, nRow, nCol ) ) != Nil
                        mnu_ToBuf( Self, x )
                        x:nLine := x:RowToLine(nRow)
                        IF x:nLine > Len( x:aText )
                           x:nLine := Len( x:aText )
                        ENDIF
                        x:nPos := x:ColToPos(x:LineToRow(x:nLine),nCol)
                     ENDIF
                  ELSE
                     nLastSec := Seconds()
                     nLastKey := nKeyExt
                     RETURN Nil
                  ENDIF
               ENDIF
               EXIT
            CASE K_RBUTTONDOWN
               IF ::nby1 >= 0 .AND. ::nby2 >= 0
                  mnu_Sele( Self )
               ENDIF
               EXIT
            CASE K_F1
               IF !::lBuiltIn
                  mnu_Help( Self )
                  ::lTextOut := .T.
                  edi_SetPos( Self )
               ENDIF
               EXIT
            CASE K_F2
               IF !::lBuiltIn
                  ::Save()
               ENDIF
               EXIT
            CASE K_SH_F2
               IF !::lBuiltIn
                  mnu_Save( Self, .T. )
               ENDIF
               EXIT
            CASE K_F3
               mnu_F3( Self )
               lNoDeselect := .T.
               nKey := K_RIGHT
               EXIT
#ifndef __BUILT_IN
            CASE K_F4
               IF !::lBuiltIn
                  mnu_F4( Self, {2, 6} )
                  ::lTextOut := .T.
               ENDIF
               EXIT
            CASE K_SH_F4
               IF !::lBuiltIn
                  mnu_NewBuf( Self )
                  ::lTextOut := .T.
               ENDIF
               EXIT
#endif
            CASE K_F7
               mnu_Search( Self )
               ::lTextOut := .T.
               EXIT
#ifndef __BUILT_IN
            CASE K_F8
               IF !::lBuiltIn
                  mnu_Syntax( Self, {2, 6} )
                  ::lTextOut := .T.
               ENDIF
               EXIT
#endif
            CASE K_F9
               IF ::nby1 >= 0 .AND. ::nby2 >= 0
                  mnu_Sele( Self )
               ELSEIF !::lBuiltIn
#ifndef __BUILT_IN
                  mnu_Main( Self )
#endif
               ENDIF
               nKey := K_RIGHT
               lNoDeselect := .T.
               ::lTextOut := .T.
               edi_SetPos( Self )
               EXIT
            CASE K_F10
            CASE K_ESC
               IF nKey == K_ESC .AND. ::nDefMode == 1
                  IF ::nMode == 0
                     edi_ChgMode( Self )
                  ENDIF
               ELSE
                  mnu_Exit( Self )
               ENDIF
               EXIT
#ifndef __BUILT_IN
            CASE K_F11
               IF !::lBuiltIn
                  mnu_Plugins( Self )
                  ::lTextOut := .T.
               ENDIF
               EXIT
            CASE K_F12
               IF !::lBuiltIn
                  mnu_Buffers( Self, {2, 6} )
                  ::lTextOut := .T.
               ENDIF
               EXIT
#endif
            CASE K_SH_F7
               mnu_SeaNext( Self, .T. )
               EXIT
            CASE K_SH_F8
               mnu_cPages( Self, {2,6} )
               ::lTextOut := .T.
               EXIT
            END
         ENDIF
      ENDIF

      IF ::lF3 .AND. nKey != K_MOUSEMOVE .AND. Ascan( aKeysMove, nKey ) == 0
         ::lF3 := .F.
      ENDIF
      IF (::lF3 .OR. lShift)
         IF !( lCtrl .AND. nKey == K_CTRL_A)
            ::nby2 := ::nLine
            IF ::nSeleMode == 1
               ::nbx2 := cp_Len( ::lUtf8, ::aText[::nLIne] ) + 1
            ELSE
               ::nbx2 := ::nPos
            ENDIF
         ENDIF
      ELSEIF !lNoDeselect
         IF ::nby1 >= 0 .AND. ::nby2 >= 0
            ::lTextOut := .T.
            ::nby1 := ::nby2 := -1
         ENDIF
      ENDIF
   ENDIF
   IF ::lTextOut .OR. (::nby1 >= 0 .AND. ::nby2 >= 0 .AND. nKey != K_MOUSEMOVE)
      ::TextOut()
   ENDIF

   IF !Empty( ::oHili ) .AND. ::oHili:hHili["bra"] .AND. !Empty( x := edi_Bracket( Self, .T., .T. ) )
      // highlighting matched parenthesis
      ::npy1 := ::nLine; ::npx1 := ::nPos
      ::npy2 := Iif( Valtype(x)=="A",x[1], ::npy1 ); ::npx2 := Iif( Valtype(x)=="A",x[2], x )
      SetColor( ::cColorBra )
      DevPos( ::LineToRow(::npy1), ::PosToCol(::npy1,::npx1) )
      DevOut( cp_Substr( ::lUtf8, ::aText[::npy1], ::npx1, 1 ) )
      IF ::npy2 >= ::nyFirst .AND. ::npy2 < ::nyFirst + ::y2 - ::y1 .AND. ;
            ::npx2 >= ::nxFirst .AND. ::npx2 < ::nxFirst + ::x2 - ::x1
         DevPos( ::LineToRow(::npy2), ::PosToCol(::npy2,::npx2) )
         DevOut( cp_Substr( ::lUtf8, ::aText[::npy2], ::npx2, 1 ) )
      ENDIF
      SetColor( ::cColor )
      DevPos( ::LineToRow(::npy1), ::PosToCol(::npy1,::npx1) )
   ENDIF

   nLastReg := 1
   IF !lAddLast
      lLastOper_Ended := .T.
   ENDIF
   ::WriteTopPane()
   nLastSec := Iif( nKeyExt == 0x41010018 .OR. nKeyExt == 0x41020018, 0, Seconds() )
   nLastKey := nKeyExt

   RETURN Nil

METHOD WriteTopPane( lClear ) CLASS TEdit

   LOCAL y := ::y1 - 1, nCol := Col(), nRow := Row(), nF9 := 0
   LOCAL cLen := Ltrim(Str(Len(::aText))), nchars := Len(cLen)

   SetColor( ::cColorPane )
   DispBegin()
   IF ::lTopPane
      Scroll( y, ::x1, y, ::x2 )
   ENDIF
   IF ::oParent != Nil .AND. ::x1 == ::oParent:x2 + 2
      Scroll( ::y1, ::x1-1, ::y2, ::x1-1 )
   ENDIF

   IF ::bWriteTopPane != Nil
      DispEnd()
      Eval( ::bWriteTopPane, Self, lClear, y )
   ELSE
      IF ::lTopPane
         IF Empty( lClear )
            DevPos( y, ::x1 )
            IF ::x2 - ::x1 > 54
               DevOut( "F9-"+_I("menu") )
               DevPos( y, ::x1+8 )
               nF9 := 8
            ENDIF
            DevOut( Iif( hb_hGetDef(::options,"pathinhead",.F.), ;
               NameShortcut(::cFileName,::nTopName,'~'), cp_Left( ::lUtf8, ;
               Iif( Left(::cFileName,1) == "$", ::cFileName, hb_fnameNameExt(::cFileName) ), ::nTopName ) ) )
            IF !Empty( cDopMode )
               DevPos( y, ::x1 )
               DevOut( Padr( cDopMode, 8 ) )
            ENDIF
            DevPos( y, ::x1 + nF9 + ::nTopName + 2 )
            DevOut( Iif( ::lUpdated, "* ", "  " ) + Lower( ::cp ) )
            DevPos( y, ::x1 + nF9 + ::nTopName + 12 )
            DevOut( PAdl(Ltrim(Str(::nLine)),nchars) + "/" + cLen )
            DevPos( y, ::x1 + nF9 + ::nTopName + 12 + nchars*2 + 3 )
            DevOut( "[" + Ltrim(Str(::PosToCol()-::x1+::nxFirst)) + "]" )
            SetColor( ::cColorWB )
            DevPos( y, ::x2-3 )
            IF ::lF3 .OR. (::nby1 >= 0 .AND. ::nby2 >= 0)
               DevOut( "Sele" )
            ELSE
               DevOut( Iif( ::nMode == 0, Iif( ::lReadOnly, "View", "Edit" ), ;
                  Iif( ::nMode == 1, " Vim", " Cmd" ) ) )
            ENDIF
         ENDIF
         DevPos( nRow, nCol )
      ENDIF
   ENDIF
   DispEnd()
   SetColor( ::cColor )

   RETURN Nil

METHOD RowToLine( nRow )  CLASS TEdit

   IF ::lWrap
      IF ::y1 > nRow
         RETURN 0
      ENDIF
      RETURN edi_CalcWrapped( Self, nRow,,, .F. )
   ENDIF
   RETURN nRow - ::y1 + ::nyFirst

METHOD ColToPos( nRow, nCol ) CLASS TEdit

   LOCAL nLine

   nCol := nCol - ::x1 + ::nxFirst

   IF ::lWrap
      IF ::y1 > nRow
         RETURN 0
      ENDIF
      edi_CalcWrapped( Self, nRow, @nCol,, .F. )
      RETURN nCol
   ENDIF
   RETURN Iif( (nLine := ::RowToLine(nRow)) > 0 .AND. nLine <= Len(::aText), ;
      edi_Col2Pos( Self, nLine, nCol ), 1 )

METHOD LineToRow( nLine, nPos ) CLASS TEdit

   nLine := Iif( nLine==Nil, ::nLine, nLine )
   IF ::lWrap
      IF nLine < ::nyFirst
         RETURN 0
      ENDIF
      nPos := Iif( nPos == Nil, ::nPos, nPos )
      RETURN edi_CalcWrapped( Self, nLine, nPos,, .T. )
   ENDIF
   RETURN nLine + ::y1 - ::nyFirst

METHOD PosToCol( nLine, nPos ) CLASS TEdit

   LOCAL nAdd := 0

   IF nPos == Nil; nPos := ::nPos; ENDIF
   IF nLine == Nil; nLine := ::nLine; ENDIF

   IF ::lWrap
      IF nLine < ::nyFirst
         RETURN 0
      ENDIF
      edi_CalcWrapped( Self, nLine, @nPos,, .T. )
   ELSE
      IF nPos > 1
         edi_ExpandTabs( Self, cp_Left(::lUtf8,::aText[nLine],nPos-1), 1, .T., @nAdd )
         nPos += nAdd
      ENDIF
   ENDIF

   RETURN nPos + ::x1 - ::nxFirst

METHOD Search( cSea, lCase, lNext, lWord, lRegex, ny, nx, lInc, nLenSea ) CLASS TEdit

   LOCAL lRes := .F., i, nLen := Len( ::aText ), nPos, nEnd, s

   nLenSea := cp_Len(::lUtf8,cSea)
   IF !lCase
      cSea := cp_Lower( ::lUtf8, cSea )
   ENDIF
   IF lNext
      s := cp_Substr( ::lUtf8, ::aText[ny], nx, nLenSea )
      IF Empty( lInc ) .AND. ( lRegex .OR. cSea == Iif( lCase, s, cp_Lower( ::lUtf8,s ) ) )
         nx ++
      ENDIF
      FOR i := ny TO nLen
         s := Iif( lCase, ::aText[i], cp_Lower( ::lUtf8, ::aText[i] ) )
         IF i > ny
            nx := 1
         ENDIF
         DO WHILE .T.
            nPos := nx; nEnd := Nil
            IF !lRegex .AND. ( nPos := cp_At( ::lUtf8, cSea, s, nx ) ) == 0
               EXIT
            ELSEIF lRegex .AND. hb_Atx( cSea, s, lCase, @nPos, @nEnd ) == Nil
               nPos := 0
               EXIT
            ENDIF
            IF lWord .AND. !lRegex
               IF ( nPos == 1 .OR. !edi_AlphaNum( cp_Asc( ::lUtf8, cedi_Peek(::lUtf8,s,nPos-1) ) ) ) ;
                  .AND. ( nPos+nLenSea > cp_Len( ::lUtf8, s ) .OR. !edi_AlphaNum( cp_Asc( ::lUtf8, cedi_Peek(::lUtf8,s,nPos+nLenSea) ) ) )
                  EXIT
               ELSE
                  nx ++
               ENDIF
            ELSEIF lRegex
               nLenSea := nEnd
               EXIT
            ELSE
               EXIT
            ENDIF
         ENDDO
         IF nPos > 0
            lRes := .T.; ny := i; nx := nPos
            EXIT
         ENDIF
      NEXT
   ELSE
      s := cp_Substr( ::lUtf8, ::aText[ny], nx, nLenSea )
      IF cSea == Iif( lCase, s, cp_Lower( ::lUtf8,s ) )
         nx --
      ENDIF
      FOR i := ny TO 1 STEP -1
         s := Iif( lCase, ::aText[i], cp_Lower( ::lUtf8, ::aText[i] ) )
         IF i < ny
            nx := cp_Len( ::lUtf8,::aText[i] )
         ENDIF
         DO WHILE ( nPos := cp_RAt( ::lUtf8, cSea, s, 1, nx ) ) > 0
            IF lWord
               IF ( nPos == 1 .OR. !edi_AlphaNum( cp_Asc( ::lUtf8, cedi_Peek(::lUtf8,s,nPos-1) ) ) ) ;
                  .AND. ( nPos+nLenSea > cp_Len( ::lUtf8, s ) .OR. !edi_AlphaNum( cp_Asc( ::lUtf8, cedi_Peek(::lUtf8,s,nPos+nLenSea) ) ) )
                  EXIT
               ELSE
                  nx --
               ENDIF
            ELSE
               EXIT
            ENDIF
         ENDDO
         IF nPos > 0
            lRes := .T.; ny := i; nx := nPos
            EXIT
         ENDIF
      NEXT
   ENDIF

   RETURN lRes

METHOD GoTo( ny, nx, nSele, lNoGo ) CLASS TEdit

   LOCAL lTextOut := .F., nRowOld

   IF ny == Nil; ny := ::nLine; ENDIF
   IF nx == Nil; nx := 1; ENDIF
   IF ny > Len(::aText)
      RETURN Nil
   ENDIF

   IF Empty( lNoGo ) .AND. ( ny != ::nLine .OR. nx != ::nPos )
      ::nPosBack := ::nPos
      ::nLineBack := ::nLine
   ENDIF
   IF ::lWrap
      ::nyFirst := ny
      ::nxOfLine := 1
      lTextOut := .T.
   ELSE
      IF ny < ::nyFirst .OR. ny > ::nyFirst + (::y2-::y1)
         ::nyFirst := Max( ny-3, 1 )
         lTextOut := .T.
      ENDIF
      IF nx < ::nxFirst .OR. nx > ::nxFirst + (::x2-::x1)
         ::nxFirst := Iif( nx < ::x2-::x1, 1, nx - Int((::x2-::x1)*0.8) )
         lTextOut := .T.
      ENDIF
   ENDIF
   IF nSele != Nil .AND. nSele > 0
      ::nby1 := ::nby2 := ny; ::nbx1 := nx; ::nbx2 := nx + nSele
   ENDIF

   IF Empty( lNoGo )
      SetColor( ::cColor )
      IF lTextOut
         ::TextOut()
      ELSE
         IF nSele != Nil .AND. nSele > 0 .AND. ( nRowOld := (::nLine - ::nyFirst + 1) ) > 0
            ::LineOut( nRowOld )
         ENDIF
         ::LineOut( ny - ::nyFirst + 1 )
      ENDIF
      edi_SetPos( Self, ny, nx )
      ::WriteTopPane()
   ELSE
      ::nLine := ny
      ::nPos := nx
   ENDIF

   RETURN Nil

METHOD ToString( cEol, cp ) CLASS TEdit

   LOCAL i, nLen := Len( ::aText ), cBom := e"\xef\xbb\xbf", s := Iif( ::lBom, cBom, "" )
   LOCAL lTrim := hb_hGetDef( TEdit():options,"trimspaces", .F. )

   IF cEol == Nil
      cEol := ::cEol
   ENDIF
   IF Empty( ::aText[nLen] )
      nLen --
   ENDIF
   FOR i := 1 TO nLen
      IF lTrim .AND. Right( ::aText[i], 1 ) == " "
         ::aText[i] := Trim( ::aText[i] )
      ENDIF
      IF cp != Nil .AND. !( cp == ::cp )
         s += hb_strToUtf8( ::aText[i], ::cp ) + Iif( i<nLen, cEol, "" )
      ELSE
         s += ::aText[i] + Iif( i<nLen, cEol, "" )
      ENDIF
   NEXT

   RETURN s

METHOD Save( cFileName ) CLASS TEdit

   LOCAL dDateMod := ::dDateMod, cTimeMod := ::cTimeMod, cPath

   IF cFileName == Nil
      cFileName := ::cFileName
   ENDIF
   cPath := edi_CurrPath()
   IF Empty( cFileName )
      cFileName := edi_SaveDlg( Self, cPath )
      ::lTextOut := .T.
   ENDIF

   IF Empty( cFileName )
      RETURN .F.
   ELSE
      IF Left(cFileName,1) == "$" .OR. Empty( hb_fnameDir( cFileName ) )
         cFileName := cPath + cFileName
      ENDIF
      IF Left(::cFileName,1) != "$"
         ::cFileName := cFileName
      ENDIF
   ENDIF

   IF !Empty( ::dDateMod ) .AND. hb_fGetDateTime( ::cFileName, @dDateMod, @cTimeMod ) .AND. ;
      ( ::dDateMod != dDateMod .OR. ::cTimeMod != cTimeMod ) .AND. ;
      edi_Alert( _I("File was modified by other program!"), _I("Save"), _I("Cancel") ) != 1
      RETURN .F.
   ENDIF

   IF Empty( ::funSave )
      hb_MemoWrit( cFileName, ::ToString() )
      IF hb_fGetDateTime( ::cFileName, @dDateMod, @cTimeMod )
         ::dDateMod := dDateMod
         ::cTimeMod := cTimeMod
      ENDIF
   ELSE
      ::funSave:exec( cFileName, ::ToString(), Self )
   ENDIF
   ::lUpdated := .F.

   RETURN .T.

METHOD InsText( nLine, nPos, cText, lOver, lChgPos, lNoUndo ) CLASS TEdit

   LOCAL arr, i, nLine2, nPos2, cTemp, cTextOld, nLineNew, nPosNew, nCol

   IF ::lReadOnly
      RETURN Nil
   ENDIF
   IF lOver == Nil; lOver := .F.; ENDIF
   IF lChgPos == Nil; lChgPos := .T.; ENDIF
   IF nLine > Len( ::aText )
      Aadd( ::aText, "" )
      nLine := Len( ::aText )
      nPos := 1
   ELSEIF ( i := (nPos - cp_Len(::lUtf8,::aText[nLine])) ) > 0
      nPos -= (i-1)
      IF lChgPos
         ::nPos -= (i-1)
      ENDIF
      cText := Space( i-1 ) + cText
   ENDIF
   nLine2 := nLine
   IF Chr(10) $ cText
      arr := hb_ATokens( cText, Chr(10) )
      IF lOver
         cTextOld := cp_Substr( ::lUtf8, ::aText[nLine], nPos ) + Chr(10)
         ::aText[nLine] := cp_Left( ::lUtf8, ::aText[nLine], nPos-1 ) + arr[1]
         FOR i := 2 TO Len(arr)-1
            cTextOld += ::aText[nLine+i-1] + Chr(10)
            ::aText[nLine+i-1] := arr[i]
            nLine2 ++
         NEXT
         cTextOld += cp_Left( ::lUtf8, ::aText[nLine+i-1], cp_Len( ::lUtf8,arr[i] ) )
         ::aText[nLine+i-1] := arr[i] + cp_Substr( ::lUtf8, ::aText[nLine+i-1], ;
            cp_Len(::lUtf8,arr[i]) + 1 )
         nLine2 ++
      ELSE
         cTemp := cp_Substr( ::lUtf8, ::aText[nLine], nPos )
         ::aText[nLine] := cp_Left( ::lUtf8, ::aText[nLine], nPos-1 ) + arr[1]
         FOR i := 2 TO Len(arr)-1
            hb_AIns( ::aText, nLine+i-1, arr[i], .T. )
            nLine2 ++
         NEXT
         hb_AIns( ::aText, nLine+i-1, arr[i] + cTemp, .T. )
         nLine2 ++
      ENDIF
      nPos2 := Max( cp_Len( ::lUtf8, arr[i] ), 1 )
      ::lTextOut := .T.
      IF lChgPos
         nLineNew := nLine + i - 1
         IF nLineNew - ::nyFirst + 1 > ::y2 - ::y1 + 1
            ::nyFirst := nLineNew - (::y2 - ::y1)
         ENDIF
         nPosNew := cp_Len( ::lUtf8, arr[i] ) + 1
         IF !::lWrap
            IF nPosNew - ::nxFirst + 1 > ::x2 - ::x1 + 1
               ::nxFirst := nPosNew - 3
            ELSEIF nPosNew < ::nxFirst
               ::nxFirst := 1
            ENDIF
         ENDIF
         edi_SetPos( Self, nLineNew, nPosNew )
      ENDIF
   ELSE
      i := cp_Len( ::lUtf8, cText )
      IF lOver
         cTextOld := cp_Substr( ::lUtf8, ::aText[nLine], nPos, i )
      ENDIF
      ::aText[nLine] := cp_Left( ::lUtf8, ::aText[nLine], nPos-1 ) + cText + ;
         cp_Substr( ::lUtf8, ::aText[nLine], nPos + Iif(lOver,i,0) )
      nPos2 := nPos + cp_Len( ::lUtf8, cText ) - 1
      IF lChgPos
         ::nPos += i
      ENDIF
      IF ( nCol := ::PosToCol() ) > ::x2
         IF lChgPos
            i := nCol - ::x2
            ::nxFirst += i
            ::nPos := ::ColToPos( ::LineToRow(),::x2 )
         ENDIF
         ::lTextOut := .T.
      ELSE
         IF !::lTextOut
            IF ::lWrap
               ::lTextOut := .T.
            ELSE
               ::LineOut( ::nLine - ::nyFirst + 1 )
            ENDIF
         ENDIF
         IF lChgPos
            edi_SetPos( Self )
         ENDIF
      ENDIF
   ENDIF

   IF Empty( lNoUndo )
      ::Undo( nLine, nPos, nLine2, nPos2, Iif( lOver,UNDO_OP_OVER,UNDO_OP_INS ), ;
         Iif( lOver,cTextOld,Nil ) )
   ENDIF
   IF !Empty( ::oHili )
      ::oHili:UpdSource( nLine )
   ENDIF
   ::lUpdated := .T.

   RETURN Nil

METHOD DelText( nLine1, nPos1, nLine2, nPos2, lNoUndo ) CLASS TEdit

   LOCAL i, n, ncou := 0, cTextOld

   IF ::lReadOnly
      RETURN Nil
   ENDIF
   IF nLine1 == nLine2
      IF nPos1 == nPos2 .AND. nPos1 > cp_Len( ::lUtf8, ::aText[nLine1] )
         cTextOld := Chr(10)
         IF nLine1 < Len( ::aText )
            ::aText[nLine1] += ::aText[nLine1+1]
            hb_ADel( ::aText, nLine1+1, .T. )
            ::lTextOut := .T.
         ENDIF
      ELSE
         cTextOld := cp_Substr( ::lUtf8, ::aText[nLine1], nPos1, nPos2-nPos1+1 )
         ::aText[nLine1] := cp_Left( ::lUtf8, ::aText[nLine1], nPos1-1 ) + ;
            cp_Substr( ::lUtf8, ::aText[nLine1], nPos2+1 )
         IF !::lTextOut
            IF ::lWrap
               ::lTextOut := .T.
            ELSE
               ::LineOut( nLine1 -::nyFirst + 1 )
            ENDIF
         ENDIF
         edi_SetPos( Self, ::nLine, nPos1 )
      ENDIF
   ELSE
      IF nPos1 > 1
         cTextOld := cp_Substr( ::lUtf8, ::aText[nLine1], nPos1 ) + Chr(10)
         ::aText[nLine1] := cp_Left( ::lUtf8, ::aText[nLine1], nPos1-1 )
      ELSE
         cTextOld := ::aText[nLine1] + Chr(10)
         ::aText[nLine1] := ""
      ENDIF
      IF nLine1 < Len( ::aText )
         n := nLine1 + 1
         FOR i := nLine1+1 TO nLine2-1
            cTextOld += ::aText[n] + Chr(10)
            ADel( ::aText, n )
            ncou ++
         NEXT

         IF nPos2 > 1
            cTextOld += cp_Left( ::lUtf8, ::aText[n], nPos2 )
            ::aText[nLine1] += cp_Substr( ::lUtf8, ::aText[n], nPos2+1 )
         ELSE
            ::aText[nLine1] += ::aText[n]
         ENDIF
      ELSE
         n := nLine1
      ENDIF
      ADel( ::aText, n )
      ncou ++

      ::aText := ASize( ::aText, Len(::aText) - ncou )
      IF !( ( i := (nLine1 - ::nyFirst + 1) ) > 0 .AND. i < (::y2-::y1+1) )
         ::nyFirst := nLine1
      ENDIF
      edi_SetPos( Self, nLine1, Max( nPos1,1 ) )
      ::lTextOut := .T.
   ENDIF

   ::aCBoards[CBOARD_MINUS,1] := cTextOld
   ::aCBoards[CBOARD_MINUS,2] := ::cp
   ::aCBoards[CBOARD_MINUS,3] := Nil
   IF Empty( lNoUndo )
      ::Undo( nLine1, nPos1, nLine2, nPos2, UNDO_OP_DEL, cTextOld )
   ENDIF
   IF !Empty( ::oHili )
      ::oHili:UpdSource( nLine1 )
   ENDIF
   ::lUpdated := .T.

   RETURN Nil

METHOD Undo( nLine1, nPos1, nLine2, nPos2, nOper, cText ) CLASS TEdit

   LOCAL alast, nOpLast := 0, arrnew, i, lNoGo

   IF ::lReadOnly
      RETURN Nil
   ENDIF
   IF ::nUndo>0
      alast := ::aUndo[::nUndo]
      nOpLast := alast[UNDO_OPER]
   ENDIF
   IF Valtype( nLine1 ) != "N"
      lNoGo := ( Valtype( nLine1 ) == "L" )
      IF alast != Nil
         IF nOpLast == UNDO_OP_INS
            ::DelText( alast[UNDO_LINE1], alast[UNDO_POS1], alast[UNDO_LINE2], ;
               alast[UNDO_POS2], .T. )
            ::GoTo( alast[UNDO_LINE1], alast[UNDO_POS1],, lNoGo )

         ELSEIF nOpLast == UNDO_OP_OVER
            ::InsText( alast[UNDO_LINE1], alast[UNDO_POS1], alast[UNDO_TEXT], ;
               .T., .F., .T. )
            ::GoTo( alast[UNDO_LINE2], alast[UNDO_POS2],, lNoGo )

         ELSEIF nOpLast == UNDO_OP_DEL
            ::InsText( alast[UNDO_LINE1], alast[UNDO_POS1], alast[UNDO_TEXT], ;
               .F., .F., .T. )
            ::GoTo( alast[UNDO_LINE1], alast[UNDO_POS1],, lNoGo )

         ELSEIF nOpLast == UNDO_OP_SHIFT
            FOR i := alast[UNDO_LINE1] TO alast[UNDO_LINE2]
               IF alast[UNDO_TEXT] > 0
                  ::aText[i] := Substr( ::aText[i], alast[UNDO_TEXT]+1 )
               ELSEIF alast[UNDO_TEXT] < 0
                  ::aText[i] := Iif( Left(::aText[i],1) == cTab, ;
                     Replicate(cTab,Abs(alast[UNDO_TEXT])), Space(Abs(alast[UNDO_TEXT])) ) + ::aText[i]
               ENDIF
            NEXT
            ::GoTo( alast[UNDO_LINE2], 1,, lNoGo )
            ::lTextOut := .T.

         ENDIF
         ::aUndo[::nUndo] := Nil
         ::nUndo --
         IF nOpLast == UNDO_OP_END
            ::lTextOut := .T.
            DO WHILE ::nUndo > 0
               nOpLast := ::aUndo[::nUndo,UNDO_OPER]
               ::Undo( Iif( ::nUndo>1 .AND. ::aUndo[::nUndo-1,UNDO_OPER] != UNDO_OP_START, .T., Nil ) )
               IF nOpLast == UNDO_OP_START
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF ::nUndo == 0
            ::lUpdated := .F.
         ENDIF
      ENDIF

   ELSEIF nOper == UNDO_OP_INS .OR. nOper == UNDO_OP_OVER
      IF nOper == nOpLast .AND. alast[UNDO_LINE2] == nLine2 .AND. alast[UNDO_POS2] == nPos1-1 ;
         .AND. alast[UNDO_LINE1] == nLine2 .AND. nLine1 == nLine2
         alast[UNDO_POS2] := nPos2
         IF nOper == UNDO_OP_OVER
            alast[UNDO_TEXT] += cText
         ENDIF
      ELSE
         arrnew := {nLine1, nPos1, nLine2, nPos2, nOper, cText}
      ENDIF
   ELSEIF nOper == UNDO_OP_DEL
      IF nOper == nOpLast .AND. alast[UNDO_LINE2] == nLine2 .AND. ;
         ( alast[UNDO_POS2] == nPos1 .OR. alast[UNDO_POS1] == nPos1+1 ) ;
         .AND. alast[UNDO_LINE1] == nLine2 .AND. nLine1 == nLine2
         IF alast[UNDO_POS2] == nPos1    // Del
            alast[UNDO_TEXT] += cText
            alast[UNDO_POS2] := nPos2
         ELSE                            // Backspace
            alast[UNDO_TEXT] := cText + alast[UNDO_TEXT]
            alast[UNDO_POS1] := nPos2
         ENDIF
      ELSE
         arrnew := {nLine1, nPos1, nLine2, nPos2, nOper, cText}
      ENDIF

   ELSEIF nOper == UNDO_OP_SHIFT
      IF nOper == nOpLast .AND. alast[UNDO_LINE2] == nLine2 .AND. alast[UNDO_LINE1] == nLine1
         alast[UNDO_TEXT] += cText
      ELSE
         arrnew := {nLine1, nPos1, nLine2, nPos2, nOper, cText}
      ENDIF

   ELSEIF nOper == UNDO_OP_START .OR. nOper == UNDO_OP_END
      arrnew := {nLine1, nPos1, nLine2, nPos2, nOper, cText}
   ENDIF
   IF arrnew != Nil
      IF Len( ::aUndo ) < ++::nUndo
         ::aUndo := ASize( ::aUndo, Len(::aUndo) + UNDO_INC )
      ENDIF
      ::aUndo[::nUndo] := arrnew
   ENDIF

   RETURN Nil

METHOD Highlighter( oHili ) CLASS TEdit

   IF oHili == Nil
      ::oHili := Nil
   ELSE
      ::oHili := oHili:Set( Self )
   ENDIF
   RETURN Nil

METHOD OnExit() CLASS TEdit

   LOCAL i, j, s := "", nSaveHis := TEdit():options["savehis"]
   LOCAL aMacros, arr, sLine, cHisDir := hb_DirBase()

   IF nSaveHis > 0
      IF !Empty( TEdit():aSeaHis )
         s += "[SEARCH]" + Chr(13) + Chr(10)
         FOR i := 1 TO Len( TEdit():aSeaHis )
            s += "h" + PAdl(Ltrim(Str(i)),3,'0') + "=" + TEdit():aSeaHis[i] + Chr(13) + Chr(10)
         NEXT
      ENDIF

      IF !Empty( TEdit():aReplHis )
         s += Chr(13) + Chr(10) + "[REPLACE]" + Chr(13) + Chr(10)
         FOR i := 1 TO Len( TEdit():aReplHis )
            s += "h" + PAdl(Ltrim(Str(i)),3,'0') + "=" + TEdit():aReplHis[i] + Chr(13) + Chr(10)
         NEXT
      ENDIF

      IF !Empty( TEdit():aCmdHis )
         s += Chr(13) + Chr(10) + "[COMMANDS]" + Chr(13) + Chr(10)
         FOR i := 1 TO Len( TEdit():aCmdHis )
            s += "h" + PAdl(Ltrim(Str(i)),3,'0') + "=" + TEdit():aCmdHis[i] + Chr(13) + Chr(10)
         NEXT
      ENDIF

      IF !Empty( aMacros := ASort( hb_hKeys( ::hMacros ) ) )
         s += Chr(13) + Chr(10) + "[MACRO]" + Chr(13) + Chr(10)
         FOR i := 1 TO Len( aMacros )
            arr := ::hMacros[aMacros[i]]
            IF !Empty( arr )
               sLine := Chr( aMacros[i] ) + "="
               FOR j := 1 TO Len( arr )
                  sLine += edi_KeyNToC( arr[j] ) + ","
               NEXT
               sLine += Chr(13) + Chr(10)
               s += sLine
            ENDIF
         NEXT
      ENDIF

      IF !Empty( TEdit():aEditHis )
         s += Chr(13) + Chr(10) + "[EDIT]" + Chr(13) + Chr(10)
         FOR i := 1 TO Len( TEdit():aEditHis )
            s += "h" + PAdl(Ltrim(Str(i)),3,'0') + "=" + TEdit():aEditHis[i,2] + "," + ;
               Ltrim(Str(TEdit():aEditHis[i,3])) + "," + Ltrim(Str(TEdit():aEditHis[i,4])) + "," + ;
               TEdit():aEditHis[i,1] + Chr(13) + Chr(10)
         NEXT
      ENDIF

#ifdef __PLATFORM__UNIX
      IF hb_dirExists( sLine := ( hb_getenv( "HOME" ) + "/hbedit" ) )
         cHisDir := sLine + "/"
      ENDIF
#endif
      hb_MemoWrit( IIf( nSaveHis==1, cHisDir, "" ) + "hbedit.his", s )
   ENDIF

   IF !Empty( aLangs )
      FOR EACH i IN aLangs
         IF !Empty( j := hb_hGetDef( i, "htrie", Nil ) )
            trie_Close( j )
            //edi_Alert( i:__enumkey + " trie_Close" )
         ENDIF
      NEXT
   ENDIF
   /*
   IF !Empty( hIdle )
      hb_IdleDel( hIdle )
      hIdle := Nil
   ENDIF
   */

   RETURN Nil

FUNCTION NameShortcut( cName, nWidth, cIns, lUtf8 )

   LOCAL nLen

   lUtf8 := !Empty( lUtf8 )
   IF ( nLen := cp_Len( lUtf8, cName ) ) > nWidth
      cIns := Iif( cIns==Nil, "...", cIns )
      IF nWidth > Len(cIns) + 3
         cName := cp_Left( lUtf8, cName,3 ) + cIns + ;
            cp_Substr( lUtf8, cName, nLen-(nWidth-3-Len(cIns)) )
      ELSE
         cName := ""
      ENDIF
   ENDIF

   RETURN cName

FUNCTION edi_GetSelected( oEdit )

   LOCAL s := "", i, j, nby1, nby2, nbx1, nbx2, nvx1, nvx2

   IF oEdit:nby1 >= 0 .AND. oEdit:nby2 >= 0
      IF oEdit:nby1 < oEdit:nby2 .OR. ( oEdit:nby1 == oEdit:nby2 .AND. oEdit:nbx1 < oEdit:nbx2 )
         nby1 := oEdit:nby1; nbx1 := oEdit:nbx1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
      ELSE
         nby1 := oEdit:nby2; nbx1 := oEdit:nbx2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
      ENDIF
      IF nby1 == nby2
         s := cp_Substr( oEdit:lUtf8, oEdit:aText[nby1], nbx1, nbx2-nbx1 )
      ELSE
         FOR i := nby1 TO nby2
            IF oEdit:nSeleMode == 2
               nvx1 := nbx1; nvx2 := nbx2
               IF i != nby1
                  nvx1 := oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol( nby1,nbx1 ) )
               ENDIF
               IF i != nby2
                  nvx2 := oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol( nby2,nbx2 ) )
               ENDIF
               IF nvx1 > nvx2
                  j := nvx1; nvx1 := nvx2; nvx2 := j
               ENDIF
               s += cp_Substr( oEdit:lUtf8, oEdit:aText[i], nvx1, nvx2-nvx1 ) + Chr(10)
            ELSE
               IF i == nby1
                  s += cp_Substr( oEdit:lUtf8, oEdit:aText[i], nbx1 ) + Chr(10)
               ELSEIF i == nby2
                  s += cp_Left( oEdit:lUtf8, oEdit:aText[i], nbx2-1 )
               ELSE
                  s += oEdit:aText[i] + Chr(10)
               ENDIF
            ENDIF
         NEXT
      ENDIF
   ENDIF

   //RETURN Iif( oEdit:lTabs, Strtran(s,cTabStr,cTab), s )
   RETURN s

/*
 * cb2Text( oEdit, nReg, lToText, s, lVert )
 * If nReg == 0, s is a text inserted and lVert should be set.
 */
FUNCTION cb2Text( oEdit, nReg, lToText, s, lVert )

   LOCAL arr, aMenu_CB, nLen := 1
   LOCAL i, j, nPos, cPref

   IF lToText == Nil; lToText := .T.; ENDIF
   IF nReg == Nil
      nReg := 1
      FOR i := 2 TO MAX_CBOARDS
         IF !Empty( TEdit():aCBoards[i,1] )
            nLen ++
         ENDIF
      NEXT
   ELSEIF nReg >= 97 .AND. nReg <= 122
      nReg -= 95
   ELSEIF nReg == 42
      nReg := CBOARD_SYS
      TEdit():aCBoards[nReg,1] := s_cb2t( oEdit )
   ELSEIF nReg == 45
      nReg := CBOARD_MINUS
   ENDIF

   IF nLen == 1
      IF nReg == 1
#ifdef __PLATFORM__UNIX
#ifdef GTHWG
         s := s_cb2t( oEdit )
#else
         s := TEdit():aCBoards[1,1]
#endif
#else
         s := s_cb2t( oEdit )
#endif
         IF !( s == TEdit():aCBoards[1,1] )
            TEdit():aCBoards[1,1] := s
            TEdit():aCBoards[1,2] := oEdit:cp
            TEdit():aCBoards[1,3] := Nil
         ENDIF
      ELSEIF nReg > 1
         s := TEdit():aCBoards[nReg,1]
      ENDIF
      IF nReg > 0
         lVert := !Empty( TEdit():aCBoards[nReg,3] )
         IF !Empty( TEdit():aCBoards[nReg,2] ) .AND. !( TEdit():aCBoards[nReg,2] == oEdit:cp )
            s := hb_Translate( s, TEdit():aCBoards[nReg,2], oEdit:cp )
         ENDIF
      ENDIF
   ELSE
      aMenu_CB := Array( nLen,3 )
      j := 0
      FOR i := 1 TO MAX_CBOARDS
         IF i == 1 .OR. !Empty( TEdit():aCBoards[i,1] )
            j ++
            cPref := Iif( i == 1, "0: ", Iif( i == CBOARD_MINUS, "-: ", ;
               Iif( i == CBOARD_SYS, "*: ",Chr( i + 95 ) + ": " ) ) )
            aMenu_CB[j,1] := cPref + cp_Left( oEdit:lUtf8, TEdit():aCBoards[i,1], 32 )
            aMenu_CB[j,2] := Nil; aMenu_CB[j,3] := i
            IF !Empty( TEdit():aCBoards[i,2] ) .AND. !( TEdit():aCBoards[i,2] == oEdit:cp )
               aMenu_CB[j,1] := hb_Translate( aMenu_CB[j,1], TEdit():aCBoards[i,2], oEdit:cp )
            ENDIF
         ENDIF
      NEXT
      IF Empty( j := FMenu( oEdit, aMenu_CB, 2, 6 ) )
         RETURN Nil
      ENDIF
      i := aMenu_CB[j,3]
      s := TEdit():aCBoards[i,1]
      lVert := !Empty( TEdit():aCBoards[i,3] )
      IF !Empty( TEdit():aCBoards[i,2] ) .AND. !( TEdit():aCBoards[i,2] == oEdit:cp )
         s := hb_Translate( s, TEdit():aCBoards[i,2], oEdit:cp )
      ENDIF
      oEdit:lTextOut := .T.
   ENDIF

   IF Empty( lToText )
      RETURN s
   ELSE
      //IF oEdit:lTabs
      IF hb_hGetDef( TEdit():options,"tabtospaces", .F. )
         s := Strtran( s, cTab, cTabStr )
      ENDIF
      IF Chr(13) $ s
         s := Strtran( s, Chr(13), "" )
      ENDIF
      IF Len( s ) == 0
         RETURN Nil
      ENDIF
      IF !lVert .AND. hb_hGetDef( TEdit():options,"autovertical", .F. )
         lVert := ( oEdit:nPos > 1 .AND. Chr(10) $ s )
      ENDIF
      IF lVert
         oEdit:Undo( oEdit:nLine, oEdit:nPos,,, UNDO_OP_START )
         arr := hb_ATokens( s, Chr(10) )
         nPos := oEdit:nPos
         FOR i := 1 TO Len( arr ) - 1
            IF oEdit:nLine+i-1 > Len( oEdit:aText )
               Aadd( oEdit:aText, "" )
            ENDIF
            oEdit:InsText( oEdit:nLine+i-1, ;
               oEdit:ColToPos( oEdit:LineToRow(oEdit:nLine+i-1), oEdit:PosToCol( oEdit:nLine,nPos ) ), arr[i], .F., .F. )
            oEdit:nPos := nPos
         NEXT
         oEdit:Undo( oEdit:nLine, oEdit:nPos,,, UNDO_OP_END )
         edi_SetPos( oEdit )
         oEdit:lTextOut := .T.
      ELSE
         oEdit:InsText( oEdit:nLine, oEdit:nPos, s, .F. )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION cbDele( oEdit )

   LOCAL nby1, nby2, nbx1, nbx2, i, j, nvx1, nvx2

   IF !oEdit:lReadOnly .AND. oEdit:nby1 >= 0 .AND. oEdit:nby2 >= 0
      IF oEdit:nby1 < oEdit:nby2 .OR. ( oEdit:nby1 == oEdit:nby2 .AND. oEdit:nbx1 < oEdit:nbx2 )
         nby1 := oEdit:nby1; nbx1 := oEdit:nbx1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
      ELSE
         nby1 := oEdit:nby2; nbx1 := oEdit:nbx2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
      ENDIF
      oEdit:nby1 := oEdit:nby2 := -1
      IF oEdit:nSeleMode == 2
         oEdit:Undo( nby1, nbx1, nby2, nbx2, UNDO_OP_START )
         FOR i := nby1 TO nby2
            nvx1 := nbx1; nvx2 := nbx2
            IF i != nby1
               nvx1 := oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol( nby1,nbx1 ) )
            ENDIF
            IF i != nby2
               nvx2 := oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol( nby2,nbx2 ) )
            ENDIF
            IF nvx1 > nvx2
               j := nvx1; nvx1 := nvx2; nvx2 := j
            ENDIF
            oEdit:DelText( i, nvx1, i, Max(nvx2-1,1) )
         NEXT
         oEdit:Undo( nby1, nbx1, nby2, nbx2, UNDO_OP_END )
      ELSE
         oEdit:DelText( nby1, nbx1, nby2, Max(nbx2-1,1) )
      ENDIF
   ENDIF
   RETURN Nil

FUNCTION edi_2cb( oEdit, nReg, s )

   IF s == Nil
      s := edi_GetSelected( oEdit )
   ENDIF

   IF Len( s ) > 0
      IF Empty( nReg )
         nReg := 1
      ELSEIF nReg >= 97 .AND. nReg <= 122
         nReg -= 95
      ELSEIF nReg == 42
         nReg := CBOARD_SYS
         s_t2cb( oEdit, s )
      ENDIF
      TEdit():aCBoards[nReg,1] := s
      IF nReg == 1
#ifdef __PLATFORM__UNIX
#ifdef GTHWG
         s_t2cb( oEdit, s )
#endif
#else
         s_t2cb( oEdit, s )
#endif
      ENDIF
      TEdit():aCBoards[nReg,2] := oEdit:cp
      TEdit():aCBoards[nReg,3] := Iif( oEdit:nSeleMode==2,.T.,Nil )
   ENDIF

   RETURN Nil

FUNCTION s_t2cb( oEdit, s )

#ifdef __PLATFORM__UNIX
#ifdef GTHWG
   IF !oEdit:lUtf8
      s := hb_Translate( s, oEdit:cp, "UTF8" )
      hb_cdpSelect( "UTF8" )
   ENDIF
   hwg_Copystringtoclipboard( s )
   IF !oEdit:lUtf8
      hb_cdpSelect( oEdit:cp )
   ENDIF
#else
   IF !Empty( TEdit():cClipCmd )
      cedi_RunConsoleApp( TEdit():cClipCmd + ' -s "' + StrTran( s,'"','\"' ) + '" 2>/dev/null', "/dev/null" )
      RETURN Nil
   ENDIF
#endif
#endif
   hb_gtInfo( HB_GTI_CLIPBOARDDATA, s )

   RETURN Nil


FUNCTION s_cb2t( oEdit )

   LOCAL s
#ifdef __PLATFORM__UNIX
#ifdef GTHWG
   IF !Empty(oEdit) .AND. !oEdit:lUtf8
      hb_cdpSelect( "UTF8" )
   ENDIF
   s := hwg_Getclipboardtext()
   IF !Empty(oEdit) .AND. !oEdit:lUtf8
      s := hb_Translate( s, "UTF8", oEdit:cp )
      hb_cdpSelect( oEdit:cp )
   ENDIF
   RETURN s
#else
   IF !Empty( TEdit():cClipCmd )
      cedi_RunConsoleApp( TEdit():cClipCmd + ' -gm 2>/dev/null', "/dev/null" )
      IF !Empty( s := cedi_shmRead() )
         RETURN s
      ELSE
         RETURN ""
      ENDIF
   ELSE
      RETURN ""
   ENDIF
#endif
#endif

   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA )

FUNCTION edi_ReadIni( xIni )

   LOCAL hIni, aIni, nSect, aSect, cSect, cLang, arr, arr1, arr2, s, n, i, j, nPos, cTemp, nTemp
   LOCAL lIncSea := .F., lAutoIndent := .F., lSyntax := .T., lTrimSpaces := .F., lAutoComplete := .F., lAutoVert := .F.
   LOCAL lTab2Spaces := .F., lPathInHead := .F.
   LOCAL nSaveHis := 1, ncmdhis := 20, nseahis := 20, nedithis := 20, nEol := 0, nAutoD := 0
   LOCAL hHili
   LOCAL aHiliOpt := { "keywords1","keywords2","keywords3","keywords4","keywords5","quotes","scomm","startline","mcomm","block" }

   TEdit():lReadIni := .T.
   hIni := Iif( Valtype( xIni ) == "C", edi_iniRead( xIni ), xIni )

   SetBlink( .F. )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   aLangs := hb_Hash()
   hPalettes := hb_Hash()
   hHili := hPalettes["default"] := hb_Hash()
   hHili["colors"] := hb_gtinfo( HB_GTI_PALETTE )
   IF !Empty( hHili["colors"] )
      hHili["colors"][2] := 0x800000
      hHili["colors"][4] := 0x808000
   ENDIF
   hHili["attrs"] := TEdit():aHiliAttrs
   hHili["colormain"] := TEdit():cColor
   hHili["colorsel"] := TEdit():cColorSel
   hHili["colorpane"] := TEdit():cColorPane
   hHili["colorbra"] := TEdit():cColorBra
   hHili["colormenu"] := TEdit():cColorMenu
   hHili["colormenusel"] := TEdit():cColorMenuSel
   hHili["colorwb"] := TEdit():cColorWB
   hHili["colorwr"] := TEdit():cColorWR
   hHili["colorget"] := TEdit():cColorGet

   IF !Empty( hIni )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "OPTIONS"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "defmode" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():nDefMode := Iif( (n := Val(cTemp)) < 2 .AND. n >= -1, n, 0 )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "cmdmode" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():lCmdMode := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "incsearch" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lIncSea := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "pathinhead" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lPathInHead := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "autoindent" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lAutoIndent := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "autocomplete" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lAutoComplete := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "autodelay" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nAutoD := Val( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "autovertical" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lAutoVert := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "trimspaces" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lTrimSpaces := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "syntax" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lSyntax := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "savehis" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nSaveHis := Val(cTemp)
                  IF nSaveHis < 0 .OR. nSaveHis > 2
                     nSaveHis := 1
                  ENDIF
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "cmdhismax" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  ncmdhis :=  Val(cTemp)
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "seahismax" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nseahis :=  Val(cTemp)
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "edithismax" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nedithis :=  Val(cTemp)
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "eol" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nEol := Val(cTemp)
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "langcp" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():cLangCP := Upper( Alltrim( cTemp ) )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "langmap_cp" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  IF hb_cdpExists( cTemp )
                     cLangMapCP := cTemp
                  ENDIF
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "langmap_upper" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  aLangMapUpper := hb_aTokens( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "langmap_lower" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  aLangMapLower := hb_aTokens( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "palette" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():cDefPal := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "keymap" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  arr := hb_aTokens( cTemp, ",", .T. )
                  hKeyMap := hb_Hash()
                  FOR i := 1 TO Len( arr )
                     IF ( nPos := At( "=>", arr[i] ) ) > 0 .AND. ;
                        ( nTemp := edi_KeyCToN(Left(arr[i],nPos-1)) ) != Nil
                        IF '|' $ ( cTemp := Substr(arr[i],nPos+2) )
                           arr1 := hb_aTokens( cTemp, "|", .T. )
                           FOR n := 1 TO Len( arr1 )
                              IF ( nPos := edi_KeyCToN(arr1[n]) ) == Nil
                                 arr1 := Nil
                                 EXIT
                              ELSE
                                 arr1[n] := nPos
                              ENDIF
                           NEXT
                           IF !Empty( arr1 )
                              hb_AIns( arr1, 1, 0, .T. )
                              hKeyMap[nTemp] := arr1
                           ENDIF
                        ELSEIF ( nPos := edi_KeyCToN(cTemp) ) != Nil
                           hKeyMap[nTemp] := nPos
                        ENDIF
                     ENDIF
                  NEXT
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "tablen" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():nTabLen := Val( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "tabtospaces" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lTab2Spaces := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "sele_plugin" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():hSelePlug := Iif( !Empty( edi_FindPath( "plugins" + hb_ps() + cTemp ) ), cTemp, Nil )
               ENDIF
            ENDIF

         ELSEIF Upper(aIni[nSect]) == "CODEPAGES"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := ASort( hb_hKeys( aSect ) )
               TEdit():aCPages := Array( Len( arr ) )
               FOR i := 1 TO Len( arr )
                  TEdit():aCPages[i] := aSect[ arr[i] ]
               NEXT
            ENDIF

         ELSEIF Upper(aIni[nSect]) == "PLUGINS"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := hb_hKeys( aSect )
               TEdit():aPlugins := {}
               j := 0
               FOR i := 1 TO Len( arr )
                  s := aSect[ arr[i] ]
                  IF ( n := At( ",", s ) ) > 0
                     cTemp := AllTrim( Left( s,n-1 ) )
                     IF !Empty( edi_FindPath( "plugins" + hb_ps() + cTemp ) )
                        s := Substr( s, n+1 )
                        IF ( n := At( ",", s ) ) > 0
                           Aadd( TEdit():aPlugins, { cTemp, Substr( s, n+1 ), AllTrim( Left( s,n-1 ) ), Nil, Nil } )
                           j ++
                           IF ( n := At( ",", s := Substr( s, n+1 ) ) ) > 0 .AND. ;
                              ( nTemp := edi_KeyCToN(Substr(s,n+1)) ) != Nil
                              IF hKeyMap == Nil
                                 hKeyMap := hb_Hash()
                              ENDIF
                              IF !Empty( arr2 := hb_hGetDef( hKeyMap, nTemp, Nil ) )
                                 Aadd( arr2, j )
                              ELSE
                                 hKeyMap[nTemp] := { j }
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ELSEIF Upper(aIni[nSect]) == "HILIGHT"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := hb_hKeys( aSect )
               FOR i := 1 TO Len( arr )
                  IF !Empty( cTemp := aSect[ arr[i] ] )
                     IF ( n := Ascan( aHiliOpt, arr[i] ) ) > 0
                        TEdit():aHiliAttrs[n] := cTemp
                     ELSEIF arr[i] == "colormain"
                        TEdit():cColor := cTemp
                     ELSEIF arr[i] == "colorsel"
                        TEdit():cColorSel := cTemp
                     ELSEIF arr[i] == "colorpane"
                        TEdit():cColorPane := cTemp
                     ELSEIF arr[i] == "colorbra"
                        TEdit():cColorBra := cTemp
                     ELSEIF arr[i] == "colormenu"
                        TEdit():cColorMenu := cTemp
                     ELSEIF arr[i] == "colormenusel"
                        TEdit():cColorMenuSel := cTemp
                     ELSEIF arr[i] == "colorwb"
                        TEdit():cColorWB := cTemp
                     ELSEIF arr[i] == "colorwr"
                        TEdit():cColorWR := cTemp
                     ELSEIF arr[i] == "colorget"
                        TEdit():cColorGet := cTemp
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT

      IF Empty( TEdit():cDefPal )
         TEdit():cDefPal := "default"
      ENDIF

#if defined ( __PLATFORM__WINDOWS ) || defined ( GTHWG )
      FOR nSect := 1 TO Len( aIni )
         IF Left( Upper(aIni[nSect]),8 ) == "PALETTE_"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := hb_hKeys( aSect )
               cLang := Nil
               FOR i := 1 TO Len( arr )
                  IF arr[i] == "name"
                     IF !Empty( cTemp := aSect[ arr[i] ] )
                         cLang := cTemp
                     ENDIF
                  ENDIF
               NEXT
               IF !Empty( cLang )
                  hHili := edi_AddPalette( cLang )
                  arr2 := AClone( TEdit():aHiliAttrs )
                  FOR i := 1 TO Len( arr )
                     IF !Empty( cTemp := aSect[ arr[i] ] )
                        IF arr[i] == "name"
                           cLang := cTemp

                        ELSEIF arr[i] == "colors"
                           arr1 := hb_ATokens( cTemp, ',' )
                           FOR n := 1 TO Len( arr1 )
                              arr1[n] := LTrim( arr1[n] )
                              arr1[n] := Iif( Asc(arr1[n]) == 35, edi_ColorC2N(arr1[n]), Val(arr1[n]) )
                           NEXT
                           hHili[arr[i]] := arr1

                        ELSEIF arr[i] == "colormain" .OR. arr[i] == "colorsel" .OR. ;
                           arr[i] == "colorpane" .OR. arr[i] == "colorbra" .OR. ;
                           arr[i] == "colormenu" .OR. arr[i] == "colormenusel" .OR. ;
                           arr[i] == "colorwb" .OR. arr[i] == "colorget" .OR. arr[i] == "colorwr"
                           hHili[arr[i]] := cTemp

                        ELSEIF ( n := Ascan( aHiliOpt, arr[i] ) ) > 0
                           arr2[n] := cTemp

                        ENDIF
                     ENDIF
                  NEXT
                  hHili["attrs"] := arr2
               ENDIF
            ENDIF
         ENDIF
      NEXT
#endif
      FOR nSect := 1 TO Len( aIni )
         IF Left( Upper(aIni[nSect]),5 ) == "LANG_"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               cLang := Lower( Substr(aIni[nSect],6) )
               hHili := aLangs[ cLang ] := hb_hash()
               hHili["colors"] := Array(Len(aHiliOpt))
               hHili["bra"] := .F.
               arr := hb_hKeys( aSect )
               FOR i := 1 TO Len( arr )
                  IF !Empty( cTemp := aSect[ arr[i] ] )
                     IF ( n := Ascan( aHiliOpt, arr[i] ) ) > 0
                        s := aSect[ arr[i] ]
                        IF ( nPos := At( ",",s ) ) > 0
                           hHili["colors"][n] := Trim(Left(s,nPos-1))
                           s := Ltrim( Substr( s, nPos+1 ) )
                        ENDIF
                        hHili[arr[i]] := s
                     ELSEIF arr[i] == "ext"
                        AAdd( aLangExten, { cLang, cTemp } )
                     ELSEIF arr[i] == "case"
                        hHili[arr[i]] := ( Lower(cTemp) == "on" )
                     ELSEIF arr[i] == "plugin"
                        hHili["plugin"] := cTemp
                     ELSEIF arr[i] == "brackets"
                        hHili["bra"] := ( Lower(cTemp) == "on" )
                     ELSEIF arr[i] == "palette"
                        hHili["palette"] := cTemp
                     ELSEIF arr[i] == "quotes"
                        hHili["quotes"] := cTemp
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   TEdit():cpInit := hb_cdpSelect()
   TEdit():options["incsearch"]  := lIncSea
   TEdit():options["savehis"]    := nSaveHis
   TEdit():options["cmdhismax"]  := ncmdhis
   TEdit():options["seahismax"]  := nseahis
   TEdit():options["eol"]        := nEol
   TEdit():options["trimspaces"] := lTrimSpaces
   TEdit():options["tabtospaces"] := lTab2Spaces
   TEdit():options["edithismax"] := nedithis
   TEdit():options["autoindent"] := lAutoIndent
   TEdit():options["autocomplete"] := lAutoComplete
   IF lAutoComplete .AND. nAutoD > 0
      TEdit():options["autodelay"] := nAutoD
      nAutoDelay := nAutoD
      //hIdle := hb_IdleAdd( {|| _FIdle() } )
   ENDIF
   TEdit():options["autovertical"] := lAutoVert
   TEdit():options["syntax"] := lSyntax
   TEdit():options["pathinhead"] := lPathInHead
   cTabStr := Space( TEdit():nTablen )

   IF Empty( TEdit():aCPages )
      TEdit():aCPages := { "RU866", "RU1251", "UTF8" }
   ENDIF

   TEdit():aCBoards := Array( MAX_CBOARDS,3 )
   FOR i := 1 TO MAX_CBOARDS
      TEdit():aCBoards[i,1] := TEdit():aCBoards[i,2] := ""
   NEXT
#ifdef __PLATFORM__UNIX
#ifdef GTHWG
   TEdit():aCBoards[1,1] := s_cb2t()
   TEdit():aCBoards[1,2] := TEdit():cpInit
   TEdit():aCBoards[1,3] := Nil
#endif
#else
   TEdit():aCBoards[1,1] := s_cb2t()
   TEdit():aCBoards[1,2] := TEdit():cpInit
   TEdit():aCBoards[1,3] := Nil
#endif

   TEdit():hMacros := hb_Hash()

   IF !Empty( TEdit():cDefPal )
      IF hb_hHaskey( hPalettes, TEdit():cDefPal ) .AND. hb_hHaskey( hPalettes[TEdit():cDefPal], "colors" )
         hb_gtinfo( HB_GTI_PALETTE, hPalettes[TEdit():cDefPal]["colors"] )
      ELSE
         TEdit():cDefPal := Nil
      ENDIF
   ENDIF

   IF nSaveHis > 0
      cTemp := hb_DirBase()
#ifdef __PLATFORM__UNIX
      IF hb_dirExists( s := ( hb_getenv( "HOME" ) + "/hbedit" ) )
         cTemp := s + "/"
      ENDIF
#endif
      hIni := edi_iniRead( Iif( nSaveHis==1, cTemp, "" ) + "hbedit.his" )
      IF !Empty( hIni )
         hb_hCaseMatch( hIni, .F. )
         IF hb_hHaskey( hIni, cTemp := "SEARCH" ) .AND. !Empty( aSect := hIni[ cTemp ] )
            arr := ASort( hb_hKeys( aSect ) )
            TEdit():aSeaHis := Array( Len(arr) )
            FOR i := 1 TO Len(arr)
               TEdit():aSeaHis[i] := aSect[ arr[i] ]
            NEXT
         ENDIF
         IF hb_hHaskey( hIni, cTemp := "REPLACE" ) .AND. !Empty( aSect := hIni[ cTemp ] )
            arr := ASort( hb_hKeys( aSect ) )
            TEdit():aReplHis := Array( Len(arr) )
            FOR i := 1 TO Len(arr)
               TEdit():aReplHis[i] := aSect[ arr[i] ]
            NEXT
         ENDIF
         IF hb_hHaskey( hIni, cTemp := "COMMANDS" ) .AND. !Empty( aSect := hIni[ cTemp ] )
            arr := ASort( hb_hKeys( aSect ) )
            TEdit():aCmdHis := Array( Len(arr) )
            FOR i := 1 TO Len(arr)
               TEdit():aCmdHis[i] := aSect[ arr[i] ]
            NEXT
         ENDIF
         IF hb_hHaskey( hIni, cTemp := "MACRO" ) .AND. !Empty( aSect := hIni[ cTemp ] )
            arr := ASort( hb_hKeys( aSect ) )
            FOR i := 1 TO Len(arr)
               arr1 := {}
               s := aSect[ arr[i] ]
               nPos := 1
               DO WHILE ( n := hb_At( ",", s, nPos ) ) > 0
                  IF n-nPos == 0
                     AAdd( arr1, edi_KeyCToN( "," ) )
                  ELSE
                     cTemp := Substr( s, nPos, n-nPos )
                     AAdd( arr1, edi_KeyCToN( cTemp ) )
                  ENDIF
                  nPos := n + 1
               ENDDO
               TEdit():hMacros[Asc(arr[i])] := arr1
            NEXT
         ENDIF
         IF hb_hHaskey( hIni, cTemp := "EDIT" ) .AND. !Empty( aSect := hIni[ cTemp ] )
            arr := ASort( hb_hKeys( aSect ) )
            TEdit():aEditHis := Array( Len(arr) )
            FOR i := 1 TO Len(arr)
               arr1 := hb_ATokens( aSect[ arr[i] ], "," )
               IF Len(arr1) < 4
                  TEdit():aEditHis[i] := { "ru866", 1, 1, "err" }
               ELSE
                  s := Upper( arr1[1] )
                  IF Ascan( TEdit():aCPages, s ) == 0
                     s := TEdit():aCPages[1]
                  ENDIF
                  arr1[2] := Max( 1, Val(arr1[2]) )
                  arr1[3] := Max( 1, Val(arr1[3]) )
                  TEdit():aEditHis[i] := { Ltrim(arr1[4]), s, arr1[2], arr1[3] }
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

#ifdef __PLATFORM__UNIX
#ifndef GTHWG
   IF File( cTemp := hb_DirBase() + "gtkclip" )
      cedi_RunConsoleApp( cTemp + ' -tm 2>/dev/null', "/dev/null" )
      IF !Empty( s := cedi_ShmRead() ) .AND. Left( s,1 ) == 'y'
         TEdit():cClipCmd := cTemp
      ENDIF
   ENDIF
#endif
#endif

   RETURN Nil

STATIC FUNCTION mnu_Main( o )

   LOCAL aMenuMain := { {_I("Exit"),@mnu_Exit(),Nil,"Esc,F10"}, {_I("Save"),@mnu_Save(),Nil,"F2"}, ;
      {_I("Save as"),@mnu_Save(),.T.,"Shift-F2"}, {_I("Open file"),@mnu_F4(),{7,16},"F4 >"}, ;
      {_I("View"),@mnu_View(),Nil,">"}, {_I("Selection"),@mnu_Selection(),Nil,">"}, ;
      {_I("Search&GoTo"),@mnu_Sea_Goto(),{8,16},">"}, ;
      {_I("Codepage"),@mnu_CPages(),{11,16},">"}, {_I("Palette"),@mnu_Palettes(),{12,16},">"}, ;
      {_I("Syntax"),@mnu_Syntax(),{13,16},"F8 >"}, {_I("Plugins"),@mnu_Plugins(),Nil,"F11 >"}, ;
      {_I("Windows"),@mnu_Windows(),{15,16},">"}, ;
      {_I("Buffers"),@mnu_Buffers(),{16,16},"F12 >"} }

#ifndef _NO_HBC
   aMenuMain := hb_AIns( aMenuMain, Len(aMenuMain), { _I("File Manager"),@hbc(),Nil }, .T. )
#endif

   FMenu( o, aMenuMain, 2, 6 )

   RETURN Nil

FUNCTION mnu_Help( oEdit, cFullPath, cMet )

   LOCAL oHelp, nCurr := TEdit():nCurr, i
   LOCAL cDop := "", cHelp, cPlugHelp, cName := "$Help"

   IF Ascan( TEdit():aWindows, {|o|o:cFileName == cName} ) > 0
      RETURN Nil
   ENDIF
#ifdef _USE_SSH2
   cDop += _I(" (with libssh2 support)")
#endif
#ifdef __BUILT_IN
   cDop += _I(" built-in version")
#else
  #ifndef _FULL
   cDop += _I(" basic version")
  #endif
  #ifdef _NO_HBC
   cDop += _I(" without HbCommander")
  #endif
#endif
#ifdef GTHWG
   cDop += " (gthwgui " + hwg_Version(1) + " b." + Ltrim(Str(hwg_Version(2))) + ")"
#endif
   IF Empty( cFullPath )
      cFullPath := edi_FindPath( "hbedit.help" )
   ENDIF
   IF !Empty( cFullPath )
      cHelp := MemoRead( cFullPath )
      IF !Empty( oEdit:hCargo ) .AND. !Empty( cPlugHelp := hb_hGetDef( oEdit:hCargo, "help", Nil ) )
         cHelp := Chr(10) + cPlugHelp + Chr(10) + cHelp
      ENDIF
      cHelp := "HbEdit - " + HBEDIT_VERSION + cDop + Chr(10) + ;
         "Copyright (C) 2019-2024  Alexander S. Kresin  http://www.kresin.ru" + Chr(10) + cHelp
      oHelp := TEdit():New( cHelp, cName, ;
         oEdit:aRectFull[1], oEdit:aRectFull[2], oEdit:aRectFull[3], oEdit:aRectFull[4] )

      oHelp:lReadOnly := .T.
      oHelp:lCtrlTab  := .F.
      IF !Empty( cMet )
         FOR i := 1 TO Len( oHelp:aText )
            IF !Empty( oHelp:aText[i] ) .AND. cMet $ oHelp:aText[i]
               oHelp:nLine := i
               EXIT
            ENDIF
         NEXT
      ENDIF
      oHelp:Edit()
      TEdit():nCurr := nCurr
   ENDIF

   RETURN Nil

FUNCTION mnu_Exit( oEdit )

   LOCAL nRes := 2

   IF !oEdit:lBuiltIn .AND. oEdit:lUpdated
      nRes := edi_Alert( _I("File has been modified. Save?"), _I("Yes"), _I("No"), _I("Cancel") )
   ENDIF
   IF nRes == 1 .OR. nRes == 2
      IF nRes == 1
         IF !oEdit:Save()
            RETURN .F.
         ENDIF
      ENDIF
      oEdit:lShow := .F.
      oEdit:lClose := .T.
   ELSE
      edi_SetPos( oEdit )
   ENDIF
   RETURN .T.

FUNCTION mnu_CPages( oEdit, aXY )

   LOCAL iRes

   IF !Empty( iRes := FMenu( oEdit, oEdit:aCPages, aXY[1], aXY[2] ) )
      oEdit:cp := oEdit:aCPages[iRes]
      hb_cdpSelect( oEdit:cp )
      oEdit:lUtf8 := ( Lower(oEdit:cp) == "utf8" )
      oEdit:TextOut()
   ENDIF

   RETURN Nil

FUNCTION mnu_Palettes( oEdit, aXY )

   LOCAL iRes, arr := hb_hKeys( hPalettes )

   IF !Empty( iRes := FMenu( oEdit, arr, aXY[1], aXY[2] ) )
      edi_SetPalette( oEdit, arr[iRes] )
      oEdit:TextOut()
   ENDIF

   RETURN Nil

FUNCTION mnu_Syntax( oEdit, aXY )

   LOCAL aMenu := { {"Syntax Off",@mnu_SyntaxOn(),Nil} }, i, arr := hb_hKeys( aLangs )

   FOR i := 1 TO Len( arr )
      AAdd( aMenu, {arr[i], @mnu_SyntaxOn(), arr[i]} )
   NEXT

   FMenu( oEdit, aMenu, aXY[1], aXY[2] )
   IF !Empty( oEdit:bStartEdit )
      Eval( oEdit:bStartEdit, oEdit )
      edi_Alert( "Init plugin is ready" )
   ENDIF

   RETURN Nil

FUNCTION mnu_SyntaxOn( oEdit, cLang )

   LOCAL cPal, xPlugin, cFullPath

   oEdit:Highlighter( Iif( Empty(cLang), Nil, Hili():New( aLangs[cLang] ) ) )
   oEdit:cSyntaxType := cLang

   IF Empty( cLang )
      RETURN Nil
   ENDIF
   IF !Empty( cPal := hb_hGetDef( oEdit:oHili:hHili, "palette", Nil ) ) .AND. ;
      hb_hHaskey( hPalettes, cPal )
      oEdit:cPalette := cPal
   ENDIF
   IF !Empty( xPlugin := hb_hGetDef( oEdit:oHili:hHili, "plugin", Nil ) )
      IF Valtype( xPlugin ) == "C" .AND. ;
         !Empty( cFullPath := edi_FindPath( "plugins" + hb_ps() + xPlugin ) )
         xPlugin := oEdit:oHili:hHili["plugin"] := { cFullPath, hb_hrbLoad( cFullPath ) }
      ENDIF
      IF Valtype( xPlugin ) == "A" .AND. !Empty( xPlugin[2] )
         hb_hrbDo( xPlugin[2], oEdit, hb_fnameDir( xPlugin[1] ) )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION mnu_Windows( oEdit, aXY, n )

   LOCAL aMenu := { {_I("Switch window"),Nil,Nil,"Ctrl-w,w"}, ;
      {_I("To full size"),Nil,Nil,"Ctrl-w,o"}, ;
      {_I("Close"),Nil,Nil,"Ctrl-w,c"}, ;
      {_I("Add window horizontally"),Nil,Nil,"Ctrl-w,s"}, ;
      {_I("Add window vertically"),Nil,Nil,"Ctrl-w,v"} }
   LOCAL i, o

   IF n == Nil
      n := FMenu( oEdit, aMenu, aXY[1], aXY[2] )
   ENDIF
   IF n == 1
      mnu_ToBuf( oEdit, edi_FindWindow( oEdit, .T. ) )
   ELSEIF n == 2
      IF oEdit:oParent != Nil
         edi_CloseWindow( oEdit, .F. )
         oEdit:oParent := Nil
         oEdit:y1 := oEdit:aRect[1] := oEdit:aRectFull[1]
         oEdit:x1 := oEdit:aRect[2] := oEdit:aRectFull[2]
         oEdit:y2 := oEdit:aRect[3] := oEdit:aRectFull[3]
         oEdit:x2 := oEdit:aRect[4] := oEdit:aRectFull[4]
         IF oEdit:lTopPane
            oEdit:y1 ++
         ENDIF
         oEdit:TextOut()
         edi_SetPos( oEdit )
      ENDIF
   ELSEIF n == 3
      IF oEdit:oParent != Nil
         mnu_Exit( oEdit )
      ENDIF
   ELSEIF n == 4
      o := edi_AddWindow( oEdit, MemoRead(oEdit:cFileName), oEdit:cFileName, 2, Int( (oEdit:y2-oEdit:y1)/2 ) )
      o:lReadOnly := .T.
   ELSEIF n == 5
      o := edi_AddWindow( oEdit, MemoRead(oEdit:cFileName), oEdit:cFileName, 3, Int( (oEdit:x2-oEdit:x1)/2 ) )
      o:lReadOnly := .T.
   ENDIF

   RETURN Nil

FUNCTION mnu_Buffers( oEdit, aXY )

   LOCAL aMenu := { }, i, nCurr := 1, aWindows := TEdit():aWindows

   FOR i := 1 TO Len( aWindows )
      IF aWindows[i] == oEdit
         nCurr := i
      ENDIF
      AAdd( aMenu, {NameShortcut(aWindows[i]:cFileName,30,'~'),@mnu_ToBuf(),i} )
   NEXT
   IF !Empty( TEdit():cLauncher )
      AAdd( aMenu, {TEdit():cLauncher,@mnu_ToBuf(),0} )
   ENDIF

   FMenu( oEdit, aMenu, aXY[1], aXY[2],,,,, nCurr )

   RETURN Nil

FUNCTION mnu_ToBuf( oEdit, x )

   oEdit:lShow := .F.
   IF Valtype( x ) == "O"
      TEdit():nCurr := Ascan( TEdit():aWindows, {|o|o==x} )
   ELSEIF Valtype( x ) == "N"
      TEdit():nCurr := x
   ENDIF

   RETURN Nil

FUNCTION mnu_Save( oEdit, lAs )

   LOCAL cFileName, cPath

   IF !Empty( lAs )
      oEdit:lTextOut := .T.
      IF Left(oEdit:cFileName,1) == "$" .OR. Empty( cPath := hb_fnameDir(oEdit:cFileName) )
         cPath := edi_CurrPath()
      ENDIF
      IF Empty( cFileName := edi_SaveDlg( oEdit, cPath ) )
         RETURN Nil
      ENDIF
      IF Left(cFileName,1) == "$" .OR. Empty( hb_fnameDir(cFileName) )
         cFileName := cPath + cFileName
      ENDIF
   ENDIF

   oEdit:Save( cFileName )

   RETURN Nil

FUNCTION mnu_View( oEdit )

   LOCAL lAutoC := TEdit():options["autocomplete"], lAutoI := TEdit():options["autoindent"], lAutoVert := TEdit():options["autovertical"]
   LOCAL aMenu := { {_I("Wrap"),,,Iif(!oEdit:lWrap,"Off","On")}, {_I("Autocomplete"),,,Iif(!lAutoC,"Off","On")}, ;
      {_I("Autovertical"),,,Iif(!lAutoVert,"Off","On")}, {_I("Autoindent"),,,Iif(!lAutoI,"Off","On")} }
   LOCAL y1 := Row(), x1 := Col()-6, i

   i := FMenu( oEdit, aMenu, y1, x1 )
   IF i == 1
      oEdit:lWrap := !oEdit:lWrap
      oEdit:lTextOut := .T.
   ELSEIF i == 2
      TEdit():options["autocomplete"] := lAutoC := !lAutoC
      nAutoDelay := Iif( lAutoC, hb_hGetDef( TEdit():options,"autodelay", 0 ), 0 )
      /*
      IF lAutoC
         IF hb_hGetDef( TEdit():options,"autodelay", 0 ) > 0
            hIdle := hb_IdleAdd( {|| _FIdle() } )
         ENDIF
      ELSEIF !Empty( hIdle )
         hb_IdleDel( hIdle )
         hIdle := Nil
      ENDIF
      */
   ELSEIF i == 3
      TEdit():options["autovertical"] := !lAutoVert
   ELSEIF i == 4
      TEdit():options["autoindent"] := !lAutoI
   ENDIF

   RETURN Nil

FUNCTION mnu_Selection( oEdit )

   LOCAL aMenu := { {_I("Mark block"),@mnu_F3(),Nil,"F3"}, {_I("Vertical block"),@mnu_F3(),2,"Ctrl-F3"} }
   LOCAL y1 := Row(), x1 := Col()-6

   FMenu( oEdit, aMenu, y1, x1 )

   RETURN Nil

FUNCTION mnu_F3( oEdit, nSeleMode )

   LOCAL i, aMenu_CB, cPref

   nSeleMode := Iif( Empty( nSeleMode ), 0, nSeleMode )

   IF oEdit:nby1 >= 0 .AND. oEdit:nby2 >= 0
      oEdit:lF3 := .T.
   ENDIF

   IF !oEdit:lF3
      oEdit:nby1 := oEdit:nLine
      IF nSeleMode == 1
         oEdit:nbx1 := 1
         oEdit:nby2 := oEdit:nLine
         oEdit:nbx2 := cp_Len( oEdit:lUtf8, oEdit:aText[oEdit:nLine] ) + 1
      ELSE
         oEdit:nbx1 := oEdit:nPos
         oEdit:nby2 := oEdit:nbx2 := -1
      ENDIF
      oEdit:nSeleMode := nSeleMode
   ENDIF
   oEdit:lF3 := !oEdit:lF3
   IF !oEdit:lF3
      aMenu_CB := Array(MAX_EDIT_CBOARDS,3)

      FOR i := 1 TO MAX_EDIT_CBOARDS
         cPref := Iif( i == 1, "0: ", Chr( i + 95 ) + ": " )
         aMenu_CB[i,1] := cPref + cp_Left( oEdit:lUtf8, TEdit():aCBoards[i,1], 32 )
         IF !Empty( TEdit():aCBoards[i,2] ) .AND. !( TEdit():aCBoards[i,2] == oEdit:cp )
            aMenu_CB[i,1] := hb_Translate( aMenu_CB[i,1], TEdit():aCBoards[i,2], oEdit:cp )
            aMenu_CB[i,2] := Nil; aMenu_CB[i,3] := i
         ENDIF
      NEXT
      IF !Empty( i := FMenu( oEdit, aMenu_CB, 2, 6 ) )
         edi_2cb( oEdit, i )
         IF i == 1
#ifdef __PLATFORM__UNIX
#ifdef GTHWG
            s_t2cb( oEdit, TEdit():aCBoards[1,1] )
#endif
#else
            s_t2cb( oEdit, TEdit():aCBoards[1,1] )
#endif
         ENDIF
      ENDIF
      oEdit:lTextOut := .T.
   ENDIF

   RETURN Nil

FUNCTION mnu_F4( oEdit, aXY )

   LOCAL aMenu := { {_I("New file"),@mnu_NewBuf(),Nil,"Shift-F4"}, {_I("Open file"),@mnu_OpenFile(),Nil,"Ctrl-F4"} }, i
   STATIC lChecked := .F.

   FOR i := 1 TO Len( oEdit:aEditHis )
      IF lChecked .OR. File( oEdit:aEditHis[i,1] )
         AAdd( aMenu, { NameShortcut(hb_Translate(oEdit:aEditHis[i,1],"UTF8"), 39,'~'), ;
            @mnu_OpenRecent(),i } )
      ELSE
         hb_ADel( oEdit:aEditHis, i, .T. )
         i --
      ENDIF
   NEXT
   lChecked := .T.

   FMenu( oEdit, aMenu, aXY[1], aXY[2],,,,,, .T. )

   RETURN Nil

FUNCTION mnu_OpenRecent( oEdit, n )

   LOCAL cFileName := hb_Translate( oEdit:aEditHis[n,1], "UTF8", oEdit:cpInit )
   LOCAL cPath

   IF Empty( cLastDir ) .AND. !Empty( cPath := hb_fnameDir( cFileName ) )
      cLastDir := cPath
   ENDIF
   RETURN mnu_OpenFile( oEdit, cFileName )

FUNCTION mnu_NewBuf( oEdit, cFileName, cText, funSave )

   LOCAL oNew, s, j

   IF !Empty( cFileName )
      s := Lower( cFileName )
      IF ( j := Ascan( oEdit:aWindows, {|o|Lower(o:cFileName)==s} ) ) > 0
         mnu_ToBuf( oEdit, j )
         RETURN oEdit:aWindows[j]
      ENDIF
      IF Empty( cText )
         IF File( cFileName )
            cText := Memoread( cFileName )
         ELSE
            edi_Alert( _I("File not found") )
            RETURN Nil
         ENDIF
      ENDIF
   ENDIF

   hb_cdpSelect( TEdit():cpInit )
   oNew := TEdit():New( cText, cFileName )
   IF !Empty( oEdit )
      oNew:funSave := Iif( Empty(funSave), oEdit:funSave, funSave )
      hb_cdpSelect( oEdit:cp )
      oEdit:lShow := .F.

      IF ( !Empty( oEdit:aText ) .AND. !Empty( oEdit:aText[1] ) ) ;
            .OR. oEdit:lUpdated .OR. !Empty( oEdit:cFilename )
      ELSE
         oEdit:lClose := .T.
      ENDIF
   ENDIF
   TEdit():nCurr := Len( TEdit():aWindows )

   RETURN oNew

FUNCTION mnu_OpenFile( oEdit, cFile )

   LOCAL cScBuf := Savescreen( 09, 10, 15, 72 )
   LOCAL oldc := SetColor( oEdit:cColorSel+","+oEdit:cColorMenu ), cName, nRes, oNew, cText
   LOCAL aGets := { {11,12,0,Iif(Empty(cFile),"",cFile),52}, ;
      {11,64,2,"[^]",3,oEdit:cColorSel,oEdit:cColorMenu,{||mnu_FileList(oEdit,aGets[1])}}, ;
      {11,68,2,"[D]",3,oEdit:cColorSel,oEdit:cColorMenu,{||mnu_DirList(oEdit,aGets[1],.F.)}}, ;
      {12,13,1,.F.,1,oEdit:cColorSel,oEdit:cColorMenu}, {12,28,1,.F.,1,oEdit:cColorSel,oEdit:cColorMenu}, ;
      {12,56,-1,.F.,1,oEdit:cColorSel,oEdit:cColorMenu}, ;
      {14,26,2,_I("[Open]"),10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {14,46,2,_I("[Cancel]"),10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   STATIC pKeys

   IF Empty( pKeys )
      // 0x41020044 - Ctrl-D
      pKeys := hb_Hash( 0x41020044, Chr(K_CTRL_HOME)+Chr(K_DOWN)+Chr(K_DOWN)+Chr(K_SPACE) )
   ENDIF
   hb_cdpSelect( "RU866" )
   @ 09, 10, 15, 72 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 13, 20 SAY "Ã"
   @ 13, 60 SAY "´"
   @ 13, 11 TO 13, 71
   hb_cdpSelect( oEdit:cp )
   @ 10, 12 SAY _I("Open file")
   @ 12, 12 SAY "[ ] " + _I("Readonly")
   @ 12, 27 SAY "[ ] " + _I("In a current window")
   IF !Empty( oEdit:cFileName )
      aGets[5,3] := 1
      @ 12, 55 SAY "[ ] " + _I("Diff")
   ENDIF
   SetColor( oEdit:cColorMenu )

   IF ( nRes := edi_READ( aGets, pKeys ) ) > 0 .AND. nRes < Len(aGets)
      IF !Empty( cName := aGets[1,4] ) .AND. File( cName )
         IF aGets[6,4]
            IF ( cText := edi_MakeDiff( oEdit, cName ) ) == Nil
               edi_Alert( _I("Diff tool not found") )
            ELSE
               edi_AddDiff( oEdit, cText, .T. )
            ENDIF
         ELSEIF aGets[5,4]
            IF oEdit:lUpdated
               IF mnu_Exit( oEdit )
                  oEdit:lShow := .T.
                  oEdit:lClose := .F.
               ELSE
                  SetColor( oldc )
                  edi_SetPos( oEdit )
                  RETURN Nil
               ENDIF
            ENDIF
            oEdit:SetText( MemoRead( cName ), cName )
            oEdit:lReadOnly := aGets[4,4]
         ELSE
            oNew := mnu_NewBuf( oEdit, cName )
            oNew:lReadOnly := aGets[4,4]
         ENDIF
      ENDIF
   ENDIF

   Restscreen( 09, 10, 15, 72, cScBuf )
   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN Nil

FUNCTION mnu_FileList( oEdit, aGet )

   LOCAL cPrefix, xFileName, i, cDir, ny2 := oEdit:aRectFull[3]-2
   LOCAL cScBuf := Savescreen( 12, 12, ny2, 67 )

#ifdef __PLATFORM__UNIX
   cPrefix := '/'
#else
   cPrefix := hb_curDrive() + ':\'
#endif

   cDir := Iif( Empty(cLastDir), cPrefix + CurDir() + hb_ps(), cLastDir )
   xFileName := edi_SeleFile( oEdit, cDir, 12, 12, ny2, 67 )
   Restscreen( 12, 12, ny2, 67, cScBuf )

   IF !Empty( xFileName )
      IF Valtype( xFileName ) == "A"
         IF Len( xFileName ) == 1
            xFileName := xFileName[1]
         ELSE
            FOR i := 1 TO Len( xFileName )
               mnu_NewBuf( oEdit, xFileName[i] )
            NEXT
            __KeyBoard( Chr(K_ESC) )
            RETURN Nil
         ENDIF
      ENDIF

      cLastDir := hb_fnameDir( xFileName )
      aGet[4] := xFileName
      ShowGetItem( aGet, .F., oEdit:lUtf8,, {.F.,1,"",.F.,.F.} )
   ENDIF

   RETURN Nil

STATIC FUNCTION mnu_DirList( oEdit, aGet, lDirOnly )

   LOCAL i, cDir, ny2 := oEdit:aRectFull[3]-2, cScBuf, aMenu := {}

   FOR i := 1 TO Len( TEdit():aEditHis )
#ifdef __PLATFORM__UNIX
      cDir := NameShortcut(hb_Translate(hb_fnameDir(TEdit():aEditHis[i,1]),"UTF8"), 48,'~' )
      IF Ascan( aMenu, {|a|a[1]==cDir} ) == 0
         AAdd( aMenu, { cDir,Nil,i} )
      ENDIF
#else
      cDir := Lower( NameShortcut(hb_Translate(hb_fnameDir(TEdit():aEditHis[i,1]),"UTF8"), 48,'~' ) )
      IF '/' $ cDir
         cDir := StrTran( cDir, '/', '\' )
      ENDIF
      IF Ascan( aMenu, {|a|Lower(a[1])==cDir} ) == 0
         AAdd( aMenu, { cDir,Nil,i} )
      ENDIF
#endif
   NEXT

   IF Empty( aMenu )
      RETURN Nil
   ENDIF
   cScBuf := Savescreen( 12, 12, ny2, 67 )
   i := FMenu( oEdit, aMenu, 12, 12, ny2, 67 )
   Restscreen( 12, 12, ny2, 67, cScBuf )

   IF i != 0
      cLastDir := hb_fnameDir( oEdit:aEditHis[aMenu[i,3],1] )
      IF lDirOnly
         aGet[4] := cLastDir
         ShowGetItem( aGet, .F., oEdit:lUtf8,, {.F.,1,"",.F.,.F.} )
      ELSE
         mnu_FileList( oEdit, aGet )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION mnu_Sea_goto( oEdit, aXY )

   LOCAL aMenu := { {_I("Search"),@mnu_Search(),Nil,"F7"}, {_I("Next"),@mnu_SeaNext(),.T.,"Shift-F7"}, ;
      {_I("Previous"),@mnu_SeaNext(),.F.,"Alt-F7"}, {_I("Replace"),@mnu_SeaAndRepl(),Nil,"Ctrl-F7"}, ;
      {_I("Go to"),@mnu_GoTo(),Nil,"Alt-F8"}, {_I("Back"),@mnu_Back(),Nil,"Alt-B"} }

   FMenu( oEdit, aMenu, aXY[1], aXY[2] )

   RETURN Nil

FUNCTION mnu_Back( oEdit )
   RETURN oEdit:GoTo( oEdit:nLineBack, oEdit:nPosBack )

FUNCTION mnu_Search( oEdit )

   LOCAL cScBuf := Savescreen( 09, 20, 16, 60 ), nRes, i
   LOCAL oldc := SetColor( oEdit:cColorSel+","+oEdit:cColorSel+",,"+oEdit:cColorGet+","+oEdit:cColorSel )
   LOCAL aGets := { {11,22,0,"",33,oEdit:cColorMenu,oEdit:cColorMenu}, ;
      {11,55,2,"[^]",3,oEdit:cColorSel,oEdit:cColorMenu,{||mnu_SeaHist(oEdit,aGets[1])}}, ;
      {12,23,1,.F.,1}, {12,44,1,.F.,1}, {13,23,1,.F.,1}, {13,44,1,.F.,1}, ;
      {15,25,2,_I("[Search]"),10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {15,40,2,_I("[Cancel]"),10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cSearch, lCase, lBack := .F., lWord, lRegex, cs_utf8
   LOCAL ny := oEdit:nLine, nx := oEdit:nPos

   hb_cdpSelect( "RU866" )
   @ 09, 20, 16, 60 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 14, 20 SAY "Ã"
   @ 14, 60 SAY "´"
   @ 14, 21 TO 14, 59
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY _I("Search for")
   @ 12, 22 SAY "[ ] " + _I("Case sensitive")
   @ 12, 43 SAY "[ ] " + _I("Backward")
   @ 13, 22 SAY "[ ] " + _I("Whole word")
   @ 13, 43 SAY "[ ] " + _I("Regular expr.")

   IF !Empty( TEdit():aSeaHis )
      aGets[1,4] := hb_Translate( TEdit():aSeaHis[1], "UTF8" )
      aGets[3,4] := lCase_Sea
      aGets[6,4] := lRegex_Sea
   ENDIF

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cSearch := aGets[1,4]
      lCase := aGets[3,4]
      lBack := aGets[4,4]
      lWord := aGets[5,4]
      lRegex := aGets[6,4]
      cs_utf8 := hb_Translate( cSearch,, "UTF8" )
      IF ( i := Ascan( TEdit():aSeaHis, {|cs|cs==cs_utf8} ) ) > 0
         ADel( TEdit():aSeaHis, i )
         hb_AIns( TEdit():aSeaHis, 1, cs_utf8, .F. )
      ELSE
         hb_AIns( TEdit():aSeaHis, 1, cs_utf8, Len(TEdit():aSeaHis)<hb_hGetDef(TEdit():options,"seahismax",10) )
      ENDIF
      IF oEdit:Search( cSearch, lCase_Sea := lCase, !lBack, lWord_Sea := lWord, lRegex_Sea := lRegex, @ny, @nx )
         oEdit:GoTo( ny, nx, 0 )
      ELSE
         edi_Alert( _I("String is not found") + ":;" + cSearch )
         oEdit:lTextOut := .T.
      ENDIF
   ENDIF

   Restscreen( 09, 20, 15, 60, cScBuf )
   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN Nil

FUNCTION mnu_SeaHist( oEdit, aGet )

   LOCAL aMenu, i, bufc
   LOCAL y1 := aGet[1]+1, x1 := aGet[2], y2, x2 := x1 + aGet[5]

   IF !Empty( TEdit():aSeaHis )
      aMenu := Array( Len(TEdit():aSeaHis) )
      FOR i := 1 TO Len(aMenu)
         aMenu[i] := { hb_Translate( TEdit():aSeaHis[i], "UTF8" ), Nil, i }
      NEXT
      y2 := y1 + Min( 6,Len(aMenu)+1 )
      bufc := SaveScreen( y1, x1, y2, x2 )
      IF !Empty( i := FMenu( oEdit, aMenu, y1, x1, y2, x2 ) )
         aGet[4] := aMenu[i,1]
         ShowGetItem( aGet, .F., oEdit:lUtf8,, {.F.,1,"",.F.,.F.} )
      ENDIF
      RestScreen( y1, x1, y2, x2, bufc )
      __KeyBoard(Chr(K_UP))
   ENDIF

   RETURN Nil

FUNCTION mnu_SeaNext( oEdit, lNext )

   LOCAL ny := oEdit:nLine, nx := oEdit:nPos
   LOCAL cSearch

   IF !Empty( TEdit():aSeaHis )
      cSearch := hb_Translate(TEdit():aSeaHis[1],"UTF8")
      IF oEdit:Search( cSearch, lCase_Sea, lNext, lWord_Sea, lRegex_Sea, @ny, @nx )
         oEdit:GoTo( ny, nx, 0 )
      ELSE
         edi_Alert( _I("String is not found") + ":;" + cSearch )
         oEdit:lTextOut := .T.
         edi_SetPos( oEdit )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION mnu_SeaAndRepl( oEdit )

   LOCAL cScBuf := Savescreen( 09, 20, 17, 60 ), nRes, i
   LOCAL oldc := SetColor( oEdit:cColorSel+","+oEdit:cColorSel+",,"+oEdit:cColorGet+","+oEdit:cColorSel )
   LOCAL aGets := { {11,22,0,"",33,"W+/BG","W+/BG"}, ;
      {11,55,2,"[^]",3,oEdit:cColorSel,oEdit:cColorMenu,{||mnu_SeaHist(oEdit,aGets[1])}}, ;
      {13,22,0,"",33,oEdit:cColorMenu,oEdit:cColorMenu}, ;
      {13,55,2,"[^]",3,oEdit:cColorSel,oEdit:cColorMenu,{||mnu_ReplHist(oEdit,aGets[3])}}, ;
      {14,23,1,.F.,1}, {14,43,1,.F.,1}, {15,23,1,.F.,1}, {15,43,1,.F.,1}, ;
      {17,25,2,_I("[Replace]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {17,40,2,_I("[Cancel]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cSearch, cRepl, lCase, lBack := .F., lWord, lRegex, cs_utf8, cr_utf8, nSeaLen
   LOCAL ny := oEdit:nLine, nx := oEdit:nPos

   hb_cdpSelect( "RU866" )
   @ 09, 20, 18, 60 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 16, 20 SAY "Ã"
   @ 16, 60 SAY "´"
   @ 16, 21 TO 16, 59
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY _I("Search for")
   @ 12,22 SAY _I("Replace with")
   @ 14, 22 SAY "[ ] " + _I("Case sensitive")
   @ 14, 42 SAY "[ ] " + _I("Backward")
   @ 15, 22 SAY "[ ] " + _I("Whole word")
   @ 15, 42 SAY "[ ] " + _I("Regular expr.")

   IF !Empty( TEdit():aSeaHis )
      aGets[1,4] := hb_Translate( TEdit():aSeaHis[1], "UTF8" )
      aGets[5,4] := lCase_Sea
   ENDIF
   IF !Empty( TEdit():aReplHis )
      aGets[3,4] := hb_Translate( TEdit():aReplHis[1], "UTF8" )
   ENDIF

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cSearch := aGets[1,4]
      nSeaLen := cp_Len( oEdit:lUtf8, cSearch )
      cRepl := aGets[3,4]
      lCase := aGets[5,4]
      lBack := aGets[6,4]
      lWord := aGets[7,4]
      lRegex := aGets[8,4]
      cs_utf8 := hb_Translate( cSearch,, "UTF8" )
      cr_utf8 := hb_Translate( cRepl,, "UTF8" )
      IF ( i := Ascan( TEdit():aSeaHis, {|cs|cs==cs_utf8} ) ) > 0
         ADel( TEdit():aSeaHis, i )
         hb_AIns( TEdit():aSeaHis, 1, cs_utf8, .F. )
      ELSE
         hb_AIns( TEdit():aSeaHis, 1, cs_utf8, Len(TEdit():aSeaHis)<hb_hGetDef(TEdit():options,"seahismax",10) )
      ENDIF
      IF ( i := Ascan( TEdit():aReplHis, {|cs|cs==cr_utf8} ) ) > 0
         ADel( TEdit():aReplHis, i )
         hb_AIns( TEdit():aReplHis, 1, cr_utf8, .F. )
      ELSE
         hb_AIns( TEdit():aReplHis, 1, cr_utf8, Len(TEdit():aReplHis)<hb_hGetDef(TEdit():options,"seahismax",10) )
      ENDIF
      nRes := 0
      lWord_Sea := lWord; lRegex_Sea := lRegex
      DO WHILE .T.
         IF oEdit:Search( cSearch, lCase_Sea := lCase, !lBack, lWord, lRegex, @ny, @nx,, @nSeaLen )
            oEdit:GoTo( ny, nx, nSeaLen )
            oEdit:TextOut()
            edi_SetPos( oEdit )
            IF nRes != 2
               nRes := mnu_ReplNext( oEdit, nSeaLen )
            ENDIF
            IF nRes == 1 .OR. nRes == 2
               oEdit:DelText( ny, nx, ny, nx + nSeaLen - 1 )
               oEdit:InsText( ny, nx, cRepl )
            ELSEIF nRes == 3
               LOOP
            ELSE
               EXIT
            ENDIF
         ELSE
            edi_Alert( _I("String is not found:;") + cSearch )
            oEdit:lTextOut := .T.
            EXIT
         ENDIF
      ENDDO
   ENDIF

   Restscreen( 09, 20, 17, 60, cScBuf )
   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN Nil

FUNCTION mnu_ReplHist( oEdit, aGet )

   LOCAL aMenu, i, bufc

   IF !Empty( TEdit():aReplHis )
      aMenu := Array( Len(TEdit():aReplHis) )
      FOR i := 1 TO Len(aMenu)
         aMenu[i] := { hb_Translate( TEdit():aReplHis[i], "UTF8" ), Nil, i }
      NEXT
      bufc := SaveScreen( 14, 22, 14 + Min(6,Len(aMenu)+1), 55 )
      IF !Empty( i := FMenu( oEdit, aMenu, 14, 22, 14 + Min(6,Len(aMenu)+1), 55 ) )
         aGet[4] := aMenu[i,1]
         ShowGetItem( aGet, .F., oEdit:lUtf8,, {.F.,1,"",.F.,.F.} )
      ENDIF
      RestScreen( 14, 22, 14 + Min(6,Len(aMenu)+1), 55, bufc )
      __KeyBoard(Chr(K_UP))
   ENDIF

   RETURN Nil

FUNCTION mnu_ReplNext( oEdit, nSeaLen )

   LOCAL oldc := SetColor( oEdit:cColorSel+","+oEdit:cColorSel+",,,"+oEdit:cColorSel )
   LOCAL y1 := Iif( Row()>oEdit:y2-6, oEdit:y1+2, oEdit:y2-6 ), x1 := oEdit:x2-40, nRes := 0
   LOCAL aGets := { ;
      {y1+4,x1+1,2,_I("[Replace]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+4,x1+13,2,_I("[All]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+4,x1+19,2,_I("[Skip]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+4,x1+32,2,_I("[Cancel]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}} }
   LOCAL cSearch, cRepl, ny, nx
   STATIC pKeys

   IF Empty( pKeys )
      pKeys := hb_Hash( 0x42000073, Chr(K_CTRL_END)+Chr(K_UP)+Chr(K_SPACE) )
   ENDIF

   IF !Empty( TEdit():aSeaHis ) .AND. !Empty( TEdit():aReplHis )
      hb_cdpSelect( "RU866" )
      @ y1, x1, y1+5, x1+40 BOX "ÚÄ¿³ÙÄÀ³ "
      @ y1+3, x1 SAY "Ã"
      @ y1+3, x1+40 SAY "´"
      @ y1+3, x1+1 TO y1+3, x1+39
      hb_cdpSelect( oEdit:cp )

      ny := oEdit:nLine
      nx := oEdit:nPos
      cSearch := cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx, nSeaLen )
      cRepl := hb_Translate(TEdit():aReplHis[1],"UTF8")
      @ y1+1,x1+2 SAY _I('Replace') + ' "' + cSearch + '"'
      @ y1+2,x1+2 SAY _I('With') + ' "' + cRepl + '"'

      nRes := edi_Read( aGets, pKeys )
      SetColor( oldc )
      edi_SetPos( oEdit )
   ENDIF

   RETURN nRes

FUNCTION mnu_GoTo( oEdit )

   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu )
   LOCAL aGets := { ;
      {10,32,11,_I("Go to position")}, ;
      {11,27,0,"",26}, ;
      {13,28,2,_I("[Ok]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {13,42,2,_I("[Cancel]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL arr, ny, nx, nRes

   hb_cdpSelect( "RU866" )
   @ 09, 25, 14, 55 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 12, 25 SAY "Ã"
   @ 12, 55 SAY "´"
   @ 12, 26 TO 12, 54
   hb_cdpSelect( oEdit:cp )

   SetColor( oEdit:cColorMenu )

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      arr := hb_aTokens( aGets[2,4], "," )
      ny := Val( arr[1] )
      nx := Iif( Len(arr)>1 .AND. Val(arr[2])>0, Val(arr[2]), 1 )
      IF ny > 0 .AND. ny <= Len(oEdit:aText)
         IF nx >= cp_Len( oEdit:lUtf8, oEdit:aText[ny] )
            nx := 1
         ENDIF
         oEdit:GoTo( ny, edi_Col2Pos( oEdit, ny, nx ), 0 )
      ENDIF
   ENDIF

   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN Nil

FUNCTION mnu_Plugins( oEdit )

   LOCAL aMenu := {}, i, nPos

   FOR i := 1 TO Len( TEdit():aPlugins )
      IF Empty( TEdit():aPlugins[i,3] ) .OR. TEdit():aPlugins[i,3] == oEdit:cSyntaxType
         nPos := At( ",", TEdit():aPlugins[i,2] )
         AAdd( aMenu, { Iif( nPos==0, TEdit():aPlugins[i,2], Left(TEdit():aPlugins[i,2],nPos-1) ), Nil, i, Iif( nPos==0, Nil, Ltrim(Substr(TEdit():aPlugins[i,2],nPos+1)) ) } )
      ENDIF
   NEXT
   IF !Empty( aMenu )
      IF ( i := FMenu( oEdit, aMenu, 2, 6 ) ) > 0
         i := aMenu[i,3]
         edi_RunPlugin( oEdit, TEdit():aPlugins, i )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION mnu_Sele( oEdit )

   LOCAL aMenu := { {_I("UPPER CASE"),@edi_ConvertCase(),.T.,"U"}, ;
      {_I("lower case"),@edi_ConvertCase(),.F.,"u"}, ;
      {_I("Indent right"),@mnu_Indent(),{.T.},"> "}, ;
      {_I("Indent left"),@mnu_Indent(),{.F.},"< "}, ;
      {_I("Insert right"),@mnu_AddToSele(),{.F.}}, {_I("Insert left"),@mnu_AddToSele(),{.T.}} }
   LOCAL cPlugin

   IF oEdit:nSeleMode == 2
      Aadd( aMenu, {_I("Sorting"), @mnu_SortSele() } )
   ENDIF
   IF !Empty( oEdit:hSelePlug )
      IF Valtype( oEdit:hSelePlug ) == "C"
         IF !Empty( cPlugin := edi_FindPath( "plugins" + hb_ps() + oEdit:hSelePlug ) )
            TEdit():hSelePlug := hb_hrbLoad( cPlugin )
         ENDIF
      ENDIF
      IF !Empty( TEdit():hSelePlug ) .AND. Valtype( oEdit:hSelePlug ) != "C"
         hb_hrbDo( TEdit():hSelePlug, oEdit, aMenu )
      ENDIF
   ENDIF

   FMenu( oEdit, aMenu, 2, 6 )

   RETURN Nil

FUNCTION mnu_Indent( oEdit, aParams )

   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu ), y1 := Row(), x1 := Col()-6
   LOCAL aGets := { {y1+1,x1+11,0,"1",2} }, nRes, i
   LOCAL nVal := Iif( Len(aParams)>1, aParams[2], Nil )

   IF Empty( nVal )
      hb_cdpSelect( "RU866" )
      @ y1, x1, y1+2, x1+15 BOX "ÚÄ¿³ÙÄÀ³ "
      @ y1+1, x1+2 SAY _I("Columns") +":"
      hb_cdpSelect( oEdit:cp )
      nRes := edi_READ( aGets )
      IF ( nVal := Val( aGets[1,4] ) ) == 0
         nVal := 1
      ENDIF
   ELSE
      nRes := 1
   ENDIF

   IF nRes > 0
      FOR i := 1 TO nVal
         edi_Indent( oEdit, aParams[1] )
      NEXT
      edi_SetLastSeleOper( {@mnu_Indent(),{aParams[1],nVal}} )
   ENDIF

   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN Nil

FUNCTION mnu_AddToSele( oEdit, aParams )

   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu ), y1 := Row(), x1 := Col()-6
   LOCAL aGets := { {y1+1,x1+11,0,"",34} }, i, cText
   LOCAL nby1, nbx1, nby2, nbx2, nPos

   IF Len( aParams ) > 1 .AND. !Empty( aParams[2] )
      cText := aParams[2]
   ELSE
      hb_cdpSelect( "RU866" )
      @ y1, x1, y1+2, x1+46 BOX "ÚÄ¿³ÙÄÀ³ "
      @ y1+1, x1+2 SAY _I("Input")+":"
      hb_cdpSelect( oEdit:cp )
      IF edi_READ( aGets ) > 0 .AND. !Empty( aGets[1,4] )
         cText := aGets[1,4]
      ENDIF
   ENDIF

   IF !Empty( cText )
      IF oEdit:nby1 <= oEdit:nby2
         nby1 := oEdit:nby1; nby2 := oEdit:nby2; nbx1 := oEdit:nbx1; nbx2 := oEdit:nbx2
      ELSE
         nby1 := oEdit:nby2; nby2 := oEdit:nby1; nbx1 := oEdit:nbx2; nbx2 := oEdit:nbx1
      ENDIF
      oEdit:Undo( nby1, nbx1,,, UNDO_OP_START )
      FOR i := nby1 TO nby2
         IF i == nby2 .AND. nbx2 == 1
            EXIT
         ENDIF
         IF aParams[1]
            nPos := Iif( oEdit:nSeleMode == 2, ;
               oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol(nby1,nbx1) ), ;
               Iif( i==nby1,nbx1,1 ) )
         ELSE
            nPos := Iif( oEdit:nSeleMode == 2, ;
               oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol(nby2,nbx2) ), ;
               Iif( i==nby2,nbx2,cp_Len(oEdit:lUtf8,oEdit:aText[i])+1 ) )
         ENDIF
         oEdit:InsText( i, nPos, cText, .F. )
      NEXT
      oEdit:Undo( nby2, nbx2,,, UNDO_OP_END )
      edi_SetLastSeleOper( {@mnu_AddToSele(),{aParams[1],cText}} )
   ENDIF

   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN Nil

FUNCTION mnu_SortSele( oEdit, nAsc )

   LOCAL i, j, arr, y1 := Row(), x1 := Col()-6
   LOCAL nby1, nbx1, nby2, nbx2, nvx1, nvx2

   IF nAsc == Nil
      nAsc := FMenu( oEdit, {_I("Ascending"),_I("Descending")}, y1, x1 )
   ENDIF
   IF nAsc > 0
      IF oEdit:nby1 <= oEdit:nby2
         nby1 := oEdit:nby1; nby2 := oEdit:nby2; nbx1 := oEdit:nbx1; nbx2 := oEdit:nbx2
      ELSE
         nby1 := oEdit:nby2; nby2 := oEdit:nby1; nbx1 := oEdit:nbx2; nbx2 := oEdit:nbx1
      ENDIF
      arr := Array( nby2-nby1+1,2 )
      FOR i := nby1 TO nby2
         nvx1 := nbx1; nvx2 := nbx2
         IF i != nby1
            nvx1 := oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol( nby1,nbx1 ) )
         ENDIF
         IF i != nby2
            nvx2 := oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol( nby2,nbx2 ) )
         ENDIF
         IF nvx1 > nvx2
            j := nvx1; nvx1 := nvx2; nvx2 := j
         ENDIF
         arr[i-nby1+1,1] := cp_Substr( oEdit:lUtf8, oEdit:aText[i], nvx1, nvx2-nvx1 )
         arr[i-nby1+1,2] := oEdit:aText[i]
      NEXT
      arr := ASort( arr,,, Iif( nAsc==1, {|z,y| z[1] < y[1] }, {|z,y| z[1] > y[1] } ) )
      oEdit:lTextOut := .T.
      oEdit:Undo( nby1, nbx1, nby2, nbx2, UNDO_OP_START )
      FOR i := nby1 TO nby2
         oEdit:DelText( i, 1, i, cp_Len( oEdit:lUtf8, oEdit:aText[i] ) )
         oEdit:InsText( i, 1, arr[i-nby1+1,2], .F. )
      NEXT
      oEdit:Undo( nby1, nbx1, nby2, nbx2, UNDO_OP_END )
      edi_SetLastSeleOper( {@mnu_SortSele(),nAsc} )
   ENDIF

   RETURN Nil

FUNCTION edi_ChgMode( oEdit, nMode, lNoDelay )

   IF Empty( lNoDelay )
      SetColor( oEdit:cColorSel )
      Scroll( oEdit:y1-1, oEdit:x1, oEdit:y1-1, oEdit:x2 )
      Inkey( 0.1 )
      edi_SetPos( oEdit )
   ENDIF
   IF nMode != Nil
      oEdit:nMode := nMode
      oEdit:WriteTopPane( 1 )
      IF nMode == 2
         mnu_CmdLine( oEdit )
      ENDIF
   ELSE
      IF oEdit:nMode == 0
         oEdit:nMode := 1
         oEdit:WriteTopPane( 1 )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION edi_SetPos( oEdit, nLine, nPos )

   IF nLine != Nil; oEdit:nLine := nLine; ENDIF
   IF nPos != Nil; oEdit:nPos := nPos; ENDIF

   RETURN DevPos( oEdit:LineToRow(nLine), oEdit:PosToCol(nLine,nPos) )

FUNCTION edi_Move( oEdit, nKey, nRepeat )

   LOCAL i, x
   LOCAL nLine, nPos, nRow, nLine1, nxOfLine

   IF nRepeat == Nil; nRepeat := 1; ENDIF
   IF nKey == 71 // G
      IF nRepeat == 1
         oEdit:nPosBack := oEdit:nPos
         oEdit:nLineBack := oEdit:nLine
         IF oEdit:lWrap
            x := Len(oEdit:aText)
            oEdit:GoTo( x, cp_Len(oEdit:lUtf8, oEdit:aText[x]) )
         ELSE
            IF Len( oEdit:aText ) > oEdit:y2-oEdit:y1+1
               oEdit:nxFirst := 1
               oEdit:nyFirst := Len( oEdit:aText ) - (oEdit:y2-oEdit:y1)
               oEdit:lTextOut := .T.
               edi_SetPos( oEdit, oEdit:nyFirst + oEdit:y2 - oEdit:y1, Min( oEdit:nPos,Iif(nKey==K_CTRL_END,1,cp_Len(oEdit:lUtf8,ATail(oEdit:aText))+1)) )
            ELSE
               edi_SetPos( oEdit, Len(oEdit:aText)+oEdit:y1-1, Iif(nKey==K_CTRL_END,1,Min(oEdit:nPos,cp_Len(oEdit:lUtf8,ATail(oEdit:aText))+1)) )
            ENDIF
         ENDIF
      ELSE
         oEdit:GoTo( nRepeat )
      ENDIF
      RETURN Nil
   ELSEIF nKey == 48  // 0
      IF oEdit:nxFirst > 1
         oEdit:nxFirst := 1
         oEdit:lTextOut := .T.
      ENDIF
      edi_SetPos( oEdit, oEdit:nLine, oEdit:nxFirst )
      RETURN Nil
   ELSEIF nKey == 94  // ^
      edi_Move( oEdit, 48 )
      edi_Move( oEdit, 119 )
      RETURN Nil
   ELSEIF nKey == 36  // $
      edi_GoEnd( oEdit )
      RETURN Nil
   ENDIF
   FOR i := 1 TO nRepeat
      SWITCH nKey
      CASE 104   // h Move left
         edi_GoLeft( oEdit )
         EXIT
      CASE 108   // l Move right
         edi_GoRight( oEdit )
         EXIT
      CASE 107   // k Move up
         edi_GoUp( oEdit )
         EXIT
      CASE 106   // j Move down
         edi_GoDown( oEdit )
         EXIT
      CASE 119   // w Move to the next word
         edi_NextWord( oEdit, .F. )
         EXIT
      CASE 87    // W Move to the next big word
         edi_NextWord( oEdit, .T. )
         EXIT
      CASE 101   // e Move to the end of word
         edi_NextWord( oEdit, .F., .T. )
         EXIT
      CASE 69    // E Move to the end of a big word
         edi_NextWord( oEdit, .T., .T. )
         EXIT
      CASE 98    // b Move to the previous word
         edi_PrevWord( oEdit, .F. )
         EXIT
      CASE 66    // B Move to the previous big word
         edi_PrevWord( oEdit, .T. )
         EXIT
      CASE K_CTRL_F   // Ctrl-F PgDn
         IF oEdit:lWrap
            nLine := oEdit:nLine; nPos := oEdit:nPos
            nRow := edi_CalcWrapped( oEdit, nLine, @nPos,, .T. ) + (oEdit:y2-oEdit:y1)
            nLine := edi_CalcWrapped( oEdit, nRow, @nPos,, .F. )
            IF nLine <= Len(oEdit:aText)
               IF nRow > oEdit:y2
                  nLine1 := edi_CalcWrapped( oEdit, oEdit:y2, 1, @nxOfLine, .F. )
                  oEdit:nyFirst := nLine1
                  oEdit:nxOfLine := nxOfLine
                  oEdit:lTextOut := .T.
               ENDIF
               edi_SetPos( oEdit, nLine, nPos )
            ELSE
               edi_Move( oEdit, 71 )
            ENDIF
         ELSE
            IF oEdit:nyFirst + (oEdit:y2-oEdit:y1) <= Len( oEdit:aText )
               oEdit:nyFirst += (oEdit:y2-oEdit:y1)
               oEdit:lTextOut := .T.
               edi_SetPos( oEdit, Min( oEdit:nLine+(oEdit:y2-oEdit:y1),Len(oEdit:aText) ), oEdit:nPos )
            ELSE
               edi_SetPos( oEdit, Len(oEdit:aText), oEdit:nPos )
            ENDIF
         ENDIF
         EXIT
      CASE K_CTRL_B   // Ctrl-B PgUp
         IF oEdit:lWrap
            nLine := oEdit:nLine; nPos := oEdit:nPos
            nRow := edi_CalcWrapped( oEdit, nLine, @nPos,, .T. ) - (oEdit:y2-oEdit:y1)
            nLine := edi_CalcWrapped( oEdit, nRow, @nPos, @nxOfLine, .F. )
            IF nLine > 0
               IF nRow < oEdit:y1
                  oEdit:nyFirst := nLine
                  oEdit:nxOfLine := nxOfLine
                  oEdit:lTextOut := .T.
               ENDIF
            ELSE
               oEdit:nxFirst := oEdit:nyFirst := oEdit:nxOfLine := nPos := nLine := 1
               oEdit:lTextOut := .T.
            ENDIF
            edi_SetPos( oEdit, nLine, nPos )
         ELSE
            IF oEdit:nyFirst > (oEdit:y2-oEdit:y1)
               oEdit:nyFirst -= (oEdit:y2-oEdit:y1)
               oEdit:lTextOut := .T.
               edi_SetPos( oEdit, oEdit:nLine-(oEdit:y2-oEdit:y1), oEdit:nPos )
            ELSEIF oEdit:nyFirst > 1
               x := oEdit:nyFirst - 1
               oEdit:nyFirst := 1
               oEdit:lTextOut := .T.
               edi_SetPos( oEdit, oEdit:nLine-x, oEdit:nPos )
            ELSE
               edi_Setpos( oEdit, 1, oEdit:nPos )
            ENDIF
         ENDIF
         EXIT
      END
   NEXT

   RETURN Nil

STATIC FUNCTION edi_GoUp( oEdit )

   LOCAL nLine := oEdit:nLine, nPos := oEdit:nPos, nRow, nxOfLine

   IF oEdit:lWrap
      nRow := edi_CalcWrapped( oEdit, nLine, @nPos,, .T. ) - 1
      nLine := edi_CalcWrapped( oEdit, nRow, @nPos, @nxOfLine, .F. )
      IF nLine > 0
         IF nRow < oEdit:y1
            oEdit:nyFirst := nLine
            oEdit:nxOfLine := nxOfLine
            oEdit:lTextOut := .T.
         ENDIF
         edi_SetPos( oEdit, nLine, nPos )
      ENDIF
   ELSE
      IF oEdit:nLine == oEdit:nyFirst
         IF oEdit:nyFirst > 1
            oEdit:nyFirst --
            oEdit:lTextOut := .T.
            edi_SetPos( oEdit, oEdit:nyFirst, oEdit:ColToPos( Row(), Col() ) )
         ENDIF
      ELSE
         edi_SetPos( oEdit, oEdit:nLine-1, oEdit:ColToPos( Row()-1, Col() ) )
      ENDIF
   ENDIF
   RETURN Nil

STATIC FUNCTION edi_GoDown( oEdit )

   LOCAL nLine := oEdit:nLine, nPos := oEdit:nPos, nRow, nLine1, nxOfLine

   IF oEdit:lWrap
      nRow := edi_CalcWrapped( oEdit, nLine, @nPos,, .T. ) + 1
      nLine := edi_CalcWrapped( oEdit, nRow, @nPos,, .F. )
      IF nLine <= Len(oEdit:aText)
         IF nRow > oEdit:y2
            nLine1 := edi_CalcWrapped( oEdit, oEdit:y1 + 1, 1, @nxOfLine, .F. )
            oEdit:nyFirst := nLine1
            oEdit:nxOfLine := nxOfLine
            oEdit:lTextOut := .T.
         ENDIF
         edi_SetPos( oEdit, nLine, nPos )
      ENDIF
   ELSE
      IF oEdit:nLine < Len(oEdit:aText)
         IF oEdit:LineToRow() == oEdit:y2
            oEdit:nyFirst ++
            oEdit:lTextOut := .T.
         ENDIF
         edi_SetPos( oEdit, oEdit:nLine+1, oEdit:ColToPos( oEdit:LineToRow(oEdit:nLine+1), Col() ) )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_GoRight( oEdit )

   IF !oEdit:lWrap .AND. oEdit:PosToCol() == oEdit:x2
      oEdit:nxFirst ++
      oEdit:lTextOut := .T.
   ENDIF
   edi_SetPos( oEdit, oEdit:nLine, oEdit:nPos+1 )

   RETURN Nil

STATIC FUNCTION edi_GoLeft( oEdit )

   IF oEdit:nPos > 1
      IF !oEdit:lWrap .AND. oEdit:PosToCol() == oEdit:x1
         IF oEdit:nxFirst > 1
            oEdit:nxFirst --
            oEdit:lTextOut := .T.
         ELSE
            RETURN Nil
         ENDIF
      ENDIF
      edi_SetPos( oEdit, oEdit:nLine, oEdit:nPos-1 )
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_GoEnd( oEdit )

   LOCAL n := oEdit:nLine, nPos, nCol

   IF oEdit:lWrap
      edi_SetPos( oEdit, n, cp_Len( oEdit:lUtf8, oEdit:aText[n] ) + 1 )
   ELSE
      nCol := edi_ExpandTabs( oEdit, oEdit:aText[n], 1, .T. ) + 1
      IF nCol < oEdit:nxFirst .OR. nCol - oEdit:nxFirst > oEdit:x2 - oEdit:x1
         oEdit:nxFirst := Max( 1, nCol - ( oEdit:x2 - oEdit:x1 ) + 1 )
         oEdit:lTextOut := .T.
      ENDIF
      edi_SetPos( oEdit, n, oEdit:ColToPos( oEdit:LineToRow(n), nCol - oEdit:nxFirst + oEdit:x1) )
   ENDIF
   RETURN Nil

STATIC FUNCTION edi_SeaWord( oEdit, lBack )

   LOCAL i, x, s, n := oEdit:nLine

   i := edi_PrevWord( oEdit, .F., .F.,,, oEdit:nPos+1 )
   x := edi_NextWord( oEdit, .F., .T., .F.,, oEdit:nPos-1 )
   s := cp_Substr( oEdit:lUtf8, oEdit:aText[n], i, x-i+1 )
   i := Iif( lBack, i-1, oEdit:nPos )
   IF oEdit:Search( s, .T., !lBack, .T., .F., @n, @i )
      oEdit:GoTo( n, i, 0 )
   ENDIF

   RETURN Nil

FUNCTION edi_SelectW( oEdit, lBigW )

   edi_PrevWord( oEdit, lBigW,, .T. )
   oEdit:nby1 := oEdit:nLine
   oEdit:nbx1 := oEdit:nPos
   edi_NextWord( oEdit, lBigW, .T. )
   edi_GoRight( oEdit )
   oEdit:nby2 := oEdit:nLine
   oEdit:nbx2 := oEdit:nPos

   RETURN Nil

FUNCTION edi_ConvertCase( oEdit, lUpper )

   LOCAL s := edi_GetSelected( oEdit ), lUtf8 := oEdit:lUtf8

   IF !Empty( s )
      edi_SetLastSeleOper( {@edi_ConvertCase(),lUpper} )
      edi_ReplSelected( oEdit, Iif( lUpper, cp_Upper(lUtf8,s), cp_Lower(lUtf8,s) ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_AlphaNum( nch )

   RETURN (nch >= 48 .AND. nch <= 57) .OR. (nch >= 65 .AND. nch <= 90) .OR. ;
      (nch >= 97 .AND. nch <= 122) .OR. nch == 95 .OR. nch >= 128

FUNCTION edi_NextWord( oEdit, lBigW, lEndWord, lChgPos, ny, nx )
   LOCAL nInitPos := nx, nLen, lUtf8 := oEdit:lUtf8, ch, nch
   LOCAL lOk := .F., lAlphaNum

   lBigW := Iif( lBigW == Nil, .F., lBigW )
   IF ny == Nil
      ny := oEdit:nLine
   ENDIF
   IF nx == Nil
      nInitPos := nx := oEdit:nPos
   ENDIF
   nLen := cp_Len( lUtf8, oEdit:aText[ny] )

   IF nx > nLen
      RETURN nLen
   ENDIF

   ch := cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 )
   IF ch == " " .OR. ch == cTab
      DO WHILE ++nx <= nLen .AND. cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 ) == ch; ENDDO
      lOk := Empty( lEndWord )
   ENDIF

   IF !lOk
      lAlphaNum := edi_AlphaNum( cp_Asc( lUtf8, cp_Substr(lUtf8,oEdit:aText[ny],nx,1) ) )
      DO WHILE ++nx <= nLen
         ch := cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 )
         IF ( ch == " " ) .OR. ( !lBigW .AND. ;
               lAlphaNum != edi_AlphaNum( cp_Asc(lUtf8,ch) ) )
            IF !Empty( lEndWord ) .AND. nx - nInitPos > 1
               nx --
               EXIT
            ENDIF
            IF ch == " " .OR. ch == cTab
               DO WHILE ++nx <= nLen .AND. cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 ) == ch; ENDDO
            ENDIF
            IF Empty( lEndWord )
               EXIT
            ENDIF
            lAlphaNum := edi_AlphaNum( cp_Asc( lUtf8, cp_Substr(lUtf8,oEdit:aText[ny],nx,1) ) )
         ENDIF
      ENDDO
      IF nx > nLen .AND. !Empty( lEndWord )
         nx --
      ENDIF
   ENDIF

   IF lChgPos == Nil .OR. lChgPos
      IF oEdit:PosToCol(ny,nx) >= oEdit:x2
         oEdit:nxFirst := nx + oEdit:x1 - oEdit:x2 + 3
         oEdit:lTextOut := .T.
      ENDIF
      edi_SetPos( oEdit, oEdit:nLine, nx )
   ENDIF

   RETURN nx

FUNCTION edi_PrevWord( oEdit, lBigW, lChgPos, lIn, ny, nx )
   LOCAL lUtf8 := oEdit:lUtf8
   LOCAL ch, lAlphaNum

   lBigW := Iif( lBigW == Nil, .F., lBigW )
   lIn := Iif( lIn == Nil, .F., lIn )
   IF ny == Nil
      ny := oEdit:nLine
   ENDIF
   IF nx == Nil
      nx := oEdit:nPos
   ENDIF

   ch := cp_Substr( lUtf8, oEdit:aText[ny], nx-1, 1 )
   IF !lIn
      --nx
   ENDIF
   IF ch == " " .OR. ch == cTab
      IF lIn
         IF lChgPos == Nil .OR. lChgPos
            edi_SetPos( oEdit, ny, nx )
         ENDIF
         RETURN nx
      ENDIF
      DO WHILE --nx > 1 .AND. ( ( ch := cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 ) ) == " " ;
         .OR. ch == cTab )
      ENDDO
   ENDIF

   lAlphaNum := edi_AlphaNum( cp_Asc( lUtf8, cp_Substr(lUtf8,oEdit:aText[ny],nx,1) ) )
   IF lIn .AND. nx > 1 .AND. !lBigW .AND. ;
      edi_AlphaNum( cp_Asc( lUtf8, cp_Substr(lUtf8,oEdit:aText[ny],nx-1,1) ) ) != lAlphaNum
      IF lChgPos == Nil .OR. lChgPos
         edi_SetPos( oEdit, ny, nx )
      ENDIF
      RETURN nx
   ENDIF
   DO WHILE --nx > 0
      IF ( ch := cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 ) ) == " " .OR. ch == cTab .OR. ;
            ( !lBigW .AND. lAlphaNum != edi_AlphaNum( cp_Asc(lUtf8,ch) ) )
         nx ++
         EXIT
      ENDIF
   ENDDO
   nx := Max( nx, 1 )

   IF lChgPos == Nil .OR. lChgPos
      IF nx < oEdit:nxFirst
         oEdit:nxFirst := Max( nx + oEdit:x1 - oEdit:x2 + 3, 1 )
         oEdit:lTextOut := .T.
      ENDIF
      edi_SetPos( oEdit, oEdit:nLine, nx )
   ENDIF

   RETURN nx

STATIC FUNCTION edi_SaveDlg( oEdit, cDir )

   LOCAL cName
   LOCAL oldc := SetColor( oEdit:cColorSel+","+oEdit:cColorSel+",,"+oEdit:cColorGet+","+oEdit:cColorSel )
   LOCAL aGets := { {11,22,0,Iif(Left(oEdit:cFileName,1)=="$",oEdit:cFileName,hb_fnameNameExt(oEdit:cFileName)),48,oEdit:cColorMenu,oEdit:cColorMenu}, ;
      {13,22,0,cDir,46,oEdit:cColorMenu,oEdit:cColorMenu}, ;
      {13,69,2,"[D]",3,oEdit:cColorSel,oEdit:cColorMenu,{||mnu_DirList(oEdit,aGets[2],.T.)}}, ;
      {15,29,3,.T.,1}, {15,47,3,.F.,1}, {15,63,3,.F.,1} }
   STATIC pKeys

   IF Empty( pKeys )
      // 0x41020044 - Ctrl-D
      pKeys := hb_Hash( 0x41020044, Chr(K_CTRL_HOME)+Chr(K_DOWN)+Chr(K_DOWN)+Chr(K_SPACE) )
   ENDIF
   hb_cdpSelect( "RU866" )
   @ 09, 20, 19, 72 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 17, 20 SAY "Ã"
   @ 17, 72 SAY "´"
   @ 17, 21 TO 17, 71
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY _I("Save file as")
   @ 12,22 SAY _I("in directory")
   @ 15, 22 SAY " Eol: ( ) Do not change  ( ) Dos/Windows ( ) Unix"
   Aadd( aGets, {18,25,2,_I("[Save]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}} )
   Aadd( aGets, {18,58,2,_I("[Cancel]"),,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} )
   IF oEdit:lUtf8
      hb_AIns( aGets, 7, {16,24,1,oEdit:lBom,1}, .T. )
      @ 16, 23 SAY "[ ] " + _I("Add BOM")
   ENDIF
   SetColor( oEdit:cColorMenu )

   IF edi_READ( aGets,pKeys ) > 0
      cName := aGets[1,4]
      cDir := AllTrim( aGets[2,4] )
      IF !( Right( cDir, 1 ) $ "\/" )
         cDir += hb_ps()
      ENDIF
      IF aGets[5,4]
         oEdit:cEol := Chr(13) + Chr(10)
      ELSEIF aGets[6,4]
         oEdit:cEol := Chr(10)
      ENDIF
      IF oEdit:lUtf8
         oEdit:lBom := aGets[7,4]
      ENDIF
   ENDIF

   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN Iif( Empty(cName), Nil, cDir + cName )

STATIC FUNCTION edi_Indent( oEdit, lRight, nLines, lAddLast )

   LOCAL i, n, nby1, nby2, nbx2, nbx1, l := .F., nRow := Row(), nCol := Col(), s

   IF oEdit:lReadOnly
      RETURN Nil
   ENDIF
   IF oEdit:nby1 < 0 .OR. oEdit:nby2 < 0
      nby1 := oEdit:nLine
      nby2 := Iif( Empty(nLines), oEdit:nLine, oEdit:nLine+nLines-1 )
      nbx2 := 0
   ELSE
      IF oEdit:nby1 <= oEdit:nby2
         nby1 := oEdit:nby1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2; nbx1 := oEdit:nbx1
      ELSE
         nby1 := oEdit:nby2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1; nbx1 := oEdit:nbx2
      ENDIF
      IF oEdit:nSeleMode == 2
         s := edi_GetSelected( oEdit )
         oEdit:Undo( nby1, oEdit:nbx1,,, UNDO_OP_START )
         cbDele( oEdit )
         nbx1 += Iif( lRight, 1, -1 )
         edi_SetPos( oEdit, nby1, nbx1 )
         cb2Text( oEdit, 0, .T., s, .T. )
         oEdit:Undo( nby1, oEdit:nbx1,,, UNDO_OP_END )
         oEdit:nby1 := nby1; oEdit:nby2 := nby2; oEdit:nbx1 := nbx1; oEdit:nbx2 := nbx2 + Iif( lRight, 1, -1 )
         RETURN Nil
      ENDIF
   ENDIF
   FOR i := nby1 TO nby2
      IF i == nby2 .AND. nbx2 == 1
         nby2 --
         EXIT
      ENDIF
      IF lRight
         oEdit:aText[i] := Iif( Left( oEdit:aText[i],1 ) == cTab, cTab, " " ) + oEdit:aText[i]
         l := .T.
      ELSEIF Left( oEdit:aText[i],1 ) == " " .OR. Left( oEdit:aText[i],2 ) == cTab + cTab
         oEdit:aText[i] := Substr( oEdit:aText[i], 2 )
         l := .T.
      ENDIF
      n := i - oEdit:nyFirst + 1
      IF n > 0 .AND. n < oEdit:y2-oEdit:y1
         oEdit:LineOut( n )
      ENDIF
   NEXT
   IF l
      oEdit:Undo( nby1, 0, nby2, 0, UNDO_OP_SHIFT, Iif(lRight,1,-1) )
      oEdit:lUpdated := .T.
   ENDIF
   DevPos( nRow, nCol )
   IF !Empty( lAddLast )
      edi_SetLastSeleOper( {@mnu_Indent(),{lRight,1}} )
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_BookMarks( oEdit, nKey, lSet )

   LOCAL arr

   IF nKey >= 97 .AND. nKey <= 122
      IF lSet
         oEdit:hBookMarks[nKey] := { oEdit:nLine, oEdit:nPos }
      ELSE
         IF hb_hHaskey( oEdit:hBookMarks, nKey )
            arr := oEdit:hBookMarks[nKey]
            oEdit:Goto( arr[1], arr[2] )
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_Bracket( oEdit, lCalcOnly, lPairOnly, c )

   LOCAL nyInit, ny := oEdit:nLine, nx := oEdit:nPos
   LOCAL nPos := 0
   LOCAL b1 := "([{", b2 := ")]}", i, np := 0

   IF ny == 0 .OR. ny > Len( oEdit:aText ) .OR. nx > cp_Len( oEdit:lUtf8, oEdit:aText[ny] )
      RETURN 0
   ENDIF
   IF Empty( c )
      c := cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx, 1 )
   ENDIF
   nyInit := ny
   IF ( i := At( c, b1 ) ) > 0
      IF edi_InQuo( oEdit, oEdit:aText[ny], nx ) == 0
         nPos := nx
         DO WHILE ny <= Len( oEdit:aText )
            DO WHILE ( nPos := edi_FindChNext( oEdit, oEdit:aText[ny], nPos, c, Substr( b2,i,1 ) ) ) > 0
               IF cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nPos, 1 ) == c
                  np ++
               ELSEIF np > 0
                  np --
               ELSE
                  EXIT
               ENDIF
            ENDDO
            IF nPos == 0
               ny ++
            ELSE
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ELSEIF ( i := At( c, b2 ) ) > 0
      IF edi_InQuo( oEdit, oEdit:aText[ny], nx ) == 0
         nPos := nx
         DO WHILE ny > 0
            DO WHILE ( nPos := edi_FindChPrev( oEdit, oEdit:aText[ny], nPos, c, Substr( b1,i,1 ) ) ) > 0
               IF cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nPos, 1 ) == c
                  np ++
               ELSEIF np > 0
                  np --
               ELSE
                  EXIT
               ENDIF
            ENDDO
            IF nPos == 0
               ny --
               nPos := Iif( ny > 0, cp_Len( oEdit:lUtf8, oEdit:aText[ny] ) + 1, 0 )
            ELSE
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ELSEIF Empty( lPairOnly )
      IF edi_InQuo( oEdit, oEdit:aText[ny], nx ) == 0
         nPos := edi_FindChNext( oEdit, oEdit:aText[ny], nx, ")", "]", "}" )
      ENDIF
   ENDIF
   IF Empty( lCalcOnly ) .AND. nPos > 0
      oEdit:GoTo( ny, nPos )
   ENDIF

   RETURN Iif( !Empty(lCalcOnly), Iif( ny==nyInit .OR. nPos==0, nPos, {ny,nPos} ), Nil )

FUNCTION edi_FindChNext( oEdit, s, nPos, ch1, ch2, ch3 )

   LOCAL cQuotes := ["']
   LOCAL c, cQuo, lQuo := .F., nLen := cp_Len( oEdit:lUtf8,s )

   IF !Empty( oEdit:oHili )
      cQuotes := hb_hGetDef( oEdit:oHili:hHili, "quotes", cQuotes )
   ENDIF

   DO WHILE ++nPos <= nLen
      c := cp_Substr( oEdit:lUtf8, s, nPos, 1 )
      IF lQuo
         IF c == cQuo
            lQuo := .F.
         ENDIF
      ELSE
        IF c == ch1 .OR. c == ch2 .OR. c == ch3
            RETURN nPos
        ELSEIF c $ cQuotes
            lQuo := .T.
            cQuo := c
         ENDIF
      ENDIF
   ENDDO

   RETURN 0

STATIC FUNCTION edi_FindChPrev( oEdit, s, nPos, ch1, ch2, ch3 )

   LOCAL cQuotes := ["']
   LOCAL c, cQuo, lQuo := .F., nPosQuo := edi_InQuo( oEdit, s, nPos )

   IF !Empty( oEdit:oHili )
      cQuotes := hb_hGetDef( oEdit:oHili:hHili, "quotes", cQuotes )
   ENDIF

   IF nPosQuo > 0
      IF ( ( c := cp_Substr( oEdit:lUtf8, s, nPos, 1 ) ) == ch1 ;
            .OR. c == ch2 .OR. c == ch3 )
         RETURN nPosQuo
      ELSE
         nPos := nPosQuo
      ENDIF
   ENDIF
   DO WHILE --nPos > 0
      c := cp_Substr( oEdit:lUtf8, s, nPos, 1 )
      IF lQuo
         IF c == cQuo
            lQuo := .F.
         ENDIF
      ELSE
        IF c == ch1 .OR. c == ch2 .OR. c == ch3
            RETURN nPos
        ELSEIF c $ cQuotes
            lQuo := .T.
            cQuo := c
         ENDIF
      ENDIF
   ENDDO

   RETURN 0

FUNCTION edi_InQuo( oEdit, s, nPos )

   LOCAL cQuotes := ["']
   LOCAL i := 0, c, cQuo, lQuo := .F., nPosQuo := 0

   IF !Empty( oEdit:oHili )
      cQuotes := hb_hGetDef( oEdit:oHili:hHili, "quotes", cQuotes )
   ENDIF

   DO WHILE ++i < nPos
      c := cp_Substr( oEdit:lUtf8, s, i,1 )
      IF lQuo
         IF c == cQuo
            lQuo := .F.
            nPosQuo := 0
         ENDIF
      ELSE
         IF c $ cQuotes
            lQuo := .T.
            cQuo := c
            nPosQuo := i
         ENDIF
      ENDIF
   ENDDO

   RETURN nPosQuo

FUNCTION edi_Col2Pos( oEdit, nLine, nCol )

   LOCAL nPos, s := oEdit:aText[nLine], nPos1 := 1, nPos2, nLenNew := 0, nAdd := 0, nLen1
   LOCAL nTabLen := oEdit:nTabLen

   DO WHILE ( nPos2 := hb_At( cTab, s, nPos1 ) ) > 0
      IF nPos2 - nPos1 > 0
         IF oEdit:lUtf8
            nLenNew += cp_Len( oEdit:lUtf8, Substr( s, nPos1, nPos2-nPos1 ) )
         ELSE
            nLenNew += ( nPos2-nPos1 )
         ENDIF
         IF nLenNew >= nCol
            RETURN nCol - nAdd
         ENDIF
      ENDIF
      IF ( nLen1 := ( nTabLen - Iif( (nLen1:=((1+nLenNew) % nTabLen))==0,nTabLen,nLen1 ) + 1 ) ) > 0
         nLenNew += nLen1
         nAdd += Int(nLen1 - 0.99)
         IF nLenNew > nCol
            RETURN nPos2
         ENDIF
      ENDIF
      nPos1 := nPos2 + 1
   ENDDO

   RETURN nCol - nAdd

FUNCTION edi_ExpandTabs( oEdit, s, nFirst, lCalcOnly, nAdd )

   LOCAL sNew := "", nLenNew := 0, s1, nPos1 := 1, nPos2, nLen1
   LOCAL nTabLen := oEdit:nTabLen

   IF lCalcOnly == Nil; lCalcOnly := .F.; ENDIF
   nAdd := 0
   DO WHILE ( nPos2 := hb_At( cTab, s, nPos1 ) ) > 0
      IF nPos2 - nPos1 > 0
         IF lCalcOnly
            IF oEdit:lUtf8
               nLenNew += cp_Len( oEdit:lUtf8, Substr( s, nPos1, nPos2-nPos1 ) )
            ELSE
               nLenNew += ( nPos2-nPos1 )
            ENDIF
         ELSE
            s1 := Substr( s, nPos1, nPos2-nPos1 )
            sNew += s1
            nLenNew += cp_Len( oEdit:lUtf8, s1 )
         ENDIF
      ENDIF
      IF ( nLen1 := ( nTabLen - Iif( (nLen1:=((nFirst+nLenNew) % nTabLen))==0,nTabLen,nLen1 ) + 1 ) ) > 0
         IF !lCalcOnly
            sNew += Space( nLen1 )
         ENDIF
         nLenNew += nLen1
         nAdd += Int(nLen1 - 0.99)
      ENDIF
      nPos1 := nPos2 + 1
   ENDDO

   IF lCalcOnly
      IF oEdit:lUtf8
         nLenNew += cp_Len( oEdit:lUtf8, Substr( s, nPos1 ) )
      ELSE
         nLenNew += ( Len(s)-nPos1+1 )
      ENDIF
   ELSE
      s1 := Substr( s, nPos1 )
      sNew += s1
      nLenNew += cp_Len( oEdit:lUtf8, s1 )
   ENDIF

   RETURN Iif( lCalcOnly, Int(nLenNew), sNew )

FUNCTION edi_ReplSelected( oEdit, s )

   LOCAL i, j, nby1, nby2, nbx1, nbx2, nvx1, nvx2, arr

   IF oEdit:nby1 >= 0 .AND. oEdit:nby2 >= 0
      IF oEdit:nby1 < oEdit:nby2 .OR. ( oEdit:nby1 == oEdit:nby2 .AND. oEdit:nbx1 < oEdit:nbx2 )
         nby1 := oEdit:nby1; nbx1 := oEdit:nbx1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
      ELSE
         nby1 := oEdit:nby2; nbx1 := oEdit:nbx2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
      ENDIF
      IF Chr(10) $ s
         arr := hb_ATokens( s, Chr(10) )
      ENDIF
      IF nby1 == nby2
         IF arr == Nil .AND. cp_Len(oEdit:lUtf8,s) == (nbx2-nbx1)
            oEdit:InsText( nby1, nbx1, s, .T., .F. )
         ENDIF
      ELSE
         IF arr == Nil //.OR. Len(arr) != (nby2-nby1+1)
            RETURN Nil
         ENDIF
         oEdit:Undo( nby1, nbx1,,, UNDO_OP_START )
         FOR i := nby1 TO nby2
            IF oEdit:nSeleMode == 2
               nvx1 := nbx1; nvx2 := nbx2
               IF i != nby1
                  nvx1 := oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol( nby1,nbx1 ) )
               ENDIF
               IF i != nby2
                  nvx2 := oEdit:ColToPos( oEdit:LineToRow(i), oEdit:PosToCol( nby2,nbx2 ) )
               ENDIF
               IF nvx1 > nvx2
                  j := nvx1; nvx1 := nvx2; nvx2 := j
               ENDIF
               IF cp_Len( oEdit:lUtf8, arr[i-nby1+1] ) == nvx2 - nvx1
                  oEdit:InsText( i, nvx1, arr[i-nby1+1], .T., .F. )
               ELSE
                  oEdit:DelText( i, nvx1, i, nvx2-1 )
                  oEdit:InsText( i, nvx1, arr[i-nby1+1], .F., .F. )
               ENDIF
            ELSE
               oEdit:InsText( i, nbx1, arr[i-nby1+1], .T., .F. )
            ENDIF
         NEXT
         oEdit:Undo( nby1, nbx1,,, UNDO_OP_END )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION edi_FillSelected( oEdit, cChar )

   LOCAL arr := hb_ATokens( edi_GetSelected( oEdit ), Chr(10) ), i, s := "", nLen := Len(arr)

   edi_SetLastSeleOper( {@edi_FillSelected(),cChar} )

   FOR i := 1 TO nLen
      s += Replicate( cChar, Len( arr[i] ) ) + Iif( i == nLen, "", Chr(10) )
   NEXT

   edi_ReplSelected( oEdit, s )

   RETURN Nil

/*
 * edi_AddWindow( oEdit, cText, cFileName, nPlace, nSpace )
 * Adds new edit window, truncates the current edit window
 * oEdit - current window
 * cText, cFilename - the data of a new window
 * nPlace: 0 - top, 1 - left, 2 - bottom, 3 - right
 * nSpace - the number of rows or columns of a new window
 * cp - a codepage
 */
FUNCTION edi_AddWindow( oEdit, cText, cFileName, nPlace, nSpace, cp )

   LOCAL oNew, y1 := oEdit:y1, x1 := oEdit:x1, y2 := oEdit:y2, x2 := oEdit:x2, lv := .F.
   LOCAL i, n

   IF nPlace == 0
      y2 := y1 + nSpace - 1
      oEdit:y1 := y2 + 1
   ELSEIF nPlace == 1
      x2 := x1 + nSpace - 1
      oEdit:x1 := x2 + 1
      lv := .T.
   ELSEIF nPlace == 2
      y1 := y2 - nSpace
      oEdit:y2 := y1-1
   ELSEIF nPlace == 3
      x1 := x2 - nSpace
      oEdit:x2 := x1-1
      lv := .T.
      SetColor( oEdit:cColorPane )
      Scroll( y1, x1, y2, x1 )
      x1 ++
   ENDIF
   IF lv .AND. oEdit:lTopPane
      oEdit:nTopName := Max( oEdit:x2 - oEdit:x1 - Iif(oEdit:x2-oEdit:x1>54,44,37), 0 )
      y1 --
   ENDIF
   oNew := TEdit():New( "", "", y1, x1, y2, x2 )
   IF !Empty( cp )
      oNew:cp := cp
   ENDIF
   oNew:SetText( cText, cFileName )
   oNew:oParent := oEdit
   // Now we need to place new child window after a last child of oEdit
   n := Len( TEdit():aWindows ) - 1
   FOR i := n TO 1 STEP -1
      IF ( TEdit():aWindows[i] == oEdit .OR. TEdit():aWindows[i]:oParent == oEdit )
         IF i < n
            AIns( TEdit():aWindows, i+1 )
            TEdit():aWindows[i+1] := oNew
            IF TEdit():nCurr > i
               TEdit():nCurr ++
            ENDIF
            EXIT
         ENDIF
      ENDIF
   NEXT
   mnu_ToBuf( oEdit, oNew )

   RETURN oNew

FUNCTION edi_CloseWindow( xEdit, lClose )

   LOCAL oEdit, i, o, lUpd, lv := .F., lh := .F.

   IF Valtype( xEdit ) == "C"
      xEdit := Ascan( TEdit():aWindows, {|o|o:cFileName == xEdit} )
   ENDIF
   IF Valtype( xEdit ) == "N"
      oEdit := Iif( xEdit > 0 .AND. xEdit <= Len(TEdit():aWindows), TEdit():aWindows[xEdit], Nil )
   ELSEIF Valtype( xEdit ) == "O"
      oEdit := xEdit
      xEdit := Ascan( TEdit():aWindows, {|o|o == oEdit} )
   ENDIF

   IF Valtype( oEdit ) == "O"

      IF !Empty( oEdit:oHili )
         oEdit:oHili:End()
      ENDIF
      FOR i := Len( oEdit:aWindows ) TO 1 STEP -1
         IF oEdit:aWindows[i]:oParent == oEdit
            edi_CloseWindow( oEdit:aWindows[i] )
         ENDIF
      NEXT

      o := oEdit
      DO WHILE !( ( o := edi_FindWindow( o, .T. ) ) == oEdit )
         lUpd := .F.
         IF oEdit:aRect[1] == o:y2 + 1
            IF o:x1 >= oEdit:x1 .AND. o:x2 <= oEdit:x2
               IF !lh
                  o:y2 := oEdit:y2
                  lUpd := .T.
                  lv := .T.
               ENDIF
            ENDIF
         ELSEIF oEdit:y2 == o:aRect[1] - 1
            IF o:x1 >= oEdit:x1 .AND. o:x2 <= oEdit:x2
               IF !lh
                  o:y1 := oEdit:y1
                  lUpd := .T.
                  lv := .T.
               ENDIF
            ENDIF
         ENDIF
         IF oEdit:x1 == o:x2 + 2
            IF o:y1 >= oEdit:y1 .AND. o:y2 <= oEdit:y2
               IF !lv
                  o:x2 := oEdit:x2
                  lUpd := .T.
                  lh := .T.
               ENDIF
            ENDIF
         ELSEIF oEdit:x2 == o:x1 - 2
            IF o:y1 >= oEdit:y1 .AND. o:y2 <= oEdit:y2
               IF !lv
                  o:x1 := oEdit:x1
                  lUpd := .T.
                  lh := .T.
               ENDIF
            ENDIF
         ENDIF
         IF lUpd
            IF o:lTopPane
               o:nTopName := Max( o:x2 - o:x1 - Iif(o:x2-o:x1>54,44,37), 0 )
            ENDIF
            o:TextOut()
         ENDIF
      ENDDO
      TEdit():aWindows[xEdit]:oParent := Nil
      IF lClose == Nil .OR. lClose
         hb_ADel( TEdit():aWindows, xEdit, .T. )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_WindowUpdated( oEdit )

   LOCAL i, j

   FOR i := Len( oEdit:aWindows ) TO 1 STEP -1
      IF oEdit:aWindows[i]:oParent == oEdit
         IF oEdit:aWindows[i]:lUpdated
            RETURN i
         ELSEIF ( j := edi_WindowUpdated( oEdit:aWindows[i] ) ) > 0
            RETURN j
         ENDIF
      ENDIF
   NEXT

   RETURN 0

FUNCTION edi_WindowDo( oEdit, nOp )

   LOCAL o

   DO WHILE !( ( o := edi_FindWindow( o, .T. ) ) == oEdit )

      IF nOp == 1            // Expand
      ELSEIF nOp == 2        //
      ENDIF
   ENDDO

   RETURN Nil

FUNCTION edi_FindWindow( oEdit, lNext, nRow, nCol )

   LOCAL oParent := oEdit, i, iCurr, o, op

   DO WHILE oParent:oParent != Nil; oParent := oParent:oParent; ENDDO
   iCurr := Ascan( oEdit:aWindows, {|o|o == oEdit} )

   FOR i := 1 TO Len( oEdit:aWindows )
      op := o := oEdit:aWindows[i]
      IF !( o == oEdit )
         DO WHILE op:oParent != Nil; op := op:oParent; ENDDO
         IF op == oParent
            IF Valtype( lNext ) == "L"
               IF lNext .AND. i > iCurr
                  RETURN o
               ENDIF
            ELSEIF nRow >= o:y1 .AND. nRow <= o:y2 .AND. nCol >= o:x1 .AND. nCol <= o:x2
               RETURN o
            ENDIF
         ENDIF
      ENDIF
   NEXT
   IF Valtype( lNext ) == "L"
      RETURN oParent
   ENDIF

   RETURN Nil

FUNCTION edi_MapKey( oEdit, nKey )

   LOCAl c, nPos, lUtf8

   IF nKey >= 127 .AND. !Empty(cLangMapCP) .AND. !Empty(aLangMapUpper) .AND. !Empty(aLangMapLower)
      lUtf8 := (Lower(cLangMapCP) == "utf8")
      c := hb_Translate( cp_Chr( oEdit:lUtf8, nKey ), oEdit:cp, cLangMapCP )
      IF ( nPos := cp_At( lUtf8, c, aLangMapUpper[1] ) ) > 0
         RETURN cp_Asc( oEdit:lUtf8, hb_Translate( cp_Substr(oEdit:lUtf8,aLangMapUpper[2],nPos,1), cLangMapCP, oEdit:cp ) )
      ELSEIF ( nPos := cp_At( lUtf8, c, aLangMapLower[1] ) ) > 0
         RETURN cp_Asc( oEdit:lUtf8, hb_Translate( cp_Substr(oEdit:lUtf8,aLangMapLower[2],nPos,1), cLangMapCP, oEdit:cp ) )
      ENDIF
   ENDIF

   RETURN nKey

FUNCTION edi_AddPalette( cPalette, aColors, aAttrs, cColorMain, cColorSel, cColorPane, cColorBra, ;
   cColorMenu, cColorMenuSel, cColorWB, cColorWR, cColorGet )

   LOCAL hHili := hb_hash(), hHiliDef

   hHiliDef := hPalettes["default"]
   hHili["colors"] := Iif( Empty(aColors), hHiliDef["colors"], aColors )
   hHili["attrs"] := Iif( Empty(aAttrs), hHiliDef["attrs"], aAttrs )
   hHili["colormain"] := Iif( Empty(cColorMain), hHiliDef["colormain"], cColorMain )
   hHili["colorsel"] := Iif( Empty(cColorSel), hHiliDef["colorsel"], cColorSel )
   hHili["colorpane"] := Iif( Empty(cColorPane), hHiliDef["colorpane"], cColorPane )
   hHili["colorbra"] := Iif( Empty(cColorBra), hHiliDef["colorbra"], cColorBra )
   hHili["colormenu"] := Iif( Empty(cColorMenu), hHiliDef["colormenu"], cColorMenu )
   hHili["colormenusel"] := Iif( Empty(cColorMenuSel), hHiliDef["colormenusel"], cColorMenuSel )
   hHili["colorwb"] := Iif( Empty(cColorWB), hHiliDef["colorwb"], cColorWB )
   hHili["colorwr"] := Iif( Empty(cColorWR), hHiliDef["colorwr"], cColorWR )
   hHili["colorget"] := Iif( Empty(cColorGet), hHiliDef["colorget"], cColorGet )

   hPalettes[cPalette] := hHili

   RETURN hHili

FUNCTION edi_SetPalette( oEdit, cPalette )

   LOCAL hHili
   IF hb_hHaskey( hPalettes, cPalette ) .AND. hb_hHaskey( hPalettes[cPalette], "colors" )
      hb_gtinfo( HB_GTI_PALETTE, hPalettes[cPalette]["colors"] )
      IF oEdit != Nil
         oEdit:cCurrPal := oEdit:cPalette := cPalette
         hHili := hPalettes[cPalette]
         oEdit:aHiliAttrs := hHili["attrs"]
         oEdit:cColor     := hHili["colormain"]
         oEdit:cColorSel  := hHili["colorsel"]
         oEdit:cColorPane := hHili["colorpane"]
         oEdit:cColorBra  := hHili["colorbra"]
         oEdit:cColorMenu := hHili["colormenu"]
         oEdit:cColorMenuSel := hHili["colormenusel"]
         oEdit:cColorWB := hHili["colorwb"]
         oEdit:cColorWR := hHili["colorwr"]
         oEdit:cColorGet := hHili["colorget"]
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION edi_ColorC2N( cColor )

   LOCAL i, res := 0, n := 1, iValue

   IF Left( cColor,1 ) == "#"
      cColor := Substr( cColor,2 )
   ENDIF
   cColor := Trim( cColor )
   FOR i := 1 TO Len( cColor )
      iValue := Asc( SubStr( cColor,i,1 ) )
      IF iValue < 58 .AND. iValue > 47
         iValue -= 48
      ELSEIF iValue >= 65 .AND. iValue <= 70
         iValue -= 55
      ELSEIF iValue >= 97 .AND. iValue <= 102
         iValue -= 87
      ELSE
         RETURN 0
      ENDIF
      iValue *= n
      IF i % 2 == 1
         iValue *= 16
      ELSE
         n *= 256
      ENDIF
      res += iValue
   NEXT

   RETURN res

FUNCTION edi_ColorN2C( nColor )

   LOCAL s := "", n1, n2, i

   FOR i := 0 to 2
      n1 := hb_BitAnd( hb_BitShift( nColor,-i*8-4 ), 15 )
      n2 := hb_BitAnd( hb_BitShift( nColor,-i*8 ), 15 )
      s += Chr( Iif(n1<10,n1+48,n1+55) ) + Chr( Iif(n2<10,n2+48,n2+55) )
   NEXT

   RETURN s

FUNCTION edi_SetLastSeleOper( aOper )

   aLastSeleOper := aOper
   RETURN Nil

FUNCTION edi_SetLastOper( xOper, lEnded )

   IF !lDoLastOper
      lAddLast := .T.

      IF lLastOper_Ended
         aLastOper := { xOper }
         lLastOper_Ended := .F.
      ELSE
         AAdd( aLastOper, xOper )
      ENDIF

      IF !Empty( lEnded )
         lLastOper_Ended := .T.
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION edi_DoLastOper( oEdit )

   LOCAL i, j

   IF !Empty( aLastOper )
      lDoLastOper := .T.
      IF Valtype( aLastOper[1] ) == "N"
         oEdit:nMode := 0
      ENDIF
      FOR i := 1 TO Len( aLastOper )
         IF Valtype( aLastOper[i] ) == "C"
            FOR j := 1 TO Len( aLastOper[i] )
               oEdit:onKey( 0x42000000 + Asc( Substr(aLastOper[i],j,1) ) )
            NEXT
         ELSE
            oEdit:onKey( aLastOper[i] )
         ENDIF
      NEXT
      IF oEdit:nMode == 0
         oEdit:nMode := 1
      ENDIF
      lDoLastOper := .F.
   ENDIF

   RETURN Nil

FUNCTION edi_CalcWrapped( oEdit, y, x, xOf_Line, l2Screen )

   LOCAL i, nWidth, nxOf_Line, ny, nLen

   nWidth := oEdit:x2 - oEdit:x1 + 1
   nxOf_Line := oEdit:nxOfLine
   ny := oEdit:nyFirst
   nLen := cp_Len( oEdit:lUtf8, oEdit:aText[ny] )

   IF l2Screen
      i := oEdit:y1
      DO WHILE ny <= y
         nxOf_Line += nWidth
         IF ny == y
            IF nxOf_Line > x
               y := i
               x := x - (nxOf_Line - nWidth) + 1
               RETURN i
            ENDIF
         ELSE
            IF nxOf_Line > nLen
               ny ++
               nLen := cp_Len( oEdit:lUtf8, oEdit:aText[ny] )
               nxOf_Line := 1
            ENDIF
         ENDIF
         i ++
      ENDDO
   ELSE
      IF y < oEdit:y1
         FOR i := oEdit:y1-1 TO y STEP -1
            nxOf_Line -= nWidth
            IF nxOf_Line < 1
               ny --
               IF ny < 1
                  EXIT
               ENDIF
               nLen := cp_Len( oEdit:lUtf8, oEdit:aText[ny] )
               nxOf_Line := nLen - (nLen % nWidth)
            ENDIF
         NEXT
      ELSE
         FOR i := oEdit:y1 TO y - 1
            nxOf_Line += nWidth
            IF nxOf_Line > nLen
               ny ++
               nxOf_Line := 1
               IF ny > Len( oEdit:aText )
                  EXIT
               ENDIF
               nLen := cp_Len( oEdit:lUtf8, oEdit:aText[ny] )
            ENDIF
         NEXT
      ENDIF
      y := ny
      IF x != Nil
         x := nxOf_Line + x - 1
      ENDIF
      xOf_Line := nxOf_Line
   ENDIF

   RETURN ny

FUNCTION TrnMsg( cMsg )

   LOCAL cp, cpmsg

   IF !Empty( cpmsg := TEdit():cLangCP ) .AND. !( cpmsg == ( cp := hb_cdpSelect() ) )
      RETURN hb_Translate( cMsg, cpmsg, cp )
   ENDIF

   RETURN cMsg

FUNCTION cp_Chr( lUtf8, n )

   RETURN Iif( lUtf8, hb_utf8Chr( n ), Chr( n ) )

FUNCTION cp_Asc( lUtf8, s )

   RETURN Iif( lUtf8, hb_utf8Asc( s ), Asc( s ) )

FUNCTION cp_Substr( lUtf8, cString, nPos, nLen )
   RETURN Iif( lUtf8, ;
      Iif( nLen==Nil, hb_utf8Substr( cString, nPos ), hb_utf8Substr( cString, nPos, nLen ) ), ;
      Iif( nLen==Nil, Substr( cString, nPos ), Substr( cString, nPos, nLen ) ) )

FUNCTION cp_Peek( lUtf8, cString, nPos )
   RETURN Iif( lUtf8, hb_utf8Peek( cString, nPos ), hb_bPeek( cString, nPos ) )

FUNCTION cp_Left( lUtf8, cString, nLen )

   RETURN Iif( lUtf8, hb_utf8Left( cString, nLen ), Left( cString, nLen ) )

FUNCTION cp_Len( lUtf8, cString )

   RETURN Iif( lUtf8, hb_utf8Len( cString ), Len( cString ) )

FUNCTION cp_At( lUtf8, cFind, cLine, nStart, nEnd )
   IF lUtf8; RETURN hb_utf8At( cFind, cLine, nStart, nEnd ); ENDIF
   RETURN hb_At( cFind, cLine, nStart, nEnd )

FUNCTION cp_RAt( lUtf8, cFind, cLine, nStart, nEnd )
   IF lUtf8; RETURN hb_utf8RAt( cFind, cLine, nStart, nEnd ); ENDIF
   RETURN hb_RAt( cFind, cLine, nStart, nEnd )

FUNCTION cp_NextPos( lUtf8, cLine, nPos )
   IF lUtf8; RETURN nPos + Len( cp_Substr( lUtf8, cLine, nPos, 1 ) ); ENDIF
   RETURN nPos + 1

FUNCTION cp_Lower( lUtf8, cString )
   IF lUtf8; RETURN cedi_utf8_Lower( cString ); ENDIF
   RETURN Lower( cString )

FUNCTION cp_Upper( lUtf8, cString )
   IF lUtf8; RETURN cedi_utf8_Upper( cString ); ENDIF
   RETURN Upper( cString )

//STATIC FUNCTION _FIdle()
STATIC FUNCTION FCheckAutoc()

   //LOCAL nDelay,
   LOCAL nKey, oEdit, nSec

   IF nLastSec <= 0 .OR. ( nSec := Seconds() ) < (nLastSec + nAutoDelay) .OR. nSec > (nLastSec + nAutoDelay*2)
      RETURN Nil
   ENDIF

   //IF ( nDelay := hb_hGetDef( TEdit():options,"autodelay", 0 ) ) > 0 ;
      //.AND. Seconds() > (nLastSec + nDelay) .AND. Seconds() < (nLastSec + nDelay*2)
   IF Empty( TEdit():aWindows ) .OR. TEdit():nCurr == 0 .OR. TEdit():nCurr > Len(TEdit():aWindows)
      RETURN Nil
   ENDIF

   oEdit := TEdit():aWindows[TEdit():nCurr]
   IF ( (nKey := hb_keyStd(nLastKey)) >= K_SPACE .AND. nKey <= 255 ) .OR. ( oEdit:lUtf8 .AND. nKey > 3000 )
      IF oEdit:nPos > 1 .AND. oEdit:nPos == cp_Len( oEdit:lUtf8,oEdit:aText[oEdit:nLine] ) + 1 ;
         .AND. cp_Substr( oEdit:lUtf8, oEdit:aText[oEdit:nLine], oEdit:nPos-1, 1 ) >= ' '
         edi_DoAuC( oEdit, .F. )
         nLastSec := 0
      ENDIF
   ENDIF

   RETURN Nil