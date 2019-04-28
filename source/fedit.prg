
/*
 * Text editor
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"
#ifdef _FULL
#include "hbfuncsfull.ch"
#else
#include "hbfuncs.ch"
#endif

#define SHIFT_PRESSED 0x010000
#define CTRL_PRESSED  0x020000
#define ALT_PRESSED   0x040000
#define MAX_CBOARDS         10

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

#define UNDO_INC       12

STATIC aMenuMain := { {"Exit",@mnu_Exit(),Nil,"Esc,F10"}, {"Save",@mnu_Save(),Nil,"F2"}, ;
   {"Save as",@mnu_Save(),.T.,"Shift-F2"}, ;
   {"Mark block",@mnu_F3(),Nil,"F3"}, {"Open file",@mnu_F4(),{7,16},"F4 >"}, ;
   {"Search&GoTo",@mnu_Sea_Goto(),{8,16},">"}, {"Change mode",@mnu_ChgMode(),Nil,"Ctrl-Q"}, ;
   {"Codepage",@mnu_CPages(),{11,16},">"}, {"Syntax",@mnu_Syntax(),{12,16},"F8 >"}, ;
   {"Plugins",@mnu_Plugins(),Nil,"F11 >"}, {"Windows",@mnu_Windows(),{13,16},"F12 >"} }

STATIC aKeysMove := { K_UP, K_DOWN, K_LEFT, K_RIGHT, K_PGDN, K_PGUP, K_HOME, K_END, K_CTRL_PGUP, K_CTRL_PGDN }
STATIC cKeysMove := "hjklwWeEbBG0$^"

STATIC aLangExten := {}
STATIC cLangMapCP, aLangMapUpper, aLangMapLower
STATIC aMenu_CB
STATIC aLangs
STATIC lCase_Sea := .F.
STATIC cDopMode := ""

CLASS TEdit

   CLASS VAR aCPages    SHARED  INIT {}
   CLASS VAR aWindows   SHARED                   // An array with all TEdit objects
   CLASS VAR nCurr      SHARED                   // A currently processed TEdit object number
   CLASS VAR cpInit     SHARED
   CLASS VAR cLauncher  SHARED  INIT ""
   CLASS VAR lReadIni   SHARED  INIT .F.         // If ini file have been read already
   CLASS VAR options    SHARED  INIT { => }
   CLASS VAR aCmdHis    SHARED  INIT {}
   CLASS VAR aSeaHis    SHARED  INIT {}
   CLASS VAR aReplHis   SHARED  INIT {}
   CLASS VAR aEditHis   SHARED  INIT {}
   CLASS VAR aCBoards   SHARED                   // An array for clipboard buffers
   CLASS VAR aHiliAttrs SHARED  INIT { "W+/B", "W+/B", "W+/B", "W+/B", "GR+/B", "W/B", "W/B", "W/B", "W/B" }
   CLASS VAR aPlugins   SHARED  INIT {}
   CLASS VAR nDefMode   SHARED  INIT 0           // A start mode ( 0 - Edit, 1- Vim )
   CLASS VAR cColor     SHARED  INIT "BG+/B"
   CLASS VAR cColorSel  SHARED  INIT "N/W"
   CLASS VAR cColorPane SHARED  INIT "N/BG"
   CLASS VAR cColorBra  SHARED  INIT "R+/B"
   CLASS VAR aRectFull  SHARED
   CLASS VAR bNew       SHARED

   DATA   aRect       INIT { 0,0,24,79 }
   DATA   y1, x1, y2, x2
   DATA   cFileName   INIT ""
   DATA   cp
   DATA   nxFirst, nyFirst
   DATA   aText
   DATA   nMode                       // Current mode (Edit, Vim, Cmd)
   DATA   nDopMode    INIT 0          // A state in a Vim mode after pressing some keys
                                      // (m, ', 0..9, ...) when other keys are expected
   DATA   cSyntaxType

   DATA   oParent                     // A TEdit object of a parent edit window
   DATA   aUndo       INIT {}
   DATA   nUndo       INIT 0

   DATA   lTopPane    INIT .T.
   DATA   nTopName    INIT 36

   DATA   lCtrlTab    INIT .T.
   DATA   lReadOnly   INIT .F.
   DATA   lUtf8       INIT .F.
   DATA   lUpdated    INIT .F.
   DATA   lIns        INIT .T.
   DATA   lShiftKey   INIT .F.

   DATA   lTabs       INIT .F.
   DATA   nTabLen     INIT 4

   DATA   nCol, nRow
   DATA   nPosBack, nLineBack
   DATA   lF3         INIT .F.
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
   DATA   bOnKey
   DATA   oHili
   DATA   hBookMarks
   DATA   npy1, npx1, npy2, npx2
   DATA   cargo

   METHOD New( cText, cFileName, y1, x1, y2, x2, cColor, lTopPane )
   METHOD SetText( cText, cFileName )
   METHOD Edit()
   METHOD TextOut( n1, n2 )
   METHOD LineOut( nLine )
   METHOD onKey( nKeyExt )
   METHOD WriteTopPane( lClear )
   METHOD RowToLine( nRow )   INLINE ( Iif(nRow==Nil,::nRow,nRow) - ::y1 + ::nyFirst )
   METHOD ColToPos( nCol )    INLINE ( Iif(nCol==Nil,::nCol,nCol) - ::x1 + ::nxFirst )
   METHOD Search( cSea, lCase, lNext, ny, nx )
   METHOD GoTo( ny, nx, nSele, lNoGo )
   METHOD ToString( cEol )
   METHOD Save( cFileName )
   METHOD InsText( nLine, nPos, cText, lOver, lChgPos, lNoUndo )
   METHOD DelText( nLine1, nPos1, nLine2, nPos2, lNoUndo )
   METHOD Undo( nLine1, nPos1, nLine2, nPos2, nOper, cText )
   METHOD Highlighter( oHili )
   METHOD OnExit()

ENDCLASS

METHOD New( cText, cFileName, y1, x1, y2, x2, cColor, lTopPane ) CLASS TEdit

   LOCAL i, cExt

   IF !::lReadIni
      edi_ReadIni( hb_DirBase() + "hbedit.ini" )
   ENDIF
   IF Empty( ::aRectFull )
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
   ::nxFirst := ::nyFirst := 1
   ::npy1 := ::npx1 := ::npy2 := ::npx2 := 0
   IF Valtype( lTopPane ) == "L" .AND. !lTopPane
      ::lTopPane := .F.
   ENDIF

   IF ::lTopPane
      ::nTopName := Max( ::x2 - ::x1 - 44, 0 )
      ::y1 ++
   ENDIF
   ::nRow := ::y1; ::nCol := ::x1

   ::nMode := ::nDefMode
   ::cp := ::cpInit

   ::SetText( cText, cFileName )

   ::hBookMarks := hb_Hash()

   IF ::aWindows == Nil
      ::aWindows := {}
   ENDIF
   Aadd( ::aWindows, Self )

   IF !Empty( ::bNew )
      Eval( ::bNew, Self )
   ENDIF

   RETURN Self

METHOD SetText( cText, cFileName ) CLASS TEdit

   LOCAL i, arr, xPlugin, cFile_utf8, cExt, cBom := e"\xef\xbb\xbf"
   LOCAL nEol := hb_hGetDef( TEdit():options,"eol", 0 )

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
         ::cFileName := cFileName
         cFile_utf8 := hb_Translate( cFileName,, "UTF8" )
         IF ( i := Ascan( TEdit():aEditHis, {|a|a[1]==cFile_utf8} ) ) > 0
            arr := TEdit():aEditHis[i]
            ADel( TEdit():aEditHis, i )
            hb_AIns( TEdit():aEditHis, 1, arr, .F. )
            hb_cdpSelect( ::cp := arr[2] )
            IF arr[3] >= ::y2 -::y1 - 1
               ::nyFirst := arr[3] - 4
               ::nRow := 4
            ELSE
               ::nRow := arr[3] + ::y1 - 1
            ENDIF
            IF arr[4] > ::x2 -::x1 - 1
               ::nxFirst := arr[4] + ::x1 - ::x2 + 3
               ::nCol := arr[4] - ::nxFirst + 1
            ELSE
               ::nCol := arr[4] + ::x1 - 1
            ENDIF
         ELSE
            hb_AIns( TEdit():aEditHis, 1, {cFile_utf8,,,}, Len(TEdit():aEditHis)<hb_hGetDef(TEdit():options,"edithismax",10) )
         ENDIF
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
         IF Chr(9) $ ::aText[i]
            ::aText[i] := Strtran( ::aText[i], Chr(9), Space(::nTablen) )
            ::lTabs := .T.
         ENDIF
         IF Right( ::aText[i],1 ) == Chr(13)
            ::aText[i] := Left( ::aText[i], Len( ::aText[i])-1 )
         ENDIF
      NEXT
   ENDIF

   ::aUndo := {}
   ::nUndo := 0

   IF hb_hGetDef( TEdit():options, "syntax", .F. ) .AND. !Empty( cFileName )
      cExt := Lower( hb_fnameExt(cFileName) )
      FOR i := 1 TO Len(aLangExten)
         IF cExt $ aLangExten[i,2] .AND. hb_hHaskey(aLangs,aLangExten[i,1])
            mnu_SyntaxOn( Self, aLangExten[i,1] )
            IF !Empty( xPlugin := hb_hGetDef( ::oHili:hHili, "plugin", Nil ) )
               IF Valtype( xPlugin ) == "C"
                  IF File( edi_CurrPath() + "plugins" + hb_ps() + xPlugin )
                     xPlugin := ::oHili:hHili["plugin"] := hb_hrbLoad( edi_CurrPath() + "plugins" + hb_ps() + xPlugin )
                  ELSEIF File( hb_DirBase() + "plugins" + hb_ps() + xPlugin )
                     xPlugin := ::oHili:hHili["plugin"] := hb_hrbLoad( hb_DirBase() + "plugins" + hb_ps() + xPlugin )
                  ENDIF
                  IF Empty( xPlugin )
                     EXIT
                  ENDIF
               ENDIF
               hb_hrbDo( xPlugin, Self )
            ENDIF
            EXIT
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

METHOD Edit() CLASS TEdit

   LOCAL i, nKeyExt, cFile_utf8

   hb_cdpSelect( ::cp )
   ::nCurr := Ascan( ::aWindows, {|o|o==Self} )

   SetCursor( SC_NONE )
   SetColor( ::cColor )
   Scroll( ::y1, ::x1, ::y2, ::x2 )

   IF ( Len( ::aText ) < ::nyFirst + ::nRow - 1 ) .OR. ::nyFirst < 1 .OR. ::nRow < ::y1
      ::nyFirst := 1
      ::nRow := ::y1; ::nCol := ::x1
   ENDIF

   ::WriteTopPane()

   IF !Empty( ::bStartEdit )
      Eval( ::bStartEdit, Self )
   ENDIF

   FOR i := Len( ::aWindows ) TO 1 STEP -1
      // Draw the child window, if found.
      IF !Empty( ::aWindows[i]:oParent ) .AND. ::aWindows[i]:oParent == Self
         ::aWindows[i]:WriteTopPane( .T. )
         ::aWindows[i]:TextOut()
      ENDIF
   NEXT

   ::TextOut()
   DevPos( ::nRow, ::nCol )

   ::nPosBack := ::ColToPos()
   ::nLineBack := ::RowToLine()
   ::lShow := .T.
   DO WHILE ::lShow
      SetCursor( Iif( ::lIns, SC_NORMAL, SC_SPECIAL1 ) )
      nKeyExt := Inkey( 0, HB_INKEY_ALL + HB_INKEY_EXT )
      SetCursor( SC_NONE )
      ::onKey( nKeyExt )
   ENDDO

   hb_cdpSelect( ::cpInit )
   IF !Empty( ::cFileName )
      cFile_utf8 := hb_Translate( ::cFileName,, "UTF8" )
      IF ( i := Ascan( TEdit():aEditHis, {|a|a[1]==cFile_utf8} ) ) > 0
         TEdit():aEditHis[i,2] := ::cp
         TEdit():aEditHis[i,3] := ::RowToLine()
         TEdit():aEditHis[i,4] := ::ColToPos()
      ENDIF
   ENDIF

   ::WriteTopPane( .T. )
   IF !Empty( ::bEndEdit )
      Eval( ::bEndEdit, Self )
   ENDIF
   IF ::lClose
      i := Ascan( ::aWindows, {|o|o==Self} )
      hb_ADel( ::aWindows, i, .T. )
      IF !Empty( ::oParent )
         // Restore the size of a parent window, if exists
         IF ::y1 > ::oParent:y2
            ::oParent:y2 := ::y2
         ENDIF
      ELSE
         FOR i := Len( ::aWindows ) TO 1 STEP -1
            IF !Empty( ::aWindows[i]:oParent ) .AND. ::aWindows[i]:oParent == Self
               // Close a child window, if found
               ::aWindows[i]:oParent := Nil
               hb_ADel( ::aWindows, i, .T. )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN Nil

METHOD TextOut( n1, n2 ) CLASS TEdit

   LOCAL i, nKol := ::y2 -::y1 + 1, x := Col(), y := Row()

   IF n1 == Nil; n1 := 1; ENDIF
   IF n2 == Nil; n2 := nKol; ENDIF

   FOR i := n1 TO n2
      ::LineOut( i )
   NEXT
   DevPos( y, x )

   RETURN Nil

METHOD LineOut( nLine, lInTextOut ) CLASS TEdit

   LOCAL n := nLine + ::nyFirst - 1, nWidth := ::x2 - ::x1 + 1, nLen
   LOCAL lSel := .F., nby1, nby2, nbx1, nbx2, xs1, xs2
   LOCAL aStru, i, aClrs

   IF n <= Len( ::aText )

      DevPos( ::y1 + nLine - 1, ::x1 )
      nLen := Max( 0, Min( nWidth, cp_Len( ::lUtf8,::aText[n])-::nxFirst+1 ) )

      //IF Empty( lInTextOut )
         DispBegin()
      //ENDIF
      IF nLen > 0
         DevOut( cp_Substr( ::lUtf8, ::aText[n], ::nxFirst, nLen ) )
      ENDIF

      IF !Empty( ::oHili ) .AND. hb_hGetDef( TEdit():options, "syntax", .F. )
         ::oHili:Do( n )
         aStru := ::oHili:aLineStru
         IF ::oHili:nItems > 0
            aClrs := ::oHili:hHili["colors"]
            FOR i := 1 TO ::oHili:nItems
               IF aStru[i,2] >= ::nxFirst .AND. aStru[i,3] > 0 .AND. aStru[i,1] < ::nxFirst + nWidth
                  nbx1 := Max( ::nxFirst, aStru[i,1] )
                  nbx2 := Min( aStru[i,2], ::nxFirst + nWidth - 1 )
                  DevPos( ::y1 + nLine - 1, nbx1 -::nxFirst )
                  SetColor( Iif( !Empty(aClrs[aStru[i,3]]), aClrs[aStru[i,3]], ::aHiliAttrs[aStru[i,3]] ) )
                  DevOut( cp_Substr( ::lUtf8, ::aText[n], nbx1, nbx2-nbx1+1 ) )
               ENDIF
            NEXT
         ENDIF
      ENDIF
      IF ::nby1 >= 0 .AND. ::nby2 >= 0
         IF ::nby1 < ::nby2 .OR. ( ::nby1 == ::nby2 .AND. ::nbx1 < ::nbx2 )
            nby1 := ::nby1; nbx1 := ::nbx1; nby2 := ::nby2; nbx2 := ::nbx2
         ELSE
            nby1 := ::nby2; nbx1 := ::nbx2; nby2 := ::nby1; nbx2 := ::nbx1
         ENDIF
         lSel := ( n >= nby1 .AND. n <= nby2 ) .AND. !( nby1 == nby2 .AND. nbx1 == nbx2 )
      ENDIF
      SetColor( ::cColor )
      IF lSel
         nbx1 := Iif( n > nby1, 1, nbx1 )
         nbx2 := Iif( n < nby2, cp_Len(::lUtf8,::aText[n])+1, nbx2 )
         IF nbx1 < (::nxFirst+nWidth) .AND. nbx2 > ::nxFirst
            nbx1 := Max( nbx1, ::nxFirst )
            nbx2 := Min( nbx2, ::nxFirst + nWidth - 1 )
            DevPos( ::y1 + nLine - 1, nbx1 -::nxFirst + ::x1 )
            SetColor( ::cColorSel )
            DevOut( cp_Substr( ::lUtf8, ::aText[n], nbx1, nbx2-nbx1 ) )
         ENDIF
         SetColor( Iif( n < nby2, ::cColorSel, ::cColor ) )
      ENDIF
      IF nLen < nWidth
         Scroll( ::y1 + nLine - 1, ::x1 + nLen, ::y1 + nLine - 1, ::x2 )
      ENDIF
      SetColor( ::cColor )
      //IF Empty( lInTextOut )
         DispEnd()
      //ENDIF
   ELSE
      Scroll( ::y1 + nLine - 1, ::x1, ::y1 + nLine - 1, ::x2 )
   ENDIF
   RETURN Nil

METHOD onKey( nKeyExt ) CLASS TEdit

   LOCAL nKey := hb_keyStd(nKeyExt), i, n, nCol := Col(), nRow := Row()
   LOCAL s, lShift, lCtrl := .F., lNoDeselect := .F., lSkip := .F., x

   ::nCol := nCol; ::nRow := nRow
   n := ::RowToLine( nRow )
   ::lTextOut := .F.

   IF ::npy1 > 0
      // drop highlighting of matched parenthesis
      DevPos( ::npy1 - ::nyFirst + ::y1, ::npx1 - ::nxFirst + ::x1 )
      DevOut( cp_Substr( ::lUtf8, ::aText[::npy1], ::npx1, 1 ) )
      IF ::npy2 >= ::nyFirst .AND. ::npy2 < ::nyFirst + ::y2 - ::y1 .AND. ;
            ::npx2 >= ::nxFirst .AND. ::npx2 < ::nxFirst + ::x2 - ::x1
         DevPos( ::npy2 - ::nyFirst + ::y1, ::npx2 - ::nxFirst + ::x1 )
         DevOut( cp_Substr( ::lUtf8, ::aText[::npy2], ::npx2, 1 ) )
      ENDIF
      DevPos( ::nRow, ::nCol )
      ::npy1 := ::npx1 := ::npy2 := ::npx2 := 0
   ENDIF

   IF !Empty( ::bOnKey )
      i := Eval( ::bOnKey, Self, nKeyExt )
      IF i == - 1
         RETURN Nil
      ELSEIF i > 0
         nKeyExt := i
         nKey := hb_keyStd(nKeyExt)
      ENDIF
   ENDIF

   IF ::nDopMode > 0
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
         CASE 49  // 1
            cDopMode += Chr( nKey )
            IF nKey == 103    // g
               ::nDopMode := 103
            ELSEIF Chr(nKey) $ cKeysMove
               edi_Move( Self, nKey, Val( cDopMode ) )
               nKey := K_RIGHT
               ::nDopMode := 0
            ELSEIF !( nKey >= 48 .AND. nKey <= 57 )
               ::nDopMode := 0
            ENDIF
            EXIT
         CASE 99   // c
         CASE 100  // d
            IF nKey == 100    // d
               IF ::nDopMode == 100 .AND. n > 0 .AND. n <= Len( ::aText )
                  ::DelText( n, 0, n+1, 0 )
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 99    // c
               IF ::nDopMode == 99 .AND. n > 0 .AND. n <= Len( ::aText )
                  ::DelText( n, 1, n, cp_Len(::lUtf8,::aText[n]) )
                  mnu_ChgMode( Self, .T. )
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 105    // i
               IF cDopMode == "d"
                  cDopMode += Chr( nKey )
               ENDIF

            ELSEIF nKey == 98     // b
               IF cDopMode $ "cd"
                  mnu_F3( Self )
                  edi_PrevWord( Self, .F. )
                  ::nby2 := ::RowToLine( Row() )
                  ::nbx2 := ::ColToPos( Col() )
                  cbDele( Self )
                  IF ::nDopMode == 99
                     mnu_ChgMode( Self, .T. )
                  ENDIF
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 101    // e
               IF cDopMode $ "cd"
                  mnu_F3( Self )
                  edi_NextWord( Self, .F., .T. )
                  edi_GoRight( Self )
                  ::nby2 := ::RowToLine( Row() )
                  ::nbx2 := ::ColToPos( Col() )
                  cbDele( Self )
                  IF ::nDopMode == 99
                     mnu_ChgMode( Self, .T. )
                  ENDIF
               ENDIF
               ::nDopMode := 0

            ELSEIF nKey == 119    // w
               IF cDopMode $ "cd"
                  mnu_F3( Self )
                  edi_NextWord( Self, .F. )
                  ::nby2 := ::RowToLine( Row() )
                  ::nbx2 := ::ColToPos( Col() )
                  cbDele( Self )
                  IF ::nDopMode == 99
                     mnu_ChgMode( Self, .T. )
                  ENDIF
               ELSEIF cDopMode == "di"
                  edi_PrevWord( Self, .F. )
                  mnu_F3( Self )
                  edi_NextWord( Self, .F., .T. )
                  edi_GoRight( Self )
                  ::nby2 := ::RowToLine( Row() )
                  ::nbx2 := ::ColToPos( Col() )
                  cbDele( Self )
               ENDIF
               ::nDopMode := 0

            ELSE
               ::nDopMode := 0
            ENDIF
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
         END
      ENDIF
      IF ::nDopMode == 0
         cDopMode := ""
      ENDIF
      lSkip := .T.
   ENDIF

   IF !lSkip
      lShift := ( hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0 .AND. Ascan( aKeysMove, nkey ) != 0 )
      IF lShift
         IF !::lShiftKey
            ::nby1 := ::RowToLine()
            ::nbx1 := ::ColToPos()
            ::lShiftKey := .T.
         ENDIF
      ELSE
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
         CASE K_ALT_BS
            ::Undo()
            EXIT
         END
      ENDIF
      IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0

         lCtrl := .T.
         SWITCH nKey
         CASE K_CTRL_INS
         CASE 3                           // Ctrl-Ins or Ctrl-c
            IF !Empty( s := Text2cb( Self ) )
               hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
               TEdit():aCBoards[1,2] := Nil
            ENDIF
            lNoDeselect := .T.
            EXIT
         CASE 22                          // Ctrl-v
            IF !::lReadOnly
               cb2Text( Self, .T. )
            ENDIF
            EXIT
         CASE K_CTRL_X
            IF !Empty( s := Text2cb( Self ) )
               hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
               TEdit():aCBoards[1,2] := Nil
            ENDIF
            cbDele( Self )
            EXIT
         CASE K_CTRL_Q
            IF hb_keyVal( nKeyExt ) == 81
               mnu_ChgMode( Self )
            ENDIF
            EXIT
         CASE K_CTRL_Y
            IF !::lReadOnly .AND. n > 0 .AND. n <= Len( ::aText )
               ::DelText( n, 1, n+1, 1 )
            ENDIF
            EXIT
         CASE K_CTRL_A
            IF !::lF3
               ::nby1 := ::nbx1 := 1
               ::nby2 := Len( ::aText )
               ::nbx2 := cp_Len( ::lUtf8, ::aText[Len(::aText)] )
               ::lF3 := .T.
            ENDIF
            EXIT
         CASE K_CTRL_TAB
            IF ::lCtrlTab
               ::lShow := .F.
               ::nCurr ++
            ENDIF
            lNoDeselect := .T.
            EXIT
         CASE K_CTRL_PGUP
         CASE K_CTRL_HOME
            ::nPosBack := ::ColToPos()
            ::nLineBack := ::RowToLine()
            ::lTextOut := (::nyFirst>1 .OR. ::nxFirst>1)
            ::nxFirst := ::nyFirst := 1
            DevPos( ::y1, ::x1 )
            EXIT
         CASE K_CTRL_PGDN
         CASE K_CTRL_END
            edi_Move( Self, 71 )
            EXIT
         CASE K_CTRL_RIGHT
            IF hb_keyVal( nKeyExt ) == 66 // Ctrl-B
               edi_Bracket( Self )
            ELSEIF hb_keyVal( nKeyExt ) == 16
               edi_NextWord( Self, .F. )
            ENDIF
            EXIT
         CASE K_CTRL_LEFT
            IF hb_keyVal( nKeyExt ) == 90// Ctrl-Z
               ::Undo()
            ELSEIF hb_keyVal( nKeyExt ) == 15
               edi_PrevWord( Self, .F. )
            ENDIF
            EXIT
         CASE K_CTRL_F4
            mnu_OpenFile( Self )
            ::lTextOut := .T.
            EXIT
         CASE K_CTRL_F7
            mnu_SeaAndRepl( Self )
            ::lTextOut := .T.
            EXIT
        END
      ELSE
         IF ( nKey >= K_SPACE .AND. nKey <= 255 ) .OR. ( ::lUtf8 .AND. nKey > 3000 )
            IF ::nby1 >= 0 .AND. ::nby2 >= 0
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
                  CASE 100   // d Deletes selection
                     cbDele( Self )
                     EXIT
                  CASE 121   // y Copy to clipboard
                     IF !Empty( s := Text2cb( Self ) )
                        hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
                        TEdit():aCBoards[1,2] := Nil
                     ENDIF
                     EXIT
                  CASE 62    // > Shift lines right
                     edi_Indent( Self, .T. )
                     lNoDeselect := .T.
                     EXIT
                  CASE 60    // > Shift lines left
                     edi_Indent( Self, .F. )
                     lNoDeselect := .T.
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
                  CASE 117   // u Undo
                     ::Undo()
                     EXIT
                  CASE 112   // p Insert clipboard after current coloumn
                     IF !::lReadOnly
                        DevPos( ::nRow, ++::nCol )
                        cb2Text( Self, .T. )
                     ENDIF
                     EXIT
                  CASE 80    // P Insert clipboard
                     IF !::lReadOnly
                        cb2Text( Self, .T. )
                     ENDIF
                     EXIT
                  CASE 105   // i - to edit mode
                     mnu_ChgMode( Self, .T. )
                     EXIT
                  CASE 73    // I - to edit mode
                     edi_Move( Self, 94 )
                     mnu_ChgMode( Self, .T. )
                     EXIT
                  CASE 97    // a - to edit mode
                     edi_GoRight( Self )
                     mnu_ChgMode( Self, .T. )
                     EXIT
                  CASE 65    // A - to edit mode
                     edi_GoEnd( Self ); edi_GoRight( Self )
                     mnu_ChgMode( Self, .T. )
                     EXIT
                  CASE 111   // o Insert line after current
                     ::InsText( n, cp_Len(::lUtf8,::aText[n])+1, Chr(10), .F., .T. )
                     mnu_ChgMode( Self, .T. )
                     EXIT
                  CASE 109
                  CASE 39  // m - set bookmark, ' - goto bookmark
                     ::nDopMode := nKey
                     cDopMode := Chr( nKey )
                     EXIT
                  CASE 99    // c - delete and edit
                     ::nDopMode := 99
                     cDopMode := Chr( nKey )
                     EXIT
                  CASE 100   // d - delete
                     ::nDopMode := 100
                     cDopMode := Chr( nKey )
                     EXIT
                  CASE 103   // g
                     ::nDopMode := 103
                     cDopMode := Chr( nKey )
                     EXIT
                  CASE 37   // %  Go to matching parentheses
                     edi_Bracket( Self )
                     EXIT
                  CASE 72   // H
                     DevPos( ::nRow := ::y1, ::nCol := ::x1 )
                     edi_Move( Self, 94 )
                     EXIT
                  CASE 77   // M
                     DevPos( ::nRow := ::y1 + Int((::y2-::y1)/2), ::nCol := ::x1 )
                     edi_Move( Self, 94 )
                     EXIT
                  CASE 76   // L
                     DevPos( ::nRow := ::y2, ::nCol := ::x1 )
                     edi_Move( Self, 94 )
                     EXIT
                  END
               ENDIF
            ELSE
               IF ( i := (::nCol - ::x1 + ::nxFirst - cp_Len(::lUtf8,::aText[n])) ) > 0
                  ::nCol -= (i-1)
                  ::InsText( n, cp_Len(::lUtf8,::aText[n])+1, Space( i-1 ) + cp_Chr(::lUtf8,nKey), !::lIns, .T. )
               ELSE
                  ::InsText( n, ::nCol-::x1+::nxFirst, cp_Chr(::lUtf8,nKey), !::lIns, .T. )
               ENDIF
            ENDIF

         ELSE
            SWITCH nKey
            CASE K_ENTER
               IF !::lReadOnly .AND. ::nMode == 0
                  nCol := ::ColToPos( nCol )
                  s := ""
                  IF hb_hGetDef( TEdit():options, "autoindent", .F. )
                     i := 0
                     DO WHILE cp_Substr( ::lUtf8, ::aText[n], i+1, 1 ) == " "; i++; ENDDO
                     IF i > 0
                        IF nCol <= i
                           i := nCol - 1
                        ENDIF
                        s := Space( i )
                     ENDIF
                  ENDIF
                  ::InsText( n, nCol, Chr(10) + s, .F., .T. )
               ENDIF
               EXIT
            CASE K_DEL
               IF !::lReadOnly
                  IF ::nby1 >= 0 .AND. ::nby2 >= 0
                     IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0 .AND. !Empty( s := Text2cb( Self ) )
                        hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
                        TEdit():aCBoards[1,2] := Nil
                     ENDIF
                     cbDele( Self )
                  ELSE
                     ::DelText( n, ::nCol-::x1+::nxFirst, n, ::nCol-::x1+::nxFirst )
                  ENDIF
               ENDIF
               EXIT
            CASE K_BS
               IF !::lReadOnly .AND. ::nMode == 0
                  IF ::nby1 >= 0 .AND. ::nby2 >= 0
                     IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0 .AND. !Empty( s := Text2cb( Self ) )
                        hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
                        TEdit():aCBoards[1,2] := Nil
                     ENDIF
                     cbDele( Self )
                  ELSE
                     IF ::nCol == ::x1
                        IF n > 1
                           edi_GoUp( Self )
                           edi_GoEnd( Self )
                           ::DelText( n-1, Col()-::x1+::nxFirst, n-1, Col()-::x1+::nxFirst )
                        ENDIF
                     ELSE
                        ::DelText( n, ::nCol-::x1+::nxFirst-1, n, ::nCol-::x1+::nxFirst-1 )
                     ENDIF
                  ENDIF
               ENDIF
               EXIT
            CASE K_TAB
               IF !::lReadOnly
               ENDIF
               EXIT
            CASE K_INS
               IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0
                  IF !::lReadOnly
                     cb2Text( Self, .T. )
                  ENDIF
               ELSE
                  ::lIns := !::lIns
               ENDIF
               EXIT
            CASE K_UP
               edi_GoUp( Self )
               EXIT
            CASE K_DOWN
               edi_GoDown( Self )
               EXIT
            CASE K_LEFT
               edi_GoLeft( Self )
               EXIT
            CASE K_RIGHT
               edi_GoRight( Self )
               EXIT
            CASE K_HOME
               edi_Move( Self, 48 )
               EXIT
            CASE K_END
               edi_GoEnd( Self )
               EXIT
            CASE K_PGUP
               IF ::nyFirst > (::y2-::y1)
                  ::nyFirst -= (::y2-::y1)
                  ::lTextOut := .T.
               ELSEIF ::nyFirst > 1
                  ::nyFirst := 1
                  ::lTextOut := .T.
               ELSE
                  Devpos( ::nRow := 1, ::nCol )
               ENDIF
               EXIT
            CASE K_PGDN
               IF ::nyFirst + (::y2-::y1) <= Len( ::aText )
                  ::nyFirst += (::y2-::y1)
                  IF ::nyFirst + ::nRow - ::y1 >= Len( ::aText )
                     DevPos( ::nRow := (Len(::aText)-::nyFirst+2-::y1), ::nCol := ::x1 )
                  ENDIF
                  ::lTextOut := .T.
               ELSE
                  DevPos( ::nRow := (Len(::aText)-::nyFirst+2-::y1), ::nCol )
               ENDIF
               EXIT
            CASE K_LBUTTONDOWN
               IF ::nDopMode == 0
                  nCol := MCol()
                  nRow := MRow()
                  IF ::lTopPane .AND. nRow == ::y1-1 .AND. nCol < 8
                     FMenu( Self, aMenuMain, 2, 6 )
                     ::lTextOut := .T.
                  ELSEIF nRow >= ::y1 .AND. nRow <= ::y2 .AND. nCol >= ::x1 .AND. nCol <= ::x2
                     IF ::RowToLine(nRow) > Len(::aText)
                        nRow := Len(::aText) - ::nyFirst + ::y1
                     ENDIF
                     DevPos( nRow, nCol )
                  ELSEIF nRow >= ::aRectFull[1] .AND. nRow <= ::aRectFull[3] .AND. nCol >= ::aRectFull[2] .AND. nCol <= ::aRectFull[4]
                     IF !Empty( x := ::oParent ) .AND. nRow >= x:y1 .AND. nRow <= x:y2 .AND. nCol >= x:x1 .AND. nCol <= x:x2
                        ::lShow := .F.
                        ::nCurr := Ascan( ::aWindows, {|o|o == ::oParent} )
                        x:nRow := nRow; x:nCol := nCol
                     ELSE
                        FOR i := 1 TO Len( ::aWindows )
                           IF !Empty( ::aWindows[i]:oParent ) .AND. (x := ::aWindows[i]):oParent == Self .AND. ;
                              nRow >= x:y1 .AND. nRow <= x:y2 .AND. nCol >= x:x1 .AND. nCol <= x:x2
                              ::lShow := .F.
                              ::nCurr := i
                              x:nRow := nRow; x:nCol := nCol
                           ENDIF
                        NEXT
                     ENDIF
                  ELSE
                     RETURN Nil
                  ENDIF
               ENDIF
               EXIT
            CASE K_F1
               mnu_Help( Self )
               ::lTextOut := .T.
               DevPos( ::nRow, ::nCol )
               EXIT
            CASE K_F2
               ::Save()
               EXIT
            CASE K_SH_F2
               mnu_Save( Self, .T. )
               EXIT
            CASE K_F3
               mnu_F3( Self )
               lNoDeselect := .T.
               nKey := K_RIGHT
               EXIT
            CASE K_F4
               mnu_F4( Self, {2, 6} )
               ::lTextOut := .T.
               EXIT
            CASE K_SH_F4
               mnu_NewWin( Self )
               ::lTextOut := .T.
               EXIT
            CASE K_F7
               mnu_Search( Self )
               ::lTextOut := .T.
               EXIT
            CASE K_F8
               mnu_Syntax( Self, {2, 6} )
               ::lTextOut := .T.
               EXIT
            CASE K_F9
               FMenu( Self, aMenuMain, 2, 6 )
               ::lTextOut := .T.
               DevPos( ::nRow, ::nCol )
               EXIT
            CASE K_F10
            CASE K_ESC
               IF ::nMode == 1 .AND. nKey == K_ESC
                  mnu_ChgMode( Self, .T. )
               ELSE
                  mnu_Exit( Self )
               ENDIF
               EXIT
            CASE K_F11
               mnu_Plugins( Self )
               ::lTextOut := .T.
               EXIT
            CASE K_F12
               mnu_Windows( Self, {2, 6} )
               ::lTextOut := .T.
               EXIT
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
            ::nby2 := ::RowToLine( Row() )
            ::nbx2 := ::ColToPos( Col() )
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

   ::nCol := Col(); ::nRow := Row()

   IF !Empty( ::oHili ) .AND. !Empty( x := edi_Bracket( Self, .T., .T. ) )
      // highlighting matched parenthesis
      ::npy1 := ::RowToLine(); ::npx1 := ::ColToPos()
      ::npy2 := Iif( Valtype(x)=="A",x[1], ::npy1 ); ::npx2 := Iif( Valtype(x)=="A",x[2], x )
      SetColor( ::cColorBra )
      DevPos( ::npy1 - ::nyFirst + ::y1, ::npx1 - ::nxFirst + ::x1 )
      DevOut( cp_Substr( ::lUtf8, ::aText[::npy1], ::npx1, 1 ) )
      IF ::npy2 >= ::nyFirst .AND. ::npy2 < ::nyFirst + ::y2 - ::y1 .AND. ;
            ::npx2 >= ::nxFirst .AND. ::npx2 < ::nxFirst + ::x2 - ::x1
         DevPos( ::npy2 - ::nyFirst + ::y1, ::npx2 - ::nxFirst + ::x1 )
         DevOut( cp_Substr( ::lUtf8, ::aText[::npy2], ::npx2, 1 ) )
      ENDIF
      SetColor( ::cColor )
      DevPos( ::nRow, ::nCol )
   ENDIF

   ::WriteTopPane()

   RETURN Nil

METHOD WriteTopPane( lClear ) CLASS TEdit

   LOCAL y := ::y1 - 1, nCol := ::nCol, nRow := ::nRow
   LOCAL cLen := Ltrim(Str(Len(::aText))), nchars := Len(cLen)

   IF ::lTopPane
      DispBegin()
      SetColor( ::cColorPane )
      Scroll( y, ::x1, y, ::x2 )
      IF Empty( lClear )
         DevPos( y, ::x1 )
         DevOut( Iif( !Empty(cDopMode), cDopMode, "F9-menu " ) )
         DevOut( cp_Left( ::lUtf8, hb_fnameNameExt(::cFileName), ::nTopName ) )
         DevPos( y, ::x1 + 8 + ::nTopName + 2 )
         DevOut( Iif( ::lUpdated, "* ", "  " ) + Lower( ::cp ) )
         DevPos( y, ::x1 + 8 + ::nTopName + 12 )
         DevOut( PAdl(Ltrim(Str(::RowToLine(nRow))),nchars) + "/" + cLen )
         DevPos( y, ::x1 + 8 + ::nTopName + 12 + nchars*2 + 3 )
         DevOut( "[" + Ltrim(Str(nCol-::x1+::nxFirst)) + "]" )
         SetColor( "W+/N" )
         DevPos( y, ::x2-3 )
         IF ::lF3 .OR. (::nby1 >= 0 .AND. ::nby2 >= 0)
            DevOut( "Sele" )
         ELSE
            DevOut( Iif( ::nMode == 0, "Edit", Iif( ::nMode == 1, " Vim", " Cmd" ) ) )
         ENDIF
      ENDIF
      SetColor( ::cColor )
      DevPos( nRow, nCol )
      DispEnd()
   ENDIF

   RETURN Nil

METHOD Search( cSea, lCase, lNext, ny, nx ) CLASS TEdit

   LOCAL lRes := .F., i, nLen := Len( ::aText ), nPos, s

   IF !lCase
      cSea := cp_Lower( ::lUtf8, cSea )
   ENDIF
   IF lNext
      s := cp_Substr( ::lUtf8, ::aText[ny], nx, cp_Len(::lUtf8,cSea) )
      IF cSea == Iif( lCase, s, cp_Lower( ::lUtf8,s ) )
         nx ++
      ENDIF
      FOR i := ny TO nLen
         s := Iif( lCase, ::aText[i], cp_Lower( ::lUtf8, ::aText[i] ) )
         IF ( nPos := cp_At( ::lUtf8, cSea, s, Iif( i == ny, nx, 1 ) ) ) > 0
            lRes := .T.; ny := i; nx := nPos
            EXIT
         ENDIF
      NEXT
   ELSE
      s := cp_Substr( ::lUtf8, ::aText[ny], nx, cp_Len(::lUtf8,cSea) )
      IF cSea == Iif( lCase, s, cp_Lower( ::lUtf8,s ) )
         nx --
      ENDIF
      FOR i := ny TO 1 STEP -1
         s := Iif( lCase, ::aText[i], cp_Lower( ::lUtf8, ::aText[i] ) )
         IF ( nPos := cp_RAt( ::lUtf8, cSea, s, 1, Iif( i == ny, nx, cp_Len(::lUtf8,::aText[i]) ) ) ) > 0
            lRes := .T.; ny := i; nx := nPos
            EXIT
         ENDIF
      NEXT
   ENDIF

   RETURN lRes

METHOD GoTo( ny, nx, nSele, lNoGo ) CLASS TEdit

   LOCAL lTextOut := .F., nRowOld

   IF ny == Nil; ny := ::RowToLine(); ENDIF
   IF nx == Nil; nx := 1; ENDIF
   IF ny > Len(::aText)
      RETURN Nil
   ENDIF

   IF Empty( lNoGo )
      ::nPosBack := ::ColToPos()
      ::nLineBack := ::RowToLine()
   ENDIF
   IF ny < ::nyFirst .OR. ny > ::nyFirst + (::y2-::y1)
      ::nyFirst := Max( ny-3, 1 )
      lTextOut := .T.
   ENDIF
   IF nx < ::nxFirst .OR. nx > ::nxFirst + (::x2-::x1)
      ::nxFirst := Iif( nx < ::x2-::x1, 1, nx - Int((::x2-::x1)*0.8) )
      lTextOut := .T.
   ENDIF

   IF nSele != Nil .AND. nSele > 0
      ::nby1 := ::nby2 := ny; ::nbx1 := nx; ::nbx2 := nx + nSele
   ENDIF

   IF Empty( lNoGo )
      SetColor( ::cColor )
      IF lTextOut
         ::TextOut()
      ELSE
         IF nSele != Nil .AND. nSele > 0 .AND. ( nRowOld := (::nRow - ::y1 + 1) ) > 0
            ::LineOut( nRowOld )
         ENDIF
         ::LineOut( ny - ::nyFirst + 1 )
      ENDIF
      ::WriteTopPane()
      DevPos( ::nRow := (ny - ::nyFirst + ::y1), ::nCol := (nx - ::nxFirst + ::x1) )
   ELSE
      ::nRow := (ny - ::nyFirst + ::y1)
      ::nCol := (nx - ::nxFirst + ::x1)
   ENDIF

   RETURN Nil

METHOD ToString( cEol ) CLASS TEdit

   LOCAL i, nLen := Len( ::aText ), cBom := e"\xef\xbb\xbf", s := Iif( ::lBom, cBom, "" )
   LOCAL lTrim := hb_hGetDef( TEdit():options,"trimspaces", .F. )

   IF Empty( cEol )
      cEol := ::cEol
   ENDIF
   IF Empty( ::aText[nLen] )
      nLen --
   ENDIF
   FOR i := 1 TO nLen
      IF lTrim .AND. Right( ::aText[i], 1 ) == " "
         ::aText[i] := Trim( ::aText[i] )
      ENDIF
      s += Iif( ::lTabs, Strtran(::aText[i],Space(::nTablen),Chr(9)), ::aText[i] ) + cEol
   NEXT

   RETURN s

METHOD Save( cFileName ) CLASS TEdit

   IF cFileName == Nil
      cFileName := ::cFileName
   ENDIF
   IF Empty( cFileName )
      cFileName := edi_FileName( Self )
      ::lTextOut := .T.
   ENDIF

   IF Empty( cFileName )
      RETURN Nil
   ELSE
      IF Empty( hb_fnameDir( cFileName ) )
         cFileName := edi_CurrPath() + cFileName
      ENDIF
      ::cFileName := cFileName
   ENDIF

   IF Empty( ::funSave )
      hb_MemoWrit( cFileName, ::ToString() )
   ELSE
      ::funsave:exec( cFileName, ::ToString() )
   ENDIF
   ::lUpdated := .F.

   RETURN Nil

METHOD InsText( nLine, nPos, cText, lOver, lChgPos, lNoUndo ) CLASS TEdit

   LOCAL arr, i, nLine2 := nLine, nPos2, cTemp, cTextOld, nLineNew, nPosNew

   IF ::lReadOnly
      RETURN Nil
   ENDIF
   IF lOver == Nil; lOver := .F.; ENDIF
   IF lChgPos == Nil; lChgPos := .T.; ENDIF
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
         IF nLineNew - ::nyFirst + 1 > ::y2 - ::y1 - 1
            ::nyFirst := nLineNew - 3
         ENDIF
         nPosNew := cp_Len( ::lUtf8, arr[i] ) + 1
         IF nPosNew - ::nxFirst + 1 > ::x2 - ::x1 - 1
            ::nxFirst := nPosNew - 3
         ELSEIF nPosNew < ::nxFirst
            nPosNew := 1
         ENDIF
         DevPos( ::nRow := nLineNew - ::nyFirst + ::y1, ::nCol := nPosNew - ::nxFirst + ::x1 )
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
         ::nCol += i
      ENDIF
      IF ::nCol > ::x2
         IF lChgPos
            i := ::nCol - ::x2
            ::nxFirst += i
            ::nCol := ::x2
         ENDIF
         ::lTextOut := .T.
      ELSE
         ::LineOut( ::nRow - ::y1 + 1 )
         IF lChgPos
            DevPos( ::nRow, ::nCol )
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
         ::LineOut( nLine1 -::nyFirst + 1 )
         DevPos( ::nRow, ::nCol := (nPos1-::nxFirst) )
      ENDIF
   ELSE
      IF nPos1 > 1
         cTextOld := cp_Substr( ::lUtf8, ::aText[nLine1], nPos1 ) + Chr(10)
         ::aText[nLine1] := cp_Left( ::lUtf8, ::aText[nLine1], nPos1-1 )
      ELSE
         cTextOld := ::aText[nLine1] + Chr(10)
         ::aText[nLine1] := ""
      ENDIF
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
      ADel( ::aText, n )
      ncou ++

      ::aText := ASize( ::aText, Len(::aText) - ncou )
      IF ( i := nLine1 - ::nyFirst + 1 ) > 0 .AND. i < (::y2-::y1+1)
         DevPos( ::nRow := (nLine1-::nyFirst-::y1+2), ::nCol := (nPos1-::nxFirst+1-::x1) )
      ELSE
         ::nyFirst := nLine1
         DevPos( ::nRow := 1, ::nCol := nPos1 )
      ENDIF
      ::lTextOut := .T.
   ENDIF

   IF Empty( lNoUndo )
      ::Undo( nLine1, nPos1, nLine2, nPos2, UNDO_OP_DEL, cTextOld )
   ENDIF
   IF !Empty( ::oHili )
      ::oHili:UpdSource( nLine1 )
   ENDIF
   ::lUpdated := .T.

   RETURN Nil

METHOD Undo( nLine1, nPos1, nLine2, nPos2, nOper, cText ) CLASS TEdit

   LOCAL alast, nOpLast := 0, arrnew, i

   IF ::lReadOnly
      RETURN Nil
   ENDIF
   IF ::nUndo>0
      alast := ::aUndo[::nUndo]
      nOpLast := alast[UNDO_OPER]
   ENDIF
   IF PCount() == 0
      IF alast != Nil
         IF nOpLast == UNDO_OP_INS
            ::DelText( alast[UNDO_LINE1], alast[UNDO_POS1], alast[UNDO_LINE2], ;
               alast[UNDO_POS2], .T. )
            ::GoTo( alast[UNDO_LINE1], alast[UNDO_POS1] )

         ELSEIF nOpLast == UNDO_OP_OVER
            ::InsText( alast[UNDO_LINE1], alast[UNDO_POS1], alast[UNDO_TEXT], ;
               .T., .F., .T. )
            ::GoTo( alast[UNDO_LINE2], alast[UNDO_POS2] )

         ELSEIF nOpLast == UNDO_OP_DEL
            ::InsText( alast[UNDO_LINE1], alast[UNDO_POS1], alast[UNDO_TEXT], ;
               .F., .F., .T. )
            ::GoTo( alast[UNDO_LINE1], alast[UNDO_POS1] )

         ELSEIF nOpLast == UNDO_OP_SHIFT
            FOR i := alast[UNDO_LINE1] TO alast[UNDO_LINE2]
               IF alast[UNDO_TEXT] > 0
                  ::aText[i] := Substr( ::aText[i], alast[UNDO_TEXT]+1 )
               ELSEIF alast[UNDO_TEXT] < 0
                  ::aText[i] := Space(Abs(alast[UNDO_TEXT])) + ::aText[i]
               ENDIF
            NEXT
            ::GoTo( alast[UNDO_LINE2], 1 )
            ::lTextOut := .T.

         ENDIF
         ::aUndo[::nUndo] := Nil
         ::nUndo --
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

   LOCAL i, s := "", nSaveHis := TEdit():options["savehis"]


   IF nSaveHis > 0
      IF !Empty( TEdit():aSeaHis )
         s += "[SEARCH]" + Chr(13) + Chr(10)
         FOR i := 1 TO Len( TEdit():aSeaHis )
            s += "h" + PAdl(Ltrim(Str(i)),3,'0') + "=" + TEdit():aSeaHis[i] + Chr(13) + Chr(10)
         NEXT
      ENDIF

      IF !Empty( TEdit():aReplHis )
         s += "[REPLACE]" + Chr(13) + Chr(10)
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

      IF !Empty( TEdit():aEditHis )
         s += Chr(13) + Chr(10) + "[EDIT]" + Chr(13) + Chr(10)
         FOR i := 1 TO Len( TEdit():aEditHis )
            s += "h" + PAdl(Ltrim(Str(i)),3,'0') + "=" + TEdit():aEditHis[i,2] + "," + ;
               Ltrim(Str(TEdit():aEditHis[i,3])) + "," + Ltrim(Str(TEdit():aEditHis[i,4])) + "," + ;
               TEdit():aEditHis[i,1] + Chr(13) + Chr(10)
         NEXT
      ENDIF

      hb_MemoWrit( IIf( nSaveHis==1, hb_DirBase(), "" ) + "hbedit.his", s )
   ENDIF

   RETURN Nil

FUNCTION NameShortcut( cName, nWidth, cIns )

   IF Len( cName ) > nWidth
      cIns := Iif( cIns==Nil, "...", cIns )
      IF nWidth > Len(cIns) + 3
         cName := Left( cName,3 ) + cIns + Substr( cName, Len(cName)-(nWidth-3-Len(cIns)) )
      ELSE
         cName := ""
      ENDIF
   ENDIF

   RETURN cName

STATIC FUNCTION Text2cb( oEdit )

   LOCAL s := "", i, nby1, nby2, nbx1, nbx2

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
            IF i == nby1
               s += cp_Substr( oEdit:lUtf8, oEdit:aText[i], nbx1 ) + Chr(10)
            ELSEIF i == nby2
               s += cp_Left( oEdit:lUtf8, oEdit:aText[i], nbx2-1 )
            ELSE
               s += oEdit:aText[i] + oEdit:cEol
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN Iif( oEdit:lTabs, Strtran(s,Space(oEdit:nTablen),Chr(9)), s )

FUNCTION cb2Text( oEdit, lToText )

   LOCAL arr
   LOCAL i, lMulti := .F., s := hb_gtInfo( HB_GTI_CLIPBOARDDATA )

   TEdit():aCBoards[1,1] := s
   TEdit():aCBoards[1,2] := Nil
   FOR i := 2 TO MAX_CBOARDS
      IF !Empty( TEdit():aCBoards[i,1] )
         lMulti := .T.
         EXIT
      ENDIF
   NEXT

   IF lMulti
      FOR i := 1 TO MAX_CBOARDS
         aMenu_CB[i,1] := cp_Left( oEdit:lUtf8, TEdit():aCBoards[i,1], 32 )
         IF !Empty( TEdit():aCBoards[i,2] ) .AND. !( TEdit():aCBoards[i,2] == oEdit:cp )
            aMenu_CB[i,1] := hb_Translate( aMenu_CB[i,1], TEdit():aCBoards[i,2], oEdit:cp )
         ENDIF
      NEXT
      IF !Empty( i := FMenu( oEdit, aMenu_CB, 2, 6 ) )
         s := TEdit():aCBoards[i,1]
         IF !Empty( TEdit():aCBoards[i,2] ) .AND. !( TEdit():aCBoards[i,2] == oEdit:cp )
            s := hb_Translate( s, TEdit():aCBoards[i,2], oEdit:cp )
         ENDIF
      ENDIF
      oEdit:lTextOut := .T.
   ENDIF

   IF Empty( lToText )
      RETURN s
   ELSE
      IF oEdit:lTabs
         s := Strtran( s, Chr(9), Space(oEdit:nTablen) )
      ENDIF
      IF Chr(13) $ s
         s := Strtran( s, Chr(13), "" )
      ENDIF

      oEdit:InsText( oEdit:nRow - oEdit:y1 + oEdit:nyFirst, oEdit:nCol-oEdit:x1+oEdit:nxFirst, s, .F., .T. )
   ENDIF

   RETURN Nil

STATIC FUNCTION cbDele( oEdit )

   LOCAL nby1, nby2, nbx1, nbx2

   IF !oEdit:lReadOnly .AND. oEdit:nby1 >= 0 .AND. oEdit:nby2 >= 0
      IF oEdit:nby1 < oEdit:nby2 .OR. ( oEdit:nby1 == oEdit:nby2 .AND. oEdit:nbx1 < oEdit:nbx2 )
         nby1 := oEdit:nby1; nbx1 := oEdit:nbx1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
      ELSE
         nby1 := oEdit:nby2; nbx1 := oEdit:nbx2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
      ENDIF
      oEdit:nby1 := oEdit:nby2 := -1
      oEdit:DelText( nby1, nbx1, nby2, Max(nbx2-1,1) )
   ENDIF
   RETURN Nil

FUNCTION edi_ReadIni( xIni )

   LOCAL hIni, aIni, nSect, aSect, cSect, cLang, arr, arr1, s, n, i, nPos, cTemp
   LOCAL lIncSea := .F., lAutoIndent := .F., lSyntax := .T., lTrimSpaces := .F.
   LOCAL nSaveHis := 1, ncmdhis := 20, nseahis := 20, nedithis := 20, nEol := 0
   LOCAL hHili
   LOCAL aHiliOpt := { "keywords1","keywords2","keywords3","keywords4","quotes","scomm","startline","mcomm","block" }

   TEdit():lReadIni := .T.
   hIni := Iif( Valtype( xIni ) == "C", edi_iniRead( xIni ), xIni )

   SetBlink( .F. )
   aLangs := hb_Hash()

   IF !Empty( hIni )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "OPTIONS"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "defmode" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():nDefMode := Iif( (n := Val(cTemp)) < 2 .AND. n >= 0, n, 0 )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "incsearch" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lIncSea := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "autoindent" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lAutoIndent := ( Lower(cTemp) == "on" )
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
               IF hb_hHaskey( aSect, cTemp := "colormain" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():cColor := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "colorsel" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():cColorSel := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "colorpane" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():cColorPane := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "colorbra" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  TEdit():cColorbra := cTemp
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
               FOR i := 1 TO Len( arr )
                  s := aSect[ arr[i] ]
                  IF ( n := At( ",", s ) ) > 0
                     cTemp := AllTrim( Left( s,n-1 ) )
                     IF File( edi_CurrPath() + "plugins" + hb_ps() + cTemp ) .OR. ;
                           File( hb_DirBase() + "plugins" + hb_ps() + cTemp )
                        s := Substr( s, n+1 )
                        IF ( n := At( ",", s ) ) > 0
                           Aadd( TEdit():aPlugins, { cTemp, Substr( s, n+1 ), AllTrim( Left( s,n-1 ) ), Nil } )
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
                  IF ( n := Ascan( aHiliOpt, arr[i] ) ) > 0
                     TEdit():aHiliAttrs[n] := aSect[ arr[i] ]
                  ENDIF
               NEXT
            ENDIF
         ELSEIF Left( Upper(aIni[nSect]),5 ) == "LANG_"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               cLang := Lower( Substr(aIni[nSect],6) )
               hHili := aLangs[ cLang ] := hb_hash()
               hHili["colors"] := Array(Len(aHiliOpt))
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
   TEdit():options["edithismax"] := nedithis
   TEdit():options["autoindent"] := lAutoIndent
   TEdit():options["syntax"] := lSyntax

   IF Empty( TEdit():aCPages )
      TEdit():aCPages := { "RU866", "RU1251", "UTF8" }
   ENDIF
   TEdit():aCBoards := Array( MAX_CBOARDS,2 )
   FOR i := 1 TO MAX_CBOARDS
      TEdit():aCBoards[i,1] := TEdit():aCBoards[i,2] := ""
   NEXT

   IF nSaveHis > 0
      hIni := edi_iniRead( Iif( nSaveHis==1, hb_DirBase(), "" ) + "hbedit.his" )
      IF !Empty( hIni )
         hb_hCaseMatch( hIni, .F. )
         IF hb_hHaskey( hIni, "SEARCH" ) .AND. !Empty( aSect := hIni[ "SEARCH" ] )
            arr := ASort( hb_hKeys( aSect ) )
            TEdit():aSeaHis := Array( Len(arr) )
            FOR i := 1 TO Len(arr)
               TEdit():aSeaHis[i] := aSect[ arr[i] ]
            NEXT
         ENDIF
         IF hb_hHaskey( hIni, "REPLACE" ) .AND. !Empty( aSect := hIni[ "REPLACE" ] )
            arr := ASort( hb_hKeys( aSect ) )
            TEdit():aReplHis := Array( Len(arr) )
            FOR i := 1 TO Len(arr)
               TEdit():aReplHis[i] := aSect[ arr[i] ]
            NEXT
         ENDIF
         IF hb_hHaskey( hIni, "COMMANDS" ) .AND. !Empty( aSect := hIni[ "COMMANDS" ] )
            arr := ASort( hb_hKeys( aSect ) )
            TEdit():aCmdHis := Array( Len(arr) )
            FOR i := 1 TO Len(arr)
               TEdit():aCmdHis[i] := aSect[ arr[i] ]
            NEXT
         ENDIF
         IF hb_hHaskey( hIni, "EDIT" ) .AND. !Empty( aSect := hIni[ "EDIT" ] )
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

   RETURN Nil

FUNCTION mnu_Help( oEdit )

   LOCAL oHelp := TEdit():New( MemoRead(hb_DirBase() + "hbedit.help"), "$Help", ;
         oEdit:aRectFull[1], oEdit:aRectFull[2], oEdit:aRectFull[3], oEdit:aRectFull[4] )

   oHelp:lReadOnly := .T.
   oHelp:lCtrlTab  := .F.
   oHelp:Edit()

   RETURN Nil

FUNCTION mnu_Exit( oEdit )

   LOCAL nRes := 2

   IF oEdit:lUpdated
      nRes := edi_Alert( "File has been modified. Save?", "Yes", "No", "Cancel" )
   ENDIF
   IF nRes == 1 .OR. nRes == 2
      IF nRes == 1
         oEdit:Save()
      ENDIF
      oEdit:lShow := .F.
      oEdit:lClose := .T.
   ENDIF
   RETURN Nil

FUNCTION mnu_CPages( oEdit, aXY )

   LOCAL iRes

   IF !Empty( iRes := FMenu( oEdit, oEdit:aCPages, aXY[1], aXY[2] ) )
      oEdit:cp := oEdit:aCPages[iRes]
      hb_cdpSelect( oEdit:cp )
      oEdit:lUtf8 := ( Lower(oEdit:cp) == "utf8" )
      oEdit:TextOut()
   ENDIF

   RETURN Nil

FUNCTION mnu_Syntax( oEdit, aXY )

   LOCAL aMenu := { {"Syntax Off",@mnu_SyntaxOn(),Nil} }, i, arr := hb_hKeys( aLangs )

   FOR i := 1 TO Len( arr )
      AAdd( aMenu, {arr[i], @mnu_SyntaxOn(), arr[i]} )
   NEXT

   FMenu( oEdit, aMenu, aXY[1], aXY[2] )

   RETURN Nil

FUNCTION mnu_SyntaxOn( oEdit, cLang )

   oEdit:Highlighter( Iif( Empty(cLang), Nil, Hili():New( aLangs[cLang] ) ) )
   oEdit:cSyntaxType := cLang

   RETURN Nil

FUNCTION mnu_Windows( oEdit, aXY )

   LOCAL aMenu := { }, i, nCurr := 1

   FOR i := 1 TO Len( oEdit:aWindows )
      IF oEdit:aWindows[i] == oEdit
         nCurr := i
      ENDIF
      AAdd( aMenu, {NameShortcut(oEdit:aWindows[i]:cFileName,30,'~'),@mnu_ToWin(),i} )
   NEXT
   IF !Empty( oEdit:cLauncher )
      AAdd( aMenu, {oEdit:cLauncher,@mnu_ToWin(),0} )
   ENDIF

   FMenu( oEdit, aMenu, aXY[1], aXY[2],,,,, nCurr )

   RETURN Nil

FUNCTION mnu_ToWin( oEdit, n )

   oEdit:lShow := .F.
   oEdit:nCurr := n

   RETURN Nil

FUNCTION mnu_Save( oEdit, lAs )

   LOCAL cFileName, cPath

   IF !Empty( lAs )
      cFileName := edi_FileName( oEdit )
      oEdit:lTextOut := .T.
      IF !Empty( cFileName ) .AND. Empty( hb_fnameDir(cFileName) ) ;
            .AND. !Empty( cPath := hb_fnameDir(oEdit:cFileName) )
         cFileName := cPath + cFileName
      ENDIF
   ENDIF

   oEdit:Save( cFileName )

   RETURN Nil

FUNCTION mnu_F3( oEdit )

   LOCAL i

   IF oEdit:nby1 >= 0 .AND. oEdit:nby2 >= 0
      oEdit:lF3 := .T.
   ENDIF
   IF !oEdit:lF3
      oEdit:nby1 := oEdit:nRow - oEdit:y1 + oEdit:nyFirst
      oEdit:nbx1 := oEdit:nCol - oEdit:x1 + oEdit:nxFirst
      oEdit:nby2 := oEdit:nbx2 := -1
   ENDIF
   oEdit:lF3 := !oEdit:lF3
   IF !oEdit:lF3
      IF Empty( aMenu_CB )
         aMenu_CB := Array(MAX_CBOARDS)
         FOR i := 1 TO MAX_CBOARDS
            aMenu_CB[i] := { Nil,, i }
         NEXT
      ENDIF

      TEdit():aCBoards[1,1] := hb_gtInfo( HB_GTI_CLIPBOARDDATA )
      TEdit():aCBoards[1,2] := Nil
      FOR i := 1 TO MAX_CBOARDS
         aMenu_CB[i,1] := cp_Left( oEdit:lUtf8, TEdit():aCBoards[i,1], 32 )
         IF !Empty( TEdit():aCBoards[i,2] ) .AND. !( TEdit():aCBoards[i,2] == oEdit:cp )
            aMenu_CB[i,1] := hb_Translate( aMenu_CB[i,1], TEdit():aCBoards[i,2], oEdit:cp )
         ENDIF
      NEXT
      IF !Empty( i := FMenu( oEdit, aMenu_CB, 2, 6 ) )
         TEdit():aCBoards[i,1] := Text2cb( oEdit )
         TEdit():aCBoards[i,2] := oEdit:cp
         IF i == 1
            hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] )
         ENDIF
      ENDIF
      oEdit:lTextOut := .T.
   ENDIF

   RETURN Nil

FUNCTION mnu_F4( oEdit, aXY )

   LOCAL aMenu := { {"New file",@mnu_NewWin(),Nil,"Shift-F4"}, {"Open file",@mnu_OpenFile(),Nil,"Ctrl-F4"} }, i

   FOR i := 1 TO Len( oEdit:aEditHis )
      AAdd( aMenu, { NameShortcut(hb_Translate(oEdit:aEditHis[i,1],"UTF8"), 36,'~'), ;
         @mnu_OpenRecent(),i } )
   NEXT

   FMenu( oEdit, aMenu, aXY[1], aXY[2] )

   RETURN Nil

FUNCTION mnu_OpenRecent( oEdit, n )

   LOCAL cFileName := hb_Translate( oEdit:aEditHis[n,1], "UTF8", oEdit:cpInit )

   RETURN mnu_NewWin( oEdit, cFileName )

FUNCTION mnu_NewWin( oEdit, cFileName )

   LOCAL oNew, s, j, cText

   IF !Empty( cFileName )
      s := Lower( cFileName )
      IF ( j := Ascan( oEdit:aWindows, {|o|Lower(o:cFileName)==s} ) ) > 0
         oEdit:lShow := .F.
         oEdit:nCurr := j
         RETURN oEdit:aWindows[j]
      ENDIF
      IF File( cFileName )
         cText := Memoread( cFileName )
      ELSE
         edi_Alert( "File not found" )
         RETURN Nil
      ENDIF
   ENDIF

   IF ( !Empty( oEdit:aText ) .AND. !Empty( oEdit:aText[1] ) ) ;
         .OR. oEdit:lUpdated .OR. !Empty( oEdit:cFilename )
      hb_cdpSelect( oEdit:cpInit )
      oNew := TEdit():New( cText, cFileName, oEdit:aRectFull[1], oEdit:aRectFull[2], oEdit:aRectFull[3], oEdit:aRectFull[4] )
      oNew:funSave := oEdit:funSave
      hb_cdpSelect( oEdit:cp )
      oEdit:lShow := .F.
      oEdit:nCurr := Len( oEdit:aWindows )
      RETURN oEdit:aWindows[oEdit:nCurr]
   ELSE
      oEdit:SetText( cText, cFileName )
      oEdit:WriteTopPane( 1 )
      DevPos( oEdit:nRow, oEdit:nCol )
   ENDIF

   RETURN oEdit

FUNCTION mnu_OpenFile( oEdit )

   LOCAL oldc := SetColor( "N/W,W+/BG" ), cName, nRes
   LOCAL aGets := { {11,12,0,"",56}, ;
      {11,68,2,"[^]",3,"N/W","W+/RB",{||mnu_FileList(oEdit,aGets[1])}}, ;
      {13,26,2,"[Open]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {13,46,2,"[Cancel]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ESC))}} }

   hb_cdpSelect( "RU866" )
   @ 09, 10, 14, 72 BOX "ÚÄ¿³ÙÄÀ³ "
   //@ 12, 10 SAY "Ã"
   //@ 12, 72 SAY "´"
   @ 12, 11 TO 12, 71
   hb_cdpSelect( oEdit:cp )
   @ 10,22 SAY "Open file"
   SetColor( "W+/BG" )

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cName := aGets[1,4]
      mnu_NewWin( oEdit, cName )
   ENDIF

   SetColor( oldc )
   DevPos( oEdit:nRow, oEdit:nCol )

   RETURN Nil

FUNCTION mnu_FileList( oEdit, aGet )

   LOCAL cPrefix, cFileName
   LOCAL cScBuf := Savescreen( 12, 12, 22, 67 )

#ifdef __PLATFORM__UNIX
   cPrefix := '/'
#else
   cPrefix := hb_curDrive() + ':\'
#endif

   cFileName := edi_SeleFile( oEdit, cPrefix + CurDir() + hb_ps(), 12, 12, 22, 67 )
   Restscreen( 12, 12, 22, 67, cScBuf )

   IF !Empty( cFileName )
      aGet[4] := cFileName
      ShowGetItem( aGet, .F., oEdit:lUtf8 )
   ENDIF

   RETURN Nil

FUNCTION mnu_Sea_goto( oEdit, aXY )

   LOCAL aMenu := { {"Search",@mnu_Search(),Nil,"F7"}, {"Next",@mnu_SeaNext(),.T.,"Shift-F7"}, ;
      {"Previous",@mnu_SeaNext(),.F.,"Alt-F7"}, {"Replace",@mnu_SeaAndRepl(),Nil,"Ctrl-F7"}, ;
      {"Go to",@mnu_GoTo(),Nil,"Alt-F8"} }

   FMenu( oEdit, aMenu, aXY[1], aXY[2] )

   RETURN Nil

FUNCTION mnu_Search( oEdit )

   LOCAL oldc := SetColor( "N/W,N/W,,,N/W" ), nRes, i
   LOCAL aGets := { {11,22,0,"",33,"W+/BG","W+/BG"}, ;
      {11,55,2,"[^]",3,"N/W","W+/RB",{||mnu_SeaHist(oEdit,aGets[1])}}, ;
      {12,23,1,.F.,1}, {12,43,1,.F.,1}, ;
      {14,25,2,"[Search]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {14,40,2,"[Cancel]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cSearch, lCase, lBack := .F., cs_utf8
   LOCAL ny := oEdit:RowToLine(), nx := oEdit:ColToPos()

   hb_cdpSelect( "RU866" )
   @ 09, 20, 15, 60 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 13, 20 SAY "Ã"
   @ 13, 60 SAY "´"
   @ 13, 21 TO 13, 59
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Search for"
   @ 12, 22 SAY "[ ] Case sensitive"
   @ 12, 42 SAY "[ ] Backward"

   IF !Empty( TEdit():aSeaHis )
      aGets[3,4] := lCase_Sea
   ENDIF

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cSearch := Trim( aGets[1,4] )
      lCase := aGets[3,4]
      lBack := aGets[4,4]
      cs_utf8 := hb_Translate( cSearch,, "UTF8" )
      IF ( i := Ascan( TEdit():aSeaHis, {|cs|cs==cs_utf8} ) ) > 0
         ADel( TEdit():aSeaHis, i )
         hb_AIns( TEdit():aSeaHis, 1, cs_utf8, .F. )
      ELSE
         hb_AIns( TEdit():aSeaHis, 1, cs_utf8, Len(TEdit():aSeaHis)<hb_hGetDef(TEdit():options,"seahismax",10) )
      ENDIF
      IF oEdit:Search( cSearch, lCase_Sea := lCase, !lBack, @ny, @nx )
         oEdit:GoTo( ny, nx, 0 )
      ELSE
         edi_Alert( "String is not found:;" + cSearch )
         oEdit:lTextOut := .T.
      ENDIF
   ENDIF

   SetColor( oldc )
   DevPos( oEdit:nRow, oEdit:nCol )

   RETURN Nil

FUNCTION mnu_SeaHist( oEdit, aGet )

   LOCAL aMenu, i, bufc
   IF !Empty( TEdit():aSeaHis )
      aMenu := Array( Len(TEdit():aSeaHis) )
      FOR i := 1 TO Len(aMenu)
         aMenu[i] := { hb_Translate( TEdit():aSeaHis[i], "UTF8" ), Nil, i }
      NEXT
      bufc := SaveScreen( 12, 22, 12 + Min(6,Len(aMenu)+1), 55 )
      IF !Empty( i := FMenu( oEdit, aMenu, 12, 22, 12 + Min(6,Len(aMenu)+1), 55 ) )
         aGet[4] := aMenu[i,1]
         ShowGetItem( aGet, .F., oEdit:lUtf8 )
      ENDIF
      RestScreen( 12, 22, 12 + Min(6,Len(aMenu)+1), 55, bufc )
      __KeyBoard(Chr(K_UP))
   ENDIF

   RETURN Nil

FUNCTION mnu_SeaNext( oEdit, lNext )

   LOCAL ny := oEdit:RowToLine(), nx := oEdit:ColToPos()
   LOCAL cSearch

   IF !Empty( TEdit():aSeaHis )
      cSearch := hb_Translate(TEdit():aSeaHis[1],"UTF8")
      IF oEdit:Search( cSearch, lCase_Sea, lNext, @ny, @nx )
         oEdit:GoTo( ny, nx, 0 )
      ELSE
         edi_Alert( "String is not found:;" + cSearch )
         oEdit:lTextOut := .T.
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION mnu_SeaAndRepl( oEdit )

   LOCAL oldc := SetColor( "N/W,N/W,,,N/W" ), nRes, i
   LOCAL aGets := { {11,22,0,"",33,"W+/BG","W+/BG"}, ;
      {11,55,2,"[^]",3,"N/W","W+/RB",{||mnu_SeaHist(oEdit,aGets[1])}}, ;
      {13,22,0,"",33,"W+/BG","W+/BG"}, ;
      {14,23,1,.F.,1}, {14,43,1,.F.,1}, ;
      {16,25,2,"[Replace]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {16,40,2,"[Cancel]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL cSearch, cRepl, lCase, lBack := .F., cs_utf8, cr_utf8, nSeaLen
   LOCAL ny := oEdit:RowToLine(), nx := oEdit:ColToPos()

   hb_cdpSelect( "RU866" )
   @ 09, 20, 17, 60 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 15, 20 SAY "Ã"
   @ 15, 60 SAY "´"
   @ 15, 21 TO 15, 59
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Search for"
   @ 12,22 SAY "Replace with"
   @ 14, 22 SAY "[ ] Case sensitive"
   @ 14, 42 SAY "[ ] Backward"

   IF !Empty( TEdit():aSeaHis )
      aGets[4,4] := lCase_Sea
   ENDIF

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cSearch := Trim( aGets[1,4] )
      nSeaLen := cp_Len( oEdit:lUtf8, cSearch )
      cRepl := Trim( aGets[3,4] )
      lCase := aGets[4,4]
      lBack := aGets[5,4]
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
      DO WHILE .T.
         IF oEdit:Search( cSearch, lCase_Sea := lCase, !lBack, @ny, @nx )
            oEdit:GoTo( ny, nx, nSeaLen )
            oEdit:TextOut()
            DevPos( oEdit:nRow, oEdit:nCol )
            IF nRes != 2
               nRes := mnu_ReplNext( oEdit )
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
            edi_Alert( "String is not found:;" + cSearch )
            oEdit:lTextOut := .T.
            EXIT
         ENDIF
      ENDDO
   ENDIF

   SetColor( oldc )
   DevPos( oEdit:nRow, oEdit:nCol )

   RETURN Nil

FUNCTION mnu_ReplNext( oEdit )

   LOCAL oldc := SetColor( "N/W,N/W,,,N/W" ), nRes := 0
   LOCAL y1 := Iif( Row()>oEdit:y2-6, oEdit:y1+2, oEdit:y2-6 ), x1 := oEdit:x2-40
   LOCAL aGets := { ;
      {y1+4,x1+2,2,"[Replace]",9,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+4,x1+14,2,"[All]",5,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+4,x1+21,2,"[Skip]",6,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+4,x1+30,2,"[Cancel]",8,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}} }
   LOCAL cSearch, cRepl, nSeaLen, ny, nx

   IF !Empty( TEdit():aSeaHis ) .AND. !Empty( TEdit():aReplHis )
      hb_cdpSelect( "RU866" )
      @ y1, x1, y1+5, x1+40 BOX "ÚÄ¿³ÙÄÀ³ "
      @ y1+3, x1 SAY "Ã"
      @ y1+3, x1+40 SAY "´"
      @ y1+3, x1+1 TO y1+3, x1+39
      hb_cdpSelect( oEdit:cp )

      ny := oEdit:RowToLine()
      nx := oEdit:ColToPos()
      cSearch := hb_Translate(TEdit():aSeaHis[1],"UTF8")
      nSeaLen := cp_Len( oEdit:lUtf8, cSearch )
      cSearch := cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx, nSeaLen )
      cRepl := hb_Translate(TEdit():aReplHis[1],"UTF8")
      @ y1+1,x1+2 SAY 'Replace "' + cSearch + '"'
      @ y1+2,x1+2 SAY 'With "' + cRepl + '"'

      nRes := edi_Read( aGets )
      SetColor( oldc )
      DevPos( oEdit:nRow, oEdit:nCol )
   ENDIF

   RETURN nRes

FUNCTION mnu_GoTo( oEdit )

   LOCAL oldc := SetColor( "N/W,W+/BG" )
   LOCAL aGets := { {11,27,0,"",26}, ;
      {13,28,2,"[Ok]",4,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {13,42,2,"[Cancel]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL arr, ny, nx, nRes

   hb_cdpSelect( "RU866" )
   @ 09, 25, 14, 55 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 12, 25 SAY "Ã"
   @ 12, 55 SAY "´"
   @ 12, 26 TO 12, 54
   hb_cdpSelect( oEdit:cp )

   @ 10,32 SAY "Go to position"
   SetColor( "W+/BG" )

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      arr := hb_aTokens( aGets[1,4], "," )
      ny := Val( arr[1] )
      nx := Iif( Len(arr)>1 .AND. Val(arr[2])>0, Val(arr[2]), 1 )
      IF ny > 0 .AND. ny <= Len(oEdit:aText)
         IF nx >= cp_Len( oEdit:lUtf8, oEdit:aText[ny] )
            nx := 1
         ENDIF
         oEdit:GoTo( ny, nx, 0 )
      ENDIF
   ENDIF

   SetColor( oldc )
   DevPos( oEdit:nRow, oEdit:nCol )

   RETURN Nil

FUNCTION mnu_Plugins( oEdit )

   LOCAL aMenu := {}, i, cPlugin, cFullPath

   FOR i := 1 TO Len( TEdit():aPlugins )
      IF Empty( TEdit():aPlugins[i,3] ) .OR. TEdit():aPlugins[i,3] == oEdit:cSyntaxType
         AAdd( aMenu, { TEdit():aPlugins[i,2], Nil, i} )
      ENDIF
   NEXT
   IF !Empty( aMenu )
      IF ( i := FMenu( oEdit, aMenu, 2, 6 ) ) > 0
         i := aMenu[i,3]
         IF Empty( TEdit():aPlugins[i,4] )
            cPlugin := TEdit():aPlugins[i,1]
            IF File( cFullPath := ( edi_CurrPath() + "plugins" + hb_ps() + cPlugin ) )
               TEdit():aPlugins[i,4] := hb_hrbLoad( cFullPath )
            ELSEIF File( cFullPath := ( hb_DirBase() + "plugins" + hb_ps() + cPlugin ) )
               TEdit():aPlugins[i,4] := hb_hrbLoad( cFullPath )
            ENDIF
         ENDIF
         IF !Empty( TEdit():aPlugins[i,4] )
            hb_hrbDo( TEdit():aPlugins[i,4], oEdit )
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION mnu_ChgMode( oEdit, lBack )

   SetColor( "N/W+" )
   Scroll( oEdit:y1-1, oEdit:x1, oEdit:y1-1, oEdit:x2 )
   Inkey( 0.2 )
   DevPos( oEdit:nRow, oEdit:nCol )

   IF !Empty( lBack )
      oEdit:nMode := 0
      oEdit:WriteTopPane( 1 )
   ELSE
      IF oEdit:nMode == 0
         oEdit:nMode := 1
         oEdit:WriteTopPane( 1 )
      ELSEIF oEdit:nMode == 1
         oEdit:nMode := 2
         oEdit:WriteTopPane( 1 )
         mnu_CmdLine( oEdit )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_Move( oEdit, nKey, nRepeat )

   LOCAL i

   IF nRepeat == Nil; nRepeat := 1; ENDIF
   IF nKey == 71 // G
      IF nRepeat == 1
         oEdit:nPosBack := oEdit:ColToPos()
         oEdit:nLineBack := oEdit:RowToLine()
         IF Len( oEdit:aText ) > oEdit:y2-oEdit:y1+1
            oEdit:nxFirst := 1
            oEdit:nyFirst := Len( oEdit:aText ) - (oEdit:y2-oEdit:y1)
            oEdit:lTextOut := .T.
            DevPos( oEdit:y2, Min( oEdit:nCol,Iif(nKey==K_CTRL_END,1,cp_Len(oEdit:lUtf8,ATail(oEdit:aText))+1)) )
         ELSE
            DevPos( Len(oEdit:aText)+oEdit:y1-1, Iif(nKey==K_CTRL_END,1,Min(oEdit:nCol,cp_Len(oEdit:lUtf8,ATail(oEdit:aText))+1)) )
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
      DevPos( Row(), oEdit:x1 )
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
      END
   NEXT

   RETURN Nil

STATIC FUNCTION edi_GoUp( oEdit )

   LOCAL nRow := Row()

   IF nRow == oEdit:y1
      IF oEdit:nyFirst > 1
         oEdit:nyFirst --
         oEdit:lTextOut := .T.
      ENDIF
   ELSE
      DevPos( oEdit:nRow := (nRow-1), oEdit:nCol )
   ENDIF
   RETURN Nil

STATIC FUNCTION edi_GoDown( oEdit )

   LOCAL nRow := Row()

   IF nRow - oEdit:y1 + oEdit:nyFirst < Len(oEdit:aText)
      IF nRow < oEdit:y2
         DevPos( nRow+1, Col() )
      ELSE
         oEdit:nyFirst ++
         oEdit:lTextOut := .T.
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_GoRight( oEdit )

   LOCAL nCol := Col()

   IF nCol == oEdit:x2
      oEdit:nxFirst ++
      oEdit:lTextOut := .T.
   ELSE
      DevPos( Row(), nCol+1 )
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_GoLeft( oEdit )

   LOCAL nCol := Col()

   IF nCol == oEdit:x1
      IF oEdit:nxFirst > 1
         oEdit:nxFirst --
         oEdit:lTextOut := .T.
      ENDIF
   ELSE
      DevPos( Row(), nCol-1 )
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_GoEnd( oEdit )

   LOCAL n := oEdit:RowToLine()
   LOCAL nCol := cp_Len( oEdit:lUtf8, oEdit:aText[n] ) - oEdit:nxFirst + Iif( oEdit:nMode==1, 0, 1 )

   IF nCol <= 0 .OR. nCol > oEdit:x2 - oEdit:x1
      oEdit:nxFirst := Max( 1, cp_Len( oEdit:lUtf8, oEdit:aText[n] ) - ( oEdit:x2 - oEdit:x1 ) + 1 )
      oEdit:lTextOut := .T.
      DevPos( Row(), oEdit:nCol := (Iif( nCol <= 0, cp_Len( oEdit:lUtf8, oEdit:aText[n] ) - oEdit:nxFirst + 1, oEdit:x2 )) )
   ELSE
      DevPos( Row(), oEdit:nCol := nCol )
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_ConvertCase( oEdit, lUpper )

   LOCAL s, i, nby1, nby2, nbx1, nbx2, lUtf8 := oEdit:lUtf8

   IF oEdit:nby1 < 0 .OR. oEdit:nby2 < 0
      RETURN Nil
   ENDIF

   IF oEdit:nby1 < oEdit:nby2 .OR. ( oEdit:nby1 == oEdit:nby2 .AND. oEdit:nbx1 < oEdit:nbx2 )
      nby1 := oEdit:nby1; nbx1 := oEdit:nbx1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
   ELSE
      nby1 := oEdit:nby2; nbx1 := oEdit:nbx2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
   ENDIF
   IF nby1 == nby2
      s := cp_Substr( lUtf8, oEdit:aText[nby1], nbx1, nbx2-nbx1 )
   ELSE
      FOR i := nby1 TO nby2
         IF i == nby1
            s := cp_Substr( lUtf8, oEdit:aText[nby1], nbx1 )
         ELSEIF i == nby2
            s += Chr(10) + cp_Left( lUtf8, oEdit:aText[i], nbx2-1 )
         ELSE
            s += oEdit:aText[i]
         ENDIF
      NEXT
   ENDIF
   oEdit:InsText( nby1, nbx1, Iif( lUpper, cp_Upper(lUtf8,s), cp_Lower(lUtf8,s) ), .T., .F. )

   RETURN Nil

STATIC FUNCTION edi_AlphaNum( nch )

   RETURN (nch >= 48 .AND. nch <= 57) .OR. (nch >= 65 .AND. nch <= 90) .OR. ;
      (nch >= 97 .AND. nch <= 122) .OR. nch == 95 .OR. nch >= 128

STATIC FUNCTION edi_NextWord( oEdit, lBigW, lEndWord )
   LOCAL ny, nx, nRow := Row(), nCol := Col(), nLen, lUtf8 := oEdit:lUtf8, ch, nch
   LOCAL lOk := .F., lAlphaNum

   ny := oEdit:RowToLine(nRow)
   nx := oEdit:ColToPos( nCol )
   nLen := cp_Len( lUtf8, oEdit:aText[ny] )

   IF nx > nLen
      RETURN Nil
   ENDIF

   ch := cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 )
   IF ch == " "
      DO WHILE ++nx <= nLen .AND. cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 ) == ch; ENDDO
      lOk := Empty( lEndWord )
   ENDIF

   IF !lOk
      lAlphaNum := edi_AlphaNum( cp_Asc( lUtf8, cp_Substr(lUtf8,oEdit:aText[ny],nx,1) ) )
      DO WHILE ++nx <= nLen
         ch := cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 )
         IF ( ch == " " ) .OR. ( !lBigW .AND. ;
               lAlphaNum != edi_AlphaNum( cp_Asc(lUtf8,ch) ) )
            IF !Empty( lEndWord ) .AND. nx - (nCol - oEdit:x1 + oEdit:nxFirst) > 1
               nx --
               EXIT
            ENDIF
            IF ch == " "
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
   IF nx - oEdit:nxFirst + oEdit:x1 >= oEdit:x2
      oEdit:nxFirst := nx + oEdit:x1 - oEdit:x2 + 3
      oEdit:lTextOut := .T.
   ENDIF
   DevPos( nRow, oEdit:nCol := ( nx - oEdit:nxFirst + oEdit:x1 ) )

   RETURN Nil

STATIC FUNCTION edi_PrevWord( oEdit, lBigW )
   LOCAL ny, nx, nRow := Row(), nCol := Col(), lUtf8 := oEdit:lUtf8
   LOCAL ch, lAlphaNum

   ny := oEdit:RowToLine(nRow)
   nx := oEdit:ColToPos( nCol )

   ch := cp_Substr( lUtf8, oEdit:aText[ny], --nx, 1 )
   IF ch == " "
      DO WHILE --nx > 1 .AND. cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 ) == " "; ENDDO
   ENDIF

   lAlphaNum := edi_AlphaNum( cp_Asc( lUtf8, cp_Substr(lUtf8,oEdit:aText[ny],nx,1) ) )
   DO WHILE --nx > 0
      IF ( ch := cp_Substr( lUtf8, oEdit:aText[ny], nx, 1 ) ) == " " .OR. ;
            ( !lBigW .AND. lAlphaNum != edi_AlphaNum( cp_Asc(lUtf8,ch) ) )
         nx ++
         EXIT
      ENDIF
   ENDDO
   nx := Max( nx, 1 )
   IF nx < oEdit:nxFirst
      oEdit:nxFirst := Max( nx + oEdit:x1 - oEdit:x2 + 3, 1 )
      oEdit:lTextOut := .T.
   ENDIF
   DevPos( nRow, oEdit:nCol := ( nx - oEdit:nxFirst + oEdit:x1 ) )

   RETURN Nil

STATIC FUNCTION edi_FileName( oEdit )

   LOCAL oldc := SetColor( "N/W,W+/BG" )
   LOCAL aGets := { {11,22,0,"",36} }, cName

   hb_cdpSelect( "RU866" )
   @ 09, 20, 13, 60 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Save file as"
   SetColor( "W+/BG" )

   IF edi_READ( aGets ) > 0
      cName := aGets[1,4]
   ENDIF

   SetColor( oldc )
   DevPos( oEdit:nRow, oEdit:nCol )

   RETURN cName

STATIC FUNCTION edi_Indent( oEdit, lRight )

   LOCAL i, n, nby1, nby2, nbx2

   IF oEdit:lReadOnly .OR. oEdit:nby1 < 0 .OR. oEdit:nby2 < 0
      RETURN Nil
   ENDIF
   IF oEdit:nby1 <= oEdit:nby2
      nby1 := oEdit:nby1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
   ELSE
      nby1 := oEdit:nby2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
   ENDIF
   FOR i := nby1 TO nby2
      IF i == nby2 .AND. nbx2 == 1
         nby2 --
         EXIT
      ENDIF
      IF lRight
         oEdit:aText[i] := " " + oEdit:aText[i]
      ELSEIF Left( oEdit:aText[i],1 ) == " "
         oEdit:aText[i] := Substr( oEdit:aText[i], 2 )
      ENDIF
      n := i - oEdit:nyFirst + 1
      IF n > 0 .AND. n < oEdit:y2-oEdit:y1
         oEdit:LineOut( n )
      ENDIF
   NEXT
   oEdit:Undo( nby1, 0, nby2, 0, UNDO_OP_SHIFT, Iif(lRight,1,-1) )
   oEdit:lUpdated := .T.

   RETURN Nil

STATIC FUNCTION edi_BookMarks( oEdit, nKey, lSet )

   LOCAL arr

   IF nKey >= 97 .AND. nKey <= 122
      IF lSet
         oEdit:hBookMarks[nKey] := { oEdit:RowToLine(), oEdit:ColToPos() }
      ELSE
         IF hb_hHaskey( oEdit:hBookMarks, nKey )
            arr := oEdit:hBookMarks[nKey]
            oEdit:Goto( arr[1], arr[2] )
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_Bracket( oEdit, lCalcOnly, lPairOnly )

   LOCAL nyInit, ny := oEdit:RowToLine(), nx := oEdit:ColToPos()
   LOCAL c, nPos := 0
   LOCAL b1 := "([{", b2 := ")]}", i, np := 0

   IF ny > Len( oEdit:aText ) .OR. nx > cp_Len( oEdit:lUtf8, oEdit:aText[ny] )
      RETURN 0
   ENDIF
   c := cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx, 1 )
   nyInit := ny
   IF ( i := At( c, b1 ) ) > 0
      IF edi_InQuo( oEdit, ny, nx ) == 0
         nPos := nx
         DO WHILE ny <= Len( oEdit:aText )
            DO WHILE ( nPos := edi_FindChNext( oEdit, ny, nPos, c, Substr( b2,i,1 ) ) ) > 0
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
      IF edi_InQuo( oEdit, ny, nx ) == 0
         nPos := nx
         DO WHILE ny > 0
            DO WHILE ( nPos := edi_FindChPrev( oEdit, ny, nPos, c, Substr( b1,i,1 ) ) ) > 0
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
               nPos := Iif( ny > 0, cp_Len( oEdit:lUtf8, oEdit:aText[ny] ) + 1, 1 )
            ELSE
               EXIT
            ENDIF
         ENDDO
      ENDIF
   ELSEIF Empty( lPairOnly )
      IF edi_InQuo( oEdit, ny, nx ) == 0
         nPos := edi_FindChNext( oEdit, ny, nx, ")", "]", "}" )
      ENDIF
   ENDIF
   IF Empty( lCalcOnly ) .AND. nPos > 0
      oEdit:GoTo( ny, nPos )
   ENDIF

   RETURN Iif( !Empty(lCalcOnly), Iif( ny==nyInit .OR. nPos==0, nPos, {ny,nPos} ), Nil )

STATIC FUNCTION edi_FindChNext( oEdit, nLine, nPos, ch1, ch2, ch3 )

   LOCAL c, s := oEdit:aText[nLine], cQuo, lQuo := .F., nLen := cp_Len( oEdit:lUtf8,s )

   DO WHILE ++nPos <= nLen
      c := cp_Substr( oEdit:lUtf8, s, nPos, 1 )
      IF lQuo
         IF c == cQuo
            lQuo := .F.
         ENDIF
      ELSE
        IF c == ch1 .OR. c == ch2 .OR. c == ch3
            RETURN nPos
        ELSEIF c $ ["']
            lQuo := .T.
            cQuo := c
         ENDIF
      ENDIF
   ENDDO

   RETURN 0

STATIC FUNCTION edi_FindChPrev( oEdit, nLine, nPos, ch1, ch2, ch3 )

   LOCAL c, s := oEdit:aText[nLine], cQuo, lQuo := .F., nPosQuo := edi_InQuo( oEdit, nLine, nPos )

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
        ELSEIF c $ ["']
            lQuo := .T.
            cQuo := c
         ENDIF
      ENDIF
   ENDDO

   RETURN 0

STATIC FUNCTION edi_InQuo( oEdit, nLine, nPos )

   LOCAL i := 0, c, s := oEdit:aText[nLine], cQuo, lQuo := .F., nPosQuo := 0

   DO WHILE ++i < nPos
      c := cp_Substr( oEdit:lUtf8, s, i,1 )
      IF lQuo
         IF c == cQuo
            lQuo := .F.
            nPosQuo := 0
         ENDIF
      ELSE
         IF c $ ["']
            lQuo := .T.
            cQuo := c
            nPosQuo := i
         ENDIF
      ENDIF
   ENDDO

   RETURN nPosQuo

/*
 * edi_AddWindow( oEdit, cText, cFileName, nPlace, nSpace )
 * Adds new edit window, truncates the current edit window
 * oEdit - current window
 * cText, cFilename - the data of a new window
 * nPlace: 0 - top, 1 - left, 2 - bottom, 3 - right
 * nSpace - the number of rows or columns of a new window
 */
FUNCTION edi_AddWindow( oEdit, cText, cFileName, nPlace, nSpace )

   LOCAL oNew, y1 := oEdit:y1, x1 := oEdit:x1, y2 := oEdit:y2, x2 := oEdit:x2

   IF nPlace == 0
      y2 := y1 + nSpace - 1
      oEdit:y1 := y2 + 1
   ELSEIF nPlace == 1
      x2 := x1 + nSpace - 1
      oEdit:x1 := x2 + 1
   ELSEIF nPlace == 2
      y1 := y2 - nSpace
      oEdit:y2 := y1-1
   ELSEIF nPlace == 3
      x1 := x2 - nSpace
      oEdit:x2 := x1-1
   ENDIF
   oNew := TEdit():New( cText, cFileName, y1, x1, y2, x2 )
   oNew:oParent := oEdit
   oEdit:lShow := .F.
   TEdit():nCurr := Len( TEdit():aWindows )

   RETURN oNew

FUNCTION edi_CurrPath()

   LOCAL cPrefix

#ifdef __PLATFORM__UNIX
   cPrefix := '/'
#else
   cPrefix := hb_curDrive() + ':\'
#endif

   RETURN cPrefix + CurDir() + hb_ps()

STATIC FUNCTION edi_MapKey( oEdit, nKey )

   LOCAl c, nPos

   IF nKey >= 127 .AND. !Empty(cLangMapCP) .AND. !Empty(aLangMapUpper) .AND. !Empty(aLangMapLower)
      c := hb_Translate( cp_Chr( oEdit:lUtf8, nKey ), oEdit:cp, cLangMapCP )
      IF ( nPos := cp_At( oEdit:lUtf8, c, aLangMapUpper[1] ) ) > 0
         RETURN cp_Asc( oEdit:lUtf8, hb_Translate( cp_Substr(oEdit:lUtf8,aLangMapUpper[2],nPos,1), cLangMapCP, oEdit:cp ) )
      ELSEIF ( nPos := cp_At( oEdit:lUtf8, c, aLangMapLower[1] ) ) > 0
         RETURN cp_Asc( oEdit:lUtf8, hb_Translate( cp_Substr(oEdit:lUtf8,aLangMapLower[2],nPos,1), cLangMapCP, oEdit:cp ) )
      ENDIF
   ENDIF

   RETURN nKey

FUNCTION cp_Chr( lUtf8, n )

   RETURN Iif( lUtf8, hb_utf8Chr( n ), Chr( n ) )

FUNCTION cp_Asc( lUtf8, s )

   RETURN Iif( lUtf8, hb_utf8Asc( s ), Asc( s ) )

FUNCTION cp_Substr( lUtf8, cString, nPos, nLen )
   RETURN Iif( lUtf8, ;
      Iif( nLen==Nil, hb_utf8Substr( cString, nPos ), hb_utf8Substr( cString, nPos, nLen ) ), ;
      Iif( nLen==Nil, Substr( cString, nPos ), Substr( cString, nPos, nLen ) ) )

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
