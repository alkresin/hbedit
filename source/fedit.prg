
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
#include "hbfunclist.ch"
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

REQUEST HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251, HB_CODEPAGE_RUKOI8, HB_CODEPAGE_FR850
REQUEST HB_CODEPAGE_FRWIN, HB_CODEPAGE_FRISO, HB_CODEPAGE_UTF8
REQUEST QOUT, MAXCOL, MAXROW
REQUEST HB_TOKENPTR

STATIC aMenuMain := { {"Exit",@mnu_Exit(),Nil,"Esc,F10"}, {"Save",@mnu_Save(),Nil,"F2"}, ;
   {"Save as",@mnu_Save(),.T.,"Shift-F2"}, ;
   {"Mark block",@mnu_F3(),Nil,"F3"}, {"Open file",@mnu_F4(),{11,40},"F4 >"}, ;
   {"Search&GoTo",@mnu_Sea_Goto(),{12,40},">"}, {"Change mode",@mnu_ChgMode(),Nil,"Ctrl-Z"}, ;
   {"Codepage",@mnu_CPages(),Nil,">"}, {"Syntax",@mnu_Syntax(),{16,40},"F8 >"}, ;
   {"Plugins",@mnu_Plugins(),Nil,"F11 >"}, {"Windows",@mnu_Windows(),{17,40},"F12 >"} }

STATIC aKeysMove := { K_UP, K_DOWN, K_LEFT, K_RIGHT, K_PGDN, K_PGUP, K_HOME, K_END, K_CTRL_PGUP, K_CTRL_PGDN }

STATIC aLangExten := { {"prg", ".prg"}, {"c", ".c.cpp"}, {"go", ".go"}, ;
   {"php",".php"}, {"js",".js"}, {"xml",".xml.fb2.htm.html"} }
STATIC cHelpName := "Help"
STATIC cLangMapCP, aLangMapUpper, aLangMapLower
STATIC aMenu_CB
STATIC aPlugins := {}
STATIC aLangs
STATIC lCase_Sea := .T.
STATIC cDopMode := ""

CLASS TEdit

   CLASS VAR aCPages    SHARED INIT { "RU866", "RU1251", "FR850", "FRWIN", "FRISO", "UTF8" }
   CLASS VAR aWindows   SHARED
   CLASS VAR nCurr      SHARED
   CLASS VAR cLauncher  SHARED INIT ""
   CLASS VAR lReadIni   SHARED INIT .F.
   CLASS VAR options    SHARED INIT { => }
   CLASS VAR aCmdHis    SHARED INIT {}
   CLASS VAR aSeaHis    SHARED INIT {}
   CLASS VAR aEditHis   SHARED INIT {}
   CLASS VAR aCBoards   SHARED
   CLASS VAR aHiliAttrs SHARED INIT { "W+/B", "W+/B", "GR+/B", "W/B" }
   CLASS VAR nDefMode   SHARED INIT 0

   DATA   aRect       INIT { 0,0,24,79 }
   DATA   y1, x1, y2, x2
   DATA   cColor      INIT "BG+/B"
   DATA   cColorSel   INIT "N/W"
   DATA   cColorPane  INIT "N/BG"
   DATA   cFileName   INIT ""
   DATA   cp, cpInit
   DATA   nxFirst, nyFirst
   DATA   aText
   DATA   nMode                       // Текущий режим (Edit, Vim, Cmd)
   DATA   nDopMode    INIT 0          // Нажатие некоторых клавиш в режиме Vim, требующее дополнительных клавиш (m, ', g, ...)
   DATA   cSyntaxType

   DATA   aUndo       INIT {}
   DATA   nUndo       INIT 0

   DATA   lBorder     INIT .F.
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
   DATA   lF3         INIT .F.
   DATA   nby1        INIT -1
   DATA   nby2        INIT -1
   DATA   nbx1, nbx2
   DATA   lTextOut    INIT .F.

   DATA   lShow
   DATA   lClose      INIT .F.
   DATA   cEol

   DATA   funSave
   DATA   oHili
   DATA   hBookMarks

   METHOD New( cText, cFileName, y1, x1, y2, x2, cColor )
   METHOD SetText( cText, cFileName )
   METHOD Edit()
   METHOD TextOut( n1, n2 )
   METHOD LineOut( nLine )
   METHOD onKey( nKeyExt )
   METHOD WriteTopPane( lFull )
   METHOD Search( cSea, lCase, lNext, ny, nx )
   METHOD GoTo( ny, nx, nSele )
   METHOD ToString( cEol )
   METHOD Save( cFileName )
   METHOD InsText( nLine, nPos, cText, lOver, lChgPos )
   METHOD DelText( nLine1, nPos1, nLine2, nPos2 )
   METHOD Undo( nLine1, nPos1, nLine2, nPos2, nOper, cText )
   METHOD Highlighter( oHili )
   METHOD OnExit()

ENDCLASS

METHOD New( cText, cFileName, y1, x1, y2, x2, cColor ) CLASS TEdit

   LOCAL i, cExt

   IF !::lReadIni
      edi_ReadIni( hb_DirBase() + "hbedit.ini" )
   ENDIF
   ::y1 := ::aRect[1] := Iif( y1==Nil, ::aRect[1], y1 )
   ::x1 := ::aRect[2] := Iif( x1==Nil, ::aRect[2], x1 )
   ::y2 := ::aRect[3] := Iif( y2==Nil, ::aRect[3], y2 )
   ::x2 := ::aRect[4] := Iif( x2==Nil, ::aRect[4], x2 )
   ::cColor := Iif( Empty(cColor), ::cColor, cColor )
   ::nxFirst := ::nyFirst := ::nRow := ::nCol := 1

   ::nMode := ::nDefMode
   ::cp := ::cpInit := hb_cdpSelect()
   
   ::SetText( cText, cFileName )

   ::hBookMarks := hb_Hash()

   IF ::aWindows == Nil
      ::aWindows := {}
   ENDIF
   Aadd( ::aWindows, Self )

   RETURN Self

METHOD SetText( cText, cFileName ) CLASS TEdit

   LOCAL i, arr

   IF !Empty( cFileName )
      IF cFileName == cHelpName
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
         IF ( i := Ascan( TEdit():aEditHis, {|a|a[1]==cFileName} ) ) > 0
            arr := TEdit():aEditHis[i]
            ADel( TEdit():aEditHis, i )
            hb_AIns( TEdit():aEditHis, 1, arr, .F. )
            hb_cdpSelect( ::cp := arr[2] )
            IF arr[3] >= ::y2 -::y1 - 1
               ::nyFirst := arr[3] - 4
               ::nRow := 4
            ELSE
               ::nRow := arr[3]
            ENDIF
            IF arr[4] > ::x2 -::x1 - 1
               ::nxFirst := arr[4] + ::x1 - ::x2 + 3
               ::nCol := arr[4] - ::nxFirst + 1
            ELSE
               ::nCol := arr[4] - 1
            ENDIF
         ELSE
            hb_AIns( TEdit():aEditHis, 1, {cFileName,,,}, Len(TEdit():aEditHis)<hb_hGetDef(TEdit():options,"edithismax",10) )
         ENDIF
      ENDIF
   ENDIF

   ::lUtf8 := ( Lower(::cp) == "utf8" )
   IF Empty( cText )
      ::aText := { "" }
      ::cEol := Chr(13) + Chr(10)
   ELSE
      ::aText := hb_ATokens( cText, Chr(10) )
      ::cEol := Iif( Right( ::aText[1],1 ) == Chr(13), Chr(13) + Chr(10), Chr(10) )
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
   IF Len( ::aText ) < ::nyFirst + ::nRow - 1
      ::nyFirst := ::nRow := 1
   ENDIF

   IF hb_hGetDef( TEdit():options, "syntax", .F. ) .AND. !Empty( cFileName )
      cExt := Lower( hb_fnameExt(cFileName) )
      FOR i := 1 TO Len(aLangExten)
         IF cExt $ aLangExten[i,2] .AND. hb_hHaskey(aLangs,aLangExten[i,1])
            mnu_SyntaxOn( Self, aLangExten[i,1] )
            EXIT
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

METHOD Edit() CLASS TEdit

   LOCAL cScBuf := Savescreen( 0, 0, 24, 79 )
   LOCAL i, nKeyExt

   hb_cdpSelect( ::cp )
   ::nCurr := Ascan( ::aWindows, {|o|o==Self} )

   SetCursor( SC_NONE )
   SetColor( ::cColor )
   ::y1 := ::aRect[1]; ::x1 := ::aRect[2]; ::y2 := ::aRect[3]; ::x2 := ::aRect[4]
   IF ::lBorder
      @ ::y1, ::x1, ::y2, ::x2 BOX "ЪДїіЩДАі "
      ::y1 ++; ::x1 ++; ::y2 --; ::x2 --
   ENDIF
   ::nTopName := Max( ::x2 - ::x1 - 44, 0 )
   IF ::lTopPane
      ::y1 ++
      DevPos( ::y1, ::x1 )
      ::WriteTopPane( .T. )
      SetColor( ::cColor )
   ENDIF
   Scroll( ::y1, ::x1, ::y2, ::x2 )

   ::TextOut()

   DevPos( ::nRow, ::nCol )
   ::lShow := .T.
   DO WHILE ::lShow
      SetCursor( Iif( ::lIns, SC_NORMAL, SC_SPECIAL1 ) )
      nKeyExt := Inkey( 0, HB_INKEY_ALL + HB_INKEY_EXT )
      SetCursor( SC_NONE )
      ::onKey( nKeyExt )
   ENDDO

   IF !Empty( ::cFileName ) .AND. ( i := Ascan( TEdit():aEditHis, {|a|a[1]==::cFileName} ) ) > 0
      TEdit():aEditHis[i,2] := ::cp
      TEdit():aEditHis[i,3] := ::nRow - ::y1 + ::nyFirst
      TEdit():aEditHis[i,4] := ::nCol - ::x1 + ::nxFirst
   ENDIF

   hb_cdpSelect( ::cpInit )
   Restscreen( 0, 0, 24, 79, cScBuf )

   IF ::lClose
      i := Ascan( ::aWindows, {|o|o==Self} )
      hb_ADel( ::aWindows, i, .T. )
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
   LOCAL aStru, i

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
            FOR i := 1 TO ::oHili:nItems
               IF aStru[i,2] >= ::nxFirst .AND. aStru[i,3] > 0 .AND. aStru[i,1] < ::nxFirst + nWidth
                  nbx1 := Max( ::nxFirst, aStru[i,1] )
                  nbx2 := Min( aStru[i,2], ::nxFirst + nWidth - 1 )
                  DevPos( ::y1 + nLine - 1, nbx1 -::nxFirst )
                  SetColor( ::aHiliAttrs[aStru[i,3]] )
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
   LOCAL s, lShift, lCtrl := .F., lNoDeselect := .F., lSkip := .F.

   ::nCol := nCol; ::nRow := nRow
   n := nRow - ::y1 + ::nyFirst
   ::lTextOut := .F.

   IF ::nDopMode > 0
      IF nKey == K_ESC
         ::nDopMode := 0
      ELSE
         nKey := edi_MapKey( Self, nKey ) 
         IF ::nDopMode == 109     // m
            edi_BookMarks( Self, nKey, .T. )
            ::nDopMode := 0
         ELSEIF ::nDopMode == 39  // '
            edi_BookMarks( Self, nKey, .F. )
            ::nDopMode := 0
         ELSEIF ::nDopMode == 49  // 1
            IF nKey >= 48 .AND. nKey <= 57
               cDopMode += Chr( nKey )
            ELSEIF nKey == 103    // g
               IF Right( cDopMode,1 ) == "g"
                  ::Goto( Val( cDopMode ) )
                  ::nDopMode := 0
               ELSE
                  cDopMode += Chr( nKey )
               ENDIF
            ELSE
               ::nDopMode := 0
            ENDIF
         ELSEIF ::nDopMode == 103  // g
            IF nKey == 103    // g
               ::Goto( 1 )
            ENDIF
            ::nDopMode := 0
         ENDIF
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
            ::nby1 := ::nRow - ::y1 + ::nyFirst
            ::nbx1 := ::nCol - ::x1 + ::nxFirst
            ::lShiftKey := .T.
         ENDIF
      ELSE
         ::lShiftKey := .F.
      ENDIF
      IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
         IF nKey == K_ALT_F7
            mnu_SeaNext( Self, .F. )

         ELSEIF nKey == K_ALT_F8
            mnu_GoTo( Self )
            ::lTextOut := .T.

         ELSEIF nKey == K_ALT_M
            ::nDopMode := 109
            cDopMode := "m"
            
         ELSEIF nKey == K_ALT_QUOTE
            ::nDopMode := 39
            cDopMode := "'"
            
         ENDIF
      ENDIF
      IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0

         lCtrl := .T.
         IF nKey == K_CTRL_INS .OR. nKey == 3       // Ctrl-Ins or Ctrl-c
            IF !Empty( s := Text2cb( Self ) )
               hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
               TEdit():aCBoards[1,2] := Nil
            ENDIF

         ELSEIF nKey == 22                          // Ctrl-v
            IF !::lReadOnly
               cb2Text( Self, .T. )
            ENDIF

         ELSEIF nKey == K_CTRL_Z .AND. hb_keyVal( nKeyExt ) == 90
            mnu_ChgMode( Self )

         ELSEIF nKey == K_CTRL_Y
            IF !::lReadOnly .AND. n > 0 .AND. n <= Len( ::aText )
               ::DelText( n, 0, n+1, 0 )
            ENDIF

         ELSEIF nKey == K_CTRL_A
            IF !::lF3
               ::nby1 := ::nbx1 := 1
               ::nby2 := Len( ::aText )
               ::nbx2 := cp_Len( ::lUtf8, ::aText[Len(::aText)] )
               ::lF3 := .T.
            ENDIF

         ELSEIF nKey == K_CTRL_TAB
            IF ::lCtrlTab
               ::lShow := .F.
               ::nCurr ++
            ENDIF

         ELSEIF nKey == K_CTRL_PGUP
            ::lTextOut := (::nyFirst>1 .OR. ::nxFirst>1)
            ::nxFirst := ::nyFirst := 1
            DevPos( ::y1, ::x1 )

         ELSEIF nKey == K_CTRL_PGDN
            IF Len( ::aText ) > ::y2-::y1+1
               ::nxFirst := 1
               ::nyFirst := Len( ::aText ) - (::y2-::y1)
               ::lTextOut := .T.
               DevPos( ::y2, Min( nCol,cp_Len( ::lUtf8,ATail(::aText))+1 ) )
            ELSE
               DevPos( Len(::aText)+::y1-1, Min( nCol,cp_Len( ::lUtf8,ATail(::aText))+1 ) )
            ENDIF

         ELSEIF nKey == K_CTRL_D
            mnu_Exit( Self )

         ELSEIF nKey == K_CTRL_RIGHT .AND. hb_keyVal( nKeyExt ) == 16
            edi_NextWord( Self )

         ELSEIF nKey == K_CTRL_LEFT .AND. hb_keyVal( nKeyExt ) == 15
            edi_PrevWord( Self )

         ENDIF
      ELSE
         IF ( nKey >= K_SPACE .AND. nKey <= 255 ) .OR. ( ::lUtf8 .AND. nKey > 3000 )
            IF !::lReadOnly
               IF ::nby1 >= 0 .AND. ::nby2 >= 0
                  nKey := edi_MapKey( Self, nKey )
                  IF nKey == 85   // U  Convert to upper case
                     edi_ConvertCase( Self, .T. )
                     lNoDeselect := .T.
                  ELSEIF nKey == 117   // u Convert to lower case
                     edi_ConvertCase( Self, .F. )
                     lNoDeselect := .T.
                  ELSEIF nKey == 119   // w Move to the next word
                     edi_NextWord( Self )
                     nKey := K_RIGHT
                  ELSEIF nKey == 101   // e Move to the end of word
                     edi_NextWord( Self, .T. )
                     nKey := K_RIGHT
                  ELSEIF nKey == 98    // b Move to the previous word
                     edi_PrevWord( Self )
                     nKey := K_LEFT
                  ELSEIF nKey == 100   // d Deletes selection
                     cbDele( Self )
                  ELSEIF nKey == 121   // y Copy to clipboard
                     IF !Empty( s := Text2cb( Self ) )
                        hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
                        TEdit():aCBoards[1,2] := Nil
                     ENDIF
                  ELSEIF nKey == 62    // > Shift lines right
                     edi_Indent( Self, .T. )
                     lNoDeselect := .T.
                  ELSEIF nKey == 60    // > Shift lines left
                     edi_Indent( Self, .F. )
                     lNoDeselect := .T.
                  ENDIF
               ELSEIF ::nMode == 1
                  nKey := edi_MapKey( Self, nKey )
                  IF nKey == 104   // h Move left
                     edi_GoLeft( Self )
                  ELSEIF nKey == 108   // l Move right
                     edi_GoRight( Self )
                  ELSEIF nKey == 107   // k Move up
                     edi_GoUp( Self )
                  ELSEIF nKey == 106   // j Move down
                     edi_GoDown( Self )
                  ELSEIF nKey == 119   // w Move to the next word
                     edi_NextWord( Self )
                  ELSEIF nKey == 101   // e Move to the end of word
                     edi_NextWord( Self, .T. )
                  ELSEIF nKey == 98    // b Move to the previous word
                     edi_PrevWord( Self )
                  ELSEIF nKey == 118   // v Start selection
                     mnu_F3( Self )
                     nKey := K_RIGHT
                  ELSEIF nKey == 112   // p Insert clipboard after current coloumn
                     IF !::lReadOnly
                        DevPos( ::nRow, ++::nCol )
                        cb2Text( Self, .T. )
                     ENDIF
                  ELSEIF nKey == 80    // P Insert clipboard
                     IF !::lReadOnly
                        cb2Text( Self, .T. )
                     ENDIF
                  ELSEIF nKey == 105   // i - to edit mode
                     mnu_ChgMode( Self, .T. )
                  ELSEIF nKey == 109 .OR. nKey == 39  // m - set bookmark, ' - goto bookmark
                     ::nDopMode := nKey
                     cDopMode := Chr( nKey )
                  ELSEIF nKey == 103   // g
                     ::nDopMode := 103
                     cDopMode := Chr( nKey )
                  ELSEIF nKey >= 49 .AND. nKey <= 57  // 1...9
                     ::nDopMode := 49
                     cDopMode := Chr( nKey )
                  ENDIF
               ELSE
                  IF ( i := (nCol - ::x1 + ::nxFirst - cp_Len(::lUtf8,::aText[n])) ) > 0
                     ::nCol -= (i-1)
                     ::InsText( n, cp_Len(::lUtf8,::aText[n])+1, Space( i-1 ) + cp_Chr(::lUtf8,nKey), !::lIns, .T. )
                  ELSE
                     ::InsText( n, nCol-::x1+::nxFirst, cp_Chr(::lUtf8,nKey), !::lIns, .T. )
                  ENDIF
               ENDIF
            ENDIF

         ELSEIF nKey == K_ENTER
            IF !::lReadOnly .AND. ::nMode == 0
               nCol := nCol - ::x1 + ::nxFirst
               s := ""
               IF hb_hGetDef( TEdit():options, "autoindent", .F. )
                  i := 0
                  DO WHILE cp_Substr( ::lUtf8, ::aText[n], i+1, 1 ) == " "; i++; ENDDO
                  IF i > 0
                     s := Space( i )
                  ENDIF
               ENDIF
               ::InsText( n, nCol, Chr(10) + s, .F., .T. )
            ENDIF

         ELSEIF nKey == K_DEL
            IF !::lReadOnly
               IF ::nby1 >= 0 .AND. ::nby2 >= 0
                  IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0 .AND. !Empty( s := Text2cb( Self ) )
                     hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
                     TEdit():aCBoards[1,2] := Nil
                  ENDIF
                  cbDele( Self )
               ELSE
                  ::DelText( n, nCol-::x1+::nxFirst, n, nCol-::x1+::nxFirst )
               ENDIF
               DevPos( nRow, nCol )
            ENDIF

         ELSEIF nKey == K_BS
            IF !::lReadOnly .AND. ::nMode == 0
               IF ::nby1 >= 0 .AND. ::nby2 >= 0
                  IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0 .AND. !Empty( s := Text2cb( Self ) )
                     hb_gtInfo( HB_GTI_CLIPBOARDDATA, TEdit():aCBoards[1,1] := s )
                     TEdit():aCBoards[1,2] := Nil
                  ENDIF
                  cbDele( Self )
               ELSE
                  IF nCol == ::x1
                     IF n > 1
                        edi_GoUp( Self )
                        edi_GoEnd( Self )
                        ::DelText( n-1, Col()-::x1+::nxFirst, n-1, Col()-::x1+::nxFirst )
                     ENDIF
                  ELSE
                     ::DelText( n, nCol-::x1+::nxFirst-1, n, nCol-::x1+::nxFirst-1 )
                     DevPos( nRow, ::nCol := (nCol-1) )
                  ENDIF
               ENDIF
            ENDIF

         ELSEIF nKey == K_TAB
            IF !::lReadOnly
            ENDIF

         ELSEIF nKey == K_INS
            IF hb_BitAnd( nKeyExt, SHIFT_PRESSED ) != 0
               IF !::lReadOnly
                  cb2Text( Self, .T. )
               ENDIF
            ELSE
               ::lIns := !::lIns
            ENDIF

         ELSEIF nKey == K_UP
            edi_GoUp( Self )

         ELSEIF nKey == K_DOWN
            edi_GoDown( Self )

         ELSEIF nKey == K_LEFT
            edi_GoLeft( Self )

         ELSEIF nKey == K_RIGHT
            edi_GoRight( Self )

         ELSEIF nKey == K_HOME
            IF ::nxFirst > 1
               ::nxFirst := 1
               ::lTextOut := .T.
            ENDIF
            DevPos( Row(), ::x1 )

         ELSEIF nKey == K_END
            edi_GoEnd( Self )

         ELSEIF nKey == K_PGUP
            i := 1
            DO WHILE i <= ::y2 - ::y1
               IF ( nRow := Row() ) == ::y1
                  IF ::nyFirst > 1
                     ::nyFirst --
                     ::lTextOut := .T.
                  ENDIF
               ELSE
                  DevPos( nRow-1, Col() )
               ENDIF
               i ++
            ENDDO

         ELSEIF nKey == K_PGDN
            i := 1
            DO WHILE ( nRow := Row() ) - ::y1 + ::nyFirst < Len(::aText) .AND. i <= ::y2 - ::y1
               IF nRow < ::y2
                  DevPos( nRow+1, Col() )
               ELSE
                  ::nyFirst ++
                  ::lTextOut := .T.
               ENDIF
               i ++
            ENDDO

         ELSEIF nKey == K_LBUTTONDOWN
            IF ::nDopMode == 0
               nCol := MCol()
               nRow := MRow()
               IF ::lTopPane .AND. nRow == ::y1-1 .AND. nCol < 8
                  FMenu( Self, aMenuMain )
                  ::lTextOut := .T.
               ELSEIF nRow >= ::y1 .AND. nRow <= ::y2 .AND. nCol >= ::x1 .AND. nCol <= ::x2
                  IF nRow - ::y1 + ::nyFirst > Len(::aText)
                     nRow := Len(::aText) - ::nyFirst + ::y1
                  ENDIF
                  DevPos( nRow, nCol )
               ENDIF
            ENDIF

         ELSEIF nKey == K_ALT_TAB
            ::lShow := .F.
            ::nCurr --

         ELSEIF nKey == K_F1
            mnu_Help( Self )
            ::lTextOut := .T.
            DevPos( ::nRow, ::nCol )

         ELSEIF nKey == K_F2
            ::Save()

         ELSEIF nKey == K_SH_F2
            mnu_Save( Self, .T. )

         ELSEIF nKey == K_F3
            mnu_F3( Self )
            nKey := K_RIGHT

         ELSEIF nKey == K_F4
            mnu_F4( Self, {7, 22} )
            ::lTextOut := .T.

         ELSEIF nKey == K_F7
            mnu_Search( Self )
            ::lTextOut := .T.

         ELSEIF nKey == K_F8
            mnu_Syntax( Self, {8, 32} )
            ::lTextOut := .T.

         ELSEIF nKey == K_F9
            FMenu( Self, aMenuMain )
            ::lTextOut := .T.
            DevPos( ::nRow, ::nCol )

         ELSEIF nKey == K_F10 .OR. nKey == K_ESC
            IF ::nMode == 1 .AND. nKey == K_ESC
               mnu_ChgMode( Self, .T. )
            ELSE
               mnu_Exit( Self )
            ENDIF

         ELSEIF nKey == K_F11
            mnu_Plugins( Self )
            ::lTextOut := .T.

         ELSEIF nKey == K_F12
            mnu_Windows( Self, {7, 22} )
            ::lTextOut := .T.

         ELSEIF nKey == K_SH_F7
            mnu_SeaNext( Self, .T. )

         ELSEIF nKey == K_SH_F8
            mnu_cPages( Self )
            ::lTextOut := .T.
         ENDIF
      ENDIF

      IF ::lF3 .AND. nKey != K_MOUSEMOVE .AND. Ascan( aKeysMove, nKey ) == 0
         ::lF3 := .F.
      ENDIF
      IF (::lF3 .OR. lShift)
         IF !( lCtrl .AND. nKey == K_CTRL_A)
            ::nby2 := Row() - ::y1 + ::nyFirst
            ::nbx2 := Col() - ::x1 + ::nxFirst
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
   ::WriteTopPane()

   RETURN Nil

METHOD WriteTopPane( lFull ) CLASS TEdit

   LOCAL y := ::y1 - 1, nCol := ::nCol, nRow := ::nRow
   LOCAL cLen := Ltrim(Str(Len(::aText))), nchars := Len(cLen)

   IF ::lTopPane
      DispBegin()
      SetColor( ::cColorPane )
      Scroll( y, ::x1, y, ::x1 + 7 )
      DevPos( y, ::x1 )
      DevOut( Iif( !Empty(cDopMode), cDopMode, "F9-menu" ) )
      IF !Empty( lFull )
         Scroll( y, ::x1 + 8 , y, ::x2 )
         DevPos( y, ::x1 + 8 )
         DevOut( NameShortcut( ::cFileName, ::nTopName, '~' ) )
      ELSE
         Scroll( y, ::x1 + 8 + ::nTopName, y, ::x2 )
      ENDIF
      DevPos( y, ::x1 + 8 + ::nTopName + 2 )
      DevOut( Iif( ::lUpdated, "* ", "  " ) + Lower( ::cp ) )
      DevPos( y, ::x1 + 8 + ::nTopName + 10 )
      DevOut( PAdl(Ltrim(Str(nRow-::y1+::nyFirst)),nchars) + "/" + cLen )
      DevPos( y, ::x1 + 8 + ::nTopName + 10 + nchars*2 + 3 )
      DevOut( "[" + Ltrim(Str(nCol-::x1+::nxFirst)) + "]" )     
      SetColor( "W+/N" )
      DevPos( y, ::x2-3 )
      IF ::lF3 .OR. (::nby1 >= 0 .AND. ::nby2 >= 0)
         DevOut( "Sele" )
      ELSE
         DevOut( Iif( ::nMode == 0, "Edit", Iif( ::nMode == 1, " Vim", " Cmd" ) ) )
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

METHOD GoTo( ny, nx, nSele ) CLASS TEdit

   LOCAL lTextOut := .F., nRowOld

   IF nx == Nil; nx := 1; ENDIF
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

   SetColor( ::cColor )
   IF lTextOut
      ::TextOut()
   ELSEIF nSele != Nil .AND. nSele > 0
      IF ( nRowOld := (::nRow - ::y1 + 1) ) > 0
         ::LineOut( nRowOld )
      ENDIF
      ::LineOut( ny - ::nyFirst + 1 )
   ENDIF
   ::WriteTopPane()
   DevPos( ::nRow := (ny - ::nyFirst + ::y1), ::nCol := (nx - ::nxFirst + ::x1) )

   RETURN Nil

METHOD ToString( cEol ) CLASS TEdit

   LOCAL i, s := ""

   IF Empty( cEol )
      cEol := ::cEol
   ENDIF
   FOR i := 1 TO Len( ::aText )
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
         cFileName := edi_CurPath() + cFileName
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

METHOD InsText( nLine, nPos, cText, lOver, lChgPos ) CLASS TEdit

   LOCAL arr, i

   IF Chr(10) $ cText
      arr := hb_ATokens( cText, Chr(10) )
      cText := cp_Substr( ::lUtf8, ::aText[nLine], nPos )
      ::aText[nLine] := cp_Left( ::lUtf8, ::aText[nLine], nPos-1 ) + arr[1]
      FOR i := 2 TO Len(arr)-1
         hb_AIns( ::aText, nLine+i-1, arr[i], .T. )
      NEXT
      hb_AIns( ::aText, nLine+i-1, arr[i] + cText, .T. )
      ::lTextOut := .T.
      IF lChgPos
         nLine := nLine + i - 1
         IF nLine - ::nyFirst + 1 > ::y2 - ::y1 - 1
            ::nyFirst := nLine - 3
         ENDIF
         nPos := cp_Len( ::lUtf8, arr[i] ) + 1
         IF nPos - ::nxFirst + 1 > ::x2 - ::x1 - 1
            ::nxFirst := nPos - 3
         ELSEIF nPos < ::nxFirst
            nPos := 1
         ENDIF
         DevPos( ::nRow := nLine - ::nyFirst + ::y1, ::nCol := nPos - ::nxFirst + ::x1 )
      ENDIF
   ELSE
      i := cp_Len( ::lUtf8, cText )
      ::aText[nLine] := cp_Left( ::lUtf8, ::aText[nLine], nPos-1 ) + cText + ;
         cp_Substr( ::lUtf8, ::aText[nLine], nPos + Iif(lOver,i,0) )
      IF lChgPos
         ::nCol += i
      ENDIF
      IF ::nCol > ::x2
         IF lChgPos
            ::nxFirst += ::nCol
         ENDIF
         ::lTextOut := .T.
      ELSE
         ::LineOut( ::nRow - ::y1 + 1 )
         IF lChgPos
            DevPos( ::nRow, ::nCol )
         ENDIF
      ENDIF
   ENDIF

   ::Undo( nLine )
   ::lUpdated := .T.

   RETURN Nil

METHOD DelText( nLine1, nPos1, nLine2, nPos2 ) CLASS TEdit

   LOCAL i, n, ncou := 0
   
   IF nLine1 == nLine2
      IF nPos1 == nPos2 .AND. nPos1 > cp_Len( ::lUtf8, ::aText[nLine1] )
         IF nLine1 < Len( ::aText )
            ::aText[nLine1] += ::aText[nLine1+1]
            hb_ADel( ::aText, nLine1+1, .T. )
            ::lTextOut := .T.
         ENDIF
      ELSE
         ::aText[nLine1] := cp_Left( ::lUtf8, ::aText[nLine1], nPos1-1 ) + ;
            cp_Substr( ::lUtf8, ::aText[nLine1], nPos2+1 )
         ::LineOut( nLine1 -::nyFirst + 1 )
      ENDIF
   ELSE
      IF nPos1 > 1
         ::aText[nLine1] := cp_Left( ::lUtf8, ::aText[nLine1], nPos1-1 )
         n := nLine1 + 1
      ELSE
         ADel( ::aText, nLine1 )
         n := nLine1
         ncou ++
      ENDIF
      FOR i := nLine1+1 TO nLine2-1
         ADel( ::aText, n )
         ncou ++
      NEXT
      ::aText := ASize( ::aText, Len(::aText) - ncou )
      IF nPos2 > 0
         ::aText[nLine1+1] := cp_Substr( ::lUtf8, ::aText[nLine1+1], nPos2 )
      ENDIF
      IF ( i := nLine1 - ::nyFirst + 1 ) > 0 .AND. i < (::y2-::y1+1)
         DevPos( ::nRow := (nLine1-::nyFirst-::y1+2), ::nCol := (nPos1-::nxFirst+1-::x1) )
      ELSE
         ::nyFirst := nLine1
         DevPos( ::nRow := 1, ::nCol := nPos1 )
      ENDIF
      ::lTextOut := .T.
   ENDIF
   
   ::Undo( nLine1 )
   ::lUpdated := .T.
   
   RETURN Nil

METHOD Undo( nLine1, nPos1, nLine2, nPos2, nOper, cText ) CLASS TEdit

   IF PCount() == 0
      IF ::nUndo > 0
         IF ( nOper := ::aUndo[::nUndo, UNDO_OPER] ) == UNDO_OP_INS
         ELSEIF nOper == UNDO_OP_OVER
         ELSEIF nOper == UNDO_OP_DEL
         ELSEIF nOper == UNDO_OP_SHIFT
         ENDIF
      ENDIF
   ELSEIF nOper == UNDO_OP_INS
   ELSEIF nOper == UNDO_OP_OVER
   ELSEIF nOper == UNDO_OP_DEL
   ELSEIF nOper == UNDO_OP_SHIFT
   ENDIF

   IF !Empty( ::oHili )
      ::oHili:UpdSource( nLine1 )
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

   LOCAL i, s := ""

   IF !Empty( TEdit():aSeaHis )
      s += "[SEARCH]" + Chr(13) + Chr(10)
      FOR i := 1 TO Len( TEdit():aSeaHis )
         s += "h" + Ltrim(Str(i)) + "=" + TEdit():aSeaHis[i] + Chr(13) + Chr(10)
      NEXT
   ENDIF

   IF !Empty( TEdit():aCmdHis )
      s += Chr(13) + Chr(10) + "[COMMANDS]" + Chr(13) + Chr(10)
      FOR i := 1 TO Len( TEdit():aCmdHis )
         s += "h" + Ltrim(Str(i)) + "=" + TEdit():aCmdHis[i] + Chr(13) + Chr(10)
      NEXT
   ENDIF

   IF !Empty( TEdit():aEditHis )
      s += Chr(13) + Chr(10) + "[EDIT]" + Chr(13) + Chr(10)
      FOR i := 1 TO Len( TEdit():aEditHis )
         s += "h" + Ltrim(Str(i)) + "=" + TEdit():aEditHis[i,2] + "," + ;
            Ltrim(Str(TEdit():aEditHis[i,3])) + "," + Ltrim(Str(TEdit():aEditHis[i,4])) + "," + ;
            TEdit():aEditHis[i,1] + Chr(13) + Chr(10)
      NEXT
   ENDIF

   hb_MemoWrit( hb_DirBase() + "hbedit.his", s )

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
      IF ( i := FMenu( oEdit, aMenu_CB ) ) != Nil
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
      oEdit:DelText( nby1, nbx1, nby2, nbx2-1 )
      oEdit:nby1 := oEdit:nby2 := -1
   ENDIF
   RETURN Nil

FUNCTION edi_ReadIni( xIni )

   LOCAL hIni, aIni, nSect, aSect, arr, arr1, s, n, i, cTemp
   LOCAL lIncSea := .F., lAutoIndent := .F., lSyntax := .T., ncmdhis := 20, nseahis := 20, nedithis := 20
   LOCAL hHili

   TEdit():lReadIni := .T.
   hIni := Iif( Valtype( xIni ) == "C", hb_iniRead( xIni ), xIni )

   SetBlink( .F. )
   aLangs := hb_Hash()

   IF !Empty( hIni )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "OPTIONS"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, "defmode" ) .AND. !Empty( cTemp := aSect[ "defmode" ] )
                  TEdit():nDefMode := Iif( (n := Val(cTemp)) < 2 .AND. n >= 0, n, 0 )
               ENDIF
               IF hb_hHaskey( aSect, "incsearch" ) .AND. !Empty( cTemp := aSect[ "incsearch" ] )
                  lIncSea := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, "autoindent" ) .AND. !Empty( cTemp := aSect[ "autoindent" ] )
                  lAutoIndent := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, "syntax" ) .AND. !Empty( cTemp := aSect[ "syntax" ] )
                  lSyntax := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, "cmdhismax" ) .AND. !Empty( cTemp := aSect[ "cmdhismax" ] )
                  ncmdhis :=  Val(cTemp)
               ENDIF
               IF hb_hHaskey( aSect, "seahismax" ) .AND. !Empty( cTemp := aSect[ "seahismax" ] )
                  nseahis :=  Val(cTemp)
               ENDIF
               IF hb_hHaskey( aSect, "edithismax" ) .AND. !Empty( cTemp := aSect[ "edithismax" ] )
                  nedithis :=  Val(cTemp)
               ENDIF
               IF hb_hHaskey( aSect, "langmap_cp" ) .AND. !Empty( cTemp := aSect[ "langmap_cp" ] )
                  IF Ascan( TEdit():aCPages, cTemp ) > 0
                     cLangMapCP := cTemp
                  ENDIF
               ENDIF
               IF hb_hHaskey( aSect, "langmap_upper" ) .AND. !Empty( cTemp := aSect[ "langmap_upper" ] )
                  aLangMapUpper := hb_aTokens( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, "langmap_lower" ) .AND. !Empty( cTemp := aSect[ "langmap_lower" ] )
                  aLangMapLower := hb_aTokens( cTemp )
               ENDIF
            ENDIF
         ELSEIF Upper(aIni[nSect]) == "KEYBOARD"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
            ENDIF
         ELSEIF Upper(aIni[nSect]) == "PLUGINS"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := hb_hKeys( aSect )
               aPlugins := {}
               FOR i := 1 TO Len( arr )
                  s := aSect[ arr[i] ]
                  IF ( n := At( ",", s ) ) > 0
                     cTemp := AllTrim( Left( s,n-1 ) )
                     IF File( hb_DirBase() + "plugins" + hb_ps() + cTemp )
                        s := Substr( s, n+1 )
                        IF ( n := At( ",", s ) ) > 0
                           Aadd( aPlugins, { cTemp, Substr( s, n+1 ), AllTrim( Left( s,n-1 ) ), Nil } )
                        ENDIF
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ELSEIF Upper(aIni[nSect]) == "HILIGHT"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, "commands" ) .AND. !Empty( cTemp := aSect[ "commands" ] )
                  TEdit():aHiliAttrs[1] := cTemp
               ENDIF
               IF hb_hHaskey( aSect, "funcs" ) .AND. !Empty( cTemp := aSect[ "funcs" ] )
                  TEdit():aHiliAttrs[2] := cTemp
               ENDIF
               IF hb_hHaskey( aSect, "quotes" ) .AND. !Empty( cTemp := aSect[ "quotes" ] )
                  TEdit():aHiliAttrs[3] := cTemp
               ENDIF
               IF hb_hHaskey( aSect, "comments" ) .AND. !Empty( cTemp := aSect[ "comments" ] )
                  TEdit():aHiliAttrs[4] := cTemp
               ENDIF
            ENDIF
         ELSEIF Left( Upper(aIni[nSect]),5 ) == "LANG_"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               hHili := aLangs[ Lower(Substr(aIni[nSect],6)) ] := hb_hash()
               IF hb_hHaskey( aSect, "commands" ) .AND. !Empty( cTemp := aSect[ "commands" ] )
                  hHili["commands"] := cTemp
               ENDIF
               IF hb_hHaskey( aSect, "funcs" ) .AND. !Empty( cTemp := aSect[ "funcs" ] )
                  hHili["funcs"] := cTemp
               ENDIF
               IF hb_hHaskey( aSect, "scomm" ) .AND. !Empty( cTemp := aSect[ "scomm" ] )
                  hHili["scomm"] := cTemp
               ENDIF
               IF hb_hHaskey( aSect, "mcomm" ) .AND. !Empty( cTemp := aSect[ "mcomm" ] )
                  hHili["mcomm"] := cTemp
               ENDIF
               IF hb_hHaskey( aSect, "case" ) .AND. !Empty( cTemp := aSect[ "case" ] )
                  hHili["case"] := ( Lower(cTemp) == "on" )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   TEdit():options["incsearch"]  := lIncSea
   TEdit():options["cmdhismax"]  := ncmdhis
   TEdit():options["seahismax"]  := nseahis
   TEdit():options["edithismax"]  := nedithis
   TEdit():options["autoindent"] := lAutoIndent
   TEdit():options["syntax"] := lSyntax

   TEdit():aCBoards := Array( MAX_CBOARDS,2 )
   FOR i := 1 TO MAX_CBOARDS
      TEdit():aCBoards[i,1] := TEdit():aCBoards[i,2] := ""
   NEXT

   hIni := hb_iniRead( hb_DirBase() + "hbedit.his" )
   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      IF hb_hHaskey( hIni, "SEARCH" ) .AND. !Empty( aSect := hIni[ "SEARCH" ] )
         arr := hb_hKeys( aSect )
         arr := ASort( arr )
         TEdit():aSeaHis := Array( Len(arr) )
         FOR i := 1 TO Len(arr)
            TEdit():aSeaHis[i] := aSect[ arr[i] ]
         NEXT
      ENDIF
      IF hb_hHaskey( hIni, "COMMANDS" ) .AND. !Empty( aSect := hIni[ "COMMANDS" ] )
         arr := hb_hKeys( aSect )
         arr := ASort( arr )
         TEdit():aCmdHis := Array( Len(arr) )
         FOR i := 1 TO Len(arr)
            TEdit():aCmdHis[i] := aSect[ arr[i] ]
         NEXT
      ENDIF
      IF hb_hHaskey( hIni, "EDIT" ) .AND. !Empty( aSect := hIni[ "EDIT" ] )
         arr := hb_hKeys( aSect )
         arr := ASort( arr )
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

   RETURN Nil

FUNCTION mnu_Help( oEdit )

   LOCAL oHelp := TEdit():New( MemoRead(hb_DirBase() + "hbedit.help"), cHelpName, ;
         oEdit:aRect[1], oEdit:aRect[2], oEdit:aRect[3], oEdit:aRect[4] )

   oHelp:lReadOnly := .T.
   oHelp:lCtrlTab  := .F.
   oHelp:Edit()

   RETURN Nil

FUNCTION mnu_Exit( oEdit )

   LOCAL nRes := 2

   IF oEdit:lUpdated
      nRes := Alert( "File has been modified. Save?", { "Yes", "No", "Cancel" } )
   ENDIF
   IF nRes == 1 .OR. nRes == 2
      IF nRes == 1
         oEdit:Save()
      ENDIF
      oEdit:lShow := .F.
      oEdit:lClose := .T.
   ENDIF
   RETURN Nil

FUNCTION mnu_CPages( oEdit )

   LOCAL iRes
   STATIC aMenu_cps := { {"cp866",,1}, {"cp1251",,2}, {"fr850",,3}, {"frwin",,4}, {"friso",,5}, {"utf8",,6} }

   IF !Empty( iRes := FMenu( oEdit, aMenu_cps, 13, 40 ) )
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
      IF ( i := FMenu( oEdit, aMenu_CB ) ) != Nil
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

   LOCAL aMenu := { {"New file",@mnu_NewWin(),Nil}, {"Open file",@mnu_OpenFile(),Nil} }, i

   FOR i := 1 TO Len( oEdit:aEditHis )
      AAdd( aMenu, {NameShortcut(oEdit:aEditHis[i,1],36,'~'),@mnu_OpenRecent(),i} )
   NEXT

   FMenu( oEdit, aMenu, aXY[1], aXY[2] )

   RETURN Nil

FUNCTION mnu_OpenRecent( oEdit, n )

   LOCAL cFileName := oEdit:aEditHis[n,1] 

   RETURN mnu_NewWin( oEdit, Memoread(cFileName), cFileName )

FUNCTION mnu_NewWin( oEdit, cText, cFileName )

   LOCAL oNew

   IF ( !Empty( oEdit:aText ) .AND. !Empty( oEdit:aText[1] ) ) ;
         .OR. oEdit:lUpdated .OR. !Empty( oEdit:cFilename )
      hb_cdpSelect( oEdit:cpInit )
      oNew := TEdit():New( cText, cFileName, oEdit:aRect[1], oEdit:aRect[2], oEdit:aRect[3], oEdit:aRect[4])
      oNew:funSave := oEdit:funSave
      hb_cdpSelect( oEdit:cp )
      oEdit:lShow := .F.
      oEdit:nCurr := Len( oEdit:aWindows )
   ELSE
      oEdit:SetText( cText, cFileName )
      DevPos( oEdit:nRow, oEdit:nCol )
   ENDIF

   RETURN Nil

FUNCTION mnu_OpenFile( oEdit )

   RETURN Nil

FUNCTION mnu_Sea_goto( oEdit, aXY )

   LOCAL aMenu := { {"Search",@mnu_Search(),Nil,"F7"}, {"Next",@mnu_SeaNext(),.T.,"Shift-F7"}, ;
      {"Previous",@mnu_SeaNext(),.F.,"Alt-F7"}, {"Go to",@mnu_GoTo(),Nil,"Alt-F8"} }

   FMenu( oEdit, aMenu, aXY[1], aXY[2] )

   RETURN Nil

FUNCTION mnu_Search( oEdit )

   LOCAL oldc := SetColor( "N/W,N/W,,,N/W" ), lRes, i
   LOCAL aGets := { {11,22,0,"",33,"W+/BG","W+/BG"}, ;
      {11,55,2,"[^]",3,"N/W","W+/RB",{||mnu_SeaHist(oEdit,aGets[1])}}, ;
      {12,23,1,.T.,1}, {12,43,1,.F.,1}, ;
      {14,25,2,"[Search]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}}, ;
      {14,40,2,"[Cancel]",10,"N/W","W+/BG",{||__KeyBoard(Chr(K_ENTER))}} }
   LOCAL cSearch, lCase, lBack := .F.
   LOCAL ny := oEdit:nRow - oEdit:y1 + oEdit:nyFirst, nx := oEdit:nCol - oEdit:x1 + oEdit:nxFirst  

   hb_cdpSelect( "RU866" )
   @ 09, 20, 15, 60 BOX "ЪДїіЩДАі "
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Search for"
   @ 12, 22 SAY "[ ] Case sensitive"
   @ 12, 42 SAY "[ ] Backward"

   IF !Empty( TEdit():aSeaHis )
      //aGets[1,4] := TEdit():aSeaHis[1]
      aGets[3,4] := lCase_Sea
   ENDIF
   lRes := edi_READ( oEdit, aGets )

   IF lRes
      cSearch := Trim( aGets[1,4] )
      lCase := aGets[3,4]
      lBack := aGets[4,4]
      IF ( i := Ascan( TEdit():aSeaHis, {|cs|cs==cSearch} ) ) > 0
         ADel( TEdit():aSeaHis, i )
         hb_AIns( TEdit():aSeaHis, 1, cSearch, .F. )
      ELSE
         hb_AIns( TEdit():aSeaHis, 1, cSearch, Len(TEdit():aSeaHis)<hb_hGetDef(TEdit():options,"seahismax",10) )
      ENDIF
      IF oEdit:Search( cSearch, lCase_Sea := lCase, !lBack, @ny, @nx )
         oEdit:GoTo( ny, nx, 0 )
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
         aMenu[i] := { TEdit():aSeaHis[i], Nil, i }
      NEXT
      bufc := SaveScreen( 12, 22, 12 + Min(6,Len(aMenu)+1), 55 )
      IF !Empty( i := FMenu( oEdit, aMenu, 12, 22, 12 + Min(6,Len(aMenu)+1), 55 ) )
         aGet[4] := TEdit():aSeaHis[i]
         ShowGetItem( aGet, .F., oEdit:lUtf8 )
      ENDIF
      RestScreen( 12, 22, 12 + Min(6,Len(aMenu)+1), 55, bufc )
      __KeyBoard(Chr(K_UP))
   ENDIF

   RETURN Nil

FUNCTION mnu_SeaNext( oEdit, lNext )

   LOCAL ny := oEdit:nRow - oEdit:y1 + oEdit:nyFirst, nx := oEdit:nCol - oEdit:x1 + oEdit:nxFirst

   IF !Empty( TEdit():aSeaHis ) .AND. oEdit:Search( TEdit():aSeaHis[1], lCase_Sea, lNext, @ny, @nx )
      oEdit:GoTo( ny, nx, 0 )
   ENDIF

   RETURN Nil

FUNCTION mnu_GoTo( oEdit )

   LOCAL oldc := SetColor( "N/W,W+/BG" )
   LOCAL aGets := { {11,32,0,"",16} }, ny, lRes

   hb_cdpSelect( "RU866" )
   @ 09, 30, 12, 50 BOX "ЪДїіЩДАі "
   hb_cdpSelect( oEdit:cp )

   @ 10,32 SAY "Go to position"
   SetColor( "W+/BG" )

   lRes := edi_READ( oEdit, aGets )

   IF lRes .AND. (ny := Val(aGets[1,4]) ) > 0 .AND. ny <= Len(oEdit:aText)
      oEdit:GoTo( ny, 1, 0 )
   ENDIF

   SetColor( oldc )
   DevPos( oEdit:nRow, oEdit:nCol )

   RETURN Nil

FUNCTION mnu_Plugins( oEdit )

   LOCAL aMenu := {}, i

   FOR i := 1 TO Len( aPlugins )
      IF Empty( aPlugins[i,3] ) .OR. aPlugins[i,3] == oEdit:cSyntaxType
         AAdd( aMenu, { aPlugins[i,2], Nil, Nil} )
      ENDIF
   NEXT
   IF !Empty( aMenu )
      IF ( i := FMenu( oEdit, aMenu ) ) > 0
         IF Empty( aPlugins[i,4] )
            aPlugins[i,4] := hb_hrbLoad( hb_DirBase() + "plugins" + hb_ps() + aPlugins[i,1] )
         ENDIF
         IF !Empty( aPlugins[i,4] )
            hb_hrbDo( aPlugins[i,4], oEdit )
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
      oEdit:WriteTopPane( .T. )
   ELSE
      IF oEdit:nMode == 0
         oEdit:nMode := 1
         oEdit:WriteTopPane( .T. )
      ELSEIF oEdit:nMode == 1
         oEdit:nMode := 2
         oEdit:WriteTopPane( .T. )
         mnu_CmdLine( oEdit )
      ENDIF
   ENDIF

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

   LOCAL n := oEdit:nRow - oEdit:y1 + oEdit:nyFirst
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

STATIC FUNCTION edi_NextWord( oEdit, lEndWord )
   LOCAL ny, nx, nRow := Row(), nCol := Col(), nLen

   ny := nRow - oEdit:y1 + oEdit:nyFirst
   nx := nCol - oEdit:x1 + oEdit:nxFirst
   nLen := cp_Len( oEdit:lUtf8, oEdit:aText[ny] )

   IF nx > nLen
      RETURN Nil
   ENDIF
   DO WHILE ++nx <= nLen
      IF cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx, 1 ) == " "
         IF !Empty( lEndWord ) .AND. nx - (nCol - oEdit:x1 + oEdit:nxFirst) > 1
            nx --
            EXIT
         ENDIF
         DO WHILE ++nx <= nLen .AND. cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx, 1 ) == " "; ENDDO
         IF Empty( lEndWord )
            EXIT
         ENDIF
      ENDIF
   ENDDO
   IF nx - oEdit:nxFirst + oEdit:x1 >= oEdit:x2
      oEdit:nxFirst := nx + oEdit:x1 - oEdit:x2 + 3
      oEdit:lTextOut := .T.
   ENDIF
   DevPos( nRow, oEdit:nCol := ( nx - oEdit:nxFirst + oEdit:x1 ) )

   RETURN Nil

STATIC FUNCTION edi_PrevWord( oEdit )
   LOCAL ny, nx, nRow := Row(), nCol := Col()

   ny := nRow - oEdit:y1 + oEdit:nyFirst
   nx := nCol - oEdit:x1 + oEdit:nxFirst

   IF cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx-1, 1 ) == " "
      DO WHILE --nx > 1 .AND. cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx, 1 ) == " "; ENDDO
   ENDIF
   DO WHILE --nx >= 0
      IF nx == 0 .OR. cp_Substr( oEdit:lUtf8, oEdit:aText[ny], nx, 1 ) == " "
         nx ++
         EXIT
      ENDIF
   ENDDO
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
   @ 09, 20, 13, 60 BOX "ЪДїіЩДАі "
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Save file as"
   SetColor( "W+/BG" ) 

   IF edi_READ( oEdit, aGets )
      cName := aGets[1,4]
   ENDIF

   SetColor( oldc )
   DevPos( oEdit:nRow, oEdit:nCol )

   RETURN cName

STATIC FUNCTION edi_Indent( oEdit, lRight )

   LOCAL i, n, nby1, nby2, nbx2

   IF oEdit:nby1 < 0 .OR. oEdit:nby2 < 0
      RETURN Nil
   ENDIF
   IF oEdit:nby1 <= oEdit:nby2
      nby1 := oEdit:nby1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
   ELSE
      nby1 := oEdit:nby2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
   ENDIF
   FOR i := nby1 TO nby2
      IF i == nby2 .AND. nbx2 == 1
         LOOP
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
   oEdit:Undo( nby1 )
   oEdit:lUpdated := .T.

   RETURN Nil

STATIC FUNCTION edi_BookMarks( oEdit, nKey, lSet )

   LOCAL arr

   IF nKey >= 97 .AND. nKey <= 122
      IF lSet
         oEdit:hBookMarks[nKey] := { oEdit:nRow - oEdit:y1 + oEdit:nyFirst, oEdit:nCol - oEdit:x1 + oEdit:nxFirst }
      ELSE
         IF hb_hHaskey( oEdit:hBookMarks, nKey )
            arr := oEdit:hBookMarks[nKey]
            oEdit:Goto( arr[1], arr[2] )
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION edi_CurPath()

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
   IF lUtf8; RETURN cString; ENDIF
   RETURN Lower( cString )

FUNCTION cp_Upper( lUtf8, cString )
   IF lUtf8; RETURN cString; ENDIF
   RETURN Upper( cString )




















