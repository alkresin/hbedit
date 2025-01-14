/*
 * Chess game
 * HbEdit plugin
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define CTRL_PRESSED  0x020000
#define K_ENTER      13
#define K_ESC        27
#define K_BS          8
#define K_SPACE      32
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_CTRL_D      4
#define K_CTRL_N     14
#define K_F1         28
#define K_F2         -1
#define K_F3         -2
#define K_F4         -3
#define K_F5         -4
#define K_F6         -5
#define K_F7         -6
#define K_F8         -7
#define K_CTRL_F8   -27
#define K_F9         -8
#define K_F10        -9
#define K_LBUTTONDOWN 1002
#define HB_GTI_FONTNAME         24


#define POS_LEN        10
#define POS_BOARD       1
#define POS_W00         2   // Is a short castling possible for white
#define POS_W000        3   // Is a long castling possible for white
#define POS_B00         4   // Is a short castling possible for black
#define POS_B000        5   // Is a long castling possible for black
#define POS_W_00        6
#define POS_W_000       7
#define POS_B_00        8
#define POS_B_000       9
#define POS_4P         10

#define MOVE_LEN        6

#define OP_SET                  1
#define OP_UNSET                2
#define OP_INVALIDATE           3
#define OP_MDOWN                4
#define OP_COLORS               5
#define OP_SIZE                 6

#define BEATKING  100000

STATIC cIniPath
STATIC oGame
STATIC lRussian := .T., lDrawUtf8 := .F., lGui
STATIC x1t, y1t, x2t, nyPos, nxPos
STATIC nScrolled
STATIC lTurnBlack, nLevelWhite, nLevelBlack, aMoveState := { Nil,0,0 }, lShah
STATIC cCompiler, hExt, nLogLevel := 1

STATIC cScreenBuff
STATIC clrBoard := "GR+/N", clrWhite := "W+", clrBlack := "N", clrbWhite := "BG", clrbBlack := "GR"
STATIC cOpenings, lOpenings

STATIC aCurrPos
STATIC aHistory, aHisView
STATIC lSetDiag := .F., lPlayGame, lViewGame, lDebug := .F., lReplayMode := .F., lShowPos := .T.
STATIC nDeep2 := 3

// White - uppercace, black - lowercase
STATIC cInitBoard := "rnbqkbnrpppppppp                                PPPPPPPPRNBQKBNR"
STATIC aFigs := {' ','p','P','r','n','b','q','k','R','N','B','Q','K'}, ;
       aFigs1 :={' ','п','п','Л','К','С','Ф','Кр','Л','К','С','Ф','Кр'}, ;
       aFigs2 :={' ','p','p','R','N','B','Q','K','R','N','B','Q','K'}, ;
       aFigs3 :={' ','♟','♟','♜','♞','♝','♛','♚','♜','♞','♝','♛','♚'}

STATIC aBoardValues := { { ;
    { 100, 100, 100, 100, 100, 100, 100, 100,  ; // P
      178, 183, 186, 173, 202, 182, 185, 190,  ;
      107, 129, 121, 144, 140, 131, 144, 107,  ;
      83, 116, 98, 115, 114, 100, 115, 87,     ;
      74, 103, 110, 109, 106, 101, 100, 77,    ;
      78, 109, 105, 89, 90, 98, 103, 81,       ;
      69, 108, 93, 63, 64, 86, 103, 69,        ;
      100, 100, 100, 100, 100, 100, 100, 100 },;
    { 514, 508, 512, 483, 516, 512, 535, 529,  ; // R
      534, 508, 535, 546, 534, 541, 513, 539,  ;
      498, 514, 507, 512, 524, 506, 504, 494,  ;
      479, 484, 495, 492, 497, 475, 470, 473,  ;
      451, 444, 463, 458, 466, 450, 433, 449,  ;
      437, 451, 437, 454, 454, 444, 453, 433,  ;
      426, 441, 448, 453, 450, 436, 435, 426,  ;
      449, 455, 461, 484, 477, 461, 448, 447 },;
    { 214, 227, 205, 205, 270, 225, 222, 210,  ; // N
      277, 274, 380, 244, 284, 342, 276, 266,  ;
      290, 347, 281, 354, 353, 307, 342, 278,  ;
      304, 304, 325, 317, 313, 321, 305, 297,  ;
      279, 285, 311, 301, 302, 315, 282, 280,  ;
      262, 290, 293, 302, 298, 295, 291, 266,  ;
      257, 265, 282, 280, 282, 280, 257, 260,  ;
      206, 257, 254, 256, 261, 245, 258, 211 },;
    { 261, 242, 238, 244, 297, 213, 283, 270,  ; // B
      309, 340, 355, 278, 281, 351, 322, 298,  ;
      311, 359, 288, 361, 372, 310, 348, 306,  ;
      345, 337, 340, 354, 346, 345, 335, 330,  ;
      333, 330, 337, 343, 337, 336, 320, 327,  ;
      334, 345, 344, 335, 328, 345, 340, 335,  ;
      339, 340, 331, 326, 327, 326, 340, 336,  ;
      313, 322, 305, 308, 306, 305, 310, 310 },;
    { 935, 930, 921, 825, 998, 953, 1017, 955, ; // Q
      943, 961, 989, 919, 949, 1005, 986, 953, ;
      927, 972, 961, 989, 1001, 992, 972, 931, ;
      930, 913, 951, 946, 954, 949, 916, 923,  ;
      915, 914, 927, 924, 928, 919, 909, 907,  ;
      899, 923, 916, 918, 913, 918, 913, 902,  ;
      893, 911, 929, 910, 914, 914, 908, 891,  ;
      890, 899, 898, 916, 898, 893, 895, 887 },;
    { 60004, 60054, 60047, 59901, 59901, 60060, 60083, 59938, ; // K
      59968, 60010, 60055, 60056, 60056, 60055, 60010, 60003, ;
      59938, 60012, 59943, 60044, 59933, 60028, 60037, 59969, ;
      59945, 60050, 60011, 59996, 59981, 60013, 60000, 59951, ;
      59945, 59957, 59948, 59972, 59949, 59953, 59992, 59950, ;
      59953, 59958, 59957, 59921, 59936, 59968, 59971, 59968, ;
      59996, 60003, 59986, 59950, 59943, 59982, 60013, 60004, ;
      60017, 60030, 59997, 59986, 60006, 59999, 60040, 60018} }, ;
  { ;
    { 100, 100, 100, 100, 100, 100, 100, 100,  ;  // p
      69, 108, 93, 63, 64, 86, 103, 69,        ;
      78, 109, 105, 89, 90, 98, 103, 81,       ;
      74, 103, 110, 109, 106, 101, 100, 77,    ;
      83, 116, 98, 115, 114, 100, 115, 87,     ;
      107, 129, 121, 144, 140, 131, 144, 107,  ;
      178, 183, 186, 173, 202, 182, 185, 190,  ;
      100, 100, 100, 100, 100, 100, 100, 100 },;
    { 449, 455, 461, 484, 477, 461, 448, 447,  ;  // r
      426, 441, 448, 453, 450, 436, 435, 426,  ;
      437, 451, 437, 454, 454, 444, 453, 433,  ;
      451, 444, 463, 458, 466, 450, 433, 449,  ;
      479, 484, 495, 492, 497, 475, 470, 473,  ;
      498, 514, 507, 512, 524, 506, 504, 494,  ;
      534, 508, 535, 546, 534, 541, 513, 539,  ;
      514, 508, 512, 483, 516, 512, 535, 529}, ;
    { 206, 257, 254, 256, 261, 245, 258, 211,  ;  // n
      257, 265, 282, 280, 282, 280, 257, 260,  ;
      262, 290, 293, 302, 298, 295, 291, 266,  ;
      279, 285, 311, 301, 302, 315, 282, 280,  ;
      304, 304, 325, 317, 313, 321, 305, 297,  ;
      290, 347, 281, 354, 353, 307, 342, 278,  ;
      277, 274, 380, 244, 284, 342, 276, 266,  ;
      214, 227, 205, 205, 270, 225, 222, 210}, ;
    { 313, 322, 305, 308, 306, 305, 310, 310,  ;  // b
      339, 340, 331, 326, 327, 326, 340, 336,  ;
      334, 345, 344, 335, 328, 345, 340, 335,  ;
      333, 330, 337, 343, 337, 336, 320, 327,  ;
      345, 337, 340, 354, 346, 345, 335, 330,  ;
      311, 359, 288, 361, 372, 310, 348, 306,  ;
      309, 340, 355, 278, 281, 351, 322, 298,  ;
      261, 242, 238, 244, 297, 213, 283, 270}, ;
    { 890, 899, 898, 916, 898, 893, 895, 887,  ;  // q
      893, 911, 929, 910, 914, 914, 908, 891,  ;
      899, 923, 916, 918, 913, 918, 913, 902,  ;
      915, 914, 927, 924, 928, 919, 909, 907,  ;
      930, 913, 951, 946, 954, 949, 916, 923,  ;
      927, 972, 961, 989, 1001, 992, 972, 931, ;
      943, 961, 989, 919, 949, 1005, 986, 953, ;
      935, 930, 921, 825, 998, 953, 1017, 955},;
    { 60017, 60030, 59997, 59986, 60006, 59999, 60040, 60018, ;  // k
      59996, 60003, 59986, 59950, 59943, 59982, 60013, 60004, ;
      59953, 59958, 59957, 59921, 59936, 59968, 59971, 59968, ;
      59945, 59957, 59948, 59972, 59949, 59953, 59992, 59950, ;
      59945, 60050, 60011, 59996, 59981, 60013, 60000, 59951, ;
      59938, 60012, 59943, 60044, 59933, 60028, 60037, 59969, ;
      59968, 60010, 60055, 60056, 60056, 60055, 60010, 60003, ;
      60004, 60054, 60047, 59901, 59901, 60060, 60083, 59938} } }

STATIC guiBoaSize := 3, guiFontName
STATIC guiClrWCell, guiClrBCell, guiClrSel, guiClrWText, guiClrBText
STATIC aThemes := { { 0xb0b0b0, 0xb0b0b0, 0, 0xffffff, 0 }, ;
   { 0x9fcfff, 0x458cd2, 0, 0xffffff, 0 }, ;
   { 0xffcf9f, 0xd28c45, 0, 0xffffff, 0 }, ;
   { 0xffffff, 0xe2e2e2, 0, 255, 0 } }, cUserTheme, nTheme := 1

FUNCTION plug_gm_Chess( oEdit, cPath )

   LOCAL i, cName := "$Chess"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      Scroll( y, o:x1, y, o:x2 )
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "F9-menu  Chess" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }

   lGUI := ( hb_gtVersion() == "HWGUI" )
   nLevelWhite := 0; nLevelBlack := 1
   lDrawUtf8 := !( "wind" $ Lower( Os() ) )

   IF Empty( cIniPath )
      cIniPath := cPath
      cOpenings := cIniPath + "chess" + hb_ps() + "chessopn.dbf"
      Read_Game_Ini( (cIniPath := cPath) + "chess.ini" )
   ENDIF

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   //DispBegin()
   oGame := mnu_NewBuf( oEdit )
   edi_SetPalette( oGame, "default" )
   //DispEnd()
   oGame:cFileName := cName
   oGame:bWriteTopPane := bWPane
   oGame:bOnKey := {|o,n| _gm_Chess_OnKey(o,n) }
   oGame:bStartEdit := {|| _gm_Chess_Start() }
   //oGame:cp := Iif( lDrawUtf8, "UTF8",  "RU866" )
   hb_cdpSelect( oGame:cp := "UTF8" )
   oGame:lUtf8 := .T.
   oGame:lIns := Nil
   aCurrPos := Array( POS_LEN )
   dbOpn_Open()

   RETURN Nil

STATIC FUNCTION _gm_Chess_Start()

   IF Empty( cScreenBuff )
      y1t := oGame:y1 + 2
      x1t := oGame:x1 + 2
      x2t := x1t + 32
      _Game_New( .T. )
   ELSE
      __PaintBo_Chess( , OP_SET )
      RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cScreenBuff )
   ENDIF

   RETURN Nil

STATIC FUNCTION _Game_Exit()

   cScreenBuff := Nil
   IF lOpenings
      dbSelectArea( "OPENINGS" )
      dbCloseArea()
      lOpenings := .F.
   ENDIF
   IF !Empty( hExt )
      ecli_Close( hExt )
      hExt := Nil
   ENDIF
   Write_Game_Ini()
   mnu_Exit( oGame )

   RETURN Nil

STATIC FUNCTION _Game_New( lFirst )

   SetColor( clrBoard )
   Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
   lPlayGame := .T.
   lViewGame := .F.
   aMoveState[2] := aMoveState[3] := 0
   aCurrPos[POS_BOARD] := cInitBoard
   aCurrPos[POS_W00] := aCurrPos[POS_W000] := aCurrPos[POS_B00] := aCurrPos[POS_B000] := .T.
   lShah := .F.
   aHistory := {}
   __PaintBo_Chess( , OP_SET )
   __PaintBo_Chess( , OP_COLORS )
   DrawBoard()
   nScrolled := 0
   lTurnBlack := .F.
   _Game_Players( !lFirst )

   RETURN Nil

STATIC FUNCTION _Game_DiagNew( cTurn )

   LOCAL nc

   SetColor( clrBoard )
   Scroll( y1t-1, x2t, oGame:y2, x2t+30 )
   Scroll( y1t+10, x1t, oGame:y2, x2t )
   aHistory := {}
   aCurrPos[POS_W00] := aCurrPos[POS_W000] := aCurrPos[POS_B00] := aCurrPos[POS_B000] := .T.
   lPlayGame := .T.
   lViewGame := .F.
   aMoveState[2] := aMoveState[3] := 0
   nScrolled := 0
   nLevelWhite := nLevelBlack := 0
   IF Empty( cTurn )
      cTurn := Iif( ( nc := Iif( lRussian, edi_Alert( "Кто начинает?", "Белые", "Черные" ), ;
         edi_Alert( "Who's turn?", "White", "Black" ) ) ) == 2, 'b', 'w' )
   ENDIF
   IF cTurn == 'b'
      AAdd( aHistory, { {Nil,Nil,Nil,Nil,Nil,Nil}, Nil } )
      lTurnBlack := .T.
   ELSE
      lTurnBlack := .F.
   ENDIF
   _Game_Players( .F. )

   RETURN Nil

STATIC FUNCTION _Game_SetDiag( nMove )

   LOCAL nc
   STATIC aMenuR := { "Пусто", "Черная пешка", "Белая пешка", "Черная ладья", ;
      "Черный конь", "Черный слон", "Черный ферзь", "Черный король", ;
      "Белая ладья", "Белый конь", "Белый слон", "Белый ферзь", "Белый король" }
   STATIC aMenuE := { "Empty", "Black pawn", "White pawn", "Black rook", ;
      "Black knight", "Black bishop", "Black queen", "Black king", ;
      "White rook", "White knight", "White bishop", "White queen", "White king" }

   IF !( nMove == Nil )
      nc := FMenu( oGame, Iif( lRussian, aMenuR, aMenuE ), y1t, x2t+2 )
      IF nc > 0
         aCurrPos[POS_BOARD] := hb_bPoke( aCurrPos[POS_BOARD], nMove, Asc(aFigs[nc]) )
         DrawBoard()
      ENDIF
      RETURN Nil
   ENDIF

   Scroll( y1t-1, x2t+2, oGame:y2, x2t+30 )
   IF !lSetDiag
      lSetDiag := .T.
      aCurrPos[POS_BOARD] := Space( 64 )
      DrawBoard()
      @ y1t-1, x2t+4 SAY Iif( lRussian, "Жмите F5 когда закончите", "Press F5 to finish" )
   ELSE
      lSetDiag := .F.
      _Game_DiagNew()
   ENDIF

   RETURN Nil

STATIC FUNCTION _Game_Level( nTitle )

   LOCAL i, aMenu := { Iif(lRussian,"Уровень 1","Level 1"), Iif(lRussian,"Уровень 2","Level 2") } //, "Sunfish" }

   IF nTitle == 1
      RETURN FMenu( oGame, aMenu, y1t, x2t+2, y1t+4, x2t+40,,,,,,,, Iif(lRussian,"Белые","White") )
   ENDIF
   AAdd( aMenu, "Sunfish" )

   i := FMenu( oGame, aMenu, y1t, x2t+2, y1t+4, x2t+40,,,,,,,, ;
      Iif( nTitle==1, Iif(lRussian,"Белые","White"), Iif(lRussian,"Черные","Black") ) )
   IF i == Len( aMenu )
      i := Iif( !Empty( cCompiler ) .OR. !Empty( cCompiler := edi_CheckPython() ), 10, 1 )
      IF i == 10 .AND. !ii_SunfishStart( nTitle )
         i := 1
      ENDIF
   ENDIF
   RETURN i

STATIC FUNCTION _Game_Players( lAsk )

   LOCAL nc, nLevel, cLevelW, cLevelB
   STATIC aMenuR := { "Человек - Компьютер", "Компьютер - Человек", "Человек - Человек", "Компьютер - Компьютер" }
   STATIC aMenuE := { "Human - Computer", "Computer - Human", "Human - Human", "Computer - Computer" }

   IF lAsk
      nc := FMenu( oGame, Iif( lRussian, aMenuR, aMenuE ), y1t, x2t+2 )

      IF nc == 0
         RETURN Nil

      ELSEIF nc == 3
         nLevelWhite := nLevelBlack := 0

      ELSEIF nc < 3
         IF ( nLevel := _Game_Level( Iif( nc == 1, 2, 1 ) ) ) > 0
            nLevelWhite := Iif( nc==1, 0, nLevel )
            nLevelBlack := Iif( nc==1, nLevel, 0 )
         ENDIF

      ELSEIF nc == 4
         IF ( nLevel := _Game_Level( 1 ) ) > 0
            nLevelWhite := nLevel
            IF ( nLevel := _Game_Level( 2 ) ) > 0
               nLevelBlack := nLevel
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   Scroll( y1t-1, x2t+10, y1t-1, x2t+32 )
   cLevelW := Iif( nLevelWhite == 10, "S", Str(nLevelWhite,1) )
   cLevelB := Iif( nLevelBlack == 10, "S", Str(nLevelBlack,1) )
   IF lRussian
      @ y1t-1, x2t+4 SAY Iif( nLevelWhite == 0, "Человек", "Компьютер (" + cLevelW + ")" )
      @ y1t-1, x2t+19 SAY Iif( nLevelBlack == 0, "Человек", "Компьютер (" + cLevelB + ")" )
   ELSE
      @ y1t-1, x2t+4 SAY Iif( nLevelWhite == 0, "Human", "Computer (" + cLevelW + ")" )
      @ y1t-1, x2t+19 SAY Iif( nLevelBlack == 0, "Human", "Computer (" + cLevelB + ")" )
   ENDIF
   IF lPlayGame .AND. ( (lTurnBlack .AND. nLevelBlack > 0) .OR. (!lTurnBlack .AND. nLevelWhite > 0) )
      ii_MakeMove( .T. )
   ENDIF

   RETURN Nil

STATIC FUNCTION _Game_MainMenu()

   LOCAL nc
   LOCAL aMenuR := { {"Выход",,,"Esc,F10"}, {"Новая партия",,,"F3"}, {"Диаграмма",,,"F5"}, {"Игроки",,,"F6"}, {"Сохранить",,,"F2"}, {"Загрузить",,,"F4"}, {"Rus/Eng",,,"F7"}, {Iif(lGui,"Настройки","---"),,,"F8"} }
   LOCAL aMenuE := { {"Exit",,,"Esc,F10"}, {"New Game",,,"F3"}, {"Set diagramm",,,"F5"}, {"Change players",,,"F6"}, {"Save",,,"F2"}, {"Load",,,"F4"}, {"Rus/Eng",,,"F7"}, {Iif(lGui,"Options","---"),,,"F8"} }

   IF ( nc := FMenu( oGame, Iif( lRussian, aMenuR, aMenuE ), y1t, x2t+2 ) ) == 1
      __PaintBo_Chess( , OP_UNSET )
      _Game_Exit()

   ELSEIF nc == 2
      _Game_New( .F. )

   ELSEIF nc == 3
      _Game_SetDiag()

   ELSEIF nc == 4
      _Game_Players( .T. )

   ELSEIF nc == 5
      chess_Save()

   ELSEIF nc == 6
      chess_Load()

   ELSEIF nc == 7
      lRussian := !lRussian
      DrawBoard()

   ELSEIF nc == 8
      chess_Settings()
   ENDIF

   RETURN Nil

STATIC FUNCTION _gm_Chess_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol, nRow, arr, n, l

   IF nKey == K_LBUTTONDOWN
      nCol := MCol()
      nRow := MRow()
      IF nRow == oEdit:y1-1 .AND. nCol < 8
         _Game_MainMenu()
      ELSE
         IF lGui
            arr := __PaintBo_Chess( , OP_MDOWN )
            nRow := y1t + arr[2]-1
            nCol := x1t + (arr[1]-1) * 3
         ENDIF
         IF lSetDiag
            IF nRow >= y1t .AND. nRow <= y1t + 7 .AND. nCol >= x1t .AND. nCol <= x1t + 24
               _Game_SetDiag( (nRow - y1t)*8 + Int((nCol - x1t) / 3) + 1 )
            ENDIF
         ELSEIF lPlayGame
            IF nRow >= y1t .AND. nRow <= y1t + 7 .AND. nCol >= x1t .AND. nCol <= x1t + 24
               MakeMove( nRow - y1t + 1, Int((nCol - x1t) / 3) + 1 )
            ENDIF
         ENDIF
      ENDIF

   ELSEIF nKey == K_BS
      IF (lPlayGame .OR. lViewGame) .AND. !Empty( aHistory )
         IF ATail( aHistory )[2] == Nil
            hb_ADel( aHistory, Len(aHistory), .T. )
            IF !Empty( aHistory )
               ATail( aHistory )[2] := Nil
            ENDIF
         ELSE
            hb_ADel( aHistory, Len(aHistory), .T. )
         ENDIF
         arr := aHistory
         l := lViewGame
         _Game_New( .T. )
         nCol := nLevelWhite; nRow := nLevelBlack; nLevelWhite := nLevelBlack := 0
         chess_ReplayGame( arr )
         nLevelWhite := nCol; nLevelBlack := nRow
         DrawBoard()
         IF l
            lPlayGame := .F.
            lViewGame := .T.
         ENDIF
      ENDIF

   ELSEIF nKey == K_SPACE
      IF lViewGame
         arr := Iif( lTurnBlack, aHisView[Len(aHistory),2], ;
            Iif( Len(aHistory)<Len(aHisView), aHisView[Len(aHistory)+1,1], Nil ) )
         IF !Empty( arr )
            Set_lb_lw( aCurrPos, lTurnBlack )
            DrawMove( arr )
            lTurnBlack := !lTurnBlack
         ENDIF
      ENDIF

   ELSEIF nKey == K_F3
      _Game_New( .F. )

   ELSEIF nKey == K_F5
      _Game_SetDiag()

   ELSEIF nKey == K_F6
      _Game_Players( .T. )

   ELSEIF nKey == K_F7
      lRussian := !lRussian
      DrawBoard()

   ELSEIF nKey == K_F8
      chess_Settings()

   ELSEIF nKey == K_CTRL_F8
      chess_Tune()

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
      __PaintBo_Chess( , OP_UNSET )
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_F1
      chess_Help()

   ELSEIF nKey == K_F2
      chess_Save()

   ELSEIF nKey == K_F4
      chess_Load()

   ELSEIF nKey == K_F9
     _Game_MainMenu()

   ELSEIF nKey == K_CTRL_D .AND. hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      lDebug := .T.

   ELSEIF nKey == K_CTRL_N
     IF lPlayGame
        ii_MakeMove()
     ENDIF

   ELSEIF nKey == K_ESC .OR. nKey == K_F10
      IF edi_Alert( "Really quit the game?", "Yes", "No" ) == 1
         __PaintBo_Chess( , OP_UNSET )
         _Game_Exit()
      ENDIF
   ENDIF

   RETURN -1

STATIC FUNCTION DrawBoard()

   LOCAL i, j, i1, lBlack := .F., c, cBoard

   IF lReplayMode
      RETURN Nil
   ENDIF
   IF lGui
      __PaintBo_Chess( , OP_INVALIDATE )
   ELSE
      DispBegin()
      cBoard := aCurrPos[POS_BOARD]
      SetColor( clrBoard )
      FOR i := 0 TO 7
         SetColor( clrBoard )
         @ y1t + i, x1t - 1 SAY Ltrim(Str(8-i))
         FOR j := 1 TO 8
            c := Substr( cBoard, i*8+j, 1 )
            SetColor( Iif( c > 'Z', clrBlack, clrWhite ) + "/" + Iif( lBlack, clrbBlack, clrbWhite ) )
            i1 := Ascan( aFigs,c )
            @ y1t + i, x1t + (j-1)*3 SAY " " + ;
               Iif( lDrawUtf8, aFigs3[i1], Iif( lRussian,aFigs1[i1],aFigs2[i1] ) ) + " "
            lBlack := !lBlack
         NEXT
         lBlack := !lBlack
      NEXT
      SetColor( clrBoard )
      FOR j := 1 TO 8
         @ y1t + 8, x1t + (j-1)*3 + 1 SAY Chr(96+j)
      NEXT
      DispEnd()
   ENDIF

   RETURN Nil

/* Возвращает .T., если есть шах королю противоположной ( lToOppo := .T. ) стороны или
   ( lToOppo := .F. ) себе
 */
STATIC FUNCTION Check4Shah( aPos, lToOppo )

   LOCAL cFig, cKing, ltb := Iif( lToOppo, lTurnBlack, !lTurnBlack )

   cKing := Iif( ltb, 'K', 'k' )

   FOR EACH cFig IN aPos[POS_BOARD]
      IF (!ltb .AND. cFig >= 'A' .AND. cFig <= 'Z') .OR. (ltb .AND. cFig >= 'a' .AND. cFig <= 'z')
         IF !Empty( chess_GenMoves( aPos, cFig:__enumindex, .T., cKing ) )
            RETURN .T.
         ENDIF
      ENDIF
   NEXT
   RETURN .F.

/* Возвращает .T., если есть мат королю противоположной ( lToOppo := .T. ) стороны или
   ( lToOppo := .F. ) себе
 */
STATIC FUNCTION Check4Mate( aPos, lToOppo, lCheck4Shah )

   LOCAL i, j, cFig, nFig, arr, nLen, cBoard := aPos[POS_BOARD]
   LOCAL aPosTemp := Array(POS_LEN), ltb := Iif( lToOppo, !lTurnBlack, lTurnBlack )

   //IF lDebug; edi_Writelog( "Mate0 " + hb_valtoexp(lTurnBlack) ); ENDIF
   ACopy( aPos, aPosTemp, 2,, 2 )
   IF !Empty( lCheck4Shah ) .AND. !Check4Shah( aPos, lToOppo )
      RETURN .F.
   ENDIF
   //IF lDebug; edi_Writelog( "Mate1 " + hb_valtoexp(lTurnBlack) ); ENDIF

   Set_lb_lw( aPos, ltb )
   FOR EACH cFig IN cBoard
      IF cFig >= 'A' .AND. ;
         ( ( ltb .AND. cFig >= 'a' ) .OR. ( !ltb .AND. cFig < 'a' ) )
         i := cFig:__enumindex
         nFig := hb_bPeek( cBoard, i )
         arr := chess_GenMoves( aPos, i )
         nLen := Len( arr )
         //IF lDebug; edi_Writelog( "Mate2 " + hb_valtoexp(lTurnBlack) + str(nFig,3) + str(i,3) + " " + hb_valtoexp(arr) ); ENDIF
         FOR j := 1 TO nLen
            PostProcess( aPosTemp, cBoard, nFig, i, arr[j] )
            //IF lDebug; edi_Writelog( "Mate3 " + hb_valtoexp(lTurnBlack) + str(j,3) ); ENDIF
            IF !Check4Shah( aPosTemp, lToOppo )
               //IF lDebug; edi_Writelog( "Mate4" ); ENDIF
               RETURN .F.
            ENDIF
         NEXT
      ENDIF
   NEXT

   RETURN .T.

STATIC FUNCTION MoveN2C( nStart, nEnd, cFigBeat )

   LOCAL i1, cMove := ""

   IF !Empty( nStart )
      i1 := Iif( nStart%8 == 0, 8, nStart%8 )
      cMove += Chr( 96 + i1 ) + Ltrim(Str(Int(9-nStart/8)))
   ENDIF
   IF !Empty( nEnd )
      i1 := Iif( nEnd%8 == 0, 8, nEnd%8 )
      cMove += Iif( Empty(nStart),'',Iif( Empty(cFigBeat),'-',':' ) ) + Chr( 96+i1 ) + Ltrim(Str(Int(9-nEnd/8)))
   ENDIF

   RETURN cMove

STATIC FUNCTION MoveC2N( s, n )
   RETURN (8-(hb_bPeek(s,2+n)-48))*8 + hb_bPeek(s,1+n)-96

STATIC FUNCTION DrawMove( aMove )

   LOCAL cFig := aMove[1], nStart, nEnd
   LOCAL nMove := Len( aHistory ) + Iif( lTurnBlack, 0, 1 ), i, j, i1, c, cMove, cFigBeat
   LOCAL lMate := .F.

   i := y1t + nMove - 1 - nScrolled
   j := x2t + Iif( lTurnBlack, 17, 2 )
   IF i > oGame:y2
      Scroll( y1t, x2t+2, oGame:y2, x2t+30, 1 )
      nScrolled ++
      i := oGame:y2
   ENDIF

   Scroll( i, j, i, j + 12 )
   DevPos( i, j )

   i1 := Ascan( aFigs,cFig )
   IF i1 == 0
      IF lRussian
         DevOut( Iif( cFig=="!", "ОШИБКА", "Ждите.." ) )
      ELSE
         DevOut( Iif( cFig=="!", "WRONG MOVE", "Wait..." ) )
      ENDIF
      RETURN .F.
   ENDIF

   nStart := aMove[2]; nEnd := Iif( Len(aMove)>2, aMove[3], Nil )
   c := Iif( Lower( cFig ) == 'p', "", Upper( Iif( lRussian, aFigs1[i1], aFigs2[i1] ) ) )
   IF Empty( nEnd )
      cMove := c + MoveN2C( nStart )
   ELSE
      cFigBeat := Iif( nEnd == aCurrPos[POS_4P], 'p', Substr( aCurrPos[POS_BOARD], nEnd, 1 ) )
      cMove := c + MoveN2C( nStart, nEnd, cFigBeat )
      PostProcess( aCurrPos, aCurrPos[POS_BOARD], Asc(cFig), nStart, nEnd, ;
         Iif( Len(aMove)>5.AND.!Empty(aMove[6]),Asc(aMove[6]),Nil ), lPlayGame )
      IF cFig == 'K' .OR. cFig == 'k'
         IF nEnd - nStart == 2
            cMove := "O-O"
         ELSEIF nEnd - nStart == -2
            cMove := "O-O-O"
         ENDIF
      ENDIF
      aMoveState[3] := nEnd
      DrawBoard()
      lShah := Check4Shah( aCurrPos, .T. )
      IF lPlayGame .AND. lShah
         lMate := Check4Mate( aCurrPos, .T. )
      ENDIF
      AddHis( aCurrPos[POS_BOARD], cFig, nStart, nEnd, cFigBeat, lShah )
      IF !Empty( c := ATail( aHistory)[Iif(lTurnBlack,2,1),6] )
         cMove += c
      ENDIF
      IF lMate
         cMove += 'x'
      ELSEIF lShah
         cMove += '+'
      ENDIF
   ENDIF

   DevPos( i, j )
   DevOut( Iif( lTurnBlack, "",Str(nMove,3)+'. ' ) + cMove )

   RETURN lMate

// Проверка возможности рокировки
STATIC FUNCTION Set_lb_lw( aPos, lBlack )

   LOCAL cFig, arr

   aPos[POS_B_00] := aPos[POS_B00] ; aPos[POS_B_000] := aPos[POS_B000] ; aPos[POS_W_00] := aPos[POS_W00] ; aPos[POS_W_000] := aPos[POS_W000]
   IF lBlack .AND. (aPos[POS_B00] .OR. aPos[POS_B000])
      IF hb_BPeek( aPos[POS_BOARD],2 ) != 32 .OR. hb_BPeek( aPos[POS_BOARD],3 ) != 32 .OR. ;
         hb_BPeek( aPos[POS_BOARD],4 ) != 32 .OR. hb_BPeek( aPos[POS_BOARD],1 ) != 114
         aPos[POS_B_000] := .F.
      ENDIF
      IF hb_BPeek( aPos[POS_BOARD],6 ) != 32 .OR. hb_BPeek( aPos[POS_BOARD],7 ) != 32 .OR. ;
         hb_BPeek( aPos[POS_BOARD],8 ) != 114  // r
         aPos[POS_B_00] := .F.
      ENDIF
      IF aPos[POS_B_00] .OR. aPos[POS_B_000]
         FOR EACH cFig IN aPos[POS_BOARD]
            IF cFig >= 'A' .AND. cFig <= 'Z'
               arr := chess_GenMoves( aPos, cFig:__enumindex )
               IF ( Ascan( arr,5 ) > 0 .OR. Ascan( arr,6 ) > 0 .OR. ;
                  Ascan( arr,7 ) > 0 .OR. Ascan( arr,8 ) > 0 )
                  aPos[POS_B_00] := .F.
               ENDIF
               IF ( Ascan( arr,1 ) > 0 .OR. Ascan( arr,2 ) > 0 .OR. ;
                  Ascan( arr,3 ) > 0 .OR. Ascan( arr,4 ) > 0 .OR. Ascan( arr,5 ) > 0 )
                  aPos[POS_B_000] := .F.
               ENDIF
            ENDIF
         NEXT
      ENDIF
   ELSEIF !lBlack .AND. (aPos[POS_W00] .OR. aPos[POS_W000])
      IF hb_BPeek( aPos[POS_BOARD],58 ) != 32 .OR. hb_BPeek( aPos[POS_BOARD],59 ) != 32 .OR. ;
         hb_BPeek( aPos[POS_BOARD],60 ) != 32 .OR. hb_BPeek( aPos[POS_BOARD],57 ) != 82
         aPos[POS_W_000] := .F.
      ENDIF
      IF hb_BPeek( aPos[POS_BOARD],62 ) != 32 .OR. hb_BPeek( aPos[POS_BOARD],63 ) != 32 .OR. ;
         hb_BPeek( aPos[POS_BOARD],64 ) != 82  // R
         aPos[POS_W_00] := .F.
      ENDIF
      IF aPos[POS_W_00] .OR. aPos[POS_W_000]
         FOR EACH cFig IN aPos[POS_BOARD]
            IF cFig >= 'a' .AND. cFig <= 'z'
               arr := chess_GenMoves( aPos, cFig:__enumindex )
               IF ( Ascan( arr,61 ) > 0 .OR. Ascan( arr,62 ) > 0 .OR. ;
                  Ascan( arr,63 ) > 0 .OR. Ascan( arr,64 ) > 0 )
                  aPos[POS_W_00] := .F.
               ENDIF
               IF ( Ascan( arr,57 ) > 0 .OR. Ascan( arr,58 ) > 0 .OR. ;
                  Ascan( arr,59 ) > 0 .OR. Ascan( arr,60 ) > 0 .OR. Ascan( arr,61 ) > 0 )
                  aPos[POS_W_000] := .F.
               ENDIF
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION MakeMove( nRow, nCol )

   LOCAL nMove := (nRow-1)*8 + nCol, nSumm, nCou := 0, nStart, lMate
   LOCAL c := Substr( aCurrPos[POS_BOARD], nMove, 1 ), cBoa16

   IF aMoveState[2] == 0
      IF ( (!lTurnBlack .AND. c >= 'A' .AND. c <= 'Z') .OR. ;
         (lTurnBlack .AND. c >= 'a' .AND. c <= 'z') )
         DrawMove( {c, nMove} )
         aMoveState[1] := c; aMoveState[2] := nMove
         DrawBoard()
      ENDIF
   ELSE
      Set_lb_lw( aCurrPos, lTurnBlack )
      IF isMoveCorrect( nMove )
         nStart := aMoveState[2]
         aMoveState[2] := 0
         lMate := DrawMove( {aMoveState[1], nStart, nMove} )
         IF !lTurnBlack
            cBoa16 := ATail(aHistory)[3]
            AEval( aHistory, {|a|nCou := Iif(Len(a)>2.AND.a[3]==cBoa16,nCou+1,nCou)} )
         ENDIF
         IF lMate // lShah .AND. Check4Mate( aCurrPos, .T. )
            GameOver( 1 )  // Победа
         ELSEIF !lTurnBlack .AND. nCou >= 5
            GameOver( 3 )  // Ничья
         ELSE
            lTurnBlack := !lTurnBlack
            IF Iif( lTurnBlack, nLevelBlack, nLevelWhite ) > 0
               ii_MakeMove()
            ENDIF
         ENDIF
      ELSE
         DrawMove( {'!'} )
      ENDIF
      aMoveState[2] := 0
   ENDIF

   RETURN Nil

/* The function returns an array of possible cells numbers (1...64) - moves
 *  for the figure in a <nStart> cell of a <aPos> position
 * If lBeat == .T., only those moves, which beats opposite figure, are returned.
 */
STATIC FUNCTION chess_GenMoves( aPos, nStart, lBeat, cFigBeat )

   LOCAL cFig := Substr( aPos[POS_BOARD], nStart, 1 ), cFigU := Upper( cFig )
   LOCAL aMoves := {}, i, j, arr, nLen, nMove, nCol1, nCol2
   LOCAl lBlack := (cFig >= 'a' .AND. cFig <= 'z')
   STATIC cFigures := "pnbrqk"
#define N     -8
#define E      1
#define S      8
#define W     -1
   STATIC aDirections := { { ;
      { N, N+N, N+W, N+E }, ;
      { N+N+E, E+N+E, E+S+E, S+S+E, S+S+W, W+S+W, W+N+W, N+N+W }, ;
      { N+E, S+E, S+W, N+W }, ;
      { N, E, S, W }, ;
      { N, E, S, W, N+E, S+E, S+W, N+W }, ;
      { N, E, S, W, N+E, S+E, S+W, N+W } }, ;
      { ;
      { -N, -N-N, -N-W, -N-E }, ;
      { -N-N-E, -E-N-E, -E-S-E, -S-S-E, -S-S-W, -W-S-W, -W-N-W, -N-N-W }, ;
      { -N-E, -S-E, -S-W, -N-W }, ;
      { -N, -E, -S, -W }, ;
      { -N, -E, -S, -W, -N-E, -S-E, -S-W, -N-W }, ;
      { -N, -E, -S, -W, -N-E, -S-E, -S-W, -N-W } } }
#undef N
#undef E
#undef S
#undef W

   IF lBeat == Nil; lBeat := .F.; ENDIF
   arr := aDirections[Iif(lBlack,2,1), At( Lower(cFig), cFigures ) ]
   nLen := Len( arr )
   //IF lDebug .AND. cFig == 'k'; edi_Writelog( "gen: " + hb_valtoexp(arr) ); ENDIF
   FOR i := 1 TO nLen
      nMove := nStart
      DO WHILE .T.
         nCol1 := nMove % 8
         IF nCol1 == 0; nCol1 := 8; ENDIF
         nMove += arr[i]
         nCol2 := nMove % 8
         IF nCol2 == 0; nCol2 := 8; ENDIF
         IF nMove <= 0 .OR. nMove > 64 .OR. Abs( nCol2-nCol1 ) > 2
            EXIT
         ENDIF
         cFig := Substr( aPos[POS_BOARD], nMove, 1 )
         IF cFigU == "P"
            IF cFig == ' '
               IF lBeat
                  EXIT
               ENDIF
               IF i > 1
                  IF i >= 2
                     IF nMove == aPos[POS_4P]
                        Aadd( aMoves, nMove )
                     ENDIF
                  ENDIF
                  EXIT
               ELSEIF ( j := Abs( nStart - nMove ) ) < 10
                  Aadd( aMoves, nMove )
               ELSEIF j < 20 .AND. ( j := Int( (nStart-1)/8 ) ) == 1 .OR. j == 6
                  Aadd( aMoves, nMove )
                  EXIT
               ELSE
                  EXIT
               ENDIF
            ELSEIF (cFig >= 'a' .AND. cFig <= 'z') == lBlack
               EXIT
            ELSE
               IF i <= 2
                  EXIT
               ELSEIF cFigBeat == Nil .OR. cFig == cFigBeat
                  Aadd( aMoves, nMove )
                  EXIT
               ELSE
                  EXIT
               ENDIF
            ENDIF
         ELSE
            IF cFig == ' '
               IF !lBeat
                  Aadd( aMoves, nMove )
               ENDIF
            ELSEIF (cFig >= 'a' .AND. cFig <= 'z') == lBlack
               EXIT
            ELSEIF cFigBeat == Nil .OR. cFig == cFigBeat
               Aadd( aMoves, nMove )
               EXIT
            ELSE
               EXIT
            ENDIF
         ENDIF
         IF cFigu == "N"
            EXIT
         ELSEIF cFigu == "K"
            //edi_writelog( valtype(aPos[POS_W_00])+valtype(aPos[POS_W_000]) )
            IF ( nStart == 61 .AND. !lBlack .AND. ;
               ( (nMove-nStart == 1 .AND. aPos[POS_W_00]) .OR. (nMove-nStart == -1 .AND. aPos[POS_W_000]) ) ) .OR. ;
               ( nStart == 5 .AND. lBlack .AND. ;
               ( (nMove-nStart == 1 .AND. aPos[POS_B_00]) .OR. (nMove-nStart == -1 .AND. aPos[POS_B_000]) ) )
            ELSE
               EXIT
            ENDIF
         ENDIF
      ENDDO
   NEXT
   RETURN aMoves

STATIC FUNCTION isMoveCorrect( nMove )

   LOCAL arr, l
   LOCAL aPosTemp := Array(POS_LEN)

   IF nMove == aMoveState[2]
      RETURN .F.
   ENDIF
   arr := chess_GenMoves( aCurrPos, aMoveState[2] )
   l := ( Ascan( arr, nMove ) ) > 0
   IF l
      ACopy( aCurrPos, aPosTemp, 2,, 2 )
      PostProcess( aPosTemp, aCurrPos[POS_BOARD], ;
         hb_bPeek( aCurrPos[POS_BOARD],aMoveState[2] ), aMoveState[2], nMove )
      l := !Check4Shah( aPosTemp, .F. )
      IF !l
         edi_Alert( Iif( lRussian, "Шах...", "Check..." ) )
      ENDIF
   ENDIF
   RETURN l

/* Функция осуществляет перемещение фигуры и обрабатывает следующие ситуации:
     - рокировка (добавляет перемещение ладьи)
     - перемещение ладьи - ставит запрет соответствующей рокировки
     - перемещение пешки на два поля
     - перемещение пешки - взятие на проходе
     - перемещение пешки на последнюю горизонталь (установка новой фигуры)
 */
STATIC FUNCTION PostProcess( aPos, cBoard, nFig, nStart, nEnd, nNewFig, lAskNewFig )

   LOCAL j1 := nEnd - nStart, nPos4p := aPos[POS_4P]
   STATIC aMenuR := { "ферзь", "ладья", "конь", "слон" }, aMenuE := { "queen", "rook", "knight", "bishop" }, aW := { 'Q', 'R', 'N', 'B' }, aB := { 'q', 'r', 'n', 'b' }

   aPos[POS_BOARD] := hb_bPoke( hb_bPoke( cBoard, nStart, 32 ), nEnd, nFig )
   aPos[POS_4P] := 0
   IF nFig == 75   // 'K'
      aPos[POS_W00] := aPos[POS_W000] := .F.
      IF j1 == 2
         // Короткая рокировка
         aPos[POS_BOARD] := hb_bPoke( hb_bPoke( aPos[POS_BOARD], 64, 32 ), 62, 82 )
      ELSEIF j1 == -2
         // Длинная рокировка
         aPos[POS_BOARD] := hb_bPoke( hb_bPoke( aPos[POS_BOARD], 57, 32 ), 60, 82 )
      ENDIF
   ELSEIF nFig == 107   // 'k'
      aPos[POS_B00] := aPos[POS_B000] := .F.
      IF j1 == 2
         // Короткая рокировка
         aPos[POS_BOARD] := hb_bPoke( hb_bPoke( aPos[POS_BOARD], 8, 32 ), 6, 114 )
      ELSEIF j1 == -2
         // Длинная рокировка
         aPos[POS_BOARD] := hb_bPoke( hb_bPoke( aPos[POS_BOARD], 1, 32 ), 4, 114 )
      ENDIF
   ELSEIF nFig == 82   // 'R'
      IF nStart == 64
         aPos[POS_W00] := .F.
      ELSEIF nStart == 57
         aPos[POS_W000] := .F.
      ENDIF
   ELSEIF nFig == 114  // 'r'
      IF nStart == 8
         aCurrPos[POS_B00] := .F.
      ELSEIF nStart == 1
         aCurrPos[POS_B000] := .F.
      ENDIF
   ELSEIF nFig == 80  // 'P'
      IF j1 == -16
         IF hb_bPeek( aPos[POS_BOARD], nEnd-1 ) == 112 .OR. hb_bPeek( aPos[POS_BOARD], nEnd+1 ) == 112 // 'p'
            aPos[POS_4P] := nEnd + 8
         ENDIF
      ELSEIF nEnd == nPos4p
         aPos[POS_BOARD] := hb_bPoke( aPos[POS_BOARD], nEnd+8, 32 )
      ELSEIF nEnd <= 8
         IF Empty( lAskNewFig ) .AND. Empty( nNewFig )
            nNewFig := 81
         ENDIF
         IF Empty( nNewFig )
            j1 := 1
            IF nLevelWhite == 0 .AND. ( j1 := FMenu( oGame, Iif(lRussian,aMenuR,aMenuE), y1t, x2t+2 ) ) == 0
               j1 := 1
            ENDIF
            nNewFig := Asc( aW[j1] )
         ENDIF
         aPos[POS_BOARD] := hb_bPoke( aPos[POS_BOARD], nEnd, nNewFig )
      ENDIF
   ELSEIF nFig == 112  // 'p'
      IF j1 == 16
         IF hb_bPeek( aPos[POS_BOARD], nEnd-1 ) == 80 .OR. hb_bPeek( aPos[POS_BOARD], nEnd+1 ) == 80 // 'P'
            aPos[POS_4P] := nEnd - 8
         ENDIF
      ELSEIF nEnd == nPos4p
         aPos[POS_BOARD] := hb_bPoke( aPos[POS_BOARD], nEnd-8, 32 )
      ELSEIF nEnd >= 57
         IF Empty( lAskNewFig ) .AND. Empty( nNewFig )
            nNewFig := 113
         ENDIF
         IF Empty( nNewFig )
            j1 := 1
            IF nLevelBlack == 0 .AND. ( j1 := FMenu( oGame, Iif(lRussian,aMenuR,aMenuE), y1t, x2t+2 ) ) == 0
               j1 := 1
            ENDIF
            nNewFig := Asc( aB[j1] )
         ENDIF
         aPos[POS_BOARD] := hb_bPoke( aPos[POS_BOARD], nEnd, nNewFig )
      ENDIF
   ENDIF

   RETURN Nil

/* Returns .F., if there is no enemy king on the board
 */
STATIC FUNCTION ii_Check4King( cBoard )

   LOCAL cFig, j, cFigK := Iif( lTurnBlack, 'K', 'k' )

   FOR EACH cFig IN cBoard
      IF cFig == cFigK
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION ii_Ocenka( cBoard )

   LOCAL cFig, j, nSumm := 0

   FOR EACH cFig IN cBoard
      IF cFig >= 'a'
         j := Ascan( aFigs, cFig ) - 2
         IF j == 0; j := 1; ENDIF
         nSumm -= aBoardValues[2,j,cFig:__enumindex]
      ELSEIF cFig >= 'A'
         j := Ascan( aFigs, Lower( cFig ) ) - 2
         IF j == 0; j := 1; ENDIF
         nSumm += aBoardValues[1,j,cFig:__enumindex]
      ENDIF
   NEXT

   RETURN nSumm

STATIC FUNCTION ii_ScanBoard_1( aPos, lReply, lSh )

   LOCAL i, j, nFig, arr, nLen, cBoard := aPos[POS_BOARD], nSumm
   LOCAL aMaxOcen := { Nil, Nil, -1000000 }, aReply, lNotShah := .F.
   LOCAL aPosTemp := Array(POS_LEN)

   ACopy( aPos, aPosTemp, 2,, 2 )

   Set_lb_lw( aPos, lTurnBlack )
   IF lDebug; DbgMsg( "---" ); ENDIF
   FOR i := 1 TO 64
      IF ( nFig := hb_bPeek( cBoard, i ) ) >= 65 .AND. ;
         ( ( lTurnBlack .AND. nFig >= 97 ) .OR. ( !lTurnBlack .AND. nFig < 97 ) )
         arr := chess_GenMoves( aPos, i )
         nLen := Len( arr )
         FOR j := 1 TO nLen
            PostProcess( aPosTemp, cBoard, nFig, i, arr[j] )
            IF lReply
               IF lSh
                  // Если мы делали шах, проверяем, отразил ли его ответ
                  IF Check4Shah( aPosTemp, .F. )   // Шах противнику?
                     LOOP
                  ELSE
                     lNotShah := .T.
                  ENDIF
               ELSEIF Check4Mate( aPosTemp, .T., .T. )   // Мат нам?
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := BEATKING
                  IF lDebug; DbgMsg( aPosTemp, ">", 2, i, arr[j],, aMaxOcen[3] ); ENDIF
                  RETURN aMaxOcen
               ENDIF
               nSumm := Iif( lTurnBlack, -ii_Ocenka( aPosTemp[POS_BOARD] ), ii_Ocenka( aPosTemp[POS_BOARD] ) )
               IF nSumm > aMaxOcen[3] .OR. ( nSumm == aMaxOcen[3] .AND. hb_RandomInt(1,2) == 1 )
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := nSumm
               ENDIF
               IF lDebug; DbgMsg( aPosTemp, ">", 2, i, arr[j], nSumm, aMaxOcen[3] ); ENDIF
            ELSE
               IF !ii_Check4King( aPosTemp[POS_BOARD] )
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := BEATKING
                  RETURN aMaxOcen
               ENDIF
               IF lDebug; DbgMsg( aPosTemp, ">", 1, i, arr[j],, aMaxOcen[3] ); ENDIF
               lSh := Check4Shah( aPosTemp, .T. )   // Шах противнику?
               IF Check4Shah( aPosTemp, .F. )       // Шах нам?
                  // Если нам шах, пропускаем этот ход - он запрещен!
                  LOOP
               ENDIF
               lTurnBlack := !lTurnBlack
               aReply := ii_ScanBoard_1( aPosTemp, .T., lSh )
               lTurnBlack := !lTurnBlack
               nSumm := -aReply[3]
               IF nSumm > aMaxOcen[3] .OR. ( nSumm == aMaxOcen[3] .AND. hb_RandomInt(1,2) == 1 )
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := nSumm
               ENDIF
               IF lDebug; DbgMsg( aPosTemp, "=", 1, i, arr[j], nSumm, aMaxOcen[3] ); ENDIF
            ENDIF
         NEXT
      ENDIF
   NEXT
   IF lReply .AND. lSh .AND. !lNotShah
      aMaxOcen[3] := -BEATKING
   ENDIF

   RETURN aMaxOcen

STATIC FUNCTION ii_ScanBoard_2( aPos, lReply, nDeep, lSh )

   LOCAL i, j, nFig, arr, nLen, cBoard := aPos[POS_BOARD], nSumm
   LOCAL aMaxOcen := { Nil, Nil, -1000000 }, aReply, lNotShah := .F.
   LOCAL aPosTemp := Array(POS_LEN)

   ACopy( aPos, aPosTemp, 2,, 2 )

   Set_lb_lw( aPos, lTurnBlack )
   IF lDebug; DbgMsg( "---" ); ENDIF
   FOR i := 1 TO 64
      IF ( nFig := hb_bPeek( cBoard, i ) ) >= 65 .AND. ;
         ( ( lTurnBlack .AND. nFig >= 97 ) .OR. ( !lTurnBlack .AND. nFig < 97 ) )
         arr := chess_GenMoves( aPos, i )
         nLen := Len( arr )
         FOR j := 1 TO nLen
            PostProcess( aPosTemp, cBoard, nFig, i, arr[j] )
            IF nDeep == 1
               IF !ii_Check4King( aPosTemp[POS_BOARD] )
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := BEATKING
                  RETURN aMaxOcen
               ENDIF
               nSumm := Iif( lTurnBlack, -ii_Ocenka( aPosTemp[POS_BOARD] ), ii_Ocenka( aPosTemp[POS_BOARD] ) )
               IF nSumm > aMaxOcen[3] .OR. ( nSumm == aMaxOcen[3] .AND. hb_RandomInt(1,2) == 1 )
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := nSumm
               ENDIF
               IF lDebug; DbgMsg( aPosTemp, ">", 3, i, arr[j], nSumm, aMaxOcen[3] ); ENDIF
            ELSEIF nDeep == 2
               IF lSh
                  // Если мы делали шах, проверяем, отразил ли его ответ
                  IF Check4Shah( aPosTemp, .F. )    // Шах противнику?
                     LOOP
                  ELSE
                     lNotShah := .T.
                  ENDIF
               ELSEIF Check4Mate( aPosTemp, .T., .T. )   // Мат нам?
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := BEATKING
                  IF lDebug; DbgMsg( aPosTemp, ">", 2, i, arr[j],, aMaxOcen[3] ); ENDIF
                  RETURN aMaxOcen
               ENDIF
               lTurnBlack := !lTurnBlack
               aReply := ii_ScanBoard_2( aPosTemp, .F., nDeep-1 )
               lTurnBlack := !lTurnBlack
               nSumm := -aReply[3]
               IF nSumm > aMaxOcen[3] .OR. ( nSumm == aMaxOcen[3] .AND. hb_RandomInt(1,2) == 1 )
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := nSumm
               ENDIF
               IF lDebug; DbgMsg( aPosTemp, ">", 2, i, arr[j],, aMaxOcen[3] ); ENDIF
            ELSE
               IF !ii_Check4King( aPosTemp[POS_BOARD] )
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := BEATKING
                  RETURN aMaxOcen
               ENDIF
               IF lDebug; DbgMsg( aPosTemp, ">", 1, i, arr[j],, aMaxOcen[3] ); ENDIF
               // Проверяем, делаем ли мы шах этим ходом
               lSh := Check4Shah( aPosTemp, .T. )   // Шах противнику?
               IF Check4Shah( aPosTemp, .F. )
                  // Если нам шах, пропускаем этот ход - он запрещен!
                  LOOP
               ENDIF
               lTurnBlack := !lTurnBlack
               aReply := ii_ScanBoard_2( aPosTemp, .T., nDeep-1, lSh )
               lTurnBlack := !lTurnBlack
               nSumm := -aReply[3]
               IF nSumm > aMaxOcen[3] .OR. ( nSumm == aMaxOcen[3] .AND. hb_RandomInt(1,2) == 1 )
                  aMaxOcen[1] := i; aMaxOcen[2] := arr[j]; aMaxOcen[3] := nSumm
               ENDIF
               IF lDebug; DbgMsg( aPosTemp, "=", 1, i, arr[j], nSumm, aMaxOcen[3] ); ENDIF
            ENDIF
         NEXT
      ENDIF
   NEXT
   IF lReply .AND. lSh .AND. !lNotShah
      aMaxOcen[3] := -BEATKING
   ENDIF

   RETURN aMaxOcen

STATIC FUNCTION ii_MakeMove( lSrazu )

   LOCAL cFig, nSec, nCou := 0, nKey
   LOCAL aMaxOcen, cBoa, cMoves, n, lFromOpn := .F., lBuilt_in := .F., lMate

   DrawMove( {'@'} )

   nSec := Seconds()
   IF ( n := Iif( lTurnBlack, nLevelBlack, nLevelWhite ) ) == 10
      aMaxOcen := ii_SunfishMove( lSrazu )
   ELSEIF lOpenings .AND. Len(aHistory) <= 10 .AND. openings->(dbSeek( board_64to32( aCurrPos[POS_BOARD] ) ))
      cMoves := hb_strReplace( openings->MOVES, "z" )
      //hb_memoWrit( "a1.move", ltrim(str(openings->(recno())))+" "+cMoves )
      n := hb_RandomInt( 1, Len(cMoves)/2 )
      aMaxOcen := { hb_BPeek( cMoves,(n-1)*2+1 ), hb_BPeek( cMoves,(n-1)*2+2 ), 0 }
      lFromOpn := .T.
   ELSEIF !Empty( aMaxOcen := ii_Openings() )
      lBuilt_in := .T.
   ELSEIF n == 1
      aMaxOcen := ii_ScanBoard_1( aCurrPos, .F. )
   ELSE
      aMaxOcen := ii_ScanBoard_2( aCurrPos, .F., nDeep2 )
   ENDIF
   lDebug := .F.
   @ y1t+Iif(lGui,Iif(guiBoaSize==3,13,18),11), x1t+2 SAY ;
      Ltrim(Str( Seconds()-nSec,6,2 )) + Iif(lFromOpn,"bd", Iif(lBuilt_in, "in", "  "))

   IF aMaxOcen[1] == Nil
      GameOver( 1 )  // Победа
   ELSE
      lMate := DrawMove( {cFig := Substr( aCurrPos[POS_BOARD], aMaxOcen[1], 1 ), aMaxOcen[1], amaxOcen[2]} )
      IF !lTurnBlack
         cBoa := ATail(aHistory)[3]
         AEval( aHistory, {|a|nCou := Iif(Len(a)>2.AND.a[3]==cBoa,nCou+1,nCou)} )
      ENDIF
      IF lMate // lShah .AND. Check4Mate( aCurrPos, .T. )
         GameOver( 1 )  // Победа
      ELSEIF aMaxOcen[3] == BEATKING
         GameOver( 2 )  // Поражение
      ELSEIF !lTurnBlack .AND. nCou >= 5
         GameOver( 3 )  // Ничья
      ENDIF
   ENDIF

   lTurnBlack := !lTurnBlack
   IF Iif( lTurnBlack, nLevelBlack, nLevelWhite ) > 0
       IF ( nKey := Inkey( 1 ) ) == K_F6
         _Game_Players( .T. )
       ELSE
          KEYBOARD Chr( K_CTRL_N )
       ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION ii_Openings()

   LOCAL nLen := Iif( lTurnBlack, Len(aHistory)*2-1, Len(aHistory)*2 ), i, a, s := ""
   STATIC aFirst := { "e2e4", "d2d4" } // Первый ход белых
   STATIC aOpenings := { ;
      ; // 1-й ход черных
      { { "e2e4/", "e7e5","e7e6","c7c5","e7e5","g8f6" }, ;
        { "d2d4/", "d7d5","g8f6","d7d5" } ;
      }, ;
      ; // Второй ход белых
      { { "e2e4/e7e5/", "g1f3" }, ;
        { "e2e4/e7e6/", "d2d4" }, ;
        { "d2d4/g8f6/", "c2c4" } ;
      }, ;
      ; // Второй ход черных
      { { "e2e4/e7e5/g1f3/", "b8c6" }, ;
        { "e2e4/e7e6/d2d4/", "d7d5" }, ;
        { "d2d4/g8f6/c2c4/", "e7e6", "d7d6", "g7g6" } ;
      }, ;
      ; // Третий ход белых
      { { "e2e4/e7e5/g1f3/b8c6/", "f1b5","f1c4","f1b5","d2d4","b1c3" }, ;
        { "e2e4/e7e6/d2d4/d7d5/", "e4e5","b1d2" }, ;
        { "d2d4/g8f6/c2c4/e7e6/", "b1c3", "g1f3" } ;
      }, ;
      ; // Третий ход черных
      { { "e2e4/e7e5/g1f3/b8c6/f1b5", "a7a6","g8f6","d7d6","g7g6" } ;
      } ;
   }

   IF nLen == 0
      i := hb_RandomInt( 1, Len(aFirst) )
      RETURN { MoveC2N( aFirst[i],0 ), MoveC2N( aFirst[i],2 ), 0 }
   ELSEIF nLen <= Len( aOpenings )
      FOR EACH a IN aHistory
         s += MoVEN2C( a[1,2] ) + MoVEN2C( a[1,3] ) + "/"
         IF a[2] != Nil
            s += MoVEN2C( a[2,2] ) + MoVEN2C( a[2,3] ) + "/"
         ELSE
            EXIT
         ENDIF
      NEXT
      //edi_writelog(s)
      FOR EACH a IN aOpenings[nLen]
         IF a[1] == s
            i := hb_RandomInt( 2, Len(a) )
            RETURN { MoveC2N( a[i],0 ), MoveC2N( a[i],2 ), 0 }
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

STATIC FUNCTION GameOver( nRes )

   IF nRes == 1
      edi_Alert( Iif( lRussian, "Поздравляем! Вы выиграли", "Congratulations! You are won!" ) )
   ELSEIF nRes == 2
      edi_Alert( Iif( lRussian, "Увы, вы проиграли...", "You lost the game..." ) )
   ELSE
      edi_Alert( Iif( lRussian, "Ничья. Партия окончена!", "The draw. Game over!" ) )
   ENDIF
   lPlayGame := .F.

   RETURN Nil

STATIC FUNCTION AddHis( cBoard, cFig, nStart, nEnd, cFigBeat, lSh )

   LOCAL arr := { cFig, nStart, nEnd, cFigBeat, lSh, "" }

   IF cFig != Substr( aCurrPos[POS_BOARD], nEnd, 1 )
      arr[6] := Substr( aCurrPos[POS_BOARD], nEnd, 1 )
   ENDIF
   IF lTurnBlack
      ATail( aHistory )[2] := arr
   ELSE
      AAdd( aHistory, { arr, Nil, board_64to32(cBoard) } )
   ENDIF

   RETURN Nil

STATIC FUNCTION DbgMsg( aPos, cPrefix, nDeep, n1, n2, ns1, ns2 )

   IF Valtype( aPos ) == "C"
      edi_Writelog( aPos )
   ELSE
      edi_Writelog( Space( nDeep*2 ) + cPrefix + " " + MoveN2C( n1,n2 ) + "  " + ;
         Iif( ns1 == Nil, Space(8), str(ns1,8) ) + "  " + str( ns2,8 ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION board_64to32( cBoard )

   LOCAL cRes := "", i, cf := " prnbqkPRNBQK"

   FOR i := 1 TO 63 STEP 2
      cRes += Chr( At( Substr(cBoard,i,1), cf ) + ;
         hb_BitShift( At( Substr(cBoard,i+1,1), cf ), 4 ) )
   NEXT

   RETURN cRes

STATIC FUNCTION board_32to64( cBoard )

   LOCAL cRes := "", i, n, cf := " prnbqkPRNBQK"

   FOR i := 1 TO 32
      n := Asc( Substr(cBoard,i,1) )
      cRes += Substr( cf, hb_BitAnd( n, 0xf ), 1 ) + ;
         Substr( cf, hb_bitShift( hb_BitAnd( n, 0xf0 ), -4 ), 1 )
   NEXT

   RETURN cRes

STATIC FUNCTION pgn_ReadHead( cBuff, nPos, cTag, lCutoff )

   LOCAL cRes := "", nPos1, nPos2

   IF ( nPos1 := hb_At( "["+cTag+" ", cBuff, nPos ) ) > 0
      nPos1 := cedi_strSkipChars( cBuff, nPos1+Len(cTag)+2 )
      nPos2 := cedi_strpbrk( "]", cBuff, nPos1 )
      cRes := Trim( StrTran( Substr( cBuff, nPos1, nPos2-nPos1 ), '"', '' ) )
      IF !Empty(lCutoff) .AND. (nPos1 := At( ',',cRes )) > 0
         cRes := Trim( Left( cRes, nPos1-1 ) )
      ENDIF
      //edi_Writelog( str(nPos) + ctag + " " + str(nPos1) + " " + str(nPos2) + " " + cRes )
   ENDIF

   RETURN cRes

STATIC FUNCTION chess_board2FEN( cBoard, cTurn, nTurn )

   LOCAL cFen := "", i, j, c, n

   FOR i := 1 TO 8
      n := 0
      FOR j := 1 TO 8
         IF ( c := Substr( cBoard, (i-1)*8+j, 1 ) ) == ' '
            n ++
         ELSE
            IF n > 0
               cFen += Ltrim(Str(n))
               n := 0
            ENDIF
            cFen += c
         ENDIF
      NEXT
      IF n > 0
         cFen += Ltrim(Str(n))
      ENDIF
      cFen += Iif( i == 8, ' ', '/' )
   NEXT

   RETURN cFen + cTurn + ' - 0 ' + Ltrim(Str(nTurn))

STATIC FUNCTION chess_FEN2board( cFen, cTurn )

   LOCAL cBoard := "", i, c

   //edi_writelog( cFen )
   FOR i := 1 TO Len( cFen )
      IF IsDigit( c := Substr( cFen, i, 1 ) )
         cBoard += Space( Val(c) )
      ELSEIF c $ "prnbqkPRNBQK"
         cBoard += c
      ELSEIF c == ' '
         EXIT
      ELSEIF c != '/'
         //edi_writelog( c + "!!!" )
         RETURN Nil
      ENDIF
   NEXT
   IF i <= Len( cFen )
      cFen := Ltrim(cFen)
      cTurn := Left( cFen,1 )
   ENDIF

   //edi_writelog( cBoard )
   RETURN Iif( Len(cBoard) == 64, cBoard, Nil )

STATIC FUNCTION pgnrec2Move( aPos, cRec, lBlack )

   LOCAL aMove := Array( MOVE_LEN ), cPos, nPos := Len( cRec ), c, n, cFrom, nFrom
   LOCAL i, nFig, arr

   aMove[4] := aMove[6] := ' '; aMove[5] := .F.
   DO WHILE ( cPos := Substr( cRec, nPos, 1 ) ) $ "+!?#"
      IF cPos == '+'
         aMove[5] := .T.
      ENDIF
      nPos --
   ENDDO
   IF ( cPos := Substr( cRec, nPos-1, 2 ) ) == "-O"
      aMove[1] := Iif( lBlack, 'k', 'K' )
      aMove[2] := Iif( lBlack, 5, 61 )
      IF nPos == 3
         aMove[3] := Iif( lBlack, 7, 63 )
      ELSE
         aMove[3] := Iif( lBlack, 3, 59 )
      ENDIF
   ELSE
      IF ( c := Substr( cRec, nPos, 1 ) ) $ "RNBQ"
         aMove[6] := c
         nPos --
         IF Substr( cRec, nPos, 1 ) == '='
            nPos --
         ENDIF
      ENDIF
      IF !IsDigit( c := Substr( cRec, nPos, 1 ) ) .OR. (n := Val( c )) < 1 .OR. n > 8
         RETURN Nil
      ENDIF
      IF ( c := Substr( cRec, --nPos, 1 ) ) < 'a' .OR. c > 'h'
         RETURN Nil
      ENDIF
      aMove[3] := (8-n) * 8 + Asc(c)-96
      IF nPos > 1 .AND. ( c := Substr( cRec, nPos-1, 1 ) ) == 'x'
         nPos --
      ENDIF
      IF nPos > 1 .AND. ( ( ( c := Substr( cRec, nPos-1, 1 ) ) >= 'a' .AND. c <= 'h' ) ;
         .OR. IsDigit(c) )
         IF IsDigit( c )
            nFrom := Val(c)
         ELSE
            cFrom := c
         ENDIF
         nPos --
      ENDIF
      IF nPos > 1
         IF ( c := Substr( cRec, --nPos, 1 ) ) >= 'A' .AND. c <= 'Z'
            aMove[1] := Iif( lBlack, Lower(c), c )
         ELSE
            RETURN Nil
         ENDIF
      ELSE
         aMove[1] := Iif( lBlack, 'p', 'P' )
      ENDIF
      FOR i := 1 TO 64
         nFig := hb_bPeek( aPos[POS_BOARD], i )
         IF nFig == Asc( aMove[1] )
            arr := chess_GenMoves( aPos, i )
            //IF Ascan( arr,aMove[3] ) > 0 .AND. ( Empty(cFrom) .OR. Chr(96 + Iif(i%8==0,8,i%8)) == cFrom )
            IF Ascan( arr,aMove[3] ) > 0 .AND. ( Empty(cFrom) .OR. Chr(96 + Iif(i%8==0,8,i%8)) == cFrom ) ;
               .AND. ( Empty(nFrom) .OR. Int(9-i/8) == nFrom )
               aMove[2] := i
               EXIT
            ENDIF
         ENDIF
      NEXT
      IF Empty( aMove[2] )
         RETURN Nil
      ENDIF
   ENDIF

   RETURN aMove

STATIC FUNC dbOpn_Open()    // Открывает базу дебютов

   LOCAL bOldError := ErrorBlock( { |e|break( e ) } ), lRes := .T.

   lOpenings := .F.
   BEGIN SEQUENCE
      dbUseArea( .T., "DBFCDX", cOpenings, "OPENINGS", .T., .F. )
   RECOVER
      lRes := .F.
   END SEQUENCE

   IF lRes
      ordSetFocus( "BOARD" )
      lOpenings := .T.
   ENDIF
   Errorblock( bOldError )

   RETURN Nil

STATIC FUNCTION chess_ReplayGame( aHis )

   LOCAL i, aMove

   lReplayMode := .T.
   aHistory  := {}
   lTurnBlack := .F.
   FOR i := 1 TO Len( aHis )
      aMove := aHis[i,1]
      Set_lb_lw( aCurrPos, lTurnBlack )
      DrawMove( aMove )
      lTurnBlack := .T.
      IF !Empty( aMove := aHis[i,2] )
         Set_lb_lw( aCurrPos, lTurnBlack )
         DrawMove( aMove )
         lTurnBlack := .F.
      ENDIF
   NEXT
   lReplayMode := .F.
   DrawBoard()
   //edi_writelog( hb_valtoexp( lTurnBlack ) )
   //edi_writelog( hb_valtoexp( aHis ) )

   RETURN Nil

STATIC FUNCTION chess_Load()

   LOCAL xFileName, nPos := 1, nPos2, cHea, cFen, cBoard, cTurn
   LOCAL cScBuf := Savescreen( y1t, x2t+2, y1t+10, x2t+40 ), cBuff, aMenu := {}, nc
   LOCAL nTurn, cRec, aPos, aMove, aHis, i
   LOCAL cWhite, cBlack, cResult

   xFileName := edi_SeleFile( oGame, cIniPath, y1t, x2t+2, y1t+10, x2t+40, "*.pgn" )
   Restscreen( y1t, x2t+2, y1t+10, x2t+40, cScBuf )

   IF Empty( xFileName )
      RETURN Nil
   ENDIF
   cBuff := MemoRead( xFileName[1] )
   DO WHILE ( nPos := hb_At( "[Event ", cBuff, nPos ) ) > 0
      cHea := pgn_ReadHead( cBuff, nPos, "Date" ) + ", " + pgn_ReadHead( cBuff, nPos, "White", .T. ) + ;
         " - " + pgn_ReadHead( cBuff, nPos, "Black", .T. )
      IF !Empty( cHea )
         AAdd( aMenu, { cHea, Nil, nPos } )
      ENDIF
      nPos ++
   ENDDO
   IF !Empty( aMenu ) .AND. ( nPos := FMenu( oGame, aMenu, y1t, x2t-2, y1t+10, x2t+44,,,, .T. ) ) > 0
      cBuff := Substr( cBuff, aMenu[nPos,3], ;
         Iif( nPos==Len(aMenu), Len(cBuff), aMenu[nPos+1,3] ) - aMenu[nPos,3] )
      IF Empty( cFen := pgn_ReadHead( cBuff, 1, "FEN" ) )
         // Loading a game
         IF ( nPos := At( Chr(10)+"1.", cBuff ) ) == 0
            edi_Alert( "Error reading a game" )
         ELSE
            cWhite := pgn_ReadHead( cBuff, 1, "White", .T. )
            cBlack := pgn_ReadHead( cBuff, 1, "Black", .T. )
            cResult:= pgn_ReadHead( cBuff, 1, "Result" )
            cBuff := Trim( hb_strReplace( Substr( cBuff,nPos+1 ), {Chr(10),Chr(13)}, {" ","" } ) )
            //edi_Writelog( cBuff )
            nTurn := 1
            aHis := {}
            aPos := Array(POS_LEN)
            aPos[POS_BOARD] := cInitBoard
            aPos[POS_W00] := aPos[POS_W000] := aPos[POS_B00] := aPos[POS_B000] := aPos[POS_W_00] := aPos[POS_W_000] := aPos[POS_B_00] := aPos[POS_B_000] := .T.
            nPos := 1
            DO WHILE .T.
               IF ( nPos := hb_At( (cRec := Ltrim(Str(nTurn))+'.'), cBuff, nPos ) ) == 0
                  EXIT
               ENDIF
               nTurn ++
               nPos += Len( cRec )
               nPos := cedi_strSkipChars( cBuff, nPos )
               IF ( nPos2 := hb_At( ' ', cBuff, nPos ) ) == 0
                  nPos2 := Len( cBuff ) + 1
               ENDIF
               cRec := Substr( cBuff, nPos, nPos2-nPos )
               IF ( aMove := pgnrec2Move( aPos, cRec, .F. ) ) == Nil
                  edi_Alert( Ltrim(Str(nTurn-1))+'.'+cRec+" : Wrong data." )
                  RETURN Nil
               ENDIF
               Aadd( aHis, { aMove, Nil } )
               PostProcess( aPos, aPos[POS_BOARD], Asc(aMove[1]), aMove[2], aMove[3], Iif(Empty(aMove[6]),Nil,Asc(aMove[6])) )
               nPos := cedi_strSkipChars( cBuff, nPos2 )
               IF ( nPos2 := hb_At( ' ', cBuff, nPos ) ) == 0
                  nPos2 := Len( cBuff ) + 1
               ENDIF
               IF !Empty( cRec := Substr( cBuff, nPos, nPos2-nPos ) ) .AND. ;
                  !( cRec == "1-0" ) .AND. !( cRec == "0-1" ) .AND. !( cRec == "1/2-1/2" )
                  IF ( aMove := pgnrec2Move( aPos, cRec, .T. ) ) == Nil
                     edi_Alert( Ltrim(Str(nTurn-1))+'. ... '+cRec+" : Wrong data." )
                     RETURN Nil
                  ENDIF
                  ATail( aHis )[2] := aMove
                  PostProcess( aPos, aPos[POS_BOARD], Asc(aMove[1]), aMove[2], aMove[3], Iif(Empty(aMove[6]),Nil,Asc(aMove[6])) )
               ENDIF
            ENDDO
            aMenu := { "View game", "Continue game" }
            IF ( nc := FMenu( oGame, aMenu, y1t, x2t+2, y1t+3, x2t+26 ) ) == 0
               nc := 1
            ENDIF
            SetColor( clrBoard )
            Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
            nLevelWhite := nLevelBlack := 0
            aMoveState[2] := aMoveState[3] := 0
            aCurrPos[POS_BOARD] := cInitBoard
            aCurrPos[POS_W00] := aCurrPos[POS_W000] := aCurrPos[POS_B00] := aCurrPos[POS_B000] := .T.
            @ y1t-1, x2t+4 SAY Iif( nc == 2, "Computer", "Human" )
            @ y1t-1, x2t+19 SAY Iif( nc == 1, "Computer", "Human" )
            nScrolled := 0
            lTurnBlack := .F.
            DrawBoard()
            IF nc == 1
               aHisView  := aHis
               nLevelWhite := nLevelBlack := 0
               _Game_New( .T. )
               lPlayGame := .F.
               lViewGame := .T.
               i := y1t + Iif( lGui, Iif(guiBoaSize==3,13,18),11 )
               @ i, x1t SAY "White: " + cWhite
               @ i+1, x1t SAY "Black: " + cBlack
               IF !Empty( cResult )
                  @ i+2, x1t+7 SAY cResult
               ENDIF
               @ i+3, x1t SAY "Press SPACE for a next turn"
            ELSE
               lPlayGame := .T.
               lViewGame := .F.
               chess_ReplayGame( aHis )
               DrawBoard()
               _Game_Players( .T. )
            ENDIF
         ENDIF
      ELSE
         // Loading a diagram
         cBoard := chess_FEN2board( cFen, @cTurn )
         IF cBoard == Nil
            edi_Alert( "FEN corrupted" )
         ELSE
            aCurrPos[POS_BOARD] := cBoard
            DrawBoard()
            _Game_DiagNew( cTurn )
            @ y1t+10, x1t SAY pgn_ReadHead( cBuff, 1, "Event" )
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION chess_Save()

   LOCAL cBuff, df := Set( 4, "yyyy.mm.dd" ), i, cMove, cLine, cTurn
   LOCAL aMenu := { "As a game", "As a diagramm" }, nc

   IF ( nc := FMenu( oGame, aMenu, y1t, x2t+2, y1t+3, x2t+26 ) ) == 0
      RETURN Nil
   ENDIF

   cBuff := '[Event ""]'+Chr(10)+'[Site ""]'+Chr(10)+'[Date "'+Dtoc(Date())+'"]'+Chr(10)+ ;
      '[Round ""]'+Chr(10)+'[White "Me"]'+Chr(10)+'[Black "Hbedit Chess"]'+Chr(10)+ ;
      '[Result "1-0"]'+Chr(10)

   Set( 4, df )

   cLine := ""
   IF nc == 1
      cBuff += Chr(10)
      FOR i := 1 TO Len( aHistory )
         //edi_writelog( hb_valtoexp( aHistory[i] ) )
         cMove := Iif( i > 1, " ", "" ) + Ltrim(Str(i)) + "." + ;
            Iif( Lower(aHistory[i,1,1])=='p',"",Upper(aHistory[i,1,1]) ) + ;
            Iif(Empty(aHistory[i,1,4]),"","x") + ;
            MoveN2C( , aHistory[i,1,3] ) + Iif(Empty(aHistory[i,1,5]),"","+") + " " + ;
            Iif( !Empty(aHistory[i,2]), Iif( Lower(aHistory[i,2,1])=='p',"",Upper(aHistory[i,2,1]) ) + ;
            Iif(Empty(aHistory[i,2,4]),"","x") + ;
            MoveN2C( , aHistory[i,2,3] ) + Iif(Empty(aHistory[i,2,5]),"","+"), "" )
         IF Len( cLine ) + Len( cMove ) < 76
            cLine += cMove
         ELSE
            cBuff += cLine + Chr(10)
            cLine := cMove
         ENDIF
      NEXT
      cBuff += cLine + Chr(10) + Chr(10)
   ELSE
      cTurn := Iif( Empty(aHistory) .OR. Atail(aHistory)[2] != Nil, 'w', 'b' )
      cLine := '[FEN "' + chess_board2FEN( aCurrPos[POS_BOARD], cTurn, 1 ) + '"]' + Chr(10) + ;
      '[Setup "1"]' + Chr(10) + '*'
      cBuff += cLine + Chr(10) + Chr(10)
   ENDIF
   hb_MemoWrit( cIniPath + "test.pgn", hb_MemoRead( cIniPath + "test.pgn" ) + cBuff )
   edi_Alert( "Saved in test.pgn" )

   RETURN Nil

STATIC FUNCTION chess_Help()

   LOCAL cBuff := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
   LOCAL oldc := SetColor( clrWhite+"/"+clrbBlack )

   hb_cdpSelect( "RU866" )
   @ y1t, x1t, y1t+12, x2t+36 BOX "�Ŀ����� "
   hb_cdpSelect( oGame:cp )

   @ y1t+1, x1t + 4 SAY Iif( lRussian, "Шахматы", "Chess game" )
   @ y1t+2, x1t + 4 SAY Iif( lRussian, "F9 - Главное меню", "F9 - Main menu" )
   @ y1t+3, x1t + 4 SAY Iif( lRussian, "F2 - Сохранить файл, F4 - Загрузить файл", "F2 - Save file, F4 - Load file" )
   @ y1t+4, x1t + 4 SAY Iif( lRussian, "F3 - Новая партия, F5 - Создать диаграмму", "F3 - New game,  F5 - Set a diagramm" )
   @ y1t+5, x1t + 4 SAY Iif( lRussian, "F6 - Изменить игроков", "F6 - Change players order and level" )
   @ y1t+5, x1t + 4 SAY Iif( lRussian, "F8 - Switch Russian/English notation", "F8 - Переключить язык (Русский/Английский)" )
   @ y1t+6, x1t + 4 SAY Iif( lRussian, "Backspace - Вернуть ход назад", "Backspace - Turn back" )
   @ y1t+7, x1t + 4 SAY Iif( lRussian, "Ctrl-N - Предоставить компьютеру право сделать ход", "Ctrl-N - Let computer make a turn" )
   @ y1t+8, x1t + 4 SAY Iif( lRussian, "ESC, F10 - Выход", "ESC, F10 - Exit" )

   Inkey( 0 )
   SetColor( oldc )
   RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cBuff )

   RETURN Nil

STATIC FUNCTION Read_Game_Ini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect, arr, n

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "GAME"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "clrboard" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrBoard := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrwhite" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrWhite := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrBlack" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrBlack := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrbwhite" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrbWhite := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrbBlack" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrbBlack := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "russian" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lRussian := ( Lower( cTemp ) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "drawutf8" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lDrawUtf8 := ( Lower( cTemp ) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "copenings" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cOpenings := cTemp
               ENDIF
            ENDIF
         ENDIF
         IF Upper(aIni[nSect]) == "GUI"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "size-medium" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  IF ( Lower( cTemp ) == "on" )
                     guiBoaSize := 4
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "usertheme" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     cUserTheme := cTemp
                     arr := hb_ATokens( cTemp, ',' )
                     FOR n := 1 TO Max( 6, Len( arr ) )
                        arr[n] := LTrim( arr[n] )
                        aThemes[4,n] := Iif( Asc(arr[n]) == 35, edi_ColorC2N(arr[n]), Val(arr[n]) )
                     NEXT
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "theme" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     nTheme := Val( cTemp )
                     IF nTheme < 1 .OR. nTheme > 4
                        nTheme := 1
                     ENDIF
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "user-theme" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     aThemes[4] := hb_ATokens( cTemp,"," )
                     FOR n := 1 TO Len(aThemes[4])
                        aThemes[4,n] := hb_HexToNum( aThemes[4,n] )
                     NEXT
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "fontname" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiFontName := cTemp
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   guiClrWCell := aThemes[nTheme,1]; guiClrBCell := aThemes[nTheme,2]; guiClrSel := aThemes[nTheme,3]
   guiClrWText := aThemes[nTheme,4]; guiClrBText := aThemes[nTheme,5]
   IF Empty( guiFontName )
      guiFontName := hb_gtinfo( HB_GTI_FONTNAME )
   ENDIF

   RETURN Nil

STATIC FUNCTION Write_Game_Ini()

   LOCAL cr := Chr(13)+Chr(10)
   LOCAL s := "[GAME]" + cr

   s += "clrboard=" + clrBoard + cr
   s += "clrwhite=" + clrWhite + cr
   s += "clrblack=" + clrBlack + cr
   s += "clrbwhite=" + clrbWhite + cr
   s += "clrbblack=" + clrbBlack + cr
   s += "russian=" + Iif( lRussian, "On", "Off" ) + cr
   s += "drawutf8=" + Iif( lDrawUtf8, "On", "Off" ) + cr
   s += "copenings=" + cOpenings + cr

   s += cr + "[GUI]" + cr
   s += "size-medium=" + Iif( guiBoaSize==4, "On", "Off" ) + cr
   IF !Empty( cUserTheme )
      s += "usertheme=" + cUserTheme + cr
   ENDIF
   s += "theme=" + Ltrim(Str( nTheme )) + cr
   s += "user-theme=" + hb_NumToHex(aThemes[4,1],6) + "," + hb_NumToHex(aThemes[4,2],6) + ;
      "," + hb_NumToHex(aThemes[4,3],6) + "," + hb_NumToHex(aThemes[4,4],6) + ;
      "," + hb_NumToHex(aThemes[4,5],6) + cr
   s += "fontname=" + guiFontName + cr

   hb_MemoWrit( cIniPath + "chess.ini", s )

   RETURN Nil

#define HB_GTI_SCREENWIDTH      1
#define HB_GTI_SCREENHEIGHT     2
#define HB_GTI_WINHANDLE        69
#define HB_GTI_MOUSEPOS_XY      70

#define DT_CENTER               1

STATIC FUNCTION chess_Tune()

   LOCAL y1 := y1t + 2, x1 := x2t+1, y2 := y1 + 7, x2 := x1 + 28
   LOCAL cBuf, cp
   LOCAL oldc := SetColor( oGame:cColorSel+","+oGame:cColorSel+",,"+oGame:cColorGet+","+oGame:cColorSel )
   LOCAL aGets
   LOCAL bSet := {||
      guiClrWCell := Iif( Left(aGets[3,4],2) == "0x", hb_HexToNum(Substr(aGets[3,4],3)), Val(aGets[3,4]) )
      guiClrBCell := Iif( Left(aGets[5,4],2) == "0x", hb_HexToNum(Substr(aGets[5,4],3)), Val(aGets[5,4]) )
      guiClrWText := Iif( Left(aGets[7,4],2) == "0x", hb_HexToNum(Substr(aGets[7,4],3)), Val(aGets[7,4]) )
      guiClrBText := Iif( Left(aGets[9,4],2) == "0x", hb_HexToNum(Substr(aGets[9,4],3)), Val(aGets[9,4]) )
      __PaintBo_Chess( , OP_COLORS )
      __PaintBo_Chess( , OP_INVALIDATE )
      RETURN Nil
   }

   aGets := { {y1,x1+4, 11, "Настройка цветов"}, ;
      { y1+1,x1+2, 11, "White cells" }, { y1+1,x1+16, 0, "0x" + hb_NumToHex(guiClrWCell,6), 10, oGame:cColorMenu,oGame:cColorMenu }, ;
      { y1+2,x1+2, 11, "Black cells" }, { y1+2,x1+16, 0, "0x" + hb_NumToHex(guiClrBCell,6), 10, oGame:cColorMenu,oGame:cColorMenu }, ;
      { y1+3,x1+2, 11, "White figures" }, { y1+3,x1+16, 0, "0x" + hb_NumToHex(guiClrWText,6), 10, oGame:cColorMenu,oGame:cColorMenu }, ;
      { y1+4,x1+2, 11, "Black figures" }, { y1+4,x1+16, 0, "0x" + hb_NumToHex(guiClrBText,6), 10, oGame:cColorMenu,oGame:cColorMenu }, ;
      {y1+6,x1+2,2,"[Save]",,,,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {y1+6,x1+10,2,"[Cancel]",,,,{||__KeyBoard(Chr(K_ESC))}}, ;
      {y1+6,x1+20,2,"[Set]",,,, bSet} }

   cBuf := Savescreen( y1, x1, y2, x2 )
   cp := hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "�Ŀ����� "
   @ y1+5, x1 SAY "�"
   @ y1+5, x2 SAY "�"
   @ y1+5, x1+1 TO y1+5, x2-1
   hb_cdpSelect( cp )

   edi_READ( aGets )
   IF LastKey() == 13
      nTheme := 4
      aThemes[4,1] := guiClrWCell
      aThemes[4,2] := guiClrBCell
      aThemes[4,4] := guiClrWText
      aThemes[4,5] := guiClrBText
   ELSE
   ENDIF
   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cBuf )

   RETURN Nil

STATIC FUNCTION chess_Settings()

   LOCAL i, aMenu, aMenu2 := { "Small", "Medium" }

   IF !lGUI
      RETURN Nil
   ENDIF

   aMenu := { "Theme: Gray", "Theme: Brown", "Theme: Blue", "Theme: User", ;
      "Tune", "Size: " + aMenu2[guiBoaSize-2], "Show position " + Iif(lShowPos,"On","Off") }

   i := FMenu( oGame, aMenu, y1t+2, x2t+2, y1t+10, x2t+26 )

   IF i == 7
      lShowPos := !lShowPos
   ELSEIF i == 6
      IF ( i := FMenu( oGame, aMenu2, y1t+2, x2t+4, y1t+5, x2t+20 ) ) > 0 .AND. i+2 != guiBoaSize
         guiBoaSize := i + 2
         __PaintBo_Chess( , OP_SIZE )
         __PaintBo_Chess( , OP_INVALIDATE )
      ENDIF
   ELSEIF i == 5
      chess_Tune()
   ELSEIF i > 0
      guiClrWCell := aThemes[i,1]; guiClrBCell := aThemes[i,2]; guiClrSel := aThemes[i,3]
      guiClrWText := aThemes[i,4]; guiClrBText := aThemes[i,5]
      nTheme := i
      __PaintBo_Chess( , OP_COLORS )
      __PaintBo_Chess( , OP_INVALIDATE )
   ENDIF

   RETURN Nil

STATIC FUNCTION ii_SunfishStart( nWorB )

   LOCAL cExe, xRes

   IF Empty( hExt )
      cExe := cCompiler + " " + cIniPath + "python" + hb_ps() + "plug_sunfish.py"
      IF Empty( hExt := ecli_Run( cExe, nLogLevel,, "sunf_py" ) )
         edi_Alert( "Can't execute python module" )
         RETURN .F.
      ENDIF
   ENDIF
   IF !( (xRes := ecli_RunFunc( hExt, "start",{aCurrPos[POS_BOARD],nWorB} )) == "+" )
      edi_Alert( "Wrong answer of start procedure: " + hb_valtoexp(xRes) )
      RETURN .F.
   ENDIF

   RETURN .T.

STATIC FUNCTION ii_SunfishMove( lSrazu )

   LOCAL arr, cLastMove
   LOCAL sAns

   IF Empty( aHistory ) .OR. !Empty(lSrazu)
      cLastMove := "-"
   ELSE
      arr := ATail( aHistory )[Iif(lTurnBlack,1,2)]
      cLastMove := MoveN2C( arr[2] ) + MoveN2C( arr[3] )
   ENDIF
   ecli_RunFunc( hExt, "makemove", { cLastMove }, .T. )
   arr := hbc_Wndinit( 8, x2t+2, 10, x2t+26,, "" )
   hbc_Wndout( arr, "Wait for answer..." )
   DO WHILE ( sAns := ecli_CheckAnswer( hExt ) ) == Nil
      IF Inkey( 0.02 ) == 27
         EXIT
      ENDIF
   ENDDO
   hbc_Wndclose( arr )
   IF Len( sAns ) == 4
      RETURN { MoveC2N( sAns,0 ), MoveC2N( sAns,2 ), 100 }
   ELSE
      edi_Alert( hb_ValtoExp(sAns) )
   ENDIF

   RETURN { Nil, Nil, Nil }

DYNAMIC GTHWG_PAINT_SETCALLBACK, HWG_INVALIDATERECT, HBRUSH, HPEN, HFONT, HWG_MSGINFO, HWG_MSGYESNO
DYNAMIC HWG_SELECTOBJECT, HWG_RECTANGLE_FILLED, HWG_DRAWLINE, HWG_DRAWTEXT
DYNAMIC HWG_SETTRANSPARENTMODE, HWG_SETTEXTCOLOR

FUNCTION __PaintBo_Chess( hDC, nOp )

   LOCAL x1, y1, x2, y2, nw, nTopMargin
   LOCAL i, j, i1, arrm
   LOCAL lWhiteCell, cBoard, c, nMove
   STATIC xKoef, yKoef
   STATIC oBrushWhite, oBrushBlack, oPen, oFont

   IF !lGUI
      RETURN Nil
   ENDIF
   IF Empty( xKoef )
      xKoef := hb_gtinfo( HB_GTI_SCREENWIDTH ) / MaxCol()
      yKoef := hb_gtinfo( HB_GTI_SCREENHEIGHT ) / MaxRow()
   ENDIF
   nw := Int( guiBoaSize * xKoef )

   IF Empty( oBrushWhite ) .OR. ( Empty( hDC ) .AND. nOp == OP_COLORS )
      IF !Empty( oBrushWhite )
         oBrushWhite:Release()
         oBrushBlack:Release()
         oPen:Release()
      ENDIF
      oBrushWhite := HBrush():Add( guiClrWCell )
      oBrushBlack := HBrush():Add( guiClrBCell )
      oPen := HPen():Add( , 2, guiClrSel )
      oFont := HFont():Add( guiFontName, 0, Int( nw*Iif(hb_Version(20),0.62,0.75) ) )
   ELSEIF Empty( hDC ) .AND. nOp == OP_SIZE
      oFont:Release()
      oFont := HFont():Add( guiFontName, 0, Int( nw*Iif(hb_Version(20),0.62,0.75) ) )
   ENDIF

   x1 := Int( x1t * xKoef )
   y1 := Int( (y1t-1) * yKoef )
   x2 := x1 + nw * 8
   y2 := y1 + nw * 8

   IF Empty( hDC )
      IF nOp == OP_SET
         gthwg_paint_SetCallback( "__PAINTBO_CHESS" )
      ELSEIF nOp == OP_UNSET
         gthwg_paint_SetCallback()
      ELSEIF nOp == OP_INVALIDATE
         hwg_Invalidaterect( hb_gtinfo(HB_GTI_WINHANDLE), 0 )
      ELSEIF nOp == OP_MDOWN
         arrm := hb_gtinfo( HB_GTI_MOUSEPOS_XY )
         IF arrm[1] > x1 .AND. arrm[1] < x2 .AND. arrm[2] > y1 .AND. arrm[2] < y2
            arrm[1] := Int( (arrm[1] - x1) / nw ) + 1
            arrm[2] := Int( (arrm[2] - y1) / nw ) + 1
         ELSE
            arrm[1] := arrm[2] := -1
         ENDIF
         RETURN arrm
      ENDIF
      RETURN Nil
   ENDIF

   cBoard := aCurrPos[POS_BOARD]
   nTopMargin := Int( ( nw - oFont:height ) / 2 )
   hwg_SelectObject( hDC, oFont:handle )
   hwg_Settransparentmode( hDC, .T. )
   lWhiteCell := .T.
   FOR i := 0 TO 7
      FOR j := 0 TO 7
         hwg_SelectObject( hDC, Iif( lWhiteCell, oBrushWhite:handle, oBrushBlack:handle ) )
         hwg_Rectangle_Filled( hDC, x1+(j*nw), y1+(i*nw), x1+((j+1)*nw), y1+((i+1)*nw), .F. )
         nMove := i*8 + j + 1
         IF aMoveState[2] == nMove .OR. ;
            ( aMoveState[2] == 0 .AND. lShowPos .AND. aMoveState[3] == nMove )
            hwg_Rectangle( hDC, x1+(j*nw)+2, y1+(i*nw)+2, x1+((j+1)*nw)-2, y1+((i+1)*nw)-2, oPen:handle )
         ENDIF
         c := Substr( cBoard, nMove, 1 )
         IF c > 'A'
            IF c > 'a'
               hwg_Settextcolor( hDC, guiClrBText )
            ELSEIF c > 'A'
               hwg_Settextcolor( hDC, guiClrWText )
            ENDIF
            i1 := Ascan( aFigs,c )
            hwg_Drawtext( hDC, Iif( lDrawUtf8, aFigs3[i1], Iif( lRussian,aFigs1[i1],aFigs2[i1] ) ), ;
               x1+j*nw+2, y1+i*nw+nTopMargin, x1+(j+1)*nw-2, y1+(i+1)*nw-2, DT_CENTER )
         ENDIF
         lWhiteCell := !lWhiteCell
      NEXT
      lWhiteCell := !lWhiteCell
   NEXT

   hwg_Settransparentmode( hDC, .F. )

   RETURN Nil