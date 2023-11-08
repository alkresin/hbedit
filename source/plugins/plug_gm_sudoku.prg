/*
 * Sudoku game
 * HbEdit plugin
 *
 * Copyright 2022 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4
#define K_BS          8
#define K_F9         -8
#define K_F10        -9
#define K_LBUTTONDOWN 1002

#define OP_SET                  1
#define OP_UNSET                2
#define OP_INVALIDATE           3
#define OP_MDOWN                4

#define HB_GTI_FONTNAME         24

STATIC cIniPath
STATIC oGame, lRu := .T., lGUI
STATIC x1t, y1t, x2t, nyPos := 1, nxPos := 1, lPaneOn := .F.
STATIC nLevel, nGameState
STATIC cScreenBuff
STATIC aBoardEasyTempl := { "164893725729156834835247196243718659917465382658932417391624578576389241482571963", ;
   "725491683843267951169385247631974825284653719597128436372816594916542378458739162", ;
   "142893675763425189895617324217964853934581267586372941451236798328759416679148532", ;
   "132547698946281357578936214864352179395718462721694583657823941219475836483169725" }
STATIC aBoardMediumTempl := { "xxx4xxxxxxx125x7xxx49xxx38xxxxx6xx27x2x3x4x9x56xx2xxxxx78xxx13xxx2x916xxxxxxx2xxx", ;
   "x6x9xx81x2xx1xx5xx8xxxx67xx7xxxx3xxxx1xxxxx3xxxx6xxxx1xx63xxxx2xx7xx5xx8x53xx2x9x", ;
   "xxxx73x1x1xxxxx6x4xx8xx92x78xxxx17xxx3xxxxx8xxx63xxxx19x35xx4xx7x5xxxxx2x4x18xxxx" }
STATIC aBoardHardTempl := { "xxxxx1xx39xxxxx46xx46x5xxxxx6x3xxx5x8xx1xx2x7x9x2xxx4xx28x9xxxx6xxxxx87xxxxxx3xx9", ;
   "6xxxxxxx5xx95x74xxxx8xxx3xx91xxxxx32xx7x1x8xx3x6xxx1x7xxxxxxxxxx9xx7xx1xx7x352x4x", ;
   "8xxxx2xxx7x2xx35xxxxxx5x2x8x7x4xxxx6xxxxx83xxx9x6xxxx2xxxx1x7x46x7xx41xx3xxxx6xxx", ;
   "xxxCxxxExBxADGECIxxxxxxFxxDxxCHIxxxxFIxxxxxxxxxxxFxxDIxExxDxxFxxBxxxxxxECxGxxxIxH" }
STATIC aBoardInit, aBoard, aHis, nHis
STATIC clrText := "+GR/N", clrBoard := "GR+/N", clrFix := "W/N", clrBorder := "GR+/B", clrCur := "N/RB"
STATIC cFileSave := "sudoku.saved"

STATIC guiBoaSize := 3, guiClrBoard := 0xffffff, guiClrRow := 0xcccccc, guiClrSel := 0xaaaaaa
STATIC guiClrText := 0xff0000, guiClrFix := 0, guiClrSep := 0xff0000, guiClrSep2 := 0x222222
STATIC guiFontName

FUNCTION plug_gm_Sudoku( oEdit, cPath )

   LOCAL i, cName := "$Sudoku"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      Scroll( y, o:x1, y, o:x2 )
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Sudoku" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }

   IF Empty( cIniPath )
      cIniPath := cPath
   ENDIF
   lGUI := ( hb_gtVersion() == "HWGUI" )

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   oGame := mnu_NewBuf( oEdit )
   IF !hb_Version(20) .OR. lGUI // 20 - HB_VERSION_UNIX_COMPAT
      edi_SetPalette( oGame, "solarized dark" )
   ELSEIF hb_Version(20)
      edi_SetPalette( oGame, "default" )
   ENDIF
   oGame:cFileName := cName
   oGame:bWriteTopPane := bWPane
   oGame:bOnKey := {|o,n| _gm_Sudoku_OnKey(o,n) }
   oGame:bStartEdit := {|| _gm_Sudoku_Start() }
   oGame:cp := "RU866"
   oGame:lIns := Nil
   nLevel := 1
   nGameState := 0
   aHis := Array( 100 )
   aBoardInit := Array( 9,9 )
   aBoard := Array( 9,9 )

   RETURN Nil

STATIC FUNCTION _gm_Sudoku_Start()

   LOCAL cSaved, i, j

   IF Empty( cScreenBuff )

      cSaved := Read_Game_Ini( cIniPath + "sudoku.ini" )

      y1t := oGame:y1 + 3
      x1t := oGame:x1 + 2
      x2t := x1t + 30

      __PaintBoa( , OP_SET )

      DispBegin()
      SetColor( clrText )
      Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )

      @ y1t, x1t+2 SAY "F9 - " + Iif( lRu, "меню", "menu" )
      @ y1t+1, x1t+2 SAY "F10, ESC - " + Iif( lRu, "Выход", "Exit" )

      @ y1t+3, x1t+2 SAY "Space - " + Iif( lRu, "Очистить", "Clean cell" )
      @ y1t+4, x1t+2 SAY "1...9 - "  + Iif( lRu, "Вставить цифру", "Put number" )
      IF lRu
         @ y1t+5, x1t+2 SAY "h,Влево/l,Вправо /"
         @ y1t+6, x1t+2 SAY "k,Вверх/j,Вниз - Перейти"
      ELSE
         @ y1t+5, x1t+2 SAY "h,Left / l,Right /"
         @ y1t+6, x1t+2 SAY "k,Up / j,Down - Movement"
      ENDIF
      @ y1t+7, x1t+2 SAY "Backspace - " + Iif( lRu, "Вернуть ход", "Turn back" )

      SetColor( clrBorder )
      @ y1t, x1t+26 TO y1t+11, x1t+26
      IF lGUI
         @ y1t+3, x2t TO y1t+3, x2t+18
         @ y1t+7, x2t TO y1t+7, x2t+18
         @ y1t, x2t+6 TO y1t+10, x2t+6
         @ y1t, x2t+13 TO y1t+10, x2t+13
      ENDIF
      DispEnd()

      IF !Empty( cSaved ) .AND. text2Boa( cSaved )
         nGameState := 1
         nHis := 0
         nyPos := nxPos := 1
         IF Look4Empty( .F., @i, @j )
            nyPos := i
            nxPos := j
         ENDIF
         DrawBoard()
      ELSE
         DO WHILE !_Game_Menu( oGame )
         ENDDO
      ENDIF
   ELSE
      __PaintBoa( , OP_SET )
      RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cScreenBuff )
   ENDIF

   RETURN Nil

STATIC FUNCTION _gm_Sudoku_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), i, j

   IF nGameState == 1 .OR. nGameState == 2
      IF nKey == K_BS
         IF nHis > 0
            nyPos := aHis[nHis,1]
            nxPos := aHis[nHis,2]
            aBoard[nyPos,nxPos] := aHis[nHis,3]
            nHis --
            DrawBoard()
         ENDIF
         RETURN -1
      ENDIF
      IF nKey == K_LEFT .OR. nKey == 104       // h
         IF nxPos > 1
            SetCurrentPos( .F. )
            nxPos --
            SetCurrentPos( .T. )
         ENDIF
      ELSEIF nKey == K_RIGHT .OR. nKey == 108  // l
         IF nxPos < 9
            SetCurrentPos( .F. )
            nxPos ++
            SetCurrentPos( .T. )
         ENDIF
      ELSEIF nKey == K_UP .OR. nKey == 107     // k
         IF nyPos > 1
            SetCurrentPos( .F. )
            nyPos --
            SetCurrentPos( .T. )
         ENDIF
      ELSEIF nKey == K_DOWN .OR. nKey == 106   // j
         IF nyPos < 9
            SetCurrentPos( .F. )
            nyPos ++
            SetCurrentPos( .T. )
         ENDIF
      ELSEIF nKey == 32 .OR. ( nKey >= 49 .AND. nKey <= 57 )
         SetCellValue( nKey )
         RETURN -1
      ELSEIF nKey == 115 .OR. nKey == 83  // s,S
         IF ( i := Solver( aBoard, nKey == 83 ) ) == 1
            _Alert( "Solved" )
         ELSEIF i > 1
            _Alert( "Too many solutions" )
         ELSE
            _Alert( "Error" )
         ENDIF
      ENDIF
   ENDIF

   IF nKey == K_LBUTTONDOWN
      j := MCol()
      i := MRow()
      IF i == oEdit:y1-1 .AND. j < 8
         _Game_Menu( oEdit )
      ELSE
         IF nGameState == 1 .OR. nGameState == 2
            IF lGUI
               __PaintBoa( , OP_MDOWN )
            ELSEIF i >= y1t .AND. i <= y1t + 10 .AND. j >= x2t .AND. j <= x2t + 20
               coors2Index( i, j, @i, @j )
               IF i > 0 .AND. i < 10 .AND. j > 0 .AND. j < 10
                  SetCurrentPos( .F. )
                  nyPos := i
                  nxPos := j
                  SetCurrentPos( .T. )
               ENDIF
            ENDIF
            IF lPaneOn .AND. ( i >= y1t+9 .AND. i <= y1t + 11 .AND. j >= x1t+16 .AND. j <= x1t + 21 )
               j := Int( (j-x1t-16)/2 )
               j += Iif( i==y1t+10, 3, Iif(i==y1t+11, 6, 0 ) )
               SetCellValue( j+49 )
            ENDIF
         ENDIF
      ENDIF

   ELSEIF nKey == 122  // z
      IF nGameState == 2
         nGameState := 1
         FOR i := 1 TO 9
            ACopy( aBoard[i], aBoardInit[i] )
         NEXT
         @ y1t+12, x2t SAY Space(40)
         DrawBoard()
      ENDIF

   ELSEIF nKey == K_F9

      _Game_Menu( oEdit )

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
      IF lGUI
         __PaintBoa( , OP_UNSET )
      ENDIF
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_ESC .OR. nKey == K_F10
      cScreenBuff := Nil
      Write_Game_Ini()
      IF lGUI
         __PaintBoa( , OP_UNSET )
      ENDIF
      mnu_Exit( oEdit )

   ENDIF

   RETURN -1

STATIC FUNCTION _Game_Menu( oEdit )

   LOCAL aMenu := { Iif( lRu, "Новая игра", "New Game" ) }
   LOCAL aMenu2 := Iif( lRu, { "Уровень 1", "Уровень 2", "Уровень 3" }, { "Level 1", "Level 2", "Level 3" } )
   LOCAL iChoic, i, j

   IF nGameState == 1
      Aadd( aMenu, Iif( lRu, "Очистить доску", "Clean board" ) )
   ENDIF
   Aadd( aMenu, Iif( lRu, "Сохранить", "Save game" ) )
   Aadd( aMenu, Iif( lRu, "Загрузить", "Load game" ) )
   Aadd( aMenu, Iif( lRu, "Создать", "Create" ) )
   Aadd( aMenu, Iif( lRu, "English", "Русский" ) )
   Aadd( aMenu, Iif( lRu, "Выход", "Exit" ) )

   iChoic := FMenu( oGame, aMenu, y1t+2, 2, y1t+10, 26 )

   IF iChoic == 1
      IF ( iChoic := FMenu( oGame, aMenu2, y1t+2, 4, y1t+6, 20 ) ) > 0
         nLevel := iChoic
         nGameState := 1
         nHis := 0
         CreateBoard()
         nyPos := nxPos := 1
         IF Look4Empty( .F., @i, @j )
            nyPos := i
            nxPos := j
         ENDIF
         DrawBoard()
      ELSE
         RETURN .F.
      ENDIF

   ELSEIF iChoic == Len( aMenu )
      cScreenBuff := Nil
      Write_Game_Ini()
      IF lGUI
         __PaintBoa( , OP_UNSET )
      ENDIF
      mnu_Exit( oEdit )

   ELSEIF iChoic == Len( aMenu ) - 1
      lRu := !lRu

   ELSEIF iChoic == Len( aMenu ) - 2
      nGameState := 2
      FOR i := 1 TO 9
         FOR j := 1 TO 9
            aBoardInit[i,j] := aBoard[i,j] := ''
         NEXT
      NEXT
      nHis := 0
      nyPos := nxPos := 1
      DrawBoard()
      @ y1t+12, x2t SAY Iif( lRu, "Жмите 'z', чтобы начать решать задачу", "Press 'z' to start solve a problem" )

   ELSEIF iChoic == Len( aMenu ) - 3
      LoadGame()

   ELSEIF iChoic == Len( aMenu ) - 4
      SaveGame()

   ELSEIF iChoic == 2 .AND. nGameState == 1
      FOR i := 1 TO 9
         ACopy( aBoardInit[i], aBoard[i] )
      NEXT
      nHis := 0
      DrawBoard()

   ELSE
      RETURN .F.
   ENDIF

   RETURN .T.

STATIC FUNCTION CreateBoard()

   LOCAL i, j, i1, n1, n2, xTmp, n2Del := 0
   LOCAL a1 := { '1','2','3','4','5','6','7','8','9' }, a2[9]
   LOCAL a3 := { {1,1}, {1,4}, {1,7}, {4,1}, {4,4}, {4,7}, {7,1}, {7,4}, {7,7} }
   LOCAL aSectors[9], s

   IF nLevel == 1
      n1 := hb_randomInt( 1, Len(aBoardEasyTempl) )
      templ2Boa( aBoardEasyTempl[n1] )
   ELSEIF nLevel == 2
      n1 := hb_randomInt( 1, Len(aBoardMediumTempl) )
      templ2Boa( aBoardMediumTempl[n1] )
   ELSEIF nLevel == 3
      n1 := hb_randomInt( 1, Len(aBoardHardTempl) )
      templ2Boa( aBoardHardTempl[n1] )
   ENDIF

   // Mix
   FOR i := 1 TO 12
      n1 := hb_randomInt( 1, 9 )
      n2 := Iif( n1%3 == 0, n1-2, n1+1 )
      IF hb_randomInt() == 0
         // Row exchange
         xTmp := aBoardInit[n1]
         aBoardInit[n1] := aBoardInit[n2]
         aBoardInit[n2] := xTmp
      ELSE
         FOR j := 1 TO 9
            // Column exchange
            xTmp := aBoardInit[j,n1]
            aBoardInit[j,n1] := aBoardInit[j,n2]
            aBoardInit[j,n2] := xTmp
         NEXT
      ENDIF
      IF i == 6
         IF hb_randomInt( 1,3 ) < 3
            // Transponir
            FOR n1 := 1 TO 9
               FOR j := 1 TO 9
                  aBoard[j,n1] := aBoardInit[n1,j]
               NEXT
            NEXT
            FOR n1 := 1 TO 9
               FOR j := 1 TO 9
                  aBoardInit[n1,j] := aBoard[n1,j]
               NEXT
            NEXT
         ENDIF
         IF ( n1 := hb_randomInt( 0,3 ) ) > 0
            // Section exchange
            n1 := Iif( n1 == 1, n1, Iif( n1 == 2, 4, 7 ) )
            n2 := Iif( n1 == 1, 4, Iif( n1 == 4, 7, 1 ) )
            IF hb_randomInt() == 0
               FOR i1 := 0 TO 2
                  xTmp := aBoardInit[n1+i1]
                  aBoardInit[n1+i1] := aBoardInit[n2+i1]
                  aBoardInit[n2+i1] := xTmp
               NEXT
            ELSE
               FOR i1 := 0 TO 2
                  FOR j := 1 TO 9
                     xTmp := aBoardInit[j,n1+i1]
                     aBoardInit[j,n1+i1] := aBoardInit[j,n2+i1]
                     aBoardInit[j,n2+i1] := xTmp
                  NEXT
               NEXT
            ENDIF
         ENDIF
      ENDIF
   NEXT

   // Replace 'a'..'i' by '1'..'9'
   FOR i := 1 TO 9
      n1 := Iif( i == 9, 1, hb_randomInt( 1, 10-i ) )
      a2[i] := a1[n1]
      ADel( a1, n1 )
   NEXT
   FOR i := 1 TO 9
      FOR j := 1 TO 9
         IF !Empty( aBoardInit[i,j] )
            n1 := Asc( aBoardInit[i,j] ) - 64
            //edi_writelog( aBoardInit[i,j] + "/" + str(n1) )
            aBoardInit[i,j] := a2[n1]
         ENDIF
      NEXT
   NEXT

   IF nLevel == 1
      // Hide cells
      FOR j := 1 TO 3
         IF j == 3
           n2 := hb_randomInt()
         ENDIF
         IF j < 3 .OR. n2 == 0
            FOR i := 1 TO 9
               i1 := 0
               DO WHILE .T.
                  n1 := hb_randomInt( 1, 9 )
                  IF !Empty( aBoardInit[i,n1] )
                     aBoardInit[i,n1] := ''
                     EXIT
                  ENDIF
                  IF ++i1 > 15
                     n2Del ++
                     EXIT
                  ENDIF
               ENDDO
            NEXT
         ENDIF
         IF j < 3 .OR. n2 > 0
            FOR i := 1 TO 9
               i1 := 0
               DO WHILE .T.
                  n1 := hb_randomInt( 1, 9 )
                  IF !Empty( aBoardInit[n1,i] )
                     aBoardInit[n1,i] := ''
                     EXIT
                  ENDIF
                  IF ++i1 > 15
                     n2Del ++
                     EXIT
                  ENDIF
               ENDDO
            NEXT
         ENDIF
      NEXT

      //
      FOR i1 := 1 TO 9
         i := a3[i1,1] - 1
         j := a3[i1,2] - 1
         n1 := 0
         DO WHILE ++i <= a3[i1,1]+2
            DO WHILE ++j <= a3[i1,2]+2
               //edi_writelog( "."+ltrim(str(i))+"/"+ltrim(str(j))+" "+aBoardInit[i,j] )
               IF !Empty( aBoardInit[i,j] ); n1 ++; ENDIF
            ENDDO
            j := a3[i1,2] - 1
         ENDDO
         aSectors[i1] := n1
      NEXT

      n2Del += Iif( nLevel==1, 5, Iif( nLevel==2, 10, 16 ) )
      n1 := 0
      DO WHILE .T.
         xTmp := 0
         //s := ""
         FOR i1 := 1 TO 9
            IF aSectors[i1] > xTmp
               n2 := i1
               xTmp := aSectors[i1]
            ENDIF
            //s += ltrim(str(aSectors[i1]))+" "
         NEXT
         //edi_writelog( s )
         i := hb_randomInt( 0,8 )
         j := Int(i%3) + a3[n2,2]
         i := Int(i/3) + a3[n2,1]

         //i := hb_randomInt( 1, 9 )
         //j := hb_randomInt( 1, 9 )
         //edi_writelog( ltrim(str(n2))+": "+ltrim(str(i))+"/"+ltrim(str(j))+" "+aBoardInit[i,j] )
         IF !Empty( aBoardInit[i,j] )
            aBoardInit[i,j] := ''
            aSectors[n2] --
            IF ++n1 > n2Del
               EXIT
            ENDIF
         ENDIF
      ENDDO

      //boa2File( aBoardInit, "a1.log" )
      DO WHILE Valtype( xTmp := Solver( aBoardInit, .F., .T. ) ) == "A"
         aBoardInit[xTmp[1],xTmp[2]] := xTmp[3]
      ENDDO
      //boa2File( aBoardInit, "a1.log" )
   ENDIF

   FOR i := 1 TO 9
      ACopy( aBoardInit[i], aBoard[i] )
   NEXT

   RETURN Nil

STATIC FUNCTION DrawBoard()

   LOCAL i, j, x1, y1

   IF lGUI
      SetCurrentPos( .T. )
   ELSE
      DispBegin()
      SetColor( clrBoard )
      FOR i := 1 TO 9
         FOR j := 1 TO 9
            index2Coors( i, j, @y1, @x1 )
            SetColor( Iif( Empty(aBoardInit[i,j]), clrBoard, clrFix ) )
            @ y1, x1 SAY Iif( Empty( aBoard[i,j] ), '.', aBoard[ i,j ] ) + ' '
         NEXT
      NEXT
      SetCurrentPos( .T. )
      DispEnd()
   ENDIF

   RETURN Nil

STATIC FUNCTION SetCurrentPos( lSet )

   LOCAL clr := SetColor( Iif( lSet, clrCur,Iif( Empty(aBoardInit[nyPos,nxPos]), clrBoard, clrFix ) ) ), y1, x1

   IF lGUI
      IF lSet
         __PaintBoa( , OP_INVALIDATE )
      ENDIF
   ELSE
      index2Coors( nyPos, nxPos, @y1, @x1 )
      @ y1, x1 SAY Iif( Empty( aBoard[nyPos,nxPos] ), '.', aBoard[nyPos,nxPos] ) + ' '
   ENDIF
   IF lSet
      DrawPane( Empty( aBoardInit[nyPos,nxPos] ) )
   ENDIF
   SetColor( clr )

   RETURN Nil

STATIC FUNCTION SetCellValue( nKey )

   LOCAL c := aBoard[nyPos,nxPos], aErr

   IF !Empty( aBoardInit[nyPos,nxPos] )
      _Alert( Iif( lRu, "Нельзя менять начальные значения!", "Can't change initial value!" ) )
      RETURN .F.
   ENDIF
   IF nKey == 32
      aBoard[nyPos,nxPos] := ''
      SetCurrentPos( .T. )
   ELSEIF CheckValue( aBoard, nyPos, nxPos, Chr(nKey) )
      aBoard[nyPos,nxPos] := Chr(nKey)
      SetCurrentPos( .T. )
      Look4Empty( .T. )
   ELSE
      _Alert( Iif( lRu, "Ошибка!", "Illegal value!" ) )
      RETURN .F.
   ENDIF
   AddHis( c, aBoard[nyPos,nxPos] )
   IF nKey != 32 .AND. !Empty( aErr := Check2( nyPos,nxPos ) )
      _Alert( Iif( lRu, "Проблема в ", "Problem at " ) + Ltrim(Str(aErr[1])) + "/" + Ltrim(Str(aErr[2])) )
   ENDIF

   RETURN .T.

STATIC FUNCTION DrawPane( lDraw )

   lPaneOn := lDraw
   IF lDraw
      SetColor( clrCur )
      @ y1t+9, x1t+16 SAY "1 2 3 "
      @ y1t+10, x1t+16 SAY "4 5 6 "
      @ y1t+11, x1t+16 SAY "7 8 9 "
   ELSE
      SetColor( clrBoard )
      @ y1t+9, x1t+16, y1t+11, x1t+24 BOX Space(9)
   ENDIF
   RETURN Nil

STATIC FUNCTION index2Coors( i, j, y, x )

   y := y1t + i - 1 + Iif( i > 6, 2, Iif( i > 3, 1, 0 ) )
   x := x2t + (j-1)*2 + Iif( j > 6, 2, Iif( j > 3, 1, 0 ) )

   RETURN Nil

STATIC FUNCTION coors2Index( y, x, i, j )

   i := y - y1t + 1
   i := Iif( i > 8, i-2, Iif( i > 4, i-1, i ) )
   j := x - x2t
   j := Int( Iif( j > 12, j-2, Iif( j > 6, j-1, j ) ) / 2 ) + 1

   RETURN Nil

// Returns .F., if there is the same value on a vert, horiz or section
STATIC FUNCTION CheckValue( aBoa, y, x, c )

   LOCAL i, j, y1, y2, x1, x2, lRes := .T.

   FOR i := 1 TO 9
      IF y != i
         IF aBoa[i,x] == c
            lRes := .F.
            EXIT
         ENDIF
      ENDIF
      IF x != i
         IF aBoa[y,i] == c
            lRes := .F.
            EXIT
         ENDIF
      ENDIF
   NEXT
   IF lRes
      y1 := Int( (y-1)/3 ) * 3 + 1
      y2 := y1 + 2
      x1 := Int( (x-1)/3 ) * 3 + 1
      x2 := x1 + 2
      FOR i := y1 TO y2
         FOR j := x1 TO x2
            IF y != i .AND. x != j
               IF aBoa[i,j] == c
                  lRes := .F.
                  EXIT
               ENDIF
            ENDIF
         NEXT
         IF !lRes
            EXIT
         ENDIF
      NEXT
   ENDIF

   RETURN lRes

STATIC FUNCTION Check2( y, x )

   LOCAL i, j, k, y1, y2, x1, x2, lRes

   FOR i := 1 TO 9
      IF Empty( aBoard[i,x] )
         lRes := .F.
         FOR k := 49 TO 57
            IF ( lRes := CheckValue( aBoard, i, x, Chr(k) ) )
               EXIT
            ENDIF
         NEXT
         IF !lRes
            RETURN { i,x }
         ENDIF
      ENDIF
      IF Empty( aBoard[y,i] )
         lRes := .F.
         FOR k := 49 TO 57
            IF ( lRes := CheckValue( aBoard, y, i, Chr(k) ) )
               EXIT
            ENDIF
         NEXT
         IF !lRes
            RETURN { y,i }
         ENDIF
      ENDIF
   NEXT
   y1 := Int( (y-1)/3 ) * 3 + 1
   y2 := y1 + 2
   x1 := Int( (x-1)/3 ) * 3 + 1
   x2 := x1 + 2
   FOR i := y1 TO y2
      FOR j := x1 TO x2
         IF Empty( aBoard[i,j] )
            lRes := .F.
            FOR k := 49 TO 57
               IF ( lRes := CheckValue( aBoard, i, j, Chr(k) ) )
                  EXIT
               ENDIF
            NEXT
            IF !lRes
               RETURN { i,j }
            ENDIF
         ENDIF
      NEXT
   NEXT

   RETURN Nil

STATIC FUNCTION Solver( aBoa, lOut, lCompare )

   LOCAL i1, i, j, k, s, aBack[9,9], nSolutions := 0
   LOCAL nMin, sMin, aCoor[2], aSolver := Array( 81 ), nSolver := 0, aCompare

   IF Empty( lCompare ); lCompare := .F.; ENDIF
   FOR i := 1 TO 9
      ACopy( aBoa[i], aBack[i] )
   NEXT

   DO WHILE .T.
      i1 := 1
      //edi_writelog( "1)", "_solver.log" )
      //boa2File( aBoa, "_solver.log" )
      DO WHILE i1 > 0
         i1 := 0
         nMin := 10
         FOR i := 1 TO 9
            FOR j := 1 TO 9
               IF Empty( aBoa[i,j] )
                  i1 ++
                  s := ""
                  FOR k := 49 TO 57
                     IF CheckValue( aBoa, i, j, Chr(k) )
                        s += Chr(k)
                        IF Len( s ) >= nMin
                           EXIT
                        ENDIF
                     ENDIF
                  NEXT
                  IF Len( s ) < nMin
                     sMin := s
                     nMin := Len( s )
                     aCoor[1] := i
                     aCoor[2] := j
                     IF nMin <= 1
                        EXIT
                     ENDIF
                  ENDIF
               ENDIF
            NEXT
            IF nMin <= 1
               EXIT
            ENDIF
         NEXT
         IF nMin == 0
            DO WHILE nSolver > 0
               //aBoa[aSolver[nSolver,1],aSolver[nSolver,2]] := Left( aSolver[nSolver,3],1 )
               IF Len(aSolver[nSolver,3]) == 1
                  aBoa[aSolver[nSolver,1],aSolver[nSolver,2]] := ''
                  nSolver --
                  IF nSolver == 0
                     FOR i := 1 TO 9
                        ACopy( aBack[i], aBoa[i] )
                     NEXT
                     RETURN nSolutions
                  ENDIF
               ELSE
                  aSolver[nSolver,3] := Substr( aSolver[nSolver,3],2 )
                  aBoa[aSolver[nSolver,1],aSolver[nSolver,2]] := Left( aSolver[nSolver,3],1 )
                  EXIT
               ENDIF
            ENDDO
         ELSE
            aBoa[aCoor[1],aCoor[2]] := Left( sMin, 1 )
            aSolver[++nSolver] := { aCoor[1], aCoor[2], sMin }
         ENDIF
      ENDDO
      //edi_writelog( "2)", "_solver.log" )
      //boa2File( aBoa, "_solver.log" )

      IF lOut
         boa2File( aBoa, "solver.log" )
      ENDIF
      IF lCompare
         IF nSolutions == 0
            aCompare := Array( 9,9 )
            FOR i := 1 TO 9
               ACopy( aBoa[i], aCompare[i] )
            NEXT
         ENDIF
      ENDIF

      IF ++nSolutions > 1
         EXIT
      ELSE
         DO WHILE nSolver > 0
            //aBoa[aSolver[nSolver,1],aSolver[nSolver,2]] := Left( aSolver[nSolver,3],1 )
            IF Len(aSolver[nSolver,3]) == 1
               aBoa[aSolver[nSolver,1],aSolver[nSolver,2]] := ''
               nSolver --
               IF nSolver == 0
                  FOR i := 1 TO 9
                     ACopy( aBack[i], aBoa[i] )
                  NEXT
                  RETURN nSolutions
               ENDIF
            ELSE
               aSolver[nSolver,3] := Substr( aSolver[nSolver,3],2 )
               aBoa[aSolver[nSolver,1],aSolver[nSolver,2]] := Left( aSolver[nSolver,3],1 )
               EXIT
            ENDIF
         ENDDO
      ENDIF

   ENDDO

   IF lCompare .AND. nSolutions == 2
      FOR i := 1 TO 9
         FOR j := 1 TO 9
            IF aBoa[i,j] != aCompare[i,j]
               nSolutions := { i, j, aCompare[i,j] }
               EXIT
            ENDIF
         NEXT
      NEXT
   ENDIF

   FOR i := 1 TO 9
      ACopy( aBack[i], aBoa[i] )
   NEXT

   RETURN nSolutions

STATIC FUNCTION Look4Empty( l4End, i, j )

   FOR i := 1 TO 9
      FOR j := 1 TO 9
         IF Empty( aBoard[i,j] )
            RETURN .T.
         ENDIF
      NEXT
   NEXT
   IF l4End
      _Alert( Iif( lRu, "Задача решена!", "The task solved!" ) )
      nGameState := 0
   ENDIF

   RETURN .F.

STATIC FUNCTION AddHis( cValPrev, cValNew )

   nHis ++
   IF nHis > Len( aHis )
      aHis := ASize( aHis, Len(aHis)+50 )
   ENDIF
   aHis[nHis] := {nyPos, nxPos, cValPrev, cValNew }

   RETURN Nil

STATIC FUNCTION boa2Text()

   LOCAL cSaved := "", i, j, c

   FOR i := 1 TO 9
      FOR j := 1 TO 9
         c := aBoard[i,j]
         c := Iif( Empty(c), 'x', Chr( Asc(c)+(97-49) ) )
         cSaved += Iif( Empty(aBoardInit[i,j]), c, Upper(c) )
      NEXT
   NEXT
   RETURN cSaved

STATIC FUNCTION templ2Boa( s )

   LOCAL i1 := 0, i, j, n

   IF Len( s ) != 81
      RETURN .F.
   ENDIF
   FOR i := 1 TO 9
      FOR j := 1 TO 9
         n := hb_bpeek( s, ++i1 )
         IF n >= 65 .AND. n <= 73   // 'A'..'I'
            aBoardInit[i,j] := Chr( n )
         ELSEIF n >= 49 .AND. n <= 57 // '1'..'9'
            aBoardInit[i,j] := Chr( n + (65-49) )
         ELSEIF n == 120  // 'x'
            aBoardInit[i,j] := ' '
         ELSE
            RETURN .F.
         ENDIF
      NEXT
   NEXT

   RETURN .T.

STATIC FUNCTION text2Boa( s )

   LOCAL i1 := 0, i, j, n

   IF Len( s ) != 81
      RETURN .F.
   ENDIF
   FOR i := 1 TO 9
      FOR j := 1 TO 9
         n := hb_bpeek( s, ++i1 )
         IF n == 120     // 'x'
            aBoardInit[i,j] := aBoard[i,j] := ''
         ELSEIF n >= 65 .AND. n <= 73   // 'A'..'I'
            aBoardInit[i,j] := aBoard[i,j] := Chr( n - (65-49) )
         ELSEIF n >= 97 .AND. n <= 105  // 'a'..'i'
            aBoardInit[i,j] := ''
            aBoard[i,j] := Chr( n  - (97-49) )
         ELSE
            RETURN .F.
         ENDIF
      NEXT
   NEXT

   RETURN .T.

STATIC FUNCTION boa2File( aBoa, cFile )

   LOCAL s := "", i, j

   FOR i := 1 TO 9
      FOR j := 1 TO 9
         s += Iif( Empty(aBoa[i,j]), ' ', aBoa[i,j] )
      NEXT
      s += hb_Eol()
   NEXT
   edi_writelog( s, cFile )

   RETURN Nil

STATIC FUNCTION SaveGame()

   LOCAL arr, cName, n, nLen, i, s := ""

   arr := Iif( File( cIniPath + cFileSave ), hb_aTokens( MemoRead( cIniPath + cFileSave ), Chr(10) ), {} )
   cName := __GetString( oGame, Iif( lRu,"Имя задачи","Name of the task" ) )
   IF ( nLen := Len( cName ) ) > 0
      IF ( n := Ascan( arr, {|s|Left(s,nLen)==cName} ) ) > 0
         IF _Alert( Iif( lRu,"Такое имя существует. Перезаписать?","This name exists already. Overwrite?" ), Iif( lRu,"Да","Yes" ), Iif( lRu,"Нет","No" ) ) != 1
            RETURN Nil
         ENDIF
      ENDIF
      FOR i := 1 TO Len( arr )
         IF i != n
            s += arr[i] + Chr(10)
         ENDIF
      NEXT
      s += cName + '=' + boa2Text()
      hb_MemoWrit( cIniPath + cFileSave, s )
   ENDIF

   RETURN Nil

STATIC FUNCTION LoadGame()

   LOCAL arr, aMenu, i, j, nPos, cSaved

   IF Empty( arr := Iif( File( cIniPath + cFileSave ), hb_aTokens( MemoRead( cIniPath + cFileSave ), Chr(10) ), {} ) )
      _Alert( Iif( lRu, "Нет сохраненных задач", "No saved tasks" ) )
      RETURN Nil
   ENDIF

   aMenu := Array( Len(arr) )
   FOR i := 1 TO Len(arr)
      IF ( nPos := At( '=', arr[i] ) ) > 0
         aMenu[i] := Left( arr[i],nPos-1 )
      ELSE
         aMenu[i] := " "
      ENDIF
   NEXT
   i := FMenu( oGame, aMenu, y1t, x2t+2, y1t+10, x2t+22 )
   IF i > 0 .AND. !Empty( aMenu[i] )
      nPos := At( '=', arr[i] )
      cSaved := Substr( arr[i], nPos+1 )
      IF !Empty( cSaved ) .AND. text2Boa( cSaved )
         nGameState := 1
         nHis := 0
         nyPos := nxPos := 1
         IF Look4Empty( .F., @i, @j )
            nyPos := i
            nxPos := j
         ENDIF
         DrawBoard()
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION __GetString( oEdit, cTitle )

   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu )
   LOCAL aGets := { {11,27,0,"",26} }
   LOCAL nRes, cRes := ""
   LOCAL bufsc := Savescreen( 09, 25, 12, 55 )

   hb_cdpSelect( "RU866" )
   @ 09, 25, 12, 55 BOX "┌─┐│┘─└│ "
   hb_cdpSelect( oEdit:cp )

   @ 10,32 SAY cTitle
   SetColor( oEdit:cColorMenu )

   IF ( nRes := edi_READ( aGets ) ) > 0
       cRes := AllTrim( aGets[1,4] )
   ENDIF

   SetColor( oldc )
   Restscreen( 09, 25, 12, 55, bufsc )

   RETURN cRes

STATIC FUNCTION Read_Game_Ini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect, cSaved := Nil

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "GAME"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "russian" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lRu := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrtext" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrText := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrboard" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrBoard := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrfix" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrFix := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrborder" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrBorder := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrcur" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrCur := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "saved" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cSaved := cTemp
               ENDIF
            ENDIF
         ENDIF
         IF Upper(aIni[nSect]) == "GUI"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "size" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  guiBoaSize := Val( cTemp )
                  IF guiBoaSize < 2 .OR. guiBoaSize > 5
                     guiBoaSize := 3
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "clrboard" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiClrBoard := Val( cTemp )
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "clrtext" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiClrText := Val( cTemp )
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "clrfix" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiClrFix := Val( cTemp )
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "clrrow" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiClrRow := Val( cTemp )
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "clrsel" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiClrSel := Val( cTemp )
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "clrsep" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiClrSep := Val( cTemp )
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "clrsep2" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiClrSep2 := Val( cTemp )
                  ENDIF
                  IF hb_hHaskey( aSect, cTemp := "fontname" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                     guiFontName := cTemp
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   IF Empty( guiFontName )
      guiFontName := hb_gtinfo( HB_GTI_FONTNAME )
   ENDIF
   RETURN cSaved

STATIC FUNCTION Write_Game_Ini()

   LOCAL cr := Chr(13)+Chr(10)
   LOCAL s := "[GAME]" + cr

   s += "russian=" + Iif( lRu, "on","off" ) + cr
   s += "clrtext=" + clrText + cr
   s += "clrboard=" + clrBoard + cr
   s += "clrfix=" + clrFix + cr
   s += "clrborder=" + clrBorder + cr
   s += "clrcur=" + clrCur + cr
   IF nGameState == 1
      s += "saved=" + boa2Text() + cr
   ENDIF

   s += cr + "[GUI]" + cr
   s += "size=" + Ltrim(Str(guiBoaSize)) + cr
   s += "clrboard=" + Ltrim(Str(guiClrBoard)) + cr
   s += "clrtext=" + Ltrim(Str(guiClrText)) + cr
   s += "clrfix=" + Ltrim(Str(guiClrFix)) + cr
   s += "clrrow=" + Ltrim(Str(guiClrRow)) + cr
   s += "clrsel=" + Ltrim(Str(guiClrSel)) + cr
   s += "clrsep=" + Ltrim(Str(guiClrSep)) + cr
   s += "clrsep2=" + Ltrim(Str(guiClrSep2)) + cr
   s += "fontname=" + guiFontName + cr

   hb_MemoWrit( cIniPath + "sudoku.ini", s )

   RETURN Nil

#define HB_GTI_SCREENWIDTH      1
#define HB_GTI_SCREENHEIGHT     2
#define HB_GTI_WINHANDLE        69
#define HB_GTI_MOUSEPOS_XY      70

#define DT_CENTER               1

DYNAMIC GTHWG_PAINT_SETCALLBACK, HWG_INVALIDATERECT, HBRUSH, HPEN, HFONT, HWG_MSGINFO, HWG_MSGYESNO
DYNAMIC HWG_SELECTOBJECT, HWG_RECTANGLE_FILLED, HWG_DRAWLINE, HWG_DRAWTEXT
DYNAMIC HWG_SETTRANSPARENTMODE, HWG_SETTEXTCOLOR

FUNCTION __PaintBoa( hDC, nOp )

   LOCAL x1, y1, x2, y2, nw
   LOCAL i, j, arrm
   STATIC xKoef, yKoef
   STATIC oBrush, oBrushSel, oBrushRow, oPen, oPen2, oFont

   IF Empty( xKoef )
      xKoef := hb_gtinfo( HB_GTI_SCREENWIDTH ) / MaxCol()
   ENDIF
   nw := Int( guiBoaSize * xKoef )

   IF Empty( yKoef )
      yKoef := hb_gtinfo( HB_GTI_SCREENHEIGHT ) / MaxRow()
      oBrush := HBrush():Add( guiClrBoard )
      oBrushSel := HBrush():Add( guiClrSel )
      oBrushRow := HBrush():Add( guiClrRow )
      oPen := HPen():Add( , 1, guiClrSep )
      oPen2 := HPen():Add( , 3, guiClrSep2 )
      oFont := HFont():Add( guiFontName, 0, nw-(guiBoaSize+2)*2 )
   ENDIF

   x1 := Int( x2t * xKoef )
   y1 := Int( y1t * yKoef )
   x2 := x1 + nw * 9
   y2 := y1 + nw * 9

   IF Empty( hDC )
      IF nOp == OP_SET
         gthwg_paint_SetCallback( "__PAINTBOA" )
      ELSEIF nOp == OP_UNSET
         gthwg_paint_SetCallback()
      ELSEIF nOp == OP_INVALIDATE
         hwg_Invalidaterect( hb_gtinfo(HB_GTI_WINHANDLE), 0 )
      ELSEIF nOp == OP_MDOWN
         arrm := hb_gtinfo( HB_GTI_MOUSEPOS_XY )
         IF arrm[1] > x1 .AND. arrm[1] < x2 .AND. arrm[2] > y1 .AND. arrm[2] < y2
            arrm[1] := Int( (arrm[1] - x1) / nw ) + 1
            arrm[2] := Int( (arrm[2] - y1) / nw ) + 1
            IF nyPos != arrm[2] .OR. nxPos != arrm[1]
               SetCurrentPos( .F. )
               nyPos := arrm[2]
               nxPos := arrm[1]
               SetCurrentPos( .T. )
            ENDIF
         ENDIF
      ENDIF
      RETURN Nil
   ENDIF

   hwg_SelectObject( hDC, oBrush:handle )
   hwg_Rectangle_Filled( hDC, x1, y1, x2, y2, .F. )
   hwg_Rectangle_Filled( hDC, x1+(nxPos-1)*nw, y1+(nyPos-1)*nw, x1+nxPos*nw, y1+nyPos*nw, .F., oBrushSel:handle )
   FOR i := 1 TO 9
      IF i != nyPos
         hwg_Rectangle_Filled( hDC, x1+(nxPos-1)*nw, y1+(i-1)*nw, x1+nxPos*nw, y1+i*nw, .F., oBrushRow:handle )
      ENDIF
      IF i != nxPos
         hwg_Rectangle_Filled( hDC, x1+(i-1)*nw, y1+(nyPos-1)*nw, x1+i*nw, y1+nyPos*nw, .F., oBrushRow:handle )
      ENDIF
   NEXT
   IF !Empty( aBoard[nyPos,nxPos] )
      FOR i := 1 TO 9
         FOR j := 1 TO 9
            IF aBoard[i,j] == aBoard[nyPos,nxPos] .AND. !( i == nyPos .AND. j == nxPos )
               hwg_Rectangle_Filled( hDC, x1+(j-1)*nw, y1+(i-1)*nw, x1+j*nw, y1+i*nw, .F., oBrushSel:handle )
            ENDIF
         NEXT
      NEXT
   ENDIF
   FOR i := 1 TO 8
      hwg_SelectObject( hDC, Iif( i%3==0, oPen2:handle, oPen:handle ) )
      hwg_DrawLine( hDC, x1, y1+(i*nw), x2, y1+(i*nw), .T. )
      hwg_DrawLine( hDC, x1+(i*nw), y1, x1+(i*nw), y2, .T. )
   NEXT
   hwg_SelectObject( hDC, oFont:handle )
   hwg_Settransparentmode( hDC, .T. )
   hwg_Settextcolor( hDC, guiClrText )
   FOR i := 1 TO 9
      FOR j := 1 TO 9
         IF !Empty( aBoard[i,j] )
            hwg_Settextcolor( hDC, Iif( Empty(aBoardInit[i,j]), guiClrText, guiClrFix ) )
            hwg_Drawtext( hDC, aBoard[i,j], x1+(j-1)*nw+2, y1+(i-1)*nw+4, ;
               x1+j*nw-2, y1+i*nw-4, DT_CENTER )
         ENDIF
      NEXT
   NEXT
   hwg_Settransparentmode( hDC, .F. )

   RETURN Nil

STATIC FUNCTION _Alert( cText, s1, s2 )

   IF lGUI
      IF s1 != Nil
         IF hwg_MsgYesNo( cText )
            RETURN 1
         ELSE
            RETURN 2
         ENDIF
      ELSE
         hwg_MsgInfo( cText )
      ENDIF
   ELSE
      RETURN edi_Alert( cText, s1, s2 )
   ENDIF

   RETURN Nil
