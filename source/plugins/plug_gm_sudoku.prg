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

STATIC cIniPath
STATIC oGame, lRu := .T.
STATIC x1t, y1t, x2t, nyPos, nxPos, lPaneOn := .F.
STATIC nLevel, nGameState
STATIC cScreenBuff
// "ABCDEFGHIDEFGHIABCGHIABCDEFBCDEFGHIAEFGHIABCDHIABCDEFGCDEFGHIABFGHIABCDEIABCDEFGH"
STATIC aBoardTempl := { "164893725729156834835247196243718659917465382658932417391624578576389241482571963", ;
   "725491683843267951169385247631974825284653719597128436372816594916542378458739162", ;
   "142893675763425189895617324217964853934581267586372941451236798328759416679148532", ;
   "132547698946281357578936214864352179395718462721694583657823941219475836483169725" }
STATIC aBoardInit, aBoard, aHis, nHis
STATIC clrText := "+GR/N", clrBoard := "GR+/N", clrFix := "W/N", clrBorder := "GR+/B", clrCur := "N/RB"

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

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   oGame := mnu_NewBuf( oEdit )
   IF !hb_Version(20) .OR. hb_gtVersion() == "HWGUI" // 20 - HB_VERSION_UNIX_COMPAT
      edi_SetPalette( oGame, "solarized dark" )
   ENDIF
   oGame:cFileName := cName
   oGame:bWriteTopPane := bWPane
   oGame:bOnKey := {|o,n| _Game_OnKey(o,n) }
   oGame:bStartEdit := {|| _Game_Start() }
   oGame:cp := "RU866"
   oGame:lIns := Nil
   nLevel := 1
   nGameState := 0
   aHis := Array( 100 )
   aBoardInit := Array( 9,9 )
   aBoard := Array( 9,9 )

   RETURN Nil

FUNCTION _Game_Start()

   LOCAL cSaved, i, j

   IF Empty( cScreenBuff )

      cSaved := Read_Game_Ini( cIniPath + "sudoku.ini" )

      y1t := oGame:y1 + 3
      x1t := oGame:x1 + 2
      x2t := x1t + 30

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

      @ y1t+3, x2t TO y1t+3, x2t+18
      @ y1t+7, x2t TO y1t+7, x2t+18
      @ y1t, x2t+6 TO y1t+10, x2t+6
      @ y1t, x2t+13 TO y1t+10, x2t+13

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
      RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cScreenBuff )
   ENDIF

   RETURN Nil

FUNCTION _Game_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), i, j

   IF nGameState == 1
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
      ELSEIF nKey == 115  // s
         IF Solver()
            edi_Alert( "Solved" )
         ELSE
            edi_Alert( "Error" )
         ENDIF
      ENDIF
   ENDIF

   IF nKey == K_LBUTTONDOWN
      j := MCol()
      i := MRow()
      IF i == oEdit:y1-1 .AND. j < 8
         _Game_Menu( oEdit )
      ELSE
         IF nGameState == 1
            IF i >= y1t .AND. i <= y1t + 10 .AND. j >= x2t .AND. j <= x2t + 20
               coors2Index( i, j, @i, @j )
               IF i > 0 .AND. i < 10 .AND. j > 0 .AND. j < 10
                  SetCurrentPos( .F. )
                  nyPos := i
                  nxPos := j
                  SetCurrentPos( .T. )
               ENDIF
               // edi_Alert( str(i)+" "+str(j) )
            ELSEIF lPaneOn .AND. ( i >= y1t+9 .AND. i <= y1t + 11 .AND. j >= x1t+16 .AND. j <= x1t + 21 )
               j := Int( (j-x1t-16)/2 )
               j += Iif( i==y1t+10, 3, Iif(i==y1t+11, 6, 0 ) )
               SetCellValue( j+49 )
            ENDIF
         ENDIF
      ENDIF
   ELSEIF nKey == K_F9

      _Game_Menu()

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_ESC .OR. nKey == K_F10
      cScreenBuff := Nil
      Write_Game_Ini()
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
   Aadd( aMenu, Iif( lRu, "English", "Russian" ) )
   Aadd( aMenu, Iif( lRu, "Выход", "Exit" ) )

   iChoic := FMenu( oGame, aMenu, y1t+2, x2t+2, y1t+7, x2t+20 )

   IF iChoic == 1
      IF ( iChoic := FMenu( oGame, aMenu2, y1t+2, x2t+2, y1t+6, x2t+18 ) ) > 0
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
      mnu_Exit( oEdit )

   ELSEIF iChoic == Len( aMenu ) - 1
      lRu := !lRu

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

   n1 := hb_randomInt( 1, Len(aBoardTempl) )
   templ2Boa( aBoardTempl[n1] )

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
   //edi_writelog( "cb-8" )

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
   //edi_writelog( "cb-8a" )
   n2Del += Iif( nLevel==1, 6, Iif( nLevel==2, 10, 14 ) )
   n1 := 0
   DO WHILE .T.
      i := hb_randomInt( 1, 9 )
      j := hb_randomInt( 1, 9 )
      IF !Empty( aBoardInit[i,j] )
         aBoardInit[i,j] := ''
         IF ++n1 > n2Del
            EXIT
         ENDIF
      ENDIF
   ENDDO

   //edi_writelog( "cb-9" )
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

   FOR i := 1 TO 9
      ACopy( aBoardInit[i], aBoard[i] )
   NEXT

   RETURN Nil

STATIC FUNCTION DrawBoard()

   LOCAL i, j, x1, y1

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

   RETURN Nil

STATIC FUNCTION SetCurrentPos( lSet )

   LOCAL clr := SetColor( Iif( lSet, clrCur,Iif( Empty(aBoardInit[nyPos,nxPos]), clrBoard, clrFix ) ) ), y1, x1

   index2Coors( nyPos, nxPos, @y1, @x1 )
   @ y1, x1 SAY Iif( Empty( aBoard[nyPos,nxPos] ), '.', aBoard[nyPos,nxPos] ) + ' '
   IF lSet
      DrawPane( Empty( aBoardInit[nyPos,nxPos] ) )
   ENDIF
   SetColor( clr )

   RETURN Nil

STATIC FUNCTION SetCellValue( nKey )

   LOCAL c := aBoard[nyPos,nxPos], aErr

   IF !Empty( aBoardInit[nyPos,nxPos] )
      edi_Alert( Iif( lRu, "Нельзя менять начальные значения!", "Can't change initial value!" ) )
      RETURN .F.
   ENDIF
   IF nKey == 32
      aBoard[nyPos,nxPos] := ''
      SetCurrentPos( .T. )
   ELSEIF CheckValue( nyPos, nxPos, Chr(nKey) )
      aBoard[nyPos,nxPos] := Chr(nKey)
      SetCurrentPos( .T. )
      Look4Empty( .T. )
   ELSE
      edi_Alert( Iif( lRu, "Ошибка!", "Illegal value!" ) )
      RETURN .F.
   ENDIF
   AddHis( c, aBoard[nyPos,nxPos] )
   IF nKey != 32 .AND. !Empty( aErr := Check2( nyPos,nxPos ) )
      edi_Alert( Iif( lRu, "Проблема в ", "Problem at " ) + Ltrim(Str(aErr[1])) + "/" + Ltrim(Str(aErr[2])) )
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
STATIC FUNCTION CheckValue( y, x, c )

   LOCAL i, j, y1, y2, x1, x2, lRes := .T.

   FOR i := 1 TO 9
      IF y != i
         IF aBoard[i,x] == c
            lRes := .F.
            EXIT
         ENDIF
      ENDIF
      IF x != i
         IF aBoard[y,i] == c
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
               IF aBoard[i,j] == c
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
            IF ( lRes := CheckValue( i, x, Chr(k) ) )
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
            IF ( lRes := CheckValue( y, i, Chr(k) ) )
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
               IF ( lRes := CheckValue( i, j, Chr(k) ) )
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

STATIC FUNCTION Solver()

   LOCAL i1 := 1, i, j, k, s, aBack[9,9]
   LOCAL nMin, sMin, aCoor[2], aSolver := Array( 81 ), nSolver := 0

   FOR i := 1 TO 9
      ACopy( aBoard[i], aBack[i] )
   NEXT

   DO WHILE i1 > 0
      i1 := 0
      nMin := 10
      FOR i := 1 TO 9
         FOR j := 1 TO 9
            IF Empty( aBoard[i,j] )
               i1 ++
               s := ""
               FOR k := 49 TO 57
                  IF CheckValue( i, j, Chr(k) )
                     s += Chr(k)
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
            aBoard[aSolver[nSolver,1],aSolver[nSolver,2]] := Left( aSolver[nSolver,3],1 )
            IF Len(aSolver[nSolver,3]) == 1
               nSolver --
               IF nSolver == 0
                  FOR i := 1 TO 9
                     ACopy( aBack[i], aBoard[i] )
                  NEXT
                  RETURN .F.
               ENDIF
            ELSE
               aSolver[nSolver,3] := Substr( aSolver[nSolver,3],2 )
               EXIT
            ENDIF
         ENDDO
      ELSE
         aBoard[aCoor[1],aCoor[2]] := Left( sMin, 1 )
         aSolver[++nSolver] := { aCoor[1], aCoor[2], sMin }
      ENDIF
   ENDDO

   s := ""
   FOR i := 1 TO 9
      FOR j := 1 TO 9
         s += aBoard[i,j]
      NEXT
      s += Chr(10)+Chr(13)
      ACopy( aBack[i], aBoard[i] )
   NEXT
   edi_writelog( s, "solver.log" )

   RETURN .T.

STATIC FUNCTION Look4Empty( l4End, i, j )

   FOR i := 1 TO 9
      FOR j := 1 TO 9
         IF Empty( aBoard[i,j] )
            RETURN .T.
         ENDIF
      NEXT
   NEXT
   IF l4End
      edi_Alert( Iif( lRu, "Задача решена!", "The task solved!" ) )
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
      NEXT
   ENDIF
   RETURN cSaved

STATIC FUNCTION Write_Game_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   s += "russian=" + Iif( lRu, "on","off" ) + Chr(13)+Chr(10)
   s += "clrtext=" + clrText + Chr(13)+Chr(10)
   s += "clrboard=" + clrBoard + Chr(13)+Chr(10)
   s += "clrfix=" + clrFix + Chr(13)+Chr(10)
   s += "clrborder=" + clrBorder + Chr(13)+Chr(10)
   s += "clrcur=" + clrCur + Chr(13)+Chr(10)
   IF nGameState == 1
      s += "saved=" + boa2Text() + Chr(13)+Chr(10)
   ENDIF

   hb_MemoWrit( cIniPath + "sudoku.ini", s )

   RETURN Nil
