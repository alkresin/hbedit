/*
 * Lines game
 * HbEdit plugin
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */
#define K_LBUTTONDOWN 1002
#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4
#define SC_NONE       0
#define YSIZ          2
#define XSIZ          4
#define _HEIGHT       9
#define _WIDTH        9
#define BOARD_CLR   "GR+/N"
#define GM_STATE_1    1
#define GM_STATE_2    2
STATIC cIniPath
STATIC oGame
STATIC hIdle
STATIC x1t, x2t, y1t, y2t
STATIC nGameState, aPosFrom[2]
STATIC nScores
STATIC nMaxScores := 0
STATIC aColors := { "N", "N+", "G+", "RB+", "W+" }
STATIC aBoard[_HEIGHT,_WIDTH]
STATIC cScreenBuff

FUNCTION plug_gm_Lines( oEdit, cPath )
   LOCAL i, cName := "$Lines"
   LOCAL bWPane := { |o, l, y|
   LOCAL nCol := Col(), nRow := Row()

   IF Empty( l )
      DevPos( y, o:x1 )
      DevOut( "Lines" )
   ENDIF
   DevPos( nRow, nCol )

   RETURN Nil

   }
   LOCAL bEndEdit := { ||
   hb_IdleDel( hIdle )

   RETURN Nil

   }
   IF Empty( cIniPath )
      _Lines_RdIni( ( cIniPath := cPath ) + "gm_lines.ini" )
   ENDIF
   IF ( i := Ascan( oEdit:aWindows, { |o|o:cFileName == cName } ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF
   oGame := mnu_NewBuf( oEdit )
   edi_SetPalette( oGame, "solarized dark" )
   oGame:cFileName := cName
   oGame:bWriteTopPane := bWPane
   oGame:bOnKey := { |o, n| _Lines_OnKey( o, n ) }
   oGame:bStartEdit := { || _Lines_Start() }
   oGame:bEndEdit := bEndEdit
   oGame:cp := "RU866"
   nGameState := 0
   nScores := 0

   RETURN Nil

STATIC FUNCTION _Lines_Start()

   IF Empty( cScreenBuff )
      y1t := oGame:y1 + Int( ( oGame:y2 - oGame:y1 - _HEIGHT * YSIZ )/2 ); y2t := y1t + _HEIGHT * YSIZ - 1
      x1t := oGame:x1 + 10; x2t := x1t + _WIDTH * XSIZ - 1
      Setcolor( BOARD_CLR )
      Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
      @ y1t + 1, x2t + 8 SAY "n   New game"
      @ y1t + 3, x2t + 8 SAY "Max scores: " + LTrim( Str( nMaxScores ) )
      @ y1t + 4, x2t + 8 SAY "Scores: 0    "
      ClearBoard()
      DrawBoard()
   ELSE
      RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cScreenBuff )
   ENDIF
   hIdle := hb_IdleAdd( { || _Lines_Tf() } )

   RETURN Nil

STATIC FUNCTION _Lines_OnKey( oEdit, nKeyExt )
   LOCAL nKey := hb_keyStd( nKeyExt ), nCol, nRow, i, j, nf, aLine

   SetCursor( SC_NONE )
   IF nKey == K_LBUTTONDOWN
      nCol := MCol()
      nRow := MRow()
      IF nGameState > 0
         IF nRow >= y1t .AND. nRow <= y2t .AND. nCol >= x1t .AND. nCol <= x2t
            i := Int( ( nRow - y1t ) / YSIZ ) + 1
            j := Int( ( nCol - x1t ) / XSIZ ) + 1
            IF nGameState == GM_STATE_1 .AND. !Empty( aBoard[i,j] )
               nGameState := GM_STATE_2
               aPosFrom[1] := i; aPosFrom[2] := j
               DrawFig( i, j, .T. )
            ELSEIF nGameState == GM_STATE_2
               IF Empty( aBoard[i,j] )
                  nf := aBoard[aPosFrom[1], aPosFrom[2]]
                  IF FindPath( aBoard, aPosFrom[1], aPosFrom[2], i, j )
                     DrawBoard()
                     Inkey( 0.3 )
                     aBoard[aPosFrom[1], aPosFrom[2]] := 0
                     aBoard[i,j] := nf
                     DrawBoard()
                     IF !Empty( aLine := CheckLineOfFive( aBoard ) )
                        FOR i := 1 TO Len( aLine )
                           aBoard[aLine[i,1], aLine[i,2]] := 0
                           nScores += 2
                        NEXT
                        @ y1t + 4, x2t + 16 SAY LTrim( Str( nScores ) )
                        Inkey( 0.3 )
                        DrawBoard()
                     ELSE
                        AddBalls()
                        DrawBoard()
                        IF !Empty( aLine := CheckLineOfFive( aBoard ) )
                           FOR i := 1 TO Len( aLine )
                              aBoard[aLine[i,1], aLine[i,2]] := 0
                           NEXT
                           Inkey( 0.3 )
                           DrawBoard()
                        ENDIF
                     ENDIF
                     nGameState := GM_STATE_1
                  ENDIF
               ELSE
                  DrawFig( aPosFrom[1], aPosFrom[2] )
                  aPosFrom[1] := i; aPosFrom[2] := j
                  DrawFig( i, j, .T. )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ELSEIF nKey == 110   // n
      Setcolor( BOARD_CLR )
      IF nScores > nMaxScores
         nMaxScores := nScores
         @ y1t + 3, x2t + 20 SAY LTrim( Str( nMaxScores ) )
      ENDIF
      nGameState := GM_STATE_1
      nScores := 0
      ClearBoard()
      AddBalls()
      DrawBoard()
      @ y1t + 4, x2t + 16 SAY LTrim( Str( nScores ) )
   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF
   ELSEIF nKey == K_ESC
      cScreenBuff := Nil
      _Lines_WrIni()
      mnu_Exit( oEdit )
   ENDIF

   RETURN - 1

FUNCTION _Lines_Tf()
   LOCAL nSec := Seconds()
   STATIC nSecPrev := 0

   IF nGameState == GM_STATE_2
      IF nSec - nSecPrev > 0.1
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION FindPath( board, sy, sx, ey, ex )
   LOCAL nSize := 9
   LOCAL nPathMark := 99

   // visited[y][x]: 0 = не посещена, 1 = посещена
   LOCAL visited := Array( nSize, nSize )
   LOCAL parent  := Array( nSize, nSize )  // хранит [py, px]
   LOCAL i, j, cx, cy, p
   // Очередь: массив ячеек, каждая ячейка ? массив [y, x]
   LOCAL queue := {}
   LOCAL head  := 1      // индекс головы (следующий элемент для чтения)
   LOCAL tail  := 0      // индекс хвоста (последний записанный)
   // Направления: вверх, вниз, влево, вправо
   LOCAL dy := { - 1, 1, 0, 0 }
   LOCAL dx := { 0, 0, - 1, 1 }
   LOCAL dir, ny, nx, cur, py, px
   FOR i := 1 TO nSize
      FOR j := 1 TO nSize
         visited[i][j] := 0
         parent[i][j]  := {}  // пустой массив = "нет родителя"
      NEXT
   NEXT
   // Добавляем старт в очередь
   AAdd( queue, { sy, sx } )
   tail := 1
   visited[sy][sx] := 1
   DO WHILE head <= tail
      cur := queue[head]
      head ++
      py := cur[1]
      px := cur[2]
      // Если достигли цели
      IF py == ey .AND. px == ex
         // Восстанавливаем путь назад от финиша к старту
         cy := ey
         cx := ex
         // Помечаем финиш
         board[cy][cx] := nPathMark
         DO WHILE !( cy == sy .AND. cx == sx )
            p := parent[cy][cx]
            // Защита на случай ошибки (если родителя нет)
            IF Empty( p )
               RETURN .F.
            ENDIF
            cy := p[1]
            cx := p[2]
            board[cy][cx] := nPathMark
         ENDDO
         RETURN .T.
      ENDIF
      // Перебираем соседей
      FOR dir := 1 TO 4
         ny := py + dy[dir]
         nx := px + dx[dir]
         // Проверка границ И проверка, что ячейка не посещена и свободна
         IF ny >= 1 .AND. ny <= nSize .AND. nx >= 1 .AND. nx <= nSize ;
               .AND. visited[ny][nx] == 0 .AND. board[ny][nx] == 0
            visited[ny][nx] := 1
            parent[ny][nx]  := { py, px }
            AAdd( queue, { ny, nx } )
            tail ++
         ENDIF
      NEXT
   ENDDO
   // Путь не найден

   RETURN .F.

FUNCTION CheckLineOfFive( board )
    LOCAL nSize := 9
    LOCAL y, x, val, count, startY, startX, i
    LOCAL aResult
    LOCAL startDy, startDx

    // --- Горизонтали ---
    FOR y := 1 TO nSize
        x := 1
        DO WHILE x <= nSize
            IF board[y][x] != 0
                startY := y
                startX := x
                val := board[y][x]
                count := 0

                // Досчитываем всю непрерывную группу одинаковых значений
                DO WHILE x <= nSize .AND. board[y][x] == val
                    count++
                    x++
                ENDDO

                IF count >= 5
                    aResult := {}
                    FOR i := 0 TO count - 1
                        AAdd( aResult, { startY, startX + i } )
                    NEXT
                    RETURN aResult
                ENDIF
            ELSE
                x++
            ENDIF
        ENDDO
    NEXT

    // --- Вертикали ---
    FOR x := 1 TO nSize
        y := 1
        DO WHILE y <= nSize
            IF board[y][x] != 0
                startY := y
                startX := x
                val := board[y][x]
                count := 0

                DO WHILE y <= nSize .AND. board[y][x] == val
                    count++
                    y++
                ENDDO

                IF count >= 5
                    aResult := {}
                    FOR i := 0 TO count - 1
                        AAdd( aResult, { startY + i, startX } )
                    NEXT
                    RETURN aResult
                ENDIF
            ELSE
                y++
            ENDIF
        ENDDO
    NEXT

    // --- Диагональ: вниз-вправо (\) ---
    FOR startY := 1 TO nSize
        FOR startX := 1 TO nSize
            // Пропускаем, если диагональ из этой точки заведомо короче 5
            IF (nSize - startY + 1) < 5 .OR. (nSize - startX + 1) < 5
               LOOP
            ENDIF

            y := startY
            x := startX
            IF board[y][x] == 0
                LOOP
            ENDIF

            val := board[y][x]
            count := 0
            startDy := y; startDx := x

            // Досчитываем по диагонали
            DO WHILE y <= nSize .AND. x <= nSize .AND. board[y][x] == val
                count++
                y++
                x++
            ENDDO

            IF count >= 5
                aResult := {}
                FOR i := 0 TO count - 1
                    AAdd( aResult, { startDy + i, startDx + i } )
                NEXT
                RETURN aResult
            ENDIF
        NEXT
    NEXT

    // --- Диагональ: вниз-влево (/) ---
    FOR startY := 1 TO nSize
        FOR startX := 1 TO nSize
            // Проверка, хватит ли длины диагонали вниз-влево
            IF (nSize - startY + 1) < 5 .OR. startX < 5
                LOOP
            ENDIF

            y := startY
            x := startX
            IF board[y][x] == 0
                LOOP
            ENDIF

            val := board[y][x]
            count := 0
            startDy := y; startDx := x

            // Досчитываем по диагонали вниз-влево
            DO WHILE y <= nSize .AND. x >= 1 .AND. board[y][x] == val
                count++
                y++
                x--
            ENDDO

            IF count >= 5
                aResult := {}
                FOR i := 0 TO count - 1
                    AAdd( aResult, { startDy + i, startDx - i } )
                NEXT
                RETURN aResult
            ENDIF
        NEXT
    NEXT

    RETURN Nil

STATIC FUNCTION ClearBoard()
   LOCAL i, j

   FOR i := 1 TO _HEIGHT
      FOR j := 1 TO _WIDTH
         aBoard[i,j] := 0
      NEXT
   NEXT

   RETURN Nil

STATIC FUNCTION DrawBoard()
   LOCAL i, j

   DispBegin()
   FOR i := 1 TO _HEIGHT
      FOR j := 1 TO _WIDTH
         DrawFig( i, j )
      NEXT
   NEXT
   DispEnd()

   RETURN Nil

STATIC FUNCTION DrawFig( i, j, lSele )
   LOCAL y := y1t + ( i - 1 ) * YSIZ, x := x1t + ( j - 1 ) * XSIZ
   LOCAL cClrB := iif( ( _WIDTH * (i - 1 ) + j ) % 2 == 1, "/GR", "/RB" )

   IF aBoard[i,j] == 0
      Setcolor( "N" + cClrB )
      @ y,   x SAY "    "
      @ y + 1, x SAY "    "
   ELSEIF aBoard[i,j] == 99
      aBoard[i,j] := 0
      Setcolor( "N" + cClrB )
      @ y,   x SAY " ▄▄ "
      @ y + 1, x SAY "    "
   ELSE
      Setcolor( aColors[aBoard[i,j]] + cClrB )
      IF !Empty( lSele )
         @ y,   x SAY "████"
         @ y + 1, x SAY "████"
      ELSE
         @ y,   x SAY " ▄▄ "
         @ y + 1, x SAY " ▀▀ "
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION AddBalls()
   LOCAL i, j, n := 0, arr, n1, n2, n3

   FOR i := 1 TO _HEIGHT
      FOR j := 1 TO _WIDTH
         IF aBoard[i,j] == 0
            n ++
         ENDIF
      NEXT
   NEXT
   arr := Array( n )
   n1 := hb_RandomInt( 1, n )
   DO WHILE ( n2 := hb_RandomInt( 1, n ) ) == n1; ENDDO
   DO WHILE ( n3 := hb_RandomInt( 1, n ) ) == n1 .OR. n3 == n2; ENDDO
   n := 0
   FOR i := 1 TO _HEIGHT
      FOR j := 1 TO _WIDTH
         IF aBoard[i,j] == 0
            n ++
            IF n == n1 .OR. n == n2 .OR. n == n3
               aBoard[i,j] := hb_RandomInt( 1, Len( aColors ) )
            ENDIF
         ENDIF
      NEXT
   NEXT

   RETURN Nil

STATIC FUNCTION _Lines_RdIni( cIni )
   LOCAL hIni, aIni, nSect, cTemp, aSect

   IF !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper( aIni[nSect] ) == "GAME"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "maxscores" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nMaxScores := Val( cTemp )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

STATIC FUNCTION _Lines_WrIni()
   LOCAL s := "[GAME]" + Chr( 13 ) + Chr( 10 )

   IF nScores > nMaxScores
      nMaxScores := nScores
   ENDIF
   s += "maxscores=" + LTrim( Str( nMaxScores ) ) + Chr( 13 ) + Chr( 10 )
   hb_MemoWrit( cIniPath + "gm_lines.ini", s )

   RETURN Nil