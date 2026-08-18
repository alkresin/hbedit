/*
 * Zmejka (Snake)
 * HbEdit plugin
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4

#define SC_NONE       0

#define BOARD_HEIGHT  20
#define BOARD_WIDTH   32
#define BOARD_CLR   "GR+/N"

STATIC cIniPath
STATIC oEdit
STATIC hIdle
STATIC x1t, x2t, y1t, y2t
STATIC nGameState, lPaused, nLevel, nScores
STATIC nStartLevel := 0, nMaxScores := 0

// Направления: 0-вверх, 1-вправо, 2-вниз, 3-влево
STATIC nDirection := 1
STATIC nNextDirection := 1

// Координаты головы змеи
STATIC nHeadX := Int(BOARD_WIDTH/2)
STATIC nHeadY := Int(BOARD_HEIGHT/2)

// Тело змеи - массив сегментов {y, x}
STATIC aSnake := {}
STATIC nSnakeLen := 3

// Еда
STATIC nFoodX := 0
STATIC nFoodY := 0

// Игровое поле
STATIC aBoard[BOARD_HEIGHT, BOARD_WIDTH]
STATIC cScreenBuff

// Очки для перехода на следующий уровень
STATIC aLevelLimits := { 150, 350, 600, 900, 1400, 2200, 3100, 5000, 7500, 11000 }

FUNCTION plug_gm_zmejka( oEditIn, cPath )

   LOCAL i, cName := "$Zmejka"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Zmejka" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      hb_IdleDel( hIdle )
      RETURN Nil
   }

   IF Empty( cIniPath )
      Read_Zmejka_Ini( (cIniPath := cPath) + "zmejka.ini" )
   ENDIF

   IF ( i := Ascan( oEditIn:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEditIn, i )
      RETURN oEditIn:aWindows[i]
   ENDIF

   oEdit := mnu_NewBuf( oEditIn )
   edi_SetPalette( oEdit, "default" )
   oEdit:cFileName := cName
   oEdit:bWriteTopPane := bWPane
   oEdit:bOnKey := {|o,n| _Zmejka_OnKey(o,n) }
   oEdit:bStartEdit := {|| _Zmejka_Start() }
   oEdit:bEndEdit := bEndEdit
   oEdit:cp := "RU866"
   nGameState := 0
   lPaused := .F.
   nLevel := nStartLevel
   nScores := 0

   RETURN Nil

STATIC FUNCTION _Zmejka_Start()

   LOCAL i, j

   IF Empty( cScreenBuff )
      y1t := oEdit:y1 + Int((oEdit:y2-oEdit:y1-BOARD_HEIGHT)/2)
      y2t := y1t + BOARD_HEIGHT - 1
      x1t := oEdit:x1 + 10
      x2t := x1t + BOARD_WIDTH - 1

      SetColor( BOARD_CLR )
      Scroll( oEdit:y1, oEdit:x1, oEdit:y2, oEdit:x2 )
      @ y1t-1, x1t-1, y2t+1, x2t+1 BOX "┌─┐│┘─└│ "
      @ y1t+1, x2t+8 SAY "n   New game"
      @ y1t+2, x2t+8 SAY "p   Pause/Continue"
      @ y1t+3, x2t+8 SAY "+/- Start level (" + Ltrim(Str(nStartLevel)) + ")"

      @ y1t+5, x2t+8 SAY "Arrows - Move"
      @ y1t+6, x2t+8 SAY "Space - Speed up"

      @ y1t+10, x2t+8 SAY "Max scores: " + Ltrim(Str(nMaxScores))
      @ y1t+12, x2t+8 SAY "Level:  " + Ltrim(Str(nStartLevel))
      @ y1t+13, x2t+8 SAY "Scores: 0    "
      @ y1t+14, x2t+8 SAY "Length: 3    "
   ELSE
      RestScreen( oEdit:y1, oEdit:x1, oEdit:y2, oEdit:x2, cScreenBuff )
   ENDIF

   // Инициализация игрового поля
   FOR i := 1 TO BOARD_HEIGHT
      FOR j := 1 TO BOARD_WIDTH
         aBoard[i,j] := 0
      NEXT
   NEXT

   hIdle := hb_IdleAdd( {|| _Zmejka_Tf() } )

   RETURN Nil

STATIC FUNCTION _Zmejka_OnKey( oEditIn, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), i, j

   SetCursor( SC_NONE )

   IF nGameState == 1 .AND. !lPaused
      // Изменение направления
      IF nKey == K_UP .AND. nDirection != 2
         nNextDirection := 0
      ELSEIF nKey == K_RIGHT .AND. nDirection != 3
         nNextDirection := 1
      ELSEIF nKey == K_DOWN .AND. nDirection != 0
         nNextDirection := 2
      ELSEIF nKey == K_LEFT .AND. nDirection != 1
         nNextDirection := 3
      ELSEIF nKey == 32   // Space - ускорение
         _Zmejka_Update()
      ENDIF
   ENDIF

   IF nKey == 110   // n - новая игра
      SetColor( BOARD_CLR )
      IF nScores > nMaxScores
         nMaxScores := nScores
         @ y1t+10, x2t+20 SAY Ltrim(Str(nMaxScores))
      ENDIF
      nGameState := 1
      lPaused := .F.
      nScores := 0
      nLevel := nStartLevel
      nDirection := 1
      nNextDirection := 1
      nHeadX := Int(BOARD_WIDTH/2)
      nHeadY := Int(BOARD_HEIGHT/2)
      nSnakeLen := 3

      // Очистка поля
      FOR i := 1 TO BOARD_HEIGHT
         FOR j := 1 TO BOARD_WIDTH
            aBoard[i,j] := 0
         NEXT
      NEXT

      // Инициализация змеи
      aSnake := {}
      FOR i := 0 TO nSnakeLen-1
         AAdd( aSnake, { nHeadY, nHeadX - i } )
         aBoard[nHeadY, nHeadX - i] := 1
      NEXT

      Scroll( y1t, x1t, y2t, x2t )
      @ y1t+7, x2t+8 SAY Space(12)
      @ y1t+12, x2t+16 SAY Ltrim(Str( nLevel ))
      @ y1t+13, x2t+16 SAY Ltrim(Str( nScores ))
      @ y1t+14, x2t+16 SAY Ltrim(Str( nSnakeLen ))

      // Рисуем змею
      DrawSnake()
      // Создаём еду
      CreateFood()

   ELSEIF nKey == 112   // p - пауза
      IF nGameState == 1
         lPaused := !lPaused
         SetColor( BOARD_CLR )
         @ y1t+7, x2t+8 SAY Iif( lPaused, "GAME PAUSED", Space(12) )
      ENDIF

   ELSEIF nKey == 43 .OR. nKey == 45   // +,-
      IF nKey == 43 .AND. nStartLevel < 9
         nStartLevel ++
      ELSEIF nKey == 45 .AND. nStartLevel > 0
         nStartLevel --
      ENDIF
      SetColor( BOARD_CLR )
      @ y1t+3, x2t+25 SAY Ltrim(Str(nStartLevel))

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oEdit:y1, oEdit:x1, oEdit:y2, oEdit:x2 )
      IF Len( oEditIn:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_ESC
      cScreenBuff := Nil
      Write_Zmejka_Ini()
      mnu_Exit( oEditIn )

   ENDIF

   RETURN -1

FUNCTION _Zmejka_Tf()

   LOCAL nSec := Seconds()
   STATIC nSecPrev := 0

   IF nSec - nSecPrev > 0.02 * (22 - nLevel*2)
      SetCursor( SC_NONE )
      nSecPrev := nSec
      IF nGameState == 0 .OR. lPaused
         RETURN Nil
      ENDIF
      _Zmejka_Update()
   ENDIF

   RETURN Nil

STATIC FUNCTION _Zmejka_Update()

   LOCAL i, y, x, newHeadY, newHeadX, lEat := .F., yTail, xTail

   DispBegin()

   // Обновляем направление
   nDirection := nNextDirection

   // Вычисляем новую позицию головы
   newHeadY := nHeadY
   newHeadX := nHeadX

   DO CASE
      CASE nDirection == 0   // вверх
         newHeadY--
      CASE nDirection == 1   // вправо
         newHeadX++
      CASE nDirection == 2   // вниз
         newHeadY++
      CASE nDirection == 3   // влево
         newHeadX--
   ENDCASE

   // Проверяем столкновение со стенами
   IF newHeadY < 1 .OR. newHeadY > BOARD_HEIGHT .OR. newHeadX < 1 .OR. newHeadX > BOARD_WIDTH
      GameOver()
      DispEnd()
      RETURN Nil
   ENDIF

   // Проверяем, съели ли еду
   lEat := (newHeadY == nFoodY .AND. newHeadX == nFoodX)

   // Сохраняем хвост для удаления
   yTail := aSnake[1, 1]
   xTail := aSnake[1, 2]

   // Проверяем столкновение с телом
   // Если съели еду - хвост остаётся, значит он должен быть пустым
   // Если не съели - хвост убирается, и он не должен учитываться
   IF lEat
      // При поедании еды проверяем все сегменты тела (включая хвост)
      FOR i := 1 TO nSnakeLen
         IF aSnake[i, 1] == newHeadY .AND. aSnake[i, 2] == newHeadX
            GameOver()
            DispEnd()
            RETURN Nil
         ENDIF
      NEXT
   ELSE
      // Без еды - проверяем все сегменты кроме хвоста
      FOR i := 1 TO nSnakeLen - 1
         IF aSnake[i, 1] == newHeadY .AND. aSnake[i, 2] == newHeadX
            GameOver()
            DispEnd()
            RETURN Nil
         ENDIF
      NEXT
   ENDIF

   // Стираем старую змею
   FOR i := 1 TO nSnakeLen
      y := aSnake[i, 1]
      x := aSnake[i, 2]
      SetColor( BOARD_CLR )
      @ y1t-1+y, x1t+x-1 SAY " "
   NEXT

   // Удаляем хвост, если не съели еду
   IF !lEat
      aBoard[yTail, xTail] := 0
      hb_ADel( aSnake, 1, .T. )
      nSnakeLen--
   ENDIF

   // Добавляем новую голову
   nHeadY := newHeadY
   nHeadX := newHeadX
   AAdd( aSnake, { nHeadY, nHeadX } )
   aBoard[nHeadY, nHeadX] := 1
   nSnakeLen++

   // Рисуем обновлённую змею
   DrawSnake()

   IF lEat
      nScores += nLevel * 5 + 10
      @ y1t+13, x2t+16 SAY Ltrim(Str( nScores ))
      @ y1t+14, x2t+16 SAY Ltrim(Str( nSnakeLen ))

      // Проверяем повышение уровня
      IF nLevel < Len( aLevelLimits ) .AND. nScores > aLevelLimits[nLevel+1]
         nLevel ++
         @ y1t+12, x2t+16 SAY Ltrim(Str( nLevel ))
      ENDIF

      CreateFood()
   ENDIF

   DispEnd()

   RETURN Nil

STATIC FUNCTION DrawSnake()

   LOCAL i, y, x

   // Рисуем тело змеи
   FOR i := 1 TO nSnakeLen
      y := aSnake[i, 1]
      x := aSnake[i, 2]
      IF i == nSnakeLen
         SetColor( "BG+/N" )  // Голова - ярко-зелёная
         @ y1t-1+y, x1t+x-1 SAY "O"
      ELSE
         SetColor( "G/N" )    // Тело - зелёное
         @ y1t-1+y, x1t+x-1 SAY "o"
      ENDIF
   NEXT

   // Рисуем еду
   IF nFoodY > 0 .AND. nFoodY <= BOARD_HEIGHT .AND. nFoodX > 0 .AND. nFoodX <= BOARD_WIDTH
      SetColor( "R/N" )  // Красный для еды
      @ y1t-1+nFoodY, x1t+nFoodX-1 SAY "*"
   ENDIF

   RETURN Nil

STATIC FUNCTION CreateFood()

   LOCAL i, j, nEmpty := 0, nPos

   // Подсчитываем пустые клетки
   FOR i := 1 TO BOARD_HEIGHT
      FOR j := 1 TO BOARD_WIDTH
         IF aBoard[i, j] == 0
            nEmpty++
         ENDIF
      NEXT
   NEXT

   // Если нет пустых клеток - победа
   IF nEmpty == 0
      GameOver()
      RETURN Nil
   ENDIF

   // Выбираем случайную пустую клетку
   nPos := hb_RandomInt( 1, nEmpty )
   nEmpty := 0
   FOR i := 1 TO BOARD_HEIGHT
      FOR j := 1 TO BOARD_WIDTH
         IF aBoard[i, j] == 0
            nEmpty++
            IF nEmpty == nPos
               nFoodY := i
               nFoodX := j
               // Не отмечаем еду в aBoard, просто рисуем её
               DrawSnake()
               RETURN Nil
            ENDIF
         ENDIF
      NEXT
   NEXT

   RETURN Nil

STATIC FUNCTION GameOver()

   nGameState := 0
   SetColor( BOARD_CLR )
   @ y1t+7, x2t+8 SAY "GAME OVER"

   IF nScores > nMaxScores
      nMaxScores := nScores
      @ y1t+10, x2t+20 SAY Ltrim(Str(nMaxScores))
   ENDIF

   RETURN Nil

STATIC FUNCTION Read_Zmejka_Ini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "GAME"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "startlevel" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nStartLevel := Val( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "maxscores" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nMaxScores := Val( cTemp )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION Write_Zmejka_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   IF nScores > nMaxScores
      nMaxScores := nScores
   ENDIF

   s += "startlevel=" + Ltrim(Str( nStartLevel )) + Chr(13)+Chr(10)
   s += "maxscores=" + Ltrim(Str( nMaxScores )) + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "zmejka.ini", s )

   RETURN Nil