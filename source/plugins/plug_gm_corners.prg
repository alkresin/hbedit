/*
 * Corners game
 * HbEdit plugin
 *
 * Copyright 2022 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define CTRL_PRESSED  0x020000
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
#define K_F8         -7
#define K_F9         -8
#define K_F10        -9
#define K_LBUTTONDOWN 1002

#define POS_LEN         1
#define POS_BOARD       1

STATIC cIniPath
STATIC oGame
STATIC x1t, y1t, x2t, nyPos, nxPos
STATIC nScrolled
STATIC nLevel1, nLevel2, nMoveState, nMoveFrom, nMoveTo
STATIC cScreenBuff
STATIC clrBoard := "GR+/N", clrWhite := "W+", clrBlack := "N", clrbWhite := "BG", clrbBlack := "GR"
STATIC clrSeleB := "N", clrSeleW := "W"
STATIC lRussian := .T.

STATIC cInitBoard := "ppp     ppp     ppp                          PPP     PPP     PPP"
STATIC aFigs := {' ','p','P'}, ;
       aFigs1 :={' ','O','H'}

STATIC aBoardValues := { ;
    { 20,18,16,10,9, 9, 8, 7,  ;  //White
      18,16,14,10,9, 8, 7, 6,  ;
      16,14,12,9, 8, 7, 6, 5,  ;
      10,10,9, 8, 7, 6, 5, 4,  ;
      9, 9, 8, 7, 6, 5, 4, 3,  ;
      9, 8, 7, 6, 5, 4, 3, 2,  ;
      8, 7, 6, 5, 4, 3, 2, 1,  ;
      7, 6, 5, 4, 3, 2, 1, 0 },;
    { 0, 1, 2, 3, 4, 4, 4, 4,  ;  //Black
      1, 2, 3, 4, 5, 6, 6, 6,  ;
      2, 3, 4, 5, 6, 7, 8, 8,  ;
      3, 4, 5, 6, 7, 8, 9, 9,  ;
      4, 5, 6, 7, 8, 9,10,10,  ;
      4, 6, 7, 8, 9,12,14,16,  ;
      4, 6, 8, 9,10,14,16,18,  ;
      4, 6, 8, 9,10,16,18,20 } }

STATIC lTurnBlack, nSummWin

STATIC aCurrPos
STATIC aHistory
STATIC lPlayGame

FUNCTION plug_gm_Corners( oEdit, cPath )

   LOCAL i, cName := "$Corners"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      Scroll( y, o:x1, y, o:x2 )
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "F9-menu  Corners" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }

   nLevel1 := 0; nLevel2 := 1

   IF Empty( cIniPath )
      cIniPath := cPath
      Read_Game_Ini( (cIniPath := cPath) + "corners.ini" )
   ENDIF

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   oGame := mnu_NewBuf( oEdit )
   edi_SetPalette( oGame, "default" )
   oGame:cFileName := cName
   oGame:bWriteTopPane := bWPane
   oGame:bOnKey := {|o,n| _Game_OnKey(o,n) }
   oGame:bStartEdit := {|| _Game_Start() }
   hb_cdpSelect( oGame:cp := "UTF8" )
   oGame:lUtf8 := .T.
   oGame:lIns := Nil
   aCurrPos := Array( POS_LEN )
   nSummWin := aBoardValues[1,1] + aBoardValues[1,2] + aBoardValues[1,3] + ;
               aBoardValues[1,9] + aBoardValues[1,10] + aBoardValues[1,11] + ;
               aBoardValues[1,17] + aBoardValues[1,18] + aBoardValues[1,19]

   RETURN Nil

STATIC FUNCTION _Game_Start()

   IF Empty( cScreenBuff )
      y1t := oGame:y1 + 3
      x1t := oGame:x1 + 4
      x2t := x1t + 24
      _Game_New( .T. )
   ELSE
      RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cScreenBuff )
   ENDIF

   RETURN Nil

STATIC FUNCTION _Game_Exit()

   cScreenBuff := Nil
   Write_Game_Ini()
   mnu_Exit( oGame )

   RETURN Nil

STATIC FUNCTION _Game_New( lFirst )

   SetColor( clrBoard )
   Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
   lPlayGame := .T.
   nMoveState := 0
   aCurrPos[POS_BOARD] := cInitBoard
   aHistory   := {}
   DrawBoard()
   nScrolled := 0
   lTurnBlack := .F.
   _Game_Players( !lFirst )

   RETURN Nil

STATIC FUNCTION _Game_Help()

   LOCAL cBuff := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
   LOCAL oldc := SetColor( clrWhite+"/"+clrbBlack )

   hb_cdpSelect( "RU866" )
   @ y1t, x1t, y1t+12, x2t+36 BOX "\Uffffffff\Uffffffff\Uffffffff\Uffffffff3 "
   hb_cdpSelect( oGame:cp )

   @ y1t+1, x1t + 4 SAY Iif( lRussian, "\u0428\u0430\u0445\u043c\u0430\u0442\u044b", "Chess game" )
   @ y1t+2, x1t + 4 SAY Iif( lRussian, "F9 - \u0413\u043b\u0430\u0432\u043d\u043e\u0435 \u043c\u0435\u043d\u044e", "F9 - Main menu" )
   @ y1t+4, x1t + 4 SAY Iif( lRussian, "F3 - \u041d\u043e\u0432\u0430\u044f \u043f\u0430\u0440\u0442\u0438\u044f", "F3 - New game" )
   @ y1t+5, x1t + 4 SAY Iif( lRussian, "F6 - \u0418\u0437\u043c\u0435\u043d\u0438\u0442\u044c \u0438\u0433\u0440\u043e\u043a\u043e\u0432", "F6 - Change players order and level" )
   @ y1t+5, x1t + 4 SAY Iif( lRussian, "F8 - Switch Russian/English notation", "F8 - \u041f\u0435\u0440\u0435\u043a\u043b\u044e\u0447\u0438\u0442\u044c \u044f\u0437\u044b\u043a (\u0420\u0443\u0441\u0441\u043a\u0438\u0439/\u0410\u043d\u0433\u043b\u0438\u0439\u0441\u043a\u0438\u0439)" )
   @ y1t+6, x1t + 4 SAY Iif( lRussian, "Backspace - \u0412\u0435\u0440\u043d\u0443\u0442\u044c \u0445\u043e\u0434 \u043d\u0430\u0437\u0430\u0434", "Backspace - Turn back" )
   @ y1t+7, x1t + 4 SAY Iif( lRussian, "Ctrl-N - \u041f\u0440\u0435\u0434\u043e\u0441\u0442\u0430\u0432\u0438\u0442\u044c \u043a\u043e\u043c\u043f\u044c\u044e\u0442\u0435\u0440\u0443 \u043f\u0440\u0430\u0432\u043e \u0441\u0434\u0435\u043b\u0430\u0442\u044c \u0445\u043e\u0434", "Ctrl-N - Let computer make a turn" )
   @ y1t+8, x1t + 4 SAY Iif( lRussian, "ESC, F10 - \u0412\u044b\u0445\u043e\u0434", "ESC, F10 - Exit" )

   Inkey( 0 )
   SetColor( oldc )
   RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cBuff )

   RETURN Nil

STATIC FUNCTION _Game_Level( nTitle )

   LOCAL aMenu := { "--- " + Iif( nTitle==1, Iif(lRussian,"Белые","White"),Iif(lRussian,"Черные","Black") ) + " ---", Iif(lRussian,"Уровень 0","Level 0"), Iif(lRussian,"Уровень 1","Level 1") }

   RETURN FMenu( oGame, aMenu, y1t, x2t+2, y1t+4, x2t+40,,, 2 )

STATIC FUNCTION _Game_Players( lAsk )

   LOCAL nc, nc2, nc3
   STATIC aMenuR := { "Человек - Компьютер", "Компьютер - Человек", "Человек - Человек", "Компьютер - Компьютер" }
   STATIC aMenuE := { "Human - Computer", "Computer - Human", "Human - Human", "Computer - Computer" }

   IF lAsk
      nc := FMenu( oGame, Iif( lRussian, aMenuR, aMenuE ), y1t, x2t+2 ) //, y1t+5, x2t+40 )

      IF nc == 0
         RETURN Nil

      ELSEIF nc == 3
         nLevel1 := nLevel2 := 0

      ELSEIF nc < 3
         IF ( nc2 := _Game_Level( Iif( nc == 1, 2, 1 ) ) ) > 1
            nLevel1 := Iif( nc==1, 0, nc2-1 ); nLevel2 := Iif( nc==1, nc2 - 1, 0 )
         ENDIF

      ELSEIF nc == 4
         IF ( nc2 := _Game_Level( 1 ) ) > 0
            nLevel1 := nc2 - 1
            IF ( nc3 := _Game_Level( 2 ) ) > 0
               nLevel2 := nc3 - 1
            ENDIF
         ENDIF

      ENDIF
   ENDIF

   IF lRussian
      @ y1t-1, x1t SAY Iif( nLevel1 == 0, "Человек  ", "Компьютер" )
      @ y1t-1, x1t+15 SAY Iif( nLevel2 == 0, "Человек  ", "Компьютер" )
   ELSE
      @ y1t-1, x1t SAY Iif( nLevel1 == 0, "Human   ", "Computer" )
      @ y1t-1, x1t+15 SAY Iif( nLevel2 == 0, "Human   ", "Computer" )
   ENDIF
   IF lPlayGame .AND. ( (lTurnBlack .AND. nLevel2 > 0) .OR. (!lTurnBlack .AND. nLevel1 > 0) )
      ii_MakeMove()
   ENDIF

   RETURN Nil

STATIC FUNCTION _Game_MainMenu()

   LOCAL nc
   STATIC aMenuR := { {"Выход",,,"Esc,F10"}, {"Новая партия",,,"F3"}, {"Игроки",,,"F6"}, {"Rus/Eng",,,"F8"} }
   STATIC aMenuE := { {"Exit",,,"Esc,F10"}, {"New Game",,,"F3"}, {"Change players",,,"F6"}, {"Rus/Eng",,,"F8"} }

   IF ( nc := FMenu( oGame, Iif( lRussian, aMenuR, aMenuE ), y1t, x2t+2 ) ) == 1
      _Game_Exit()

   ELSEIF nc == 2
      _Game_New( .F. )

   ELSEIF nc == 3
      _Game_Players( .T. )

   ELSEIF nc == 4
      lRussian := !lRussian
      DrawBoard()

   ENDIF

   RETURN Nil

STATIC FUNCTION _Game_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol, nRow, arr, n

   IF nKey == K_LBUTTONDOWN
      nCol := MCol()
      nRow := MRow()
      IF nRow == oEdit:y1-1 .AND. nCol < 8
         _Game_MainMenu()
      ELSE
         IF lPlayGame
            IF nRow >= y1t .AND. nRow <= y1t + 7 .AND. nCol >= x1t .AND. nCol <= x1t + 24
               MakeMove( nRow - y1t + 1, Int((nCol - x1t) / 3) + 1 )
            ENDIF
         ENDIF
      ENDIF

   ELSEIF nKey == K_BS
      IF lPlayGame .AND. !Empty( aHistory )
         IF ATail( aHistory )[2] == Nil
            hb_ADel( aHistory, Len(aHistory), .T. )
            ATail( aHistory )[2] := Nil
         ELSE
            hb_ADel( aHistory, Len(aHistory), .T. )
         ENDIF
         arr := aHistory
         nCol := nLevel1; nRow := nLevel2; nLevel1 := nLevel2 := 0
         _Game_New( .T. )
         ReplayGame( arr )
         nLevel1 := nCol; nLevel2 := nRow
      ENDIF

   ELSEIF nKey == K_F3
      _Game_New( .F. )

   ELSEIF nKey == K_F6
      _Game_Players( .T. )

   ELSEIF nKey == K_F8
      lRussian := !lRussian
      DrawBoard()

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_F1
      _Game_Help()

   ELSEIF nKey == K_F9
     _Game_MainMenu()

   ELSEIF nKey == K_CTRL_N
     IF lPlayGame
        ii_MakeMove()
     ENDIF

   ELSEIF nKey == K_ESC .OR. nKey == K_F10
      _Game_Exit()

   ENDIF

   RETURN -1

STATIC FUNCTION DrawBoard()

   LOCAL i, j, i1, lBlack := .F., nPos, c, cBoard := aCurrPos[POS_BOARD]

   DispBegin()
   SetColor( clrBoard )
   FOR i := 0 TO 7
      SetColor( clrBoard )
      @ y1t + i, x1t - 1 SAY Ltrim(Str(8-i))
      FOR j := 1 TO 8
         nPos := i*8+j
         c := Substr( cBoard, nPos, 1 )
         IF ( nMoveState == 1 .AND. nPos == nMoveFrom ) .OR. ;
            ( nMoveState == 2 .AND. ( nPos == nMoveFrom .OR. nPos == nMoveTo ) )
            SetColor( Iif( lTurnBlack, clrBlack+"/"+clrSeleW, clrWhite+ "/"+clrSeleB ) )
         ELSE
            SetColor( Iif( c > 'Z', clrBlack, clrWhite ) + "/" + Iif( lBlack, clrbBlack, clrbWhite ) )
         ENDIF
         i1 := Ascan( aFigs,c )
         @ y1t + i, x1t + (j-1)*3 SAY " " + aFigs1[i1] + " "
         lBlack := !lBlack
      NEXT
      lBlack := !lBlack
   NEXT
   SetColor( clrBoard )
   FOR j := 1 TO 8
      @ y1t + 8, x1t + (j-1)*3 + 1 SAY Chr(96+j)
   NEXT
   DispEnd()

   RETURN Nil

STATIC FUNCTION MakeMove( nRow, nCol )

   LOCAL nMove := (nRow-1)*8 + nCol, nSumm, nCou := 0
   LOCAL c := Substr( aCurrPos[POS_BOARD], nMove, 1 ), cBoa16

   IF nMoveState == 0 .AND. ( (!lTurnBlack .AND. c == 'P') .OR. ;
      (lTurnBlack .AND. c == 'p') )
      nMoveFrom := nMove
      nMoveState := 1
      DrawMove( nMoveFrom )

   ELSEIF nMoveState == 1
      IF isMoveCorrect( nMove )
         nMoveState := 2
         nMoveTo := nMove
         DrawMove( nMoveFrom, nMove )
         nMoveState := 0
         //Inkey( 0.5 )
         //DrawBoard()

         //nSumm := Iif( lTurnBlack, -ii_Ocenka( aCurrPos[POS_BOARD] ), ii_Ocenka( aCurrPos[POS_BOARD] ) )
         nSumm := ii_Ocenka( aCurrPos[POS_BOARD], lTurnBlack )
         //edi_writelog( str(nSumm)+" "+str(nSummWin) )
         IF nSumm == nSummWin
            GameOver( 1 )
         ELSE
            lTurnBlack := !lTurnBlack
            IF Iif( lTurnBlack, nLevel2, nLevel1 ) > 0
               ii_MakeMove()
            ENDIF
         ENDIF
      ELSE
         DrawMove( -1 )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION MoveN2C( nStart, nEnd )

   LOCAL i1, cMove := ""

   IF !Empty( nStart )
      i1 := Iif( nStart%8 == 0, 8, nStart%8 )
      cMove += Chr( 96 + i1 ) + Ltrim(Str(Int(9-nStart/8)))
   ENDIF
   IF !Empty( nEnd )
      i1 := Iif( nEnd%8 == 0, 8, nEnd%8 )
      cMove += Iif( Empty(nStart),'', '-' ) + Chr( 96+i1 ) + Ltrim(Str(Int(9-nEnd/8)))
   ENDIF

   RETURN cMove

STATIC FUNCTION DrawMove( nStart, nEnd )

   LOCAL cFig, xPos := x1t+2

   IF lTurnBlack
      xPos := x1t + 12
   ELSE
      @ y1t+10, xPos+10 SAY Space( 8 )
   ENDIF
   @ y1t+10, xPos SAY Space( 8 )

   IF nStart < 0
      DevPos( y1t+10, xPos )
      IF lRussian
         DevOut( Iif( nStart==-1, "ОШИБКА", "Ждите.." ) )
      ELSE
         DevOut( Iif( nStart==-1, "WRONG MOVE", "Wait..." ) )
      ENDIF
      DrawBoard()
      RETURN Nil
   ENDIF

   IF !Empty( nEnd )
      cFig := Substr( aCurrPos[POS_BOARD], nStart, 1 )
      aCurrPos[POS_BOARD] := hb_bPoke( hb_bPoke( aCurrPos[POS_BOARD], nStart, 32 ), nEnd, Asc(cFig) )
      AddHis( nStart, nEnd )
   ENDIF
   DrawBoard()

   DevPos( y1t+10, xPos )
   DevOut( MoveN2C( nStart,nEnd ) )

   RETURN Nil

STATIC FUNCTION GenMoves( aPos, nStart, aMoves )

   STATIC arr := { -1, 1, -8, 8 }
   LOCAL lFirst := .F., i, nMove, nCol1

   IF aMoves == Nil
      aMoves := {}
      lFirst := .T.
   ENDIF
   FOR i := 1 TO Len( arr )
      nMove := nStart

      nCol1 := nMove % 8
      nMove += arr[i]
      IF nMove < 0 .OR. nMove > 64 .OR. ( nCol1==1 .AND. arr[i]==-1 ) .OR. ( nCol1==0 .AND. arr[i]==1 )
         LOOP
      ENDIF

      IF Substr( aPos[POS_BOARD], nMove, 1 ) == ' '
         IF lFirst
            Aadd( aMoves, nMove )
         ELSE
            LOOP
         ENDIF
      ELSE
         nCol1 := nMove % 8
         nMove += arr[i]
         IF nMove < 0 .OR. nMove > 64 .OR. ( nCol1==1 .AND. arr[i]==-1 ) .OR. ( nCol1==0 .AND. arr[i]==1 )
            LOOP
         ENDIF
         IF Substr( aPos[POS_BOARD], nMove, 1 ) == ' '
            IF Ascan( aMoves, nMove ) == 0
               Aadd( aMoves, nMove )
               GenMoves( aPos, nMove, aMoves )
            ENDIF
         ENDIF
      ENDIF

   NEXT
/*
   IF lFirst
      for i := 1 to len(amoves)
        edi_writelog( "= "+str(amoves[i]) )
      next
      edi_writelog( "" )
   ENDIF
*/
   RETURN aMoves

STATIC FUNCTION isMoveCorrect( nMove )

   IF nMove == nMoveFrom
      RETURN .F.
   ENDIF
   IF Ascan( GenMoves( aCurrPos, nMoveFrom ), nMove ) == 0
      RETURN .F.
   ENDIF

   RETURN .T.

STATIC FUNCTION AddHis( nStart, nEnd )

   LOCAL arr := { nStart, nEnd }

   IF lTurnBlack
      ATail( aHistory )[2] := arr
   ELSE
      AAdd( aHistory, { arr, Nil } )
   ENDIF

   RETURN Nil

STATIC FUNCTION ii_Ocenka( cBoard, lBlack )

   LOCAL i, cFig, nSumm := 0

   IF lBlack
      FOR i := 1 TO 64
         IF ( cFig := Substr( cBoard, i, 1 ) ) == 'p'
            nSumm += aBoardValues[2,i]
         ENDIF
      NEXT
   ELSE
      FOR i := 1 TO 64
         IF ( cFig := Substr( cBoard, i, 1 ) ) == 'P'
            nSumm += aBoardValues[1,i]
         ENDIF
      NEXT
   ENDIF
   RETURN nSumm

STATIC FUNCTION ii_ScanBoard_1( nLevel )

   LOCAL i, j, i1, j1, nFig, arr, arr2, nLen, nLen2, nOcen := -1000000, nSumm, nSumm2
   LOCAL aMaxOcen := { Nil, Nil, nOcen }, aReply, lExit := .F.
   LOCAL aPosTemp := Array(POS_LEN), aPosT2 := Array(POS_LEN), cFig

   FOR i := 1 TO 64
      IF ( nFig := hb_bPeek( aCurrPos[POS_BOARD], i ) ) >= 65 .AND. ;
         ( ( lTurnBlack .AND. nFig >= 97 ) .OR. ( !lTurnBlack .AND. nFig < 97 ) )
         arr := GenMoves( aCurrPos, i )
         nLen := Len( arr )
         FOR j := 1 TO nLen
            cFig := Substr( aCurrPos[POS_BOARD], i, 1 )
            aPosTemp[POS_BOARD] := hb_bPoke( hb_bPoke( aCurrPos[POS_BOARD], i, 32 ), arr[j], Asc(cFig) )
            nSumm := ii_Ocenka( aPosTemp[POS_BOARD], lTurnBlack )
            IF nSumm == nSummWin
               lExit := .T.
               aMaxOcen[3] := nOcen := nSumm; aMaxOcen[1] := i; aMaxOcen[2] := arr[j]
               EXIT
            ENDIF
            IF nLevel > 1
               FOR i1 := 1 TO 64
                  IF ( nFig := hb_bPeek( aPosTemp[POS_BOARD], i1 ) ) >= 65 .AND. ;
                     ( ( lTurnBlack .AND. nFig >= 97 ) .OR. ( !lTurnBlack .AND. nFig < 97 ) )
                     arr2 := GenMoves( aPosTemp, i1 )
                     nLen2 := Len( arr2 )
                     FOR j1 := 1 TO nLen2
                        cFig := Substr( aPosTemp[POS_BOARD], i1, 1 )
                        aPosT2[POS_BOARD] := hb_bPoke( hb_bPoke( aPosTemp[POS_BOARD], i1, 32 ), arr2[j1], Asc(cFig) )
                        nSumm2 := nSumm + ii_Ocenka( aPosT2[POS_BOARD], lTurnBlack )
                        IF nSumm2 > nOcen .OR. ( nSumm2 == nOcen .AND. hb_Random() > 0.75 )
                           aMaxOcen[3] := nOcen := nSumm2; aMaxOcen[1] := i; aMaxOcen[2] := arr[j]
                        ENDIF
                     NEXT
                  ENDIF
               NEXT
            ELSE
               IF nSumm > nOcen .OR. ( nSumm == nOcen .AND. hb_Random() > 0.75 )
                  aMaxOcen[3] := nOcen := nSumm; aMaxOcen[1] := i; aMaxOcen[2] := arr[j]
               ENDIF
            ENDIF
            //edi_writelog( MoveN2C(i,arr[j]) + "  " + str(nSumm,8) + " " + str(nOcen,8) + " " + Valtype(aMaxOcen[1]) )
         NEXT
         IF lExit
            EXIT
         ENDIF
      ENDIF
   NEXT

   RETURN aMaxOcen

STATIC FUNCTION ii_MakeMove()

   LOCAL cFig, nSec, nCou := 0, nKey
   LOCAL aMaxOcen, cBoa, cMoves, n

   DrawMove( -2 )

   nSec := Seconds()
   aMaxOcen := ii_ScanBoard_1( Iif( lTurnBlack, nLevel2, nLevel1 ) )
   @ y1t+11, Iif( lTurnBlack, x1t+12, x1t+2 ) SAY Ltrim(Str( Seconds()-nSec,6,2 ))

   IF aMaxOcen[1] == Nil
      GameOver( 1 )
   ELSE
      nMoveState := 2
      nMoveFrom := amaxOcen[1]
      nMoveTo := amaxOcen[2]
      DrawMove( aMaxOcen[1], amaxOcen[2] )
      nMoveState := 0
      //Inkey( 1 )
      //DrawBoard()

      IF aMaxOcen[3] > 50000
         GameOver( 2 )
      ENDIF
   ENDIF

   lTurnBlack := !lTurnBlack
   IF Iif( lTurnBlack, nLevel2, nLevel1 ) > 0
       IF ( nKey := Inkey( 1 ) ) == K_F6
         _Game_Players( .T. )
       ELSE
          KEYBOARD Chr( K_CTRL_N )
       ENDIF
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

STATIC FUNCTION ReplayGame( aHis )

   LOCAL i, aMove

   aHistory  := {}
   FOR i := 1 TO Len( aHis )
      aMove := aHis[i,1]
      DrawMove( aMove[1], aMove[2] )
      lTurnBlack := .T.
      IF !Empty( aMove := aHis[i,2] )
         DrawMove( aMove[1], aMove[2] )
         lTurnBlack := .F.
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION Read_Game_Ini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect
   LOCAL cFigureB, cFigureW

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
               IF hb_hHaskey( aSect, cTemp := "clrseleb" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrSeleB := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrselew" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrSeleW := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "figurewhite" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  aFigs1[3] := cFigureW := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "figureblack" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  aFigs1[2] := cFigureB := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "russian" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lRussian := ( Lower( cTemp ) == "on" )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION Write_Game_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   s += "clrboard=" + clrBoard + Chr(13)+Chr(10)
   s += "clrwhite=" + clrWhite + Chr(13)+Chr(10)
   s += "clrblack=" + clrBlack + Chr(13)+Chr(10)
   s += "clrbwhite=" + clrbWhite + Chr(13)+Chr(10)
   s += "clrbblack=" + clrbBlack + Chr(13)+Chr(10)
   s += "clrseleb=" + clrSeleB + Chr(13)+Chr(10)
   s += "clrselew=" + clrSeleW + Chr(13)+Chr(10)
   s += "figurewhite=" + aFigs1[3] + Chr(13)+Chr(10)
   s += "figureblack=" + aFigs1[2] + Chr(13)+Chr(10)
   s += "russian=" + Iif( lRussian, "On", "Off" ) + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "corners.ini", s )

   RETURN Nil
