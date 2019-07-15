#define K_ESC        27
#define K_CTRL_TAB  404
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4

#define SC_NONE       0

#define BOARD_HEIGHT  20
#define BOARD_WIDTH   12
#define BOARD_CLR   "GR+/N"

STATIC cIniPath
STATIC oTetr
STATIC hIdle
STATIC x1t, x2t, y1t, y2t
STATIC nGameState, lPaused, nLevel, nCurrFig, nyPos, nxPos, nFigAngle
STATIC nScores
STATIC nStartLevel := 0, nMaxScores := 0

STATIC aFigs := { { { {0,1}, {1,0}, {1,1} }, { {0,1}, {1,0}, {1,1} }, ;
     { {0,1}, {1,0}, {1,1} }, { {0,1}, {1,0}, {1,1} } }, ;
   { { {1,0}, {2,0}, {3,0} }, { {0,-1}, {0,1}, {0,2} }, ;
     { {1,0}, {2,0}, {3,0} }, { {0,-1}, {0,1}, {0,2} } }, ;
   { { {1,0}, {1,-1}, {2,0} }, { {0,-1}, {0,1}, {1,0} }, ;
     { {1,0}, {1,1}, {2,0} }, { {1,-1}, {1,0}, {1,1} } }, ;
   { { {1,0}, {2,0}, {2,-1} }, { {0,-1}, {0,1}, {1,1} }, ;
     { {0,1}, {1,0}, {2,0} }, { {0,-1}, {-1,-1}, {0,1} } }, ;
   { { {1,0}, {2,0}, {2,1} }, { {0,-1}, {0,1}, {-1,1} }, ;
     { {0,-1}, {1,0}, {2,0} }, { {0,-1}, {1,-1}, {0,1} } }, ;
   { { {0,1}, {1,-1}, {1,0} }, { {1,0}, {1,1}, {2,1} }, ;
     { {0,1}, {1,-1}, {1,0} }, { {1,0}, {1,1}, {2,1} } }, ;
   { { {0,-1}, {1,1}, {1,0} }, { {1,0}, {1,-1}, {2,-1} }, ;
     { {0,-1}, {1,1}, {1,0} }, { {1,0}, {1,-1}, {2,-1} } } ;
   }

STATIC aFigColors := { "N/GR","N/BG","N/RB","N/W","N/B","N/G","N/R" }
STATIC aLevelLimits := { 150, 350, 600, 900, 1300, 1900, 2700, 4000, 5500, 7500 }
STATIC aBoard[BOARD_HEIGHT,BOARD_WIDTH]
STATIC cScreenBuff

FUNCTION plug_Tetris( oEdit, cPath )

   LOCAL i, cName := "$Tetris"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      SetColor( o:cColorPane )
      Scroll( y, o:x1, y, o:x2 )
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Tetris" )
      ENDIF
      SetColor( o:cColor )
      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      hb_IdleDel( hIdle )
      RETURN Nil
   }

   IF Empty( cIniPath )
      Read_Tetr_Ini( (cIniPath := cPath) + "tetris.ini" )
   ENDIF

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   oTetr := mnu_NewBuf( oEdit )
   edi_SetPalette( oTetr, "default" )
   oTetr:cFileName := cName
   oTetr:bWriteTopPane := bWPane
   oTetr:bOnKey := {|o,n| _Tetris_OnKey(o,n) }
   oTetr:bStartEdit := {|| _Tetris_Start() }
   oTetr:bEndEdit := bEndEdit
   oTetr:cp := "RU866"
   nGameState := 0
   lPaused := .F.
   nLevel := nStartLevel
   nScores := 0

   RETURN Nil

FUNCTION _Tetris_Start()

   IF Empty( cScreenBuff )
      y1t := oTetr:y1 + Int((oTetr:y2-oTetr:y1-BOARD_HEIGHT)/2); y2t := y1t + BOARD_HEIGHT-1
      x1t := oTetr:x1 + 10; x2t := x1t + BOARD_WIDTH*2-1

      SetColor( BOARD_CLR )
      Scroll( oTetr:y1, oTetr:x1, oTetr:y2, oTetr:x2 )
      @ y1t, x1t-1, y2t+1, x2t+1 BOX "   ³ÙÄÀ³ "
      @ y1t+1, x2t+8 SAY "n   New game"
      @ y1t+2, x2t+8 SAY "p   Pause/Continue"
      @ y1t+3, x2t+8 SAY "+/- Start level (" + Ltrim(Str(nStartLevel)) + ")"

      @ y1t+5, x2t+8 SAY "h,Left / l,Right  - Movement"
      @ y1t+6, x2t+8 SAY "k,Up - Rotate;  Space - Down"

      @ y1t+10, x2t+8 SAY "Max scores: " + Ltrim(Str(nMaxScores))
      @ y1t+12, x2t+8 SAY "Level:  " + Ltrim(Str(nStartLevel))
      @ y1t+13, x2t+8 SAY "Scores: 0    "
   ELSE
      RestScreen( oTetr:y1, oTetr:x1, oTetr:y2, oTetr:x2, cScreenBuff )
   ENDIF
   hIdle := hb_IdleAdd( {|| _Tetr_Tf() } )

   RETURN Nil

FUNCTION _Tetris_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), i, j

   SetCursor( SC_NONE )
   IF nGameState == 1 .AND. !lPaused
      IF nKey == K_LEFT .OR. nKey == 104       // h
         DrawFig( .F. )
         IF IsEmptyPos( nFigAngle, nyPos, nxPos-1 )
            nxPos --
         ENDIF
         DrawFig( .T. )

      ELSEIF nKey == K_RIGHT .OR. nKey == 108  // l
         DrawFig( .F. )
         IF IsEmptyPos( nFigAngle, nyPos, nxPos+1 )
            nxPos ++
         ENDIF
         DrawFig( .T. )

      ELSEIF nKey == K_UP .OR. nKey == 107     // k
         DrawFig( .F. )
         IF IsEmptyPos( Iif( nFigAngle == 4, 1, nFigAngle + 1 ), nyPos, nxPos )
            nFigAngle := Iif( nFigAngle == 4, 1, nFigAngle + 1 )
         ENDIF
         DrawFig( .T. )

      ELSEIF nKey == K_DOWN .OR. nKey == 106   // j
         DrawFig( .F. )
         IF IsEmptyPos( nFigAngle, nyPos+1, nxPos )
            nyPos ++
            DrawFig( .T. )
         ELSE
            DrawFig( .T. )
            LastRow()
         ENDIF

      ELSEIF nKey == 32   // Space
         DrawFig( .F. )
         DO WHILE IsEmptyPos( nFigAngle, nyPos+1, nxPos )
            nyPos ++
         ENDDO
         DrawFig( .T. )
         LastRow()
      ENDIF
   ENDIF

   IF nKey == 110   // n
      SetColor( BOARD_CLR )
      IF nScores > nMaxScores
         nMaxScores := nScores
         @ y1t+10, x2t+20 SAY Ltrim(Str(nMaxScores))
      ENDIF
      nGameState := 1
      lPaused := .F.
      nCurrFig := 0
      nScores := 0
      nLevel := nStartLevel
      FOR i := 1 TO BOARD_HEIGHT
         FOR j := 1 TO BOARD_WIDTH
            aBoard[i,j] := 0
         NEXT
      NEXT
      Scroll( y1t, x1t, y2t, x2t )
      @ y1t+7, x2t+8 SAY Space(12)
      @ y1t+12, x2t+16 SAY Ltrim(Str( nLevel ))
      @ y1t+13, x2t+16 SAY Ltrim(Str( nScores ))

   ELSEIF nKey == 112   // p
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

   ELSEIF nKey == K_CTRL_TAB
      cScreenBuff := SaveScreen( oTetr:y1, oTetr:x1, oTetr:y2, oTetr:x2 )
      RETURN 0

   ELSEIF nKey == K_ESC
      cScreenBuff := Nil
      Write_Tetr_Ini()
      mnu_Exit( oEdit )

   ENDIF

   RETURN -1

FUNCTION _Tetr_Tf()

   LOCAL nSec := Seconds(), lLast := .F.
   STATIC nSecPrev := 0

   IF nSec - nSecPrev > 0.1 * (10 - nLevel)
      SetCursor( SC_NONE )
      nSecPrev := nSec
      IF nGameState == 0 .OR. lPaused
         RETURN Nil
      ENDIF
      IF nCurrFig == 0
         nCurrFig := hb_RandomInt( 1, 7 )
         nFigAngle := 1
         nyPos := 0
         nxPos := Int( BOARD_WIDTH/2 )
         IF !IsEmptyPos( nFigAngle, 1, nxPos )
            LastRow()
            nGameState := 0
            SetColor( BOARD_CLR )
            @ y1t+7, x2t+8 SAY "GAME OVER"
            RETURN Nil
         ENDIF
      ELSE
         DrawFig( .F. )
      ENDIF
      nyPos ++
      lLast := !IsEmptyPos( nFigAngle, nyPos+1, nxPos )
      DrawFig( .T. )
      IF lLast
         LastRow()
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION DrawFig( lDraw )

   LOCAL i, n := Iif( lDraw, nCurrFig, 0 ), y, x
   LOCAL s

   IF nCurrFig > 0 .AND. nCurrFig <= Len(aFigs) .AND. nyPos > 0 .AND. nxPos > 0 ;
      .AND. nyPos <= BOARD_HEIGHT .AND. nxPos <= BOARD_WIDTH
      SetColor( Iif( lDraw, aFigColors[nCurrFig], BOARD_CLR ) )
      aBoard[nyPos,nxPos] := n
      @ y1t-1+nyPos, x1t+(nxPos-1)*2 SAY "  "
      FOR i := 1 TO 3
         IF nFigAngle > 0 .AND. ;
            nFigAngle <= Len(aFigs[nCurrFig]) .AND. i <= Len(aFigs[nCurrFig,nFigAngle]) ;
            .AND. Len(aFigs[nCurrFig,nFigAngle,i]) == 2
            y := nyPos+aFigs[nCurrFig,nFigAngle,i,1]
            x := nxPos+aFigs[nCurrFig,nFigAngle,i,2]
            IF y > 0 .AND. y <= BOARD_HEIGHT .AND. x > 0 .AND. x <= BOARD_WIDTH
               aBoard[y,x] := n
               @ y1t-1+y, x1t+(x-1)*2 SAY "  "
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION IsEmptyPos( nAngle, ny, nx )

   LOCAL i, y, x

   IF nCurrFig <= 0 .OR. nCurrFig > Len(aFigs) .OR. nAngle <= 0 .OR. nAngle > Len(aFigs[nCurrFig])
      RETURN .F.
   ENDIF
   y := ny; x := nx
   IF y <= 0 .OR. y > BOARD_HEIGHT .OR. x <= 0 .OR. x > BOARD_WIDTH .OR. aBoard[y,x] != 0
      RETURN .F.
   ENDIF
   FOR i := 1 TO 3
      y := ny + aFigs[nCurrFig,nAngle,i,1]
      x := nx + aFigs[nCurrFig,nAngle,i,2]
      IF y <= 0 .OR. y > BOARD_HEIGHT .OR. x <= 0 .OR. x > BOARD_WIDTH .OR. aBoard[y,x] != 0
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.

STATIC FUNCTION LastRow()

   LOCAL i, j, lFilled, i1

   nCurrFig := 0
   nScores += nLevel + 1

   SetColor( BOARD_CLR )
   FOR i := BOARD_HEIGHT TO 1 STEP -1
      lFilled := .T.
      FOR j := 1 TO BOARD_WIDTH
         IF aBoard[i,j] == 0
            lFilled := .F.
            EXIT
         ENDIF
      NEXT
      IF lFilled
         FOR i1 := i TO 2 STEP -1
            FOR j := 1 TO BOARD_WIDTH
               aBoard[i1,j] := aBoard[i1-1,j]
            NEXT
         NEXT
         FOR j := 1 TO BOARD_WIDTH
            aBoard[1,j] := 0
         NEXT
         Scroll( y1t, x1t, y1t+i-1, x2t, -1 )
         nScores += 10 * (nLevel+1)
         i ++
      ENDIF
   NEXT
   IF nLevel < Len( aLevelLimits ) .AND. nScores > aLevelLimits[nLevel+1]
      nLevel ++
      @ y1t+12, x2t+16 SAY Ltrim(Str( nLevel ))
   ENDIF

   @ y1t+13, x2t+16 SAY Ltrim(Str( nScores ))

   RETURN Nil

STATIC FUNCTION Read_Tetr_Ini( cIni )

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

STATIC FUNCTION Write_Tetr_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   IF nScores > nMaxScores
      nMaxScores := nScores
   ENDIF

   s += "startlevel=" + Ltrim(Str( nStartLevel )) + Chr(13)+Chr(10)
   s += "maxscores=" + Ltrim(Str( nMaxScores )) + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "tetris.ini", s )

   RETURN Nil
