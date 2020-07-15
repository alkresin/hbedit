/*
 * A game of Life by John Horton Conway
 * HbEdit plugin
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_CTRL_TAB  404
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4
#define K_HOME        1
#define K_CTRL_UP     397
#define K_CTRL_DOWN   401
#define K_CTRL_LEFT   26
#define K_CTRL_RIGHT  2

#define K_SPACE      32
#define K_F1         28
#define K_F2         -1
#define K_F9         -8

#define SC_NONE       0
#define SC_NORMAL     1

#define BORDER_CLR  "RB/N"
#define BOARD_CLR   "B/R"

#define POINT_CHR   "þ"

STATIC cIniPath
STATIC oLife
STATIC hIdle
STATIC x1t, x2t, y1t, y2t
STATIC px0, py0
STATIC lPaused, lStep
STATIC nTics := 0
STATIC cCellChar := "þ"
STATIC nSpeed := 0.5

STATIC aBoard, aBoard_tmp, nBoardHeight, nBoardWidth
STATIC cPal := "solarized dark", cBorderClr := "RB/N", cBoardClr := "B/R"
STATIC cScreenBuff

FUNCTION plug_gm_Life( oEdit, cPath )

   LOCAL i, j, cName := "$Life"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 ); DevOut( "Game of Life" )
         DevPos( y, o:x1 + 15 ); DevOut( "F9 - Menu" )
         DevPos( oLife:y1-1, oLife:x2-8 ); DevOut( "Paused  " )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      hb_IdleDel( hIdle )
      RETURN Nil
   }

   y1t := oEdit:y1+1; y2t := oEdit:y2-1
   x1t := oEdit:x1+1; x2t := oEdit:x2-1
   nBoardHeight := y2t - y1t + 1
   nBoardWidth := x2t -x1t + 1
   px0 := py0 := 0

   IF Empty( cIniPath )
      Read_Life_Ini( (cIniPath := cPath) + "life.ini" )
   ENDIF

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   aBoard := Array( nBoardHeight,nBoardWidth )
   aBoard_tmp := Array( nBoardHeight,nBoardWidth )
   FOR i := 1 TO nBoardHeight
      FOR j := 1 TO nBoardWidth
         aBoard[i,j] := 0
         aBoard_tmp[i,j] := 0
      NEXT
   NEXT

   oLife := mnu_NewBuf( oEdit )
   edi_SetPalette( oLife, cPal )
   oLife:cFileName := cName
   oLife:bWriteTopPane := bWPane
   oLife:bOnKey := {|o,n| _Life_OnKey(o,n) }
   oLife:bStartEdit := {|| _Life_Start() }
   oLife:bEndEdit := bEndEdit
   oLife:cp := "RU866"
   lPaused := .T.
   lStep := .F.
   IF nBoardHeight > y2t - y1t + 1
      py0 := Int( (nBoardHeight - (y2t - y1t + 1)) / 2 )
   ENDIF
   IF nBoardWidth > x2t -x1t + 1
      px0 := Int( (nBoardWidth - (x2t - x1t + 1)) / 2 )
   ENDIF

   RETURN Nil

FUNCTION _Life_Start()

   IF Empty( cScreenBuff )
      SetColor( cBorderClr )
      Scroll( oLife:y1, oLife:x1, oLife:y2, oLife:x2 )
      life_Redraw()
   ELSE
      RestScreen( oLife:y1, oLife:x1, oLife:y2, oLife:x2, cScreenBuff )
   ENDIF
   hIdle := hb_IdleAdd( {|| _Life_Tf() } )

   RETURN Nil

FUNCTION _Life_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), i, j

   SetCursor( SC_NONE )
   IF lPaused
      IF nKey == K_UP
         IF ( i := Row() ) > y1t
            DevPos( i - 1, Col() )
         ELSE
            DevPos( y2t, Col() )
         ENDIF

      ELSEIF nKey == K_DOWN
         IF ( i := Row() ) < y2t
            DevPos( i + 1, Col() )
         ELSE
            DevPos( y1t, Col() )
         ENDIF

      ELSEIF nKey == K_LEFT
         IF ( i := Col() ) > x1t
            DevPos( Row(), i - 1 )
         ELSE
            DevPos( Row(), x2t )
         ENDIF

      ELSEIF nKey == K_RIGHT
         IF ( i := Col() ) < x2t
            DevPos( Row(), i + 1 )
         ELSE
            DevPos( Row(), x1t )
         ENDIF

      ELSEIF nKey == K_HOME
         life_GoHome()

      ELSEIF nKey == K_SPACE
         life_SetCell( Row() - y1t + 1 + py0, Col() - x1t + 1 + px0 )
         IF Col() > x2t
            DevPos( Row(), x2t )
         ENDIF

      ELSEIF nKey == K_F2
         life_SetPatt()

      ELSEIF nKey == K_F1
         life_Help()

      ELSEIF nKey == 99  // c - Clear board
         life_Clear()

      ENDIF
   ENDIF

   IF nKey == 112        // p
      life_Pause()

   ELSEIF nKey == 115    // s
      lStep := .T.
      IF !lPaused
         life_Pause()
      ENDIF

      ELSEIF nKey == K_CTRL_UP
         IF py0 > 0
            py0 -= 5
            IF py0 < 0
               py0 := 0
            ENDIF
            life_Redraw()
         ENDIF

      ELSEIF nKey == K_CTRL_DOWN
         IF py0 + y2t-y1t+1 <= nBoardHeight - 5
            py0 += 5
            life_Redraw()
         ENDIF

      ELSEIF nKey == K_CTRL_LEFT
         IF px0 > 0
            px0 -= 5
            IF px0 < 0
               px0 := 0
            ENDIF
            life_Redraw()
         ENDIF

      ELSEIF nKey == K_CTRL_RIGHT
         IF px0 + x2t-x1t+1 <= nBoardWidth - 5
            px0 += 5
            life_Redraw()
         ENDIF

   ELSEIF nKey == K_CTRL_TAB
      cScreenBuff := SaveScreen( oLife:y1, oLife:x1, oLife:y2, oLife:x2 )
      RETURN 0

   ELSEIF nKey == K_ESC
      cScreenBuff := Nil
      //Write_Life_Ini()
      mnu_Exit( oEdit )

   ELSEIF nKey == K_F9
      life_Menu()

   ENDIF

   RETURN -1

STATIC FUNCTION life_Menu()

   LOCAL aMenu1 := { {"Play",,,"p"}, {"Step",,,"s"}, {"Clear board",,,"c"}, ;
      {"Set cell",,,"Space"}, {"Set pattern",,,"F2"}, {"Help",,,"F1"} }
   LOCAL aMenu2 := { {"Pause",,,"p"}, {"Step",,,"s"}, {"Speed",,,""} }
   LOCAL aMenuD := { "0.1", "0.3", "0.5", "1.0" }
   LOCAL i

   IF lPaused
      i := FMenu( oLife, aMenu1, 2, 6 )
      IF i == 1
         life_Pause()

      ELSEIF i == 2
         lStep := .T.

      ELSEIF i == 3
         life_Clear()

      ELSEIF i == 4
         life_SetCell( Row() - y1t + 1 + py0, Col() - x1t + 1 + px0 )

      ELSEIF i == 5
         life_SetPatt()

      ELSEIF i == 6
         life_Help()

      ENDIF
   ELSE
      life_Pause()
      i := FMenu( oLife, aMenu2, 2, 6 )
      IF i == 1

      ELSEIF i == 2
         lStep := .T.

      ELSEIF i == 3
         IF ( i := FMenu( oLife, aMenuD, 5, 20 ) ) != 0
            nSpeed := Val( aMenuD[i] )
            life_Pause()
         ENDIF

      ELSE
         life_Pause()
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION life_Pause()

   lPaused := !lPaused
   SetCursor( Iif( lPaused, SC_NORMAL, SC_NONE ) )
   SetColor( oLife:cColorPane )
   @ oLife:y1-1, oLife:x2-8 SAY Iif( lPaused, "Paused  ", "        " )
   SetColor( cBoardClr )
   DevPos( y1t, x1t )

   RETURN Nil

STATIC FUNCTION life_Clear()

   LOCAL i, j

   FOR i := 1 TO nBoardHeight
      FOR j := 1 TO nBoardWidth
         aBoard[i,j] := 0
         aBoard_Tmp[i,j] := 0
      NEXT
   NEXT
   SetColor( cBoardClr )
   Scroll( y1t, x1t, y2t, x2t )
   nTics := 0

   RETURN Nil

STATIC FUNCTION life_SetCell( j, i, n )

   LOCAL y := y1t + j - 1 - py0, x := x1t + i - 1 - px0

   aBoard[j,i] := Iif( n != Nil, n, Iif( aBoard[j,i]==0, 1, 0 ) )
   aBoard_Tmp[j,i] := Iif( n != Nil, n, Iif( aBoard_Tmp[j,i]==0, 1, 0 ) )
   IF y >= y1t .AND. y <= y2t .AND. x >= x1t .AND. x <= x2t
      SetColor( cBoardClr )
      DevPos( y, x )
      DevOut( Iif( aBoard[j,i]==0, ' ', cCellChar ) )
   ENDIF

   RETURN Nil

STATIC FUNCTION life_SetPatt()

   LOCAL aMenu := { "Glider", "Light ship", "r-pentamino", "Eight" }
   LOCAL aPatt := { "1x,2x,xxx", "3x,4x,x3x,1xxxx", "1xx,xx,1x", "xxx,xxx,xxx,3xxx,3xxx,3xxx" }
   LOCAL i

   i := FMenu( oLife, aMenu, 2, 6 )
   IF i > 0
      life_DrawPatt( aPatt[i] )
   ENDIF

   RETURN Nil

STATIC FUNCTION life_DrawPatt( cPatt )

   LOCAL i, i1, j1, j2, j3, n, y0 := Row(), x0 := Col()
   LOCAL aPatt := hb_aTokens( cPatt, ',' )

   SetCursor( SC_NONE )
   FOR i := 1 TO Len( aPatt )
      j1 := j2 := j3 := 1
      DO WHILE ( j2 := hb_At( 'x', aPatt[i], j1 ) ) > 0
         IF (j2 - j1) > 0
            n := Val( Substr( aPatt[i], j1, j2-j1 ) )
            FOR i1 := 1 TO n
               life_SetCell( y0+i-y1t+py0, x0+j1+i1-1-x1t+px0, 0 )
               j3 ++
            NEXT
         ENDIF
         life_SetCell( y0+i-y1t+py0, x0+j3-x1t+px0, 1 )
         j3 ++
         j1 := j2 + 1
      ENDDO
   NEXT
   IF Col() > x2t
      DevPos( Row(), x2t )
   ENDIF
   IF Row() > y2t
      DevPos( y2t, Col() )
   ENDIF
   SetCursor( SC_NORMAL )

   RETURN Nil

STATIC FUNCTION life_GoHome()

   DevPos( y1t+Int((y2t-y1t)/2), x1t+Int((x2t-x1t)/2) )
   RETURN Nil

STATIC FUNCTION life_Help()

   LOCAL cBuff := SaveScreen( oLife:y1, oLife:x1, oLife:y2, oLife:x2 )
   LOCAL oldc := SetColor( cBorderClr )

   hb_cdpSelect( "RU866" )
   @ y1t+2, x1t+2, y2t-2, x2t-2 BOX "ÚÄ¿³ÙÄÀ³ "
   hb_cdpSelect( oLife:cp )

   @ y1t+4, x1t + 4 SAY "Game of Life by John Horton Conway"
   @ y1t+6, x1t + 4 SAY ""

   Inkey( 0 )
   SetColor( oldc )
   RestScreen( oLife:y1, oLife:x1, oLife:y2, oLife:x2, cBuff )

   RETURN Nil

STATIC FUNCTION life_Redraw()

   LOCAL x, y, i, j

   SetColor( cBorderClr )
   @ y1t-1, x1t SAY PAdr( Ltrim(Str(py0))+","+Ltrim(Str(px0)), 8 )
   @ y2t+1, x1t SAY PAdr( Ltrim(Str(py0+y2t-y1t+1))+","+Ltrim(Str(px0)), 8 )
   @ y1t-1, x2t-8 SAY PAdl( Ltrim(Str(py0))+","+Ltrim(Str(px0+x2t-x1t+1)), 8 )
   @ y2t+1, x2t-8 SAY PAdl( Ltrim(Str(py0+y2t-y1t+1))+","+Ltrim(Str(px0+x2t-x1t+1)), 8 )
   SetColor( cBoardClr )
   FOR x := x1t TO x2t
      FOR y := y1t TO y2t
         j := y + py0 - y1t + 1; i := x + px0 -x1t + 1
         DevPos( y, x )
         IF j <= nBoardHeight .AND. i <= nBoardWidth
            DevOut( Iif( aBoard[j,i]==0, ' ', cCellChar ) )
         ELSE
            DevOut( ' ' )
         ENDIF
      NEXT
   NEXT
   life_GoHome()

   RETURN Nil

FUNCTION _Life_Tf()

   LOCAL nSec := Seconds(), lLast := .F., i, j, n, i1, i2, j1, j2, i0, j0, y, x
   LOCAL lt, lb, ll, lr
   STATIC nSecPrev := 0

   IF nSec - nSecPrev > nSpeed .OR. lStep
      nSecPrev := nSec
      IF lPaused .AND. !lStep
         RETURN Nil
      ELSE
         SetCursor( SC_NONE )
         IF !lPaused
            SetColor( oLife:cColorPane )
            @ oLife:y1-1, oLife:x2-8 SAY Str( nTics, 8 )
         ENDIF
         i0 := x1t - 1
         j0 := y1t - 1
         FOR i := 1 TO nBoardWidth
            FOR j := 1 TO nBoardHeight
               i1 := Iif( i==1, nBoardWidth, i-1 )
               i2 := Iif( i==nBoardWidth, 1, i+1 )
               j1 := Iif( j==1, nBoardHeight, j-1 )
               j2 := Iif( j==nBoardHeight, 1, j+1 )
               n := aBoard[j1,i1] + aBoard[j1,i] + aBoard[j1,i2] + aBoard[j,i1] + aBoard[j,i2] + aBoard[j2,i1] + aBoard[j2,i] + aBoard[j2,i2]
               IF n == 2 .OR. n == 3
                  IF n == 3
                     aBoard_Tmp[j,i] := 1
                  ENDIF
               ELSE
                  aBoard_Tmp[j,i] := 0
               ENDIF
            NEXT
         NEXT
         SetColor( cBorderClr )
         DevPos( y1t + Int((y2t-y1t)/2), x1t-1 ); DevOut( ' ' )        // lt
         DevPos( y1t + Int((y2t-y1t)/2), x2t+1 ); DevOut( ' ' )        // lb
         DevPos( y1t - 1, x1t + Int((x2t-x1t)/2) ); DevOut( ' ' )      // lr
         DevPos( y2t + 1, x1t + Int((x2t-x1t)/2) ); DevOut( ' ' )      // ll
         SetColor( cBoardClr )
         lt := lb := ll := lr := .F.
         FOR i := 1 TO nBoardWidth
            FOR j := 1 TO nBoardHeight
               IF aBoard[j,i] != aBoard_Tmp[j,i]
                  y := j0 + j - py0; x := i0 + i -px0
                  IF y >= y1t
                     IF y <= y2t
                        IF x >= x1t
                           IF x <= x2t
                              DevPos( y, x )
                              DevOut( Iif( aBoard_Tmp[j,i]==0, ' ', cCellChar ) )
                           ELSEIF aBoard_Tmp[j,i] == 1 .AND. !lr
                              SetColor( cBorderClr )
                              DevPos( y1t + Int((y2t-y1t)/2), x2t+1 ); DevOut( '' )
                              SetColor( cBoardClr )
                              lr := .T.
                           ENDIF
                        ELSEIF aBoard_Tmp[j,i] == 1 .AND. !ll
                           SetColor( cBorderClr )
                           DevPos( y1t + Int((y2t-y1t)/2), x1t-1 ); DevOut( '' )
                           SetColor( cBoardClr )
                           ll := .T.
                        ENDIF
                     ELSEIF aBoard_Tmp[j,i] == 1 .AND. !lb
                        SetColor( cBorderClr )
                        DevPos( y2t + 1, x1t + Int((x2t-x1t)/2) ); DevOut( '' )
                        SetColor( cBoardClr )
                        lb := .T.
                     ENDIF
                  ELSEIF aBoard_Tmp[j,i] == 1 .AND. !lt
                     SetColor( cBorderClr )
                     DevPos( y1t - 1, x1t + Int((x2t-x1t)/2) ); DevOut( '' )
                     SetColor( cBoardClr )
                     lt := .T.
                  ENDIF
                  aBoard[j,i] := aBoard_Tmp[j,i]
               ENDIF
            NEXT
         NEXT
         lStep := .F.
         nTics ++
         life_GoHome()
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION Read_Life_Ini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "GAME"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "cellchar" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cCellChar := Chr(Val( cTemp ))
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "palette" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cPal := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "borderclr" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cBorderClr := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "boardclr" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cBoardClr := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "boardheight" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nBoardHeight := Val( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "boardwidth" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nBoardWidth := Val( cTemp )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION Write_Life_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "life.ini", s )

   RETURN Nil
