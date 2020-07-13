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
#define K_SPACE      32
#define K_F2         -1
#define K_F9         -8

#define SC_NONE       0
#define SC_NORMAL     1

#define BOARD_CLR   "GR+/N"

#define POINT_CHR   "þ"

STATIC cIniPath
STATIC oLife
STATIC hIdle
STATIC x1t, x2t, y1t, y2t
STATIC lPaused, lStep
STATIC nTics := 0
STATIC cCellChar := "þ"

STATIC aBoard, aBoard_tmp, nBoardHeight, nBoardWidth
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

   IF Empty( cIniPath )
      Read_Life_Ini( (cIniPath := cPath) + "life.ini" )
   ENDIF

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
      mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   nBoardHeight := oEdit:y2 - oEdit:y1 + 1
   nBoardWidth := oEdit:x2 - oEdit:x1 + 1
   aBoard := Array( nBoardHeight,nBoardWidth )
   aBoard_tmp := Array( nBoardHeight,nBoardWidth )
   FOR i := 1 TO nBoardHeight
      FOR j := 1 TO nBoardWidth
         aBoard[i,j] := 0
         aBoard_tmp[i,j] := 0
      NEXT
   NEXT

   oLife := mnu_NewBuf( oEdit )
   edi_SetPalette( oLife, "default" )
   oLife:cFileName := cName
   oLife:bWriteTopPane := bWPane
   oLife:bOnKey := {|o,n| _Life_OnKey(o,n) }
   oLife:bStartEdit := {|| _Life_Start() }
   oLife:bEndEdit := bEndEdit
   oLife:cp := "RU866"
   lPaused := .T.
   lStep := .F.

   RETURN Nil

FUNCTION _Life_Start()

   IF Empty( cScreenBuff )
      y1t := oLife:y1; y2t := y1t + nBoardHeight-1
      x1t := oLife:x1; x2t := x1t + nBoardWidth-1

      SetColor( BOARD_CLR )
      Scroll( oLife:y1, oLife:x1, oLife:y2, oLife:x2 )
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
         IF ( i := Row() ) > oLife:y1
            DevPos( i - 1, Col() )
         ENDIF

      ELSEIF nKey == K_DOWN
         IF ( i := Row() ) < oLife:y2
            DevPos( i + 1, Col() )
         ENDIF

      ELSEIF nKey == K_LEFT
         IF ( i := Col() ) > oLife:x1
            DevPos( Row(), i - 1 )
         ENDIF

      ELSEIF nKey == K_RIGHT
         IF ( i := Col() ) < oLife:x2
            DevPos( Row(), i + 1 )
         ENDIF

      ELSEIF nKey == K_SPACE
         life_SetCell( Row() - oLife:y1 + 1, Col() - oLife:x1 + 1 )

      ELSEIF nKey == K_F2
         life_SetPatt()

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
   LOCAL aMenu2 := { {"Pause",,,"p"}, {"Step",,,"s"} }
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
         life_SetCell( Row() - oLife:y1 + 1, Col() - oLife:x1 + 1 )

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
   SetColor( BOARD_CLR )
   DevPos( oLife:y1, oLife:x1 )

   RETURN Nil

STATIC FUNCTION life_Clear()

   LOCAL i, j

   FOR i := 1 TO nBoardHeight
      FOR j := 1 TO nBoardWidth
         aBoard[i,j] := 0
         aBoard_Tmp[i,j] := 0
      NEXT
   NEXT
   SetColor( BOARD_CLR )
   Scroll( y1t, x1t, y2t, x2t )
   nTics := 0

   RETURN Nil

STATIC FUNCTION life_SetCell( j, i, n )

   aBoard[j,i] := Iif( n != Nil, n, Iif( aBoard[j,i]==0, 1, 0 ) )
   aBoard_Tmp[j,i] := Iif( n != Nil, n, Iif( aBoard_Tmp[j,i]==0, 1, 0 ) )
   SetColor( BOARD_CLR )
   DevPos( oLife:y1 + j - 1, oLife:x1 + i - 1 )
   DevOut( Iif( aBoard[j,i]==0, ' ', cCellChar ) )

   RETURN Nil

STATIC FUNCTION life_SetPatt()

   LOCAL aMenu := { "Glider", "Light ship", "Eight" }
   LOCAL aPatt := { "1x,2x,xxx", "3x,4x,x3x,1xxxx", "xxx,xxx,xxx,3xxx,3xxx,3xxx" }
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
               life_SetCell( y0+i-oLife:y1, x0+j1+i1-1-oLife:x1, 0 )
               j3 ++
            NEXT
         ENDIF
         life_SetCell( y0+i-oLife:y1, x0+j3-oLife:x1, 1 )
         j3 ++
         j1 := j2 + 1
      ENDDO
   NEXT
   SetCursor( SC_NORMAL )

   RETURN Nil

STATIC FUNCTION life_Help()

   RETURN Nil

FUNCTION _Life_Tf()

   LOCAL nSec := Seconds(), lLast := .F., i, j, n, i1, i2, j1, j2, i0, j0
   STATIC nSecPrev := 0

   IF nSec - nSecPrev > 0.5 .OR. lStep
      nSecPrev := nSec
      IF lPaused .AND. !lStep
         RETURN Nil
      ELSE
         SetCursor( SC_NONE )
         IF !lPaused
            SetColor( oLife:cColorPane )
            @ oLife:y1-1, oLife:x2-8 SAY Str( nTics, 8 )
         ENDIF
         i0 := oLife:x1 - 1
         j0 := oLife:y1 - 1
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
         SetColor( BOARD_CLR )
         FOR i := 1 TO nBoardWidth
            FOR j := 1 TO nBoardHeight
               IF aBoard[j,i] != aBoard_Tmp[j,i]
                  DevPos( j0+j, i0+i )
                  DevOut( Iif( aBoard_Tmp[j,i]==0, ' ', cCellChar ) )
                  aBoard[j,i] := aBoard_Tmp[j,i]
               ENDIF
            NEXT
         NEXT
         lStep := .F.
         nTics ++
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
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION Write_Life_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "life.ini", s )

   RETURN Nil
