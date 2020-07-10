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

#define SC_NONE       0
#define SC_NORMAL     1

#define BOARD_CLR   "GR+/N"

#define POINT_CHR   "Û"

STATIC cIniPath
STATIC oLife
STATIC hIdle
STATIC x1t, x2t, y1t, y2t
STATIC lPaused

STATIC aBoard, aBoard_tmp, nBoardHeight, nBoardWidth
STATIC cScreenBuff

FUNCTION plug_gm_Life( oEdit, cPath )

   LOCAL i, j, cName := "$Life"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Life" )
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
         i := Col() - oLife:x1 + 1
         j := Row() - oLife:y1 + 1
         aBoard[j,i] := Iif( aBoard[j,i]==0, 1, 0 )
         aBoard_Tmp[j,i] := Iif( aBoard_Tmp[j,i]==0, 1, 0 )
         DevOut( Iif( aBoard[j,i]==0, ' ', POINT_CHR ) )
         DevPos( oLife:y1 + j - 1, oLife:x1 + i - 1 )

      ELSEIF nKey == 110   // n - Clear board
         FOR i := 1 TO nBoardHeight
            FOR j := 1 TO nBoardWidth
               aBoard[i,j] := 0
               aBoard_Tmp[i,j] := 0
            NEXT
         NEXT
         Scroll( y1t, x1t, y2t, x2t )
      ENDIF
   ENDIF

   IF nKey == 112   // p
      lPaused := !lPaused
      SetCursor( Iif( lPaused, SC_NORMAL, SC_NONE ) )
      SetColor( BOARD_CLR )
      @ oLife:y1-1, oLife:x2-8 SAY Iif( lPaused, "Paused  ", "        " )
      DevPos( oLife:y1, oLife:x1 )

   ELSEIF nKey == K_CTRL_TAB
      cScreenBuff := SaveScreen( oLife:y1, oLife:x1, oLife:y2, oLife:x2 )
      RETURN 0

   ELSEIF nKey == K_ESC
      cScreenBuff := Nil
      //Write_Life_Ini()
      mnu_Exit( oEdit )

   ENDIF

   RETURN -1

FUNCTION _Life_Tf()

   LOCAL nSec := Seconds(), lLast := .F., i, j, n, i1, i2, j1, j2, i0, j0
   STATIC nSecPrev := 0

   IF nSec - nSecPrev > 0.5
      nSecPrev := nSec
      IF lPaused
         RETURN Nil
      ELSE
         SetCursor( SC_NONE )
         @ oLife:y1-1, oLife:x2-8 SAY Time()
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
         FOR i := 1 TO nBoardWidth
            FOR j := 1 TO nBoardHeight
               IF aBoard[j,i] != aBoard_Tmp[j,i]
                  DevPos( j0+j, i0+i )
                  DevOut( Iif( aBoard_Tmp[j,i]==0, ' ', POINT_CHR ) )
               ENDIF
               aBoard[j,i] := aBoard_Tmp[j,i]
            NEXT
         NEXT
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION Read_Life_Ini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "GAME"
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION Write_Life_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "life.ini", s )

   RETURN Nil
