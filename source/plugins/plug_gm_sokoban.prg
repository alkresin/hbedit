/*
 * Sokoban game
 * HbEdit plugin
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
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
#define K_F10        -9

STATIC cIniPath
STATIC oGame
STATIC x1t, y1t, x2t, nyPos, nxPos
STATIC nStartLevel, nLevel, nGameState
STATIC cScreenBuff
STATIC aData, aBoard, aHis, nHis
STATIC clrBoard := "GR+/N", clrBorder := "GR+/B", clrBox := "N/GR", clrTarget := "W/N", clrMan := "W+/N"
STATIC ccBorder := "°°", ccBox := "::", ccTarget := "x ", ccMan := "NN"

FUNCTION plug_gm_Sokoban( oEdit, cPath )

   LOCAL i, cName := "$Sokoban"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      Scroll( y, o:x1, y, o:x2 )
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Sokoban" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }

   nStartLevel := 1
   IF Empty( cIniPath )
      Read_Game_Ini( (cIniPath := cPath) + "sokoban.ini" )
      Read_Game_Data( (cIniPath := cPath) + "sokoban.dat" )
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
   oGame:cp := "RU866"
   oGame:lIns := Nil
   nGameState := 1
   nLevel := nStartLevel
   aHis := Array( 100 )
   CreateBoard()

   RETURN Nil

FUNCTION _Game_Start()

   IF Empty( cScreenBuff )
      y1t := oGame:y1 + 3
      x1t := oGame:x1 + 2
      x2t := x1t + 30

      SetColor( clrBoard )
      Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
      @ y1t-1, x1t, y1t+13, x1t+26 BOX "   ³ÙÄÀ³ "
      @ y1t+1, x1t+2 SAY "The player "
      @ y1t+1, x1t+13 SAY ccMan COLOR clrBoard
      @ y1t+1, x1t+16 SAY "should"
      @ y1t+2, x1t+2 SAY "move all boxes "
      @ y1t+2, x1t+17 SAY ccBox COLOR clrBox
      @ y1t+3, x1t+2 SAY "to targets "
      @ y1t+3, x1t+13 SAY ccTarget COLOR clrTarget
      @ y1t+3, x1t+16 SAY "..."
      @ y1t+5, x1t+2 SAY "n - play again"
      @ y1t+6, x1t+2 SAY "+/- Level:  " + Ltrim(Str(nLevel)) + "/" + Iif( Valtype(aData)=="A",Ltrim(Str(Len(aData))),"" )
      @ y1t+7, x1t+2 SAY "F10, ESC - Exit"

      @ y1t+9, x1t+2 SAY "h,Left / l,Right /"
      @ y1t+10, x1t+2 SAY "k,Up / j,Down - Movement"
      @ y1t+11, x1t+2 SAY "Backspace - Turn back"

      DrawBoard()
   ELSE
      RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cScreenBuff )
   ENDIF

   RETURN Nil

FUNCTION _Game_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), i, j, lOnTarget, lNoEnd := .T.

   IF nGameState == 1 .AND. nyPos > 0
      IF nKey == K_BS
         IF nHis > 0
            FOR i := Len( aHis[nHis] ) TO 2 STEP -1
               aBoard[aHis[nHis,i,1],aHis[nHis,i,2]] := aHis[nHis,i,3]
            NEXT
            nyPos := aHis[nHis,1,1]
            nxPos := aHis[nHis,1,2]
            nHis --
            DrawBoard()
         ENDIF
         RETURN -1
      ENDIF
      lOnTarget := (aBoard[nyPos,nxPos]<0)
      IF nKey == K_LEFT .OR. nKey == 104       // h
         IF IsEmptyPos( nyPos, nxPos-1, nyPos, nxPos-2 )
            AAdd( aHis[nHis], {nyPos,nxPos,aBoard[nyPos,nxPos]} )
            aBoard[nyPos,nxPos] := Iif( lOnTarget, 6, Nil )
            nxPos --
            AAdd( aHis[nHis], {nyPos,nxPos,aBoard[nyPos,nxPos]} )
            aBoard[nyPos,nxPos] := Iif( aBoard[nyPos,nxPos]==6, -7, 7 )
            DrawBoard()
         ENDIF

      ELSEIF nKey == K_RIGHT .OR. nKey == 108  // l
         IF IsEmptyPos( nyPos, nxPos+1, nyPos, nxPos+2 )
            AAdd( aHis[nHis], {nyPos,nxPos,aBoard[nyPos,nxPos]} )
            aBoard[nyPos,nxPos] := Iif( lOnTarget, 6, Nil )
            nxPos ++
            AAdd( aHis[nHis], {nyPos,nxPos,aBoard[nyPos,nxPos]} )
            aBoard[nyPos,nxPos] := Iif( aBoard[nyPos,nxPos]==6, -7, 7 )
            DrawBoard()
         ENDIF

      ELSEIF nKey == K_UP .OR. nKey == 107     // k
         IF IsEmptyPos( nyPos-1, nxPos, nyPos-2, nxPos )
            AAdd( aHis[nHis], {nyPos,nxPos,aBoard[nyPos,nxPos]} )
            aBoard[nyPos,nxPos] := Iif( lOnTarget, 6, Nil )
            nyPos --
            AAdd( aHis[nHis], {nyPos,nxPos,aBoard[nyPos,nxPos]} )
            aBoard[nyPos,nxPos] := Iif( aBoard[nyPos,nxPos]==6, -7, 7 )
            DrawBoard()
         ENDIF

      ELSEIF nKey == K_DOWN .OR. nKey == 106   // j
         IF IsEmptyPos( nyPos+1, nxPos, nyPos+2, nxPos )
            AAdd( aHis[nHis], {nyPos,nxPos,aBoard[nyPos,nxPos]} )
            aBoard[nyPos,nxPos] := Iif( lOnTarget, 6, Nil )
            nyPos ++
            AAdd( aHis[nHis], {nyPos,nxPos,aBoard[nyPos,nxPos]} )
            aBoard[nyPos,nxPos] := Iif( aBoard[nyPos,nxPos]==6, -7, 7 )
            DrawBoard()
         ENDIF

      ENDIF
      lNoEnd := .F.
      FOR i := 1 TO Len( aBoard )
         FOR j := 1 TO Len( aBoard[i] )
            IF aBoard[i,j] == 5
              lNoEnd := .T.
              EXIT
            ENDIF
         NEXT
      NEXT
      IF !lNoEnd
         edi_Alert( "Game over!" )
         nGameState := 0
      ENDIF
   ENDIF

   IF nKey == 110   // n
      SetColor( clrBoard )
      nGameState := 1
      CreateBoard()
      DrawBoard()

   ELSEIF nKey == 43 .OR. nKey == 45   // +,-
      IF nKey == 43 .AND. nLevel < Len( aData )
         nLevel ++
         @ y1t+6, x1t+2 SAY "+/- Level:  " + Ltrim(Str(nLevel)) + "/" + Iif( Valtype(aData)=="A",Ltrim(Str(Len(aData))),"" ) + "  "
         Scroll( y1t, x2t, oGame:y2, oGame:x2 )
         nGameState := 1
         CreateBoard()
         DrawBoard()
      ELSEIF nKey == 45 .AND. nLevel > 1
         nLevel --
         @ y1t+6, x1t+2 SAY "+/- Level:  " + Ltrim(Str(nLevel)) + "/" + Iif( Valtype(aData)=="A",Ltrim(Str(Len(aData))),"" ) + "  "
         Scroll( y1t, x2t, oGame:y2, oGame:x2 )
         nGameState := 1
         CreateBoard()
         DrawBoard()
      ENDIF

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

STATIC FUNCTION CreateBoard()

   LOCAL i := 0, j, cGame, c, x1, y1, z1

   IF Empty( aData )
      RETURN Nil
   ENDIF
   IF nLevel > Len( aData )
      nLevel := 1
   ENDIF
   nHis := 0
   nyPos := -1
   nxPos := -1

   cGame := aData[nLevel]
   //edi_writelog( cGame )
   DO WHILE ++i < Len( cGame ) - 1
      c := Substr( cGame, i, 1 )
      y1 := Iif( ( y1 := Asc( Substr( cGame, ++i, 1 ) ) - Asc( '0' ) ) > 9, y1 - 7, y1 )
      x1 := Iif( ( x1 := Asc( Substr( cGame, ++i, 1 ) ) - Asc( '0' ) ) > 9, x1 - 7, x1 )

      IF c == '0'          // Dimension
         aBoard := Array( y1, x1 )
      ELSEIF Empty( aBoard ) .OR. y1 > Len( aBoard ) .OR. x1 > Len( aBoard[y1] )
         edi_Alert( "Data error " + Valtype(aBoard) + " " + Ltrim(Str(y1)) + " " + Ltrim(Str(x1)) )
         Return Nil
      ENDIF
      //edi_writelog( "soko_2 " + c + " " + Ltrim(Str(y1)) + " " + Ltrim(Str(x1)) )
      IF c == '-' .OR. c == ' '
         z1 := Iif( ( z1 := Asc( Substr( cGame, ++i, 1 ) ) - Asc( '0' ) ) > 9, z1 - 7, z1 )
         IF c == '-'
            FOR j := x1 TO z1
               aBoard[y1,j] := 1
            NEXT
         ELSE
            FOR j := y1 TO z1
               aBoard[j,x1] := 2
            NEXT
         ENDIF
      ELSEIF c == '.'      // Blocks
         aBoard[y1,x1] := 5
      ELSEIF c == '$'      // Target
         aBoard[y1,x1] := 6
      ELSEIF c == '@'      // Player
         aBoard[y1,x1] := 7
         nyPos := y1
         nxPos := x1
      ENDIF
   ENDDO

   RETURN Nil

STATIC FUNCTION DrawBoard()

   LOCAL x, y, c, clr

   IF Empty( aBoard )
      RETURN Nil
   ENDIF

   DispBegin()
   FOR y := 1 TO Len( aBoard )
      FOR x := 1 TO Len( aBoard[y] )
         c := "  "
         clr := clrBoard
         IF aBoard[y,x] == Nil
         ELSEIF aBoard[y,x] == 1 .OR. aBoard[y,x] == 2
            clr := clrBorder
            c := ccBorder
         ELSEIF Abs(aBoard[y,x]) == 5
            clr := clrBox
            c := ccBox
         ELSEIF aBoard[y,x] == 6
            clr := clrTarget
            c := ccTarget
         ELSEIF Abs(aBoard[y,x]) == 7
            clr := clrMan
            c := ccMan
         ENDIF
         SetColor( clr )
         @ y1t+y-1, x2t+(x-1)*2 SAY c
      NEXT
   NEXT
   SetColor( clrBoard )
   DispEnd()

   RETURN Nil

STATIC FUNCTION AddHis()

   nHis ++
   IF nHis > Len( aHis )
      aHis := ASize( aHis, Len(aHis)+50 )
   ENDIF
   aHis[nHis] := { {nyPos, nxPos} }

   RETURN Nil

STATIC FUNCTION IsEmptyPos( ny, nx, nyNext, nxNext )

   LOCAL c

   IF ny == 0 .OR. nx == 0 .OR. ny > Len(aBoard) .OR. nx > Len(aBoard[ny])
      RETURN .F.
   ENDIF
   IF Empty( c := aBoard[ny,nx] ) .OR. c == 6
      AddHis()
      RETURN .T.
   ELSEIF c == 1 .OR. c == 2
      RETURN .F.
   ELSEIF c == 5 .OR. c == -5
      IF nyNext == 0 .OR. nxNext == 0 .OR. nyNext > Len(aBoard) .OR. nxNext > Len(aBoard[nyNext])
         RETURN .F.
      ELSEIF Empty( aBoard[nyNext,nxNext] )
         AddHis()
         IF c == -5
            AAdd( aHis[nHis], {ny,nx,aBoard[ny,nx]} )
            aBoard[ny,nx] := 6
         ENDIF
         AAdd( aHis[nHis], {nyNext,nxNext,aBoard[nyNext,nxNext]} )
         aBoard[nyNext,nxNext] := 5
         RETURN .T.
      ELSEIF aBoard[nyNext,nxNext] == 6
         AddHis()
         IF c == -5
            AAdd( aHis[nHis], {ny,nx,aBoard[ny,nx]} )
            aBoard[ny,nx] := 6
         ENDIF
         AAdd( aHis[nHis], {nyNext,nxNext,aBoard[nyNext,nxNext]} )
         aBoard[nyNext,nxNext] := -5
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION Read_Game_Data( cFile )

   LOCAL cBuff := MemoRead( cFile ), arr, i

   IF Chr(13)+Chr(10) $ cBuff
      arr := hb_ATokens( cBuff, Chr(13)+Chr(10) )
   ELSE
      arr := hb_ATokens( cBuff, Chr(10) )
   ENDIF
   FOR i := Len( arr ) TO 1 STEP -1
      IF Len( arr[i] ) < 32 .AND. Left( arr[i], 1 ) != '0'
         ADel( arr, i )
         ASize( arr, Len(arr)-1 )
      ENDIF
   NEXT
   IF Len( arr ) == 0
      RETURN Nil
   ENDIF

   aData := arr

   RETURN Nil

STATIC FUNCTION Read_Game_Ini( cIni )

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
               IF hb_hHaskey( aSect, cTemp := "clrboard" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrBoard := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrborder" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrBorder := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrbox" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrBox := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrtarget" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrTarget := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrman" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrMan := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "ccborder" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  ccBorder := PAdr( cTemp, 2 )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "ccbox" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  ccBox := PAdr( cTemp, 2 )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "cctarget" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  ccTarget := PAdr( cTemp, 2 )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "ccman" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  ccMan := PAdr( cTemp, 2 )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION Write_Game_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   s += "startlevel=" + Ltrim(Str( nStartLevel )) + Chr(13)+Chr(10)
   s += "clrboard=" + clrBoard + Chr(13)+Chr(10)
   s += "clrborder=" + clrBorder + Chr(13)+Chr(10)
   s += "clrbox=" + clrBox + Chr(13)+Chr(10)
   s += "clrtarget=" + clrTarget + Chr(13)+Chr(10)
   s += "clrman=" + clrMan + Chr(13)+Chr(10)
   s += "ccborder=" + ccBorder + Chr(13)+Chr(10)
   s += "ccbox=" + ccBox + Chr(13)+Chr(10)
   s += "cctarget=" + ccTarget + Chr(13)+Chr(10)
   s += "ccman=" + ccMan + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "sokoban.ini", s )

   RETURN Nil
