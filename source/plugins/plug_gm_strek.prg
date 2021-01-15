/*
 * Star trek
 * HbEdit plugin
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_ENTER      13
#define K_BS          8
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_F1         28
#define K_F2         -1
#define K_F3         -2
#define K_F4         -3
#define K_F5         -4
#define K_F6         -5
#define K_F7         -6
#define K_F8         -7
#define K_F10        -9

STATIC cIniPath
STATIC oGame
STATIC x1t, y1t, x2t, y2t, yCmd
STATIC nLevel, nGameState
STATIC cScreenBuff
STATIC clrBoard := "W/N", clrAlert := "W+/R", clrGreen := "N/G", clrSRS := "G+/N", clrYellow := "GR+/RB"

STATIC nKliOnStart, nKlings, nBases, nStars
STATIC aBoard, nCurrQua, nCurrSect
STATIC nDateInit, nStardate, nTorpedInit, nTorped, nEnergyInit, nEnergy, nShield, nTime
STATIC aKli, aBase, aStars, nmaxk, nmaxs, lDocked
STATIC aDamages
STATIC cCommand, cMessage
STATIC lSRS

FUNCTION plug_gm_STrek( oEdit, cPath )

   LOCAL i, cName := "$StarTrek"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      Scroll( y, o:x1, y, o:x2 )
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "StarTrek" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }

   nLevel := 1
   IF Empty( cIniPath )
      Read_Game_Ini( (cIniPath := cPath) + "strek.ini" )
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
   nGameState := 1

   RETURN Nil

FUNCTION _Game_Start()

   lSRS := .T.
   IF Empty( cScreenBuff )
      y1t := oGame:y1 + 2
      x1t := oGame:x1 + 2
      x2t := x1t + 35
      y2t := y1t + 12
      yCmd := y1t + 10

      SetColor( clrBoard )
      Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )

      lDocked := .F.
      cMessage := ""
      CreateBoard()
      DrawInit()
      Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )

      DrawText()
      DrawSRS()
      DrawMessages()
      DrawCommand( "" )
   ELSE
      RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cScreenBuff )
      DrawCommand( cCommand )
   ENDIF

   RETURN Nil

FUNCTION _Game_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), i, j, lOnTarget, lNoEnd := .T.
   LOCAL nCmd
   STATIC aCmds := { "nav", "srs", "lrs", "pha", "tor", "she", "dam", "com", "doc", "res" }

   IF nGameState == 1
   ENDIF
   IF (nKey >= 48 .AND. nKey <= 57) .OR. (nKey >= 65 .AND. nKey <= 90) ;
      .OR. (nKey >= 97 .AND. nKey <= 122) .OR. nKey == 46

      IF Len( cCommand ) < 12
         DrawCommand( cCommand + Chr( nKey ) )
      ENDIF

   ELSEIF nKey == K_BS
      IF Len( cCommand ) > 0
         DrawCommand( Left( cCommand, Len(cCommand)-1 ) )
      ENDIF

   ELSEIF nKey == K_ENTER
      IF !Empty( cCommand ) .AND. ( nCmd := Ascan( aCmds, Lower( Left(cCommand,3) ) ) ) > 0
         DoCmd( nCmd )
      ELSE
         DrawCmds()
         DrawCommand( "" )
      ENDIF

   ELSEIF nKey == K_F1
      DoCmd( 1, aCmds[1] )

   ELSEIF nKey == K_F2
      DoCmd( 2, aCmds[2] )

   ELSEIF nKey == K_F3
      DoCmd( 3, aCmds[3] )

   ELSEIF nKey == K_F4
      DoCmd( 4, aCmds[4] )

   ELSEIF nKey == K_F5
      DoCmd( 5, aCmds[5] )

   ELSEIF nKey == K_F6
      DoCmd( 6, aCmds[6] )

   ELSEIF nKey == K_F7
      DoCmd( 7, aCmds[7] )

   ELSEIF nKey == K_F8
      DoCmd( 8, aCmds[8] )

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      cScreenBuff := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_F10
      EndOfTime()

   ENDIF

   RETURN -1

STATIC FUNCTION DoCmd( nCmd, cCmd )

   LOCAL aAns, nAns, oHelp, cScrBuf

   ClearDop()

   IF cCmd != Nil
      DrawCommand( cCmd )
   ENDIF

   IF nCmd == 1        // nav
      IF !Empty( aAns := AskDop( "Course (1-9)?","9.999","Warp factor (0-8)?","9.999" ) )
         DoNavi( aAns[1], aAns[2] )
         lSRS := .T.
         DrawSRS()
      ELSE
         ClearDop()
      ENDIF

   ELSEIF nCmd == 2    // srs
      lSRS := .T.
      DrawSRS()

   ELSEIF nCmd == 3    //lrs
      DrawLRS()

   ELSEIF nCmd == 4    // pha
      IF aDamages[4] < 0
         cMessage += "Phasers Inoperative" + Chr(10)
      ELSEIF aBoard[Int(nCurrQua / 8) + 1, Int(nCurrQua % 8) + 1, 1] == 0
         cMessage += "Science Officer Spock reports:" + Chr(10) + "Sensors show no enemy ships in this quadrant" + Chr(10)
      ELSE
         IF !Empty( nAns := AskDop( "Number of units to fire:","9999" ) )
            IF nEnergy - nShield < nAns
               cMessage += "Not enough energy available." + Chr(10)
            ELSE
               DoPhazer( nAns )
            ENDIF
         ELSE
            ClearDop()
         ENDIF
      ENDIF

   ELSEIF nCmd == 5    // tor
      IF aDamages[5] < 0
         cMessage += "Photon Tubes not operational" + Chr(10)
      ELSEIF nTorped == 0
         cMessage += "All photon torpedoes expended" + Chr(10)
      ELSEIF !Empty( nAns := AskDop( "Photon torpedo course?","9.999" ) )
         DoTor( nAns )
      ELSE
         ClearDop()
      ENDIF

   ELSEIF nCmd == 6    // shi
      IF aDamages[6] < 0
         cMessage += "Sheild Control inoperable" + Chr(10)
      ELSEIF ( nAns := AskDop( "Number of units to shields?","9999" ) ) == Nil
         ClearDop()
      ELSE
         DoShi( nAns )
      ENDIF

   ELSEIF nCmd == 7    // dam
      DoDam()

   ELSEIF nCmd == 8    // com
      DO WHILE .T.
         IF aDamages[8] < 0
            cMessage += "Library Computer inoperable" + Chr(10)
            EXIT
         ELSEIF ( nAns := AskDop( "Computer awaiting your command","9" ) ) == Nil
            ClearDop()
            EXIT
         ELSEIF nAns == 0 .OR. nAns > 6
            DrawCompCmds()
         ELSE
            DoCompute( nAns )
            EXIT
         ENDIF
      ENDDO

   ELSEIF nCmd == 9    // doc
      IF File( cIniPath + "strek.doc" )
         cScrBuf := SaveScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
         oHelp := TEdit():New( Memoread(cIniPath + "strek.doc"), "$Instructions", ;
            TEdit():aRectFull[1], TEdit():aRectFull[2], TEdit():aRectFull[3], TEdit():aRectFull[4] )

         oHelp:lReadOnly := .T.
         oHelp:lCtrlTab  := .F.
         oHelp:Edit()
         IF !( oGame:cPalette == TEdit():cCurrPal )
            edi_SetPalette( oGame, oGame:cPalette )
         ENDIF
         SetColor( clrBoard )
         RestScreen( oGame:y1, oGame:x1, oGame:y2, oGame:x2, cScrBuf )
      ELSE
         cMessage += "strek.doc isn't found..." + Chr(10)
      ENDIF

   ELSEIF nCmd == 10    // res
      EndOfTime()

   ENDIF

   IF nCmd != 3
      DrawMessages()
   ENDIF
   DrawText()
   DrawCommand( "" )

   RETURN Nil

STATIC FUNCTION DoNavi( nAngle, nDistance )

   LOCAL n := 0, nBase := Int(nAngle), nKoef := nAngle - nBase, ndx, ndy
   LOCAL ny1 := Int(nCurrSect/8)+1, nx1 := Int(nCurrSect%8)+1, ny, nx, nCurr
   LOCAL nyq1 := Int(nCurrQua / 8) + 1, nxq1 := Int(nCurrQua % 8) + 1, nyq := nyq1, nxq := nxq1
   LOCAL aQua := aBoard[nyq1, nxq1]

   /* ndx := nKoef * ndy
    * (nKoef * ndy)^2 + ndy^2 == nDistance^2
    * (nKoef*nKoef+1) * ndy * ndy == nDistance * nDistance
    * ndy := Sqrt( nDistance * nDistance / (nKoef*nKoef+1)
    */

   IF nAngle < 1 .OR. nAngle > 9
      cMessage += "Lt. Sulu roports:" + Chr(10)
      cMessage += "  Incorrect course data, sir!" + Chr(10)
      RETURN Nil
   ELSEIF nAngle == 9
      nAngle := 1
   ENDIF
   IF aDamages[1] < 0 .AND. nDistance > 0.2
      cMessage += "Warp Engines are damaged." + Chr(10)
      cMessage += "Maximum speed = Warp 0.2." + Chr(10)
      RETURN Nil
   ENDIF
   IF nEnergy - nShield < ( 10 + n )
      cMessage += "Engineering reports:" + Chr(10)
      cMessage += "Insufficient energy available for maneuvering" + Chr(10)
      RETURN Nil
   ENDIF

   DO WHILE n < nDistance
      IF nDistance - n > 0.125
         n += 0.125
      ELSE
         n := nDistance
      ENDIF
      DirDist( n, nBase, nKoef, @ndy, @ndx )
      ndy *= 8
      ndx *= 8
      ndy := Int( ndy ) + Iif( ndy-Int(ndy)>0.1, 1, 0 )
      ndx := Int( ndx ) + Iif( ndx-Int(ndx)>0.1, 1, 0 )
      ny := Iif( nBase >= 5, ny1 + ndy, ny1 - ndy )
      nx := Iif( nBase >= 3 .AND. nBase < 7, nx1 - ndx, nx1 + ndx )
      nCurr := Int( (ny-1)*8 + nx-1 )
      IF nx <= 0 .OR. nx > 8 .OR. ny <= 0 .OR. ny > 8
         IF ny > 8
            nyq := nyq1 + Int( ny/8 )
            ny := Int( ny%8 )
         ELSEIF ny <= 0
            nyq := nyq1 - Int( Abs(ny/8) + 1 )
            ny := 8 - Int( Abs(ny%8) )
         ENDIF
         IF nx > 8
            nxq := nxq1 + Int( nx/8 )
            nx := Int( nx%8 )
         ELSEIF nx <= 0
            nxq := nxq1 - Int( Abs(nx/8) + 1 )
            nx := 8 - Int( Abs(nx%8) )
         ENDIF
         IF nxq <= 0 .OR. nxq > 8 .OR. nyq <= 0 .OR. nyq > 8
            nyq := Int(nCurrQua / 8) + 1
            nxq := Int(nCurrQua % 8) + 1
            cMessage += "LT. Uhura reports:" + Chr(10) + " Message from Starfleet Command:" + Chr(10) + ;
               " Permission to attempt crossing of galactic perimeter" + Chr(10) + ;
               "  is hereby *denied*. Shut down your engines." + Chr(10) + ;
               "Chief Engineer Scott reports:" + Chr(10) + "  Warp Engines shut down at sector " + ;
               Ltrim(Str(Int(nCurrSect/8)+1)) + "," + Ltrim(Str(Int(nCurrSect%8)+1)) + ;
               " of quadrant " + Ltrim(Str(Int(nCurrQua/8)+1)) + "," + Ltrim(Str(Int(nCurrQua%8)+1)) + Chr(10)
            EXIT
         ELSE
            nCurr := Int( (ny-1)*8 + nx-1 )
            nCurrQua := Int( (nyq-1)*8 + nxq-1 )
            aQua := aBoard[nyq, nxq]
         ENDIF
      ELSEIF Ascan( aKli[1], nCurr ) > 0 .OR. Ascan( aBase, nCurr ) > 0 .OR. ;
         Ascan( aStars, nCurr ) > 0
         cMessage += "Warp engines shut down at" + Chr(10) + "sector " + ;
            Ltrim(Str(Int(nCurrSect/8)+1)) + "," + Ltrim(Str(Int(nCurrSect%8)+1)) + " due to bad navigation" + Chr(10)
         EXIT
      ENDIF
      nCurrSect := nCurr
   ENDDO

   aBoard[nyq, nxq, 4] := .T.
   n := Int( Round( n * 8, 0 ) )
   nEnergy -= ( 10 + n )
   nStardate += Round( n/10, 1 )
   IF nStardate > nDateInit + nTime
      EndOfTime()
   ENDIF

   IF nyq != nyq1 .OR. nxq != nxq1
      SetQuadrant()
   ENDIF

   lDocked := .F.
   IF aBoard[nyq, nxq, 2] > 0
      IF ( nCurrSect % 8 > 0 .AND. nCurrSect-1 == aBase[1] ) .OR. ;
         ( nCurrSect % 8 < 7 .AND. nCurrSect+1 == aBase[1] ) .OR. ;
         ( Int(nCurrSect/8) > 0 .AND. nCurrSect-8 == aBase[1] ) .OR. ;
         ( Int(nCurrSect/8) < 7 .AND. nCurrSect+8 == aBase[1] )
         SetDocked()
      ENDIF
   ENDIF

   Repair( Iif( nDistance>1, nDistance/10, nDistance ) )
   IF hb_Random() < 0.2
      IF hb_Random() < 0.6
         SetDamage( hb_Random() * 4 )
      ELSE
         SetDamage( hb_Random() * 3,, .T. )
      ENDIF
   ENDIF

   KlingonAttack()

   RETURN Nil

STATIC FUNCTION DoPhazer( nUnits )

   LOCAL nKli := aBoard[Int(nCurrQua / 8) + 1, Int(nCurrQua % 8) + 1, 1]
   LOCAL nUniPerKli, nUni, i, nDist, dx, dy, lKill := .F.

   IF !lDocked
      nEnergy -= nUnits
   ENDIF
   IF aDamages[8] < 0
      cMessage += "Computer failure reduces accuracy" + Chr(10)
      nUnits *= hb_Random()
   ENDIF

   nUniPerKli := Int(nUnits / nKli)
   //edi_writelog( "dopha: " + ltrim(str(nkli)) + " " + ltrim(str(len(akli[1]))) )
   FOR i := nKli TO 1 STEP -1
      dy := Int(nCurrSect / 8) - Int(aKli[1,i] / 8)
      dx := Int(nCurrSect % 8) - Int(aKli[1,i] % 8)
      nDist := Sqrt( dy * dy + dx * dx )
      nUni := Min( ( nUniPerKli / ndist * ( hb_Random() + 2 ) ), nUniPerKli )
      IF nUni < 0.15 * aKli[2,i]
         cMessage += "Sensors show no damage to enemy at " + ;
            Ltrim(Str(Int(aKli[1,i] / 8)+1)) + "," + Ltrim(Str(Int(aKli[1,i] % 8)+1)) + Chr(10)
      ELSE
         aKli[2,i] -= nUni
         cMessage += Ltrim(Str(nUni)) + " unit hit on Klingon at sector " + ;
            Ltrim(Str(Int(aKli[1,i] / 8)+1)) + "," + Ltrim(Str(Int(aKli[1,i] % 8)+1)) + Chr(10)
         IF aKli[2,i] <= 0
            cMessage += "*** Klingon Destroyed ***" + Chr(10)
            aBoard[Int(nCurrQua / 8) + 1, Int(nCurrQua % 8) + 1, 1] --
            nKlings --
            @ y1t+1+Int(aKli[1,i]/8)+1, x2t+((aKli[1,i]%8)+1)*3 SAY "°"
            ADel( aKli[1], i )
            ADel( aKli[2], i )
            lKill := .T.
         ENDIF
      ENDIF
   NEXT
   IF lKill
      Inkey( 0.5 )
      DrawSRS()
      IF nKlings == 0
         WinGame()
      ENDIF
   ENDIF
   KlingonAttack()

   RETURN Nil

STATIC FUNCTION DoTor( nAngle )

   LOCAL n := 0, nBase := Int(nAngle), nKoef := nAngle - nBase, ndx, ndy
   LOCAL ny1 := Int(nCurrSect/8)+1, nx1 := Int(nCurrSect%8)+1, ny, nx, nCurr, i
   LOCAL aQua := aBoard[Int(nCurrQua / 8) + 1, Int(nCurrQua % 8) + 1]

   IF !lDocked
      nEnergy -= 2
      nTorped --
   ENDIF

   DO WHILE .T.
      n += 0.125
      DirDist( n, nBase, nKoef, @ndy, @ndx )
      ndy *= 8
      ndx *= 8
      ndy := Int( ndy ) + Iif( ndy-Int(ndy)>0.1, 1, 0 )
      ndx := Int( ndx ) + Iif( ndx-Int(ndx)>0.1, 1, 0 )
      ny := Iif( nBase >= 5, ny1 + ndy, ny1 - ndy )
      nx := Iif( nBase >= 3 .AND. nBase < 7, nx1 - ndx, nx1 + ndx )
      nCurr := Int( (ny-1)*8 + nx-1 )

      IF nx <= 0 .OR. nx > 8 .OR. ny <= 0 .OR. ny > 8
         cMessage += "Torpedo missed" + Chr(10)
         RETURN Nil

      ELSEIF ( i := Ascan( aKli[1], nCurr ) ) > 0
         cMessage += "*** Klingon Destroyed ***" + Chr(10)
         aQua[1] --
         @ y1t+1+Int(aKli[1,i]/8)+1, x2t+((aKli[1,i]%8)+1)*3 SAY "°"
         ADel( aKli[1], i )
         ADel( aKli[2], i )
         Inkey( 0.5 )
         DrawSRS()
         IF --nKlings == 0
            WinGame()
         ENDIF
         EXIT

      ELSEIF ( i := Ascan( aBase, nCurr ) ) > 0
         cMessage += "*** Starbase Destroyed ***" + Chr(10)
         aQua[2] --
         @ y1t+1+Int(aBase[i]/8)+1, x2t+((aBase[i]%8)+1)*3 SAY "°"
         ADel( aBase, i )
         lDocked := .F.
         Inkey( 0.5 )
         EXIT

      ELSEIF Ascan( aStars, nCurr ) > 0
         cMessage += "Star at " + Ltrim(Str(ny)) + "," + Ltrim(Str(nx)) + " absorbed torpedo energy" + Chr(10)
         EXIT

      ENDIF
   ENDDO

   KlingonAttack()

   RETURN Nil

STATIC FUNCTION DoShi( nShi )

   IF nShi < 0 .OR. nShi > nEnergy
      cMessage += "Wrong number! Sheilds Unchanged." + Chr(10)
   ELSE
      nShield := nShi
   ENDIF

   RETURN Nil

STATIC FUNCTION DoDam()

   LOCAL i, nDam := 0

   IF aDamages[7] < 0
      cMessage += "Damage Control report not available." + Chr(10)
      IF !lDocked
         RETURN Nil
      ENDIF
   ENDIF
   IF lDocked
      FOR i := 1 TO 8
         IF aDamages[i] < 0
            nDam += 0.1
         ENDIF
      NEXT
      IF !Empty( i := AskDop( "Will you authorize" + Chr(10) + "the repair order (Y/N)? ", "X" ) ) .AND. ;
         Lower( i ) == "y"
         FOR i := 1 TO 8
            IF aDamages[i] < 0
               aDamages[i] := 0
            ENDIF
         NEXT
         nStardate += nDam + 0.1
      ENDIF
   ENDIF
   ClearSRS()
   @ y1t, x2t SAY "Device        State of Repair"
   FOR i := 1 TO 8
      @ y1t+i, x2t SAY PAdr( DevName(i), 22 ) + Ltrim(Str(aDamages[i]))
   NEXT

   RETURN Nil

STATIC FUNCTION DoCompute( n )

   LOCAL aAns1, aAns2, i, nKli, ny, nx, arr

   ClearDop()

   IF n == 1
      DrawFull()

   ELSEIF n == 2
      cMessage += "Klingons left: " + Ltrim(Str(nKlings)) + Chr(10)
      cMessage += "Mission must be completed in " + Ltrim(Str(nDateInit+nTime-nStardate)) + " stardates." + Chr(10)
      cMessage += "The Federation is maintaining " + Ltrim(Str(nBases)) + " starbases" + Chr(10)
      cMessage += "in the galaxy." + Chr(10)

   ELSEIF n == 3
      IF !Empty( aAns1 := AskCoor( yCmd + 1, "Initial coordinates (y,x)?" ) ) .AND. ;
         !Empty( aAns2 := AskCoor( yCmd + 2, "Final coordinates (y,x)?" ) )
         aAns1 := Compute_DirDist( aAns1, aAns2 )
         @ yCmd+3, x1t+8 SAY "Course: " + Ltrim(Str( aAns1[1] ) )
         @ yCmd+4, x1t+8 SAY "Distance: " + Ltrim(Str( aAns1[2] ) )
      ENDIF

   ELSEIF n == 4
      IF aBoard[Int(nCurrQua / 8) + 1, Int(nCurrQua % 8) + 1, 2] == 0
         @ yCmd+1, x1t+2 SAY "Mr.Spok reports:" + Chr(10)
         @ yCmd+2, x1t+2 SAY " Sensors show no starbases in this quadrant." + Chr(10)
      ELSE
         arr := {}
         IF aBase[1] % 8 > 0
            Aadd( arr, {aBase[1]-1,Nil} )
         ENDIF
         IF aBase[1] % 8 < 7
            Aadd( arr, {aBase[1]+1,Nil} )
         ENDIF
         IF Int(aBase[1]/8) > 0
            Aadd( arr, {aBase[1]-8,Nil} )
         ENDIF
         IF Int(aBase[1]/8) < 7
            Aadd( arr, {aBase[1]+8,Nil} )
         ENDIF
         ny := Int(nCurrSect / 8) + 1
         nx := Int(nCurrSect % 8) + 1
         FOR i := Len( arr ) TO 1 STEP -1
            arr[i,2] := Compute_DirDist( {ny,nx}, {Int(arr[i,1] / 8) + 1, Int(arr[i,1] % 8) + 1} )
            IF !TestWay( arr[i,2,1], arr[i,2,2] )
               hb_ADel( arr, i, .T. )
            ENDIF
         NEXT
         IF Len( arr ) > 0
            ny := 1
            FOR i := 2 TO Len( arr )
               IF arr[i,2,2] < arr[ny,2,2]
                  ny := i
               ENDIF
            NEXT
            @ yCmd+1, x1t+2 SAY "Direction: " + Ltrim(Str(arr[ny,2,1])) + ", Distance: " + Ltrim(Str(arr[ny,2,2]))
         ELSE
            @ yCmd+1, x1t+2 SAY "Mr.Spok reports:"
            @ yCmd+2, x1t+2 SAY " No way to starbase."
         ENDIF
      ENDIF

   ELSEIF n == 5
      IF ( nKli := aBoard[Int(nCurrQua / 8) + 1, Int(nCurrQua % 8) + 1, 1] ) == 0
         @ yCmd+1, x1t+2 SAY "Science Officer Spock reports:"
         @ yCmd+2, x1t+3 SAY "Sensors show no enemy ships in this quadrant"
      ELSE
         ny := Int(nCurrSect / 8) + 1
         nx := Int(nCurrSect % 8) + 1
         aAns2 := { 0,1 }
         FOR i := 1 TO nKli
            aAns1 := Compute_DirDist( {ny,nx}, {Int(aKli[1,i] / 8) + 1, Int(aKli[1,i] % 8) + 1} )
            @ yCmd+i, x1t+2 SAY "Direction: " + Ltrim(Str(aAns1[1])) + ", Distance: " + Ltrim(Str(aAns1[2]))
         NEXT
      ENDIF

   ELSEIF n == 6
      DrawMap()
   ENDIF

   nStardate += 0.01
   KlingonAttack()

   RETURN Nil


STATIC FUNCTION AskDop( cQue1, cPict1, cQue2, cPict2 )

   LOCAL nAns1 := Iif(Left(cPict1,1)=='9',0,Space(Len(cPict1)))
   LOCAL nAns2 := Iif(cPict2!=Nil,Iif(Left(cPict2,1)=='9',0,Space(Len(cPict2))),0)
   LOCAL y := yCmd+1, arr, i
   MEMVAR getlist

   SET CONFIRM ON
   SET SCORE OFF

   SetColor( clrBoard+','+clrBoard+','+clrBoard+','+clrBoard+','+clrBoard )
   arr := hb_ATokens( cQue1, Chr(10) )
   FOR i := 1 TO Len(arr)-1
      @ y, x1t+2 SAY arr[i]
      y ++
   NEXT
   @ y, x1t+2 SAY ATail(arr) GET nAns1 PICTURE cPict1
   y ++
   READ

   IF LastKey() != K_ESC .AND. cQue2 != Nil
      arr := hb_ATokens( cQue2, Chr(10) )
      FOR i := 1 TO Len(arr)-1
         @ y, x1t+2 SAY arr[i]
         y ++
      NEXT
      @ y, x1t+2 SAY ATail(arr) GET nAns2 PICTURE cPict2
      READ
   ENDIF

   RETURN Iif( LastKey() == K_ESC, Nil, Iif( cQue2 == Nil, nAns1, {nAns1,nAns2} ) )

STATIC FUNCTION AskCoor( y, cQue )

   LOCAL cAns := "   "
   MEMVAR getlist

   SET CONFIRM ON
   SET SCORE OFF

   SetColor( clrBoard+','+clrBoard+','+clrBoard+','+clrBoard+','+clrBoard )
   @ y, x1t+2 SAY cQue GET cAns PICTURE "9,9"
   READ
   IF LastKey() == K_ESC
      Return Nil
   ENDIF

   RETURN { Val(Left(cAns,1)), Val(Right(cAns,1))}

STATIC FUNCTION CreateBoard()

   LOCAL i, j
   LOCAL nk, nb, ns, n, nCurr, npos, y, x

   aBoard := Array( 8,8 )

   nDateInit := nStardate := 2400
   nTime := 25 + hb_RandomInt( 0, 10 )

   IF nLevel == 1
      nEnergy := 3000
      nShield := 1000
      nKliOnStart := nKlings := hb_RandomInt( 12, 19 )
      nBases := hb_RandomInt( 2, 4 )
      nStars := 150
      nTorpedInit := nTorped := 10
      nmaxk := 3
      nmaxs := 5
   ELSEIF nLevel == 2
      nEnergy := 5000
      nShield := 1500
      nKliOnStart := nKlings := hb_RandomInt( 36, 48 )
      nBases := hb_RandomInt( 4, 6 )
      nStars := 190
      nTorpedInit := nTorped := 15
      nmaxk := 4
      nmaxs := 6
   ELSEIF nLevel == 3
      nEnergy := 6500
      nShield := 1500
      nKliOnStart := nKlings := hb_RandomInt( 56, 72 )
      nBases := hb_RandomInt( 7, 10 )
      nStars := 190
      nTorpedInit := nTorped := 15
      nmaxk := 5
      nmaxs := 6
   ENDIF

   nEnergyInit := nEnergy
   nk := nKlings
   nb := nBases
   ns := nStars
   aKli := Array( 2,nmaxk )
   aBase := Array( 1 )
   aStars := Array( nmaxs )
   aDamages := Array( 8 )
   AFill( aDamages, 0 )

   FOR i := 1 TO 8
      FOR j := 1 TO 8
         aBoard[i,j] := { 0, 0, 0, .F. }
      NEXT
   NEXT

   DO WHILE nk > 0 .OR. nb > 0 .OR. ns > 0
      IF nk > 0
         n := hb_RandomInt( 1, Min( nmaxk,nk ) )
         npos := hb_RandomInt( 0, 63 )
         x := npos % 8 + 1
         y := Int(npos / 8) + 1
         nCurr := aBoard[y,x,1]
         IF nCurr < nmaxk
            n := Iif( nmaxk - nCurr >= n, n, nmaxk - nCurr )
            nk -= n
            aBoard[y,x,1] := n
         ENDIF
      ENDIF

      IF nb > 0
         npos := hb_RandomInt( 0, 63 )
         x := npos % 8 + 1
         y := Int(npos / 8) + 1
         nCurr := aBoard[y,x,2]
         IF nCurr == 0
            nb -= 1
            aBoard[y,x,2] := 1
         ENDIF
      ENDIF

      IF ns > 0
         n := hb_RandomInt( 1, Min( nmaxs,ns ) )
         npos := hb_RandomInt( 0, 63 )
         x := npos % 8 + 1
         y := Int(npos / 8) + 1
         nCurr := aBoard[y,x,3]
         IF nCurr < nmaxs
            n := Iif( nmaxs - nCurr >= n, n, nmaxs - nCurr )
            ns -= n
            aBoard[y,x,3] := n
         ENDIF
      ENDIF

   ENDDO

   nCurrQua := hb_RandomInt( 0, 63 )
   nCurrSect := hb_RandomInt( 0, 63 )
   SetQuadrant()

   RETURN Nil

STATIC FUNCTION SetQuadrant()

   LOCAL n
   LOCAL nyq := Int(nCurrQua / 8) + 1, nxq := Int(nCurrQua % 8) + 1
   LOCAL aQua := aBoard[nyq, nxq]

   aQua[4] := .T.
   AFill( aKli[1], -1 )
   AFill( aKli[2], 0 )
   AFill( aBase, -1 )
   AFill( aStars, -1 )

   IF ( n := aQua[1] ) > 0
      SetObjects( 1, n, nCurrSect )
   ENDIF
   IF ( n := aQua[2] ) > 0
      SetObjects( 2, n, nCurrSect )
   ENDIF
   IF ( n := aQua[3] ) > 0
      SetObjects( 3, n, nCurrSect )
   ENDIF

   cMessage += "Now entering quadrant " + QuaName( nyq, nxq, .T. ) + Chr(10)
   IF aQua[1] > 0
      cMessage += "Combat Area Condition Red" + Chr(10)
      IF nEnergy < 200
         cMessage += "Shields Dangerously Low" + Chr(10)
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION SetObjects( ntype, n, nExcl )

   LOCAL i, l, npos

   FOR i := 1 TO n
      l := .F.
      DO WHILE !l
         npos := hb_RandomInt( 0, 63 )
         IF Ascan( aKli[1], npos ) == 0 .AND. Ascan( aBase, npos ) == 0 ;
            .AND. Ascan( aStars, npos ) == 0 .AND. ( nExcl == Nil .OR. npos != nExcl )
            IF ntype == 1
               aKli[1,i] := npos
               aKli[2,i] := Iif( nLevel == 1, 100, Iif( nLevel == 2, 200, 300 ) ) + hb_RandomInt(0,200)
            ELSEIF ntype == 2
               aBase[i] := npos
            ELSE
               aStars[i] := npos
            ENDIF
            l := .T.
         ENDIF
      ENDDO
   NEXT

   RETURN Nil

STATIC FUNCTION KlingonAttack()

   LOCAL nKli := aBoard[Int(nCurrQua / 8) + 1, Int(nCurrQua % 8) + 1, 1]
   LOCAL i, nUni, dy, dx, nDist, arr, nSumm := 0, nDam

   IF nKli == 0
      RETURN Nil
   ENDIF
   IF lDocked
      cMessage += "Starbase shields protect the Enterprise" + Chr(10)
      RETURN Nil
   ENDIF

   arr := Array( nKli )
   FOR i := 1 TO nKli
      dy := Int(nCurrSect / 8) - Int(aKli[1,i] / 8)
      dx := Int(nCurrSect % 8) - Int(aKli[1,i] % 8)
      nDist := Sqrt( dy * dy + dx * dx )
      nUni := Int( Min( ( (aKli[2,i] / ndist) * ( hb_Random() + 2 ) ), aKli[2,i] ) )
      aKli[2,i] := aKli[2,i] / ( 3 + hb_Random() )
      arr[i] := nUni
      nSumm += nUni
   NEXT

   nShield -= nSumm
   nEnergy -= nSumm
   Inkey( 1 )
   cMessage += "!!" + Ltrim(Str(nUni)) + " unit hit on Enterprise " + Chr(10)
   cMessage += "Shields down to " + Ltrim(Str(nShield)) + " units" + Chr(10)
   IF nShield < 0
      DrawMessages()
      Inkey(3)
      LoseGame()
      RETURN Nil
   ENDIF
   arr := ASort( arr )
   FOR i := nKli TO Max( 1,nKli-1 ) STEP -1
     IF arr[i] >= 20 .OR. hb_Random() <= 0.6 .OR. arr[i]/nShield > 0.2
        nDam := ( arr[i] / nShield ) - ( 0.4 * hb_Random() )
        SetDamage( Iif( nDam>0, nDam, -nDam ), .T. )
     ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION Repair( nRepa )

   LOCAL i

   FOR i := 1 TO 8
      IF aDamages[i] < 0
         aDamages[i] += nRepa
         IF aDamages[i] > 0
            aDamages[i] := 1
            cMessage += "Damage Control report:" + Chr(10) + "  " + DevName( i ) + " repair completed" + Chr(10)
         ENDIF
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION SetDamage( nDam, lHit, lRepair )

   LOCAL n
   STATIC cMess := "Damage Control reports" + Chr(10)

   IF Empty( lRepair )
      n := hb_RandomInt( 1, 8 )
      aDamages[n] -= nDam
      IF aDamages[n] < 0
         cMessage += cMess
         cMessage += DevName( n ) + " is damaged" + Iif( !Empty(lHit), " by hit", "" ) + Chr(10)
      ENDIF
   ELSE
      FOR n := 1 TO 8
         IF aDamages[n] < 0
            aDamages[n] += nDam
            cMessage += cMess
            cMessage += DevName( n ) + " state of repair improved" + Chr(10)
            EXIT
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

STATIC FUNCTION SetDocked()

   lDocked := .T.
   nEnergy := nEnergyInit
   nTorped := nTorpedInit
   IF nShield > 0
      cMessage += "Shields dropped for docking purposes" + Chr(10)
   ENDIF
   nShield := 0

   RETURN Nil

STATIC FUNCTION WinGame()

   LOCAL nEffi

   SetColor( clrBoard )
   Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )

   @ y1t+1, x1t+8 SAY "Congradulations, Captain!  The last Klingon Battle Cruiser"
   @ y1t+2, x1t+8 SAY "menacing the Federation has been destoyed."

   IF nStarDate - nDateInit < nTime
      nEffi := Round( nKliOnStart / ( nStarDate - nDateInit ), 2 )
      @ y1t+3, x1t+8 SAY "Your efficiency rating is " + Ltrim(Str( Int(1000 * nEffi * nEffi) ))
   ENDIF
   Inkey( 0 )
   EndOfGame()

   RETURN Nil

STATIC FUNCTION LoseGame()

   SetColor( clrBoard )
   Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )

   @ y1t+1, x1t+8 SAY "The Enterprise has been destroyed."
   @ y1t+2, x1t+8 SAY "The Federation will be conquered."

   RETURN EndOfTime( .T. )

STATIC FUNCTION EndOfTime( lNoClear )

   IF Empty( lNoClear )
      SetColor( clrBoard )
      Scroll( oGame:y1, oGame:x1, oGame:y2, oGame:x2 )
   ENDIF
   @ y1t+3, x1t+8 SAY "It is stardate " + Ltrim(Str(nStardate))
   @ y1t+4, x1t+8 SAY "There were " + Ltrim(Str(nKlings)) + " Klingon Battlecruisers left"
   @ y1t+5, x1t+9 SAY "at the end of your mission."

   Inkey( 0 )

   RETURN EndOfGame()

STATIC FUNCTION EndOfGame()

   cScreenBuff := Nil
   Write_Game_Ini()
   mnu_Exit( oGame )

   RETURN Nil

STATIC FUNCTION DrawCommand( s )

   cCommand := s
   SetColor( clrBoard )
   @ yCmd, x1t+8 SAY PAdr( s, 12 )
   DevPos( yCmd, x1t+8+Len(s) )

   RETURN Nil

STATIC FUNCTION DrawCmds()

   lSRS := .F.
   SetColor( clrBoard )
   Scroll( y1t, x2t, y1t+10, x2t+35 )
   @ y1t-1, x2t+6 SAY "Commands:"
   @ y1t, x2t SAY "nav (F1) - to move"
   @ y1t+1, x2t SAY "srs (F2) - short range sensor scan"
   @ y1t+2, x2t SAY "lrs (F3) - long range sensor scan"
   @ y1t+3, x2t SAY "pha (F4) - to fire phasers"
   @ y1t+4, x2t SAY "tor (F5) - to fire photon torpedoes"
   @ y1t+5, x2t SAY "she (F6) - to raise or lower shields"
   @ y1t+6, x2t SAY "dam (F7) - damage control report"
   @ y1t+7, x2t SAY "com (F8) - to call on library-computer"
   @ y1t+8, x2t SAY "doc      - show game instructions"
   @ y1t+9, x2t SAY "res (F10)- resign, exit"

   RETURN Nil

STATIC FUNCTION DrawText()

   SetColor( clrBoard )
   @ y1t+1, x1t SAY PAdr( "Stardate", 18 ) + Ltrim(Str(Round(nStardate,1)))
   @ y1t+2, x1t SAY PAdr( "Condition", 18 )
   IF lDocked
      @ y1t+2, x1t+18 SAY "DOCKED"
   ELSEIF aBoard[Int(nCurrQua / 8) + 1,Int(nCurrQua % 8) + 1,1] > 0
      @ y1t+2, x1t+18 SAY " RED  " COLOR clrAlert
   ELSEIF nEnergy < nEnergyInit / 10
      @ y1t+2, x1t+18 SAY "YELLOW" COLOR clrYellow
   ELSE
      @ y1t+2, x1t+18 SAY "GREEN " COLOR clrGreen
   ENDIF
   @ y1t+3, x1t SAY PAdr( "Quadrant", 18 ) + Ltrim(Str(Int(nCurrQua/8)+1))+","+Ltrim(Str(Int(nCurrQua%8)+1))
   @ y1t+4, x1t SAY PAdr( "Sector", 18 ) + Ltrim(Str(Int(nCurrSect/8)+1))+","+Ltrim(Str(Int(nCurrSect%8)+1))
   @ y1t+5, x1t SAY PAdr( "Photon torpedoes", 18 ) + PAdr( Ltrim(Str(nTorped)), 2 )
   @ y1t+6, x1t SAY PAdr( "Total energy", 18 ) + PAdr( Ltrim(Str(nEnergy)), 4 )
   @ y1t+7, x1t SAY PAdr( "Shield", 18 ) + PAdr( Ltrim(Str(nShield)), 4 ) ;
      COLOR Iif( !lDocked .AND. nShield < 100, clrAlert, clrBoard )
   @ y1t+8, x1t SAY PAdr( "Klingons", 18 ) + PAdr( Ltrim(Str(nKlings)), 2 )

   @ yCmd, x1t SAY "Command> "

   RETURN Nil

STATIC FUNCTION DrawSRS()

   LOCAL y, x, ny, nx, nyb, nxb, i

   IF !lSRS
      RETURN Nil
   ENDIF

   x := Int(nCurrQua % 8) + 1
   y := Int(nCurrQua / 8) + 1
   nx := Int(nCurrSect % 8) + 1
   ny := Int(nCurrSect / 8) + 1

   ClearSRS()
   IF aDamages[2] < 0
      cMessage += "*** Short Range Sensors are out ***" + Chr(10)
      RETURN Nil
   ENDIF

   @ y1t-1, x2t+6 SAY "Short range scan"
   SetColor( clrSRS )
   @ y1t, x2t+3 SAY     "1  2  3  4  5  6  7  8"
   @ y1t+1, x2t+1 SAY "ÚÄÂÄÄÂÄÄÂÄÄÂÄÄÂÄÄÂÄÄÂÄÄÂÄ¿"
   @ y1t+2, x2t SAY  "1Ã" + Space( 24 ) + "´"
   @ y1t+3, x2t SAY  "2Ã" + Space( 24 ) + "´"
   @ y1t+4, x2t SAY  "3Ã" + Space( 24 ) + "´"
   @ y1t+5, x2t SAY  "4Ã" + Space( 24 ) + "´"
   @ y1t+6, x2t SAY  "5Ã" + Space( 24 ) + "´"
   @ y1t+7, x2t SAY  "6Ã" + Space( 24 ) + "´"
   @ y1t+8, x2t SAY  "7Ã" + Space( 24 ) + "´"
   @ y1t+9, x2t SAY  "8Ã" + Space( 24 ) + "´"
   @ y1t+10,x2t+1 SAY "ÀÄÁÄÄÁÄÄÁÄÄÁÄÄÁÄÄÁÄÄÁÄÄÁÄÙ"

   @ y1t+1+ny, x2t+nx*3 SAY "E"

   FOR i := 1 TO aBoard[y,x,1]
      @ y1t+1+Int(aKli[1,i]/8)+1, x2t+((aKli[1,i]%8)+1)*3 SAY "K"
   NEXT
   IF aBoard[y,x,2] > 0
      nxb := Int(aBase[1] % 8) + 1
      nyb := Int(aBase[1] / 8) + 1
      @ y1t+1+nyb, x2t+nxb*3 SAY "B"
      IF lDocked
         IF ny == nyb
            @ y1t+1+nyb, x2t+nxb*3 + Iif( nxb > nx, -2, 1 ) SAY "--"
         ENDIF
      ENDIF
   ENDIF
   FOR i := 1 TO aBoard[y,x,3]
      @ y1t+1+Int(aStars[i]/8)+1, x2t+((aStars[i]%8)+1)*3 SAY "*"
   NEXT
   SetColor( clrBoard )

   RETURN Nil

STATIC FUNCTION DrawLRS()

   LOCAL y, x, i, j

   x := Int(nCurrQua % 8) + 1
   y := Int(nCurrQua / 8) + 1

   ClearLRS()
   IF aDamages[3] < 0
      cMessage += "*** Long Range Sensors are inoperable ***" + Chr(10)
      DrawMessages()
      RETURN Nil
   ENDIF

   @ y2t, x2t+6 SAY "Long range scan"
   @ y2t+1, x2t+7 SAY "Quadrant " + Ltrim(Str(y)) + "," + Ltrim(Str(x))
   @ y2t+2, x2t+4 SAY "ÚÄÄÄÂÄÄÄÄÂÄÄÄÄÂÄÄÄ¿"
   @ y2t+3, x2t+3 SAY  " Ã" + Space(17) + "´"
   @ y2t+4, x2t+3 SAY  " Ã" + Space(17) + "´"
   @ y2t+5, x2t+3 SAY  " Ã" + Space(17) + "´"
   @ y2t+6,x2t+4 SAY "ÀÄÄÄÁÄÄÄÄÁÄÄÄÄÁÄÄÄÙ"

   FOR i := 1 TO 3
      FOR j := 1 TO 3
         IF y-2+i>0.AND.x-2+j>0.AND.y-2+i<9.AND.x-2+j<9
            @ y2t+2+i, x2t+2+j*5 SAY Ltrim(Str(aBoard[y-2+i,x-2+j,1])) + ;
               Ltrim(Str(aBoard[y-2+i,x-2+j,2])) + Ltrim(Str(aBoard[y-2+i,x-2+j,3]))
            aBoard[y-2+i,x-2+j,4] := .T.
         ELSE
            @ y2t+2+i, x2t+2+j*5 SAY "xxx"
         ENDIF
      NEXT
   NEXT

   RETURN Nil

STATIC FUNCTION DrawFull()

   LOCAL y, x

   lSRS := .F.

   ClearSRS()

   @ y1t-1, x2t+6 SAY ""
   @ y1t, x2t+3 SAY     "1   2   3   4   5   6   7   8"
   @ y1t+1, x2t+1 SAY "ÚÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄ¿"
   @ y1t+2, x2t SAY  "1Ã" + Space( 31 ) + "´"
   @ y1t+3, x2t SAY  "2Ã" + Space( 31 ) + "´"
   @ y1t+4, x2t SAY  "3Ã" + Space( 31 ) + "´"
   @ y1t+5, x2t SAY  "4Ã" + Space( 31 ) + "´"
   @ y1t+6, x2t SAY  "5Ã" + Space( 31 ) + "´"
   @ y1t+7, x2t SAY  "6Ã" + Space( 31 ) + "´"
   @ y1t+8, x2t SAY  "7Ã" + Space( 31 ) + "´"
   @ y1t+9, x2t SAY  "8Ã" + Space( 31 ) + "´"
   @ y1t+10,x2t+1 SAY "ÀÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÙ"

   FOR y := 1 TO 8
      FOR x := 1 TO 8
         @ y1t+1+y, x2t-2+x*4 SAY Iif( aBoard[y,x,4], ;
            Ltrim(Str(aBoard[y,x,1])) + Ltrim(Str(aBoard[y,x,2])) + ;
            Ltrim(Str(aBoard[y,x,3])), "xxx" )
      NEXT
   NEXT

   RETURN Nil

STATIC FUNCTION DrawMap()

   LOCAL y

   ClearSRS()

   @ y1t-1, x2t+6 SAY "The Galaxy"
   @ y1t, x2t+2 SAY  " 1    2    3    4    5    6    7    8"
   @ y1t, x2t+1 SAY "---- ---- ---- ---- ---- ---- ---- ----"

   FOR y := 1 TO 8
      @ y1t+y, x2t SAY Str( y,1 ) + PAdc( QuaName(y,1,.F.), 19 ) + PAdc( QuaName(y,5,.F.), 20 )
   NEXT

   RETURN Nil

STATIC FUNCTION DrawCompCmds()

   ClearLRS()

   @ y2t, x2t SAY "Functions available:"
   @ y2t+1, x2t SAY "1 - Cumulative galactic record"
   @ y2t+2, x2t SAY "2 - Status report"
   @ y2t+3, x2t SAY "3 - Direction/distance calculator"
   @ y2t+4, x2t SAY "4 - Starbase nav data"
   @ y2t+5, x2t SAY "5 - Photon torpedo data"
   @ y2t+6, x2t SAY "6 - Galaxy 'region name'"

   RETURN Nil

STATIC FUNCTION DrawInit()

   LOCAL cAns := "n"
   Memvar getlist

   @ y1t, x1t+20 SAY "* * Star Trek * *"
   @ y1t+1, x1t+16 SAY  "                ------*------"
   @ y1t+2, x1t+16 SAY  "-------------   `---  ------'"
   @ y1t+3, x1t+16 SAY  "`-------- --'      / /"
   @ y1t+4, x1t+16 SAY  "         \\\\-------  --"
   @ y1t+5, x1t+16 SAY  "         '-----------'"
   @ y1t+7, x1t+14 SAY  "The USS Enterprise --- NCC - 1701"

   @ y1t+9, x1t+3 SAY "Your orders are as follows:"
   @ y1t+10, x1t+5 SAY "Destroy the " + Ltrim(Str(nKlings)) + " Klingon warships which have invaded"
   @ y1t+11, x1t+5 SAY "the galaxy before they can attack Federation Headquarters"
   @ y1t+12, x1t+5 SAY "on stardate " + Ltrim(Str(nDateInit+nTime)) + ". This gives you " + Ltrim(Str(nTime)) + " days. There are"
   @ y1t+13, x1t+5 SAY Ltrim(Str(nBases)) + " starbases in the galaxy for resupplying your ship."

   SET CONFIRM OFF
   SET SCORE OFF
   SetColor( clrBoard+','+clrBoard+','+clrBoard+','+clrBoard+','+clrBoard )
   @ y1t+15, x1t+5 SAY "Do you need to see instructions (y/n)" GET cAns PICTURE "X"
   READ
   IF LastKey() != 27 .AND. Lower(cAns) == "y"
     KEYBOARD "doc" + Chr(13)
   ENDIF

   RETURN Nil

STATIC FUNCTION ClearSRS()

   SetColor( clrBoard )
   Scroll( y1t, x2t, y1t+10, x2t+35 )

   RETURN Nil

STATIC FUNCTION DrawMessages()

   LOCAL y := y2t-1, arr := hb_ATokens( cMessage, Chr(10) ), i

   ClearLRS()

   FOR i := 1 TO Len( arr )
      @ y+i, x2t SAY arr[i] COLOR Iif( Left(arr[i],1)=='!', clrAlert, clrBoard )
   NEXT
   cMessage := ""

   RETURN Nil

STATIC FUNCTION ClearLRS()

   SetColor( clrBoard )
   Scroll( y2t, x2t, oGame:y2, oGame:x2 )

   RETURN Nil

STATIC FUNCTION ClearDop()

   SetColor( clrBoard )
   Scroll( yCmd+1, x1t, yCmd+8, x2t-1 )

   RETURN Nil

STATIC FUNCTION DevName( nDev )

   STATIC arr := { "Warp Engines", "Short Range Sensors", "Long Range Sensors", ;
      "Phaser Control", "Photon Tubes", "Sheild Control", "Damage Control", "Library-Computer" }

   RETURN arr[nDev]

STATIC FUNCTION QuaName( nyq, nxq, lNum )

   LOCAL s
   STATIC arr1 := { "Antares", "Rigel", "Procyon", "Vega", "Canopus", "Altair", "Sagittarius", "Pollux", ;
      "Sirius", "Deneb", "Capella", "Betelgeuse", "Aldebaran", "Regulus", "Arcturus", "Spica" }
   STATIC arr2 := { "I", "II", "III", "IV" }

   IF nxq <= 4
      s := arr1[nyq]
   ELSE
      s := arr1[nyq+8]
   ENDIF
   IF nxq > 4
      nxq -= 4
   ENDIF

   RETURN Iif( lNum, s + " " + arr2[nxq], s )

STATIC FUNCTION Compute_DirDist( aAns1, aAns2 )
   LOCAL aAns := { 0,0 }
   LOCAL ndx := aAns2[2]-aAns1[2], ndy := aAns2[1]-aAns1[1], nBase, nDop, lx

   IF ndx == 0
      aAns[1] := Iif( ndy>0, 7, 3 )
      aAns[2] := Abs( ndy ) / 8

   ELSEIF ndy == 0
      aAns[1] := Iif( ndx>0, 1, 5 )
      aAns[2] := Abs( ndx ) / 8

   ELSE
      nBase := Iif( ndy<0.AND.ndx>0, 1, Iif( ndy<0.AND.ndx<0, 3, Iif( ndy>0.AND.ndx<0, 5, 7 ) ) )
      ndx := Abs(ndx)
      ndy := Abs(ndy)
      IF ndx == ndy
         aAns[1] := nBase + 1
      ELSE
         nDop := Round( Iif( (lx := (ndx > ndy)), ndy/ndx, ndx/ndy ), 3 )
         aAns[1] := nBase + nDop
         IF (nBase == 1 .AND. !lx) .OR. (nBase == 7 .AND. lx) .OR. ;
            (nBase == 5 .AND. !lx) .OR. (nBase == 3 .AND. lx)
            aAns[1] += 1
         ENDIF
      ENDIF
      aAns[2] := Round( Sqrt( ndx*ndx/64 + ndy*ndy/64 ), 3 )

   ENDIF

   RETURN aAns

STATIC FUNCTION DirDist( n, nBase, nKoef, ndy, ndx )
      IF nKoef == 0
         IF nBase == 3 .OR. nBase == 7
            ndy := n
            ndx := 0
         ELSEIF nBase == 5 .OR. nBase == 1
            ndy := 0
            ndx := n
         ELSE
            ndy := ndx := Sqrt( n*n/2 )
         ENDIF
      ELSE
         IF nBase == 2 .OR. nBase == 7 .OR. nBase == 6 .OR. nBase == 3
            ndy := Sqrt( n * n / (nKoef*nKoef+1) )
            ndx := nKoef * ndy
         ELSE
            ndx := Sqrt( n * n / (nKoef*nKoef+1) )
            ndy := nKoef * ndx
         ENDIF
      ENDIF

   RETURN Nil

STATIC FUNCTION TestWay( nAngle, nDistance )

   LOCAL n := 0, nBase := Int(nAngle), nKoef := nAngle - nBase, ndx, ndy
   LOCAL ny1 := Int(nCurrSect/8)+1, nx1 := Int(nCurrSect%8)+1, ny, nx, nCurr

   DO WHILE n < nDistance
      IF nDistance - n > 0.125
         n += 0.125
      ELSE
         n := nDistance
      ENDIF
      DirDist( n, nBase, nKoef, @ndy, @ndx )
      ndy *= 8
      ndx *= 8
      ndy := Int( ndy ) + Iif( ndy-Int(ndy)>0.1, 1, 0 )
      ndx := Int( ndx ) + Iif( ndx-Int(ndx)>0.1, 1, 0 )
      ny := Iif( nBase >= 5, ny1 + ndy, ny1 - ndy )
      nx := Iif( nBase >= 3 .AND. nBase < 7, nx1 - ndx, nx1 + ndx )
      nCurr := Int( (ny-1)*8 + nx-1 )
      IF Ascan( aKli[1], nCurr ) > 0 .OR. Ascan( aBase, nCurr ) > 0 .OR. ;
         Ascan( aStars, nCurr ) > 0
         RETURN .F.
      ENDIF
   ENDDO

   RETURN .T.

STATIC FUNCTION Read_Game_Ini( cIni )

   LOCAL hIni, aIni, nSect, cTemp, aSect

   IF !Empty( cIni ) .AND. !Empty( hIni := edi_iniRead( cIni ) )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "GAME"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "level" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nLevel := Val( cTemp )
                  IF nLevel < 1 .OR. nLevel > 3
                     nLevel := 1
                  ENDIF
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrboard" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrBoard := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clralert" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrAlert := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrgreen" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrGreen := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clryellow" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrYellow := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "clrsrs" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  clrSRS := cTemp
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION Write_Game_Ini()

   LOCAL s := "[GAME]" + Chr(13)+Chr(10)

   s += "level=" + Ltrim(Str( nLevel )) + Chr(13)+Chr(10)
   s += "clrboard=" + clrBoard + Chr(13)+Chr(10)
   s += "clrAlert=" + clrAlert + Chr(13)+Chr(10)
   s += "clrGreen=" + clrGreen + Chr(13)+Chr(10)
   s += "clrYellow=" + clrYellow + Chr(13)+Chr(10)
   s += "clrSRS=" + clrSRS + Chr(13)+Chr(10)

   hb_MemoWrit( cIniPath + "strek.ini", s )

   RETURN Nil
