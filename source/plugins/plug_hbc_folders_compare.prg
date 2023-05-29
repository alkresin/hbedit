
#define  K_ENTER   13
#define  K_ESC     27
#define  K_TAB      9
#define  K_F1      28
#define  READ_BUFF_LEN 32768

STATIC aCount

FUNCTION plug_hbc_folders_compare( oPane )

   LOCAL i, lUtf8 := FilePane():cp=="utf8", aGets
   LOCAL oPane1 := FilePane():aPanes[1], oPane2 := FilePane():aPanes[2]
   LOCAL y1 := oPane:y1+5, x1 := oPane1:x2-20, y2 := y1+12, x2 := x1+40
   LOCAL cScBuf := Savescreen( y1, x1, y2, x2 )
   LOCAL oldc := SetColor( "N/W"+","+"N/W"+",,"+"N+/W"+","+"N/W" )
   LOCAL cDir1, cDir2, arr1, arr2, lRecur, lSizOnly, lContent, lNoEof
   LOCAL cAppCompare := Iif( hb_hHaskey(FilePane():hMisc,"foldcompare"), FilePane():hMisc["foldcompare"], Nil )

   aGets := { ;
      { y1,x1+5, 11, " Compare folders " }, ;
      { y1+1,x1+2, 11, NameShortcut( oPane1:cCurrPath, 36, "~", lUtf8 ) }, ;
      { y1+2,x1+2, 11, NameShortcut( oPane2:cCurrPath, 36, "~", lUtf8 ) }, ;
      { y1+4,x1+2, 11, "( ) by file dates and sizes" }, { y1+4,x1+3, 3, .T., 2,,,,2 }, ;
      { y1+5,x1+2, 11, "( ) by file content" }, { y1+5,x1+3, 3, .F., 2,,,,2 }, ;
      { y1+6,x1+2, 11, "( ) ignore eof" }, { y1+6,x1+3, 3, .F., 2,,,,2 }, ;
      { y1+7,x1+2, 11, Iif(Empty(cAppCompare),"","( ) external app") }, { y1+7,x1+3, Iif(Empty(cAppCompare),-1,3), .F., 2,,,,2 }, ;
      { y1+9,x1+2, 11, "[ ] Recursive" }, { y1+9,x1+3, 1, .F., 2 }, ;
      { y1+11,x1+5, 2, "[Ok]", 6, "N/W","N/W",{||__KeyBoard(Chr(K_ENTER))} }, ;
      { y1+11,x1+15, 2, "[Cancel]", 10, "N/W","N/W",{||__KeyBoard(Chr(K_ESC))} } }

   hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+3, x1 SAY "Ã"
   @ y1+3, x2 SAY "´"
   @ y1+3, x1+1 TO y1+3, x2-1
   @ y1+8, x1 SAY "Ã"
   @ y1+8, x2 SAY "´"
   @ y1+8, x1+1 TO y1+8, x2-1
   @ y1+10, x1 SAY "Ã"
   @ y1+10, x2 SAY "´"
   @ y1+10, x1+1 TO y1+10, x2-1
   hb_cdpSelect( TEdit():aWindows[TEdit():nCurr]:cp )

   i := edi_READ( aGets )
   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cScBuf )
   IF i > 0
      cDir1 := oPane1:cCurrPath
      cDir2 := oPane2:cCurrPath
      aCount := { {oPane1, 0, 0, cDir1}, {oPane2, 0, 0, cDir2} }
      lSizOnly := aGets[5,4]
      lContent := aGets[7,4]
      lNoEof := aGets[9,4]
      lRecur := aGets[Len(aGets)-2,4]

      IF lSizOnly .OR. lContent .OR. lNoEof
         arr1 := {}
         arr2 := {}
         dirCompare( arr1, arr2, cDir1, cDir2, "", "", lSizOnly, lNoEof, lRecur )
         IF Empty( arr1 ) .AND. Empty( arr2 )
            edi_Alert( "Folders are identical" )
         ELSE
            hb_AIns( arr1, 1, { "..",0,Date(),"","D" }, .T. )
            oPane1:nPanelMod := 1
            oPane1:aDir := arr1
            oPane1:cIOpref_bak := oPane1:cIOpref
            oPane1:net_cAddress_bak := oPane1:net_cAddress
            oPane1:cIOpref := "sea:"
            oPane1:bOnKey := {|o,n|_plug_OnKey(o,n)}
            oPane1:bDrawCell := {|o,n,l|DrawCell(o,n,l)}
            oPane1:nShift := 0
            oPane1:nCurrent := 1
            oPane1:Refresh( .T. )
            oPane1:Draw()
            oPane1:DrawCell( ,.T. )
            oPane1:DrawHead( .T. )

            hb_AIns( arr2, 1, { "..",0,Date(),"","D" }, .T. )
            oPane2:nPanelMod := 1
            oPane2:aDir := arr2
            oPane2:cIOpref_bak := oPane2:cIOpref
            oPane2:net_cAddress_bak := oPane2:net_cAddress
            oPane2:cIOpref := "sea:"
            oPane2:bDrawCell := {|o,n,l|DrawCell(o,n,l)}
            oPane2:bOnKey := {|o,n|_plug_OnKey(o,n)}
            oPane2:nShift := 0
            oPane2:nCurrent := 1
            oPane2:Refresh( .T. )
            oPane2:Draw()
            oPane2:DrawCell( ,.T. )
            oPane2:DrawHead( .T. )

         ENDIF
      ELSE
         cedi_RunApp( cAppCompare + " " + hb_strShrink( cDir1,1 ) + " " + hb_strShrink( cDir2,1 ) )
      ENDIF

   ENDIF

   RETURN Nil

STATIC FUNCTION _plug_OnKey( oPane, nKeyExt )

   LOCAL nKey := hb_keyStd( nKeyExt ), aDir, oPane2, n

   IF nKey == K_TAB
      aDir := oPane:aDir[oPane:nCurrent+oPane:nShift]
      IF Len( aDir ) > 5 .AND. aDir[6] == 1
         oPane2 := Iif( Filepane():aPanes[1] == oPane, Filepane():aPanes[2], Filepane():aPanes[1] )
         IF oPane2:nPanelMod == 1 .AND. ( n := Ascan2( oPane2:aDir, aDir[1] ) ) > 0
            oPane2:nCurrent := n
            IF n > oPane2:nShift + oPane2:nCells
               oPane2:nShift := n - 1
               oPane2:nCurrent := 1
            ELSEIF n < oPane2:nShift + 1
               IF n < oPane2:nCells
                  oPane2:nCurrent := n
                  oPane2:nShift := 0
               ELSE
                  oPane2:nShift := n - 1
                  oPane2:nCurrent := 1
               ENDIF
            ELSE
               oPane2:nCurrent := n
            ENDIF
         ENDIF
      ENDIF
      RETURN 0

   ELSEIF nKey == K_F1
      _plug_Help( oPane )
      RETURN -1
   ENDIF

   RETURN 0

STATIC FUNCTION dirCompare( arr1, arr2, cDir1, cDir2, cRel1, cRel2, lSizOnly, lNoEof, lRecur )

   LOCAL aDir1, aDir2
   LOCAL i, j, ps := hb_ps(), lRes

   IF !( Right( cDir1,1 ) $ "/\" )
      cDir1 += ps
   ENDIF
   IF !( Right( cDir2,1 ) $ "/\" )
      cDir2 += ps
   ENDIF
   aDir1 := hb_vfDirectory( cDir1, "HSD" )
   aDir2 := hb_vfDirectory( cDir2, "HSD" )

   FOR i := 1 TO Len( aDir1 )
      IF ( j := Ascan2( aDir2, aDir1[i,1] ) ) > 0
         IF 'D' $ aDir1[i,5]
            IF lRecur .AND. !(aDir1[i,1] == "..") .AND. !(aDir1[i,1] == ".")
               dirCompare( arr1, arr2, cDir1+aDir1[i,1], cDir2+aDir2[j,1], cRel1+aDir1[i,1]+ps, cRel2+aDir2[j,1]+ps, lSizOnly, lNoEof, lRecur )
            ENDIF
         ELSE  //IF lSizOnly
            IF ( lNoEof .AND. !fileCompare(cDir1+aDir1[i,1], cDir2+aDir2[j,1], lNoEof) ) .OR. ;
               ( !lNoEof .AND. ;
                  ( aDir1[i,2] != aDir2[j,2] .OR. ;
                     ( aDir1[i,3] != aDir2[j,3] .AND. ;
                     ( lSizOnly .OR. !fileCompare(cDir1+aDir1[i,1], cDir2+aDir2[j,1], lNoEof) ) ) ) )
               AAdd( arr1, { cRel1+aDir1[i,1], aDir1[i,2], aDir1[i,3], aDir1[i,4], aDir1[i,5], 1 } )
               AAdd( arr2, { cRel2+aDir2[j,1], aDir2[j,2], aDir2[j,3], aDir2[j,4], aDir2[j,5], 1 } )
               aCount[1,3] ++
               aCount[2,3] ++
            ENDIF
         ENDIF
         aDir2[j,1] := Nil
      ELSE
         IF 'D' $ aDir1[i,5]
            IF lRecur .AND. !(aDir1[i,1] == "..") .AND. !(aDir1[i,1] == ".")
               dirAdd( arr1, cDir1+aDir1[i,1], cRel1+aDir1[i,1]+ps, aCount[1] )
            ENDIF
            //AAdd( arr1, { cRel1+aDir1[i,1]+ps, aDir1[i,2], aDir1[i,3], aDir1[i,4], aDir1[i,5] , 0 } )
         ELSE
            AAdd( arr1, { cRel1+aDir1[i,1], aDir1[i,2], aDir1[i,3], aDir1[i,4], aDir1[i,5], 0 } )
            aCount[1,2] ++
         ENDIF
      ENDIF
   NEXT
   FOR j := 1 TO Len( aDir2 )
      IF !Empty( aDir2[j,1] )
         IF 'D' $ aDir2[j,5]
            IF lRecur .AND. !(aDir2[j,1] == "..") .AND. !(aDir2[j,1] == ".")
               dirAdd( arr2, cDir2+aDir2[j,1], cRel2+aDir2[j,1]+ps, aCount[2] )
            ENDIF
         ELSE
            AAdd( arr2, { cRel2+aDir2[j,1], aDir2[j,2], aDir2[j,3], aDir2[j,4], aDir2[j,5], 0 } )
            aCount[2,2] ++
         ENDIF
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION dirAdd( arr, cDir, cRel, aCou )

   LOCAL aDir, i, ps := hb_ps()

   IF !( Right( cDir, ) $ "/\" )
      cDir += ps
   ENDIF

   aDir := hb_vfDirectory( cDir, "HSD" )
   FOR i := 1 TO Len( aDir )
      IF 'D' $ aDir[i,5]
         IF !(aDir[i,1] == "..") .AND. !(aDir[i,1] == ".")
            dirAdd( arr, cDir+aDir[i,1], cRel+aDir[i,1]+ps, aCou )
         ENDIF
      ELSE
         AAdd( arr, { cRel+aDir[i,1], aDir[i,2], aDir[i,3], aDir[i,4], aDir[i,5], 0 } )
         aCou[2] ++
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION fileCompare( cFile1, cFile2, lNoEof )

   LOCAL handle1, handle2, cBuf1, cBuf2, i, nRet1, nRet2, n1, n2, lRes := .T.
   LOCAL cn := Chr(10), cr := Chr(13), c1, c2

   IF Empty( handle1 := hb_vfOpen( cFile1 ) )
      RETURN .F.
   ENDIF
   IF Empty( handle2 := hb_vfOpen( cFile2 ) )
      hb_vfClose( handle1 )
      RETURN .F.
   ENDIF
   cBuf1 := Space( READ_BUFF_LEN )
   cBuf2 := Space( READ_BUFF_LEN )
   IF lNoEof
      n1 := n2 := 0
      nRet1 := nRet2 := -1
      DO WHILE lRes
         IF n1 > nRet1
            nRet1 := hb_vfRead( handle1, @cBuf1, READ_BUFF_LEN )
            IF nRet1 <= 0
               lRes := (n2 > nRet2)
               //edi_writelog( "1: " + Iif(lRes,"T ", "F ") )
               EXIT
            ENDIF
            n1 := 0
         ENDIF
         IF n2 > nRet1
            nRet2 := hb_vfRead( handle2, @cBuf2, READ_BUFF_LEN )
            IF nRet2 <= 0
               lRes := (n1 > nRet1)
               //edi_writelog( "2: " + Iif(lRes,"T ", "F ") )
               EXIT
            ENDIF
            n2 := 0
         ENDIF
         DO WHILE .T.
            DO WHILE ++n1 <= nRet1 .AND. ( c1 := hb_bpeek( cBuf1, n1 ) ) == 13
            ENDDO
            DO WHILE ++n2 <= nRet2 .AND. ( c2 := hb_bpeek( cBuf2, n2 ) ) == 13
            ENDDO
            IF n1 > nRet1
               IF n2 <= nRet1
                  n2 --
               ENDIF
               EXIT
            ENDIF
            IF n2 > nRet2
               n1 --
               EXIT
            ENDIF
            IF c1 != c2
               lRes := .F.
               //edi_writelog( "3: " + ltrim(str(n1)) + " " + ltrim(str(n2)) )
               EXIT
            ENDIF
         ENDDO
      ENDDO
      //edi_Writelog( Iif(lRes,"T ", "F ") + cFile1 )
   ELSE
      DO WHILE lRes .AND. ( nRet1 := hb_vfRead( handle1, @cBuf1, READ_BUFF_LEN ) ) > 0
         hb_vfRead( handle2, @cBuf2, READ_BUFF_LEN )
         FOR i := 1 TO nRet1
            IF hb_bpeek( cBuf1, i ) != hb_bpeek( cBuf2, i )
               lRes := .F.
               EXIT
            ENDIF
         NEXT
      ENDDO
   ENDIF
   hb_vfClose( handle1 )
   hb_vfClose( handle2 )

   RETURN lRes

STATIC FUNCTION DrawCell( oPane, nCell, lCurr )

   LOCAL arr, nRow, x1 := oPane:x1 + 1, cText, nWidth, cDop, nLen
   LOCAL cDate, dDate, cSize, cClrFil, lUtf8 := (Lower(oPane:cp) == "utf8")

   nRow := nCell := Iif( nCell==Nil,oPane:nCurrent,nCell )
   arr := oPane:aDir[nCell+oPane:nShift]

   nWidth := oPane:nWidth
   IF oPane:nDispMode == 2 .AND. nRow > oPane:nRows
      x1 += ( oPane:nWidth+1 )
      IF ( nRow := (nRow % oPane:nRows) ) == 0
         nRow := oPane:nRows
      ENDIF
   ENDIF

   cText := Trim( arr[1] )
   cClrFil := Iif('D' $ arr[5], oPane:cClrDir, Iif( arr[6] == 0, ;
      oPane:cClrFil, oPane:cClrExe ) )

   SetColor( Iif( lCurr, oPane:cClrCurr, cClrFil ) )
   IF ( nLen := cp_Len( lUtf8, cText ) ) > nWidth
      cText := cp_Left( lUtf8, cText, nWidth-1 ) + '>'
   ENDIF
   @ oPane:y1 + nRow, x1 SAY cText
   IF nLen < nWidth
      @ oPane:y1 + nRow, x1+nLen SAY Space( nWidth-nLen )
   ENDIF

   SetColor( oPane:cClrFil )
   IF oPane:lViewStatus .AND. lCurr
      cDop := Iif( 'D' $ arr[5] .AND. arr[2]==0, "<dir>", Ltrim(Str(arr[2])) ) + " " + hb_Dtoc(arr[3]) + " " + Left(arr[4],5)
      nWidth := oPane:x2 - oPane:x1 - 3 - Len(cDop)
      cText := NameShortcut( Trim( oPane:aDir[nCell+oPane:nShift,1] ), nWidth, "~", lUtf8 )
      @ oPane:y2 - 1, oPane:x1 + 1 SAY cText
      @ oPane:y2 - 1, oPane:x1 + 1 + Len(cText) SAY Space( oPane:x2 - oPane:x1 - 1 - Len(cText) )
      @ oPane:y2 - 1, oPane:x2 - Len(cDop) SAY cDop
      nRow := Iif( oPane == aCount[1,1], 1, 2 )
      @ oPane:y2 - 2, oPane:x1 + Int( (oPane:x2-oPane:x1-15)/2 ) SAY ;
         Ltrim(Str(aCount[nRow,2])) + "/" + Ltrim(Str(aCount[nRow,3])) COLOR oPane:cClrSel
   ENDIF

   RETURN Nil

STATIC FUNCTION _plug_Help( oPane )

   LOCAL nw := 44, y1 := 09, x1 := Int( (Maxcol()-nw)/2 ), y2 := 19
   LOCAL oldc := SetColor( oPane:cClrCurr )
   LOCAL cBufScr := Savescreen( y1, x1, y2, x1+nw )
   LOCAL nRow := Row(), nCol := Col()
   LOCAL lUtf8 := (Lower(oPane:cp) == "utf8")

   @ y1, x1, y2, x1+nw BOX "         "
   @ y1+1, x1+2 SAY NameShortcut( aCount[1,4], 40, "~", lUtf8 )
   @ y1+2, x1+2 SAY NameShortcut( aCount[2,4], 40, "~", lUtf8 )
   @ y1+4, x1+10 SAY "Different files: " + Ltrim(Str(aCount[1,2]+aCount[1,3]+aCount[2,2]))
   @ y1+5, x1+10 SAY Ltrim(Str(aCount[1,2])) + " unique files in 1st dir"
   @ y1+6, x1+10 SAY Ltrim(Str(aCount[2,2])) + " unique files in 2nd dir"
   @ y1+7, x1+10 SAY Ltrim(Str(aCount[1,3])) + " files differs"
   @ y2-1, x1+10 SAY "Press any key..."
   Inkey( 0 )
   Restscreen( y1, x1, y2, x1+nw, cBufScr )
   SetColor( oldc )
   DevPos( nRow, nCol )

   RETURN Nil
