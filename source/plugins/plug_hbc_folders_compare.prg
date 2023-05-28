
#define  K_ENTER   13
#define  K_ESC     27
#define  READ_BUFF_LEN 8192

FUNCTION plug_hbc_folders_compare( oPane )

   LOCAL i, lUtf8 := FilePane():cp=="utf8", aGets
   LOCAL oPane1 := FilePane():aPanes[1], oPane2 := FilePane():aPanes[2]
   LOCAL y1 := oPane:y1+5, x1 := oPane1:x2-20, y2 := y1+12, x2 := x1+40
   LOCAL cScBuf := Savescreen( y1, x1, y2, x2 )
   LOCAL oldc := SetColor( "N/W"+","+"N/W"+",,"+"N+/W"+","+"N/W" )
   LOCAL cDir1, cDir2, arr1, arr2, lRecur, lSizOnly, lContent
   LOCAL cAppCompare := Iif( hb_hHaskey(FilePane():hMisc,"foldcompare"), FilePane():hMisc["foldcompare"], Nil )

   aGets := { ;
      { y1,x1+5, 11, " Compare folders " }, ;
      { y1+1,x1+2, 11, NameShortcut( oPane1:cCurrPath, 36, "~", lUtf8 ) }, ;
      { y1+2,x1+2, 11, NameShortcut( oPane2:cCurrPath, 36, "~", lUtf8 ) }, ;
      { y1+4,x1+2, 11, "( ) by file dates and sizes" }, { y1+4,x1+3, 3, .T., 2,,,,2 }, ;
      { y1+5,x1+2, 11, "( ) by file content" }, { y1+5,x1+3, 3, .F., 2,,,,2 }, ;
      { y1+6,x1+2, 11, Iif(Empty(cAppCompare),"","( ) external app") }, { y1+6,x1+3, Iif(Empty(cAppCompare),-1,3), .F., 2,,,,2 }, ;
      { y1+8,x1+2, 11, "[ ] Recursive" }, { y1+8,x1+3, 1, .F., 2 }, ;
      { y1+11,x1+5, 2, "[Ok]", 6, "N/W","N/W",{||__KeyBoard(Chr(K_ENTER))} }, ;
      { y1+11,x1+15, 2, "[Cancel]", 10, "N/W","N/W",{||__KeyBoard(Chr(K_ESC))} } }

   hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+3, x1 SAY "Ã"
   @ y1+3, x2 SAY "´"
   @ y1+3, x1+1 TO y1+3, x2-1
   @ y1+7, x1 SAY "Ã"
   @ y1+7, x2 SAY "´"
   @ y1+7, x1+1 TO y1+7, x2-1
   hb_cdpSelect( TEdit():aWindows[TEdit():nCurr]:cp )

   i := edi_READ( aGets )
   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cScBuf )
   IF i > 0
      cDir1 := oPane1:cCurrPath
      cDir2 := oPane2:cCurrPath
      lSizOnly := aGets[5,4]
      lContent := aGets[7,4]
      lRecur := aGets[Len(aGets)-2,4]

      IF lSizOnly .OR. lContent
         arr1 := {}
         arr2 := {}
         dirCompare( arr1, arr2, cDir1, cDir2, "", "", lSizOnly, lRecur )
         IF Empty( arr1 ) .AND. Empty( arr2 )
            edi_Alert( "Folders are identical" )
         ELSE
            hb_AIns( arr1, 1, { "..","","","","D" }, .T. )
            oPane1:nPanelMod := 1
            oPane1:aDir := arr1
            oPane1:cIOpref_bak := oPane1:cIOpref
            oPane1:net_cAddress_bak := oPane1:net_cAddress
            oPane1:cIOpref := "sea:"
            oPane1:bDrawCell := {|o,n,l|DrawCell(o,n,l)}
            oPane1:Refresh( .T. )
            oPane1:Draw()
            oPane1:DrawCell( ,.T. )
            oPane1:DrawHead( .T. )

            hb_AIns( arr2, 1, { "..","","","","D" }, .T. )
            oPane2:nPanelMod := 1
            oPane2:aDir := arr2
            oPane2:cIOpref_bak := oPane2:cIOpref
            oPane2:net_cAddress_bak := oPane2:net_cAddress
            oPane2:cIOpref := "sea:"
            oPane2:bDrawCell := {|o,n,l|DrawCell(o,n,l)}
            oPane2:Refresh( .T. )
            oPane2:Draw()
            oPane2:DrawCell( ,.T. )
            oPane2:DrawHead( .T. )

         ENDIF
      ELSE
         cedi_RunApp( cAppCompare + " " + cDir1 + " " + cDir2 )
      ENDIF

   ENDIF

   RETURN Nil

STATIC FUNCTION dirCompare( arr1, arr2, cDir1, cDir2, cRel1, cRel2, lSizOnly, lRecur )

   LOCAL aDir1, aDir2
   LOCAL i, j, ps := hb_ps()

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
               dirCompare( arr1, arr2, cDir1+aDir1[i,1], cDir2+aDir2[j,1], cRel1+aDir1[i,1]+ps, cRel2+aDir2[j,1]+ps, lSizOnly, lRecur )
            ENDIF
         ELSE  //IF lSizOnly
            IF aDir1[i,2] != aDir2[j,2] .OR. ( aDir1[i,3] != aDir2[j,3] .AND. ;
               ( lSizOnly .OR. !fileCompare( cDir1+aDir1[i,1], cDir2+aDir2[j,1] ) ) )
               AAdd( arr1, { cRel1+aDir1[i,1], aDir1[i,2], aDir1[i,3], aDir1[i,4], aDir1[i,5], 1 } )
               AAdd( arr2, { cRel2+aDir2[j,1], aDir2[j,2], aDir2[j,3], aDir2[j,4], aDir2[j,5], 1 } )
            ENDIF
         ENDIF
         aDir2[j,1] := Nil
      ELSE
         IF 'D' $ aDir1[i,5]
            AAdd( arr1, { cRel1+aDir1[i,1]+ps, aDir1[i,2], aDir1[i,3], aDir1[i,4], aDir1[i,5] , 0 } )
         ELSE
            AAdd( arr1, { cRel1+aDir1[i,1], aDir1[i,2], aDir1[i,3], aDir1[i,4], aDir1[i,5], 0 } )
         ENDIF
      ENDIF
   NEXT
   FOR j := 1 TO Len( aDir2 )
      IF !Empty( aDir2[j,1] )
         IF 'D' $ aDir2[j,5]
            IF !(aDir2[j,1] == "..") .AND. !(aDir2[j,1] == ".")
               AAdd( arr2, { cRel2+aDir2[j,1]+ps, aDir2[j,2], aDir2[j,3], aDir2[j,4], aDir2[j,5], 0 } )
            ENDIF
         ELSE
            AAdd( arr2, { cRel2+aDir2[j,1], aDir2[j,2], aDir2[j,3], aDir2[j,4], aDir2[j,5], 0 } )
         ENDIF
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION fileCompare( cFile1, cFile2 )

   LOCAL handle1, handle2, cBuf1, cBuf2, i, nRet, lRes := .T.

   IF Empty( handle1 := hb_vfOpen( cFile1 ) )
      RETURN .F.
   ENDIF
   IF Empty( handle2 := hb_vfOpen( cFile2 ) )
      hb_vfClose( handle1 )
      RETURN .F.
   ENDIF
   cBuf1 := Space( READ_BUFF_LEN )
   cBuf2 := Space( READ_BUFF_LEN )
   DO WHILE lRes .AND. ( nRet := hb_vfRead( handle1, @cBuf1, READ_BUFF_LEN ) ) > 0
      hb_vfRead( handle2, @cBuf2, READ_BUFF_LEN )
      FOR i := 1 TO nRet
         IF hb_bpeek( cBuf1, i ) != hb_bpeek( cBuf2, i )
            lRes := .F.
            EXIT
         ENDIF
      NEXT
   ENDDO
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
   ENDIF

   RETURN Nil
