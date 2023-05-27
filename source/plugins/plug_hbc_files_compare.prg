
#define  K_ENTER   13
#define  K_ESC     27
#define  READ_BUFF_LEN 8192

FUNCTION plug_hbc_files_compare( oPane )

   LOCAL i, lUtf8 := FilePane():cp=="utf8", lSele := ( Len( oPane:aSelected ) == 2 )
   LOCAL oPane1 := FilePane():aPanes[1], oPane2 := FilePane():aPanes[2]
   LOCAL y1 := oPane:y1+5, x1 := oPane1:x2-20, y2 := y1+10, x2 := x1+40
   LOCAL cScBuf := Savescreen( y1, x1, y2, x2 )
   LOCAL oldc := SetColor( "N/W"+","+"N/W"+",,"+"N+/W"+","+"N/W" )
   LOCAL aGets, aDir1, aDir2, cFile1, cFile2, handle1, handle2, cBuf1, cBuf2, nRet, nDiffs := 0
   LOCAL aWnd, oNew, nCurr, nAddr := 0
   LOCAL cAppCompare := Iif( hb_hHaskey(FilePane():hMisc,"filecompare"), FilePane():hMisc["filecompare"], Nil )

   IF lSele
      aDir1 := oPane:aDir[oPane:aSelected[1]]
      aDir2 := oPane:aDir[oPane:aSelected[2]]
      cFile1 := oPane:cCurrPath + aDir1[1]
      cFile2 := oPane:cCurrPath + aDir2[1]
   ELSE
      aDir1 := oPane1:aDir[oPane1:nCurrent+oPane1:nShift]
      aDir2 := oPane2:aDir[oPane2:nCurrent+oPane2:nShift]
      cFile1 := oPane1:cCurrPath + aDir1[1]
      cFile2 := oPane2:cCurrPath + aDir2[1]
   ENDIF
   IF 'D' $ aDir1[5] .OR. 'D' $ aDir1[5]
      edi_Alert( "Wrong files selected to compare" )
      RETURN Nil
   ENDIF
   aGets := { ;
      { y1,x1+5, 11, " Compare files " }, ;
      { y1+2,x1+2, 11, NameShortcut( cFile1, 36, "~", lUtf8 ) }, ;
      { y1+3,x1+2, 11, NameShortcut( cFile2, 36, "~", lUtf8 ) }, ;
      { y1+5,x1+2, 11, "( ) as binary" }, { y1+5,x1+3, 3, .F., 2,,,,2 }, ;
      { y1+6,x1+2, 11, "( ) as text" }, { y1+6,x1+3, 3, .T., 2,,,,2 }, ;
      { y1+7,x1+2, 11, Iif(Empty(cAppCompare),"","( ) external app") }, ;
      { y1+7,x1+3, Iif(Empty(cAppCompare),-1,3), .F., 2,,,,2 }, ;
      { y1+9,x1+8, 2, "[Ok]", 6, "N/W","N/W",{||__KeyBoard(Chr(K_ENTER))} }, ;
      { y1+9,x1+18, 2, "[Cancel]", 10, "N/W","N/W",{||__KeyBoard(Chr(K_ESC))} } }

   hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+4, x1 SAY "Ã"
   @ y1+4, x2 SAY "´"
   @ y1+4, x1+1 TO y1+4, x2-1
   @ y1+8, x1 SAY "Ã"
   @ y1+8, x2 SAY "´"
   @ y1+8, x1+1 TO y1+8, x2-1
   hb_cdpSelect( TEdit():aWindows[TEdit():nCurr]:cp )

   i := edi_READ( aGets )
   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cScBuf )
   IF i > 0
      IF aGets[5,4]  // Binary
         IF aDir1[2] != aDir2[2]
            edi_Alert( "Files has differrent sizes" )
            RETURN Nil
         ENDIF
         aWnd := hbc_Wndinit( 05, oPane1:x2-15, 17, oPane1:x2+15,, "Compare" )
         IF Empty( handle1 := hb_vfOpen( cFile1 ) )
            edi_Alert( "Can't open " + cFile1 )
            RETURN Nil
         ENDIF
         IF Empty( handle2 := hb_vfOpen( cFile2 ) )
            hb_vfClose( handle1 )
            edi_Alert( "Can't open " + cFile2 )
            RETURN Nil
         ENDIF
         cBuf1 := Space( READ_BUFF_LEN )
         cBuf2 := Space( READ_BUFF_LEN )
         DO WHILE ( nRet := hb_vfRead( handle1, @cBuf1, READ_BUFF_LEN ) ) > 0
            hb_vfRead( handle2, @cBuf2, READ_BUFF_LEN )
            FOR i := 1 TO nRet
               IF hb_bpeek( cBuf1, i ) != hb_bpeek( cBuf2, i )
                  nDiffs ++
                  IF nDiffs > 10
                     EXIT
                  ENDIF
                  hbc_Wndout( aWnd, hb_NumToHex(nAddr+i-1,8) + ": " + ;
                     hb_NumToHex(hb_bpeek(cBuf1,i),2) + " " + hb_NumToHex(hb_bpeek(cBuf2,i),2) )
               ENDIF
            NEXT
            IF nDiffs > 10
               EXIT
            ENDIF
            nAddr += READ_BUFF_LEN
         ENDDO
         hb_vfClose( handle1 )
         hb_vfClose( handle2 )
         hbc_Wndclose( aWnd, Iif( nDiffs==0, "Files are identical", Iif( nDiffs>10, ;
           "More than 10", Ltrim(Str(nDiffs)) ) + " differences" )  )
      ELSEIF !aGets[7,4]
         cedi_RunApp( cAppCompare + " " + cFile1 + " " + cFile2 )
      ELSE
         nCurr := TEdit():nCurr
         oNew := TEdit():New( Memoread(cFile1), cFile1 )
         IF ( cBuf1 := edi_MakeDiff( oNew, cFile2 ) ) == Nil
            edi_Alert( "Diff tool not found" )
         ELSE
            edi_AddDiff( oNew, cBuf1 )
         ENDIF
         mnu_ToBuf( TEdit():aWindows[nCurr], nCurr+1 )
      ENDIF

   ENDIF

   RETURN Nil
