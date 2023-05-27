
#define  K_ENTER   13
#define  K_ESC     27

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
      { y1+8,x1+2, 11, "[ ] Recursive" }, { y1+7,x1+3, 3, .F., 2,,,,2 }, ;
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
      lRecur := aGets[Len(aGets)-4,4]

      IF lSizOnly .OR. lContent
         arr1 := {}
         arr2 := {}
         dirCompare( arr1, arr2, cDir1, cDir2, lSizOnly, lRecur )
         IF Empty( arr1 ) .AND. Empty( arr2 )
            edi_Alert( "Folders are identical" )
         ELSE
         ENDIF
      ELSE
         cedi_RunApp( cAppCompare + " " + cDir1 + " " + cDir2 )
      ENDIF

   ENDIF

   RETURN Nil

STATIC FUNCTION dirCompare( arr1, arr2, cDir1, cDir2, lSizOnly, lRecur )

   LOCAL aDir1 := hb_vfDirectory( cDir1, "HSD" )
   LOCAL aDir2 := hb_vfDirectory( cDir2, "HSD" )
   LOCAL i, j

   IF !( Right( cDir1,1 ) $ "/\" )
      cDir1 += hb_ps()
   ENDIF
   IF !( Right( cDir2,1 ) $ "/\" )
      cDir2 += hb_ps()
   ENDIF
   FOR i := 1 TO Len( aDir1 )
      IF ( j := Ascan2( aDir2, aDir1[i,1] ) ) > 0
         IF 'D' $ aDir1[i,5]
            IF lRecur
               dirCompare( arr1, arr2, cDir1+aDir1[i,1], cDir2+aDir2[j,1], lSizOnly, lRecur )
            ENDIF
         ELSEIF lSizOnly
            IF aDir1[i,2] != aDir2[j,2]
               AAdd( arr1, { cDir1+aDir1[i,1], 1 } )
               AAdd( arr2, { cDir2+aDir2[j,1], 1 } )
            ENDIF
         ELSE
         ENDIF
         aDir2[j,1] := Nil
      ELSE
         IF 'D' $ aDir1[i,5]
            AAdd( arr1, { cDir1+aDir1[i,1]+hb_ps(), 0 } )
         ELSE
            AAdd( arr1, { cDir1+aDir1[i,1], 0 } )
         ENDIF
      ENDIF
   NEXT
   FOR j := 1 TO Len( aDir2 )
      IF !Empty( aDir2[j,1] )
         IF 'D' $ aDir2[j,5]
            AAdd( arr2, { cDir2+aDir2[j,1]+hb_ps(), 0 } )
         ELSE
            AAdd( arr2, { cDir2+aDir2[j,1], 0 } )
         ENDIF
      ENDIF
   NEXT

   RETURN Nil
