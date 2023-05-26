
#define  K_ENTER   13
#define  K_ESC     27

FUNCTION plug_hbc_folders_compare( oPane )

   LOCAL i, lUtf8 := FilePane():cp=="utf8"
   LOCAL oPane1 := FilePane():aPanes[1], oPane2 := FilePane():aPanes[2]
   LOCAL y1 := oPane:y1+5, x1 := oPane1:x2-20, y2 := y1+12, x2 := x1+40
   LOCAL l2 := ('D' $ oPane1:aDir[oPane1:nCurrent+oPane1:nShift,5]) .AND. ;
      ('D' $ oPane2:aDir[oPane2:nCurrent+oPane2:nShift,5]) .AND. ;
      !(oPane1:aDir[oPane1:nCurrent+oPane1:nShift,1]=="..") .AND. ;
      !(oPane2:aDir[oPane2:nCurrent+oPane2:nShift,1]=="..")
   LOCAL cScBuf := Savescreen( y1, x1, y2, x2 )
   LOCAL oldc := SetColor( "N/W"+","+"N/W"+",,"+"N+/W"+","+"N/W" )
   LOCAL aGets := { ;
      { y1,x1+5, 11, " Compare folders " }, ;
      { y1+1,x1+2, 11, "( )" }, { y1+1,x1+3, 3, .T., 2,,,,1 }, ;
      { y1+1,x1+6, 11, NameShortcut( oPane1:cCurrPath, 32, "~", lUtf8 ) }, ;
      { y1+2,x1+6, 11, NameShortcut( oPane2:cCurrPath, 32, "~", lUtf8 ) }, ;
      { y1+3,x1+2, 11, Iif(l2,"( )","") }, { y1+3,x1+3, Iif(l2,3,-1), .F., 2,,,,1 }, ;
      { y1+3,x1+6, 11, Iif(l2,NameShortcut( oPane1:cCurrPath+oPane1:aDir[oPane1:nCurrent+oPane1:nShift,1], 32, "~", lUtf8 ),"") }, ;
      { y1+4,x1+6, 11, Iif(l2,NameShortcut( oPane2:cCurrPath+oPane2:aDir[oPane2:nCurrent+oPane2:nShift,1], 32, "~", lUtf8 ),"") }, ;
      { y1+6,x1+2, 11, "( ) by file dates and sizes" }, { y1+6,x1+3, 3, .T., 2,,,,2 }, ;
      { y1+7,x1+2, 11, "( ) by file content" }, { y1+7,x1+3, 3, .F., 2,,,,2 }, ;
      { y1+9,x1+2, 11, "[ ] Recursive" }, { y1+9,x1+3, 3, .F., 2,,,,2 }, ;
      { y1+11,x1+5, 2, "[Ok]", 6, "N/W","N/W",{||__KeyBoard(Chr(K_ENTER))} }, ;
      { y1+11,x1+15, 2, "[Cancel]", 10, "N/W","N/W",{||__KeyBoard(Chr(K_ESC))} } }
   LOCAL cDir1, cDir2, aDir1, aDir2, lRecur, lSizOnly

   //hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+5, x1 SAY "Ã"
   @ y1+5, x2 SAY "´"
   @ y1+5, x1+1 TO y1+5, x2-1
   @ y1+10, x1 SAY "Ã"
   @ y1+10, x2 SAY "´"
   @ y1+10, x1+1 TO y1+10, x2-1
   //hb_cdpSelect( oEdit:cp )

   i := edi_READ( aGets )
   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cScBuf )
   IF i > 0
      IF aGets[3,4]
         cDir1 := oPane1:cCurrPath
         cDir2 := oPane2:cCurrPath
      ELSE
         cDir1 := oPane1:cCurrPath + oPane1:aDir[oPane1:nCurrent+oPane1:nShift,1]
         cDir2 := oPane2:cCurrPath + oPane1:aDir[oPane2:nCurrent+oPane2:nShift,1]
      ENDIF
      aDir1 := hb_vfDirectory( cDir1, "HSD" )
      aDir2 := hb_vfDirectory( cDir2, "HSD" )
      lSizOnly := aGets[11,4]
      lRecur := aGets[15,4]

   ENDIF

   RETURN Nil
