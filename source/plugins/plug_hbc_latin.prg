
#define  K_ENTER   13
#define  K_ESC     27

FUNCTION plug_hbc_latin( oPane )

   LOCAL y1 := oPane:y1+5, x1 := oPane:x1+5, y2 := y1+12, x2 := x1+30
   LOCAL aFiles, cIn, cOut, sLog := "", arr := {}, i
   LOCAL cTrnl := " ¡¢£¤¥¦§¨©ª«¬­®¯àáâãäåæçèéêëìíîï"
   LOCAL cTrnu := "€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ"
   LOCAL aTranslL := { "a", "b", "v", "g", "d", "e", "zh", "z", "i", "j", "k", "l", "m", "n", "o", "p", "r", "s", "t", "u", "f", "kh", "c", "ch", "sh", "shch", "_", "y", "_", "e", "ju", "ja" }
   LOCAL aTranslU := { "A", "B", "V", "G", "D", "E", "ZH", "Z", "I", "J", "K", "L", "M", "N", "O", "P", "R", "S", "T", "U", "F", "Kh", "C", "Ch", "Sh", "Shch", "_", "Y", "_", "E", "Ju", "Ja" }

   LOCAL cScBuf := Savescreen( y1, x1, y2, x2 )
   LOCAL oldc := SetColor( "N/W"+","+"N/W"+",,"+"N+/W"+","+"N/W" )
   LOCAL aGets := { ;
      { y1+1,x1+5, 11, "Convert file names" }, ;
      { y1+3,x1+2, 11, "( ) from cyrillic to latin" }, { y1+3,x1+3, 3, .T., 2,,,,1 }, ;
      { y1+4,x1+2, 11, "( ) to lower case" }, { y1+4,x1+3, 3, .F., 2,,,,1 }, ;
      { y1+5,x1+2, 11, "( ) to title case" }, { y1+5,x1+3, 3, .F., 2,,,,1 }, ;
      { y1+7,x1+2, 11, "( ) Current" }, { y1+7,x1+3, 3, .T., 2,,,,2 }, ;
      { y1+8,x1+2, 11, "( ) Selected" }, { y1+8,x1+3, 3, .F., 2,,,,2 }, ;
      { y1+9,x1+2, 11, "( ) All" }, { y1+9,x1+3, 3, .F., 2,,,,2 }, ;
      { y1+11,x1+5, 2, "[Ok]", 6, "N/W","N/W",{||__KeyBoard(Chr(K_ENTER))} }, ;
      { y1+11,x1+15, 2, "[Cancel]", 10, "N/W","N/W",{||__KeyBoard(Chr(K_ESC))} } }

   //hb_cdpSelect( "RU866" )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "
   @ y1+10, x1 SAY "Ã"
   @ y1+10, x2 SAY "´"
   @ y1+10, x1+1 TO y1+10, x2-1
   //hb_cdpSelect( oEdit:cp )

   i := edi_READ( aGets )
   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cScBuf )
   IF i > 0
      IF aGets[9,4]
         aFiles := { oPane:aDir[oPane:nCurrent + oPane:nShift,1] }
      ELSEIF aGets[11,4]
         aFiles := {}
         FOR i := 1 TO Len( oPane:aSelected )
            Aadd( aFiles, oPane:aDir[oPane:aSelected[i],1] )
         NEXT
      ELSE
         aFiles := Directory( "./*.*" )
         FOR i := 1 TO Len( aFiles )
            aFiles[i] := aFiles[i,1]
         NEXT
      ENDIF

      i := 0
      DO WHILE ++ i <= Len( aFiles )
         cIn := aFiles[i]
         IF aGets[3,4]
            cOut := hb_strReplace( cIn, cTrnl, aTranslL )
            cOut := hb_strReplace( cOut, cTrnu, aTranslU )
            IF ' ' $ cOut
               cOut := StrTran( cOut, ' ', '_' )
            ENDIF
            IF '-' $ cOut
               cOut := StrTran( cOut, '-', '_' )
            ENDIF
         ELSEIF aGets[5,4]
            cOut := Lower( cIn )
         ELSE
            cOut := Upper( Left(cIn,1) ) + Lower( Substr(cIn,2) )
         ENDIF
         IF !Empty( cOut ) .AND. !( cIn == cOut )
            Aadd( arr, cIn + "  --> " + cOut )
            FRename( cIn, cOut )
         ENDIF
      ENDDO
      IF Empty( arr )
         edi_Alert( "No files to convert..." )
      ELSE
         arr := hb_AIns( arr, 1, "Done! " + Ltrim(Str(Len(arr))) + " files:", .T. )
         FMenu( oPane, arr, y1, x1 )
         oPane:Refresh()
         oPane:Draw()
         oPane:DrawCell( ,.T. )
      ENDIF
   ENDIF

   RETURN Nil
