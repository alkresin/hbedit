/*
 * HbEdit plugin - aiTutor support
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ENTER  13

STATIC cIniPath
STATIC oEd
STATIC lShowHelp := .T.
STATIC aTutor, cTutorCurr, cLang, aTrace
STATIC aLangs := { "harbour", "python", "c", "c++", "go", "java", "php", "other" }
STATIC aExt := { ".prg", ".py", ".c", ".cpp", ".go", ".java", ".php", "" }
STATIC cn := e"\n"

FUNCTION plug_itutor( oEdit, cPath )

   LOCAL i, s

   oEd := oEdit
   cIniPath := cPath

   IF Empty( aTutor ) .AND. !_itu_SeleBook()
      RETURN Nil
   ENDIF

   IF Empty( aTutor )
      edi_Alert( "No info found" )
      RETURN Nil
   ENDIF

   IF !_itu_Show( aTutor, 0 )
      aTrace := {}
   ENDIF

   RETURN Nil

STATIC FUNCTION _itu_Show( arr, nLevel )

   LOCAL nDop := Iif( nLevel==0, 2, 0 ), oNew, cRes, lLoop := .F., cpold
   LOCAL aMenu, i, j, nCurr
   LOCAL bKeys := {|nKeyExt, nRow|
      IF nKeyExt == 0x41000001  // F1
         lShowHelp := .T.
         lLoop := .T.
         RETURN .F.
      ELSEIF nKeyExt == 0x41000007  // F7
         _itu_Zip()
         lLoop := .T.
         RETURN .F.
      ELSEIF nRow > nDop
         IF nKeyExt == 0x41000015   // Ins
            IF !_itu_Ins( arr, nRow-nDop )
               RETURN .T.
            ENDIF
            lLoop := .T.
            RETURN .F.
         ELSEIF nKeyExt == 0x41000016  // Del
            _itu_Del( arr, nRow-nDop )
            lLoop := .T.
            RETURN .F.
         ELSEIF nKeyExt == 0x41000003  // F3
            IF Valtype( arr[nRow-nDop,2] ) == "A"
               _itu_Comment( arr, nRow-nDop )
            ENDIF
            lLoop := .T.
            RETURN .F.
         ELSEIF nKeyExt == 0x41000004  // F4
            _itu_Rename( arr, nRow-nDop )
            lLoop := .T.
            RETURN .F.
         ENDIF
      ENDIF
      RETURN .T.
   }

   DO WHILE .T.
      aMenu := Array( Len(arr)+nDop )
      IF nDop > 0
         aMenu[1] := { "== " + cTutorCurr + " ==" ,, }
         aMenu[2] := { "---",, }
      ENDIF
      FOR i := 1 TO Len( arr )
         IF Valtype( arr[i,2] ) == "A".AND. !Empty( arr[i,2] )
            aMenu[i+nDop] := { arr[i,1],,, ">" }
         ELSE
            aMenu[i+nDop] := { arr[i,1],, }
         ENDIF
      NEXT
      IF lShowHelp
         edi_Alert( "iTutor plugin hotkeys:;F1 - Show this screen;Ins - Insert chapter or module near current position;Del - Delete chapter or module;F3 - View/edit chapter's comments;F4 - Rename chapter or module;F7 - zip;Esc - Level up;F10 - Exit" )
         lShowHelp := .F.
      ENDIF
      lLoop := .F.
      IF !Empty(aTrace)
         nCurr := aTrace[1]
         hb_ADel( aTrace, 1, .T. )
         KEYBOARD Chr(K_ENTER)
      ELSE
         nCurr := Nil
      ENDIF
      cpOld := hb_cdpSelect( "UTF8" )
      i := FMenu( oEd, aMenu, oEd:y1+2, oEd:x1+4,,,,, nCurr,,,, bKeys )
      hb_cdpSelect( cpold )
      IF i == 0
         IF lLoop
            lLoop := .F.
            LOOP
         ELSEIF Lastkey() == -9
            aTrace := {}
            RETURN .T.
         ELSE
            RETURN .F.
         ENDIF
      ENDIF
      lLoop := .F.
      IF nDop > 0
         IF i == 1
            _itu_SeleBook()
            arr := aTutor
            LOOP
         ELSE
            i -= nDop
         ENDIF
      ENDIF
      IF Valtype( arr[i,2] ) == "A"
         IF !Empty( arr[i,2] )
            IF _itu_Show( arr[i,2], nLevel+1 )
               hb_AIns( aTrace, 1, i+nDop, .T. )
               RETURN .T.
            ENDIF
         ELSE
            _itu_Ins( arr[i,2], 1, .T. )
         ENDIF
      ELSE
         oNew := TEdit():aWindows[TEdit():nCurr]
         cRes := "$" + arr[i,1] + Iif( (j:=Ascan(aLangs,cLang))>0,aExt[j],"" )
         IF __ObjHasMsg( oNew, "hcargo" ) .AND. !Empty( oNew:hCargo ) .AND. ;
            hb_hHaskey( oNew:hCargo, "itutor_arr" )
            oNew:Settext( arr[i,2], cRes )
            oNew:lUpdated := .F.
         ELSE
            oNew := mnu_NewBuf( oEd, cRes, arr[i,2], @_itu_Save() )
            oNew:cp := "UTF8"
            hb_cdpSelect( oNew:cp )
            oNew:lUtf8 := .T.
            IF Empty( oNew:hCargo )
               oNew:hCargo := hb_hash()
            ENDIF
         ENDIF
         oNew:hCargo["itutor_arr"] := arr[i]
         aTrace := {}
         RETURN .T.
      ENDIF
   ENDDO

   RETURN .T.

STATIC FUNCTION _itu_Ins( arr, nRow, lToEmpty )

   LOCAL i, cName, cText, lComm := .F., lMod := .F., arr1, aMenu

   IF !Empty( lToEmpty )
      IF ( i := FMenu( oEd, {"Insert module to", "Insert chapter to"}, oEd:y1+2, oEd:x1+4 ) ) == 0
         RETURN .F.
      ENDIF
      IF i == 2
         i := 3
      ENDIF
   ELSE
      IF ( i := FMenu( oEd, {"Insert module before", "Insert module after", ;
         "Insert chapter before", "Insert chapter after"}, oEd:y1+2, oEd:x1+4 ) ) == 0
         RETURN .F.
      ENDIF
   ENDIF
   // Get the name of module or chapter
   IF Empty( cName := edi_MsgGet( "Name:", oEd:y1+8, oEd:x1+24, oEd:x2-24,,, "UTF8" ) )
      RETURN .F.
   ENDIF

   IF i < 3
      // If module, get the code
      IF Empty( cText := edi_MsgGet_ext( "", oEd:y1+2, oEd:x1+16, oEd:y1+18, oEd:x2-16, "UTF8", .T. ) )
         RETURN .F.
      ENDIF
      hb_AIns( arr, nRow + i-1, { cName, cText, ">" }, .T. )
   ELSE
      // If chapter, add it and request a comment and module for it
      hb_AIns( arr, nRow + i-3, arr1 := { cName, {} }, .T. )
      DO WHILE !lMod .OR. !lComm
         aMenu := {}
         IF !lComm
            AAdd( aMenu, "Add comment" )
         ENDIF
         IF !lMod
            AAdd( aMenu, "Add module" )
         ENDIF
         IF ( i := FMenu( oEd, aMenu, oEd:y1+2, oEd:x1+4 ) ) == 0
            EXIT
         ELSEIF i == 1 .AND. !lComm
            IF !Empty( cText := edi_MsgGet_ext( "", oEd:y1+2, oEd:x1+16, oEd:y1+18, oEd:x2-16, "UTF8", .T. ) )
               AAdd( arr1, cText )
               lComm := .T.
            ENDIF
         ELSE
            IF !Empty( cName := edi_MsgGet( "Name:", oEd:y1+8, oEd:x1+24, oEd:x2-24,,, "UTF8" ) )
               IF !Empty( cText := edi_MsgGet_ext( "", oEd:y1+2, oEd:x1+16, oEd:y1+18, oEd:x2-16, "UTF8", .T. ) )
                  AAdd( arr1[2], { cName, cText, ">" } )
                  lMod := .T.
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   IF edi_Alert( "Really update the book?", "Yes", "No" ) == 1
      _itu_WriteBook()
   ENDIF

   RETURN .T.

STATIC FUNCTION _itu_Del( arr, nRow )

   IF Valtype( arr[nRow,2] ) == "A" .AND. !Empty( arr[nRow,2] )
      edi_Alert( "The Chapter isn't empty!" )
      RETURN .T.
   ENDIF
   IF edi_Alert( "Really update the book?", "Yes", "No" ) == 1
      hb_ADel( arr, nRow, .T. )
      _itu_WriteBook()
   ENDIF

   RETURN .T.

STATIC FUNCTION _itu_Comment( arr, nRow )

   LOCAL cComm := Iif( Len(arr[nRow]) > 2, arr[nRow,3], "" ), cRes

   cRes := edi_MsgGet_ext( cComm, oEd:y1+2, oEd:x1+16, oEd:y1+16, oEd:x2-16, "UTF8", .T. )
   IF !Empty( cRes ) .AND. !( cRes == cComm ) .AND. ;
      edi_Alert( "Really update the book?", "Yes", "No" ) == 1
      arr[nRow,3] := cRes
      _itu_WriteBook()
   ENDIF

   RETURN .T.

STATIC FUNCTION _itu_Rename( arr, nRow )

   LOCAL cRes := edi_MsgGet( "Change title:", oEd:y1+8, oEd:x1+24, oEd:x2-24,, arr[nRow,1], "UTF8" )

   IF !Empty( cRes ) .AND. edi_Alert( "Really update the book?", "Yes", "No" ) == 1
      arr[nRow,1] := cRes
      _itu_WriteBook()
   ENDIF

   RETURN .T.

FUNCTION _itu_Save( cFileName, cText, oEdit )

   LOCAL arr := oEdit:hCargo["itutor_arr"], dDateMod, cTimeMod, nPos

   IF ( nPos := At( "$", cFileName ) ) > 0 .AND. Substr( cFileName,nPos+1 ) == arr[1] + hb_fnameExt( cFileName )
      IF edi_Alert( "Really update the book?", "Yes", "No" ) == 1
         arr[2] := cText
         _itu_WriteBook()
      ENDIF
   ELSE
      hb_MemoWrit( cFileName, cText )
      IF hb_fGetDateTime( cFileName, @dDateMod, @cTimeMod )
         oEdit:dDateMod := dDateMod
         oEdit:cTimeMod := cTimeMod
      ENDIF
      oEdit:lUpdated := .F.
   ENDIF

   RETURN Nil

STATIC FUNCTION _itu_WriteBook()

   LOCAL cFile := cIniPath + "itutor" + hb_ps() + cTutorCurr
   LOCAL cFileNew := cFile + ".new", han, i
   LOCAL cFileBack := cFile + ".bak"

   IF Right( cTutorCurr,4 ) == ".zip"
      cFile := hb_strShrink( cFile, 4 )
   ENDIF
   IF (han := FCreate( cFileNew )) == -1
      edi_Alert( "Can't create " + cFileNew )
   ENDIF
   FWrite( han, '<?xml version="1.0" encoding="UTF-8"?>' + cn + '<init lang="' + ;
      cLang + '">' + cn )

   FOR i := 1 TO Len( aTutor )
      IF Valtype( aTutor[i,2] ) == "A"
         _itu_WriteChapter( han, aTutor[i], 1 )
      ELSE
         FWrite( han, Space( 2 ) + '<module name="' + aTutor[i,1] + '"' + aTutor[i,3] + cn + ;
         Space( 4 ) + '<![CDATA[' + aTutor[i,2] + ']]>' + cn + Space( 2 ) + '</module>' + cn )
      ENDIF
   NEXT
   FWrite( han, '</init>' )
   FClose( han )

   FErase( cFileBack )
   IF !File( cFile ) .OR. FRename( cFile, cFileBack ) == 0
      FRename( cFileNew, cFile )
   ELSE
      edi_Alert( "Can't rename " + cFile + " to " + cFileBack )
   ENDIF

   RETURN Nil

STATIC FUNCTION _itu_WriteChapter( han, arr, nLevel )

   LOCAL i

   FWrite( han, Space( nLevel*2 ) + '<chapter name="' + arr[1] + '" >' + cn )
   IF Len( arr ) > 2 .AND. !Empty( arr[3] )
      FWrite( han, Space( nLevel*2 + 2 ) + '<comment>' + cn + ;
         Space( nLevel*2 + 4 ) + '<![CDATA[' + arr[3] + ']]>' + cn + ;
         Space( nLevel*2 + 2 ) + '</comment>' + cn )
   ENDIF
   FOR i := 1 TO Len( arr[2] )
      IF Valtype( arr[2,i,2] ) == "A"
         _itu_WriteChapter( han, arr[2,i], nLevel+1 )
      ELSE
         FWrite( han, Space( nLevel*2 + 2 ) + '<module name="' + arr[2,i,1] + '"' + arr[2,i,3] + cn + ;
         Space( nLevel*2 + 4 ) + '<![CDATA[' + arr[2,i,2] + ']]>' + cn + ;
         Space( nLevel*2 + 2 ) + '</module>' + cn )
      ENDIF
   NEXT
   FWrite( han, Space( nLevel*2 ) + '</chapter>' + cn )

   RETURN Nil

STATIC FUNCTION _itu_SeleBook()

   LOCAL aDir1 := Directory( cIniPath + "itutor" + hb_ps() + "*.xml" )
   LOCAL aDir2 := Directory( cIniPath + "itutor" + hb_ps() + "*.xml.zip" )
   LOCAL i, aMenu := Array( Len(aDir1) + Len(aDir2) )

   FOR i := 1 TO Len( aDir1 )
      aMenu[i] := aDir1[i,1]
   NEXT
   FOR i := 1 TO Len( aDir2 )
      aMenu[i+Len(aDir1)] := aDir2[i,1]
   NEXT
   ASort( aMenu )
   hb_AIns( aMenu, 1, "---", .T. )
   hb_AIns( aMenu, 1, "Create new", .T. )

   IF ( i := FMenu( oEd, aMenu, oEd:y1+2, oEd:x1+4 ) ) == 0
      RETURN .F.
   ENDIF

   IF i == 1
      _itu_Create()
      RETURN .T.
   ENDIF
   cTutorCurr := aMenu[i]
   _itu_Load()

   RETURN .T.

STATIC FUNCTION _itu_Create()

   LOCAL cFile := edi_MsgGet( "File name:", oEd:y1+6, oEd:x1+24, oEd:x2-24 )
   LOCAL i, s

   IF Empty( cFile )
      RETURN Nil
   ENDIF
   IF ( i := FMenu( oEd, aLangs, oEd:y1+6, oEd:x1+24 ) ) == 0
      RETURN Nil
   ENDIF

   cLang := aLangs[i]
   cTutorCurr := hb_fnameExtSet( cFile, ".xml" )
   aTutor := { {"Introduction", {}, "Start"} }
   _itu_WriteBook()

   RETURN Nil

STATIC FUNCTION _itu_Load()

   LOCAL cBuff, hUnzip, nSize := 64000
   LOCAL nPos := 0, nPos2, nPos3, c

   IF Right( cTutorCurr,4 ) == ".zip"
      IF !Empty( hUnzip := hb_unzipOpen( cIniPath + "itutor" + hb_ps() + cTutorCurr ) )
         hb_unzipFileFirst( hUnzip )
         IF hb_unzipFileOpen( hUnzip, Nil ) == 0
            cBuff := Space( nSize )
            hb_unzipFileRead( hUnzip, @cBuff )
            hb_unzipFileClose( hUnzip )
            //hb_memoWrit(cIniPath + "itutor" + hb_ps() + "aaa.aaa", cBuff)
         ENDIF
         hb_unzipClose( hUnzip )
      ELSE
         edi_Alert( "No " + cIniPath + "itutor" + hb_ps() + cTutorCurr )
      ENDIF
   ELSE
      cBuff := Memoread( cIniPath + "itutor" + hb_ps() + cTutorCurr )
   ENDIF
   aTutor := {}
   IF ( nPos := At( "<init", cBuff ) ) == 0
      edi_Writelog( "Error in tutor 1" )
      RETURN Nil
   ENDIF
   IF ( cLang := _itu_GetAttr( cBuff, nPos, "lang" ) ) == Nil
      edi_Writelog( "Error in tutor 2" )
      RETURN Nil
   ENDIF

   DO WHILE .T.
      nPos2 := hb_At( "<chapter", cBuff, nPos )
      nPos3 := hb_At( "<module", cBuff, nPos )
      IF nPos2 == 0 .AND. nPos3 == 0
         EXIT
      ENDIF
      nPos := Iif( nPos3 < nPos2 .AND. nPos3 > 0 , nPos3, Iif(nPos2>0,nPos2,nPos3) )
      //edi_writelog( "1: " + str(nPos) + " " + str(npos2) + " " + str(npos3) )
      IF ( nPos == nPos2 .AND. ( nPos2 := _itu_AddChapter( cBuff, nPos, aTutor ) ) == -1 ) ;
          .OR. ( nPos == nPos3 .AND. ( nPos2 := _itu_AddModule( cBuff, nPos, aTutor ) ) == -1 )
         //edi_writelog( "2: " + str(nPos) + " " + str(npos2) + " " + str(npos3) )
         edi_Alert( "Wrong tutor file" )
         aTutor := {}
         RETURN Nil
      ENDIF
      //edi_writelog( "3: " + str(nPos) + " " + str(npos2) + " " + str(npos3) )
      nPos := nPos2
      nPos ++
   ENDDO

   RETURN Nil

STATIC FUNCTION _itu_AddChapter( cBuff, nPos, arr )

   LOCAL c, nPos2, arr1, lFirst := .T., cTemp

   IF ( c := _itu_GetAttr( cBuff, nPos, "name" ) ) == Nil
      RETURN -1
   ENDIF
   // add chapter array { cName, {} }
   AAdd( arr, { c, arr1 := {} } )
   nPos += 12
   DO WHILE ( nPos := hb_At( "<", cBuff, nPos ) ) > 0
      nPos ++
      IF ( c := Substr( cBuff, nPos, 1 ) ) == "/"
         nPos ++
         IF ( c := Substr( cBuff, nPos, 1 ) ) == "c"
            IF Substr( cBuff, nPos, 7 ) == "chapter"
               RETURN nPos + 7
            ENDIF
         ENDIF
      ELSEIF c == "m"
         IF Substr( cBuff, nPos, 6 ) == "module"
            IF ( nPos := _itu_AddModule( cBuff, nPos, arr1 ) ) == -1
               RETURN -1
            ENDIF
         ENDIF
      ELSEIF c == "c"
         IF Substr( cBuff, nPos, 7 ) == "chapter"
            lFirst := .F.
            IF ( nPos2 := _itu_AddChapter( cBuff, nPos, arr1 ) ) == -1
               RETURN -1
            ENDIF
            nPos := nPos2
         ELSEIF Substr( cBuff, nPos, 7 ) == "comment" .AND. lFirst
            lFirst := .F.
            IF ( nPos2 := hb_At( "</comment", cBuff, nPos ) ) == 0
               edi_Writelog( "Error in tutor 3:" + Chr(10) + Substr( cBuff, nPos-10, 40 ) )
               RETURN -1
            ENDIF
            IF ( nPos := hb_At( "<![CDATA[", cBuff, nPos ) ) < nPos2 .AND. ;
               ( nPos2 := hb_Rat( "]]>", cBuff, nPos, nPos2 ) ) > 0
               nPos += 9
               AAdd( ATail(arr), Substr( cBuff, nPos, nPos2-nPos ) )
            ENDIF
            nPos := nPos2 + 7
         ENDIF
      ENDIF

   ENDDO

   RETURN nPos

STATIC FUNCTION _itu_AddModule( cBuff, nPos, arr )

    LOCAL nPos2, cTemp, c

    IF ( nPos2 := hb_At( "</module", cBuff, nPos ) ) == 0
       edi_Writelog( "Error in tutor 3:" + Chr(10) + Substr( cBuff, nPos-10, 40 ) )
       RETURN -1
    ENDIF
    IF ( c := _itu_GetAttr( cBuff, nPos, "name", @cTemp ) ) == Nil
       RETURN -1
    ENDIF
    IF ( nPos := hb_At( "<![CDATA[", cBuff, nPos ) ) < nPos2 .AND. ;
       ( nPos2 := hb_Rat( "]]>", cBuff, nPos, nPos2 ) ) > 0
       nPos += 9
       AAdd( arr, { c,Substr( cBuff, nPos, nPos2-nPos ), cTemp } )
    ELSE
       AAdd( arr, { c,"", cTemp } )
    ENDIF
    nPos := nPos2 + 7

    RETURN nPos

STATIC FUNCTION _itu_GetAttr( cBuff, nPos, cAttr, cRest )

   LOCAL nPos2, nPos3

   // Find chapter name
   nPos3 := hb_At( ">", cBuff, nPos )
   IF ( nPos2 := hb_At( cAttr, cBuff, nPos ) ) > 0
      nPos2 := nPos2 + Len(cAttr)
      DO WHILE Substr( cBuff, nPos2, 1 ) == ' '; nPos2++; ENDDO
   ENDIF
   IF nPos2 == 0 .OR. Substr( cBuff, nPos2, 1 ) != "=" .OR. nPos2 > nPos3
      edi_Writelog( "Error in tutor 4:" + Chr(10) + Substr( cBuff, nPos-10, 40 ) )
      RETURN Nil
   ENDIF
   nPos := nPos2 + 1
   DO WHILE Substr( cBuff, nPos, 1 ) == ' '; nPos++; ENDDO
   IF Substr( cBuff, nPos, 1 ) != '"'
      edi_Writelog( "Error in tutor 5:" + Chr(10) + Substr( cBuff, nPos-10, 40 ) )
      RETURN Nil
   ENDIF
   nPos ++
   IF ( nPos2 := hb_At( '"', cBuff, nPos ) ) == 0
      edi_Writelog( "Error in tutor 6:" + Chr(10) + Substr( cBuff, nPos-10, 40 ) )
      RETURN Nil
   ENDIF

   cRest := Substr( cBuff, nPos2+1, nPos3-nPos2 )
   RETURN Substr(cBuff,nPos,nPos2-nPos)

STATIC FUNCTION _itu_Zip()

   LOCAL cZip, hZip

   IF Right( cTutorCurr,4 ) == ".zip"
      RETURN Nil
   ENDIF

   cZip := cIniPath + "itutor" + hb_ps() + cTutorCurr + ".zip"
   IF File( cZip )
      IF edi_Alert( cTutorCurr + ".zip file exists. Overwrite?", "Yes", "No" ) != 1
         RETURN Nil
      ENDIF
      FErase( cZip )
   ENDIF
   hZip := hb_zipOpen( cZip )
   cZip := cIniPath + "itutor" + hb_ps() + cTutorCurr
   hb_zipStoreFile( hZip, cZip, cTutorCurr )
   hb_zipClose( hZip )
   edi_Alert( "Dome." )

   RETURN Nil