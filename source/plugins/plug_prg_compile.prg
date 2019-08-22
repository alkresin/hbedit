#define K_ENTER       13
#define K_LDBLCLK     1006

Function plug_prg_compile( oEdit )

   LOCAL acmd := Array( 5 ), cHrb, i, cName := "", cTemp, nPos, cFile := "$hb_compile_err", oNew
   LOCAL cFileRes := hb_DirTemp() + "hb_compile_err.out"
   LOCAL nRow := Row(), nCol := Col()

   edi_CloseWindow( cFile )

   acmd[1] := oEdit:ToString()
   acmd[2] := "harbour"
   acmd[3] := "-n2"
   acmd[4] := "-q"
   acmd[5] := "-w"

   cedi_rediron( 2, cFileRes )
   cHrb := hb_compileFromBuf( hb_ArrayToParams( acmd ) )
   cedi_rediroff( 2 )
   CLEAR SCREEN

   FOR i := 1 TO Len( oEdit:aText )
      IF ( cTemp := Lower(Left(oEdit:aText[i],8)) ) == "function" .OR. cTemp == "procedur"
         IF ( nPos := hb_At( " ", oEdit:aText[i], 8 ) ) > 0
            cTemp := Ltrim( Substr( oEdit:aText[i], nPos + 1 ) )
            IF ( nPos := hb_At( "(", cTemp ) ) > 0
               cName := Lower( Left( cTemp, nPos-1 ) ) + ".hrb"
            ELSE
               cName := Lower( cTemp ) + ".hrb"
            ENDIF
            EXIT
         ENDIF
      ENDIF
   NEXT

   cTemp := Memoread( cFileRes )
   IF Empty( cHrb ) .OR. ( !Empty( cTemp ) .AND. " Warning " $ cTemp .AND. ;
         edi_Alert( "There are warnings;Run anyway?","Yes","No" ) == 2 )

      oNew := edi_AddWindow( oEdit, cTemp, cFile, 2, 7 )
      oNew:lReadOnly := .T.
      oNew:bOnKey := {|o,n| _prg_Err_OnKey(o,n) }
   ELSE
      SaveHrb( oEdit, cHrb, cName )
      DevPos( nRow, nCol )
   ENDIF

   SetColor( oEdit:cColor )
   oEdit:WriteTopPane()
   oEdit:TextOut()

   RETURN Nil

STATIC FUNCTION SaveHrb( oEdit, cHrb, cName )

   LOCAL aGets := { {11,22,0,"",33,"W+/BG","W+/BG"}, ;
      {12,29,0,"",26,"W+/BG","W+/BG"}, {13,23,1,.F.,1}, ;
      {15,25,2,"[Save]",8,"N/W","W+/BG",{||__KeyBoard(Chr(13))}}, ;
      {15,40,2,"[Cancel]",10,"N/W","W+/BG",{||__KeyBoard(Chr(27))}} }
   LOCAL oldc, nRes, arr, i, s := "", cEol, cFull, cTitle

   oldc := SetColor( "N/W,N/W,,,N/W" )
   hb_cdpSelect( "RU866" )
   @ 09, 20, 16, 60 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 14, 20 SAY "Ã"
   @ 14, 60 SAY "´"
   @ 14, 21 TO 14, 59
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Compiled successfully! Save as ..."
   @ 12,22 SAY "Title:"
   @ 13,22 SAY "[ ] Add a line to hbedit.ini"

   aGets[1,4] := cName

   nRes := edi_READ( aGets )
   SetColor( oldc )

   IF nRes > 0 .AND. nRes < Len(aGets) .AND. !Empty( cName := aGets[1,4] )
      cTitle := Iif( Empty(aGets[2,4]), cName, hb_fnameName(aGets[2,4]) )
      cFull := hb_DirBase() + "plugins" + hb_ps() + cName
      IF File( cFull ) .AND. edi_Alert( cName + " already exist. Overwrite it?", "Yes", "No" ) != 1
         RETURN Nil
      ENDIF
      hb_Memowrit( cFull, cHrb )
      IF aGets[3,4] .AND. Ascan( TEdit():aPlugins, {|a|a[1]==cName} ) == 0
         arr := hb_aTokens( MemoRead( hb_DirBase() + "hbedit.ini" ), Chr(10) )
         cEol := Iif( Right(arr[1],1) == Chr(13), Chr(13)+Chr(10), Chr(10) )
         FOR i := 1 TO Len( arr )
            s += arr[i] + Chr(10)
            IF Left( arr[i],9 ) == "[PLUGINS]"
               s += "p" + Ltrim(Str(Len(TEdit():aPlugins)+1)) + "=" + cName + ",," + ;
                  cTitle + cEol
            ENDIF
         NEXT
         hb_MemoWrit( hb_DirBase() + "hbedit.ini", s )
         AAdd( TEdit():aPlugins, { cName, cTitle, "", Nil } )
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION _prg_Err_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nRow, s, nPos, nLine

   IF nKey == K_ENTER .OR. nKey == K_LDBLCLK
      IF nKey == K_LDBLCLK
         nRow := MRow()
         IF nRow < oEdit:y1 .OR. nRow > oEdit:y2
            RETURN 0
         ENDIF
      ELSE
         nRow := Row()
      ENDIF
      s := Lower( oEdit:aText[ oEdit:RowToLine( nRow ) ] )
      IF ( nPos := At( " error ", s ) ) > 0 .OR. ( nPos := At( " warning ", s ) ) > 0
         s := AllTrim( Left( s, nPos ) )
         IF Right( s, 1 ) == ")" .AND. ( nPos := Rat( "(",s ) ) > 0
            nLine := Val( Substr( s,nPos+1 ) )
            oEdit:oParent:GoTo( nLine, 1,, .T. )
            oEdit:lShow := .F.
            oEdit:nCurr := Ascan( oEdit:aWindows, {|o|o==oEdit:oParent} )
         ENDIF
      ENDIF
   ENDIF

   RETURN 0
