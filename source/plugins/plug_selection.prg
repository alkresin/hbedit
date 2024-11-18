#define HB_GTI_CLIPBOARDDATA    15
#define UNDO_OP_START   5
#define UNDO_OP_END     6

STATIC cTextSurr := ""

FUNCTION plug_Selection( oEdit, aMenu )

   LOCAL s

   Aadd( aMenu, {"Surround",@_plug_sele_surround(),Nil} )
   IF oEdit:nSeleMode == 2
      Aadd( aMenu, {"Summ",@_plug_sele_summ(),Nil} )
      Aadd( aMenu, {"Align",@_plug_sele_align(),Nil,">"} )
   ELSE
      IF oEdit:nby1 != oEdit:nby2 .AND. !Empty( TEdit():aCBoards[1,1] ) .AND. ;
         Empty( TEdit():aCBoards[1,3] ) .AND. ( Chr(10) $ TEdit():aCBoards[1,1] )
         Aadd( aMenu, {"Compare",@_plug_sele_diff(),Nil} )
      ENDIF
   ENDIF
   s := edi_GetSelected( oEdit )
   IF Left( s, 4 ) == "http"
      Aadd( aMenu, {"Open url",@_plug_sele_url(),Nil} )
   ELSEIF Left( s,4 ) == "{XX|"
      IF Right( s,2 ) == "}}" .AND. hb_At( "{",s,4 ) > 0
         Aadd( aMenu, {"Decode",@_plug_sele_decode(),Nil} )
      ENDIF
   ELSE
      Aadd( aMenu, {"Encode",@_plug_sele_encode(),Nil} )
   ENDIF
   RETURN Nil

FUNCTION _plug_sele_summ( oEdit )

   LOCAL s := edi_GetSelected( oEdit ), arr, i, nSum := 0

   IF !Empty( s )
      arr := hb_ATokens( s, Chr(10) )
      FOR i := 1 TO Len( arr )
         nSum += Val( Ltrim(arr[i]) )
      NEXT
      edi_SetLastSeleOper( {@_plug_sele_summ(),Nil} )
      hb_gtInfo( HB_GTI_CLIPBOARDDATA, Ltrim(Str( nSum )) )
      edi_Alert( "Summ: " + Ltrim(Str( nSum )) )
   ENDIF

   RETURN Nil

FUNCTION _plug_sele_surround( oEdit, cTextS )

   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu ), y1 := Row(), x1 := Col()-6
   LOCAL aGets := { {y1+1,x1+8,0,cTextSurr,16} }, cText1, cText2, i
   LOCAL nby1, nby2, nbx1, nbx2, lRevert := .F., abr1 := "([{<", abr2 := ")]}>"

   IF Empty( cTextS )
      hb_cdpSelect( "RU866" )
      @ y1, x1, y1+2, x1+26 BOX "ÚÄ¿³ÙÄÀ³ "
      @ y1+1, x1+2 SAY "Text:"
      hb_cdpSelect( oEdit:cp )

      IF edi_READ( aGets ) > 0
         cTextSurr := Trim( aGets[1,4] )
      ENDIF
   ELSE
      cTextSurr := cTextS
   ENDIF

   SetColor( oldc )
   edi_SetPos( oEdit )

   IF !Empty( cTextSurr )
      IF cTextSurr $ ["']
         cText1 := cText2 := cTextSurr
      ELSEIF ( i := At( cTextSurr, abr1 ) ) != 0
         cText1 := Substr( abr1, i, 1 ); cText2 := Substr( abr2, i, 1 )
      ELSE
         cText1 := '<' + cTextSurr + '>'; cText2 := '</' + cTextSurr + '>'
      ENDIF
      IF oEdit:nby1 < oEdit:nby2 .OR. ( oEdit:nby1 == oEdit:nby2 .AND. oEdit:nbx1 < oEdit:nbx2 )
         nby1 := oEdit:nby1; nbx1 := oEdit:nbx1; nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
      ELSE
         nby1 := oEdit:nby2; nbx1 := oEdit:nbx2; nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
         lRevert := .T.
      ENDIF

      oEdit:Undo( nby1, nbx1,,, UNDO_OP_START )
      IF oEdit:nSeleMode == 2
      ELSE
         IF lRevert
            oEdit:nbx2 += Len( cText1 )
            IF nby1 == nby2
               oEdit:nbx1 += Len( cText1 )
            ENDIF
         ELSE
            oEdit:nbx1 += Len( cText1 )
            IF nby1 == nby2
               oEdit:nbx2 += Len( cText1 )
            ENDIF
         ENDIF
         oEdit:InsText( nby2, nbx2, cText2,, .F. )
         oEdit:InsText( nby1, nbx1, cText1,, .F. )
      ENDIF
      oEdit:Undo( nby1, nbx1,,, UNDO_OP_END )
      edi_SetLastSeleOper( {@_plug_sele_surround(),cTextSurr} )
   ENDIF

   RETURN Nil

FUNCTION _plug_sele_align( oEdit, lLeft )

   LOCAL aMenu := { "Align Left", "Align Right" }, y1 := Row(), x1 := Col()-6
   LOCAL i, s, arr, nLen, nWidth := 0  //lTabs := oEdit:lTabs

   //oEdit:lTabs := .F.
   s := edi_GetSelected( oEdit )
   //oEdit:lTabs := lTabs
   arr := hb_ATokens( s, Chr(10) )
   nLen := Len(arr)
   FOR i := 1 TO nLen
      nWidth := Max( nWidth, Len(arr[i]) )
   NEXT
   s := ""

   IF lLeft == Nil
      IF ( i := FMenu( oEdit, aMenu, y1, x1 ) ) == 1
         lLeft := .T.
      ELSEIF i == 2
         lLeft := .F.
      ELSE
         RETURN Nil
      ENDIF
   ENDIF

   IF lLeft
      FOR i := 1 TO nLen
         s += Padr( Alltrim(arr[i]), nWidth ) + Iif( i == nLen, "", Chr(10) )
      NEXT
      edi_ReplSelected( oEdit, s )
   ELSE
      FOR i := 1 TO nLen
         s += Padl( Alltrim(arr[i]), nWidth ) + Iif( i == nLen, "", Chr(10) )
      NEXT
      edi_ReplSelected( oEdit, s )
   ENDIF
   edi_SetLastSeleOper( {@_plug_sele_align(),lLeft} )

   RETURN Nil

FUNCTION _plug_sele_diff( oEdit )

   LOCAL f1 := hb_DirTemp() + "hbedit1.out", f2 := hb_DirTemp() + "hbedit2.out"
   LOCAL cText, o1, cAddw1 := "$1"

   hb_Memowrit( f1, TEdit():aCBoards[1,1] )
   hb_Memowrit( f2, edi_GetSelected( oEdit ) )

   edi_CloseWindow( cAddw1 )
   o1 := TEdit():New( Memoread( f1 ), f1, oEdit:aRectFull[1], oEdit:aRectFull[2], oEdit:aRectFull[3], oEdit:aRectFull[4] )
   IF ( cText := edi_MakeDiff( o1, f2 ) ) == Nil
      edi_Alert( "Diff tool not found" )
   ELSE
      edi_AddDiff( o1, cText )
      o1:cFileName := cAddw1
      mnu_ToBuf( oEdit, o1 )
   ENDIF

   RETURN Nil

FUNCTION _plug_sele_url( oEdit )

   LOCAL s := edi_GetSelected( oEdit ), nbx2, i, nl := oEdit:nby1, c, ct := e"\9", cf

   IF s == "http"
      IF oEdit:nbx1 > oEdit:nbx2
         i := nbx2 := oEdit:nbx1 - 1
      ELSE
         i := nbx2 := oEdit:nbx2 - 1
      ENDIF
      DO WHILE ++i <= Len( oEdit:aText[nl] ) .AND. ;
         !( (c := Substr(oEdit:aText[nl],i,1)) == " " .OR. c == ct )
      ENDDO
      s += Substr( oEdit:aText[nl], nbx2+1, i-nbx2-1 )
      //edi_writelog(s)
   ENDIF

   cf := Iif( hb_Version(20) .AND. hb_gtVersion()=="HWGUI", 'hwg_ShellExecute("', '"cedi_ShellExecute("' )
   i := &( cf + s + '")' )

   RETURN Nil

FUNCTION _plug_sele_decode( oEdit )

   LOCAL s := edi_GetSelected( oEdit ), s2, nPos, nby2, nbx2
   LOCAL cPass := edi_MsgGet( "Password", oEdit:y1+5, oEdit:x1+10, oEdit:x1+30, .T. )

   IF Empty( cPass )
      RETURN Nil
   ENDIF

   nPos := hb_At( "{", s, 4 )
   s := Substr( s, nPos+1, Len(s)-nPos-2 )
   s2 := hb_blowfishDecrypt( hb_blowfishKey( cPass ), hb_base64decode(s), .T. )
   s2 := edi_MsgGet_ext( s2, oEdit:y1+2, oEdit:x1+8, oEdit:y1+18, oEdit:x2-8,, .T., .T. )
   IF !Empty( s2 )
      IF oEdit:nby1 < oEdit:nby2 .OR. ( oEdit:nby1 == oEdit:nby2 .AND. oEdit:nbx1 < oEdit:nbx2 )
         nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
      ELSE
         nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
      ENDIF
      oEdit:InsText( nby2, nbx2, Chr(10) + s2 + Chr(10) )
   ENDIF

   RETURN Nil

FUNCTION _plug_sele_encode( oEdit )

   LOCAL s := edi_GetSelected( oEdit ), s2, nby2, nbx2
   LOCAL cPass := edi_MsgGet( "Password", oEdit:y1+5, oEdit:x1+10, oEdit:x1+30, .T. )

   IF Empty( cPass )
      RETURN Nil
   ENDIF

   s2 := "{XX|bfr|64{" + hb_base64encode( hb_blowfishEncrypt( hb_blowfishKey( cPass ), s, .T. ) ) + "}}"
   s2 := edi_MsgGet_ext( s2, oEdit:y1+2, oEdit:x1+8, oEdit:y1+18, oEdit:x2-8,, .T., .T. )
   IF !Empty( s2 )
      IF oEdit:nby1 < oEdit:nby2 .OR. ( oEdit:nby1 == oEdit:nby2 .AND. oEdit:nbx1 < oEdit:nbx2 )
         nby2 := oEdit:nby2; nbx2 := oEdit:nbx2
      ELSE
         nby2 := oEdit:nby1; nbx2 := oEdit:nbx1
      ENDIF
      oEdit:InsText( nby2, nbx2, Chr(10) + s2 + Chr(10) )
   ENDIF

   RETURN Nil