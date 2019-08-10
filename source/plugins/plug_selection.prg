#define HB_GTI_CLIPBOARDDATA    15
#define UNDO_OP_START   5
#define UNDO_OP_END     6

STATIC cTextSurr := ""

FUNCTION plug_Selection( oEdit, aMenu )

   Aadd( aMenu, {"Surround",@_plug_sele_surround(),Nil} )
   IF oEdit:nSeleMode == 2
      Aadd( aMenu, {"Summ",@_plug_sele_summ(),Nil} )
      Aadd( aMenu, {"Align",@_plug_sele_align(),Nil,">"} )
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

FUNCTION _plug_sele_align( oEdit )

   LOCAL aMenu := { "Align Left", "Align Right" }, y1 := Row(), x1 := Col()-6
   LOCAL i, s, arr, nLen, lTabs := oEdit:lTabs, nWidth := 0

   oEdit:lTabs := .F.
   s := edi_GetSelected( oEdit )
   oEdit:lTabs := lTabs
   arr := hb_ATokens( s, Chr(10) )
   nLen := Len(arr)
   FOR i := 1 TO nLen
      nWidth := Max( nWidth, Len(arr[i]) )
   NEXT
   s := ""

   IF ( i := FMenu( oEdit, aMenu, y1, x1 ) ) == 1
      FOR i := 1 TO nLen
         s += Padr( Alltrim(arr[i]), nWidth ) + Iif( i == nLen, "", Chr(10) )
      NEXT
      edi_ReplSelected( oEdit, s )
   ELSEIF i == 2
      FOR i := 1 TO nLen
         s += Padl( Alltrim(arr[i]), nWidth ) + Iif( i == nLen, "", Chr(10) )
      NEXT
      edi_ReplSelected( oEdit, s )
   ENDIF

   RETURN Nil
