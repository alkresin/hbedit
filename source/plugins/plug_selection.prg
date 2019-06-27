#define HB_GTI_CLIPBOARDDATA    15

FUNCTION plug_Selection( oEdit, aMenu )

   IF oEdit:nSeleMode == 2
      Aadd( aMenu, {"Summ",@_plug_sele_summ(),Nil} )
   ENDIF
   RETURN Nil

FUNCTION _plug_sele_summ( oEdit )

   LOCAL s := edi_GetSelected( oEdit ), arr, i, nSum := 0

   IF !Empty( s )
      arr := hb_ATokens( s, Chr(10) )
      FOR i := 1 TO Len( arr )
         nSum += Val( Ltrim(arr[i]) )
      NEXT
      hb_gtInfo( HB_GTI_CLIPBOARDDATA, Ltrim(Str( nSum )) )
      edi_Alert( "Summ: " + Ltrim(Str( nSum )) )
   ENDIF

   RETURN Nil
