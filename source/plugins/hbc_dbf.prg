/*
 * HbEdit plugin to show a dbf file on a pane
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_UP          5
#define K_DOWN       24
#define K_LEFT       19
#define K_RIGHT       4
#define K_HOME        1
#define K_END         6
#define K_PGUP       18
#define K_PGDN        3
#define K_CTRL_PGUP  31
#define K_CTRL_PGDN  30
#define K_F4         -3

#define K_NCMOUSEMOVE           1016
#define HB_K_MENU               1108
#define K_MOUSEMOVE             1001

#define _ROW_FIRST    2

STATIC nAlias := 0
STATIC nBottom

FUNCTION hbc_dbf( cFileName )

   LOCAL oDbfV, cAlias, cName := "$DbfViewer", i, l := .T., lLeto := .F.
   LOCAL oEdit := TEdit():aWindows[1], bOldError
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Dbf Viewer: " + hb_fnameNameExt(o:hCargo["name"]) )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {|o|
      IF o:lClose
         dbSelectArea( o:hCargo["alias"] )
         dbCloseArea()
      ENDIF
      RETURN Nil
   }

   IF Left( cFileName,5 ) == "leto:"
      i := At( '/', cFileName )
      cFileName := SubStr( cFileName, i )
      lLeto := .T.
   ENDIF
   oDbfV := mnu_NewBuf( oEdit )
   oDbfV:cFileName := cName + " " + hb_fnameName(cFileName)
   oDbfV:bWriteTopPane := bWPane
   oDbfV:bOnKey := {|o,n| _dbf_OnKey(o,n) }
   oDbfV:bStartEdit := {|o| _dbf_Start(o) }
   oDbfV:bEndEdit := bEndEdit
   oDbfV:cp := "RU866"

   oDbfV:hCargo := hb_hash()
   oDbfV:hCargo["help"] := "Harbour plugin hotkeys:" + Chr(10)

   cAlias := "A" + Ltrim(Str(++nAlias))

   bOldError := ErrorBlock( { |e|MacroError( e ) } )
   BEGIN SEQUENCE
      IF lLeto
         USE (cFileName) NEW SHARED ALIAS (cAlias) CODEPAGE "RU866" VIA "LETO"
      ELSE
         USE (cFileName) NEW SHARED ALIAS (cAlias) CODEPAGE "RU866"
      ENDIF
   RECOVER
      l := .F.
   END SEQUENCE
   ErrorBlock( bOldError )

   IF !l
      edi_Alert( "Can't open file" )
      mnu_Exit( oDbfV )
   ENDIF

   nBottom := oDbfV:y2 -oDbfV:y1 - 1

   oDbfV:hCargo["name"]  := cFileName
   oDbfV:hCargo["alias"] := cAlias
   oDbfV:hCargo["nLeft"] := 1
   oDbfV:hCargo["nRecF"] := 1
   oDbfV:hCargo["nRow"]  := _ROW_FIRST
   oDbfV:hCargo["lEof"]  := .F.
   oDbfV:hCargo["pBuff"] := hb_hash()
   oDbfV:hCargo["nRecCou"] := RecCount()

   RETURN Nil

STATIC FUNCTION _dbf_Start( oDbfV )

   IF !hb_hHaskey( oDbfV:hCargo, "aCols" )
      dbSelectArea( oDbfV:hCargo["alias"] )
      _dbf_LineTest( oDbfV )
   ENDIF
   TableOut( oDbfV, .T. )

   RETURN Nil

STATIC FUNCTION _dbf_OnKey( oDbfV, nKeyExt )

   LOCAL nKey := hb_keyStd( nKeyExt ), n, n1, nh

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE
      RETURN -1
   ENDIF

   IF nKey == K_LEFT

      IF oDbfV:hCargo["nLeft"] > 1
         oDbfV:hCargo["nLeft"] := oDbfV:hCargo["nLeft"] - 1
         oDbfV:hCargo["pBuff"] := hb_hash()
         _dbf_LineTest( oDbfV )
         TableOut( oDbfV, .T. )
      ENDIF

   ELSEIF nKey == K_RIGHT

      IF oDbfV:hCargo["nRight"] < FCount()
         oDbfV:hCargo["nLeft"] := oDbfV:hCargo["nLeft"] + 1
         oDbfV:hCargo["pBuff"] := hb_hash()
         _dbf_LineTest( oDbfV )
         TableOut( oDbfV, .T. )
      ENDIF

   ELSEIF nKey == K_UP

      IF oDbfV:hCargo["nRow"] > _ROW_FIRST
         oDbfV:hCargo["nRow"] := oDbfV:hCargo["nRow"] - 1
         TableOut( oDbfV )
      ELSEIF oDbfV:hCargo["nRecF"] > 1
         oDbfV:hCargo["nRecF"] := oDbfV:hCargo["nRecF"] - 1
         TableOut( oDbfV )
      ENDIF
      _dbf_BuffClear( oDbfV )

   ELSEIF nKey == K_DOWN

      IF oDbfV:hCargo["nRow"] < oDbfV:hCargo["nBott"]
         oDbfV:hCargo["nRow"] := oDbfV:hCargo["nRow"] + 1
         TableOut( oDbfV )
      ELSEIF oDbfV:hCargo["nRecF"] + oDbfV:hCargo["nBott"] - _ROW_FIRST < oDbfV:hCargo["nRecCou"]
         oDbfV:hCargo["nRecF"] := oDbfV:hCargo["nRecF"] + 1
         TableOut( oDbfV )
      ENDIF
      _dbf_BuffClear( oDbfV )

   ELSEIF nKey == K_PGUP

      n1 := oDbfV:hCargo["nRecF"]
      nh := oDbfV:hCargo["nBott"] - _ROW_FIRST + 1
      IF n1 - nh < 0
         oDbfV:hCargo["nRecF"] := 1
         oDbfV:hCargo["nRow"] := _ROW_FIRST
      ELSE
         oDbfV:hCargo["nRecF"] := n1 - nh
      ENDIF
      TableOut( oDbfV )
      _dbf_BuffClear( oDbfV )

   ELSEIF nKey == K_PGDN

      n := oDbfV:hCargo["nRecCou"]
      n1 := oDbfV:hCargo["nRecF"]
      nh := oDbfV:hCargo["nBott"] - _ROW_FIRST + 1
      IF n1 + nh >= n
         _dbf_GoBottom( oDbfV )
      ELSE
         oDbfV:hCargo["nRecF"] := n1 + nh - 1
         TableOut( oDbfV, .T. )
      ENDIF
      _dbf_BuffClear( oDbfV )

   ELSEIF nKey == K_CTRL_PGUP

      oDbfV:hCargo["nRecF"] := 1
      oDbfV:hCargo["nRow"] := _ROW_FIRST
      TableOut( oDbfV )
      _dbf_BuffClear( oDbfV )

   ELSEIF nKey == K_CTRL_PGDN

      _dbf_GoBottom( oDbfV )

   ELSEIF nKey == K_F4

      _dbf_Stru( oDbfV )

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      IF Len( TEdit():aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_ESC
      mnu_Exit( oDbfV )

   ENDIF

   RETURN -1

STATIC FUNCTION _dbf_GoBottom( oDbfV )

   LOCAL n := oDbfV:hCargo["nRecCou"]

   IF oDbfV:hCargo["nBott"] - _ROW_FIRST + 1 >= n
      oDbfV:hCargo["nRow"] := oDbfV:hCargo["nBott"]
   ELSE
      oDbfV:hCargo["nRecF"] := n - (oDbfV:hCargo["nBott"] - _ROW_FIRST)
      oDbfV:hCargo["nRow"] := oDbfV:hCargo["nBott"]
   ENDIF
   TableOut( oDbfV )
   _dbf_BuffClear( oDbfV )

   RETURN Nil

STATIC FUNCTION TableOut( oDbfV, lClear )

   LOCAL i, s, nCurr := Recno(), nTo := oDbfV:hCargo["nRecF"], nRecCou := oDbfV:hCargo["nRecCou"]
   LOCAL nLeft := oDbfV:hCargo["nLeft"], nRight := oDbfV:hCargo["nRight"]
   LOCAL arr := oDbfV:hCargo["aCols"], nRowCurr := oDbfV:hCargo["nRow"]
   LOCAL h := oDbfV:hCargo["pBuff"]

   dbSelectArea( oDbfV:hCargo["alias"] )

   IF !Empty( lClear )
      Scroll( oDbfV:y1, oDbfV:x1, oDbfV:y2, oDbfV:x2 )
      FOR i := nLeft TO nRight
         DevPos( _ROW_FIRST-1, arr[i-nLeft+1] )
         DevOut( FieldName(i) )
      NEXT
   ENDIF

   i := _ROW_FIRST
   DO WHILE nTo <= nRecCou .AND. i <= nBottom
      IF hb_hHasKey( h, nTo )
         s := h[nTo]
         //edi_Writelog( "2> " + Str(nTo) )
      ELSE
         //edi_Writelog( "1> " + Str(nTo) )
         IF nTo != nCurr
            IF nTo == nCurr + 1
               SKIP
            ELSE
               dbGoTo( nTo )
            ENDIF
         ENDIF
         nCurr := nTo
         s := _dbf_LineOut( oDbfV )
         h[nTo] := s
      ENDIF
      IF i == nRowCurr
         SetColor( oDbfV:cColorPane )
      ENDIF
      DevPos( i, oDbfV:x1+1 )
      DevOut( s )
      IF i == nRowCurr
         SetColor( oDbfV:cColor )
      ENDIF
      i ++
      nTo ++
   ENDDO
   oDbfV:hCargo["nBott"] := i - 1
   DevPos( oDbfV:y2, oDbfV:x1 + 2 )
   DevOut( PAdr( Ltrim(Str(oDbfV:hCargo["nRecF"]+oDbfV:hCargo["nRow"]-_ROW_FIRST)) + "/" + ;
      Ltrim(Str(oDbfV:hCargo["nRecCou"])),18 ) )

   RETURN Nil

STATIC FUNCTION _dbf_LineTest( oDbfV )

   LOCAL nLeft := oDbfV:hCargo["nLeft"], i, nFields := FCount(), s := ""
   LOCAL nWidth := oDbfV:x2 - oDbfV:x1 - 2, arr := {}, n

   FOR i := nLeft TO nFields
      n := Len( s )
      s += _dbf_FieldOut( i ) + " "
      IF Len( s ) > nWidth
         s := Left( s, nWidth )
         EXIT
      ENDIF
      AAdd( arr, n )
   NEXT
   oDbfV:hCargo["nRight"] := nLeft + Len(arr) - 1
   oDbfV:hCargo["aCols"] := arr

   RETURN Nil

STATIC FUNCTION _dbf_LineOut( oDbfV )

   LOCAL nLeft := oDbfV:hCargo["nLeft"], nRight := oDbfV:hCargo["nRight"]
   LOCAL i, nFields := FCount(), s := ""
   LOCAL nWidth := oDbfV:x2 - oDbfV:x1 - 2

   FOR i := nLeft TO nRight
      s += _dbf_FieldOut( i ) + " "
      IF Len( s ) > nWidth
         s := Left( s, nWidth )
         EXIT
      ENDIF
   NEXT

   RETURN s

STATIC FUNCTION _dbf_FieldOut( numf )
   LOCAL fldtype := dbFieldInfo( 2, numf ), xRez, vartmp, nItem := numf

   xRez := Fieldget( numf )

   DO CASE
   CASE fldtype == "C"
      RETURN xRez

   CASE fldtype $ "NIBYZ842+^"
      RETURN Str( xRez, dbFieldInfo(3, numf), dbFieldInfo(4, numf) )

   CASE fldtype = "D"
      RETURN Dtoc( xRez )

   CASE fldtype = "L"
      RETURN Iif( xRez, "T", "F" )

   CASE fldtype = "M"
      RETURN "  <Memo>  "

   CASE fldtype $ "T=@"
      IF dbFieldInfo( 3, numf ) == 4
         RETURN Transform( xRez, "@T" )
      ELSE
         RETURN hb_ttoc( xRez )
      ENDIF

   ENDCASE

RETURN ''

STATIC FUNCTION _dbf_BuffClear( oDbfV )

   LOCAL p := oDbfV:hCargo["pBuff"], key, v, arr := {}
   LOCAL n1 := oDbfV:hCargo["nRecF"], nh := oDbfV:hCargo["nBott"] - _ROW_FIRST + 1

   FOR EACH v IN p
      key := v:__enumKey()
      IF key < n1 - nh * 1.5 .OR. key > n1 + nh * 2.5
         AAdd( arr, key )
      ENDIF
   NEXT

   FOR EACH key IN arr
      hb_hdel( p, key )
   NEXT

   RETURN Nil

STATIC FUNCTION _dbf_Stru( oDbfV )

   LOCAL i, nFields, arr := {}

   dbSelectArea( oDbfV:hCargo["alias"] )
   nFields := FCount()
   FOR i := 1 TO nFields
      AAdd( arr, PAdr(FieldName(i),11) + FieldType(i) + " " + Str(FieldLen(i),5) + Str(FieldDec(i),2) )
   NEXT

   fMenu( oDbfV, arr )
   RETURN Nil