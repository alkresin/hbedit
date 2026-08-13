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

   oDbfV:hCargo["name"] := cFileName
   oDbfV:hCargo["alias"] := cAlias
   oDbfV:hCargo["nLeft"] := 1
   oDbfV:hCargo["nRecF"] := 1
   oDbfV:hCargo["nRow"] := _ROW_FIRST
   oDbfV:hCargo["lEof"] := .F.

   RETURN Nil

STATIC FUNCTION _dbf_Start( oDbfV )

   IF !hb_hHaskey( oDbfV:hCargo, "aCols" )
      _dbf_LineTest( oDbfV )
   ENDIF
   TableOut( oDbfV, .T. )

   RETURN Nil

STATIC FUNCTION _dbf_OnKey( oDbfV, nKeyExt )

   LOCAL nKey := hb_keyStd( nKeyExt ), n

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE
      RETURN -1
   ENDIF

   IF nKey == K_LEFT

      IF oDbfV:hCargo["nLeft"] > 1
         oDbfV:hCargo["nLeft"] := oDbfV:hCargo["nLeft"] - 1
         _dbf_LineTest( oDbfV )
         TableOut( oDbfV, .T. )
      ENDIF

   ELSEIF nKey == K_RIGHT

      IF oDbfV:hCargo["nRight"] < FCount()
         oDbfV:hCargo["nLeft"] := oDbfV:hCargo["nLeft"] + 1
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

   ELSEIF nKey == K_DOWN

      IF oDbfV:hCargo["nRow"] < oDbfV:hCargo["nBott"]
         oDbfV:hCargo["nRow"] := oDbfV:hCargo["nRow"] + 1
         TableOut( oDbfV )
      ELSEIF oDbfV:hCargo["nRecF"] + oDbfV:hCargo["nBott"] - _ROW_FIRST < RecCount()
         oDbfV:hCargo["nRecF"] := oDbfV:hCargo["nRecF"] + 1
         TableOut( oDbfV )
      ENDIF

   ELSEIF nKey == K_PGUP

   ELSEIF nKey == K_PGDN

   ELSEIF nKey == K_CTRL_PGUP

      oDbfV:hCargo["nRecF"] := 1
      oDbfV:hCargo["nRow"] := _ROW_FIRST
      TableOut( oDbfV )

   ELSEIF nKey == K_CTRL_PGDN

      n := RecCount()
      IF oDbfV:hCargo["nBott"] - _ROW_FIRST + 1 >= n
         oDbfV:hCargo["nRow"] := oDbfV:hCargo["nBott"]
      ELSE
         oDbfV:hCargo["nRecF"] := n - (oDbfV:hCargo["nBott"] - _ROW_FIRST)
         oDbfV:hCargo["nRow"] := oDbfV:hCargo["nBott"]
      ENDIF
      TableOut( oDbfV )

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

STATIC FUNCTION TableOut( oDbfV, lClear )

   LOCAL i, nRowCurr := oDbfV:hCargo["nRow"]
   LOCAL nLeft := oDbfV:hCargo["nLeft"], nRight := oDbfV:hCargo["nRight"]
   LOCAL arr := oDbfV:hCargo["aCols"]

   dbSelectArea( oDbfV:hCargo["alias"] )
   dbGoTo( oDbfV:hCargo["nRecF"] )

   IF !Empty( lClear )
      Scroll( oDbfV:y1, oDbfV:x1, oDbfV:y2, oDbfV:x2 )
      FOR i := nLeft TO nRight
         DevPos( _ROW_FIRST-1, arr[i-nLeft+1] )
         DevOut( FieldName(i) )
      NEXT
   ENDIF

   i := _ROW_FIRST
   DO WHILE !Eof() .AND. i <= nBottom
      _dbf_LineOut( oDbfV, i, (i == nRowCurr) )
      i ++
      SKIP
   ENDDO
   oDbfV:hCargo["nBott"] := i - 1
   DevPos( oDbfV:y2, oDbfV:x1 + 2 )
   DevOut( PAdr( Ltrim(Str(oDbfV:hCargo["nRecF"]+oDbfV:hCargo["nRow"]-_ROW_FIRST)) + "/" + ;
      Ltrim(Str(RecCount())),18 ) )

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

STATIC FUNCTION _dbf_LineOut( oDbfV, nL, lCurr )

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
   IF lCurr
      SetColor( oDbfV:cColorPane )
   ENDIF
   DevPos( nL, oDbfV:x1+1 )
   DevOut( s )
   IF lCurr
      SetColor( oDbfV:cColor )
   ENDIF

   RETURN Nil

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