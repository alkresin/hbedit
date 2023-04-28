#define K_ENTER    13
#define K_ESC      27

STATIC lIsCurl := .F.

FUNCTION Plug_WebServices( oEdit )

   LOCAL cFileRes := hb_DirTemp() + "hbedit_curl.out", cFileOut := hb_DirTemp() + "hbedit.out", cBuff
   LOCAL aMenu := { "Get external IP", "Word definition", "Lorem ipsum" }, iChoic
   LOCAL nRow := Row(), nCol := Col(), cAddW := "$Result"

   IF !lIsCurl
      FErase( cFileRes )
      cedi_RunConsoleApp( "curl --version",, @cBuff )
      IF !Empty( cBuff ) .AND. "libcurl" $ cBuff
         lIsCurl := .T.
      ELSE
         edi_Alert( "Curl must be installed to use this plugin; curl executable should be in PATH" )
         DevPos( nRow, nCol )
         RETURN Nil
      ENDIF
   ENDIF

   IF !Empty( iChoic := FMenu( oEdit, aMenu, 3, 10 ) )
      FErase( cFileRes )
      IF iChoic  == 1
         cedi_RunConsoleApp( "curl ifconfig.me -s",, @cBuff )
         edi_writelog( cBuff )
         IF !Empty( cBuff )
            IF Len( cBuff ) > 20
               edi_Alert( "Error.;" + cBuff )
            ELSE
               edi_2cb( oEdit,, cBuff )
               edi_Alert( cBuff )
            ENDIF
         ENDIF
      ELSEIF iChoic  == 2
         edi_CloseWindow( cAddW )
         edi_SelectW( oEdit )
         cBuff := cp_Substr( oEdit:lUtf8, oEdit:aText[oEdit:nLine], oEdit:nbx1, oEdit:nbx2-oEdit:nbx1 )
         IF !Empty( cBuff := _plug_GetString( oEdit, "Find word:", cBuff ) )
            cedi_RunConsoleApp( "curl dict://dict.org/d:" + cBuff + " -s",, @cBuff )
            IF !Empty( cBuff )
               edi_AddWindow( oEdit, cBuff, cAddW, 2, 10 )
            ELSE
               edi_Alert( "Error" )
            ENDIF
         ENDIF
      ELSEIF iChoic  == 3
         edi_CloseWindow( cAddW )
         IF !Empty( cBuff := _lorem_GetString( oEdit ) )
            cedi_RunConsoleApp( "curl " + cBuff + "-s",, @cBuff )
            IF !Empty( cBuff )
               edi_AddWindow( oEdit, cBuff, cAddW, 2, 10 )
            ELSE
               edi_Alert( "Error" )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   DevPos( nRow, nCol )

   RETURN Nil

STATIC FUNCTION _plug_GetString( oEdit, sTitle, s )

   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu )
   LOCAL aGets := { {11,27,0,Iif(s==Nil,s := "",s),26}, ;
      {13,28,2,"[Ok]",4,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {13,42,2,"[Cancel]",10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL nRes

   hb_cdpSelect( "RU866" )
   @ 09, 25, 14, 55 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 12, 25 SAY "Ã"
   @ 12, 55 SAY "´"
   @ 12, 26 TO 12, 54
   hb_cdpSelect( oEdit:cp )

   @ 10,27 SAY sTitle
   SetColor( oEdit:cColorMenu )

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      s := aGets[1,4]
   ELSE
      s := ""
   ENDIF

   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN s

STATIC FUNCTION _lorem_GetString( oEdit )

   LOCAL s := "https://loripsum.net/api/1", nRes
   LOCAL aGets := { {11,23,3,.T.,1}, {11,33,3,.F.,1}, {11,44,3,.F.,1}, {11,52,3,.F.,1}, ;
      {12,23,1,.F.,1}, {12,38,1,.F.,1}, ;
      {13,23,1,.F.,1}, {13,38,1,.F.,1}, {13,46,1,.F.,1}, {13,53,1,.F.,1}, ;
      {14,23,1,.F.,1}, {14,38,1,.F.,1}, {14,53,1,.F.,1}, ;
      {15,23,1,.F.,1}, ;
      {17,28,2,"[Ok]",4,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {17,42,2,"[Cancel]",10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }

   hb_cdpSelect( "RU866" )
   @ 09, 20, 18, 67 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 16, 20 SAY "Ã"
   @ 16, 67 SAY "´"
   @ 16, 21 TO 16, 66
   hb_cdpSelect( oEdit:cp )

   @ 10,22 SAY "Lorem options:"
   @ 11, 22 SAY "( ) Short ( ) Medium ( ) Long ( ) Very long"
   @ 12, 22 SAY "[ ] Decorate   [ ] Links"
   @ 13, 22 SAY "[ ] ul (lists) [ ] ol  [ ] dl [ ] blockquotes"
   @ 14, 22 SAY "[ ] code       [ ] headers    [ ] prude"
   @ 15, 22 SAY "[ ] Plain text"

   IF ( nRes := edi_READ( aGets ) ) == 0 .OR. nRes == Len(aGets)
      RETURN Nil
   ENDIF

   IF aGets[1,4]
      s += "/short"
   ELSEIF aGets[2,4]
      s += "/medium"
   ELSEIF aGets[3,4]
      s += "/long"
   ELSEIF aGets[4,4]
      s += "/verylong"
   ENDIF

   IF aGets[5,4]
      s += "/ul"
   ENDIF

   IF aGets[6,4]
      s += "/ol"
   ENDIF

   IF aGets[7,4]
      s += "/dl"
   ENDIF

   IF aGets[8,4]
      s += "/bq"
   ENDIF

   IF aGets[9,4]
      s += "/code"
   ENDIF

   IF aGets[10,4]
      s += "/headers"
   ENDIF

   IF aGets[11,4]
      s += "/prode"
   ENDIF

   IF aGets[12,4]
      s += "/plaintext"
   ENDIF

   RETURN s
