/*
 * Client for Hugging Face Chat
 * HbEdit plugin
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ENTER      13
#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_F1         28
#define K_F2         -1
#define K_F3         -2
#define K_F5         -4
#define K_F9         -8
#define K_F10        -9
#define K_PGDN        3

#define CTRL_PRESSED 0x020000

#define S_INIT            0
#define S_LOGGED          1
#define S_ASKING          2
#define S_GETTOKEN        3

STATIC cIniPath
STATIC oClient
STATIC hExt
STATIC cEmail
STATIC nLogLevel := 0
STATIC nStatus, lPaused := .F.
STATIC cCompiler
STATIC lShareWith, lStream, lWeb, lWebHint

FUNCTION plug_HugClient( oEdit, cPath )

   LOCAL i, cRes, cName := "$ClientHug"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "Hugging Face Client" )
      ENDIF
      IF nStatus == S_LOGGED
         DevPos( y, o:x1 + 22 )
         DevOut( "F1 - Help  F2 - Ask F9 - Menu  F10 - Close" )
      ELSEIF nStatus == S_ASKING .OR. nStatus == S_GETTOKEN
         DevPos( y, o:x1 + 22 )
         DevOut( "ESC - Cancel" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      IF oClient:lClose
         IF !Empty( hExt )
            ecli_Close( hExt )
            hExt := Nil
         ENDIF
      ENDIF
      RETURN Nil
   }

   cedi_RunConsoleApp( 'python --version',, @cRes )
   IF !Empty( cRes )
      cCompiler := "python"
   ELSE
      cedi_RunConsoleApp( 'python3 --version',, @cRes )
      IF !Empty( cRes )
         cCompiler := "python3"
      ELSE
         edi_Alert( "You need to install Python to use this plugun" )
         RETURN Nil
      ENDIF
   ENDIF
   cRes := Nil
   cedi_RunConsoleApp( cCompiler + ' -c "import hugchat"',, @cRes )
   IF !Empty( cRes )
      edi_Alert( "You need to install 'hugchat' module for Python" )
      RETURN Nil
   ENDIF

   _clihug_IniRead( (cIniPath := cPath) + "hugclient.ini" )

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
       mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   oClient := mnu_NewBuf( oEdit )
   oClient:cFileName := cName
   oClient:bWriteTopPane := bWPane
   oClient:bOnKey := {|o,n| _clihug_OnKey(o,n) }
   oClient:bStartEdit := {|| _clihug_Start() }
   oClient:bEndEdit := bEndEdit
   oClient:cp := "UTF8"
   hb_cdpSelect( oClient:cp )
   oClient:lUtf8 := .T.
   oClient:lWrap := .T.
   nStatus := S_INIT
   lShareWith := .T.; lStream := .T.; lWeb := .F.; lWebHint := .F.

   RETURN Nil

STATIC FUNCTION _clihug_Start()

   LOCAL aMenu, i, cRes, cPass
   LOCAL cExe := cCompiler + " " + cIniPath + "hugclient" + hb_ps() + "hugclient.py"

   IF oClient:hCargo == Nil
      oClient:hCargo := hb_hash()
      oClient:hCargo["help"] := "Hugging Face client menu:" + Chr(10) + ;
         "  F9 - Menu" + Chr(10) + ;
         "  Ctrl-Tab - Switch Buffer" + Chr(10) + "  F10 - Exit" + Chr(10) + ;
         "Menu items:" + Chr(10) + ;
         "  Ask: Input new prompt" + Chr(10) + ;
         "  new: Create and switch to a new conversation." + Chr(10) + ;
         "  switch: Shows a list of all conversations' info in *current session*." + Chr(10) + ;
         "    Then you can choose one to switch to." + Chr(10) + ;
         "  switch all: Shows a list of all conversations' info in *your account*." + Chr(10) + ;
         "    Then you can choose one to switch to." + Chr(10) + ;
         "  Get history: Returns a history of a current conversation." + Chr(10) + ;
         "  del <id>: Deletes a conversation by index. Will not delete active session." + Chr(10) + ;
         "  llm: Get available models you can switch to." + Chr(10) + ;
         "  llm <index>: Switches model to given model index based on /llm." + Chr(10) + ;
         "  sharewithauthor <on|off>: Changes settings for sharing data with model author." + Chr(10) + ;
         "    On by default." + Chr(10) + ;
         "  stream <on|off>: streaming the response." + Chr(10) + ;
         "  web <on|off>: web search." + Chr(10) + ;
         "  web-hint <on|off>: display web search hint." + Chr(10) + ;
         "  exit: ends this app."
   ENDIF

   IF nStatus == S_INIT
      _Textout( "Ext module launching..." )
      IF !Empty( hExt := ecli_Run( cExe, nLogLevel,, "hbedit_hug" ) )
         _Textout( Time() + " Module loaded. Logging..." )
         IF Empty( cEmail )
            IF Empty( cEmail := edi_MsgGet( "Input your email:", oClient:y1+2, oClient:x1+4, oClient:x1+30 ) )
               oClient:lClose := .T.
               RETURN Nil
            ENDIF
         ENDIF
         IF ( cRes := ecli_RunFunc( hExt, "setemail",{cEmail} ) ) == "need psw"
            IF Empty( cPass := edi_MsgGet( "Password:", oClient:y1+2, oClient:x1+4, oClient:x1+30 ) )
               oClient:lClose := .T.
               RETURN Nil
            ENDIF
            DirChange( cIniPath + "hugclient" )
            IF ( cRes := ecli_RunFunc( hExt, "setpass",{cPass} ) ) == "Err"
               edi_Alert( "Error..." )
               oClient:lClose := .T.
               RETURN Nil
            ELSEIF cRes = "Error"
               edi_Alert( "Wrong email or password" )
               oClient:lClose := .T.
               RETURN Nil
            ENDIF
            nStatus := S_LOGGED
         ELSEIF cRes == "Ok"
            nStatus := S_LOGGED
         ENDIF
         IF nStatus == S_LOGGED
            _Textout( "Logged in to Hugging Face" )
            _Textout( ecli_RunFunc( hExt, "execcmd",{"/getcurrent"} ) )
            oClient:GoTo( Len( oClient:aText ), 1 )
            _clihug_Menu()
         ENDIF
         oClient:WriteTopPane()
      ELSE
         edi_Alert( "Failed to start module" )
         oClient:lClose := .T.
      ENDIF
   ELSEIF nStatus == S_ASKING .OR. nStatus == S_GETTOKEN
      oClient:WriteTopPane()
   ENDIF

   RETURN Nil

STATIC FUNCTION _clihug_Wait( lNoEsc, lNoTab, lShowTime )

   LOCAL nKey, sAns := ""
   STATIC nTicks := 0

   DO WHILE .T.
      nKey := Inkey( 0.05 )
      IF (nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB) .AND. Empty( lNoTab )
         IF Len( TEdit():aWindows ) == 1
            Hbc( oClient )
         ELSE
            oClient:lShow := .F.
            TEdit():nCurr ++
         ENDIF
         RETURN ""
      ELSEIF nKey == K_ESC .AND. Empty( lNoEsc )
         RETURN Nil
      ENDIF
      IF ( sAns := ecli_CheckAnswer( hExt ) ) != Nil
         EXIT
      ENDIF
   ENDDO

   RETURN sAns

STATIC FUNCTION _clihug_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey == K_F2
      IF nStatus == S_LOGGED
         _clihug_Ask()
         RETURN -1
      ENDIF

   ELSEIF nKey == 32
      IF nStatus == S_ASKING .OR. nStatus == S_GETTOKEN
         IF lPaused
            lPaused := .F.
            oClient:WriteTopPane()
           _clihug_Wait4Answer()
         ENDIF
      ENDIF

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ELSEIF nKey == K_F9
      _clihug_Menu()
      RETURN -1

   ELSEIF nKey == K_F1
      mnu_Help( oClient )
      hb_cdpSelect( oClient:cp )
      _clihug_Start()
      RETURN -1
   ENDIF

   RETURN 0

STATIC FUNCTION _clihug_Menu()

   LOCAL aMenu := { {"Ask your question",,,"F2"}, {"New conversation",,}, ;
   {"Switch",,}, {"Switch all",,}, {"Get history",,}, {"del <id>",,}, ;
      {"llm - get list",,}, {"llm - switch to <id>",,}, ;
      {"Share with author <"+Iif(lShareWith,"off","on")+">",,}, ;
      {"stream <"+Iif(lStream,"off","on")+">",,}, {"web <"+Iif(lWeb,"off","on")+">",,}, ;
      {"web-hint <"+Iif(lWebHint,"off","on")+">",,}, {"exit",,,"F10"} }
   LOCAL i, cRes, xVal

   i := FMenu( oClient, aMenu, oClient:y1+2, oClient:x1+4 )
   IF i == 1
      cRes := _clihug_Ask()
   ELSEIF i == 2
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/new"} ) )
         _Textout( cRes )
      ENDIF
   ELSEIF i == 3
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/switch"} ) )
         _clihug_Switch( cRes )
      ENDIF
   ELSEIF i == 4
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/switch", "all"} ) )
         _clihug_Switch( cRes )
      ENDIF

   ELSEIF i == 5
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/gethistory"} ) )
         _Textout( cRes )
      ENDIF

   ELSEIF i == 6
      IF !Empty( xVal := edi_MsgGet( "Input number" ) ) .AND. !Empty( xVal := Val(xVal) )
         IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/del",ltrim(Str(xVal))} ) )
            _Textout( cRes )
         ENDIF
      ENDIF
   ELSEIF i == 7
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/llm"} ) )
         _Textout( cRes )
      ENDIF
   ELSEIF i == 8
      IF !Empty( xVal := edi_MsgGet( "Input number" ) ) .AND. !Empty( xVal := Val(xVal) )
         IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/llm",ltrim(Str(xVal))} ) )
            _Textout( cRes )
         ENDIF
      ELSE
         cRes := 1
      ENDIF
   ELSEIF i == 9
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/sharewithauthor",Iif(lShareWith,"off","on") } ) )
         lShareWith := !lShareWith
         _Textout( cRes )
      ENDIF
   ELSEIF i == 10
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/stream",Iif(lStream,"off","on") } ) )
         lStream := !lStream
         _Textout( cRes )
      ENDIF
   ELSEIF i == 11
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/web",Iif(lWeb,"off","on") } ) )
         lWeb := !lWeb
         _Textout( cRes )
      ENDIF
   ELSEIF i == 12
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/web-hint",Iif(lWebHint,"off","on") } ) )
         lWebHint := !lWebHint
         _Textout( cRes )
      ENDIF

   ELSEIF i == 13
      hb_keyPut( K_F10 )
   ENDIF
   IF i > 0 .AND. i < 13
      IF !Empty( cRes )
         oClient:GoTo( Len( oClient:aText ), 1 )
         oClient:TextOut()
      ELSE
         edi_Alert( "Something goes wrong..." )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _clihug_Switch( cList )

   LOCAL aList, i, cRes

   IF Empty( cList )
      RETURN Nil
   ENDIF

   aList = hb_ATokens( cList, Chr(10) )
   IF ( i := FMenu( oClient, aList, oClient:y1+2, oClient:x1+4,,,,,, .T. ) ) > 0
      IF !Empty( cRes := ecli_RunFunc( hExt, "execcmd",{"/switch",Ltrim(Str(i))} ) )
         _Textout( cRes )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _clihug_Ask()

   LOCAL cQue, cRes

   cQue := edi_MsgGet_ext( "", oClient:y1+2, oClient:x1+4, oClient:y1+10, oClient:x2-12, oClient:cp )
   IF !Empty( cQue )
      _Textout( cQue )
      _Textout( "-----------" )
      _Textout( "" )
      oClient:TextOut()
      IF lStream
        ecli_RunFunc( hExt, "ask",{cQue} )
        nStatus := S_GETTOKEN
        oClient:WriteTopPane()
        ecli_RunFunc( hExt, "nexttoken",{2}, .T. )
        _clihug_Wait4Answer()
        cRes := 1
      ELSE
         IF !Empty( cRes := ecli_RunFunc( hExt, "ask",{cQue} ) )
            _Textout( cRes )
            oClient:TextOut()
         ENDIF
      ENDIF
   ENDIF
   RETURN Empty( cQue ) .OR. !Empty( cRes )

STATIC FUNCTION _clihug_Wait4Answer()

   LOCAL xRes

   IF ( xRes := _clihug_Wait() ) == Nil
      // ESC pressed

      nStatus := S_LOGGED
      _Textout( "Canceled." )
   ELSEIF xRes == ""
      // Ctrl-Tab
      lPaused := .T.
   ELSE
      nStatus := S_GETTOKEN
      oClient:WriteTopPane()
      ecli_RunFunc( hExt, "nexttoken",{2}, .T. )
      DO WHILE .T.
         IF ( xRes := _clihug_Wait() ) == Nil
            // ESC pressed
            nStatus := S_LOGGED
            EXIT
         ELSEIF xRes == ""
            // Ctrl-Tab
            lPaused := .T.
            EXIT
         ELSE
            IF Right( xRes,4 ) == '===='
               nStatus := S_LOGGED
               _Textout( hb_strShrink(xRes,4) + " ==", .T. )
               EXIT
            ELSE
               ecli_RunFunc( hExt, "nexttoken",{2}, .T. )
               _Textout( xRes, .T. )
               IF oClient:LineToRow( oClient:nLine ) >= oClient:y2
                  oClient:GoTo( oClient:nLine, Len( oClient:aText[oClient:nLine] ) )
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   oClient:WriteTopPane()

   RETURN Nil

STATIC FUNCTION _Textout( cLine, lSameLine, lFromStart )

   LOCAL n := Len( oClient:aText ), nf

   IF Empty( lSameLine )
      n ++
      nf := n
      oClient:InsText( n, 1, cLine )
   ELSE
      nf := n
      IF Empty( lFromStart )
         oClient:InsText( n, hb_utf8Len( oClient:aText[n] ) + 1, cLine )
      ELSE
         oClient:InsText( n, 1, cLine, .T. )
      ENDIF
   ENDIF
   nf := Max( 1, Row() - oClient:y1 )
   oClient:TextOut( nf )

   RETURN Nil

STATIC FUNCTION _clihug_IniRead( cFileName )

   LOCAL cText := Memoread( cFileName ), aText, i, s, nPos, s1, s2

   IF Empty( cText )
      RETURN Nil
   ENDIF

   aText := hb_aTokens( cText, Chr(10) )

   FOR i := 1 TO Len( aText )
      s := Iif( Left( aText[i],1 ) == ' ', Ltrim( aText[i] ), aText[i] )
      IF Left( s, 1 ) $ ";#"
         LOOP
      ENDIF
      s := Trim( Iif( Right(s,1)==Chr(13), Left( s,Len(s)-1 ), s ) )
      IF Empty( s )
         LOOP
      ENDIF
      IF ( nPos := At( '=', s ) ) > 0
         s1 := Trim( Left(s,nPos-1) )
         s2 := Ltrim( Substr( s,nPos+1 ) )
         IF Left( s1, 5 ) == "email"
            cEmail := s2
         ELSEIF s1 == "log"
            nLogLevel := Val( s2 )
         ENDIF
      ENDIF
   NEXT

   RETURN Nil