/*
 * Client for llama.prg module
 * HbEdit plugin
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_F2         -1

#define S_INIT            0
#define S_MODULE_STARTED  1
#define S_MODEL_PARAMS    2
#define S_MODEL_LOADING   3
#define S_MODEL_LOADED    4
#define S_CNT_CREATING    5
#define S_ASKING          6
#define S_GETTOKEN        7

DYNAMIC ECLI_CLOSE, ECLI_RUN, ECLI_RUNPROC, ECLI_RUNFUNC, ECLI_CHECKANSWER

STATIC cIniPath
STATIC oClient
STATIC hPlugExtCli
STATIC aModels, cCurrModel
STATIC nLogLevel := 0
STATIC nStatus, lPaused := .F.
STATIC n_ctx := 512, n_predict := -1, n_keep := 0, temp := 0.8, penalty_r := 1.1, top_k := 40, top_p := 0.95

FUNCTION plug_extLLM( oEdit, cPath )

   LOCAL i, x, cName := "$ClientLLM", cExtPlug := edi_FindPath( "plugins" + hb_ps() + "hbextcli.hrb" )
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "LLM Client  " )
         IF nStatus == S_MODEL_LOADED
            DevOut( "F2 - Ask a question" )
         ELSEIF nStatus == S_ASKING .OR. nStatus == S_GETTOKEN
            IF lPaused
               DevOut( "Space - Continue" )
            ELSE
               DevOut( "Esc - Stop" )
            ENDIF
         ENDIF
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      IF oClient:lClose
         IF hb_isFunction( "HBEXTCLI" )
            ecli_Close()
         ENDIF
      ENDIF
      RETURN Nil
   }

   IF !hb_isFunction( "HBEXTCLI" ) .AND. File( cExtPlug )
      x := hb_hrbLoad( cExtPlug )
      IF hb_isFunction( "FILEPANE" )
         FilePane():hMisc["extcli_plug"] := x
      ELSE
         hPlugExtCli := x
      ENDIF
   ENDIF
   IF !hb_isFunction( "HBEXTCLI" )
      edi_Alert( "Can't load hbectcli.hrb" )
      RETURN Nil
   ENDIF

   aModels := {}
   _clillm_IniRead( (cIniPath := cPath) + "models.ini" )
   IF Empty( aModels )
      edi_Alert( "Check your models.ini" )
      RETURN Nil
   ENDIF

   IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cName} ) ) > 0
       mnu_ToBuf( oEdit, i )
      RETURN oEdit:aWindows[i]
   ENDIF

   oClient := mnu_NewBuf( oEdit )
   oClient:cFileName := cName
   oClient:bWriteTopPane := bWPane
   oClient:bOnKey := {|o,n| _clillm_OnKey(o,n) }
   oClient:bStartEdit := {|| _clillm_Start() }
   oClient:bEndEdit := bEndEdit
   oClient:cp := "RU866"
   oClient:lWrap := .T.
   nStatus := S_INIT

   RETURN Nil

STATIC FUNCTION _clillm_Start()

   LOCAL aMenu, iChoic, xRes
   LOCAL cExe := cIniPath + "llama_exsrv"
#ifndef __PLATFORM__UNIX
   cExe += ".exe"
#endif

   IF oClient:hCargo == Nil
      //oClient:hCargo := hb_hash()
      //oClient:hCargo["help"] := "Llama plugin hotkeys:" + Chr(10) + ;
      //   "  F2  - Ask New Question"
   ENDIF

   IF nStatus == S_INIT
      IF !Empty( iChoic := FMenu( oClient, aModels, 3, 10 ) )
         cCurrModel := aModels[ iChoic,1 ]
         _Textout( "Ext module launching..." )
         IF ecli_Run( cExe, nLogLevel )
            _clillm_SetParams()
            _Textout( "Model " + hb_fnameNameExt( cCurrModel ) + " loading..." )
            nStatus := S_MODEL_LOADING
            ecli_RunFunc( "OpenModel", {cCurrModel}, .T. )
            IF ( xRes := _clillm_Wait() ) == Nil
               oClient:lClose := .T.
            ELSEIF xRes == ""
            ELSEIF xRes == "ok"
               nStatus := S_MODEL_LOADED
               _Textout( "Model loaded" )
               oClient:WriteTopPane()
            ELSE
               nStatus := S_MODULE_STARTED
               _Textout( "Can't load model" )
            ENDIF
         ELSE
            _Textout( "Failed to start module" )
         ENDIF
      ELSE
         oClient:lClose := .T.
      ENDIF
   ELSEIF nStatus == S_ASKING .OR. nStatus == S_GETTOKEN
      oClient:WriteTopPane()
   ENDIF

   RETURN Nil

STATIC FUNCTION _clillm_SetParams()

   LOCAL xRes := "", cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets, y1, x1, x2, y2, i, j

   y1 := Int( MaxRow()/2 ) - 1
   x1 := Int( MaxCol()/2 ) - 20
   x2 := x1 + 40
   y2 := y1 + 6

   aGets := { {y1,x1+4, 11, "Parameters"}, ;
      { y1+1,x1+2, 11, "n_ctx" }, { y1+1,x1+10, 0, Ltrim(Str(n_ctx)), 6 }, ;
      { y1+1,x1+20, 11, "n_predict" }, { y1+1,x1+31, 0, Ltrim(Str(n_predict)), 6 }, ;
      { y1+2,x1+2, 11, "n_keep" }, { y1+2,x1+10, 0, Ltrim(Str(n_keep)), 6 }, ;
      { y1+3,x1+2, 11, "temp" }, { y1+3,x1+10, 0, Ltrim(Str(temp)), 6 }, ;
      { y1+3,x1+20, 11, "penalty_r" }, { y1+3,x1+31, 0, Ltrim(Str(penalty_r)), 6 }, ;
      { y1+4,x1+2, 11, "top_k" }, { y1+4,x1+10, 0, Ltrim(Str(top_k)), 6 }, ;
      { y1+4,x1+20, 11, "top_p" }, { y1+4,x1+31, 0, Ltrim(Str(top_p)), 6 } ;
      }

   cBuf := Savescreen( y1, x1, y2, x2 )
   @ y1, x1, y2, x2 BOX "........ "

   edi_READ( aGets )
   IF LastKey() == 13
      IF Val(AllTrim(aGets[3,4])) != n_ctx
         n_ctx := Val(AllTrim(aGets[3,4]))
         xRes += 'c=' + Ltrim(Str(n_ctx)) + Chr(1)
      ENDIF
      IF Val(AllTrim(aGets[5,4])) != n_predict
         n_predict := Val(AllTrim(aGets[5,4]))
         xRes += 'n=' + Ltrim(Str(n_predict)) + Chr(1)
      ENDIF
      IF Val(AllTrim(aGets[7,4])) != n_keep
         n_keep := Val(AllTrim(aGets[7,4]))
         xRes += 'n-keep=' + Ltrim(Str(n_keep)) + Chr(1)
      ENDIF
      IF Val(AllTrim(aGets[9,4])) != temp
         temp := Val(AllTrim(aGets[9,4]))
         xRes += 'temp=' + Ltrim(Str(temp)) + Chr(1)
      ENDIF
      IF Val(AllTrim(aGets[11,4])) != penalty_r
         penalty_r := Val(AllTrim(aGets[11,4]))
         xRes += 'repeat-penalty=' + Ltrim(Str(penalty_r)) + Chr(1)
      ENDIF
      IF Val(AllTrim(aGets[13,4])) != top_k
         top_k := Val(AllTrim(aGets[13,4]))
         xRes += 'top-k=' + Ltrim(Str(top_k)) + Chr(1)
      ENDIF
      IF Val(AllTrim(aGets[15,4])) != top_p
         top_p := Val(AllTrim(aGets[15,4]))
         xRes += 'top-k=' + Ltrim(Str(top_p)) + Chr(1)
      ENDIF

      IF !Empty( xRes )
         nStatus := S_MODEL_PARAMS
         ecli_RunFunc( "SetParams",{xRes} )
      ENDIF
   ENDIF

   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cBuf )

   RETURN Nil

STATIC FUNCTION _clillm_Wait()

   LOCAL nKey, sAns := ""

   DO WHILE .T.
      nKey := Inkey( 0.1 )
      IF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
         IF Len( TEdit():aWindows ) == 1
            Hbc( oClient )
         ELSE
            oClient:lShow := .F.
            TEdit():nCurr ++
         ENDIF
         EXIT
      ELSEIF nKey == K_ESC
         RETURN Nil
      ENDIF
      IF !Empty( sAns := ecli_CheckAnswer() )
         sAns := _DropQuotes( sAns )
         EXIT
      ENDIF
   ENDDO

   RETURN sAns

STATIC FUNCTION _clillm_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey == K_F2
      IF nStatus == S_MODEL_LOADED
         _clillm_Ask()
         RETURN -1
      ENDIF

   ELSEIF nKey == 32
      IF nStatus == S_ASKING .OR. nStatus == S_GETTOKEN
         IF lPaused
            lPaused := .F.
            oClient:WriteTopPane()
            _clillm_Wait4Answer()
         ENDIF
      ENDIF

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ENDIF

   RETURN 0

STATIC FUNCTION _clillm_Ask()

   LOCAL x := Int( (oClient:x2 + oClient:x1)/2 )

   lPaused := .F.
   IF !Empty( x := edi_MsgGet( "Your question", 3, x-30, x+30 ) )
      nStatus := S_CNT_CREATING
      IF ecli_RunFunc( "CreateContext",{} ) == "ok"
         nStatus := S_ASKING
         ecli_RunFunc( "Ask",{x}, .T. )
         _Textout( "> " + x )
         _clillm_Wait4Answer()
      ELSE
         nStatus := S_MODEL_LOADED
         _Textout( "Can't create context" )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _clillm_Wait4Answer()

   LOCAL xRes

   IF ( xRes := _clillm_Wait() ) == Nil
      // ESC pressed
      ecli_RunProc( "CloseContext",{} )
      nStatus := S_MODEL_LOADED
      _Textout( "Canceled." )
   ELSEIF xRes == ""
      // Ctrl-Tab
   ELSE
      nStatus := S_GETTOKEN
      oClient:WriteTopPane()
      ecli_RunFunc( "GetNextToken",{}, .T. )
      DO WHILE .T.
         IF ( xRes := _clillm_Wait() ) == Nil
            // ESC pressed
            ecli_RunProc( "CloseContext",{} )
            nStatus := S_MODEL_LOADED
            EXIT
         ELSEIF xRes == ""
            // Ctrl-Tab
            EXIT
         ELSE
            xRes := _DropQuotes( xRes )
            IF xRes == '===='
               ecli_RunProc( "CloseContext",{} )
               nStatus := S_MODEL_LOADED
               _Textout( xRes, .T. )
               EXIT
            ELSE
               ecli_RunFunc( "GetNextToken",{}, .T. )
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

STATIC FUNCTION _Textout( cLine, lSameLine )

   LOCAL n := Len( oClient:aText )

   IF Empty( lSameLine )
      n ++
      oClient:InsText( n, 0, cLine )
   ELSE
      oClient:InsText( n, Len( oClient:aText[n] ), cLine )
   ENDIF
   n := Max( 1, Row() - oClient:y1 )
   oClient:TextOut( n )

   RETURN Nil

STATIC FUNCTION _DropQuotes( s )

   LOCAL nPos

   IF Left( s,1 ) == '"'
      s := Substr( s, 2, Len( s ) - 2 )
   ENDIF
   IF ( nPos := At( '\n', s ) ) > 0
      s := Iif( nPos==1, "", Left( s,nPos-1 ) ) + Chr(10) + Substr( s,nPos+2 )
   ENDIF

   RETURN s

STATIC FUNCTION _clillm_IniRead( cFileName )

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
         IF Left( s1, 5 ) == "model"
            AAdd( aModels, { s2, "" } )
         ELSEIF s1 == "c" .OR. s1 == "n" .OR. s1 == "temp" .OR. s1 == "repeat-penalty" ;
            .OR. s1 == "top-k" .OR. s1 == "top-n" .OR. s1 == "n-keep"
            ATail( aModels )[2] += s1 + '=' + s2 + Chr(1)
         ELSEIF Left( s1, 3 ) == "log"
            nLogLevel := Val( s2 )
         ENDIF
      ENDIF
   NEXT

   RETURN Nil
