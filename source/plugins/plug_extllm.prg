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

STATIC cIniPath
STATIC oClient
STATIC hPlugExtCli
STATIC aModels := {}, cCurrModel
STATIC nStatus
STATIC n_ctx := 512, n_predict := -1, n_keep := 0, temp := 0.8, penalty_r := 1.1, top_k := 40, top_p := 0.95

FUNCTION plug_extLLM( oEdit, cPath )

   LOCAL i, x, cName := "$ClientLLM", cExtPlug := edi_FindPath( "plugins" + hb_ps() + "hbextcli.hrb" )
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "LLM Client" )
      ENDIF
      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      IF oClient:lClose
         edi_Writelog( "Close" )
         IF hb_isFunction( "HBEXTCLI" )
            Eval( &("{||ecli_Close()}") )
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
   nStatus := S_INIT

   RETURN Nil

STATIC FUNCTION _clillm_Start()

   LOCAL aMenu, iChoic, xRes
#ifdef __PLATFORM__UNIX
   LOCAL cExe := "./llama_exsrv"
#else
   LOCAL cExe := "llama_exsrv.exe"
#endif

   //hIdle := hb_IdleAdd( {|| _Tetr_Tf() } )
   IF nStatus == S_INIT
      IF !Empty( iChoic := FMenu( oClient, aModels, 3, 10 ) )
         cCurrModel := aModels[ iChoic,1 ]

         InsText( 1, 0, "Ext module launching..." )
         IF Eval( &( '{||ecli_Run("' + cExe + '",1,2 )}' ) )
            InsText( 2, 0, "Model " + cCurrModel + " loading..." )
            nStatus := S_MODEL_LOADING
            Eval( &( '{||ecli_RunFunc("OpenModel",{"' + cCurrModel + '"}, .T. )' ) )
            IF ( xRes := _clillm_Wait() ) == Nil
               mnu_Exit( oClient )
            ELSEIF xRes == ""
            ELSEIF xRes == "ok"
               nStatus := S_MODEL_LOADED
               InsText( 3, 0, "Model loaded" )
            ELSE
               nStatus := S_MODULE_STARTED
               InsText( 3, 0, "Can't load model" )
            ENDIF
         ELSE
            InsText( 2, 0, "Failed to start module" )
         ENDIF

      ENDIF
   ENDIF

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
      IF !Empty( sAns := Eval( &("{||ecli_CheckAnswer()}") ) )
         sAns := _DropQuotes( sAns )
      ENDIF
   ENDDO

   RETURN sAns

STATIC FUNCTION _clillm_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), i, j

   IF nKey == K_F2
      IF nStatus == S_MODEL_LOADED
         edi_Alert( "F2" )
      ENDIF

   ELSEIF nKey == K_CTRL_TAB .OR. nKey == K_SH_TAB
      IF Len( oEdit:aWindows ) == 1
         RETURN 0x41010004   // Shift-F4
      ELSE
         RETURN 0
      ENDIF

   ENDIF

   RETURN 0

STATIC FUNCTION _DropQuotes( s )

   LOCAL nPos

   IF Left( s,1 ) == '"'
      s := Substr( s, 2, Len( s ) - 2 )
   ENDIF
   IF ( nPos := At( '\n', s ) ) > 0
      s := Iif( nPos==1, "", Left( s,nPos-1 ) ) + Chr(13) + Chr(10) + Substr( s,nPos+2 )
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
         ENDIF
      ENDIF
   NEXT

   RETURN Nil
