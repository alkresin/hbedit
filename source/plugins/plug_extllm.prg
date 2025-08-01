/*
 * Client for llama.prg module
 * HbEdit plugin
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define K_ENTER      13
#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_F2         -1
#define K_F3         -2
#define K_F5         -4
#define K_F10        -9
#define K_PGDN        3

#define CTRL_PRESSED 0x020000

#define S_INIT            0
#define S_MODULE_STARTED  1
#define S_MODEL_PARAMS    2
#define S_MODEL_LOADING   3
#define S_MODEL_LOADED    4
#define S_CNT_CREATING    5
#define S_CNT_CREATED     6
#define S_ASKING          7
#define S_GETTOKEN        8

STATIC cIniPath
STATIC oClient
STATIC hExt
STATIC aModels, cCurrModel, nCurrModel
STATIC cImgPath, cImgPrefix
STATIC cLastImage, cLastPrompt := ""
STATIC nStartProc := 0
STATIC nLogLevel := 0
STATIC nStatus, lPaused := .F., cModType
STATIC cEndl := e"\r\n"

FUNCTION plug_extLLM( oEdit, cPath )

   LOCAL i, x, cName := "$ClientLLM"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      IF Empty( l )
         DevPos( y, o:x1 )
         DevOut( "LLM Client  " )
         IF nStatus == S_MODEL_LOADING
            DevOut( "Esc - Abort" )
         ELSE
            IF nStatus == S_CNT_CREATED
               IF cModType == "sd"
                  DevOut( "F2 - Input prompt" )
               ELSE
                  DevOut( "F2 - Ask a question  F5 - Reset" )
               ENDIF
            ELSEIF nStatus == S_ASKING .OR. nStatus == S_GETTOKEN
               IF lPaused
                  DevOut( "Space - Continue" )
               ELSE
                  DevOut( "Esc - Stop" )
               ENDIF
            ENDIF
            DevOut( "  Ctrl-Tab - Switch Buffer" )
            IF !( (nStatus == S_ASKING .OR. nStatus == S_GETTOKEN) .AND. !lPaused )
               DevOut( "  F10 - Close" )
            ENDIF
         ENDIF
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
   oClient:cp := "UTF8"
   hb_cdpSelect( oClient:cp )
   oClient:lUtf8 := .T.
   oClient:lWrap := .T.
   nStatus := S_INIT
   cLastPrompt := ""

   RETURN Nil

STATIC FUNCTION _clillm_Start()

   LOCAL aMenu, i, xRes, cParams, cQue
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
      aMenu := Array( Len( aModels ) )
      FOR i := 1 TO Len( aModels )
         aMenu[i] := Iif( Empty(aModels[i,3]), "", "("+aModels[i,3]+")" ) + hb_fnameName( aModels[i,1] )
      NEXT
      IF !Empty( nCurrModel := FMenu( oClient, aMenu, 3, 10 ) )
         cCurrModel := aModels[ nCurrModel,1 ]
         cModType := aModels[ nCurrModel,3 ]
         _Textout( "Ext module launching..." )
         IF !Empty( hExt := ecli_Run( cExe, nLogLevel,, "hbedit_llm" ) )
            IF cModType == "sd"
               IF !_clillm_sd_SetParams()
                  oClient:lClose := .T.
                  RETURN Nil
               ENDIF
            ELSE
               IF ( cParams := _clillm_SetParams() ) == Nil
                  oClient:lClose := .T.
                  RETURN Nil
               ENDIF
            ENDIF
            _Textout( Time() + " Model " + hb_fnameNameExt( cCurrModel ) + " loading..." )
            nStatus := S_MODEL_LOADING
            oClient:WriteTopPane()
            ecli_RunFunc( hExt, Iif( cModType=="sd", "sd__OpenModel", "OpenModel" ), ;
               {cCurrModel, Iif( Empty(cParams),Nil,cParams)}, .T. )

            i := Int( (oClient:x2 + oClient:x1)/2 )
            cQue := edi_MsgGet_ext( cLastPrompt, 4, i-30, 8, i+30, oClient:cp )

            IF ( xRes := _clillm_Wait( ,.T. ) ) == Nil
               oClient:lClose := .T.
            ELSEIF xRes == "ok"
               IF cModType == "sd"
                  nStatus := S_CNT_CREATED
                  _Textout( Time() + " Model loaded" )
               ELSE
                  IF ecli_RunFunc( hExt, "CreateContext",{} ) == "ok"
                     nStatus := S_CNT_CREATED
                     _Textout( Time() + " Model loaded" )
                  ELSE
                     nStatus := S_MODULE_STARTED
                     ecli_RunProc( hExt, "CloseModel",{} )
                     _Textout( "Can't create context" )
                  ENDIF
               ENDIF
               IF !Empty( cQue )
                  _clillm_Ask( cQue )
               ELSE
                  _Textout( "Press F2 to start dialog" )
               ENDIF
               oClient:WriteTopPane()
            ELSE
               nStatus := S_MODULE_STARTED
               _Textout( "Can't load model" )
            ENDIF
         ELSE
            //_Textout( "Failed to start module" )
            edi_Alert( "Failed to start module" )
            oClient:lClose := .T.
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
   LOCAL n_ctx := 4096, n_predict := -1, temp := 0.8, penalty_r := 1.1, top_k := 40, top_p := 0.95
   LOCAL min_p := 0.05, penalty_l := 64, t := -1

   y1 := Int( MaxRow()/2 ) - 1
   x1 := Int( MaxCol()/2 ) - 20
   x2 := x1 + 40
   y2 := y1 + 7

   aGets := { {y1,x1+4, 11, "Parameters"}, ;
      { y1+1,x1+2, 11, "n_ctx" }, { y1+1,x1+10, 0, Ltrim(Str(n_ctx)), 6 }, ;
      { y1+1,x1+20, 11, "n_predict" }, { y1+1,x1+32, 0, Ltrim(Str(n_predict)), 6 }, ;
      { y1+3,x1+2, 11, "temp" }, { y1+3,x1+10, 0, Ltrim(Str(temp)), 6 }, ;
      { y1+3,x1+20, 11, "repeat_pen" }, { y1+3,x1+32, 0, Ltrim(Str(penalty_r)), 6 }, ;
      { y1+4,x1+2, 11, "top_k" }, { y1+4,x1+10, 0, Ltrim(Str(top_k)), 6 }, ;
      { y1+4,x1+20, 11, "top_p" }, { y1+4,x1+32, 0, Ltrim(Str(top_p)), 6 }, ;
      { y1+5,x1+2, 11, "min_p" }, { y1+5,x1+10, 0, Ltrim(Str(min_p)), 6 }, ;
      { y1+5,x1+20, 11, "repeat_last" }, { y1+5,x1+32, 0, Ltrim(Str(penalty_l)), 6 }, ;
      { y1+6,x1+2, 11, "t" }, { y1+5,x1+8, 0, Ltrim(Str(t)), 6 } ;
      }

   cBuf := Savescreen( y1, x1, y2, x2 )
   @ y1, x1, y2, x2 BOX "........ "

   edi_READ( aGets )
   IF LastKey() == K_ENTER .OR. LastKey() == K_PGDN
      IF Val(AllTrim(aGets[3,4])) != n_ctx
         n_ctx := Val(AllTrim(aGets[3,4]))
         xRes += '-c ' + Ltrim(Str(n_ctx)) + " "
      ENDIF
      IF Val(AllTrim(aGets[5,4])) != n_predict
         n_predict := Val(AllTrim(aGets[5,4]))
         xRes += '-n ' + Ltrim(Str(n_predict)) + ' '
      ENDIF
      IF Val(AllTrim(aGets[7,4])) != temp
         temp := Val(AllTrim(aGets[7,4]))
         xRes += '-temp ' + Ltrim(Str(temp)) + ' '
      ENDIF
      IF Val(AllTrim(aGets[9,4])) != penalty_r
         penalty_r := Val(AllTrim(aGets[9,4]))
         xRes += '--repeat-penalty ' + Ltrim(Str(penalty_r)) + ' '
      ENDIF
      IF Val(AllTrim(aGets[11,4])) != top_k
         top_k := Val(AllTrim(aGets[11,4]))
         xRes += '--top-k ' + Ltrim(Str(top_k)) + ' '
      ENDIF
      IF Val(AllTrim(aGets[13,4])) != top_p
         top_p := Val(AllTrim(aGets[13,4]))
         xRes += '--top-p ' + Ltrim(Str(top_p)) + ' '
      ENDIF
      IF Val(AllTrim(aGets[15,4])) != min_p
         min_p := Val(AllTrim(aGets[15,4]))
         xRes += '--min-p ' + Ltrim(Str(min_p)) + ' '
      ENDIF
      IF Val(AllTrim(aGets[17,4])) != penalty_l
         penalty_l := Val(AllTrim(aGets[17,4]))
         xRes += '--repeat_last_n ' + Ltrim(Str(penalty_l)) + ' '
      ENDIF
      IF Val(AllTrim(aGets[19,4])) != t
         t := Val(AllTrim(aGets[19,4]))
         xRes += '-t ' + Ltrim(Str(t)) + ' '
      ENDIF
   ELSE
      RETURN Nil
   ENDIF

   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cBuf )

   RETURN xRes

STATIC FUNCTION _clillm_sd_SetParams()

   LOCAL xRes := "", cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets, y1, x1, x2, y2, i, j, lOk := .F.
   LOCAL t := -1

   y1 := Int( MaxRow()/2 ) - 1
   x1 := Int( MaxCol()/2 ) - 20
   x2 := x1 + 40
   y2 := y1 + 3

   aGets := { {y1,x1+4, 11, "Parameters"}, ;
      { y1+1,x1+2, 11, "t" }, { y1+1,x1+8, 0, Ltrim(Str(t)), 6 } ;
      }

   cBuf := Savescreen( y1, x1, y2, x2 )
   @ y1, x1, y2, x2 BOX "........ "

   edi_READ( aGets )
   IF LastKey() == 13
      IF Val(AllTrim(aGets[3,4])) != t
         t := Val(AllTrim(aGets[3,4]))
         xRes += 't=' + Ltrim(Str(t)) + Chr(1)
      ENDIF

      IF !Empty( xRes )
         nStatus := S_MODEL_PARAMS
         ecli_RunFunc( hExt, "sd__SetParams",{xRes} )
      ENDIF
      lOk := .T.
   ENDIF

   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cBuf )

   RETURN lOk

STATIC FUNCTION _clillm_Wait( lNoEsc, lNoTab, lShowTime )

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
         sAns := _DropQuotes( sAns )
         EXIT
      ENDIF
      IF !Empty(lShowTime) .AND. ++ nTicks >= 20
         nTicks := 0
         edi_Wait( _Timediff( nStartProc, Seconds() ),, .T. )
      ENDIF
   ENDDO

   RETURN sAns

STATIC FUNCTION _clillm_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey == K_F2
      IF nStatus == S_CNT_CREATED
         _clillm_Ask()
         RETURN -1
      ENDIF

   ELSEIF nKey == K_F3

      IF !Empty( cLastImage ) .AND. !( Left( cLastImage,1 ) == Chr(1) )
         //_ShowImage( cLastImage )
         hbc_RunPlugin( "gthwg_plug", cIniPath + "hbc_gthwg_q.hrb",, cLastImage, "dlg" )
      ENDIF

   ELSEIF nKey == K_F5
      IF Empty( cModType ) .AND. nStatus == S_CNT_CREATED
         _clillm_Reset()
         RETURN -1
      ENDIF

   ELSEIF nKey == 32
      IF nStatus == S_ASKING .OR. nStatus == S_GETTOKEN
         IF lPaused
            lPaused := .F.
            oClient:WriteTopPane()
            IF cModType == "sd"
               _clillm_Wait4ImgReady()
            ELSE
               _clillm_Wait4Answer()
            ENDIF
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

STATIC FUNCTION _clillm_Ask( cQue )

   LOCAL x := Int( (oClient:x2 + oClient:x1)/2 )
   LOCAL cImg, nPos, cParams

   lPaused := .F.
   IF Empty( cQue )
      cQue := edi_MsgGet_ext( cLastPrompt, 4, x-30, 8, x+30, oClient:cp )
   ENDIF
   IF !Empty( cQue )
      cLastPrompt := cQue
      nStatus := S_ASKING
      oClient:WriteTopPane()
      IF cModType == "sd"
         IF Empty( hb_fnameName( cImg := _NewImgName() ) )
            _Textout( cImg + " is full. Clean it before generate new images." )
            RETURN Nil
         ELSE
            cParams := "o=" + cImg
            IF Left( cQue,1 ) == '{' .AND. ( nPos := At( '}', cQue ) ) > 3
               cParams += '~' + Substr( cQue, 2, nPos-2 )
               cQue := Substr( cQue, nPos+1 )
            ENDIF
            nStartProc := Seconds()
            ecli_RunFunc( hExt, "sd__SetParams",{cParams} )
            ecli_RunFunc( hExt, "sd__Txt2Img",{cQue}, .T. )
            _Textout( "> " + cQue )
            _Textout( Time() + " Waiting for " + cImg + " ..." )
            cLastImage := Chr(1) + cImg
            _clillm_Wait4ImgReady()
         ENDIF
      ELSE
         ecli_RunFunc( hExt, "Ask",{cQue}, .T. )
         _Textout( "> " + cQue )
         //_Textout( "" )
         _Textout( Replicate( "-", 12 ) )
         _Textout( "" )
         _clillm_Wait4Answer()
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _clillm_Reset()

   ecli_RunFunc( hExt, "CloseContext",{} )
   IF ecli_RunFunc( hExt, "CreateContext",{} ) == "ok"
      nStatus := S_CNT_CREATED
      _Textout( "Ok. Press F2 to start dialog" )
   ELSE
      nStatus := S_MODULE_STARTED
      ecli_RunProc( hExt, "CloseModel",{} )
      _Textout( "Can't create context" )
   ENDIF

   RETURN Nil

STATIC FUNCTION _clillm_Wait4Answer()

   LOCAL xRes

   IF ( xRes := _clillm_Wait() ) == Nil
      // ESC pressed
      nStatus := S_CNT_CREATED
      _Textout( "Canceled." )
   ELSEIF xRes == ""
      // Ctrl-Tab
      lPaused := .T.
   ELSE
      nStatus := S_GETTOKEN
      oClient:WriteTopPane()
      ecli_RunFunc( hExt, "GetNextToken",{2}, .T. )
      DO WHILE .T.
         IF ( xRes := _clillm_Wait() ) == Nil
            // ESC pressed
            nStatus := S_CNT_CREATED
            //_Textout( "" )
            _Textout( Replicate( "-", 12 ) )
            _Textout( "" )
            EXIT
         ELSEIF xRes == ""
            // Ctrl-Tab
            lPaused := .T.
            EXIT
         ELSE
            xRes := _DropQuotes( xRes )
            IF Right( xRes,4 ) == '===='
               nStatus := S_CNT_CREATED
               //_Textout( hb_strShrink(xRes,4) + " ==", .T. )
               _Textout( hb_strShrink(xRes,4), .T. )
               //_Textout( "" )
               _Textout( Replicate( "-", 12 ) )
               _Textout( "" )
               EXIT
            ELSE
               ecli_RunFunc( hExt, "GetNextToken",{2}, .T. )
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

STATIC FUNCTION _clillm_Wait4ImgReady()

   LOCAL xRes

   edi_Wait( "00:00:00" )
   IF Empty( xRes := _clillm_Wait( .T.,, .T. ) )
      lPaused := .T.
   ELSE
      _Textout( "Done (" + _Timediff( nStartProc, Seconds() ) + "). Press F3 to view" )
      nStatus := S_CNT_CREATED
      IF !Empty( cLastImage ) .AND. Left( cLastImage,1 ) == Chr(1)
         cLastImage := Substr( cLastImage, 2 )
      ENDIF
   ENDIF
   oClient:WriteTopPane()
   edi_Wait()

   RETURN Nil

/*
STATIC FUNCTION _ShowImage( cFileName )

   LOCAL cGthwgHrb := "hbc_gthwg_q.hrb"

   IF !hb_hHaskey( FilePane():hMisc,"gthwg_plug" )
      FilePane():hMisc["gthwg_plug"] := Iif( File( cIniPath + cGthwgHrb ), ;
         hb_hrbLoad( cIniPath + cGthwgHrb ), Nil )
   ENDIF
   IF !Empty( FilePane():hMisc["gthwg_plug"] )
      hb_hrbDo( FilePane():hMisc["gthwg_plug"],, cFileName, "dlg" )
   ENDIF

   RETURN Nil
*/
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

STATIC FUNCTION _DropQuotes( s )
/*
   LOCAL nPos
   IF Left( s,1 ) == '"'
      s := Substr( s, 2, Len( s ) - 2 )
   ENDIF
   IF ( nPos := At( '\n', s ) ) > 0
      s := Iif( nPos==1, "", Left( s,nPos-1 ) ) + Chr(10) + Substr( s,nPos+2 )
   ENDIF
*/
   IF Chr(10)+Chr(10) $ s
      s := StrTran( s, Chr(10)+Chr(10), Chr(10) )
   ENDIF

   RETURN s

STATIC FUNCTION _Timediff( n1, n2 )

   LOCAL n := Int( n2 - n1 )
   RETURN Padl(Ltrim(Str(Int(n/3600))),2,'0') + ":" + Padl(Ltrim(Str(Int((n%3600)/60))),2,'0') + ;
      ":" + Padl(Ltrim(Str(Int(n%60))),2,'0')

STATIC FUNCTION _NewImgName()

   LOCAL i, cRes, cImg := Iif( !Empty(aModels[nCurrModel,4]), aModels[nCurrModel,4], ;
      Iif( !Empty(cImgPath), cImgPath, hb_DirTemp() ) ) + ;
      Iif( !Empty(aModels[nCurrModel,5]), aModels[nCurrModel,5], ;
      Iif( !Empty(cImgPrefix), cImgPrefix, "out_" ) )

      FOR i := 1 TO 9999
         IF !File( cRes := ( cImg + PAdl( Ltrim(Str(i)), 4, '0' ) + ".png" ) )
            RETURN cRes
         ENDIF
      NEXT

   RETURN hb_fnameDir( cImg )

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
            AAdd( aModels, { s2, "", "", "", "" } )
         ELSEIF s1 == "c" .OR. s1 == "n" .OR. s1 == "temp" .OR. s1 == "repeat-penalty" ;
            .OR. s1 == "top-k" .OR. s1 == "top-n" .OR. s1 == "n-keep" ;
            .OR. s1 == "penalize-nl" .OR. s1 == "min-p" .OR. s1 == "tb"
            ATail( aModels )[2] += s1 + '=' + s2 + '~'
         ELSEIF s1 == "mod-type"
            ATail( aModels )[3] := s2
         ELSEIF s1 == "img-path"
            IF !( Right( s2,1 ) $ "\/" )
               s2 += hb_ps()
            ENDIF
            IF Empty( aModels )
               cImgPath := s2
            ELSE
               ATail( aModels )[4] := s2
            ENDIF
         ELSEIF s1 == "img-prefix"
            IF Empty( aModels )
               cImgPrefix := s2
            ELSE
               ATail( aModels )[5] := s2
            ENDIF
         ELSEIF s1 == "log"
            nLogLevel := Val( s2 )
         ENDIF
      ENDIF
   NEXT

   RETURN Nil