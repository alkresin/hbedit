
#define K_ENTER      13
#define K_ESC        27
#define K_CTRL_TAB  404
#define K_SH_TAB    271
#define K_F1         28
#define K_F2         -1
#define K_F3         -2
#define K_F5         -4
#define K_F6         -5
#define K_F9         -8
#define K_F10        -9
#define K_PGDN        3

DYNAMIC LLM_Service, LLM_OpenAI, LLM_Llama, LLM_Gigachat

STATIC oClient, cPlugPath
STATIC oService
STATIC cPathPrompt := "prompts"
STATIC cPathTool   := "tools"
STATIC cPathSkill  := "skills"

FUNCTION plug_aiagent( oEdit, cPath )

   LOCAL cHrb := "aiagent_class.hrb", aList := {}, i
   LOCAL cName := "$AI Agent"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      DevPos( y, o:x1 )
      DevOut( "AI Agent   F3-Ask  F5-New dialog  " + ;
         Iif( Empty( oService:aHistory ), "F6-System prompt", Space(16) ) )
      DevPos( nRow, nCol )
      RETURN Nil
   }

   cPlugPath := cPath

   IF !edi_CheckCurl()
      RETURN Nil
   ENDIF
   IF !hb_hHaskey( FilePane():hMisc,"aiagent_class" )
      FilePane():hMisc["aiagent_class"] := hb_hrbLoad( cPath + cHrb )
      ag_RdIni()
   ENDIF

   oClient := mnu_NewBuf( oEdit )
   oClient:cFileName := cName
   oClient:bWriteTopPane := bWPane
   oClient:bOnKey := {|o,n| ag_OnKey(o,n) }
   oClient:cp := "UTF8"
   hb_cdpSelect( oClient:cp )
   oClient:lUtf8 := .T.
   oClient:lWrap := .T.

   FOR i := 1 TO Len( LLM_Service():aList )
      AAdd( aList, LLM_Service():aList[i]:id )
   NEXT

   i := 1
   IF Empty( aList ) .OR. ( Len( aList ) > 1 .AND. ( ( i := FMenu( oEdit, aList ) ) == 0 ) )
      RETURN Nil
   ENDIF
   oService := LLM_Service():aList[i]

   ag_Textout( oService:cUrl )
   ag_Ask()

   RETURN Nil

STATIC FUNCTION ag_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey == K_F3
      ag_Ask()

   ELSEIF nKey == K_F5

      oService:ClearContext()
      ag_Textout( Chr(10) + Replicate( '-', 24 ) + Chr(10) )

   ELSEIF nKey == K_F6

      IF Empty( oService:aHistory )
         ag_SystemPrompt()
      ENDIF

   ENDIF

   RETURN 0

STATIC FUNCTION ag_Ask()

   LOCAL cQue, aAnswer

   IF !Empty( cQue := edi_MsgGet_ext( "", oClient:y1+2, oClient:x1+4, oClient:y1+10, oClient:x2-12, oClient:cp ) )
      ag_Textout( cQue )
      ag_Textout( ">>> Wait <<<" )
      aAnswer := oService:Ask( ,cQue )
      IF !Empty( aAnswer )
         ag_Textout( aAnswer[1] )
      ELSE
         ag_Textout( "No answer" )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION ag_SystemPrompt()

   LOCAL cQue

   oService:AddTools()
   oService:AddSkills()
   IF !Empty( cQue := edi_MsgGet_ext( oService:cSystem, oClient:y1+2, oClient:x1+4, oClient:y1+10, oClient:x2-12, oClient:cp ) )
      oService:cSystem := cQue
   ENDIF

   RETURN Nil

STATIC FUNCTION ag_Textout( cLine )

   LOCAL n := Len( oClient:aText ), nf

   n ++
   nf := n
   oClient:InsText( n, 1, cLine )

   nf := Max( 1, Row() - oClient:y1 )
   oClient:TextOut( nf )

   RETURN Nil

STATIC FUNCTION ag_RdIni()

   LOCAL cFile := "plug_aiagent.ini", hIni, aIni, aSect, nSect, cTmp

   IF !File( cPlugPath + cFile )
      WriteIni()
   ENDIF

   hIni := edi_IniRead( cPlugPath + cFile )

   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF aIni[nSect] == "MAIN" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            IF hb_hHaskey( aSect, cTmp := "path_prompts" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               cPathPrompt := Lower( cTmp )
            ENDIF
            IF hb_hHaskey( aSect, cTmp := "path_tools" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               cPathTool := Lower( cTmp )
            ENDIF
            IF hb_hHaskey( aSect, cTmp := "path_skills" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               cPathSkill := Lower( cTmp )
            ENDIF

         ELSEIF Left(aIni[nSect],6) == "OPENAI" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            LLM_Llama():New( aSect )

         ELSEIF Left(aIni[nSect],8) == "GIGACHAT" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            LLM_Gigachat():New( aSect )

         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION WriteIni()

   LOCAL cEol := Chr(10)

   LOCAL s := "[MAIN]" + cEol + ;
      "path_prompts=" + cPathPrompt + cEol + ;
      "path_tools=" + cPathTool + cEol + ;
      "path_skills=" + cPathSkill + cEol + ;
      cEol + ;
      "[OPENAI]" + cEol + "id=llama" + cEol

   hb_MemoWrit( cPlugPath + "plug_aiagent.ini", s )

   RETURN Nil