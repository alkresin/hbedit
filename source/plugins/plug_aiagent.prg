/*
 * Simple AI Agent
 * HbEdit plugin
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"

DYNAMIC LLM_Service, LLM_OpenAI, LLM_Llama, LLM_Gigachat
DYNAMIC HWINDOW, HWG_PROCESSMESSAGE

STATIC oClient, cPlugPath
STATIC oService
STATIC nDelayInCycle := 1

FUNCTION plug_aiagent( oEdit, cPath )

   LOCAL cHrb := "aiagent_class.hrb", cProjPath
   LOCAL cName := "$AI Agent"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      DevPos( y, o:x1 )
      DevOut( "AI Agent   F2: Menu  F3: New prompt F5: Cycle" )

      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      LOCAL i
      IF oClient:lClose
         FOR i := 1 TO Len( LLM_Service():aList )
            LLM_Service():aList[i]:ClearContext()
            LLM_Service():aList[i]:cSystem := ""
            LLM_Service():aList[i]:aHistory := {}
            LLM_Service():aList[i] := Nil
         NEXT
         LLM_Service():aList := {}
         oService := Nil
      ENDIF
      RETURN Nil
   }

   cPlugPath := cPath

   IF !edi_CheckCurl()
      RETURN Nil
   ENDIF

   IF !hb_hHaskey( FilePane():hMisc,"aiagent_class" )
      FilePane():hMisc["aiagent_class"] := hb_hrbLoad( cPath + cHrb )
   ENDIF

   LLM_Service():cWorkDir    := "ai_work"
   LLM_Service():cPromptsDir := "ai_prompts"
   LLM_Service():cToolsDir   := "ai_tools"
   LLM_Service():cSkillsDir  := "ai_skills"
   LLM_Service():cLogDir     := "ai_log"

   LLM_Service():cIniFile := "plug_aiagent.ini"
   LLM_Service():cProjPath := LLM_Service():cBasePath := cPlugPath
   cProjPath := Iif( hb_Version(20), "/", hb_curDrive() + ":\" ) + CurDir() + hb_ps()
   IF File( cProjPath + LLM_Service():cIniFile )
      LLM_Service():cProjPath := cProjPath
   ELSE
      IF edi_Alert( NameShortcut( cProjPath, 42, '~', oEdit:lUtf8 ) + ;
         ";Do you want to use agent in this directory;and create " + ;
         LLM_Service():cIniFile + " here?", "No", "Yes" ) == 2

         LLM_Service():cProjPath := cProjPath
         IF !File( cPlugPath + LLM_Service():cIniFile )
            WriteIni( cPlugPath + LLM_Service():cIniFile )
         ENDIF
         hb_vfCopyFile( cPlugPath + LLM_Service():cIniFile, cProjPath + LLM_Service():cIniFile )
         IF !hb_dirExists( cProjPath + LLM_Service():cLogDir )
            MakeDir( cProjPath + LLM_Service():cLogDir )
         ENDIF
         IF !hb_dirExists( cProjPath + LLM_Service():cWorkDir )
            MakeDir( cProjPath + LLM_Service():cWorkDir )
         ENDIF
         IF !hb_dirExists( cProjPath + LLM_Service():cPromptsDir )
            MakeDir( cProjPath + LLM_Service():cPromptsDir )
         ENDIF
      ENDIF
   ENDIF

   ag_RdIni( LLM_Service():cProjPath + LLM_Service():cIniFile )

   IF Empty( oService := ag_SelectModel() )
      RETURN Nil
   ENDIF

   oClient := mnu_NewBuf( oEdit )
   oClient:cFileName := cName
   oClient:bWriteTopPane := bWPane
   oClient:bOnKey := {|o,n| ag_OnKey(o,n) }
   oClient:bEndEdit := bEndEdit
   oClient:cp := "UTF8"
   hb_cdpSelect( oClient:cp )
   oClient:lUtf8 := .T.
   oClient:lWrap := .T.

   oClient:hCargo := hb_hash()
   oClient:hCargo["help"] := "AI Agent hot keys" + Chr(10) + ;
      "  F2 - Menu" + Chr(10) + "  F3 - New prompt" + Chr(10) + "  F5 - Run agent cycle" + Chr(10) + ;
      "  Ctrl-Tab - Switch Buffer" + Chr(10) + "  F10 - Exit" + Chr(10)

   ag_Textout( oService:id + ": " + oService:cUrl )

   RETURN Nil

STATIC FUNCTION ag_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey == K_F3
      ag_Ask()

   ELSEIF nKey == K_F2

      ag_Menu()
      RETURN -1

   ELSEIF nKey == K_F5

      ag_Cycle()
      RETURN -1

   ELSEIF nKey == K_F1

      mnu_Help( oClient )
      hb_cdpSelect( oClient:cp )
      RETURN -1

   ENDIF

   RETURN 0

STATIC FUNCTION ag_Menu()

   LOCAL aMenu := { {"New prompt",,,"F3"}, {"Run agent cycle",,,"F5"}, ;
   {"Clear context",,}, {"Change model",,}, {"Exit",,,"F10"} }
   LOCAL i, xVal

   i := FMenu( oClient, aMenu, oClient:y1+2, oClient:x1+4 )
   IF i == 1
      ag_Ask()

   ELSEIF i == 2
      ag_Cycle()

   ELSEIF i == 3
      oService:ClearContext()
      ag_Textout( Chr(10) + Replicate( '-', 24 ) + Chr(10) )

   ELSEIF i == 4
      IF !Empty( xVal := ag_SelectModel() )
         oService := xVal
         ag_Textout( Chr(10) + oService:id + ": " + oService:cUrl )
      ENDIF

   ENDIF

   RETURN Nil

STATIC FUNCTION ag_Ask()

   LOCAL cPrompt := "", aAnswer, cPath
   STATIC nPrompt := 1

   IF Empty( oService:aHistory )
      nPrompt := 1
      IF ag_SystemPrompt()
         ag_Textout( "--- System prompt ---" )
         ag_Textout( oService:ParsePrompt( oService:cSystem ) )
      ENDIF
      IF File( cPath :=  ( LLM_Service():cProjPath + LLM_Service():cPromptsDir + ;
         hb_ps() + "tick_init_prompt.txt" ) )
         cPrompt := Memoread( cPath )
      ENDIF
   ELSE
      cPrompt := oService:cPrompt
   ENDIF
   IF !Empty( cPrompt := edi_MsgGet_ext( cPrompt, oClient:y1+2, oClient:x1+4, ;
      oClient:y1+10, oClient:x2-12, oClient:cp,,, "Prompt" ) )
      ag_Textout( "------ Prompt " + Ltrim(Str( nPrompt )) + " ------" )
      nPrompt ++
      ag_Textout( oService:ParsePrompt( cPrompt ) )
      ag_Textout( "------ Answer ------" )
      edi_Wait( "Wait..." )
      oService:cPrompt := cPrompt
      aAnswer := oService:Send()
      edi_Wait()
      IF !Empty( aAnswer )
         ag_Textout( aAnswer[1] )
      ELSE
         ag_Textout( "No answer" )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION ag_Cycle()

   LOCAL cPath, cInitPrompt := "", n := 0, aParams

   IF ( aParams := ag_GetCycleParams() ) == Nil
      RETURN Nil
   ENDIF
   nDelayInCycle := Max( 0.1, aParams[2] )

   IF aParams[3]
      oService:pFuncMsg := @cbFuncMsg()
   ENDIF
   IF Empty( oService:aHistory )
      IF ag_SystemPrompt()
         ag_Textout( "--- System prompt ---" )
         ag_Textout( oService:ParsePrompt( oService:cSystem ) )
      ELSE
         RETURN Nil
      ENDIF
   ENDIF
   IF File( cPath := ( LLM_Service():cProjPath + LLM_Service():cPromptsDir + hb_ps() + "tick_init_prompt.txt" ) )
      cInitPrompt := Memoread( cPath )
   ENDIF

   IF ( !Empty( cInitPrompt := edi_MsgGet_ext( cInitPrompt, oClient:y1+2, oClient:x1+4, ;
      oClient:y1+10, oClient:x2-12, oClient:cp,,, "Prompt" ) ) .OR. !Empty( oService:cSystem ) ) .AND. ;
      Lastkey() != K_ESC

      oService:cPrompt := cInitPrompt
      edi_Wait( "Wait (1)" )
      oService:MainCycle( @cbFunc(), aParams[1], aParams[4] )
      edi_Wait()
      oService:Log( "--------------" + Chr(10) )

   ENDIF

   RETURN Nil

STATIC FUNCTION ag_GetCycleParams()

   LOCAL y1 := 5, x1 := Int(MaxCol()/2)-15, x2 := x1+30
   LOCAL cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets := { ;
      {y1+1,x1+2, 11, "Cycles:"}, ;
      { y1+1,x1+10, 0, Ltrim(Str(oService:nCyclesMax)), 3 }, ;
      {y1+2,x1+2, 11, "Delay (sec):"}, ;
      { y1+2,x1+15, 0, "1", 4 }, ;
      {y1+4,x1+3, 1, .T., 1 }, {y1+4,x1+2, 11, "[x] Ask Man" }, ;
      {y1+5,x1+3, 1, .T., 1 }, {y1+5,x1+2, 11, "[x] Clear context" } }

   cBuf := Savescreen( y1, x1, y1 + 6, x2 )
   @ y1, x1, y1 + 6, x2 BOX "┌─┐│┘─└│ "
   @ y1+3, x1 SAY "├"
   @ y1+3, x2 SAY "┤"
   @ y1+3, x1+1 TO y1+3, x2-1

   edi_READ( aGets )
   Restscreen( y1, x1, y1 + 6, x2, cBuf )
   SetColor( oldc )
   IF LastKey() == K_ESC
      RETURN Nil
   ENDIF

   RETURN { Val(aGets[2,4]), Val(aGets[4,4]), aGets[5,4], aGets[7,4] }

STATIC FUNCTION cbFuncMsg( oService, pArr )

   LOCAL cPrompt

   IF !Empty( cPrompt := edi_MsgGet_ext( oService:cPrompt, oClient:y1+2, oClient:x1+30, ;
      oClient:y1+9, oClient:x2-4, oClient:cp,,, "Answer_to_Message_for_man" ) )
      oService:cPrompt := cPrompt
   ENDIF
   RETURN Nil

STATIC FUNCTION cbFunc( cUserPrompt, aAnswer, n )

   LOCAL nSec, nKey

   edi_Wait()
   ag_Textout( "------ " + Iif( n<0, "Last", Ltrim(Str(n)) ) + " ------" )
   IF !Empty( cUserPrompt )
      ag_Textout( cUserPrompt )
   ENDIF
   ag_Textout( Replicate( "=", 15 ) )
   IF !Empty( aAnswer )
      ag_Textout( aAnswer[1] )
   ELSE
      ag_Textout( "Empty answer" )
   ENDIF
   ag_Textout( Chr(10) )
   IF n < 0
      ag_Textout( "Finish!" )
      RETURN Nil
   ENDIF
   edi_Wait( "Wait (" + Ltrim(Str(n+1)) + ")" )

   nSec := Seconds()
   DO WHILE Seconds() - nSec < nDelayInCycle
      IF ( nKey := Inkey( 0.1 ) ) == K_SPACE
         EXIT
      ELSEIF nKey == K_ESC
         oService:lFinish := .T.
         EXIT
      ENDIF
   ENDDO

   RETURN Nil

STATIC FUNCTION ag_SystemPrompt()

   LOCAL cQue, cPath

   IF File( cPath := ( LLM_Service():cProjPath + LLM_Service():cPromptsDir + hb_ps() + "system.txt" ) )
      oService:SetSystemPrompt( Memoread( cPath ) )
   ENDIF
   IF !Empty( cQue := edi_MsgGet_ext( oService:cSystem, oClient:y1+2, oClient:x1+4, ;
      oClient:y1+10, oClient:x2-12, oClient:cp,,, "System_prompt" ) )
      oService:cSystem := cQue
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION ag_Textout( cLine )

   LOCAL n := Len( oClient:aText )

   IF n == 1 .AND. Empty( oClient:aText[1] )
      oClient:aText[1] := cLine
   ELSE
      n ++
      oClient:InsText( n, 1, cLine )
   ENDIF

   oClient:TextOut()
   oClient:lWrap := .F.
   edi_Move( oClient, 71 )   // Go to end
   oClient:lWrap := .T.
   edi_Move( oClient, 106 )  // Go down
   IF hb_isFunction( "HWINDOW" )
      HWindow():GetMain():Refresh()
      hwg_ProcessMessage()
      //hwg_ProcessMessage()
      //hwg_ProcessMessage()
   ENDIF
   cedi_Sleep( 50 )
   Inkey( 0.1 )

   RETURN Nil

STATIC FUNCTION ag_SelectModel()

   LOCAL aList := {}, i, oNew

   FOR i := 1 TO Len( LLM_Service():aList )
      AAdd( aList, LLM_Service():aList[i]:id )
   NEXT

   i := 1
   IF Empty( aList ) .OR. ( Len( aList ) > 1 .AND. ( ( i := FMenu( oClient, aList ) ) == 0 ) )
      RETURN Nil
   ENDIF

   oNew := LLM_Service():aList[i]
   IF !Empty( oService ) .AND. !( oNew == oService )
      oNew:cSystem := oService:cSystem
      oNew:aHistory := oService:aHistory
      oService:aHistory := {}
   ENDIF

   RETURN oNew

STATIC FUNCTION ag_RdIni( cIni )

   LOCAL hIni, aIni, aSect, nSect, cTmp

   hIni := edi_IniRead( cIni )

   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      aIni := hb_hKeys( hIni )
      IF hb_hHaskey( hIni, cTmp := "MAIN" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         LLM_Service():Init( aSect )
      ENDIF
      FOR nSect := 1 TO Len( aIni )
         IF Left(aIni[nSect],6) == "OPENAI" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            LLM_Llama():New( aSect )

         ELSEIF Left(aIni[nSect],8) == "GIGACHAT" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            LLM_Gigachat():New( aSect )

         ENDIF
      NEXT
   ENDIF
   RETURN Nil

STATIC FUNCTION WriteIni( cIni )

   LOCAL cEol := Chr(10)

   LOCAL s := "[MAIN]" + cEol + cEol + ;
      "[OPENAI]" + cEol + "id=llama" + cEol

   hb_MemoWrit( cIni, s )

   RETURN Nil

FUNCTION ag_GetHtml( cUrl )

   LOCAL cResult

   cedi_RunConsoleApp( "curl -s -L " + cUrl,, @cResult )

   RETURN cResult

FUNCTION ag_CleanHTML( cText )

   // 1. Удаляем блоки <head>...</head>, <script>...</script> и <style>...</style>
   //    (?is): i - игнорировать регистр, s - точка соответствует переводу строки.
   cText := ag_regexReplace( "(?is)<head.*?>.*?</head>", cText, "", .F. )
   cText := ag_regexReplace( "(?is)<script.*?>.*?</script>", cText, "", .F. )
   cText := ag_regexReplace( "(?is)<style.*?>.*?</style>", cText, "", .F. )

   // 2. Удаляем HTML-комментарии <!-- ? -->.
   cText := ag_regexReplace( "(?s)<!--.*?-->", cText, "", .F. )

   // 3. Удаляем <img...>
   cText := ag_regexReplace( "(?is)<img.*?>", cText, "", .F. )

   // 4. Удаляем все оставшиеся теги (включая <img>, <h1>-<h6>, <a> и т.д.).
   //cText := ag_regexReplace( "<[^>]+>", cText, "", .F. )

   // 5. Декодируем основные HTML-сущности.
   cText := StrTran( cText, "&nbsp;", " " )
   cText := StrTran( cText, "&amp;",  "&" )
   cText := StrTran( cText, "&lt;",   "<" )
   cText := StrTran( cText, "&gt;",   ">" )
   cText := StrTran( cText, "&quot;", '"' )
   cText := StrTran( cText, "&#39;",  "'" )
   cText := StrTran( cText, "&apos;", "'" )

   // 6. Нормализация пробелов: множественные пробелы, табы и переводы строк - в один пробел.
   cText := ag_regexReplace( "\s+", cText, " ", .F. )

   RETURN cText

#define MATCH_STRING  1
#define MATCH_START   2
#define MATCH_END     3

FUNCTION ag_regexReplace( cRegex, cString, cReplace, lCaseSensitive, lNewLine, nMaxMatches, nGetMatch )

   LOCAL aMatches, aMatch
   LOCAL cReturn
   LOCAL nOffSet := 0
   LOCAL cSearch, nStart, nLenSearch, nLenReplace

   aMatches := hb_regexAll( cRegEx, cString, lCaseSensitive, lNewLine, nMaxMatches, ;
      Iif( nGetMatch==Nil, 0, nGetMatch ), .F. )
   cReturn := cString

   IF ! Empty( aMatches )
      FOR EACH aMatch IN aMatches
         IF HB_ISARRAY( aMatch ) .AND. Len( aMatch ) >= 1 .AND. ;
            HB_ISARRAY( aMatch[ 1 ] )
            aMatch := aMatch[ 1 ]
            IF Len( aMatch ) == 3 // if regex matches I must have an array of 3 elements
               cSearch := aMatch[ MATCH_STRING ]
               nStart  := aMatch[ MATCH_START ]
               nLenSearch  := Len( cSearch )
               nLenReplace := Len( cReplace )
               cReturn := Stuff( cReturn, nStart - nOffSet, nLenSearch, cReplace )
               nOffSet += nLenSearch - nLenReplace
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN cReturn