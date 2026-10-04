/*
 * Simple AI Agent
 * HbEdit plugin
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"

DYNAMIC LLM_Service, LLM_OpenAI, LLM_Llama, LLM_Gigachat

STATIC oClient, cPlugPath
STATIC oService

FUNCTION plug_aiagent( oEdit, cPath )

   LOCAL cHrb := "aiagent_class.hrb"
   LOCAL cName := "$AI Agent"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      DevPos( y, o:x1 )
      DevOut( "AI Agent   F2: Menu" )

      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      IF oClient:lClose
         LLM_Service():ClearContext()
      ENDIF
      RETURN Nil
   }

   cPlugPath := cPath

   IF !edi_CheckCurl()
      RETURN Nil
   ENDIF
   IF !hb_hHaskey( FilePane():hMisc,"aiagent_class" )
      FilePane():hMisc["aiagent_class"] := hb_hrbLoad( cPath + cHrb )
      LLM_Service():cPromptsPath := "plugins" + hb_ps() + "ai_prompts"
      LLM_Service():cToolsPath := "plugins" + hb_ps() + "ai_tools"
      LLM_Service():cSkillsPath := "plugins" + hb_ps() + "ai_skills"
      LLM_Service():cLogPath := "plugins" + hb_ps() + "ai_log"
      ag_RdIni()
   ENDIF

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
      "  F2 - Menu" + Chr(10) + ;
      "  Ctrl-Tab - Switch Buffer" + Chr(10) + "  F10 - Exit" + Chr(10)

   ag_Textout( oService:id + ": " + oService:cUrl )

   RETURN Nil

STATIC FUNCTION ag_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey == K_F3
      ag_Ask()

   ELSEIF nKey == K_F9

      ag_Menu()
      RETURN -1

   ELSEIF nKey == K_CTRL_N

      oService:ClearContext()
      ag_Textout( Chr(10) + Replicate( '-', 24 ) + Chr(10) )

   ELSEIF nKey == K_CTRL_S

      IF Empty( oService:aHistory )
         ag_SystemPrompt( .F. )
      ENDIF

   ELSEIF nKey == K_CTRL_T

      IF Empty( oService:aHistory )
         ag_SystemPrompt( .T. )
      ENDIF

   ELSEIF nKey == K_F1

      mnu_Help( oClient )
      hb_cdpSelect( oClient:cp )
      RETURN -1

   ENDIF

   RETURN 0

STATIC FUNCTION ag_Menu()

   LOCAL aMenu := { {"Send new prompt",,,"F3"}, {"Set system prompt",,,"Ctrl-S"}, ;
   {"Clear context",,,"Ctrl-N"}, {"Change model",,}, {"Exit",,,"F10"} }
   LOCAL i, xVal

   i := FMenu( oClient, aMenu, oClient:y1+2, oClient:x1+4 )
   IF i == 1
      ag_Ask()

   ELSEIF i == 2
      ag_SystemPrompt( .F. )

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

   LOCAL cQue, aAnswer

   IF !Empty( cQue := edi_MsgGet_ext( "", oClient:y1+2, oClient:x1+4, oClient:y1+10, oClient:x2-12, oClient:cp ) )
      ag_Textout( cQue )
      ag_Textout( ">>> Wait <<<" )
      oService:cPrompt := cQue
      aAnswer := oService:Send()
      IF !Empty( aAnswer )
         ag_Textout( aAnswer[1] )
      ELSE
         ag_Textout( "No answer" )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION ag_SystemPrompt( lAddTools )

   LOCAL cQue

   IF lAddTools
      oService:AddTools()
   ENDIF
   IF !Empty( cQue := edi_MsgGet_ext( oService:cSystem, oClient:y1+2, oClient:x1+4, oClient:y1+10, oClient:x2-12, oClient:cp ) )
      oService:cSystem := cQue
   ENDIF

   RETURN Nil

STATIC FUNCTION ag_Textout( cLine )

   LOCAL n := Len( oClient:aText ), nf := 1

   IF n == 1 .AND. Empty( oClient:aText[1] )
      oClient:aText[1] := cLine
   ELSE
      n ++
      oClient:InsText( n, 1, cLine )
      nf := Max( 1, Row() - oClient:y1 )
   ENDIF

   oClient:TextOut( nf )

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
   IF !Empty( oService )
      oNew:cSystem := oService:cSystem
      oNew:aHistory := oService:aHistory
      oService:aHistory := {}
      oNew:lToolsAutoRun := oService:lToolsAutoRun
   ENDIF

   RETURN oNew

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
            LLM_Service():SetOptions( aSect )

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
      cEol + ;
      "[OPENAI]" + cEol + "id=llama" + cEol

   hb_MemoWrit( cPlugPath + "plug_aiagent.ini", s )

   RETURN Nil