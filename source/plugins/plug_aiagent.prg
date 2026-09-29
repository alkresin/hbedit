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
STATIC cPathPrompt := "prompts"

FUNCTION plug_aiagent( oEdit, cPath )

   LOCAL cHrb := "aiagent_class.hrb", aList := {}, i
   LOCAL cName := "$AI Agent"
   LOCAL bWPane := {|o,l,y|
      LOCAL nCol := Col(), nRow := Row()
      DevPos( y, o:x1 )
      DevOut( "AI Agent   F3:Ask  Ctrl-N:New dialog  " + ;
         Iif( Empty( oService:aHistory ), "Ctrl-S:System prompt  Ctrl-T:Add tools", "" ) )
      DevPos( nRow, nCol )
      RETURN Nil
   }
   LOCAL bEndEdit := {||
      IF oClient:lClose
         //LLM_Service():cSystem := ""
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
      LLM_Service():cToolsPath := "plugins" + hb_ps() + "ai_tools"
      LLM_Service():cSkillsPath := "plugins" + hb_ps() + "ai_skills"
      LLM_Service():cLogPath := "plugins" + hb_ps() + "ai_log"
      ag_RdIni()
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

   FOR i := 1 TO Len( LLM_Service():aList )
      AAdd( aList, LLM_Service():aList[i]:id )
   NEXT

   i := 1
   IF Empty( aList ) .OR. ( Len( aList ) > 1 .AND. ( ( i := FMenu( oEdit, aList ) ) == 0 ) )
      RETURN Nil
   ENDIF
   oService := LLM_Service():aList[i]

   ag_Textout( oService:id + ": " + oService:cUrl )

   RETURN Nil

STATIC FUNCTION ag_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey == K_F3
      ag_Ask()

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

   ENDIF

   RETURN 0

STATIC FUNCTION ag_Ask()

   LOCAL cQue, aAnswer

   IF !Empty( cQue := edi_MsgGet_ext( "", oClient:y1+2, oClient:x1+4, oClient:y1+10, oClient:x2-12, oClient:cp ) )
      ag_Textout( cQue )
      ag_Textout( ">>> Wait <<<" )
      aAnswer := oService:Send( ,cQue )
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
            IF hb_hHaskey( aSect, cTmp := "path_tools" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               LLM_Service():cToolsPath := Lower( cTmp )
            ENDIF
            IF hb_hHaskey( aSect, cTmp := "path_skills" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               LLM_Service():cSkillsPath := Lower( cTmp )
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
      cEol + ;
      "[OPENAI]" + cEol + "id=llama" + cEol

   hb_MemoWrit( cPlugPath + "plug_aiagent.ini", s )

   RETURN Nil