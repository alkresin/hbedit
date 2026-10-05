/*
 * Set of classes to communicate with LLMs via API
 */

#include "hbclass.ch"

STATIC cMessageFromMan := "message_from_man.txt"

#ifdef _HBEDIT_PLUGIN
#define  _LETO
#xtranslate _RunConsoleApp([<n,...>])  => cedi_RunConsoleApp(<n>)
FUNCTION Aiagent_class
   RETURN Nil
#endif

CLASS LLM_Service

   CLASS VAR aList SHARED INIT {}
   CLASS VAR nLogLevel    SHARED INIT 1
   CLASS VAR cBasePath    SHARED INIT ""
   CLASS VAR cWorkPath    SHARED INIT "work"
   CLASS VAR cLogPath     SHARED
   CLASS VAR cPromptsPath SHARED INIT "prompts"
   CLASS VAR cToolsPath   SHARED INIT "tools"
   CLASS VAR cSkillsPath  SHARED INIT "skills"
   CLASS VAR aTools       SHARED INIT {}
   CLASS VAR aSysTools    SHARED INIT {}
   CLASS VAR aSkills      SHARED INIT {}
   CLASS VAR nCyclesMax   SHARED INIT 5

   DATA id         INIT ""
   DATA cSystem    INIT ""
   DATA cPrompt    INIT ""
   DATA aHistory   INIT {}
   DATA lToolsAutoRun  INIT .T.
   DATA lFinish    INIT .F.

   DATA cUrl
   DATA cEndPoint INIT ""
   DATA key
   DATA cModelDef

   DATA cSertif

   METHOD New()
   METHOD SetQuery( cModel )
   METHOD ParsePrompt( cText )
   METHOD ParseResult( cResult )
   METHOD SetSystemPrompt( cText )
   METHOD LoadTools()
   METHOD AddTools()
   METHOD RunTool( cToolName, pParams )
   METHOD RunSysTool( cToolName, pParams )
   METHOD AddEvent( cType, pOpt, cValue )
   METHOD AddMsgFromMan()
   METHOD AddSkills()
   METHOD ClearContext()
   METHOD MainCycle( pFunc )
   METHOD SetOptions( pOptions )
   METHOD Log( cText )

ENDCLASS

METHOD New( cId ) CLASS LLM_Service

   AAdd( ::aList, Self )

   RETURN Self

METHOD SetQuery( cModel ) CLASS LLM_Service

   LOCAL pArr := hb_hash()

   IF !Empty( cModel )
      pArr["model"] := cModel
   ENDIF

   IF Empty( ::aHistory )
      IF !Empty( ::cSystem )
         AAdd( ::aHistory, hb_hash( "role", "system", "content", ::ParsePrompt( ::cSystem ) ) )
      ENDIF
   ENDIF

   AAdd( ::aHistory, hb_hash( "role", "user", "content", ::ParsePrompt( ::cPrompt ) ) )
   ::cPrompt := ""
   pArr["messages"] := ::aHistory

   RETURN hb_jsonEncode( pArr )

METHOD ParsePrompt( cText ) CLASS LLM_Service

   LOCAL nPos := 1, nPos1, nPos2, nPos3, cTool, cParams, arr1, cResult

   DO WHILE ( nPos := hb_At( "<systool_", cText, nPos ) ) > 0
      IF ( nPos2 := hb_At( ">", cText, nPos + 9 ) ) == 0
         EXIT
      ENDIF
      cTool := Trim( Substr( cText, nPos+1, nPos2 - nPos - 1 ) )
      nPos1 := nPos2 + 1
      IF ( nPos2 := hb_At( "</systool_", cText, nPos1 ) ) == 0 .OR. ;
         ( nPos3 := hb_At( ">", cText, nPos2 + 9 ) ) == 0
         EXIT
      ENDIF
      cParams := AllTrim( Strtran( Strtran( Substr( cText,nPos1,nPos2-nPos1 ), ;
         Chr(10), " " ), Chr(13), " " ) )
      arr1 := Nil
      IF !Empty( cParams )
         hb_jsonDecode( cParams, @arr1 )
      ENDIF
      cResult := ::RunSysTool( cTool, arr1 )
      cText := Left( cText, nPos-1 ) + cResult + Substr( cText, nPos3+1 )
   ENDDO

   RETURN cText

METHOD ParseResult( cResult ) CLASS LLM_Service

   LOCAL pArr, arr, arr1, cContent, cReason, nPos, nPos2, cToolName, aTools := {}, i

   ::Log( cResult, "<--" )
   hb_jsonDecode( cResult, @pArr )
   IF !Empty( pArr ) .AND. !Empty( arr := hb_hGetDef( pArr, "choices", Nil ) )
      cContent := arr[1]["message"]["content"]
      cReason := hb_hGetDef( arr[1]["message"], "reasoning_content", "" )
      AAdd( ::aHistory, hb_hash( "role", "assistant", "content", cContent ) )

      nPos := 1
      DO WHILE ( nPos := hb_At( "<tool_call", cContent, nPos ) ) > 0
         nPos += 11
         DO WHILE Substr( cContent, nPos, 1 ) == " "; nPos ++; ENDDO
         IF Substr( cContent, nPos, 4 ) == "name"
            nPos += 4
            DO WHILE Substr( cContent, nPos, 1 ) $ [ ="']; nPos ++; ENDDO
            nPos2 := nPos + 1
            DO WHILE !(Substr( cContent, nPos2, 1 ) $ [ "']); nPos2 ++; ENDDO
            cToolName := SubStr( cContent, nPos, nPos2-nPos )
            nPos := hb_At( ">", cContent, nPos2 )
            nPos ++
            arr1 := Nil
            hb_jsonDecode( Substr( cContent, nPos ), @arr1 )
            AAdd( aTools, { cToolName, arr1 } )
         ENDIF
      ENDDO
      IF ::lToolsAutoRun
         FOR i := 1 TO Len( aTools )
            ::RunTool( aTools[i,1], aTools[i,2] )
         NEXT
         RETURN { cContent, cReason }
      ENDIF

      RETURN { cContent, cReason, aTools }
   ENDIF

   RETURN Nil

METHOD SetSystemPrompt( cText ) CLASS LLM_Service

   ::cSystem := Iif( Empty(cText), "", cText )
   ::AddTools()
   ::AddSkills()

   RETURN Nil

METHOD LoadTools() CLASS LLM_Service

   LOCAL cPath, arr, i, cBuff, nPos1, nPos2, arrJson
   IF !hb_DirExists( cPath := ( hb_ps() + Curdir() + hb_ps() + ::cToolsPath ) ) .AND. ;
      !hb_DirExists( cPath := ( hb_dirBase() + ::cToolsPath ) )
      RETURN Nil
   ENDIF

   arr := hb_Directory( cPath + hb_ps() + "tool_*" )
   FOR i := 1 TO Len( arr )
      IF !Empty( cBuff := MemoRead( cPath + hb_ps() + arr[i,1] ) ) .AND. ;
         ( nPos1 := At( "/*", cBuff ) ) > 0 .AND. ( nPos2 := hb_At( "*/", cBuff, nPos1 ) ) > 0
         cBuff := AllTrim( StrTran( StrTran( Substr( cBuff, nPos1+2, nPos2-nPos1-2 ), Chr(10), "" ), Chr(13), "" ) )
         hb_jsonDecode( cBuff, @arrJson )
         IF !Empty( arrJson ) .AND. hb_hHasKey( arrJson, "name" ) .AND. hb_hHasKey( arrJson, "description" )
            AAdd( ::aTools, { arrJson["name"], arrJson, cPath + hb_ps() + arr[i,1] } )
         ENDIF
      ENDIF
   NEXT

   arr := hb_Directory( cPath + hb_ps() + "systool_*" )
   FOR i := 1 TO Len( arr )
      AAdd( ::aSysTools, { hb_fnameName(arr[i,1]), cPath + hb_ps() + arr[i,1] } )
   NEXT

   RETURN Nil

METHOD AddTools() CLASS LLM_Service

   LOCAL s := "", aTool, cParams, oParam

   IF Empty( ::aTools )
      ::LoadTools()
   ENDIF

   FOR EACH aTool IN ::aTools
      s += "- " + aTool[2]["name"] + ": " + aTool[2]["description"] + Chr(10)
      IF hb_hHasKey( aTool[2], "parameters" ) .AND. hb_hHasKey( aTool[2]["parameters"], "properties" )
         cParams := ""
         FOR EACH oParam IN aTool[2]["parameters"]["properties"]
            cParams += "    * " + oParam:__enumkey + " (" + oParam["type"] + "): " + oParam["description"] + Chr(10)
         NEXT
         IF !Empty( cParams )
            s += "  Parameters:" + Chr(10) + cParams
         ENDIF
      ENDIF
   NEXT

   IF !Empty( s )
      s := Chr(10) + "Available tools:" + Chr(10) + s
      ::cSystem += s
   ENDIF

   RETURN s

METHOD RunTool( cToolName, pParams ) CLASS LLM_Service

   LOCAL n := Ascan( ::aTools, {|a|a[1] == cToolName} ), acmd

   ::Log( "runtool " + cToolName + " " + Ltrim(Str(n)) + " " + hb_ValtoExp( pParams ) )
   IF n == 0
      RETURN Nil
   ENDIF
   IF Valtype( ::aTools[n,3] ) == "C"
      acmd := { Memoread( ::aTools[n,3] ), "harbour", "-n2", "-q2" }
      ::aTools[n,3] := { hb_compileFromBuf( hb_ArrayToParams( acmd ) ) }
   ENDIF

   hb_hrbRun( ::aTools[n,3,1], Self, pParams )

   RETURN Nil

METHOD RunSysTool( cToolName, pParams ) CLASS LLM_Service

   LOCAL n := Ascan( ::aSysTools, {|a|a[1] == cToolName} ), acmd

   IF n == 0
      RETURN Nil
   ENDIF

   IF Valtype( ::aSysTools[n,2] ) == "C"
      acmd := { Memoread( ::aSysTools[n,2] ), "harbour", "-n2", "-q2" }
      ::aSysTools[n,2] := { hb_compileFromBuf( hb_ArrayToParams( acmd ) ) }
   ENDIF

   RETURN hb_hrbRun( ::aSysTools[n,2,1], Self, pParams )

METHOD AddEvent( cType, pOpt, cValue ) CLASS LLM_Service

   LOCAL x, s := '<event type="' + cType + '"'

   IF !Empty( pOpt )
      FOR EACH x IN pOpt
         s += ' ' + x:__enumkey + '="' + x:__enumvalue + '"'
      NEXT
   ENDIF
   s += '>' + Chr(10)

   s += '  <time>' + hb_dtoc( Date(), "yyyy-mm-dd" ) + " " + Left( Time(),5 ) + '</time>' + Chr(10)
   IF !Empty( cValue )
      s += cValue + Chr(10)
   ENDIF
   s += '</event>'

   ::cPrompt += ( Iif( Empty( ::cPrompt ), "", Chr(10) ) ) + s
   RETURN Nil

METHOD AddMsgFromMan() CLASS LLM_Service

   LOCAL cFile

   IF File( cFile := ( ::cBasePath + ::cPromptsPath + hb_ps() + cMessageFromMan ) )
      ::AddEvent( "message_from_man",, Memoread( cFile ) )
      FErase( cFile )
   ENDIF

   RETURN Nil

METHOD AddSkills() CLASS LLM_Service

   LOCAL cPath, s := ""

   IF !hb_DirExists( cPath := ( hb_ps() + Curdir() + hb_ps() + ::cToolsPath ) ) .AND. ;
      !hb_DirExists( cPath := ( hb_dirBase() + ::cSkillsPath ) )
      RETURN Nil
   ENDIF

   RETURN s

METHOD ClearContext() CLASS LLM_Service

   ::aHistory := {}

   RETURN Nil

METHOD MainCycle( pFunc ) CLASS LLM_Service

   LOCAL n := 0, aAns

   ::lFinish := .F.
   DO WHILE n < ::nCyclesMax .AND. !::lFinish
      ::AddMsgFromMan()
      aAns := ::Send()
      IF !Empty( pFunc )
         pFunc:exec( aAns )
      ENDIF
      n ++
   ENDDO

   ::lFinish := .F.

   RETURN Nil

METHOD SetOptions( pOptions )

   LOCAL cTmp

   IF !Empty( pOptions )
      IF hb_hHaskey( pOptions, cTmp := "path_work" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
         ::cWorkPath := Lower( cTmp )
      ENDIF
      IF hb_hHaskey( pOptions, cTmp := "path_prompts" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
         ::cPromptsPath := Lower( cTmp )
      ENDIF
      IF hb_hHaskey( pOptions, cTmp := "path_tools" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
         ::cToolsPath := Lower( cTmp )
      ENDIF
      IF hb_hHaskey( pOptions, cTmp := "path_skills" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
         ::cSkillsPath := Lower( cTmp )
      ENDIF
      IF hb_hHaskey( pOptions, cTmp := "cycles_max" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
         ::nCyclesMax := Val( cTmp )
      ENDIF
   ENDIF
   ::cBasePath := Iif( hb_Version(20), "/", hb_curDrive() + ":\" ) + CurDir() + hb_ps()
   IF !hb_DirExists( ::cBasePath + LLM_Service():cPromptsPath )
      ::cBasePath := hb_DirBase()
   ENDIF

   RETURN Nil

METHOD Log( cText, cTitle ) CLASS LLM_Service

   LOCAL nHand, cPath, fname

   IF ::nLogLevel == 0
      RETURN Nil
   ENDIF

   IF Empty( ::cLogPath ) .OR. ( ;
      !hb_DirExists( cPath := ( hb_ps() + Curdir() + hb_ps() + ::cLogPath ) ) .AND. ;
      !hb_DirExists( cPath := ( hb_dirBase() + ::cLogPath ) ) )
      ::cLogPath := "log"
      IF !hb_DirExists( cPath := ( hb_dirBase() + ::cLogPath ) )
         hb_DirCreate( cPath )
      ENDIF
   ENDIF
   fname := cPath + hb_ps() + "service.log"

   IF !File( fname )
      nHand := FCreate( fname )
   ELSE
      nHand := FOpen( fname, 1 )
   ENDIF
   FSeek( nHand, 0, 2 )
   IF !Empty( cTitle )
      FWrite( nHand, hb_Dtoc(Date(),"yyyy-mm-dd") + " " + Time() + " " + ::id + " " + cTitle + Chr( 10 ) )
   ENDIF
   FWrite( nHand, cText + Chr( 10 ) )
   FClose( nHand )

   RETURN Nil

/*
 */
CLASS LLM_OpenAI INHERIT LLM_Service

   METHOD New( pOptions )
   METHOD Send( cModel )

ENDCLASS

METHOD New( pOptions ) CLASS LLM_OpenAI

   LOCAL cTmp

   ::Super:New()

   ::cEndPoint := "v1/chat/completions"
   IF hb_hHaskey( pOptions, cTmp := "id" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::id := cTmp
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "key" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::key := cTmp
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "url" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::cUrl := cTmp
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "endpoint" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::cEndPoint := cTmp
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "model_def" )
      ::cModelDef := pOptions[ cTmp ]
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "sertificat" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::cSertif := cTmp
   ENDIF

   RETURN Self

METHOD Send( cModel ) CLASS LLM_OpenAI

   LOCAL cContent, cCmd

   cContent := ::SetQuery( Iif( Empty(cModel), ::cModelDef, cModel ) )
   hb_Memowrit( "body.json", cContent )

   cCmd := "curl -s " + ::cUrl + ::cEndPoint + ;
      Iif( Empty(::cSertif), '', ' --cacert ' + hb_DirBase() + ::cSertif ) + ;
      Iif( Empty(::key), '', ' -H "Authorization: Bearer ' + ::key + '"') + ;
      ' -H "Content-Type: application/json" -H "Accept: application/json" -d @body.json'
   ::Log( cCmd, "-->" )
   ::Log( "body.json: " + cContent )
   _RunConsoleApp( cCmd,, @cContent )
   FErase( "body.json" )

   RETURN ::ParseResult( cContent )

/*
 */
CLASS LLM_Llama INHERIT LLM_OpenAI

#ifdef _LETO
   DATA leto_addr
   DATA leto_user, leto_pass
   DATA leto_path

   METHOD Send( cModel )
#endif

   METHOD New( pOptions )

ENDCLASS

METHOD New( pOptions ) CLASS LLM_Llama

   LOCAL cTmp

   ::id := "llama"
   ::cUrl := "http://127.0.0.1:8080/"
   ::cModelDef := "local"
   ::Super:New( pOptions )

#ifdef _LETO
   IF hb_hHaskey( pOptions, cTmp := "leto_address" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::leto_addr := cTmp
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "leto_user" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::leto_user := cTmp
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "leto_pass" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::leto_pass := cTmp
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "leto_path" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      IF !( Right( cTmp,1 ) $ "/\" )
         cTmp += '/'
      ENDIF
      ::leto_path := cTmp
   ENDIF
#endif

   RETURN Self

#ifdef _LETO
METHOD Send( cModel ) CLASS LLM_Llama

   LOCAL pArr, cCmd, cContent, cReason, arr, lRes, cFile

   IF Empty( ::leto_addr )
      RETURN ::Super:Send( cModel )

   ELSEIF leto_Connect( ::leto_addr, ::leto_user, ::leto_pass ) > 0
      cContent := ::SetQuery( ::cModelDef )
      cFile := ::leto_addr + ::leto_path + "body.json"
      lRes := leto_MemoWrite( cFile, cContent )
      IF !lRes
         ::Log( "Can't write to " + cFile, "Error" )
         RETURN Nil
      ENDIF
      cCmd := "curl -s " + ::cUrl + ::cEndPoint + ;
         ' -H "Content-Type: application/json" -d @%base/' + ::leto_path + 'body.json'
      ::Log( cCmd, "-->" )
      ::Log( "body.json: " + cContent )
      cContent := leto_RunSync( cCmd )
      leto_FErase( cFile )

      RETURN ::ParseResult( cContent )
   ENDIF

   ::Log( "Can't connect to " + ::leto_addr, "Error" )
   RETURN Nil
#endif

/*
 */
CLASS LLM_Gigachat INHERIT LLM_OpenAI

   DATA cUrlGetToken   INIT "https://ngw.devices.sberbank.ru:9443/api/v2/oauth"
   DATA authkey

   METHOD New( pOptions )
   METHOD Send( cModel )

ENDCLASS

METHOD New( pOptions ) CLASS LLM_Gigachat

   LOCAL cTmp

   ::id := "gigachat"
   ::cModelDef := "Gigachat-2"
   ::cUrl := "https://api.giga.chat/"
   ::Super:New( pOptions )

   IF hb_hHaskey( pOptions, cTmp := "authkey" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::authkey := cTmp
   ENDIF
   IF hb_hHaskey( pOptions, cTmp := "url_gettoken" ) .AND. !Empty( cTmp := pOptions[ cTmp ] )
      ::cUrlGetToken := cTmp
   ENDIF

   RETURN Self

METHOD Send( cModel ) CLASS LLM_Gigachat

   LOCAL cCmd, cResult, pArr

   IF Empty( ::key )
      cCmd := "curl -s -X POST " + ::cUrlGetToken + ;
         Iif( Empty(::cSertif), '', ' --cacert ' + hb_DirBase() + ::cSertif ) + ;
         ' -H "Content-Type: application/x-www-form-urlencoded" -H "Accept: application/json" -H "RqUID: 6f0b1291-c7f3-43c6-bb2e-9f3efb2dc98e" -H "Authorization: Basic ' + ;
         ::authkey + '" -d "scope=GIGACHAT_API_PERS"'
      ::Log( cCmd, "-->" )
      _RunConsoleApp( cCmd,, @cResult )
      ::Log( cResult, "<--" )
      hb_jsonDecode( cResult, @pArr )
      IF Valtype( pArr ) == "H" .AND. hb_hHasKey( pArr, "access_token" )
         ::key := pArr["access_token"]
      ELSE
         ::Log( "Can't get token", "Error" )
         RETURN Nil
      ENDIF
   ENDIF

   RETURN ::Super:Send( cModel )