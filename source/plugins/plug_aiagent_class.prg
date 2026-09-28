/*
 * Set of classes to communicate with LLMs via API
 */

#include "hbclass.ch"

#define  _LETO
#xtranslate _RunConsoleApp([<n,...>])  => cedi_RunConsoleApp(<n>)
FUNCTION plug_aiagent_class
   RETURN Nil

CLASS LLM_Service

   CLASS VAR aList SHARED INIT {}
   CLASS VAR nLogLevel SHARED INIT 1
   CLASS VAR cLogPath  SHARED

   DATA id         INIT ""
   DATA cSystem
   DATA aHistory   INIT {}

   DATA  cUrl
   DATA  cEndPoint INIT ""
   DATA  key
   DATA  cModelDef

   DATA  cSertif

   METHOD New()
   METHOD SetQuery( cModel, cPrompt )
   METHOD ParseResult( cResult )
   METHOD SetSystemPrompt( cText )
   METHOD AddTools()
   METHOD AddSkills()
   METHOD ClearContext()
   METHOD Log( cText )

ENDCLASS

METHOD New( cId ) CLASS LLM_Service

   AAdd( ::aList, Self )

   RETURN Self

METHOD SetQuery( cModel, cPrompt ) CLASS LLM_Service

   LOCAL pArr := hb_hash(), pArr1, i

   pArr["model"] := cModel

   IF Empty( ::aHistory )
      IF !Empty( ::cSystem )
         AAdd( ::aHistory, hb_hash( "role", "system", "content", ::cSystem ) )
      ENDIF
   ENDIF
   AAdd( ::aHistory, hb_hash( "role", "user", "content", cPrompt ) )
   pArr["messages"] := ::aHistory

   RETURN hb_jsonEncode( pArr )

METHOD ParseResult( cResult ) CLASS LLM_Service

   LOCAL pArr, arr, cContent, cReason

   ::Log( cResult, "<--" )
   hb_jsonDecode( cResult, @pArr )
   IF !Empty( pArr ) .AND. !Empty( arr := hb_hGetDef( pArr, "choices", Nil ) )
      cContent := arr[1]["message"]["content"]
      cReason := hb_hGetDef( arr[1]["message"], "reasoning_content", "" )
      AAdd( ::aHistory, hb_hash( "role", "assistant", "content", cContent ) )
      RETURN { cContent, cReason }
   ENDIF

   RETURN Nil

METHOD SetSystemPrompt( cText ) CLASS LLM_Service

   ::cSystem := Iif( Empty(cText), "", cText )
   ::AddTools()
   ::AddSkills()

   RETURN Nil

METHOD AddTools() CLASS LLM_Service
   RETURN Nil

METHOD AddSkills() CLASS LLM_Service
   RETURN Nil

METHOD ClearContext() CLASS LLM_Service

   ::aHistory := {}

   RETURN Nil

METHOD Log( cText, cTitle ) CLASS LLM_Service

   LOCAL nHand, fname

   IF ::nLogLevel == 0
      RETURN Nil
   ENDIF
   IF Empty( ::cLogPath ) .OR. !hb_DirExists( ::cLogPath )
      ::cLogPath := hb_DirBase() + "log"
      IF !hb_DirExists( ::cLogPath )
         hb_DirCreate( ::cLogPath )
      ENDIF
      ::cLogPath += "/"
   ENDIF
   fname := ::cLogPath + "service.log"

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

   METHOD New( aSect )
   METHOD Ask( cModel, cTask )

ENDCLASS

METHOD New( aSect ) CLASS LLM_OpenAI

   LOCAL cTmp

   ::Super:New()

   ::cEndPoint := "v1/chat/completions"
   IF hb_hHaskey( aSect, cTmp := "id" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::id := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "key" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::key := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "url" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::cUrl := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "endpoint" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::cEndPoint := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "model_def" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::cModelDef := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "sertificat" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::cSertif := cTmp
   ENDIF

   RETURN Self

METHOD Ask( cModel, cTask ) CLASS LLM_OpenAI

   LOCAL cContent, cCmd

   cContent := ::SetQuery( Iif( Empty(cModel), ::cModelDef, cModel ), cTask )
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

   METHOD Ask( cModel, cTask )
#endif

   METHOD New( aSect )

ENDCLASS

METHOD New( aSect ) CLASS LLM_Llama

   LOCAL cTmp

   ::id := "llama"
   ::cUrl := "http://127.0.0.1:8080/"
   ::cModelDef := "local"
   ::Super:New( aSect )

#ifdef _LETO
   IF hb_hHaskey( aSect, cTmp := "address" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::leto_addr := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "user" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::leto_user := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "pass" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::leto_pass := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      IF !( Right( cTmp,1 ) $ "/\" )
         cTmp += '/'
      ENDIF
      ::leto_path := cTmp
   ENDIF
#endif

   RETURN Self

#ifdef _LETO
METHOD Ask( cModel, cTask ) CLASS LLM_Llama

   LOCAL pArr, cCmd, cContent, cReason, arr, lRes, cFile

   IF Empty( ::leto_addr )
      RETURN ::Super:Ask( cModel, cTask )

   ELSEIF leto_Connect( ::leto_addr, ::leto_user, ::leto_pass ) > 0
      cContent := ::SetQuery( ::cModelDef, cTask )
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

   METHOD New( aSect )
   METHOD Ask( cModel, cTask )

ENDCLASS

METHOD New( aSect ) CLASS LLM_Gigachat

   LOCAL cTmp

   ::id := "gigachat"
   ::cModelDef := "Gigachat-2"
   ::cUrl := "https://api.giga.chat/"
   ::Super:New( aSect )

   IF hb_hHaskey( aSect, cTmp := "authkey" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::authkey := cTmp
   ENDIF
   IF hb_hHaskey( aSect, cTmp := "url_gettoken" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
      ::cUrlGetToken := cTmp
   ENDIF

   RETURN Self

METHOD Ask( cModel, cTask ) CLASS LLM_Gigachat

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

   RETURN ::Super:Ask( cModel, cTask )