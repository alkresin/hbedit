/*
 * Client for extgui modules
 * HbEdit plugin
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "fileio.ch"
#define PROTOCOL_VER "1.1"
#define  BUFFLEN   512

#define GUIS_VERSION   "1.4"

STATIC cn := e"\n"
STATIC cLogFile := "extclient.log"
STATIC aExt := {}, nExtId := 0
STATIC nInterval := 20

STATIC cVersion := "1.0"
STATIC nMyId, nHisId

FUNCTION hbExtCli()
   RETURN Nil

FUNCTION ecli_Run( cExe, nLog, cDir, cFile )

   LOCAL h := hb_hash(), nSec, cRun
   LOCAL nLogOn, cDirRoot, cFileRoot := "gs"

   nLogOn := Iif( Valtype( nLog ) == "N", nLog, 0 )
   cDirRoot := Iif( Empty( cDir ), hb_DirTemp(), cDir )
   IF !( Right( cDirRoot,1 ) $ "\/" )
      cDirRoot += hb_ps()
   ENDIF
   IF ' ' $ cDirRoot
      cDirRoot := '"' + cDirRoot + '"'
   ENDIF

   h["log"] := nLogOn
   h["id"] := ++ nExtId
   h["dir"] := cDirRoot
   h["cb"] := Nil
   h["active"] := .F.
   h["hin"] := -1
   h["hout"] := -1
   h["bufres"] := ""

   IF !Empty( cFile ) .AND. Valtype( cFile ) == "C"
      cFileRoot := cFile
   ENDIF
   IF !srv_conn_Create( h, cDirRoot + cFileRoot, .F. )
      RETURN .F.
   ENDIF

   cRun := cExe + ' dir=' + cDirRoot + ' type=2 ' + Iif( nLogOn>0, "log="+Str(nLogOn,1), "" ) + ;
      Iif( !Empty(cFile).AND.Valtype(cFile)=="C", " file="+cFile, "" )
   gWritelog( h, cRun )
   cedi_RunBackgroundApp( cRun )

   nSec := Seconds()
   DO WHILE Seconds() - nSec < 1
      IF !Empty( ecli_CheckAnswer( h ) )
         AAdd( aExt, h )
         RETURN h
      ENDIF
      cedi_Sleep( nInterval*2 )
   ENDDO

   RETURN Nil

FUNCTION ecli_Close( h )

   LOCAL i, id := h["id"]

   SendOut( h, '["endapp"]', .T. )
   cedi_Sleep( nInterval*2 )

   conn_Exit( h )
   cedi_Sleep( nInterval*2 )

   FOR i := 1 TO Len( aExt )
      IF aExt[i]["id"] == id
         hb_ADel( aExt, i, .T. )
         EXIT
      ENDIF
   NEXT

   RETURN Nil

FUNCTION ecli_RunProc( h, cFunc, aParams )

   LOCAL i

   FOR i := 1 TO Len(aParams)
      IF Valtype( aParams[i] ) != "C"
         aParams[i] := CnvVal( aParams[i] )
      ENDIF
   NEXT
   SendOut( h, hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( aParams ) } ) )

   RETURN Nil

FUNCTION ecli_RunFunc( h, cFunc, aParams, lNoWait )

   LOCAL cRes := SendOut( h, hb_jsonEncode( { "runfunc", cFunc, hb_jsonEncode( aParams ) } ), lNoWait )

   IF Valtype( cRes ) == "C" .AND. Left( cRes,1 ) == '"'
      RETURN Substr( cRes, 2, Len(cRes)-2 )
   ENDIF
   RETURN cRes

FUNCTION ecli_CheckAnswer( h )

   RETURN conn_CheckOut( h )

FUNCTION ecli_SetVar( h, cVarName, cValue )

   SendOut( h, hb_jsonEncode( { "setvar", cVarName, cValue } ) )

   RETURN Nil

FUNCTION ecli_GetVar( h, cVarName )

   LOCAL cRes := SendOut( h, hb_jsonEncode( { "getvar", cVarName } ) )

   RETURN Substr( cRes,2,Len(cRes)-2 )

STATIC FUNCTION CnvVal( xRes )

   LOCAL cRes := Valtype(xRes), nPos2

   IF cRes == "A"
      cRes := "Array"
   ELSEIF cRes == "O"
      cRes := "Object of " + xRes:Classname()
   ELSEIF cRes == "H"
      cRes := "Hash array"
   ELSEIF cRes == "U"
      cRes := "Nil"
   ELSEIF cRes == "C"
      cRes := xRes
   ELSEIF cRes == "L"
      cRes := Iif( xRes, "t", "f" )
   ELSE
      cRes := Trim( Transform( xReS, "@B" ) )
   ENDIF
   IF Valtype( xRes ) == "N" .AND. Rat( ".", cRes ) > 0
     nPos2 := Len( cRes )
     DO WHILE Substr( cRes, nPos2, 1 ) == '0'; nPos2 --; ENDDO
     IF Substr( cRes, nPos2, 1 ) == '.'
        nPos2 --
     ENDIF
     cRes := Left( cRes, nPos2 )
   ENDIF

   RETURN cRes

STATIC FUNCTION SendOut( h, s, lNoWait )

   LOCAL cRes
   gWritelog( h, " " + s )

   cRes := conn_Send2SocketOut( h, "+" + s + cn, lNoWait )

   RETURN Iif( Empty(cRes), "", cRes )

STATIC FUNCTION SendIn( h, s )

   conn_Send2SocketIn( h, s )

   RETURN Nil

STATIC FUNCTION MainHandler( h )

   LOCAL arr, cBuffer

   cBuffer := conn_GetRecvBuffer( h )

   gWritelog( h, cBuffer )

   hb_jsonDecode( cBuffer, @arr )
   IF Valtype(arr) != "A" .OR. Empty(arr)
      SendIn( h, "+Wrong" + cn )
      RETURN Nil
   ENDIF
   SendIn( h, "+Ok" + cn )

   RETURN Nil

FUNCTION gWritelog( h, s )

   LOCAL nHand, cFile

   IF h["log"] > 0
      cFile := h["dir"] + cLogFile
      IF ! File( cFile )
         nHand := FCreate( cFile )
      ELSE
         nHand := FOpen( cFile, 1 )
      ENDIF
      FSeek( nHand, 0, 2 )
      FWrite( nHand, s + cn )
      FClose( nHand )
   ENDIF
   RETURN Nil

STATIC FUNCTION conn_Read( h, lOut )

   LOCAL n, nPos, s := ""
   LOCAL han := Iif( lOut, h["hout"], h["hin"] )
   LOCAL cBuffer := Space( BUFFLEN )

   FSeek( han, 1, 0 )
   DO WHILE ( n := FRead( han, @cBuffer, Len(cBuffer ) ) ) > 0
      IF ( nPos := At( Chr(10), cBuffer ) ) > 0
         s += Left( cBuffer, nPos-1 )
         EXIT
      ELSEIF n < Len(cBuffer )
         s += Left( cBuffer, n )
         EXIT
      ELSE
         s += cBuffer
      ENDIF
   ENDDO

   h["bufres"] := s

   RETURN Len( s )

STATIC FUNCTION conn_GetRecvBuffer( h )

   RETURN Substr( h["bufres"], 2 )

STATIC FUNCTION conn_Send( h, lOut, cLine )

   LOCAL han := Iif( lOut, h["hout"], h["hin"] )

   IF h["active"]
      FSeek( han, 1, 0 )
      FWrite( han, cLine )
      FSeek( han, 0, 0 )
      FWrite( han, Chr(nMyId) )
   ENDIF

   RETURN Nil

STATIC FUNCTION conn_Send2SocketIn( h, s )

   IF h["active"]
      conn_Send( h, .F., s )
   ENDIF

   RETURN Nil

STATIC FUNCTION conn_Send2SocketOut( h, s, lNoWait )

   LOCAL cAns

   IF h["active"]
      conn_Send( h, .T., s )
      IF Empty( lNoWait )
         DO WHILE h["active"]
            conn_CheckIn( h )
            IF !Empty( cAns := conn_CheckOut( h ) )
               RETURN cAns
            ENDIF
            IF !Empty( h["cb"] )
               Eval( h["cb"] )
            ENDIF
         ENDDO
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION conn_CheckIn( h )

   LOCAL hIn := h["hin"], bufIn := Space( 10 )

   IF h["active"]
      FSeek( hIn, 0, 0 )
      IF FRead( hIn, @bufIn, 1 ) > 0 .AND. Asc( bufIn ) == nHisId
         gWritelog( h, "Checkin" )
         IF conn_Read( h, .F. ) > 0
            MainHandler( h )
         ENDIF
         RETURN .T.
      ENDIF
   ENDIF
   RETURN .F.

STATIC FUNCTION conn_CheckOut( h )

   LOCAL hOut := h["hout"], bufOut := Space( 10 )

   IF h["active"]
      FSeek( hOut, 0, 0 )
      IF FRead( hOut, @bufOut, 1 ) > 0 .AND. Asc( bufOut ) == nHisId
         conn_Read( h, .T. )
         gWritelog( h, "Checkout: " + conn_GetRecvBuffer( h ) )
         RETURN conn_GetRecvBuffer( h )
      ENDIF
   ENDIF
   RETURN Nil

STATIC FUNCTION srv_conn_Create( h, cFile )

   LOCAL handlIn, handlOut

   nMyId := 2
   nHisId := 1

   handlIn := FCreate( cFile + ".gs1" )
   FClose( handlIn )

   handlOut := FCreate( cFile + ".gs2" )
   FClose( handlOut )

   handlIn := FOpen( cFile + ".gs1", FO_READWRITE + FO_SHARED )
   gwritelog( h, "Open in " + cFile + ".gs1" + " " + str(handlIn) )
   handlOut := FOpen( cFile + ".gs2", FO_READWRITE + FO_SHARED )
   gwritelog( h, "Open out " + cFile + ".gs2" + " " + str(handlOut) )

   h["active"] := ( handlIn >= 0 .AND. handlOut >= 0 )
   h["hin"] := handlIn
   h["hout"] := handlOut

   conn_Send( h, .F., "+v" + cVersion + "/" + PROTOCOL_VER + Chr(10) )
   conn_Send( h, .T., "+Ok" + Chr(10) )

   RETURN h["active"]

STATIC PROCEDURE conn_Exit( h )

   h["active"] := .F.
   FClose( h["hin"] )
   FClose( h["hout"] )

   RETURN
