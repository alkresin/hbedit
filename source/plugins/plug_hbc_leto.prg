
#include "inkey.ch"
#include "fileio.ch"

#define  READ_BUFF_LEN  4096

STATIC aKeys1 := { K_DOWN, K_UP, K_MWBACKWARD, K_MWFORWARD, K_LEFT, K_RIGHT, ;
   K_PGDN, K_PGUP, K_HOME, K_END, K_TAB, K_CTRL_TAB, K_LBUTTONDOWN, K_RBUTTONDOWN, K_LDBLCLK, ;
   K_ENTER, K_INS, K_CTRL_R, K_CTRL_P, K_CTRL_PGUP, K_F9, K_F10, K_F5, K_F6, K_F7, ;
   K_F8, 68, 100 }
STATIC aMonths := { "jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec" }
STATIC cNotPerm := "Operation isn't permitted"

FUNCTION plug_hbc_leto( oPane, cPlugPath, aParams )

   LOCAL cAddr, cPath, cLogin := "", cPass := "", lSave := .F.
   LOCAL nConnection, nPort
   LOCAL i, aMenu

   IF Empty( aParams )
      aMenu := { "New address" }
      FOR i := 1 TO Len( FilePane():aDefPaths )
         IF FilePane():aDefPaths[i,1] = "ftp:"
            Aadd( aMenu, Substr( FilePane():aDefPaths[i,1],5 ) )
         ENDIF
      NEXT
      NetInfoLoad()
      FOR i := 1 TO Len( FilePane():aNetInfo )
         IF FilePane():aNetInfo[i,1] == "leto:" .AND. Ascan( aMenu, {|s|s=FilePane():aNetInfo[i,2]} ) == 0
            Aadd( aMenu, FilePane():aNetInfo[i,2] + Iif( Empty(FilePane():aNetInfo[i,3]), "", ;
               ":"+Ltrim(Str(FilePane():aNetInfo[i,3])) ) )
         ENDIF
      NEXT
      i := FMenu( oPane, aMenu, oPane:y1+1, oPane:x1+1, oPane:y1+Len(aMenu)+2,, oPane:aClrMenu[1], oPane:aClrMenu[2] )
      IF i == 0
         RETURN .F.
      ELSEIF i == 1
         IF Empty( cAddr := edi_MsgGet( "Leto server Address:" ) )
            RETURN .F.
         ELSE
            RETURN oPane:ChangeDir( "leto:" + cAddr )
         ENDIF
      ELSE
         RETURN oPane:ChangeDir( "leto:" + aMenu[i] )
      ENDIF
   ELSEIF Empty( aParams[1] )
      RETURN .F.
   ENDIF

   cAddr := aParams[1]
   nPort := Iif( Empty(Val(aParams[2])), 2812, Val(aParams[2]) )
   cPath := aParams[3]
   cLogin := aParams[4]
   cPass := aParams[5]                    	

   IF Empty( cLogin ) .OR. Empty( cPass )
      IF !hbc_GetLogin( @cLogin, @cPass, @lSave )
         RETURN .F.
      ENDIF
   ENDIF

   cAddr :=  "//" + cAddr + ":" + Ltrim( Str(nPort) )
   //ftplog( "Connect to: " + cAddr )

   IF ( nConnection := leto_Connect( cAddr, cLogin, cPass ) ) > 0

      oPane:pSess := nConnection
      oPane:bRefresh := {|o|_plug_Refresh(o)}
      oPane:bOnKey := {|o,n|_plug_OnKey(o,n)}

      aParams[4] := cLogin
      aParams[5] := cPass
      aParams[6] := lSave

      RETURN .T.
   ELSE
      edi_Alert( "Can't login" )
   ENDIF

   RETURN .F.

FUNCTION plug_hbc_leto_close( o )

   leto_Disconnect( o:pSess )

   RETURN 0

FUNCTION plug_hbc_leto_copyfrom( oPane, aParams )

   LOCAL cFileName, cFileTo, nPos, oPaneTo, cBuffer, i, aDir, nFirst, nSize, dDate

   cFileName := aParams[1]
   cFileTo := aParams[2]
   nFirst := aParams[4]
   nSize := aParams[5]
   dDate := aParams[6]

   IF aParams[3]
      edi_Alert( "From leto: " + cNotPerm )
      RETURN 2
   ENDIF

   IF hb_vfExists( cFileTo )
      aDir := oPane:aDir[oPane:nCurrent + oPane:nShift]
      hb_vfTimeGet( cFileTo, @i )
      IF !FAsk_Overwrite( nFirst, hb_fnameNameExt(cFileTo), nSize, dDate, hb_vfSize(cFileTo), i )
         RETURN  1
      ENDIF
   ENDIF

   oPaneTo := Iif( oPane == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   nPos := At( '/', cFileName )
   cFileName := Substr( cFileName, nPos )

   //ftplog( cFileName + " => " + cFileTo )
   IF !Empty( cBuffer := leto_Memoread( cFileName ) )
      //ftplog( "Read - ok" )
      hb_Memowrit( cFileTo, cBuffer )
      IF nFirst == 0
         oPaneTo:Refresh()
         oPaneTo:RedrawAll()
      ENDIF
   ELSE
      RETURN 3
   ENDIF

   RETURN 0

FUNCTION plug_hbc_leto_copyto( o, aParams )

   LOCAL cFileName, cFileTo, nPos, nFirst, n, aDir, nSize, oPaneCurr, cBuffer

   cFileName := aParams[1]
   cFileTo := aParams[2]
   nFirst := aParams[4]
   nSize := aParams[5]

   IF aParams[3] .OR. !( cFileTo = "leto:" )
      edi_Alert( "To leto: " + cNotPerm )
      RETURN 2
   ENDIF

   oPaneCurr := Iif( o == FilePane():aPanes[1], FilePane():aPanes[2], FilePane():aPanes[1] )
   IF ( n := Ascan2( o:aDir, FTransl( hb_fnameNameExt(cFileName),oPaneCurr:cpPane,o:cpPane ) ) ) > 0
      aDir := o:aDir[n]
      hb_vfTimeGet( cFileName, @n )
      IF !FAsk_Overwrite( nFirst, hb_fnameNameExt(cFileName), hb_vfSize(cFileName), n, aDir[2], aDir[3] )
         RETURN  1
      ENDIF
   ENDIF

   nPos := At( '/', cFileTo )
   cFileTo := Substr( cFileTo, nPos )
   //ftplog( cFileName + " => " + cFileTo )
   //cFileTo := StrTran( Substr( cFileTo, nPos ), "\", "/" )

   IF !Empty( cBuffer := Memoread( cFileName ) )
      //ftplog( "Read Ok" )
      IF leto_Memowrite( cFileTo, cBuffer )
         //ftplog( "Write Ok" )
      ELSE
         RETURN 3
      ENDIF
   ELSE
      RETURN 3
   ENDIF
   IF nFirst == 0
      o:Refresh()
      o:RedrawAll()
   ENDIF

   RETURN 0

FUNCTION plug_hbc_leto_delete( oPane, aParams )

   LOCAL cFileName, nPos

   cFileName := aParams[1]

   IF !( cFileName = "leto:" )
      edi_Alert( cNotPerm )
      RETURN 2
   ENDIF

   nPos := At( '/', cFileName )

   IF aParams[2]
      IF leto_DirRemove( Substr(cFileName,nPos) ) == -1
         RETURN 2
      ENDIF
   ELSE
      IF leto_FErase( Substr(cFileName,nPos) ) == -1
         RETURN 2
      ENDIF
   ENDIF

   RETURN 0

FUNCTION plug_hbc_leto_mkdir( oPane, aParams )

   LOCAL cDirName, nPos

   cDirName := aParams[1]

   IF !( cDirName = "leto:" )
      edi_Alert( cNotPerm )
      RETURN 2
   ENDIF

   nPos := At( '/', cDirName )
   cDirName := StrTran( Substr( cDirName, nPos ), "\", "/" )
   //edi_Writelog( "MKD " + Substr(cDirName,nPos) )

   IF leto_Makedir( cDirName ) == -1
      RETURN 2
   ENDIF

   RETURN 0

STATIC FUNCTION _plug_Refresh( oPane )

   //ftplog( "Dir: " + oPane:cCurrPath )
   oPane:aDir := leto_Directory( oPane:cCurrPath, "HSD" )
   IF Empty( oPane:aDir )
      AAdd( oPane:aDir, { "..", 0, Date(), "", "D" } )
   ENDIF

   RETURN 0

STATIC FUNCTION _plug_OnKey( oPane, nKeyExt )

   LOCAL nKey := hb_keyStd( nKeyExt ), cBuffer, cFileTo, cName, o, aDir

   IF (nKey >= K_NCMOUSEMOVE .AND. nKey <= HB_K_MENU) .OR. nKey == K_MOUSEMOVE
      RETURN -1
   ENDIF
   IF Ascan( aKeys1, nKey ) > 0
      RETURN 0
   ELSEIF nKey == K_F4
      aDir := oPane:aDir[oPane:nCurrent + oPane:nShift]
      IF 'D' $ aDir[5]
         RETURN 0
      ELSE
         o := TEdit():aWindows[TEdit():nCurr]
         cName := aDir[1]
         IF !Empty( cBuffer := leto_MemoRead( oPane:cCurrPath + cName ) )
            mnu_NewBuf( o, oPane:cIOpref + oPane:net_cAddress + oPane:net_cPort + ;
            oPane:cCurrPath + cName, cBuffer, ):lReadOnly := .T.
         ENDIF
      ENDIF
   ELSEIF nKey == K_CTRL_F7 .OR. nKey == 43
      edi_Alert( cNotPerm )
   ENDIF

   RETURN -1

STATIC FUNCTION ftplog( cText )
   //? cText
   edi_Writelog( cText, "_leto.log" )

   RETURN Nil