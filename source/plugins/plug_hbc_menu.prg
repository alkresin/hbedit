
STATIC cIniPath
STATIC cPath_Hrb

FUNCTION plug_hbc_menu( aMenu, oPane, cPath )

   LOCAL i, lGit := .F., lFoss := .F., lGo := .F.
   LOCAL cFile, cExt

   IF Empty( cIniPath )
      cIniPath := cPath
      _hbc_readini()
   ENDIF

   FOR i := 1 TO Len( oPane:aDir )
      cFile := oPane:aDir[i,1]
      cExt := hb_fnameExt( cFile )
      IF cFile == ".git"
         lGit := .T.
      ELSEIF cExt == ".fossil"
         lFoss := .T.
      ELSEIF cFile == "go.mod"
         lGo := .T.
      ENDIF
   NEXT

   IF lGit
      IF !Empty( aMenu )
         Aadd( aMenu, { "---",,, } )
      ENDIF
      Aadd( aMenu, { "git pull",,11, } )
      Aadd( aMenu, { "git status",,12, } )
      Aadd( aMenu, { "git describe",,13, } )
      Aadd( aMenu, { "git commit",,14, } )
   ENDIF
   IF lFoss
      IF !Empty( aMenu )
         Aadd( aMenu, { "---",,, } )
      ENDIF
      Aadd( aMenu, { "fossil changes",,15, } )
   ENDIF
   IF lGo
      IF !Empty( aMenu )
         Aadd( aMenu, { "---",,, } )
      ENDIF
      Aadd( aMenu, { "go build",,31, } )
   ENDIF

   cFile := oPane:aDir[oPane:nCurrent + oPane:nShift,1]
   cExt := hb_fnameExt( cFile )
   IF cExt == ".prg"
      IF !Empty( aMenu )
         Aadd( aMenu, { "---",,, } )
      ENDIF
      Aadd( aMenu, { "format",,21 } )
      Aadd( aMenu, { "compile",,22 } )
   ELSEIF cExt == ".go"
      IF !Empty( aMenu ) .AND. !lGo
         Aadd( aMenu, { "---",,, } )
      ENDIF
      Aadd( aMenu, { "run "+cFile,,32 } )
   ENDIF

   RETURN {|n| _hbc_menu_exec(n,oPane)}

STATIC FUNCTION _hbc_menu_exec( n,oPane )

   LOCAL lRefr := .F., cTemp, cSep := hb_ps()

   IF n == 11
      hbc_Console( "git pull" )
      lRefr := .T.
   ELSEIF n == 12
      hbc_Console( "git status" )
   ELSEIF n == 13
      hbc_Console( "git describe --tags" )
   ELSEIF n == 14
      hbc_Console( 'git commit -a -m "%m"' )
   ELSEIF n == 15
      hbc_Console( "fossil changes" )
   ELSEIF n == 21
      hbc_Console(  cPath_Hrb + "bin/hbformat %f" )
      lRefr := .T.
   ELSEIF n == 22
      cTemp := hb_fnameName( oPane:aDir[oPane:nCurrent + oPane:nShift,1] )
      hbc_Console( { cPath_Hrb + "bin" + cSep + "harbour %f -n -w -i" + cPath_Hrb + cSep + "include", ;
         "bcc32 -O2 -d -I" + cPath_Hrb + "include -L" + cPath_Hrb + "lib" + cSep + "win" + cSep + "bcc " + ;
          cTemp + ".c " + "hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbcpage.lib hbct.lib hbpcre.lib hbcplr.lib hbzlib.lib hbziparc.lib hbmzip.lib minizip.lib hbtip.lib hbnetio.lib hbwin.lib ws2_32.lib ws2_32.lib iphlpapi.lib", ;
          "del " + cTemp + ".c", "del " + cTemp + ".obj", "del " + cTemp + ".tds" } )
      lRefr := .T.
   ELSEIF n == 31
      hbc_Console( "go build" )
   ELSEIF n == 32
      hbc_Console( "go run %f" )
   ENDIF
   IF lRefr
      oPane:Refresh()
   ENDIF

   RETURN Nil

STATIC FUNCTION _hbc_readini()

   LOCAL hIni := edi_iniRead( cIniPath + "plug_hbc_menu.ini" ), cTmp, aSect

   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )

      IF hb_hHaskey( hIni, cTmp := "MAIN" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         IF hb_hHaskey( aSect, cTmp := "harbour_path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cPath_Hrb := cTmp
            IF !Right( cPath_Hrb,1 ) $ "/\"
               cPath_Hrb += hb_ps()
            ENDIF
         ENDIF

      ENDIF
   ENDIF

   IF Empty( cPath_Hrb )
      IF hb_Version(20) // 20 - HB_VERSION_UNIX_COMPAT
         cPath_Hrb := "/usr/local/bin/"
      ELSE
         cPath_Hrb := "c:\Harbour\"
      ENDIF
   ENDIF

   RETURN Nil
