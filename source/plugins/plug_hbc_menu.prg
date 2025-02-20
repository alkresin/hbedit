
STATIC cIniPath
STATIC cPath_Hrb
STATIC aMenuGit, aMenuFoss, aMenuC
STATIC aExt := {}, aCmds := {}

FUNCTION plug_hbc_menu( aMenu, oPane, cPath )

   LOCAL i, lGit := .F., lFoss := .F., lGo := .F.
   LOCAL cFile, cExt, n

   IF Empty( cIniPath )
      cIniPath := cPath
      _hbc_readini()
   ENDIF

   IF oPane:nPanelMod == 1 .AND. oPane:nCurrent > 1
      Aadd( aMenu, { "go to file",,5, } )
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
      Aadd( aMenu, { "git remote",,13, } )
      Aadd( aMenu, { "git describe",,14, } )
      Aadd( aMenu, { "git log",,15, } )
      Aadd( aMenu, { "git commit",,16, } )
      IF !Empty( aMenuGit )
         FOR i := 1 TO Len( aMenuGit )
            Aadd( aMenu, { aMenuGit[i,1],,100+i, } )
         NEXT
      ENDIF
   ENDIF
   IF lFoss
      IF !Empty( aMenu )
         Aadd( aMenu, { "---",,, } )
      ENDIF
      Aadd( aMenu, { "fossil changes",,18, } )
      IF !Empty( aMenuFoss )
         FOR i := 1 TO Len( aMenuFoss )
            Aadd( aMenu, { aMenuFoss[i,1],,130+i, } )
         NEXT
      ENDIF
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
   ELSEIF cExt == ".c" .OR. cExt == ".cpp"
      IF !Empty( aMenu )
         Aadd( aMenu, { "---",,, } )
      ENDIF
      IF hb_Version(20)
         Aadd( aMenu, { "compile",,37 } )
       ELSE
         Aadd( aMenu, { "compile with Mingw",,37 } )
         Aadd( aMenu, { "compile with Borland",,38 } )
      ENDIF
   ENDIF
   IF !(cExt == ".") .AND. !Empty( aExt ) .AND. aExt[1,1] == "*"
      FOR i := 1 TO Len( aCmds[1] )
         Aadd( aMenu, { aCmds[1,i,1],,200+i, } )
      NEXT
   ENDIF
   IF !Empty( cExt ) .AND. !Empty( aExt ) .AND. ( n := Ascan(aExt, {|a|hb_Ascan(a,cExt,,,.T.)>0}) ) > 0
      FOR i := 1 TO Len( aCmds[n] )
         Aadd( aMenu, { aCmds[n,i,1],,230+i, } )
      NEXT
   ENDIF

   RETURN {|n| _hbc_menu_exec(n,oPane)}

STATIC FUNCTION _hbc_menu_exec( n,oPane )

   LOCAL lRefr := .F., cFile, cDir, cExt, i, cCmd

   IF n == 5
      oPane:cIOpref := oPane:cIOpref_bak
      oPane:net_cAddress := oPane:net_cAddress_bak
      cFile := hb_fnameNameExt( oPane:aDir[oPane:nCurrent + oPane:nShift,1] )
      cDir := oPane:cCurrPath + hb_fnameDir( oPane:aDir[oPane:nCurrent + oPane:nShift,1] )
      IF !( Right( cDir, 1 ) $ "/\" )
         cDir += hb_ps()
      ELSEIF Right( cDir, 1 ) != hb_ps()
         cDir := hb_strShrink( cDir, 1 ) + hb_ps()
      ENDIF
      oPane:SetDir( cDir )
      IF ( i := Ascan2( oPane:aDir, cFile ) ) > 0
         IF i <= oPane:nCells
            oPane:nCurrent := i
         ELSE
            oPane:nShift := i
         ENDIF
      ENDIF
      oPane:Draw()
      oPane:DrawCell( ,.T. )
      oPane:DrawHead( .T. )

   ELSEIF n == 11
      hbc_Console( "git pull" )
      lRefr := .T.
   ELSEIF n == 12
      hbc_Console( "git status" )
   ELSEIF n == 13
      hbc_Console( "git remote -v" )
   ELSEIF n == 14
      hbc_Console( "git describe --tags" )
   ELSEIF n == 15
      hbc_Console( 'git log --after=@{%m.days.ago} --pretty=format:"%ad%x09%s"' )
   ELSEIF n == 16
      hbc_Console( 'git commit -a -m "%m"' )
   ELSEIF n == 18
      hbc_Console( "fossil changes" )
   ELSEIF n == 21
      hbc_Console(  "hbformat %p" )
      lRefr := .T.
   ELSEIF n == 22
      hbc_Console( { cPath_Hrb + "bin%/harbour %p -n -w -i" + cPath_Hrb + "%/include", ;
         "bcc32 -O2 -d -I" + cPath_Hrb + "include -L" + cPath_Hrb + "lib%/win%/bcc " + ;
          "%n.c " + "hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbcpage.lib hbct.lib hbpcre.lib hbcplr.lib hbzlib.lib hbziparc.lib hbmzip.lib minizip.lib hbtip.lib hbnetio.lib hbwin.lib ws2_32.lib ws2_32.lib iphlpapi.lib", ;
          "cmd /C del %n.c", "cmd /C del %n.obj", "cmd /C del %n.tds" } )
      lRefr := .T.
   ELSEIF n == 31
      hbc_Console( "go build" )
   ELSEIF n == 32
      hbc_Console( "go run %p" )
   ELSEIF n == 37
      hbc_Console( "gcc %p" )
   ELSEIF n == 38
      hbc_Console( "bcc32 %p" )
   ELSEIF !Empty( aMenuGit ) .AND. n > 100 .AND. n < 130 .AND. ;
      (n-100) <= Len( aMenuGit ) .AND. !Empty( aMenuGit[n-100] )
      IF hb_TokenCount( aMenuGit[n-100,2], ",", .T. ) > 1
         hbc_Console( hb_ATokens( aMenuGit[n-100,2],",",.T. ) )
      ELSE
         hbc_Console( aMenuGit[n-100,2] )
      ENDIF
   ELSEIF !Empty( aMenuFoss ) .AND. n > 130 .AND. n < 160 .AND. ;
      (n-130) <= Len( aMenuFoss ) .AND. !Empty( aMenuFoss[n-130] )
      IF hb_TokenCount( aMenuFoss[n-100,2], ",", .T. ) > 1
         hbc_Console( hb_ATokens( aMenuFoss[n-130,2],",",.T. ) )
      ELSE
         hbc_Console( aMenuFoss[n-130,2] )
      ENDIF
   ELSEIF !Empty( aCmds ) .AND. n > 200 .AND. n < 230
      cCmd := aCmds[1,n-200,2]
      IF Left( cCmd,1 ) == "@"
         hbc_Console( Substr(cCmd,2),, .F. )
       ELSE
         hbc_Console( cCmd,, .T. )
      ENDIF
   ELSEIF !Empty( aCmds ) .AND. n > 230
      cExt := hb_fnameExt( oPane:aDir[oPane:nCurrent + oPane:nShift,1] )
      IF ( i := Ascan(aExt, {|a|hb_Ascan(a,cExt,,,.T.)>0}) ) > 0
         cCmd := aCmds[i,n-230,2]
         IF Left( cCmd,1 ) == "@"
             hbc_Console( Substr(cCmd,2),, .F. )
          ELSE
             hbc_Console( cCmd,, .T. )
         ENDIF
      ENDIF
   ENDIF
   IF lRefr
      oPane:Refresh()
   ENDIF

   RETURN Nil

STATIC FUNCTION _hbc_readini()

   LOCAL hIni := edi_iniRead( cIniPath + "plug_hbc_menu.ini" ), aIni
   LOCAL cTmp, nSect, cSect, aSect, arr, arr1, i, nPos

   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         cSect := Upper(aIni[nSect])
         IF Left( cSect, 4 ) == "EXT_"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := hb_hKeys( aSect )
               arr1 := {}
               FOR i := 1 TO Len( arr )
                  IF !(arr[i]=="ext") .AND. !Empty( cTmp := aSect[ arr[i] ] )
                     AAdd( arr1, hb_ATokens( cTmp, ',' ) )
                  ENDIF
               NEXT
               IF hb_hHaskey( aSect, cTmp := "ext" ) .AND. !Empty( cTmp := aSect[ cTmp ] ) ;
                  .AND. !Empty( arr1 )
                  IF AllTrim(cTmp) == "*"
                     hb_Ains( aExt, 1, {cTmp}, .T. )
                     hb_Ains( aCmds, 1, arr1, .T. )
                  ELSE
                     AAdd( aExt, hb_ATokens( cTmp ) )
                     AAdd( aCmds, arr1 )
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF cSect == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTmp := "harbour_path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
                  cPath_Hrb := cTmp
                  IF !Right( cPath_Hrb,1 ) $ "/\"
                     cPath_Hrb += hb_ps()
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF Upper(aIni[nSect]) == "GIT"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := hb_hKeys( aSect )
               aMenuGit := Array( Len( arr ) )
               FOR i := 1 TO Len( arr )
                  IF !Empty( cTmp := aSect[ arr[i] ] )
                     IF ( nPos := At( ',', cTmp ) ) > 0
                        aMenuGit[i] := { Left( cTmp, nPos-1 ), AllTrim( Substr( cTmp, nPos+1 ) ) }
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ELSEIF Upper(aIni[nSect]) == "FOSSIL"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               arr := hb_hKeys( aSect )
               aMenuFoss := Array( Len( arr ) )
               FOR i := 1 TO Len( arr )
                  IF !Empty( cTmp := aSect[ arr[i] ] )
                     IF ( nPos := At( ',', cTmp ) ) > 0
                        aMenuFoss[i] := { Left( cTmp, nPos-1 ), AllTrim( Substr( cTmp, nPos+1 ) ) }
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
   ENDIF
   IF Empty( cPath_Hrb )
      IF hb_Version(20) // 20 - HB_VERSION_UNIX_COMPAT
         cPath_Hrb := "/usr/local/bin/"
      ELSE
         cPath_Hrb := "c:\Harbour\"
      ENDIF
   ENDIF

   RETURN Nil