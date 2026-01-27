/*
 * HbEdit plugin
 * HwBuild - HwGUI Builder
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"

#define HWB_VERSION  "1.20"

#define COMP_ID      1
#define COMP_EXE     2
#define COMP_LIBPATH 3
#define COMP_FLAGS   4        // C compiler flags
#define COMP_LFLAGSG 5        // Linker flags for gui app
#define COMP_LFLAGSC 6        // Linker flags for console app
#define COMP_LFLAGSL 7        // Linker flags for library
#define COMP_HVM     8
#define COMP_HWG     9
#define COMP_CMD1    10       // Template for compiling
#define COMP_CMD2    11       // Template for resource compiling
#define COMP_CMDL    12       // Template for lib linking
#define COMP_CMDE    13       // Template for exe linking
#define COMP_OBJEXT  14
#define COMP_LIBEXT  15
#define COMP_TMPLLIB 16
#define COMP_BINLIB  17
#define COMP_SYSLIBS 18

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0
#define CLR_DBLUE    0x614834
#define CLR_DGRAY1   0x222222
#define CLR_DGRAYA   0x404040
#define CLR_DGRAY2   0x555555
#define CLR_DGRAY3   0x888888
#define CLR_LGRAY1   0xdddddd

#define CLR_BLUE1   0x552916
#define CLR_BLUE2   0x794431
#define CLR_BLUE3   0x8e624f
#define CLR_BLUE4   0xab8778
#define CLR_BLUE5   0xc9a596
#define CLR_BLUE6   0xe7c3b4

#define K_DOWN        24

STATIC cIniPath
STATIC oPrg
STATIC lQ := .F.

STATIC cPathHrb := "", cPathHrbBin := "", cPathHrbInc := ""
STATIC cHrbDefFlags := "-n -q -w"
STATIC cGuiId := "hwgui", cPathHwgui := "", cPathHwguiInc := "", cPathHwguiLib := ""
STATIC lPathHrb := .F., lPathHrbBin := .F., lPathHrbInc := .F.
STATIC lPathHwgui := .F.

#ifdef __PLATFORM__UNIX
STATIC lUnix := .T.
STATIC cExeExt := ""
STATIC cLibsHrb := "hbvm hbrtl gtcgi gttrm hbcpage hblang hbrdd hbmacro hbpp rddntx rddcdx rddfpt hbsix hbcommon hbct hbcplr hbpcre hbzlib"
#else
STATIC lUnix := .F.
STATIC cExeExt := ".exe"
STATIC cLibsHrb := "hbvm hbrtl gtgui gtwin hbcpage hblang hbrdd hbmacro hbpp rddntx rddcdx rddfpt hbsix hbcommon hbct hbcplr hbpcre hbzlib"
#endif
STATIC cLibsHwGUI := ""

STATIC cFontMain := "", lProgressOn := .T., cExtView := ""
STATIC sResult
STATIC lCreatScr

FUNCTION HwBuilder( cFile, lFromEdit )

   LOCAL cExt, oPrg, lClean := .F., lNoGui := .F., oComp, cComp, aUserPar := {}, aFiles := {}
   LOCAL i, j, cDop, aDop, cGTlib := "", cLibsDop := "", cFlagsPrg := ""
   LOCAL cAddW := "$hb_compile_err", oOld, oPane

   IF Empty( lFromEdit ); lFromEdit := .F.; ENDIF
   cExt := Lower( hb_fnameExt( cFile ) )

   sResult := ""
   HCompiler():aList := {}
   HGuiLib():aList := {}
   ReadIni( "hwbuild.ini" )
   lQ := .F.
   lCreatScr := .F.
   IF( cDop := _GetParams( @oComp, (cExt==".hwprj") ) ) == Nil
      RETURN Nil
   ENDIF

   IF !Empty( cDop )
      IF "-q" $ cDop
         lQ := .T.
      ENDIF
      IF "-clean" $ cDop
         lClean := .T.
      ENDIF
      IF "-gui=" $ cDop
         lNoGui := .T.
      ENDIF
      IF "-creatscr" $ cDop
         lCreatScr := .T.
      ENDIF
      aDop := hb_Atokens( cDop, " ", .T. )
      FOR i := 1 TO Len( aDop )
         IF Left( aDop[i],3 ) == "-gt"
            cGTlib := Substr( aDop[i], 2 )
         ELSEIF Left( aDop[i],3 ) == "-pf"
            cFlagsPrg := _DropQuotes( Substr( aDop[1],4 ) )
         ELSEIF Left( aDop[i],2 ) == '-{'
            IF ( j := At( "}", aDop[i] ) ) > 3
               AAdd( aUserPar, Substr( aDop[i], 3, j-3 ) )
            ENDIF
         ELSEIF Left( aDop[i],2 ) == "-l"
            cLibsDop += _DropQuotes( Substr( aDop[1],3 ) )
         ELSEIF Left( aDop[i],1 ) != "-" .AND. Lower(hb_fnameExt(aDop[i])) == ".hwprj"
            Aadd( aFiles, {cFile,""} )
            cExt := ".hwprj"
            cFile := aDop[i]
         ENDIF
      NEXT
   ENDIF
   IF cExt == ".hwprj" .OR. cExt == ".prg" .OR. cExt == ".c" .OR. cExt == ".cpp"
      IF !lCreatScr
         @ 10, Int(MaxCol()/2)-4 SAY " Wait... " COLOR TEdit():cColorSel
      ENDIF
      IF cExt == ".hwprj"
         IF !Empty( oPrg := HwProject():Open( cFile, oComp, aUserPar, aFiles ) )
            oPrg:Build( lClean )
         ENDIF
      ELSEIF File( hb_fnameDir(cFile) + ".hwprj" )
         Aadd( aFiles, {cFile,""} )
         Aadd( aFiles, {".hwprj",""} )
         IF !Empty( oPrg := HwProject():Open( hb_fnameDir(cFile) + ".hwprj", oComp, aUserPar, aFiles ) )
            oPrg:Build()
         ENDIF
      ELSE
         oPrg := HwProject():New( {{cFile,""}}, oComp, cGTlib, Trim(cLibsDop), ;
            "", cFlagsPrg, "", "", "", .F., .F., lNoGui )
         oPrg:Build()
      ENDIF
      cFile := hb_fnameNameExt( cFile )
      IF Len( (oPane := FilePane():aPanes[1]):aDir ) >= oPane:nCurrent ;
        .AND. oPane:aDir[oPane:nCurrent,1] == cFile
        oPane:Refresh()
      ELSEIF Len( ( oPane := FilePane():aPanes[2]):aDir ) >= oPane:nCurrent ;
        .AND. oPane:aDir[oPane:nCurrent,1] == cFile
        oPane:Refresh()
      ENDIF
      IF !lFromEdit .AND. !lCreatScr
         oOld := TEdit():aWindows[TEdit():nCurr]
         TEdit():New( sResult, cAddW, TEdit():aRectFull[1], TEdit():aRectFull[2], TEdit():aRectFull[3], TEdit():aRectFull[4] )
         oOld:lShow := .F.
         TEdit():nCurr := Len( TEdit():aWindows )
      ENDIF
   ENDIF

   RETURN sResult

STATIC FUNCTION ShowResult()
   RETURN Nil

STATIC FUNCTION FPaths()
   RETURN Nil

STATIC FUNCTION _GetParams( oComp, lHwprj )

   LOCAL xRes := "", cBuf, oldc := SetColor( TEdit():cColorSel + "," + TEdit():cColorMenu )
   LOCAL aGets, y1, x1, x2, y2, i := 0, j

   y1 := Int( MaxRow()/2 ) - 1
   x1 := Int( MaxCol()/2 ) - 16
   x2 := x1 + 32

   aGets := { {y1,x1+4, 11, "Plugin parameters"}, ;
      { y1+1,x1+2, 11, "[ ] Short output" }, { y1+1,x1+3, 1, .T., 2 }, ;
      { y1+1,x1+21, 11, "[ ] Clean" }, { y1+1,x1+22, Iif(lHwprj,1,-1), .F., 2 }, ;
      { y1+2,x1+2, 11, "[ ] GUI app" }, { y1+2,x1+3, Iif(lHwprj,-1,1), .T., 2 }, ;
      { y1+2,x1+21, 11, "[ ] CrScr" }, { y1+2,x1+22, Iif(lHwprj,1,-1), .F., 2 }, ;
      { y1+3,x1+2, 11, Iif( lHwprj,'-{...}', '-gt... -l"lib1 lib2"' ) }, ;
      { y1+4,x1+2, 0, "", x2-x1-4 } }

   IF Len( HCompiler():aList ) > 1
      AAdd( aGets, { y1+5, x1+3, 3, .T., 1 } )
      AAdd( aGets, { y1+5,x1+2, 11, "(x) default" } )
      FOR i := 1 TO Len( HCompiler():aList )
         AAdd( aGets, { y1+5+i, x1+3, 3, .F., 1 } )
         AAdd( aGets, { y1+5+i,x1+2, 11, "( ) " + HCompiler():aList[i]:id } )
      NEXT
   ELSEIF Len( HCompiler():aList ) == 1
      oComp := HCompiler():aList[1]
   ENDIF
   y2 := y1 + 5 + i

   cBuf := Savescreen( y1, x1, y2, x2 )
   @ y1, x1, y2, x2 BOX "ÚÄ¿³ÙÄÀ³ "

   KEYBOARD Chr( K_DOWN ) + Chr( K_DOWN ) + Chr( K_DOWN )
   edi_READ( aGets )
   IF LastKey() == 13
      xRes := AllTrim( aGets[11,4] )
      IF aGets[3,4]
         xRes := Iif( Empty(xRes), "-q", xRes + " -q" )
      ENDIF
      IF aGets[5,4]
         xRes := Iif( Empty(xRes), "-clean", xRes + " -clean" )
      ENDIF
      IF !aGets[7,4]
         xRes := Iif( Empty(xRes), "-gui=", xRes + " -gui=" )
      ENDIF
      IF aGets[9,4]
         xRes := Iif( Empty(xRes), "-creatscr", xRes + " -creatscr" )
      ENDIF
      j := 0
      FOR i := 12 TO Len( aGets ) STEP 2
         IF aGets[i,4]
            IF j > 0
               oComp := HCompiler():aList[j]
            ENDIF
            EXIT
         ENDIF
         j ++
      NEXT
   ELSE
      xRes := Nil
   ENDIF
   SetColor( oldc )
   Restscreen( y1, x1, y2, x2, cBuf )

   RETURN xRes

STATIC FUNCTION CheckOptions( oProject, cLine )

   LOCAL nDef, oComp := oProject:oComp

   IF Empty( cPathHrbBin ) .OR. !File( _EnvVarsTran(cPathHrbBin) + hb_ps() + "harbour" + cExeExt )
      cLine := "Empty or wrong harbour executables path"
      RETURN 1
   ENDIF
   IF Empty( cPathHrbInc ) .OR. !File( _EnvVarsTran(cPathHrbInc) + hb_ps() + "hbsetup.ch" )
      cLine := "Empty or wrong harbour include path"
      RETURN 1
   ENDIF
   IF cGuiId == "hwgui" .AND. ( Empty( cPathHwguiInc ) .OR. ;
      !File( cPathHwguiInc + hb_ps() + "hwgui.ch" ) )
      cLine := "Empty or wrong hwgui include path"
      RETURN 2
   ENDIF

   IF ( nDef := Ascan( HCompiler():aDef, {|a|a[COMP_ID] == oComp:id} ) ) > 0
      IF !oProject:lLib .AND. cGuiId == "hwgui" .AND. ( Empty( cPathHwguiLib ) .OR. ;
         ( !File( cPathHwguiLib + hb_ps() + HCompiler():aDef[nDef,COMP_HWG] ) .AND. ;
         !File( cPathHwguiLib + hb_ps() + oComp:id + hb_ps() + HCompiler():aDef[nDef,COMP_HWG] ) ) )
         cLine := "Empty or wrong hwgui libraries path"
         RETURN 2
      ENDIF
#ifndef __PLATFORM__UNIX
      IF Empty( oComp:cPath ) .OR. !File( _EnvVarsTran(oComp:cPath) + hb_ps() + ;
         HCompiler():aDef[nDef,COMP_EXE] )
         cLine := "Empty or wrong " + oComp:id + " path"
         RETURN 3
      ENDIF
#endif
      IF !oProject:lLib .AND. ( Empty( oComp:cPathHrbLib ) .OR. ;
         !File( _EnvVarsTran(oComp:cPathHrbLib) + hb_ps() + HCompiler():aDef[nDef,COMP_HVM] ) )
         cLine := "Empty or wrong " + oComp:id + " harbour libraries path"
         RETURN 1
      ENDIF
   ENDIF

   RETURN 0

STATIC FUNCTION FindHarbour()

   LOCAL aEnv, cTmp, cPath, nPos

   IF Empty( cPathHrb ) .AND. Empty( cPathHrb := getenv("HB_PATH") ) .AND. Empty( cPathHrb := getenv("HB_ROOT") )
      cTmp := "harbour" + cExeExt
      aEnv := hb_ATokens( getenv("PATH"), hb_osPathListSeparator() )
      FOR EACH cPath IN aEnv
         IF File( _DropSlash(cPath) + hb_ps() + cTmp )
            cPathHrbBin := _DropSlash( cPath )
            EXIT
         ENDIF
      NEXT
      IF !Empty( cPathHrbBin )
         IF ( nPos := hb_At( hb_ps()+"bin", cPathHrbBin ) ) > 0
            cPathHrb := Left( cPathHrbBin, nPos-1 )
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION ReadIni( cFile )

   LOCAL cPath, hIni, aIni, arr, nSect, aSect, cTmp, i, key, nPos, cFam, oComp, oGui
   LOCAL aEnv, aMsvc := Array(4), aEnvM

   IF File( cPath := ( _CurrPath() + cFile ) ) .OR. ;
      File( cPath := ( hb_DirBase() + "plugins" + hb_ps() + cFile ) )
      hIni := edi_IniRead( cPath )
      _MsgInfo( "Read options from " + cPath + hb_eol() )
   ENDIF
   cIniPath := cPath

   IF !Empty( hIni )
      hb_hCaseMatch( hIni, .F. )
      IF hb_hHaskey( hIni, cTmp := "HARBOUR" ) .AND. !Empty( aSect := hIni[ cTmp ] )
         hb_hCaseMatch( aSect, .F. )
         IF hb_hHaskey( aSect, cTmp := "harbour_path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cPathHrb := _DropSlash( cTmp )
            lPathHrb := .T.
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "harbour_bin_path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cPathHrbBin := _DropSlash( cTmp )
            lPathHrbBin := .T.
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "harbour_include_path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cPathHrbInc := _DropSlash( cTmp )
            lPathHrbInc := .T.
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "def_flags" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cHrbDefFlags := cTmp
         ENDIF
         IF hb_hHaskey( aSect, cTmp := "libs" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
            cLibsHrb := cTmp
         ENDIF
      ENDIF
      FindHarbour()

      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Left( aIni[nSect], 6 ) == "GUILIB" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            IF hb_hHaskey( aSect, cTmp := "id" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               cGuiId := cTmp
               oGui := HGuilib():New( cTmp )

               IF hb_hHaskey( aSect, cTmp := "path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
                  oGui:cPath := _DropSlash( cTmp )
                  lPathHwgui := .T.
               ENDIF
               IF hb_hHaskey( aSect, cTmp := "inc_path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
                  oGui:cPathInc := _DropSlash( cTmp )
               ENDIF
               IF hb_hHaskey( aSect, cTmp := "lib_path" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
                  oGui:cPathLib := _DropSlash( cTmp )
               ENDIF
               IF hb_hHaskey( aSect, cTmp := "libs" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
                  oGui:cLibs := cTmp
               ENDIF
            ENDIF

         ELSEIF Left( aIni[nSect], 10 ) == "C_COMPILER" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            IF hb_hHaskey( aSect, cTmp := "id" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               cFam := Iif( hb_hHaskey( aSect, cFam := "family" ) .AND. ;
                  !Empty( cFam := aSect[ cFam ] ), cFam, "" )
               oComp := HCompiler():New( cTmp, cFam )
               arr := hb_hKeys( aSect )
               FOR EACH key IN arr
                  IF key == "bin_path" .AND. !Empty( cTmp := aSect[ key ] )
                     oComp:cPath := _DropSlash( cTmp )
                     oComp:lPath := .T.
                  ELSEIF key == "harbour_lib_path" .AND. !Empty( cTmp := aSect[ key ] )
                     oComp:cPathHrbLib := _DropSlash( cTmp )
                     oComp:lPathHrbLib := .T.
                  ELSEIF key == "def_cflags" .AND. !Empty( cTmp := aSect[ key ] )
                     oComp:cFlags := cTmp
                     oComp:lFlags := .T.
                  ELSEIF key == "def_linkflags" .AND. !Empty( cTmp := aSect[ key ] )
                     oComp:cLinkFlagsGui := cTmp
                     oComp:lLinkFlagsGui := .T.
                  ELSEIF key == "def_linkflagscons" .AND. !Empty( cTmp := aSect[ key ] )
                     oComp:cLinkFlagsCons := cTmp
                     oComp:lLinkFlagsCons := .T.
                  ELSEIF key == "def_libflags" .AND. !Empty( cTmp := aSect[ key ] )
                     oComp:cLinkFlagsLib := cTmp
                     oComp:lLinkFlagsLib := .T.
                  ELSEIF key == "def_syslibs" .AND. !Empty( cTmp := aSect[ key ] )
                     oComp:cSysLibs := cTmp
                     oComp:lSysLibs := .T.
                  ELSEIF Left(key,4) == "env_" .AND. !Empty( cTmp := aSect[ key ] )
                     IF ( nPos := At( '=', cTmp ) ) > 0
                        AAdd( oComp:aEnv, {Left( cTmp,nPos-1 ), Substr( cTmp,nPos+1 )} )
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ELSEIF aIni[nSect] == "VIEW" .AND. !Empty( aSect := hIni[ aIni[nSect] ] )
            hb_hCaseMatch( aSect, .F. )
            IF hb_hHaskey( aSect, cTmp := "font" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               cFontMain := cTmp
            ENDIF
            IF hb_hHaskey( aSect, cTmp := "progressbar" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               lProgressOn := ( Lower(cTmp) == "on" )
            ENDIF
            IF hb_hHaskey( aSect, cTmp := "extview" ) .AND. !Empty( cTmp := aSect[ cTmp ] )
               cExtView := cTmp
            ENDIF
         ENDIF
      NEXT
   ELSE
      FindHarbour()
   ENDIF

   IF !Empty( cPathHrb )
      IF Empty( cPathHrbBin )
         cPathHrbBin := cPathHrb + hb_ps() + "bin"
      ENDIF
      IF Empty( cPathHrbInc )
         cPathHrbInc := cPathHrb + hb_ps() + "include"
#ifdef __PLATFORM__UNIX
         IF !hb_DirExists( _EnvVarsTran(cPathHrbInc) ) .AND. cPathHrbBin == "/usr/local/bin"
            cPathHrbInc := "/usr/local/include/harbour"
         ENDIF
#endif
      ENDIF
   ENDIF

   IF Empty( HGuilib():aList )
      HGuilib():New( "hwgui" )
   ENDIF
   oGui := HGuilib():aList[1]
   cGuiId := oGui:id
   cPathHwgui := _EnvVarsTran(oGui:cPath)
   cPathHwguiInc := _EnvVarsTran(oGui:cPathInc)
   cPathHwguiLib := _EnvVarsTran(oGui:cPathLib)
   cLibsHwGUI := oGui:cLibs

   IF !Empty( cPathHwgui )
      IF Empty( cPathHwguiInc )
         cPathHwguiInc := cPathHwgui + hb_ps() + "include"
      ENDIF
      IF Empty( cPathHwguiLib )
         cPathHwguiLib := cPathHwgui + hb_ps() + "lib"
      ENDIF
   ENDIF

#ifdef __PLATFORM__UNIX
   IF ( i := Ascan( HCompiler():aList, {|o|o:id == "gcc"} ) ) > 0
      oComp := HCompiler():aList[i]
   ELSE
      oComp := HCompiler():New( "gcc" )
   ENDIF
   IF !Empty( cPathHrb ) .AND. Empty( oComp:cPathHrbLib )
      oComp:cPathHrbLib := cPathHrb + "/lib/linux/gcc"
      IF !hb_DirExists( _EnvVarsTran(oComp:cPathHrbLib) ) .AND. cPathHrbBin == "/usr/local/bin"
         oComp:cPathHrbLib := "/usr/local/lib/harbour"
      ENDIF
   ENDIF

#else
   IF Empty( HCompiler():aList )
      HCompiler():New( "bcc" )
      HCompiler():New( "mingw" )
   ENDIF
   FOR EACH oComp IN HCompiler():aList

      IF Empty( oComp:cPath ) .AND. ( i := Ascan( HCompiler():aDef, {|a|a[COMP_ID] == oComp:id} ) ) > 0
         IF Empty( aEnv )
            aEnv := hb_ATokens( getenv("PATH"), hb_osPathListSeparator() )
         ENDIF
         IF oComp:id == "bcc" .OR. oComp:id == "mingw"
            FOR EACH cPath IN aEnv
               IF File( _DropSlash(cPath) + hb_ps() + HCompiler():aDef[i,COMP_EXE] )
                  oComp:cPath := _DropSlash( cPath )
                  EXIT
               ENDIF
            NEXT
         ELSEIF oComp:id == "msvc"
            IF Empty( oComp:aEnv )
               IF !Empty( aMsvc[4] := getenv( "LIBPATH" ) ) .AND. "Microsoft" $ aMsvc[4] .AND. ;
                  !Empty( aMsvc[3] := getenv( "LIB" ) ) .AND. !Empty( aMsvc[2] := getenv( "INCLUDE" ) ) ;
                  .AND. !Empty( aMsvc[1] := getenv( "PATH" ) )
                  aEnvM := hb_ATokens( aMsvc[1], hb_osPathListSeparator() )
                  FOR EACH cPath IN aEnv
                     IF File( _DropSlash(cPath) + hb_ps() + HCompiler():aDef[i,COMP_EXE] )
                        oComp:cPath := _DropSlash( cPath )
                        EXIT
                     ENDIF
                  NEXT
                  IF !Empty( oComp:cPath )
                     AAdd( oComp:aEnv, { "PATH",aMsvc[1] } )
                     AAdd( oComp:aEnv, { "INCLUDE", aMsvc[2] } )
                     AAdd( oComp:aEnv, { "LIB", aMsvc[3] } )
                     AAdd( oComp:aEnv, { "LIBPATH", aMsvc[4] } )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT
#endif

   IF IsIniDataChanged()
      WriteIni()
   ENDIF

   RETURN Nil

STATIC FUNCTION WriteIni()

   LOCAL cr := hb_eol(), oComp, oGui, n := 0, n1, aEnv
   LOCAL s := "[HARBOUR]" + cr + "harbour_path=" + cPathHrb + cr + "harbour_bin_path=" + ;
      cPathHrbBin + cr + "harbour_include_path=" + cPathHrbInc + cr + "def_flags=" + cHrbDefFlags + ;
      cr + "libs=" + cLibsHrb + cr + cr

   FOR EACH oGui IN HGuilib():aList
      n ++
      s += "[GUILIB" + Iif( n == 1, "", "_" + Ltrim(Str(n)) ) + "]" + cr + ;
         "id=" + oGui:id + cr + "path=" + oGui:cPath + cr + ;
         "inc_path=" + oGui:cPathInc + cr + "lib_path=" + oGui:cPathLib + cr + ;
         "libs=" + oGui:cLibs + cr + cr
   NEXT

   n := 0
   FOR EACH oComp IN HCompiler():aList
      n ++
      s += "[C_COMPILER" + Iif( n == 1, "", "_" + Ltrim(Str(n)) ) + "]" + cr + ;
         "id=" + oComp:id + "family=" + oComp:family + cr + "bin_path=" + ;
         oComp:cPath + cr + "harbour_lib_path=" + oComp:cPathHrbLib + cr + ;
         "def_cflags=" + oComp:cFlags + cr + "def_linkflags=" + oComp:cLinkFlagsGui + cr + ;
         + "def_libflags=" + oComp:cLinkFlagsLib + cr + "def_syslibs=" + oComp:cSysLibs + cr
      n1 := 0
      FOR EACH aEnv in oComp:aEnv
         s += "env_" + Ltrim(Str(++n1)) + "=" + aEnv[1] + "=" + aEnv[2] + cr
      NEXT
      s += cr
   NEXT

   s += "[VIEW]" + cr + "font=" + cFontMain + cr + "progressbar=" + Iif( lProgressOn, "On", "" ) + ;
      cr + "extview=" + cExtView + cr + cr

   _MsgInfo( "Update options in " + cIniPath + hb_eol() )
   hb_MemoWrit( cIniPath, s )

   RETURN Nil

STATIC FUNCTION IsIniDataChanged()

   LOCAL oComp

   IF !lPathHrb .AND. !Empty(cPathHrb)
      RETURN ( lPathHrb := .T. )
   ENDIF
   IF !lPathHrbBin .AND. !Empty(cPathHrbBin)
      RETURN ( lPathHrbBin := .T. )
   ENDIF
   IF !lPathHrbInc .AND. !Empty(cPathHrbInc)
      RETURN ( lPathHrbInc := .T. )
   ENDIF
   IF !lPathHwgui .AND. !Empty(cPathHwgui)
      RETURN ( lPathHwgui := .T. )
   ENDIF

   FOR EACH oComp IN HCompiler():aList
      IF !oComp:lPath .AND. !Empty( oComp:cPath )
         RETURN ( oComp:lPath := .T. )
      ENDIF
      IF !oComp:lPathHrbLib .AND. !Empty( oComp:cPathHrbLib )
         RETURN ( oComp:lPathHrbLib := .T. )
      ENDIF
      IF !oComp:lFlags .AND. !Empty( oComp:cFlags )
         RETURN ( oComp:lFlags := .T. )
      ENDIF
      IF !oComp:lLinkFlagsGui .AND. !Empty( oComp:cLinkFlagsGui )
         RETURN ( oComp:lLinkFlagsGui := .T. )
      ENDIF
      IF !oComp:lLinkFlagsCons .AND. !Empty( oComp:cLinkFlagsCons )
         RETURN ( oComp:lLinkFlagsCons := .T. )
      ENDIF
      IF !oComp:lLinkFlagsLib .AND. !Empty( oComp:cLinkFlagsLib )
         RETURN ( oComp:lLinkFlagsLib := .T. )
      ENDIF
      IF !oComp:lSysLibs .AND. !Empty( oComp:cSysLibs )
         RETURN ( oComp:lSysLibs := .T. )
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION _DropQuotes( cLine )

   IF Left( cLine, 1 ) == '"' .AND. Right( cLine, 1 ) == '"'
      RETURN Substr( cLine, 2, Len( cLine ) - 2 )
   ENDIF

   RETURN cLine

STATIC FUNCTION _DropSlash( cLine )

   IF Right( cLine,1 ) $ "/\"
      RETURN hb_strShrink( cLine, 1 )
   ENDIF
   RETURN cLine

STATIC FUNCTION _DropBr( cLine )

   LOCAL nPos

   IF Left( cLine,1 ) == "{"
      IF ( nPos := At( "}", cLine ) ) > 0
         RETURN Substr( cLine,nPos+1 )
      ELSE
         RETURN ""
      ENDIF
   ENDIF

   RETURN cLine

STATIC FUNCTION _HasError( cLine )

   LOCAL nPos := 1, c, l

   DO WHILE ( nPos := hb_AtI( "error", cLine, nPos ) ) > 0

      l := .F.
      IF nPos > 1
         c := Substr( cLine, nPos-1, 1 )
         IF c < 'A' .OR. (c > 'Z' .AND. c < 'a' .AND. c != '_' ) .OR. c > 'z'
            l := .T.
         ENDIF
      ENDIF
      nPos += 5
      c := Iif( nPos > Len( cLine ), 'a', Substr( cLine, nPos, 1 ) )
      IF c < 'A' .OR. (c > 'Z' .AND. c < 'a' .AND. c != '_') .OR. c > 'z'
         IF l
            RETURN .T.
         ENDIF
      ENDIF
   ENDDO

   RETURN .F.

STATIC FUNCTION _EnvVarsTran( cLine )

   LOCAL nPos := 1, nPos2, nLen, cVar, cValue

   DO WHILE ( nPos := hb_At( '%', cLine, nPos ) ) > 0
      IF ( nPos2 := hb_At( '%', cLine, nPos+1 ) ) > 0
         cVar := Substr( cLine, nPos+1, nPos2-nPos-1 )
         IF !Empty( cValue := Getenv( cVar ) )
            cLine := Left( cLine, nPos-1 ) + cValue + Substr( cLine, nPos2+1 )
         ELSE
            _MsgStop( cVar, "Variable does not exist" )
            RETURN cLine
         ENDIF
      ELSE
         _MsgStop( cLine, "Wrong line in ini" )
         RETURN cLine
      ENDIF
   ENDDO

   nPos := 1
   DO WHILE ( nPos := hb_At( '$', cLine, nPos ) ) > 0
      nPos2 := nPos + 2
      nLen := Len( cLine )
      DO WHILE nPos2 <= nLen .AND. !( Substr( cLine, nPos2, 1 ) $ "/\. " )
         nPos2 ++
      ENDDO
      cVar := Substr( cLine, nPos+1, nPos2-nPos-1 )
      IF !Empty( cValue := Getenv( cVar ) )
         cLine := Left( cLine, nPos-1 ) + cValue + Substr( cLine, nPos2 )
      ELSE
         _MsgStop( cVar, "Variable does not exist" )
         RETURN cLine
      ENDIF
      nPos ++
   ENDDO

   RETURN cLine

STATIC FUNCTION _PrjVarsTran( aPrjVars, cLine )

   LOCAL i, nPos := 1, nPos2, nLen, cVar, cValue

#ifdef __PLATFORM__UNIX
   IF !Empty( aPrjVars ) .AND. !lCreatScr
#else
   IF !Empty( aPrjVars )
#endif
      DO WHILE ( nPos := hb_At( '$', cLine, nPos ) ) > 0
         nPos2 := nPos + 2
         nLen := Len( cLine )
         DO WHILE nPos2 <= nLen .AND. !( Substr( cLine, nPos2, 1 ) $ "/\.,;: " )
            nPos2 ++
         ENDDO
         cVar := Substr( cLine, nPos+1, nPos2-nPos-1 )
         IF lCreatScr
            cLine := Left( cLine, nPos-1 ) + "%" + cVar + "%" + Substr( cLine, nPos2 )
         ELSE
            IF ( i := Ascan( aPrjVars, {|a|a[1] == cVar} ) ) > 0
               cValue := aPrjVars[i,2]
               cLine := Left( cLine, nPos-1 ) + cValue + Substr( cLine, nPos2 )
            ENDIF
         ENDIF
         nPos ++
      ENDDO
   ENDIF

   RETURN cLine

STATIC FUNCTION _AddFromFile( cFile, l2Line )

   LOCAL cPath

   IF Empty( hb_fnameDir( cFile := _DropQuotes(cFile) ) )
#ifdef __PLATFORM__UNIX
      IF File( cPath := ( _CurrPath() + cFile ) ) .OR. ;
         File( cPath := ( getenv("HOME") + "/hwbuild/" + cFile ) ) .OR. ;
         File( cPath := ( hb_DirBase() + cFile ) )
#else
      IF File( cPath := ( _CurrPath() + cFile ) ) .OR. ;
         File( cPath := ( hb_DirBase() + cFile ) )
#endif
         cFile := cPath
      ELSE
         RETURN Nil
      ENDIF
   ELSEIF !File( cFile )
      RETURN Nil
   ENDIF

   cPath := StrTran( Memoread( cPath ), Chr(13), "" )
   IF l2Line
      RETURN StrTran( cPath, Chr(10), " " )
   ENDIF

   RETURN cPath

#ifdef __PLATFORM__UNIX
STATIC FUNCTION _RunApp( cLine, cOut )
   RETURN cedi_RunConsoleApp( cLine + " 2>&1",, @cOut )
#else
STATIC FUNCTION _RunApp( cLine, cOut )
   cOut := ""
   RETURN cedi_RunConsoleApp( cLine,, @cOut )
#endif

STATIC FUNCTION _ShowProgress( cText, nAct, cTitle, cFull )

   HB_SYMBOL_UNUSED( cFull )
   IF !lQ .OR. nAct == 2 .OR. "warning" $ Lower(cText) .OR. "error" $ Lower(cText)
      IF !Empty( cTitle )
         sResult += "*** " + cTitle + " *** " + hb_eol()
      ENDIF
      IF nAct == 2
         sResult += "=== " + cText + " ===" + hb_eol()
      ELSE
         sResult += cText + hb_eol()
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION _MsgStop( cText, cTitle )

   IF !Empty( cTitle )
      sResult += hb_eol() + cTitle
   ENDIF
   sResult += hb_eol() + cText
   RETURN Nil

STATIC FUNCTION _MsgInfo( cText, cTitle )

   IF !Empty( cTitle )
      sResult += hb_eol() + cTitle
   ENDIF
   sResult += hb_eol() + cText
   RETURN Nil

STATIC FUNCTION _PS( cPath )

   RETURN Iif( !Empty( cPath ), Iif( lUnix .AND. '\' $ cPath, StrTran( cPath, '\', '/' ), ;
      Iif( !lUnix .AND. '/' $ cPath, StrTran( cPath, '/', '\' ), cPath ) ), cPath )

STATIC FUNCTION _CurrPath()
   RETURN Iif( hb_Version(20), '/', hb_curDrive() + ':\' ) + CurDir() + hb_ps()

#define HB_FA_XUSR    0x00400000  /* 0100 execute/search by owner */

STATIC FUNCTION _CreateScr( cLine )

   LOCAL cFile := Iif( hb_Version(20), "_hwprj.sh", "_hwprj.bat" ), nAttr
   STATIC s := ""

   IF Empty( cLine )
      hb_MemoWrit( cFile, s )
      s := ""
      IF hb_Version(20)
         IF hb_fGetAttr( cFile, @nAttr )
            hb_fSetAttr( cFile, hb_BitOr( nAttr, HB_FA_XUSR ) )
         ENDIF
      ENDIF
      edi_Alert( "Build script " + cFile + " created!" )
   ELSE
      s += cLine + hb_eol()
   ENDIF
   RETURN Nil

CLASS HCompiler

   CLASS VAR aDef        SHARED INIT { ;
      {"bcc", "bcc32.exe", "\lib\win\bcc", "-c -d -w -O2", "-Gn -aa -Tpe c0w32.obj", "-Gn -ap c0x32.obj", "", ;
         "hbvm.lib", "hwgui.lib", ;
         "{path}\bcc32.exe {f} -I{hi} -I{gi} -o{obj} {src}", ;
         "{path}\brc32 -r {src} -fo{out}", ;
         "{path}\tlib {f} {out} {objs}", ;
         "{path}\ilink32 -L{hL} -L{gL} {dL} {f} {objs}, {out},, {libs},, {res}", ;
         "", "", "", "", "ws2_32.lib cw32.lib import32.lib iphlpapi.lib" }, ;
      {"mingw", "gcc.exe", "\lib\win\mingw", "-c -Wall", "-Wall -mwindows", "-Wall", "", ;
         "libhbvm.a", "libhwgui.a", ;
         "{path}\gcc {f} -I{hi} -I{gi} -o{obj} {src}", ;
         "{path}\windres {src} {out}.o", ;
         "{path}\ar rc {f} {out} {objs}", ;
         "{path}\gcc {f} -o{out} {objs} {res} -L{hL} -L{gL} {dL} -Wl,--allow-multiple-definition -Wl,--start-group {libs} -Wl,--end-group", ;
         ".o", ".a", "-l{l}", "lib{l}.a", ;
         "-luser32 -lwinspool -lcomctl32 -lcomdlg32 -lgdiplus -lgdi32 -lole32 -loleaut32 -luuid -lwinmm -lws2_32 -lwsock32 -liphlpapi" }, ;
      {"msvc", "cl.exe", "\lib\win\msvc", "/TP /W3 /nologo /c", "-SUBSYSTEM:WINDOWS", "", "", ;
         "hbvm.lib", "hwgui.lib", ;
         "cl.exe {f} /I{hi} /I{gi} /Fo{obj} {src}", ;
         "rc -fo {out}.res {src}", ;
         "lib {f} /out:{out} {objs}", ;
         "link {f} /LIBPATH:{hL} /LIBPATH:{gL} {dL} {objs} {res} {libs}", "", "", "", "", ;
         "user32.lib gdi32.lib comdlg32.lib shell32.lib comctl32.lib winspool.lib advapi32.lib winmm.lib ws2_32.lib iphlpapi.lib OleAut32.Lib Ole32.Lib" }, ;
      {"gcc", "gcc", "/lib/linux/gcc", "-c -Wall -Wunused `pkg-config --cflags gtk+-2.0`", ;
         "`pkg-config --libs gtk+-2.0`", "", "", "libhbvm.a", "libhwgui.a", ;
         "gcc {f} -I{hi} -I{gi} -o{obj} {src}", ;
         "", ;
         "ar rc {f} {out} {objs}", ;
         "gcc {objs} -o{out} -L{hL} -L{gL} {dL} -Wl,--start-group {libs} -Wl,--end-group {f}", ;
         ".o", ".a", "-l{l}", "lib{l}.a", "-lm -lz -lpcre -ldl" } }
   //,, hwgui_xp.res
   CLASS VAR aList       SHARED INIT {}

   DATA id
   DATA family           INIT ""
   DATA cPath            INIT ""
   DATA cPathInc         INIT ""
   DATA cPathLib         INIT ""
   DATA cPathHrbLib      INIT ""
   DATA cFlags           INIT ""
   DATA cLinkFlagsGui    INIT ""
   DATA cLinkFlagsCons   INIT ""
   DATA cLinkFlagsLib    INIT ""
   DATA cObjExt          INIT ".obj"
   DATA cLibExt          INIT ".obj"
   DATA cSysLibs         INIT ""

   DATA cCmdComp         INIT ""
   DATA cCmdRes          INIT ""
   DATA cCmdLinkLib      INIT ""
   DATA cCmdLinkExe      INIT ""
   DATA cTmplLib         INIT "{l}.lib"
   DATA cBinLib          INIT "{l}.lib"

   DATA aEnv             INIT {}

   DATA lPath            INIT .F.
   DATA lPathHrbLib      INIT .F.
   DATA lFlags           INIT .F.
   DATA lLinkFlagsGui    INIT .F.
   DATA lLinkFlagsCons   INIT .F.
   DATA lLinkFlagsLib    INIT .F.
   DATA lSysLibs         INIT .F.

   METHOD New( id, cFam )

ENDCLASS

METHOD New( id, cFam ) CLASS HCompiler

   LOCAL nDef, cTmp

   ::id := id
   IF !Empty( cFam ) .AND. ( nDef := Ascan( HCompiler():aDef, {|a|a[COMP_ID] == cFam} ) ) > 0
      ::family := cFam
   ELSEIF ( nDef := Ascan( HCompiler():aDef, {|a|a[COMP_ID] == id} ) ) > 0
      ::family := id
   ENDIF
   IF nDef > 0
      ::cPathHrbLib := cPathHrb + HCompiler():aDef[nDef,COMP_LIBPATH]
      ::cFlags := HCompiler():aDef[nDef,COMP_FLAGS]
      ::cLinkFlagsGui := HCompiler():aDef[nDef,COMP_LFLAGSG]
      ::cLinkFlagsCons := HCompiler():aDef[nDef,COMP_LFLAGSC]
      ::cLinkFlagsLib := HCompiler():aDef[nDef,COMP_LFLAGSL]
      ::cCmdComp := HCompiler():aDef[nDef,COMP_CMD1]
      ::cCmdRes  := HCompiler():aDef[nDef,COMP_CMD2]
      ::cCmdLinkLib := HCompiler():aDef[nDef,COMP_CMDL]
      ::cCmdLinkExe := HCompiler():aDef[nDef,COMP_CMDE]
      ::cSysLibs := HCompiler():aDef[nDef,COMP_SYSLIBS]
      IF !Empty( cTmp := HCompiler():aDef[nDef,COMP_TMPLLIB] )
         ::cTmplLib  := cTmp
      ENDIF
      IF !Empty( cTmp := HCompiler():aDef[nDef,COMP_BINLIB] )
         ::cBinLib  := cTmp
      ENDIF
      IF !Empty( cTmp := HCompiler():aDef[nDef,COMP_OBJEXT] )
         ::cObjExt  := cTmp
      ENDIF
      IF !Empty( cTmp := HCompiler():aDef[nDef,COMP_LIBEXT] )
         ::cLibExt  := cTmp
      ENDIF
   ENDIF

   AAdd( ::aList, Self )

   RETURN Self

CLASS HGuilib

   CLASS VAR aList       SHARED INIT {}
   DATA id
   DATA cPath      INIT ""
   DATA cPathInc   INIT ""
   DATA cPathLib   INIT ""
   DATA cLibs      INIT ""

   METHOD New( id )

ENDCLASS

METHOD New( id ) CLASS HGuilib

   ::id := id

   IF id == "hwgui"
      ::cLibs := "hwgui hbxml procmisc"
   ENDIF

   AAdd( ::aList, Self )

   RETURN Self

CLASS HwProject

   DATA cFile
   DATA aFiles     INIT {}
   DATA oComp
   DATA cGtLib
   DATA cLibsDop   INIT ""
   DATA cLibsPath  INIT ""
   DATA cFlagsPrg  INIT ""
   DATA cFlagsC    INIT ""
   DATA cOutPath, cOutName, cObjPath
   DATA lLib       INIT .F.
   DATA lMake      INIT .F.
   DATA lHarbour   INIT .F.
   DATA lGuiLib    INIT .T.
   DATA lBuildRes  INIT .T.
   DATA lGuiLinkFlag  INIT .F.

   DATA cDefFlagsC INIT Nil
   DATA cDefFlagsL INIT Nil
   DATA cDefFlagsLib INIT Nil

   DATA aProjects  INIT {}

   METHOD New( aFiles, oComp, cGtLib, cLibsDop, cLibsPath, cFlagsPrg, cFlagsC, ;
      cOutName, cObjPath, lLib, lMake, lNoGui )
   METHOD Open( xSource, oComp, aUserPar, aFiles, aParentVars )
   METHOD Build( lClean )

ENDCLASS

METHOD New( aFiles, oComp, cGtLib, cLibsDop, cLibsPath, cFlagsPrg, cFlagsC, ;
      cOutName, cObjPath, lLib, lMake, lNoGui ) CLASS HwProject

   LOCAL i

   IF PCount() > 1
      ::aFiles := aFiles
      ::oComp  := Iif( Empty( oComp ), HCompiler():aList[1], oComp )
      ::cGtLib    := cGtLib
      ::cLibsDop  := Iif( Empty( cLibsDop ) , "", cLibsDop )
      IF !Empty( cGtLib )
         //::cLibsDop := Iif( Empty( cLibsDop ), cGtLib, cLibsDop + " " + cGtLib )
         ::cFlagsPrg += " -d__" + Upper( cGtLib ) + "__"
      ENDIF
      ::cLibsPath := Iif( Empty(cLibsPath), "", cLibsPath )
      ::cFlagsPrg := Iif( Empty(cFlagsPrg), "", cFlagsPrg )
      ::cFlagsC   := Iif( Empty(cFlagsC), "", cFlagsC )
      IF !Empty( cOutName )
         ::cOutName := hb_fnameNameExt( cOutName )
         IF Len( ::cOutName ) < Len( cOutName )
            ::cOutPath := Left( cOutName, Len( cOutName ) - Len( ::cOutName ) - 1 )
         ENDIF
      ENDIF
      ::cObjPath  := cObjPath
      ::lLib  := lLib
      ::lMake := lMake
      IF lNoGui
         ::lGuiLib := .F.
      ENDIF
      FOR i := 1 TO Len( ::aFiles )
         IF Lower( hb_fnameExt( ::aFiles[i,1] ) ) == ".prg"
           ::lHarbour := .T.
         ENDIF
      NEXT
   ENDIF

   RETURN Self

METHOD Open( xSource, oComp, aUserPar, aFiles, aParentVars ) CLASS HwProject

   LOCAL arr, i, j, n, l, lYes, nPos, af, ap, o, oGui
   LOCAL cLine, cTmp, cTmp2, cSrcPath := "", lLib, lCompDefault := .F.
   LOCAL aPrjVars := Iif( Empty( aParentVars ), {}, AClone( aParentVars ) )

   IF Empty( oComp )
      oComp := HCompiler():aList[1]
      lCompDefault := .T.
   ENDIF

   IF Valtype( xSource ) == "A"
      arr := AClone( xSource )
   ELSEIF Chr(10) $ xSource
      arr := hb_Atokens( xSource, Chr(10) )
   ELSEIF !File( xSource )
      _MsgStop( xSource + " not found", "Wrong file" )
      RETURN Nil
   ELSE
      ::cFile := xSource
      arr := hb_Atokens( Memoread( xSource ), Chr(10) )
   ENDIF
   IF Empty( aPrjVars )
      _MsgInfo( "User params: " + hb_ValToExp( aUserPar )  + hb_eol() )
      AAdd( aPrjVars, {"COMPILER",oComp:id} )
      AAdd( aPrjVars, {"HRBPATH", cPathHrb} )
      AAdd( aPrjVars, {"HWGUIPATH", cPathHwgui} )
   ENDIF

   IF !Empty( aFiles )
      FOR i := 1 TO Len( aFiles )
         IF !( Lower( hb_fnameExt( "x"+hb_fnameNameExt(aFiles[i,1]) ) ) == ".hwprj" )
            AAdd( ::aFiles, aFiles[i] )
         ENDIF
      NEXT
   ENDIF
   FOR i := 1 TO Len( arr )
      IF !Empty( cLine := AllTrim( StrTran( arr[i], Chr(13), "" ) ) ) .AND. !( Left( cLine, 1 ) == "#" )
         DO WHILE Left( cLine,1 ) == '{'
            IF ( nPos := At( "}", cLine ) ) > 0
               cTmp := AllTrim( Substr( cLine, 2, nPos-2 ) )
               l := .T.
               IF Left( cTmp,1 ) == "!"
                  cTmp := LTrim( Substr( cTmp,2 ) )
                  l := .F.
               ENDIF
               lYes := ( ( cTmp == "unix" .AND. lUnix ) .OR. ;
                  ( cTmp == "win" .AND. !lUnix ) .OR. oComp:family == cTmp .OR. oComp:id == cTmp .OR. ;
                  ( !Empty( aUserPar ) .AND. hb_Ascan( aUserPar,cTmp,,,.T. ) > 0 ) )
               IF !l
                  lYes := !lYes
               ENDIF
               IF lYes
                  cLine := LTrim( Substr( cLine, nPos + 1 ) )
               ELSE
                  cLine := ""
                  EXIT
               ENDIF
            ELSE
               _MsgStop( cLine, "Wrong option" )
               RETURN Nil
            ENDIF
         ENDDO
         IF Empty( cLine )
            LOOP
         ENDIF
         IF ( nPos := At( "=", cLine ) ) > 0 .AND. !( " " $ (cTmp := Trim( Left( cLine, nPos-1 ) )) )
            IF Left( cTmp,1 ) == "$"
               AAdd( aPrjVars, { Substr( cTmp, 2 ), AllTrim( Substr( cLine, nPos + 1 ) ) } )
               IF lCreatScr
#ifdef __PLATFORM__UNIX
                  _CreateScr( "export " + Substr(cTmp,2) + '="' + AllTrim( Substr( cLine, nPos + 1 ) )  + '"')
#else
                  _CreateScr( "set " + Substr(cTmp,2) + "=" + AllTrim( Substr( cLine, nPos + 1 ) ) )
#endif
               ENDIF
            ELSEIF ( cTmp := Lower( cTmp ) ) == "srcpath"
               cSrcPath := _PS( _DropSlash( Substr( cLine, nPos + 1 ) ) ) + hb_ps()

            ELSEIF cTmp == "def_cflags"
               ::cDefFlagsC := _PrjVarsTran( aPrjVars, Substr( cLine, nPos + 1 ) )

            ELSEIF cTmp == "def_lflags"
               ::cDefFlagsL := _PrjVarsTran( aPrjVars, Substr( cLine, nPos + 1 ) )

            ELSEIF cTmp == "def_libflags"
               ::cDefFlagsLib := _PrjVarsTran( aPrjVars, Substr( cLine, nPos + 1 ) )

            ELSEIF cTmp == "prgflags"
               ::cFlagsPrg += ( Iif( Empty(::cFlagsPrg), "", " " ) + ;
                  _PrjVarsTran( aPrjVars, Substr( cLine, nPos + 1 ) ) )

            ELSEIF cTmp == "cflags"
               ::cFlagsC += ( Iif( Empty(::cFlagsC), "", " " ) + ;
                  _PrjVarsTran( aPrjVars, Substr( cLine, nPos + 1 ) ) )

            ELSEIF cTmp == "gtlib"
               ::cGtLib := Substr( cLine, nPos + 1 )

            ELSEIF cTmp == "libs"
               ::cLibsDop += Iif( Empty(::cLibsDop), "", " " ) + ;
                  _PrjVarsTran( aPrjVars, Substr( cLine, nPos + 1 ) )

            ELSEIF cTmp == "libspath"
               ::cLibsPath := _PrjVarsTran( aPrjVars, _DropSlash( Substr( cLine, nPos + 1 ) ) )

            ELSEIF cTmp == "outpath"
               ::cOutPath := _PrjVarsTran( aPrjVars, _DropSlash( Substr( cLine, nPos + 1 ) ) )

            ELSEIF cTmp == "outname"
               ::cOutName := Substr( cLine, nPos + 1 )

            ELSEIF cTmp == "objpath"
               ::cObjPath := _PrjVarsTran( aPrjVars, _DropSlash( Substr( cLine, nPos + 1 ) ) )

            ELSEIF cTmp == "target"
               ::lLib := Substr( cLine, nPos + 1 ) == "lib"

            ELSEIF cTmp == "makemode"
               ::lMake := ( cTmp := Lower( Substr( cLine, nPos + 1 ) ) ) == "on" .OR. cTmp == "yes"

            ELSEIF cTmp == "c_compiler"
               cTmp := Substr( cLine, nPos + 1 )
               IF ( j := Ascan( HCompiler():aList, {|o|o:id == cTmp} ) ) > 0
                  IF lCompDefault
                     oComp := HCompiler():aList[j]
                     aPrjVars[1,2] := oComp:id
                  ENDIF
               ELSE
                  _MsgStop( cLine, "Wrong compiler id" )
                  RETURN Nil
               ENDIF
            ELSEIF cTmp == "guilib"
               cTmp := Substr( cLine, nPos + 1 )
               IF Empty( cTmp ) .OR. cTmp == '""'
                  ::lGuiLib := .F.
               ELSEIF ( j := Ascan( HGuilib():aList, {|o|o:id == cTmp} ) ) > 0
                  oGui := HGuilib():aList[j]
                  cGuiId := oGui:id
                  cPathHwgui := _EnvVarsTran(oGui:cPath)
                  cPathHwguiInc := _EnvVarsTran(oGui:cPathInc)
                  cPathHwguiLib := _EnvVarsTran(oGui:cPathLib)
                  cLibsHwGUI := oGui:cLibs
               ENDIF

            ELSE
               _MsgStop( cLine, "Wrong option" )
               RETURN Nil
            ENDIF

         ELSEIF Left( cLine,1 ) == '@'
            cTmp := _AddFromFile( Substr( cLine,2 ), .F. )
            ap := hb_ATokens( cTmp, Chr(10) )
            n := i + 1
            FOR j := 1 TO Len( ap )
               IF !Empty( ap[j] )
                  hb_AIns( arr, n, ap[j], .T. )
                  n ++
               ENDIF
            NEXT

         ELSEIF Left( cLine,1 ) == ':'
            IF Left( cLine,8 ) == ':project'
               ap := {}
               DO WHILE ++i <= Len( arr ) .AND. !Left( Ltrim(_DropBr(arr[i])),8 ) == ':project'
                  AAdd( ap, arr[i] )
               ENDDO
               IF i < Len( arr )
                  i --
               ENDIF
               AAdd( ::aProjects, o := HwProject():Open( ap, oComp, aUserPar,, aPrjVars ) )
               IF o == Nil
                  RETURN Nil
               ENDIF
               IF o:lHarbour
                  ::lHarbour := .T.
               ENDIF
               IF Empty( o:cObjPath ) .AND. !Empty( ::cObjPath )
                  o:cObjPath := ::cObjPath
               ENDIF
               IF Empty( o:cOutPath ) .AND. !Empty( ::cOutPath )
                  o:cOutPath := ::cOutPath
               ENDIF
               IF Empty( o:lMake ) .AND. !Empty( ::lMake )
                  o:lMake := ::lMake
               ENDIF
               IF Empty( o:cFlagsPrg ) .AND. !Empty( ::cFlagsPrg )
                  o:cFlagsPrg := ::cFlagsPrg
               ENDIF
               IF Empty( o:cFlagsC ) .AND. !Empty( ::cFlagsC )
                  o:cFlagsC := ::cFlagsC
               ENDIF
               IF Empty( o:cDefFlagsC ) .AND. !Empty( ::cDefFlagsC )
                  o:cDefFlagsC := ::cDefFlagsC
               ENDIF
               IF Empty( o:cDefFlagsL ) .AND. !Empty( ::cDefFlagsL )
                  o:cDefFlagsL := ::cDefFlagsL
               ENDIF
               IF Empty( o:cDefFlagsLib ) .AND. !Empty( ::cDefFlagsLib )
                  o:cDefFlagsLib := ::cDefFlagsLib
               ENDIF
            ELSE
               _MsgStop( cLine, "Wrong option" )
               RETURN Nil
            ENDIF

         ELSE
            IF ( nPos := At( " ", cLine ) ) > 0
               cTmp := Left( cLine, nPos - 1 )
               cTmp2 := _PrjVarsTran( aPrjVars, AllTrim( Substr( cLine, nPos + 1 ) ) )
            ELSE
               cTmp := cLine
               cTmp2 := Nil
            ENDIF
            IF '*' $ cTmp
               ap := Nil
               IF !Empty( cTmp2 ) .AND. ( nPos := At( "-(", cTmp2 ) ) > 0 .AND. ;
                  ( j := hb_At( ")", cTmp2, nPos ) ) > 0
                  ap := hb_aTokens( Substr( cTmp2, nPos+2, j-nPos-2 ), ' ' )
                  cTmp2 := AllTrim( Left( cTmp2, nPos-1 ) + Substr( cTmp2, j+1 ) )
               ENDIF
               af := hb_Directory( cSrcPath + cTmp )
               FOR j := 1 TO Len( af )
                  IF Empty( ap ) .OR. hb_AScan( ap, af[j,1],,, .T. ) == 0
                     AAdd( ::aFiles, { cSrcPath+af[j,1], cTmp2 } )
                  ENDIF
               NEXT
            ELSE
               AAdd( ::aFiles, { Iif( Empty( cSrcPath ) .AND. Empty( hb_fnameDir(cTmp) ), ;
                  cTmp, cSrcPath + cTmp ), cTmp2 } )
            ENDIF

         ENDIF
      ENDIF
   NEXT

   IF !Empty( ::aProjects ) .AND. Empty( ::aFiles ) .AND. !::lLib
      lLib := .T.
      FOR i := 1 TO Len( ::aProjects )
         IF !::aProjects[i]:lLib
            lLib := .F.
            EXIT
         ENDIF
      NEXT
      ::lLib := lLib
   ENDIF
   IF Empty( ::aFiles ) .AND. Empty( ::aProjects )
      _MsgStop( "Source files missing", "Project error" )
      RETURN Nil
   ELSE
      FOR i := 1 TO Len( ::aFiles )
         IF Lower( hb_fnameExt( ::aFiles[i,1] ) ) == ".prg"
           ::lHarbour := .T.
         ENDIF
      NEXT
   ENDIF

   ::oComp := oComp

   RETURN Self

METHOD Build( lClean, lSub ) CLASS HwProject

   LOCAL i, cCmd, cLine, cOut, cFullOut := "", lErr := .F., to, tc
   LOCAL cObjs := "", cFile, cExt, cBinary, cObjFile, cObjPath
   LOCAL aLibs, cLibs := "", a4Delete := {}, tStart := hb_DtoT( Date(), Seconds()-1 )
   LOCAL aEnv, cResFile, cResList := ""
   LOCAL cCompPath, cCompHrbLib, cCompGuiLib

   ::lBuildRes := ::lGuiLib
   IF !Empty( ::cGtLib )
      ::lBuildRes := .F.
      IF !( ::cGtLib == "gthwg" )
         ::lGuiLib := .F.
      ENDIF
      ::cFlagsPrg += " -d__" + Upper( ::cGtLib ) + "__"
      IF ::cGtLib $ "gttrm;gtwvt;gtxwc;gtwvg;gtwvw;gthwg"
         ::lGuiLinkFlag := .T.
      ENDIF
   ENDIF
   IF ::lGuiLib
      ::lGuiLinkFlag := .T.
   ENDIF

   IF Empty( lSub ) .AND. Empty( lClean ) .AND. !Empty( i := CheckOptions( Self, @cLine ) )
      IF !( i == 2 .AND. !::lGuiLib )
         _MsgStop( cLine + hb_eol() + "Check your hwbuild.ini", "Wrong options" )
         FPaths()
         //RETURN Nil
      ENDIF
   ENDIF

   IF Empty( lSub )
      IF lCreatScr
#ifdef __PLATFORM__UNIX
        _CreateScr( "export COMPILER=" + ::oComp:id )
#else
        _CreateScr( "set COMPILER=" + ::oComp:id )
#endif
      ENDIF
      IF !Empty( ::oComp:aEnv )
         aEnv := Array( Len(::oComp:aEnv),2 )
         FOR i := 1 TO Len( ::oComp:aEnv )
            aEnv[i,1] := ::oComp:aEnv[i,1]
            aEnv[i,2] := getenv( aEnv[i,1] )
            hb_setenv( ::oComp:aEnv[i,1], ::oComp:aEnv[i,2] )
            IF lCreatScr
#ifdef __PLATFORM__UNIX
               _CreateScr( "export " + ::oComp:aEnv[i,1] + "=" + ::oComp:aEnv[i,2] )
#else
               _CreateScr( "set " + ::oComp:aEnv[i,1] + "=" + ::oComp:aEnv[i,2] )
#endif
            ENDIF
         NEXT
      ELSEIF ::oComp:family == "msvc"
         _ShowProgress( "Error: Environment variables are absent in hwbuild.ini", 1,, @cFullOut )
      ENDIF
   ENDIF

   FOR i := 1 TO Len( ::aProjects )
      cFullOut += ::aProjects[i]:Build( lClean, .T. )
   NEXT

   IF Empty( ::aFiles )
      IF !Empty( cFullOut )
         ShowResult( cFullOut )
      ENDIF
      RETURN ""
   ENDIF

   IF !Empty( ::cObjPath )
      cObjPath := _PS( ::cObjPath ) + hb_ps() + ::oComp:id
      IF !hb_DirExists( cObjPath )
         hb_DirBuild( cObjPath )
      ENDIF
      cObjPath += hb_ps()
   ELSE
      cObjPath := ""
   ENDIF

   IF !Empty( lClean )
      FOR i := 1 TO Len( ::aFiles )
         IF Lower( hb_fnameExt( ::aFiles[i,1] )) == ".prg"
            FErase( cObjPath + hb_fnameName( ::aFiles[i,1] ) + ".c" )
         ENDIF
         FErase( cObjPath + hb_fnameName( ::aFiles[i,1] ) + ::oComp:cObjExt )
      NEXT
      cBinary := Iif( Empty( ::cOutName ), hb_fnameNameExt( ::aFiles[1,1] ), ::cOutName )
      IF ::lLib
         cBinary := Iif( Empty(::cOutPath), "", ::cOutPath+hb_ps() ) + StrTran( ::oComp:cBinLib, "{l}", cBinary )
      ELSE
         cBinary := Iif( Empty(::cOutPath), "", ::cOutPath+hb_ps() ) + hb_fnameExtSet( cBinary, cExeExt )
      ENDIF
      FErase( cBinary )
      _MsgInfo( "Cleaned" )
      RETURN ""
   ENDIF

   IF !Empty( ::cOutPath := _PS(::cOutPath) ) .AND. !hb_DirExists( ::cOutPath )
      hb_DirBuild( ::cOutPath )
   ENDIF

   FOR i := 1 TO Len( ::aFiles )
      IF !( cFile := Lower( hb_fnameExt(::aFiles[i,1]) ) ) == ".prg" .AND. !( cFile == ".c" ) ;
         .AND. !( cFile == ".cpp" ).AND. !( cFile == ::oComp:cObjExt ) .AND. !( cFile == ".rc" )
         _MsgStop( "Wrong source file extention", hb_fnameNameExt(::aFiles[i,1]) )
         RETURN ""
      ENDIF
   NEXT

   cCompPath := _EnvVarsTran( ::oComp:cPath )
   cCompHrbLib := _EnvVarsTran( ::oComp:cPathHrbLib )

   _ShowProgress( "Harbour: "+Iif(::lHarbour,"Yes","No") + ;
      " Guilib: "+Iif(::lGuiLib,"Yes","No") + " BuildRes: "+Iif(::lBuildRes,"Yes","No") + ;
      " GuiFlags: "+Iif(::lGuiLinkFlag,"Yes","No"), 1,, @cFullOut )
   // Compile prg sources with Harbour
   cCmd := _EnvVarsTran(cPathHrbBin) + hb_ps() + "harbour " + cHrbDefFlags + ;
      " -i" + _EnvVarsTran(cPathHrbInc) + Iif( ::lGuiLib, " -i" + cPathHwguiInc, "" ) + ;
      Iif( Empty( ::cFlagsPrg ), "", " " + ::cFlagsPrg ) + ;
      Iif( Empty( cObjPath ), "", " -o" + cObjPath )
   FOR i := 1 TO Len( ::aFiles )
      cFile := _PS( ::aFiles[i,1] )
      IF Lower( hb_fnameExt( cFile )) == ".prg"
         cObjFile := cObjPath + hb_fnameName( cFile ) + ".c"
         IF ::lMake .AND. File( cObjFile ) .AND. hb_vfTimeGet( cObjFile, @to ) .AND. ;
            hb_vfTimeGet( cFile, @tc ) .AND. to >= tc
         ELSE
            cLine := cCmd + Iif( Empty( ::aFiles[i,2] ), "", " " + ::aFiles[i,2] ) + " " + cFile
            IF lCreatScr
               _CreateScr( cLine )
            ELSE
               _ShowProgress( "> " + cLine, 1, hb_fnameNameExt( cFile ), @cFullOut )
               _RunApp( cLine, @cOut )
               IF Valtype( cOut ) != "C"
                  _ShowProgress( "Error: the Harbour compiler didn't start", 1,, @cFullOut )
                  lErr := .T.
                  EXIT
               ENDIF
               _ShowProgress( cOut, 1,, @cFullOut )
               IF "Error" $ cOut
                  lErr := .T.
                  EXIT
               ENDIF
            ENDIF
         ENDIF
         ::aFiles[i,1] := cObjFile
         ::aFiles[i,2] := Nil
         IF !::lMake
            AAdd( a4Delete, ::aFiles[i,1] )
         ENDIF
      ENDIF
   NEXT

   IF !lErr
      // Compile C sources with C compiler
      cOut := Nil
      cCmd := StrTran( StrTran( StrTran( ::oComp:cCmdComp, "{hi}", ;
         Iif( ::lLib.OR.::lHarbour,_EnvVarsTran(cPathHrbInc),"." ) ), ;
         "{gi}", Iif( ::lGuiLib.AND.::lHarbour,cPathHwguiInc,"." ) ), "{path}", cCompPath )

      FOR i := 1 TO Len( ::aFiles )
         cFile := _PS( ::aFiles[i,1] )
         IF ( cExt := Lower( hb_fnameExt( cFile )) ) == ".c" .OR. cExt == ".cpp"
            cObjFile := cObjPath + hb_fnameName( cFile ) + ::oComp:cObjExt
            IF ::lMake .AND. !lCreatScr .AND. File( cObjFile ) .AND. hb_vfTimeGet( cObjFile, @to ) .AND. ;
               hb_vfTimeGet( cFile, @tc ) .AND. to >= tc
            ELSE
               cLine := StrTran( StrTran( StrTran( cCmd, "{obj}", cObjFile ), ;
                  "{src}", cFile ), ;
                  "{f}", Iif( ::cDefFlagsC==Nil, ::oComp:cFlags, ::cDefFlagsC ) + ;
                  Iif( Empty( ::cFlagsC ), "", " " + ::cFlagsC ) + ;
                  Iif( Empty( ::aFiles[i,2] ), "", " " + ::aFiles[i,2] ) )

               IF lCreatScr
                  _CreateScr( cLine )
               ELSE
                  _ShowProgress( "> " + cLine, 1, hb_fnameNameExt(cFile), @cFullOut )
                  _RunApp( cLine, @cOut )
                  IF Valtype( cOut ) != "C"
                     _ShowProgress( "Error: the compiler didn't start", 1,, @cFullOut )
                     lErr := .T.
                     EXIT
                  ENDIF
                  _ShowProgress( cOut, 1,, @cFullOut )
                  IF _HasError( cOut )
                     //_ShowProgress( "Error: "+cLine, 1,, @cFullOut )
                     lErr := .T.
                     EXIT
                  ENDIF
              ENDIF
            ENDIF
            cObjs += " " + cObjFile
            IF !::lMake
               AAdd( a4Delete, cObjFile )
            ENDIF
         ENDIF
      NEXT
   ENDIF

#ifndef __PLATFORM__UNIX
   IF ::lBuildRes .AND. !::lLib .AND. !lErr .AND. !Empty( ::oComp:cCmdRes )
      // Compile resource files
      cOut := Nil
      IF File( cPathHwgui + "\image\WindowsXP.Manifest" )
         cLine := '1 24 "' + cPathHwgui + '\image\WindowsXP.Manifest"'
         cResFile := "hwgui_xp.res"
         IF ::oComp:family == "mingw"
            cLine := Strtran( cLine, '\', '/' )
            cResFile := "hwgui_xp.o"
         ENDIF
         hb_MemoWrit( "hwgui_xp.rc", cLine )
         cLine := StrTran( StrTran( StrTran( ::oComp:cCmdRes, "{path}", cCompPath ), ;
         "{src}", "hwgui_xp.rc" ), "{out}", "hwgui_xp" )
         IF lCreatScr
            _CreateScr( cLine )
         ELSE
            _ShowProgress( "> " + cLine, 1,, @cFullOut)
            _RunApp( cLine, @cOut )
            IF Valtype( cOut ) == "C"
               _ShowProgress( cOut, 1,, @cFullOut )
               AAdd( a4Delete, "hwgui_xp.rc" )
               AAdd( a4Delete, cResFile )
               cResList += cResFile
            ELSE
               _ShowProgress( "Error: the resource compiler didn't start", 1,, @cFullOut )
               lErr := .T.
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   IF ::lGuiLinkFlag .AND. !::lLib .AND. !lErr .AND. !Empty( ::oComp:cCmdRes )
      cOut := Nil
      FOR i := 1 TO Len( ::aFiles )
         cFile := _PS( ::aFiles[i,1] )
         IF Lower( hb_fnameExt( cFile )) == ".rc"
            cLine := StrTran( StrTran( StrTran( ::oComp:cCmdRes, "{path}", cCompPath ), ;
               "{src}", cFile ), "{out}", hb_fnameName( cFile ) + ;
               Iif( ::oComp:family == "mingw", "_rc", "" ) )
            IF lCreatScr
               _CreateScr( cLine )
            ELSE
               _ShowProgress( "> " + cLine, 1,, @cFullOut)
               _RunApp( cLine, @cOut )
               IF Valtype( cOut ) == "C"
                  _ShowProgress( cOut, 1,, @cFullOut )
                  cResFile := hb_fnameName( cFile ) + Iif( ::oComp:family == "mingw", "_rc.o", ".res" )
                  AAdd( a4Delete, cResFile )
                  cResList += " " + cResFile
               ELSE
                 _ShowProgress( "Error: the resource compiler didn't start", 1,, @cFullOut )
                 lErr := .T.
                 EXIT
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF
#endif

   IF !lErr
      // Link the app
      cBinary := Iif( Empty( ::cOutName ), hb_fnameNameExt( ::aFiles[1,1] ), ::cOutName )
      cOut := Nil
      IF hb_Ascan( hb_ATokens( ::cFlagsPrg ), "-b",,, .T. ) > 0
         aLibs := { "hwgdebug", "hbdebug" }
         FOR i := 1 TO Len( aLibs )
            cLibs += " " + StrTran( ::oComp:cTmplLib, "{l}", aLibs[i] )
         NEXT
      ENDIF
      IF ::lGuiLib .AND. ::lHarbour
         aLibs := hb_ATokens( cLibsHwGUI, " " )
         FOR i := 1 TO Len( aLibs )
            cLibs += " " + StrTran( ::oComp:cTmplLib, "{l}", aLibs[i] )
         NEXT
      ENDIF
      IF ::lHarbour
         IF !Empty( ::cGtLib )
            cLibs += " " + StrTran( ::oComp:cTmplLib, "{l}", ::cGtLib )
         ENDIF
         aLibs := hb_ATokens( cLibsHrb, " " )
         FOR i := 1 TO Len( aLibs )
            cLibs += " " + StrTran( ::oComp:cTmplLib, "{l}", aLibs[i] )
         NEXT
      ENDIF
      IF !Empty( ::cLibsDop )
         aLibs := hb_ATokens( ::cLibsDop, Iif( ',' $ ::cLibsDop, ",", " " ) )
         FOR i := 1 TO Len( aLibs )
            cLibs += " " + StrTran( ::oComp:cTmplLib, "{l}", aLibs[i] )
         NEXT
      ENDIF
      IF ::lLib
         cBinary := Iif( Empty(::cOutPath), Iif(lUnix,"./",""), ::cOutPath+hb_ps() ) + StrTran( ::oComp:cBinLib, "{l}", cBinary )
         FErase( cBinary )
         cLine := StrTran( StrTran( StrTran( StrTran( ::oComp:cCmdLinkLib, "{out}", cBinary ), ;
            "{objs}", Iif( ::oComp:family == "bcc", StrTran( cObjs, " ", " +" ), cObjs ) ), ;
            "{path}", cCompPath ), "{f}", Iif( ::cDefFlagsLib == Nil, ::oComp:cLinkFlagsLib, ::cDefFlagsLib ) )
      ELSE
         cBinary := Iif( Empty(::cOutPath), "", ::cOutPath+hb_ps() ) + hb_fnameExtSet( cBinary, cExeExt )
         cCompGuiLib := cPathHwguiLib + hb_ps() + ::oComp:id
         cCompGuiLib := Iif( hb_direxists(cCompGuiLib), cCompGuiLib, cPathHwguiLib )
         cLine := StrTran( StrTran( StrTran( StrTran( StrTran( StrTran( StrTran( StrTran( StrTran( ;
             ::oComp:cCmdLinkExe, "{out}", cBinary ), "{objs}", cObjs ), "{path}", cCompPath ), ;
             "{f}", Iif( ::cDefFlagsL == Nil, Iif( ::lGuiLinkFlag, ::oComp:cLinkFlagsGui, ;
             ::oComp:cLinkFlagsCons ), ::cDefFlagsL ) ), ;
             "{hL}", cCompHrbLib ), "{gL}", Iif( ::lGuiLib.AND.::lHarbour, cCompGuiLib,"." ) ), ;
             "{dL}", Iif( Empty(::cLibsPath), "", Iif(::oComp:family=="msvc","/LIBPATH:","-L") + ::cLibsPath ) ), ;
             "{libs}", cLibs + " " + ::oComp:cSysLibs ), "{res}", cResList )
         IF ::oComp:family == "bcc"
            AAdd( a4Delete, hb_fnameExtSet( cBinary, "tds" ) )
            AAdd( a4Delete, hb_fnameExtSet( cBinary, "map" ) )
         ENDIF
      ENDIF

      IF lCreatScr
         _CreateScr( cLine )
      ELSE
         _ShowProgress( "> " + cLine, 1, hb_fnameNameExt(cBinary), @cFullOut )
         FErase( cBinary )
         _RunApp( cLine, @cOut )
         IF Valtype( cOut ) == "C"
            _ShowProgress( cOut, 1,, @cFullOut )
         ENDIF

         cLine := Iif( File( cBinary ) .AND. hb_vfTimeGet( cBinary, @to ) .AND. to > tStart, ;
            cBinary + " " + "created successfully!", "Error. Can't create " + cBinary )
         _ShowProgress( cLine, 2,, @cFullOut )
      ENDIF
   ELSE
      _ShowProgress( "Error...", 2,, @cFullOut )
   ENDIF

   IF !Empty( aEnv ) .AND. Empty( lSub )
      FOR i := 1 TO Len( aEnv )
         hb_setenv( aEnv[i,1], aEnv[i,2] )
      NEXT
   ENDIF

   IF lCreatScr
      _CreateScr()
   ELSE
      IF Empty( lSub )
         ShowResult( cFullOut )
      ENDIF
   ENDIF
   FOR i := 1 TO Len( a4Delete )
      FErase( a4Delete[i] )
   NEXT

   RETURN Iif( Empty( lSub ), Nil, cFullOut )