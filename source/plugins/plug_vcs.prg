#define K_ENTER  13
#define K_ESC    27
STATIC cSvnName := "", cSvnPass := ""

FUNCTION Plug_Vcs( oEdit )

   LOCAL aMenu := { "Git: History", "Fossil: History" }, iChoic, ic
   LOCAL aMenu1 := { "Show file", "Diff with next", "Diff with last", "Diff with current" }, i1, o, o0
   LOCAL cCurrDir
   LOCAL cAddW0 := "$NextVer", cAddW1 := "$Vcs", cAddW2 := "$Diff", cBuff, cBuff0, arrh
   LOCAL cFileName := hb_FNameNameExt( oEdit:cFileName ), cv1, cv2
   LOCAL nRow := Row(), nCol := Col()

   cCurrDir := hb_FNameDir( oEdit:cFileName )
   IF ( ic := FMenu( oEdit, aMenu, 3, 10 ) ) == 1 .OR. ic == 2 .OR. ic == 3
      edi_Wait( Padc( "Wait...", 16 ), TEdit():cColorWR )
      IF ic == 1
         DirChange( cCurrDir )
         cBuff := cRun( 'git log --pretty=format:"%h %ad | %s%d [%an]" --graph --date=short ' + cFileName )
      ELSEIF ic == 2
         DirChange( cCurrDir )
         cBuff := cRun( 'fossil finfo -b ' + cFileName )
      ELSEIF ic == 3
         IF Empty( cSvnName )
            _vcs_ReadIni()
         ENDIF
         IF Empty( cSvnName ) .OR. Empty( cSvnPass )
            _vcs_GetPass( oEdit )
         ENDIF
         IF !Empty( cSvnName ) .AND. !Empty( cSvnPass )
            DirChange( cCurrDir )
            cBuff := cRun( 'svn log -q --username ' + cSvnName + ' --password ' + ;
               cSvnPass + " " + cFileName )
         ELSE
            DevPos( nRow, nCol )
            RETURN Nil
         ENDIF
      ENDIF
      edi_Wait()

      IF Empty( cBuff )
         edi_Alert( "No result" )
         DevPos( nRow, nCol )
         DirChange( edi_CurrPath() )
         RETURN Nil
      ENDIF

      arrh := hb_ATokens( cBuff, Chr(10) )
      FOR i1 := Len( arrh ) TO 1 STEP -1
         IF Right( arrh[i1],1 ) == Chr(13 )
            arrh[i1] := Left( arrh[i1], Len(arrh[i1])-1 )
         ENDIF
         IF Empty( arrh[i1] )
            hb_ADel( arrh, i1, .T. )
         ENDIF
      NEXT

      IF ( iChoic := FMenu( oEdit, arrh, 3, 10 ) ) > 0
         i1 := FMenu( oEdit, aMenu1, Int(MaxRow()/2)-3, Int(MaxCol()/2)-9 )
         cv1 := Iif( ic == 1, Substr( arrh[iChoic],3,7 ), Left( arrh[iChoic],10 ) )
         edi_Wait( Padc( "Wait...", 16 ), TEdit():cColorWR )
         IF i1 == 1
            cBuff0 := _vcs_GetFile( ic, cv1, cFileName )
         ELSEIF i1 == 2
            iChoic --
         ELSEIF i1 == 3
            iChoic := 1
         ELSEIF i1 == 4
            iChoic := 0
         ENDIF

         IF i1 > 1
            IF iChoic == 0
               IF ic == 1
                  cBuff := cRun( 'git diff ' + cv1 + " " + cFileName )
               ELSEIF ic == 2
                  cBuff := cRun( 'fossil diff --from ' + cv1 + " " + cFileName )
               ENDIF
            ELSE
               cv2 := Iif( ic == 1, Substr( arrh[iChoic],3,7 ), Left( arrh[iChoic],10 ) )
               IF !Empty( cBuff0 := _vcs_GetFile( ic, cv2, cFileName ) )
                  IF ic == 1
                     cBuff := cRun( 'git diff ' + cv2 + " " + cv1 + " " + cFileName )
                  ELSEIF ic == 2
                     cBuff := cRun( 'fossil diff --from ' + cv1 + " --to " + cv2 + " " + cFileName )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

         IF Empty( cBuff ) .OR. ( i1 > 1 .AND. iChoic > 0 .AND. Empty( cBuff0 ) )
            edi_Alert( "No result" )
         ELSE
            edi_CloseWindow( cAddW1 )
            edi_CloseWindow( cAddW0 )
            IF i1 == 1
               edi_CloseWindow( cAddW2 )
               o := edi_AddWindow( oEdit, cBuff, cAddW1, 3, Int(MaxCol()/2) )
               o:lReadOnly := .T.
            ELSEIF iChoic == 0
               edi_AddDiff( oEdit, cBuff, .T. )
            ELSE
               o0 := TEdit():New( cBuff0, cAddw0, oEdit:aRectFull[1], oEdit:aRectFull[2], oEdit:aRectFull[3], oEdit:aRectFull[4] )
               o0:lReadOnly := .T.
               o := edi_AddDiff( o0, cBuff, .T. )
               mnu_ToBuf( oEdit, o )
            ENDIF
         ENDIF
      ENDIF
      DirChange( edi_CurrPath() )
   ENDIF
   DevPos( nRow, nCol )

   RETURN Nil

STATIC FUNCTION _vcs_GetFile( ic, cv1, cFileName )

   LOCAL cBuff

   IF ic == 1
      cBuff := cRun( 'git show ' + cv1 + ":./" + cFileName )
   ELSEIF ic == 2
      cBuff := cRun( 'fossil finfo -p -r ' + cv1 + " " + cFileName )
   ENDIF

   RETURN cBuff

STATIC FUNCTION _vcs_GetPass( oEdit )

   LOCAL oldc := SetColor( oEdit:cColorSel + "," + oEdit:cColorMenu )
   LOCAL aGets := { {10,34,0,cSvnName,20}, {11,37,0,cSvnPass,17 }, {12,28,1,.F.,1}, ;
      {14,28,2,"[Ok]",4,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ENTER))}}, ;
      {14,42,2,"[Cancel]",10,oEdit:cColorSel,oEdit:cColorMenu,{||__KeyBoard(Chr(K_ESC))}} }
   LOCAL nRes

   hb_cdpSelect( "RU866" )
   @ 09, 25, 15, 56 BOX "ÚÄ¿³ÙÄÀ³ "
   @ 13, 25 SAY "Ã"
   @ 13, 55 SAY "´"
   @ 13, 26 TO 13, 54
   hb_cdpSelect( oEdit:cp )

   @ 10,27 SAY "Login: "
   @ 11,27 SAY "Password: "
   @ 12,27 SAY "[ ] Save"
   SetColor( oEdit:cColorMenu )

   IF ( nRes := edi_READ( aGets ) ) > 0 .AND. nRes < Len(aGets)
      cSvnName := aGets[1,4]
      cSvnPass := aGets[2,4]
      IF !Empty( cSvnName ) .AND. aGets[3,4]
      ENDIF
   ENDIF

   SetColor( oldc )
   edi_SetPos( oEdit )

   RETURN Nil

STATIC FUNCTION _vcs_ReadIni()

   RETURN Nil