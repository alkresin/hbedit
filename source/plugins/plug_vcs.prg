#define ALT_PRESSED   0x040000
#define K_ALT_D   288
#define K_ALT_I   279
#define K_ENTER    13
#define K_ESC      27

STATIC lCurr, oParent

FUNCTION Plug_Vcs( oEdit )

   LOCAL aMenu := { "Git: History", "Fossil: History" }, iChoic, ic
   LOCAL aMenu1 := { "Show file", "Diff with next", "Diff with last", "Diff with current" }, i1, nPos, o
   LOCAL cCurrDir, cFileRes := hb_DirTemp() + "pluggit.out", cAddW := "$Vcs", cBuff, arrh
   LOCAL cFileName := hb_FNameNameExt( oEdit:cFileName ), cv1, cv2
   LOCAL nRow := Row(), nCol := Col()

   cCurrDir := hb_FNameDir( oEdit:cFileName )
   IF ( ic := FMenu( oEdit, aMenu, 3, 10 ) ) == 1 .OR. ic == 2
      DirChange( cCurrDir )
      IF ic == 1
         cedi_RunConsoleApp( 'git log --pretty=format:"%h %ad | %s%d [%an]" --graph --date=short ' + ;
            cFileName, cFileRes )
      ELSEIF ic == 2
         cedi_RunConsoleApp( 'fossil finfo -b ' + cFileName, cFileRes )
      ENDIF

      IF Empty( cBuff := MemoRead(cFileRes) )
         edi_Alert( "No result" )
         DevPos( nRow, nCol )
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
         edi_Wait( Padc( "Wait...", 16 ), TEdit():cColorWR )
         cv1 := Iif( ic == 1, Substr( arrh[iChoic],3,7 ), Left( arrh[iChoic],10 ) )
         IF i1 == 1
             IF ic == 1
               cedi_RunConsoleApp( 'git show ' + cv1 + ":./" + cFileName, cFileRes )
            ELSEIF ic == 2
               cedi_RunConsoleApp( 'fossil finfo -p -r ' + cv1 + " " + cFileName, cFileRes )
            ENDIF
         ELSEIF i1 == 2
            iChoic --
         ELSEIF i1 == 3
            iChoic := 1
         ELSEIF i1 == 4
            iChoic := 0
         ENDIF

         IF iChoic == 0
            lCurr := .T.
            IF ic == 1
               cedi_RunConsoleApp( 'git diff ' + cv1 + " " + cFileName, cFileRes )
            ELSEIF ic == 2
               cedi_RunConsoleApp( 'fossil diff --from ' + cv1 + " " + cFileName, cFileRes )
            ENDIF
         ELSE
            lCurr := .F.
            cv2 := Iif( ic == 1, Substr( arrh[iChoic],3,7 ), Left( arrh[iChoic],10 ) )
            IF ic == 1
               cedi_RunConsoleApp( 'git diff ' + cv2 + " " + cv1 + " " + cFileName, cFileRes )
            ELSEIF ic == 2
               cedi_RunConsoleApp( 'fossil diff --from ' + cv1 + " --to " + cv2 + " " + cFileName, cFileRes )
            ENDIF
         ENDIF

         IF Empty( cBuff := MemoRead(cFileRes) )
            edi_Alert( "No result" )
         ELSE
            oParent := oEdit
            IF ( nPos := Ascan( oEdit:aWindows, {|o|o:cFileName==cAddW} ) ) > 0
               o := oEdit:aWindows[nPos]
               o:SetText( cBuff, cAddW )
               mnu_ToBuf( oEdit, nPos )
            ELSE
               o := edi_AddWindow( oEdit, cBuff, cAddW, 3, Int(MaxCol()/2) )
               o:lReadOnly := .T.
            ENDIF
            FOR i1 := Len( o:aText ) TO 1 STEP -1
               IF Right( o:aText[i1],1 ) == Chr(13 )
                  o:aText[i1] := Left( o:aText[i1], Len(o:aText[i1])-1 )
               ENDIF
            NEXT
            IF i1 == 1
               o:bOnKey := Nil
            ELSE
               o:bOnKey := {|o,n|_plug_vcs_onkey( o,n )}
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   DevPos( nRow, nCol )

   RETURN Nil

FUNCTION _plug_vcs_onkey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), n, nminus, c, nPos

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
   ELSEIF nKey == K_ENTER
      IF lCurr
         n := oEdit:nLine
         nminus := 0
         DO WHILE n >= 1
           IF ( c := Left( oEdit:aText[n],1 ) ) == '-'
              nMinus ++
           ELSEIF c == '@' .AND. Left( oEdit:aText[n],2 ) == '@@'
              IF ( nPos := At( '+', oEdit:aText[n] ) ) > 0
                 nPos := Val( Substr( oEdit:aText[n], nPos+1 ) )
                 oParent:GoTo( oEdit:nLine - n - nminus + nPos - 1, 1 )
                 mnu_ToBuf( oEdit, oParent )
              ENDIF
              EXIT
           ENDIF
           n --
         ENDDO
      ENDIF
   ENDIF

   RETURN 0
