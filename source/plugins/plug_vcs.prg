FUNCTION Plug_Vcs( oEdit )

   LOCAL aMenu := { "Git: History", "Fossil: History" }, iChoic, ic
   LOCAL aMenu1 := { "Show file", "Diff with next", "Diff with previous", "Diff with last", "Diff with current" }, i1, cAddW := "$Git", nPos, o
   LOCAL cCurrDir, cFileRes := hb_DirTemp() + "pluggit.out", cBuff, arrh
   LOCAL nRow := Row(), nCol := Col()

   cCurrDir := hb_FNameDir( oEdit:cFileName )
   IF ( ic := FMenu( oEdit, aMenu, 3, 10 ) ) == 1 .OR. ic == 2
      DirChange( cCurrDir )
      IF ic == 1
         cedi_RunConsoleApp( 'git log --pretty=format:"%h %ad | %s%d [%an]" --graph --date=short ' + ;
            hb_FNameNameExt( oEdit:cFileName ), cFileRes )
      ELSEIF ic == 2
         cedi_RunConsoleApp( 'fossil finfo -b ' + hb_FNameNameExt( oEdit:cFileName ), cFileRes )
      ENDIF
      IF Empty( cBuff := MemoRead(cFileRes) )
         edi_Alert( "No result" )
      ELSE
         arrh := hb_ATokens( cBuff, Chr(10) )
         FOR i1 := Len( arrh ) TO 1 STEP -1
            IF Right( arrh[i1],1 ) == Chr(13 )
               arrh[i1] := Left( arrh[i1], Len(arrh[i1])-1 )
               IF Empty( arrh[i1] )
                  hb_ADel( arrh, i1, .T. )
               ENDIF
            ENDIF
         NEXT
         IF ( iChoic := FMenu( oEdit, arrh, 3, 10 ) ) > 0
            IF ( i1 := FMenu( oEdit, aMenu1, Int(MaxRow()/2)-3, Int(MaxCol()/2)-9 ) ) > 0
               IF i1 == 1
                  IF ic == 1
                     cedi_RunConsoleApp( 'git show ' + Substr( arrh[iChoic],3,7 ) + ":./" + ;
                        hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ELSEIF ic == 2
                     cedi_RunConsoleApp( 'fossil finfo -p -r ' + Left( arrh[iChoic],10 ) + ;
                        " " + hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ENDIF
               ELSEIF i1 == 2
                  IF iChoic == 1
                     edi_Alert( "Not available" )
                     i1 := 0
                  ELSEIF ic == 1
                     cedi_RunConsoleApp( 'git diff ' + Substr( arrh[iChoic],3,7 ) + " " + ;
                        Substr( arrh[iChoic-1],3,7 ) + " " + hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ELSEIF ic == 2
                     cedi_RunConsoleApp( 'fossil diff --from ' + Left( arrh[iChoic-1],10 ) + " --to " + ;
                        Left( arrh[iChoic],10 ) + " " + hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ENDIF
               ELSEIF i1 == 3
                  IF iChoic == Len( arrh )
                     edi_Alert( "Not available" )
                     i1 := 0
                  ELSEIF ic == 1
                     cedi_RunConsoleApp( 'git diff ' + Substr( arrh[iChoic+1],3,7 ) + " " + ;
                        Substr( arrh[iChoic],3,7 ) + " " + hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ELSEIF ic == 2
                     cedi_RunConsoleApp( 'fossil diff --from ' + Left( arrh[iChoic],10 ) + " --to " + ;
                        Left( arrh[iChoic+1],10 ) + " " + hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ENDIF
               ELSEIF i1 == 4
                  IF ic == 1
                     cedi_RunConsoleApp( 'git diff ' + Substr( arrh[1],3,7 ) + " " + ;
                        Substr( arrh[iChoic],3,7 ) + " " + hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ELSEIF ic == 2
                     cedi_RunConsoleApp( 'fossil diff --from ' + Left( arrh[iChoic],10 ) + " --to " + ;
                        Left( arrh[1],10 ) + " " + hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ENDIF
               ELSEIF i1 == 5
                  IF ic == 1
                     cedi_RunConsoleApp( 'git diff ' + Substr( arrh[iChoic],3,7 ) + " " + ;
                        hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ELSEIF ic == 2
                     cedi_RunConsoleApp( 'fossil diff --from ' + Left( arrh[iChoic],10 ) + " " + ;
                        hb_FNameNameExt( oEdit:cFileName ), cFileRes )
                  ENDIF
               ENDIF
               IF i1 > 0 .AND. Empty( cBuff := MemoRead(cFileRes) )
                  edi_Alert( "No result" )
               ELSE
                  IF ( nPos := Ascan( oEdit:aWindows, {|o|o:cFileName==cAddW} ) ) > 0
                     o := oEdit:aWindows[nPos]
                     o:SetText( cBuff, cAddW )
                     mnu_ToBuf( oEdit, nPos )
                  ELSE
                     edi_AddWindow( oEdit, cBuff, cAddW, 3, Int(MaxCol()/2) )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   DevPos( nRow, nCol )

   RETURN Nil
