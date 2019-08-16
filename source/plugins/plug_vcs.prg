FUNCTION Plug_Vcs( oEdit )

   LOCAL aMenu := { "Git: History", "Fossil: History" }, iChoic, ic
   LOCAL aMenu1 := { "Show file", "Diff with next", "Diff with last", "Diff with current" }, i1, o
   LOCAL cCurrDir, cFileRes := hb_DirTemp() + "pluggit.out", cAddW1 := "$Vcs", cAddW2 := "$Diff", cBuff, arrh
   LOCAL cFileName := hb_FNameNameExt( oEdit:cFileName ), cv1, cv2
   LOCAL nRow := Row(), nCol := Col()

   cCurrDir := hb_FNameDir( oEdit:cFileName )
   IF ( ic := FMenu( oEdit, aMenu, 3, 10 ) ) == 1 .OR. ic == 2
      DirChange( cCurrDir )
      edi_Wait( Padc( "Wait...", 16 ), TEdit():cColorWR )
      IF ic == 1
         cedi_RunConsoleApp( 'git log --pretty=format:"%h %ad | %s%d [%an]" --graph --date=short ' + ;
            cFileName, cFileRes )
      ELSEIF ic == 2
         cedi_RunConsoleApp( 'fossil finfo -b ' + cFileName, cFileRes )
      ENDIF
      edi_Wait()

      IF Empty( cBuff := MemoRead(cFileRes) )
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

         edi_Wait( Padc( "Wait...", 16 ), TEdit():cColorWR )
         IF iChoic == 0
            IF ic == 1
               cedi_RunConsoleApp( 'git diff ' + cv1 + " " + cFileName, cFileRes )
            ELSEIF ic == 2
               cedi_RunConsoleApp( 'fossil diff --from ' + cv1 + " " + cFileName, cFileRes )
            ENDIF
         ELSE
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
            edi_CloseWindow( cAddW1 )
            IF i1 == 1
               edi_CloseWindow( cAddW2 )
               o := edi_AddWindow( oEdit, cBuff, cAddW1, 3, Int(MaxCol()/2) )
               o:lReadOnly := .T.
            ELSE
               edi_AddDiff( oEdit, cBuff, (iChoic == 0) )
            ENDIF
         ENDIF
      ENDIF
      DirChange( edi_CurrPath() )
   ENDIF
   DevPos( nRow, nCol )

   RETURN Nil
