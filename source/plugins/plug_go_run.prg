STATIC cTerm

FUNCTION Plug_Go_Run( oEdit )

   LOCAL cTmpDir := hb_DirTemp(), cTmpGo := cTmpDir + "tmp_hbedit.go", cTmpScr
   LOCAL cFileRes, arr, i, cBuff

   hb_MemoWrit( cTmpGo, oEdit:ToString() )

   IF hb_version(20)
      cTmpScr := cTmpDir + "tmp_hbedit.sh"
      hb_MemoWrit( cTmpScr, "#!/bin/bash" + Chr(10) + ;
         "go run " + cTmpgo + Chr(10) + "echo ''" + Chr(10) + 'read -n 1 -p "Press any key"' )
      __Run( "chmod a+x " + cTmpScr )
      IF hb_gtVersion() == "HWGUI"
         IF Empty( cTerm )
            arr := { "gnome-terminal", "x-terminal-emulator", "konsole", "xfce4-terminal" }
            cFileRes := cTmpDir + "tmp_hbedit.out"
            FOR i := 1 TO Len( arr )
               cedi_RunConsoleApp( "which " + arr[i], cFileRes )
               IF !Empty( cBuff := MemoRead( cFileRes ) )
                  cBuff := StrTran( cBuff, Chr(10), "" )
                  IF Substr( cBuff, Len(cBuff)-Len(arr[i])+1, Len(arr[i]) ) == arr[i]
                     cTerm := arr[i]
                     EXIT
                  ENDIF
               ENDIF
            NEXT
         ENDIF
         IF Empty( cTerm )
            edi_Alert( "Terminal program not found" )
         ELSE
            __Run( cTerm + " -e " + cTmpScr )
         ENDIF
      ELSE
         CLEAR SCREEN
         Devpos( 0,0 )
         __Run( cTmpScr )
      ENDIF
   ELSE
      cTmpScr := cTmpDir + "tmp_hbedit.bat"
      hb_MemoWrit( cTmpScr, + Chr(13) + Chr(10) + ;
         "go run " + cTmpgo + Chr(13) + Chr(10) + "pause" )
      __Run( cTmpScr )
   ENDIF

   SetColor( oEdit:cColor )
   oEdit:WriteTopPane()
   oEdit:TextOut()

   edi_Alert( "Done!" )

   RETURN Nil
