FUNCTION Plug_Go_Run( oEdit )

   hb_MemoWrit( "tmp_hbedit.go", oEdit:ToString() )

   IF hb_version(20)
      hb_MemoWrit( "tmp_hbedit.sh", "#!/bin/bash" + Chr(10) + ;
         "go run tmp_hbedit.go" + Chr(10) + "echo ''" + Chr(10) + 'read -n 1 -p "Press any key"' )
      __Run( "chmod a+x tmp_hbedit.sh" )
      IF hb_gtVersion() == "HWGUI"
      ELSE
         CLEAR SCREEN
         Devpos( 0,0 )
         __Run( "./tmp_hbedit.sh" )
      ENDIF
   ELSE
      hb_MemoWrit( "tmp_hbedit.bat", + Chr(13) + Chr(10) + ;
         "go run tmp_hbedit.go" + Chr(13) + Chr(10) + "pause" )
      __Run( "tmp_hbedit.bat" )
   ENDIF

   SetColor( oEdit:cColor )
   oEdit:WriteTopPane()
   oEdit:TextOut()

   edi_Alert( "Done!" )

   RETURN Nil
