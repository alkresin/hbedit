FUNCTION Plug_Go_Run( oEdit )

   hb_MemoWrit( "tmp_hbedit.go", oEdit:ToString() )

   IF hb_version(20)
      hb_MemoWrit( "tmp_hbedit.sh", "#!/bin/sh" + Chr(10) + ;
         "go run tmp_hbedit.go" + Chr(10) + 'read -n 1 -p "Press any key"' )
      __Run( "./tmp_hbedit.sh" )
   ELSE
      hb_MemoWrit( "tmp_hbedit.bat", + Chr(13) + Chr(10) + ;
         "go run tmp_hbedit.go" + Chr(13) + Chr(10) + "pause" )
      __Run( "tmp_hbedit.bat" )
   ENDIF

   edi_Alert( "Done!" )

   RETURN Nil