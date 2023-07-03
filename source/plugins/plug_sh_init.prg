#define ALT_PRESSED   0x040000
#define K_ALT_R        275

STATIC cIniPath

FUNCTION Plug_sh_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "Bash plugin:  Alt-R Run" )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         IF oEdit:hCargo == Nil
            oEdit:hCargo := hb_hash()
         ENDIF
         oEdit:hCargo["help"] := "Alt-R  launches the bash file"
      ENDIF
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _sh_Init_OnKey(o,n)
      IF bOnKeyOrig != Nil .AND. nRes >= 0
         nRes := Eval( bOnKeyOrig, o, Iif( nRes==0, n, nRes ) )
      ENDIF
      RETURN nRes
   }

   cIniPath := cPath
   oEdit:bStartEdit := bStartEdit
   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey

   RETURN Nil

FUNCTION _sh_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cWord

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_R
         _sh_init_run( oEdit, cIniPath )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

FUNCTION _sh_init_run( oEdit, cPath )

   LOCAL cBuff, oNew, cDir := Curdir()
   LOCAL cFileRes := hb_DirTemp() + "hb_sh_result"

   edi_CloseWindow( cFileRes )

   DirChange( hb_fnameDir( oEdit:cFileName ) )
   cedi_RunConsoleApp( oEdit:cFileName + " >" + cFileRes + " 2>" + cFileRes )
   DirChange( cDir )

   IF Empty( cBuff := MemoRead( cFileRes ) )
      edi_Alert( "Done!" )
   ELSE
      oNew := edi_AddWindow( oEdit, cBuff, cFileRes, 2, 7 )
      oNew:lReadOnly := .T.
   ENDIF

   RETURN Nil
