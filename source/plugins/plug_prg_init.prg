#define CTRL_PRESSED  0x020000
#define K_CTRL_H      8
#define K_CTRL_I      9

FUNCTION Plug_prg_Init( oEdit )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1

      SetColor( o:cColorPane )
      Scroll( y, o:x1 + 8, y, o:x2 )
      DevPos( y, o:x1 + 8 )
      DevOut( "Harbour plugin:  Ctrl-H Help  Ctrl-I Info" )
      SetColor( o:cColor )
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _prg_Init_OnKey(o,n)
      IF bOnKeyOrig != Nil .AND. nRes >= 0
         nRes := Eval( bOnKeyOrig, o, Iif( nRes==0, n, nRes ) )
      ENDIF
      RETURN nRes
   }

   oEdit:bStartEdit := bStartEdit
   IF !Empty( oEdit:bOnKey )
      bOnKeyOrig := oEdit:bOnKey
   ENDIF
   oEdit:bOnKey := bOnKey

   RETURN Nil

FUNCTION _prg_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol, nRow

   IF hb_BitAnd( nKeyExt, CTRL_PRESSED ) != 0
      IF nKey == K_CTRL_H
         edi_Alert( "Help" )
         RETURN -1
      ELSEIF nKey == K_CTRL_I
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0
