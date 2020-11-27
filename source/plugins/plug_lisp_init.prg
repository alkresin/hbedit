#define ALT_PRESSED   0x040000
#define K_ALT_R        275
#define K_ENTER         13
#define K_LDBLCLK     1006

DYNAMIC LISP_EVAL

STATIC cIniPath
STATIC hrbHandle

FUNCTION Plug_lisp_Init( oEdit, cPath )

   LOCAL bOnKeyOrig
   LOCAL bStartEdit := {|o|
      LOCAL y := o:y1 - 1, nRow := Row(), nCol := Col()
      IF o:lTopPane
         SetColor( o:cColorPane )
         Scroll( y, o:x1 + 8, y, o:x2 )
         DevPos( y, o:x1 + 8 )
         DevOut( "Lisp plugin:  Alt-R Run" )
         SetColor( o:cColor )
         DevPos( nRow, nCol )
         oEdit:oHili:hHili["help"] := "Alt-R  launches the interpreter, based on Lisp 1.5." + Chr(10) + ;
            "Following functions are implemented:" + Chr(10) + ;
            "Basic: quote atom eq car cdr (and all cxyr like) cons cond lambda label defun." + Chr(10) + ;
            "Additional: if null and not or append list pair length load numberp set setq" + Chr(10) + ;
            "Arithmetic: + - * / % ** ++ -- > < = max min" + Chr(10) + ;
            "and a new one: defunhb, which allows to use Harbour functions in a Lisp code."
      ENDIF
      o:bStartEdit := Nil

      RETURN Nil
   }
   LOCAL bOnKey := {|o,n|
      LOCAL nRes := _lisp_Init_OnKey(o,n)
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

FUNCTION _lisp_Init_OnKey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), nCol := Col(), nRow := Row(), cWord

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_R
         _lisp_init_run( oEdit, cIniPath )
         RETURN -1
      ENDIF
   ENDIF

   RETURN 0

FUNCTION _lisp_init_run( oEdit, cPath )

   LOCAL cHrb, cBuff, i, oNew
   LOCAL nRow, nCol
   LOCAL cLispRun := "lisp_run.hrb", cFile := "$hb_lisp_result"

   IF Empty( hrbHandle )
      IF !File( cPath + cLispRun )
         edi_Alert( cLispRun + " not found" )
         RETURN Nil
      ENDIF

      hrbHandle := hb_hrbLoad( cPath + cLispRun )
      hb_hrbDo( hrbHandle )
   ENDIF

   edi_CloseWindow( cFile )

   nRow := Row(); nCol := Col()
   CLEAR SCREEN

   cBuff := lisp_Eval( oEdit:aText )

   IF Empty( i := edi_Alert( "Evaluation finished!", "Close", "Show", "Open in window" ) ) .OR. i == 2
      Inkey(0)
   ENDIF

   DevPos( nRow, nCol )
   SetColor( oEdit:cColor )
   oEdit:WriteTopPane()
   oEdit:TextOut()

   IF i == 3
      oNew := edi_AddWindow( oEdit, cBuff, cFile, 2, 7 )
      oNew:lReadOnly := .T.
   ENDIF

   RETURN Nil
