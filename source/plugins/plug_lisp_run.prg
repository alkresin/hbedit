#define K_ENTER       13
#define K_LDBLCLK     1006

DYNAMIC LISP_EVAL

STATIC hrbHandle

Function plug_lisp_run( oEdit, cPath )

   LOCAL cHrb, cBuff, i, oNew
   LOCAL nRow, nCol
   LOCAL cLispRun := "lisp_run.hrb"

   IF Empty( hrbHandle )
      IF !File( cPath + cLispRun )
         edi_Alert( cLispRun + " not found" )
         RETURN Nil
      ENDIF

      hrbHandle := hb_hrbLoad( cPath + cLispRun )
      hb_hrbDo( hrbHandle )
   ENDIF

   nRow := Row(); nCol := Col()
   CLEAR SCREEN

   lisp_Eval( oEdit:aText )

   Inkey(0)
   DevPos( nRow, nCol )

   SetColor( oEdit:cColor )
   oEdit:WriteTopPane()
   oEdit:TextOut()

   RETURN Nil
