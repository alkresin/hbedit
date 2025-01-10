/*
 * HbCommander plugin, which is executed after pressing Space key
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

FUNCTION plug_hbc_ext_all( oEdit, cPath, aParams )

   LOCAL cFile, cExt, i
   LOCAL cExtImg := ".bmp.jpg.gif.png.tif", cGthwgHrb := "hbc_gthwg_q.hrb"

   IF Empty( aParams )
   ELSE
      cFile := aParams[2]
      cExt := Lower( hb_fnameExt( cFile ) )
      IF cExt $ cExtImg
         /*
         IF !hb_hHaskey( FilePane():hMisc,"gthwg_plug" )
            FilePane():hMisc["gthwg_plug"] := Iif( File( cPath + cGthwgHrb ), ;
               hb_hrbLoad( cPath + cGthwgHrb ), Nil )
         ENDIF
         IF !Empty( FilePane():hMisc["gthwg_plug"] )
            hb_hrbDo( FilePane():hMisc["gthwg_plug"],, cFile, "dlg", .T. )
         ENDIF
         */
         hbc_RunPlugin( "gthwg_plug", cPath + cGthwgHrb,, cFile, "dlg", .T. )
      ELSEIF cExt == ".fb2"
         IF ( i := Ascan2( FilePane():aPlugins, "plug_hbc_ext_fb2zip.hrb" ) ) > 0
            edi_RunPlugin( oEdit, FilePane():aPlugins, i, aParams )
         ENDIF
      ELSEIF cExt == ".hwprj" .OR. cExt == ".prg" .OR. cExt == ".c" .OR. cExt == ".cpp"
         IF !hb_isFunction( "HWBUILDER" ) .AND. File( cPath + "hwbuilder.hrb" )
            FilePane():hMisc["hwbc_plug"] := hb_hrbLoad( cPath + "hwbuilder.hrb" )
         ENDIF
         IF hb_isFunction( "HWBUILDER" )
            Eval( &( '{||HWBC_RUN("' + aParams[2] + '")}' ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN Nil