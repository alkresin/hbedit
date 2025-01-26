/*
 * HbCommander plugin, which is executed after pressing Space key
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

FUNCTION plug_hbc_ext_all( oEdit, cPath, aParams )

   LOCAL cFile, cExt, i

   IF Empty( aParams )
   ELSE
      cFile := aParams[2]
      cExt := Lower( hb_fnameExt( cFile ) )
      IF cExt $ ".bmp.jpg.gif.png.tif"
         hbc_RunPlugin( "gthwg_plug", cPath + "hbc_gthwg_q.hrb",, cFile, "dlg", .T. )
      ELSEIF cExt == ".fb2"
         IF ( i := Ascan2( FilePane():aPlugins, "plug_hbc_ext_fb2zip.hrb" ) ) > 0
            edi_RunPlugin( oEdit, FilePane():aPlugins, i, aParams )
         ENDIF
      ELSEIF cExt == ".hwprj" .OR. cExt == ".prg" .OR. cExt == ".c" .OR. cExt == ".cpp"
         hbc_RunPlugin( "hwbc_plug", cPath + "hwbuilder.hrb", aParams[2] )
      ENDIF
   ENDIF

   RETURN Nil