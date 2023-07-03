
FUNCTION plug_hbc_ext_fb2( oEdit, cPath, aParams )

   LOCAL i

   IF ( i := Ascan2( FilePane():aPlugins, "plug_hbc_ext_fb2zip.hrb" ) ) > 0
      edi_RunPlugin( oEdit, FilePane():aPlugins, i, aParams )
   ENDIF
   RETURN Nil
