FUNCTION systool_add_tools( oService, arr )

   IF Valtype( arr ) == "A"
      IF !Empty(arr)
         RETURN oService:AddTools( arr )
      ELSE
         RETURN oService:AddTools()
      ENDIF
   ENDIF

   RETURN ""