
FUNCTION Plug_go_Init( oEdit, cPath )

   oEdit:bAutoC := {|o,s| _go_AutoC(o,s)}

   RETURN Nil

STATIC FUNCTION _go_AutoC( oEdit, cPrefix )

   LOCAL hTrieLang, o := oEdit:oHili
   LOCAL arr, i, nPos

   IF Empty( hb_hGetDef( o:hHili, "htrie", Nil ) )
      arr := hb_ATokens( Iif(Empty(o:cKeywords1),"",o:cKeywords1) + " " + ;
         Iif(Empty(o:cKeywords2),"",o:cKeywords2) + " " + Iif(Empty(o:cKeywords3),"",o:cKeywords3) + ;
         " " + Iif(Empty(o:cKeywords4),"",o:cKeywords4), " " )
      hTrieLang := o:hHili["htrie"] := trie_Create( .T. )
      FOR i := 1 TO Len( arr )
         IF Len( arr[i] ) > 3
            trie_Add( hTrieLang, arr[i] )
         ENDIF
      NEXT

   ENDIF

   RETURN Nil
