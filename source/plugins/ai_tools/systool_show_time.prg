FUNCTION systool_show_time()

   RETURN "<time>" + hb_dtoc( Date(), "yyyy-mm-dd" ) + " " + Left( Time(),5 ) + "</time>"