/*
 *
 */

#xtranslate inkey([<n,...>])    => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:In(<n>) )
#xtranslate mrow([<n,...>])      => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:MY(<n>) )
#xtranslate mcol([<n,...>])      => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:MX(<n>) )
#xtranslate row([<n,...>])      => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Y(<n>) )
#xtranslate col([<n,...>])      => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:X(<n>) )
#xtranslate maxrow([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:MaxY(<n>) )
#xtranslate maxcol([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:MaxX(<n>) )
#xtranslate setpos([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Pos(<n>) )
#xtranslate devpos([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Pos(<n>) )
#xtranslate devout([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Out(<n>) )
#xtranslate setcolor([<n,...>]) => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Color(<n>) )
#xtranslate qout([<n,...>])     => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Q_Out(.T.,<n>) )
#xtranslate qqout([<n,...>])    => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Q_Out(.F.,<n>) )
#xtranslate scroll([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Scro(<n>) )
#xtranslate dispbox([<n,...>])  => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Box(<n>) )
#xtranslate __keyboard([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:SendKey(<n>) )
#xtranslate savescreen([<n,...>])  => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:SaveScr(<n>) )
#xtranslate restscreen([<n,...>])  => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:RestScr(<n>) )
#xtranslate __accept([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Getline(<n>) )
#xtranslate hb_gtinfo([<n,...>])   => Iif( Empty(HPseudoGT():oCurrent), HPseudoGT():ErrMsg(),HPseudoGT():oCurrent:Info(<n>) )
