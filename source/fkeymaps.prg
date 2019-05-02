/*
 * Functions to convert special keys codes to string representation and vice versa/
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"

STATIC hKeysMap1 := { 0x41020051=>"Ctrl-Q", 0x41020057=>"Ctrl-W", 0x41020045=>"Ctrl-E", ;
   0x41020052=>"Ctrl-R", 0x41020054=>"Ctrl-T", 0x41020059=>"Ctrl-Y", 0x41020055=>"Ctrl-U", ;
   0x41020049=>"Ctrl-I", 0x4102004F=>"Ctrl-O", 0x41020050=>"Ctrl-P", 0x4102005B=>"Ctrl-[", ;
   0x4102005D=>"Ctrl-]", 0x41020041=>"Ctrl-A", 0x41020053=>"Ctrl-S", 0x41020044=>"Ctrl-D", ;
   0x41020046=>"Ctrl-F", 0x41020047=>"Ctrl-G", 0x41020048=>"Ctrl-H", 0x4102004A=>"Ctrl-J", ;
   0x4102004B=>"Ctrl-K", 0x4102004C=>"Ctrl-L", 0x4102005A=>"Ctrl-Z", 0x41020058=>"Ctrl-X", ;
   0x41020043=>"Ctrl-C", 0x41020056=>"Ctrl-V", 0x41020042=>"Ctrl-B", 0x4102004E=>"Ctrl-N", ;
   0x4102004D=>"Ctrl-M", ;
   0x41040051=>"Alt-Q", 0x41040057=>"Alt-W", 0x41040045=>"Alt-E", 0x41040052=>"Alt-R", ;
   0x41040054=>"Alt-T", 0x41040059=>"Alt-Y", 0x41040055=>"Alt-U", 0x41040049=>"Alt-I", ;
   0x4104004F=>"Alt-O", 0x41040050=>"Alt-P", 0x4104005B=>"Alt-[", 0x4104005D=>"Alt-]", ;
   0x41040041=>"Alt-A", 0x41040053=>"Alt-S", 0x41040044=>"Alt-D", 0x41040046=>"Alt-F", ;
   0x41040047=>"Alt-G", 0x41040048=>"Alt-H", 0x4104004A=>"Alt-J", 0x4104004B=>"Alt-K", ;
   0x4104004C=>"Alt-L", 0x4104003B=>"Alt-;", 0x41040027=>"Alt-'", 0x4104005A=>"Alt-Z", ;
   0x41040058=>"Alt-X", 0x41040043=>"Alt-C", 0x41040056=>"Alt-V", 0x41040042=>"Alt-B", ;
   0x4104004E=>"Alt-N", 0x4104004D=>"Alt-M", 0x4104002C=>"Alt-,", 0x4104002E=>"Alt-.", ;
   0x4104002F=>"Alt-/", 0x41040031=>"Alt-1", 0x41040032=>"Alt-2", 0x41040033=>"Alt-3", ;
   0x41040034=>"Alt-4", 0x41040035=>"Alt-5", 0x41040036=>"Alt-6", 0x41040037=>"Alt-7", ;
   0x41040038=>"Alt-8", 0x41040039=>"Alt-9", 0x41040030=>"Alt-0", 0x4104002D=>"Alt--", ;
   0x4104003D=>"Alt-=", 0x41040060=>"Alt-`", ;
   0x41020001=>"Ctrl-F1", 0x41020002=>"Ctrl-F2", 0x41020003=>"Ctrl-F3", 0x41020004=>"Ctrl-F4", ;
   0x41020005=>"Ctrl-F5", 0x41020006=>"Ctrl-F6", 0x41020007=>"Ctrl-F7", 0x41020008=>"Ctrl-F8", ;
   0x41020009=>"Ctrl-F9", 0x4102000A=>"Ctrl-F10", 0x4102000B=>"Ctrl-F11", 0x4102000C=>"Ctrl-F12", ;
   0x41040001=>"Alt-F1", 0x41040002=>"Alt-F2", 0x41040003=>"Alt-F3", 0x41040004=>"Alt-F4", ;
   0x41040005=>"Alt-F5", 0x41040006=>"Alt-F6", 0x41040007=>"Alt-F7", 0x41040008=>"Alt-F8", ;
   0x41040009=>"Alt-F9", 0x4104000A=>"Alt-F10", 0x4104000B=>"Alt-F11", 0x4104000C=>"Alt-F12", ;
   0x41010001=>"Shift-F1", 0x41010002=>"Shift-F2", 0x41010003=>"Shift-F3", 0x41010004=>"Shift-F4", ;
   0x41010005=>"Shift-F5", 0x41010006=>"Shift-F6", 0x41010007=>"Shift-F7", 0x41010008=>"Shift-F8", ;
   0x41010009=>"Shift-F9", 0x4101000A=>"Shift-F10", 0x4101000B=>"Shift-F11", 0x4101000C=>"Shift-F12", ;
   0x4102000D=>"Ctrl-Up", 0x4102000E=>"Ctrl-Down", 0x4102000F=>"Ctrl-Left", 0x41020010=>"Ctrl-Right", ;
   0x41020016=>"Ctrl-Del", 0x41020015=>"Ctrl-Ins", 0x41020012=>"Ctrl-End", 0x41020011=>"Ctrl-Home", ;
   0x41020014=>"Ctrl-PgDn", 0x41020013=>"Ctrl-PgUp", 0x41020018=>"Ctrl-Tab", 0x4102001A=>"Ctrl-Enter", ;
   0x41020017=>"Ctrl-Bs", 0x4102005C=>"Ctrl-\", 0x41020020=>"Ctrl-Space", ;
   0x41040017=>"Alt-Bs", 0x4104005C=>"Alt-\", ;
   0x41000001=>"F1", 0x41000002=>"F2", 0x41000003=>"F3", 0x41000004=>"F4", 0x41000005=>"F5", ;
   0x41000006=>"F6", 0x41000007=>"F7", 0x41000008=>"F8", 0x41000009=>"F9", 0x4100000A=>"F10", ;
   0x4100000B=>"F11", 0x4100000C=>"F12" }

STATIC hKeysMap2 := { "Ctrl-Q"=>0x41020051, "Ctrl-W"=>0x41020057, "Ctrl-E"=>0x41020045, ;
   "Ctrl-R"=>0x41020052, "Ctrl-T"=>0x41020054, "Ctrl-Y"=>0x41020059, "Ctrl-U"=>0x41020055, ;
   "Ctrl-I"=>0x41020049, "Ctrl-O"=>0x4102004F, "Ctrl-P"=>0x41020050, "Ctrl-["=>0x4102005B, ;
   "Ctrl-]"=>0x4102005D, "Ctrl-A"=>0x41020041, "Ctrl-S"=>0x41020053, "Ctrl-D"=>0x41020044, ;
   "Ctrl-F"=>0x41020046, "Ctrl-G"=>0x41020047, "Ctrl-H"=>0x41020048, "Ctrl-J"=>0x4102004A, ;
   "Ctrl-K"=>0x4102004B, "Ctrl-L"=>0x4102004C, "Ctrl-Z"=>0x4102005A, "Ctrl-X"=>0x41020058, ;
   "Ctrl-C"=>0x41020043, "Ctrl-V"=>0x41020056, "Ctrl-B"=>0x41020042, "Ctrl-N"=>0x4102004E, ;
   "Ctrl-M"=>0x4102004D, ;
   "Alt-Q"=>0x41040051, "Alt-W"=>0x41040057, "Alt-E"=>0x41040045, "Alt-R"=>0x41040052, ;
   "Alt-T"=>0x41040054, "Alt-Y"=>0x41040059, "Alt-U"=>0x41040055, "Alt-I"=>0x41040049, ;
   "Alt-O"=>0x4104004F, "Alt-P"=>0x41040050, "Alt-["=>0x4104005B, "Alt-]"=>0x4104005D, ;
   "Alt-A"=>0x41040041, "Alt-S"=>0x41040053, "Alt-D"=>0x41040044, "Alt-F"=>0x41040046, ;
   "Alt-G"=>0x41040047, "Alt-H"=>0x41040048, "Alt-J"=>0x4104004A, "Alt-K"=>0x4104004B, ;
   "Alt-L"=>0x4104004C, "Alt-;"=>0x4104003B, "Alt-'"=>0x41040027, "Alt-Z"=>0x4104005A, ;
   "Alt-X"=>0x41040058, "Alt-C"=>0x41040043, "Alt-V"=>0x41040056, "Alt-B"=>0x41040042, ;
   "Alt-N"=>0x4104004E, "Alt-M"=>0x4104004D, "Alt-,"=>0x4104002C, "Alt-."=>0x4104002E, ;
   "Alt-/"=>0x4104002F, "Alt-1"=>0x41040031, "Alt-2"=>0x41040032, "Alt-3"=>0x41040033, ;
   "Alt-4"=>0x41040034, "Alt-5"=>0x41040035, "Alt-6"=>0x41040036, "Alt-7"=>0x41040037, ;
   "Alt-8"=>0x41040038, "Alt-9"=>0x41040039, "Alt-0"=>0x41040030, "Alt--"=>0x4104002D, ;
   "Alt-="=>0x4104003D, "Alt-`"=>0x41040060, ;
   "Ctrl-F1"=>0x41020001, "Ctrl-F2"=>0x41020002, "Ctrl-F3"=>0x41020003, "Ctrl-F4"=>0x41020004, ;
   "Ctrl-F5"=>0x41020005, "Ctrl-F6"=>0x41020006, "Ctrl-F7"=>0x41020007, "Ctrl-F8"=>0x41020008, ;
   "Ctrl-F8"=>0x41020009, "Ctrl-F10"=>0x4102000A, "Ctrl-F11"=>0x4102000B, "Ctrl-F12"=>0x4102000C, ;
   "Alt-F1"=>0x41040001, "Alt-F2"=>0x41040002, "Alt-F3"=>0x41040003, "Alt-F4"=>0x41040004, ;
   "Alt-F5"=>0x41040005, "Alt-F6"=>0x41040006, "Alt-F7"=>0x41040007, "Alt-F8"=>0x41040008, ;
   "Alt-F9"=>0x41040009, "Alt-F10"=>0x4104000A, "Alt-F11"=>0x4104000B, "Alt-F12"=>0x4104000C, ;
   "Shift-F1"=>0x41010001, "Shift-F2"=>0x41010002, "Shift-F3"=>0x41010003, "Shift-F4"=>0x41010004, ;
   "Shift-F5"=>0x41010005, "Shift-F6"=>0x41010006, "Shift-F7"=>0x41010007, "Shift-F8"=>0x41010008, ;
   "Shift-F9"=>0x41010009, "Shift-F10"=>0x4101000A, "Shift-F11"=>0x4101000B, "Shift-F12"=>0x4101000C, ;
   "Ctrl-Up"=>0x4102000D, "Ctrl-Down"=>0x4102000E, "Ctrl-Left"=>0x4102000F, "Ctrl-Right"=>0x41020010, ;
   "Ctrl-Del"=>0x41020016, "Ctrl-Ins"=>0x41020015, "Ctrl-End"=>0x41020012, "Ctrl-Home"=>0x41020011, ;
   "Ctrl-PgDn"=>0x41020014, "Ctrl-PgUp"=>0x41020013, "Ctrl-Tab"=>0x41020018, "Ctrl-Enter"=>0x4102001A, ;
   "Ctrl-Bs"=>0x41020017, "Ctrl-\"=>0x4102005C, "Ctrl-Space"=>0x41020020, ;
   "Alt-Bs"=>0x41040017, "Alt-\"=>0x4104005C, ;
   "F1"=>0x41000001, "F2"=>0x41000002, "F3"=>0x41000003, "F4"=>0x41000004, "F5"=>0x41000005, ;
   "F6"=>0x41000006, "F7"=>0x41000007, "F8"=>0x41000008, "F9"=>0x41000009, "F10"=>0x4100000A, ;
   "F11"=>0x4100000B, "F12"=>0x4100000C }

FUNCTION edi_KeyNToC( nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt)

   IF nKey >= K_SPACE .AND. nKey <= 255
      RETURN Chr( nKey )

   ELSEIF nKey > 3000
      RETURN hb_utf8Chr( nKey )

   ELSE
      RETURN hb_hGetDef( hKeysMap1, nKey, Nil )

   ENDIF
   RETURN Nil

FUNCTION edi_KeyCToN( cKey )

   IF Len( cKey ) == 1
      RETURN Asc( cKey )

   ELSEIF Len( cKey ) <= 3 .AND. Lower(Left(cKey,1)) != "f"
      RETURN hb_utf8Asc( cKey )

   ELSE
      hb_hCaseMatch( hKeysMap2, .F. )
      RETURN hb_hGetDef( hKeysMap2, cKey, Nil )

   ENDIF
   RETURN Nil
