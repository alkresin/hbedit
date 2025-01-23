#!/bin/sh
export HB_INS=/opt/harbour
export SRC_DIR=source
export HWG_INS=$HB_INS/hwgui-code
export HWGUI_LIBS="-lhwgui -lhbxml -lprocmisc -lhwgdebug"
export HARBOUR_LIBS="-lhbdebug -lhbvm -lhbrtl -lgthwg -lgtcgi -lhblang -lhbrdd -lhbmacro -lhbpp -lhbcommon -lrddntx -lrddcdx -lrddfpt -lhbsix -lhbct -lhbcpage -lpcre -lhbcplr -lhbsqlit3 -lhbnetio -lhbmzip -lminizip"

$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hbedit.prg  -n -q0 -es2 -gc0 -d__GTHWG__ -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fedit.prg -n -q0 -es2 -gc0 -d_FULL -d_USE_SSH2 -d__GTHWG__ -I$HB_INS/include -I$HWG_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fcmd.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fmenu.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fgetsys.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/falert.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/ffiles.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fkeymaps.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fdiff.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hilight.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fautoc.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/errorsys.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hbcommander.prg  -n -q0 -es2 -gc0 -d__GTHWG__ -d_USE_SSH2 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fview.prg -n -q0 -es2 -gc0 -d_USE_SSH2 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hbextcli.prg -n -q0 -es2 -gc0 -d_USE_SSH2 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hbcvf.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/ssh2/hbcssh2.prg  -n -q0 -es2 -gc0 -I$HB_INS/include

gcc hbedit.c fedit.c fcmd.c fmenu.c fgetsys.c falert.c ffiles.c fkeymaps.c fdiff.c \
     hilight.c fautoc.c errorsys.c hbcommander.c fview.c hbextcli.c $SRC_DIR/cfuncs.c \
     $SRC_DIR/trie/trie.c $SRC_DIR/trie/hbtrie.c hbcvf.c hbcssh2.c $SRC_DIR/ssh2/hb_ssh2.c \
      -ohbedit_hwg -I. -I$HB_INS/include -L $HB_INS/lib/linux/gcc -L $HWG_INS/lib -D__GTHWG__ -D_USE_HB \
	  -Wl,--start-group $HWGUI_LIBS $HARBOUR_LIBS -Wl,--end-group \
      `pkg-config --cflags gtk+-2.0` `pkg-config gtk+-2.0 --libs` \
      -lm -lz -lrt -ldl -lsqlite3 -lpthread -lssh2 >bld.log 2>bld.log

rm *.c
