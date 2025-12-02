#!/bin/sh
export HB_INS="/opt/harbour"
export SRC_DIR=source
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hbedit.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fedit.prg  -n -q0 -es2 -gc0 -d_FULL -I$HB_INS/include
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
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hbcommander.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fview.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hbextcli.prg  -n -q0 -es2 -gc0 -I$HB_INS/include

gcc -I. -I$HB_INS/include -Wall -c hbedit.c fedit.c fcmd.c fmenu.c fgetsys.c falert.c ffiles.c fkeymaps.c fdiff.c hilight.c fautoc.c errorsys.c hbcommander.c fview.c hbextcli.c $SRC_DIR/cfuncs.c $SRC_DIR/trie/trie.c $SRC_DIR/trie/hbtrie.c $SRC_DIR/sqlidyn/hbsqlit3.c $SRC_DIR/sqlidyn/sqlit3.c
gcc -Wall -ohbedit hbedit.o fedit.o fcmd.o fmenu.o fgetsys.o falert.o ffiles.o fdiff.o fkeymaps.o hilight.o fautoc.o errorsys.o hbcommander.o fview.o hbextcli.o cfuncs.o trie.o hbtrie.o hbsqlit3.o sqlit3.o -L $HB_INS/lib/linux/gcc  \
	  -Wl,--start-group -lhbdebug  -lhbvm  -lhbrtl  -lhblang  -lhbrdd  \
	  -lhbmacro -lhbpp -lhbcommon -lrddntx -lrddcdx -lrddfpt -lhbsix \
        -lhbct -lgttrm -lhbcpage -lhbnetio -lpcre -lhbcplr -lhbmzip -lminizip -lm -ldl -lz -lrt -Wl,--end-group
gcc $SRC_DIR/gtkclip.c -ogtkclip  `pkg-config --cflags gtk+-2.0` `pkg-config gtk+-2.0 --libs` -lrt
rm *.o
rm *.c