#!/bin/sh
export HB_INS="/opt/harbour"
export SRC_DIR=source
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hbedit.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fedit.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fcmd.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fmenu.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fgetsys.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/falert.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/ffiles.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fkeymaps.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fdiff.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/hilight.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour $SRC_DIR/errorsys.prg  -n -q0 -es2 -gc0 -I$HB_INS/include
gcc -I. -I$HB_INS/include -Wall -c hbedit.c fedit.c fcmd.c fmenu.c fgetsys.c falert.c ffiles.c fkeymaps.c fdiff.c hilight.c errorsys.c $SRC_DIR/cfuncs.c
gcc -Wall -ohbedit hbedit.o fedit.o fcmd.o fmenu.o fgetsys.o falert.o ffiles.o fkeymaps.o hilight.o errorsys.o cfuncs.o -L $HB_INS/lib/linux/gcc  \
	  -Wl,--start-group -lhbdebug  -lhbvm  -lhbrtl  -lhblang  -lhbrdd  \
	  -lhbmacro -lhbpp -lhbcommon -lrddntx -lrddcdx -lrddfpt -lhbsix \
        -lhbct -lgttrm -lhbcpage -lhbnetio -lpcre -lhbcplr -lm -lz -Wl,--end-group
rm *.o
rm *.c
