#!/bin/sh
export HB_INS=/home/alkresin/apps/harbour
export SRC_DIR=../source

$HB_INS/bin/linux/gcc/harbour $SRC_DIR/fdiff.prg $SRC_DIR/fedit.prg $SRC_DIR/fmenu.prg $SRC_DIR/fview.prg $SRC_DIR/hbcommander.prg -j -n -q -i$HB_INS/include

$HB_INS/bin/linux/gcc/hbi18n -m -ohbedit_ru_1.pot fdiff.pot fedit.pot fmenu.pot fview.pot hbcommander.pot
$HB_INS/bin/linux/gcc/hbi18n -a -ohbedit_ru_1.pot hbedit_ru_1.pot hbedit_utf8.pot
mv hbedit_ru_1.pot hbedit_utf8.pot

rm *.c
rm fdiff.pot
rm fedit.pot
rm fmenu.pot
rm fview.pot
rm hbcommander.pot
