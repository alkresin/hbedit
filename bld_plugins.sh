#!/bin/sh
export HB_INS="/opt/harbour"

cd source/plugins
$HB_INS/bin/linux/gcc/harbour plug_c_spis.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_spis.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_fmt.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_run.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_build.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_prg_compile.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_prg_run.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_prg_spis.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbp_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_chartable.prg -n -gh
cd ../../

