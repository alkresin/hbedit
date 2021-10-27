#!/bin/sh
export HB_INS="/opt/harbour"

cd source/plugins
$HB_INS/bin/linux/gcc/harbour plug_1c_spis.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_c_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_c_spis.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_spis.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_fmt.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_run.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_go_build.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_php_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_prg_compile.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_prg_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_prg_run.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_py_spis.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbp_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_java_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_lisp_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_selection.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_chartable.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_calculator.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_palette.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_tetris.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_sokoban.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_strek.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_life.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_webservices.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_vcs.prg -n -gh
$HB_INS/bin/linux/gcc/harbour lisp_run.prg -n -gh
cd ../../

