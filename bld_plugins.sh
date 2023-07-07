#!/bin/sh
export HB_INS="/opt/harbour"

cd source/plugins
$HB_INS/bin/linux/gcc/harbour plug_1c_spis.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_android_project.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_bat_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_c_init.prg -n -gh
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
$HB_INS/bin/linux/gcc/harbour plug_sh_init.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_selection.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_chartable.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_calculator.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_palette.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_tetris.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_sokoban.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_strek.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_sudoku.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_life.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_chess.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_gm_ugolki.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_webservices.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_vcs.prg -n -gh
$HB_INS/bin/linux/gcc/harbour lisp_run.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbc_ext_fb2.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbc_ext_fb2zip.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbc_fb2_quick.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbc_files_compare.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbc_folders_compare.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbc_ftp.prg -n -gh -I$HB_INS/include
$HB_INS/bin/linux/gcc/harbour plug_hbc_img_quick.prg -n -gh -q
$HB_INS/bin/linux/gcc/harbour hbc_gthwg_q.hrb -n -gh -q
$HB_INS/bin/linux/gcc/harbour plug_hbc_latin.prg -n -gh
$HB_INS/bin/linux/gcc/harbour plug_hbc_menu.prg -n -gh
cd ../../

