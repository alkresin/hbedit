@echo off
set HB_INSTALL_BIN=c:\harbour\bin

cd source\plugins
harbour plug_1c_spis.prg -n -gh
harbour plug_c_init.prg -n -gh
harbour plug_c_spis.prg -n -gh
harbour plug_go_init.prg -n -gh
harbour plug_go_spis.prg -n -gh
harbour plug_go_fmt.prg -n -gh
harbour plug_go_run.prg -n -gh
harbour plug_go_build.prg -n -gh
harbour plug_prg_compile.prg -n -gh
harbour plug_prg_init.prg -n -gh
harbour plug_prg_run.prg -n -gh
harbour plug_prg_spis.prg -n -gh
harbour plug_py_spis.prg -n -gh
harbour plug_hbp_init.prg -n -gh
harbour plug_lisp_init.prg -n -gh
harbour plug_selection.prg -n -gh
harbour plug_chartable.prg -n -gh
harbour plug_calculator.prg -n -gh
harbour plug_palette.prg -n -gh
harbour plug_gm_tetris.prg -n -gh
harbour plug_gm_sokoban.prg -n -gh
harbour plug_gm_strek.prg -n -gh
harbour plug_gm_life.prg -n -gh
harbour plug_webservices.prg -n -gh
harbour plug_vcs.prg -n -gh
harbour lisp_run.prg -n -gh
cd ..\..\

