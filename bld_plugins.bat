@echo off
set HB_INSTALL_BIN=c:\harbour\bin

cd source\plugins
harbour plug_c_spis.prg -n -gh
harbour plug_go_spis.prg -n -gh
harbour plug_go_fmt.prg -n -gh
harbour plug_go_run.prg -n -gh
harbour plug_go_build.prg -n -gh
harbour plug_prg_compile.prg -n -gh
harbour plug_prg_run.prg -n -gh
harbour plug_prg_spis.prg -n -gh
harbour plug_hbp_init.prg -n -gh
cd ..\..\

