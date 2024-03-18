@echo off
set HB_INSTALL=c:\harbour

cd source\plugins
harbour plug_1c_spis.prg -n -gh -q
harbour plug_android_project.prg -n -gh -q
harbour plug_c_init.prg -n -gh -q
harbour plug_bat_init.prg -n -gh -q
harbour plug_go_init.prg -n -gh -q
harbour plug_go_spis.prg -n -gh -q
harbour plug_go_fmt.prg -n -gh -q
harbour plug_go_run.prg -n -gh -q
harbour plug_go_build.prg -n -gh -q
harbour plug_php_init.prg -n -gh -q
harbour plug_prg_compile.prg -n -gh -q
harbour plug_prg_init.prg -n -gh -q
harbour plug_prg_run.prg -n -gh -q
harbour plug_py_spis.prg -n -gh -q
harbour plug_hbp_init.prg -n -gh -q
harbour plug_hwprj_init.prg -n -gh -q
harbour plug_java_init.prg -n -gh -q
harbour plug_lisp_init.prg -n -gh -q
harbour plug_sh_init.prg -n -gh -q
harbour plug_selection.prg -n -gh -q
harbour plug_chartable.prg -n -gh -q
harbour plug_calculator.prg -n -gh -q
harbour plug_palette.prg -n -gh -q
harbour plug_gm_tetris.prg -n -gh -q
harbour plug_gm_sokoban.prg -n -gh -q
harbour plug_gm_strek.prg -n -gh -q
harbour plug_gm_sudoku.prg -n -gh -q
harbour plug_gm_life.prg -n -gh -q
harbour plug_gm_chess.prg -n -gh -q
harbour plug_gm_ugolki.prg -n -gh -q
harbour plug_webservices.prg -n -gh -q
harbour plug_vcs.prg -n -gh -q
harbour lisp_run.prg -n -gh -q
harbour plug_hbc_ext_all.prg -n -gh -q
harbour plug_hbc_ext_fb2zip.prg -n -gh -q
harbour plug_hbc_fb2_quick.prg -n -gh -q
harbour plug_hbc_files_compare.prg -n -gh -q
harbour plug_hbc_folders_compare.prg -n -gh -q
harbour plug_hbc_ftp.prg -n -gh -q -I%HB_INSTALL%\include
harbour plug_hbc_img_quick.prg -n -gh -q
harbour hbc_gthwg_q.prg -n -gh -q
harbour hwbuilder.prg -n -gh -q -I%HB_INSTALL%\include
harbour hbextcli.prg -n -gh -q -I%HB_INSTALL%\include
harbour plug_hbc_latin.prg -n -gh -q
harbour plug_hbc_menu.prg -n -gh -q
cd ..\..\

