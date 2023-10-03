# hbedit
A fullscreen console multiplatform text editor with built-in file manager

** Attention! Since October 6, 2023 we are forced to use two-factor authentication to be able to
   update the repository. Because it's not suitable for me, I will probably use another place for projects.
   Maybe, https://gitflic.ru/, maybe, Sourceforge... Follow the news on my website, http://www.kresin.ru/

   Внимание! С 6 октября 2023 года нас вынуждают использовать двухфакторную идентификацию для того, чтобы 
   продолжать работать над проектами. Поскольку для меня это крайне неудобно, я, возможно, переведу проекты
   на другое место. Это может быть https://gitflic.ru/, Sourceforge, или что-то еще. Следите за новостями
   на моем сайте http://www.kresin.ru/ **

### Project files

  + bld_edit.bat        - command file to build Hbedit for Windows (Borland C compiler).
  + bld_edit.sh         - shell script to build Hbedit for Linux.
  + bld_edit_hwg.sh     - shell script to build Hbedit for Linux with GTHWG driver
  + bld_edit_hwg_ssh.sh - shell script to build Hbedit for Linux with GTHWG driver and ssh2 support
  + bld_gcc_ssh.bat     - command file to build full Hbedit for Windows (Mingw C compiler) with ssh2 support
  + bld_gcc_hwg_ssh.bat - command file to build full Hbedit for Windows (Mingw C compiler) with ssh2 support
                          and GTHWG driver (HwGUI support)
  + bld_plugins.bat     - command file to build plugins.
  + bld_plugins.sh      - shell script to build plugins.
  + hbedit.hbp          - project file to build hbedit with hbmk2.
  + hbedit_full.hbp     - project file to build full hbedit with hbmk2.
  + hbc.help            - HbCommander (built-in file manager) help file.
  + hbedit.help         - Hbedit help file (Russian).
  + hbedit_en.help      - Hbedit help file (English).
  + hbc.ini             - HbCommander (built-in file manager) ini file.
  + hbedit.ini          - Hbedit ini file.

  + source/
    + hbfuncsfull.ch    - header file.

    + cfuncs.c
    + errorsys.prg
    + falert.prg
    + fautoc.prg
    + fcmd.prg
    + fedit.prg
    + fdiff.prg
    + ffiles.prg
    + fgetsys.prg
    + fkeymaps.prg
    + fmenu.prg
    + hilight.prg       - editor source files, which implements the TEdit class.
                        To include the TEdit in your application you need to link them all.
    + hbcommander.prg
    + hbcvf.prg
    + fview.prg         - built-in file manager source files

    + hbedit.prg        - a wrapper for TEdit class, which implements the editor.

  + source/trie/*       - a subsystem, implementing prefix tree subsystem
  + source/ssh2/*       - a subsystem, implementing libssh2 support
  + source/plugins/     - plugins source files
    + hb_funcs.txt          - Harbour functions list for plug_prg_init
    + hwg_funcs.txt         - HwGUI functions list for plug_prg_init
    + plug_android_project.prg - creating and maintaining java android projects
    + plug_1c_spis.prg      - 1C functions list
    + plug_bat_init.prg     - a start plugin for .bat files
    + plug_c_init.prg       - a start plugin for .c files
    + plug_calculator.prg   - Calculator
    + plug_chartable.prg    - Chartable
    + plug_gm_chess.prg     - A Chess game
    + plug_gm_life.prg      - A Life game
    + plug_gm_sokoban.prg   - A Sokoban game
    + plug_gm_strek.prg     - A Star Trek game
    + plug_gm_sudoku.prg    - A Sudoku game
    + plug_gm_tetris.prg    - A Tetris game
    + plug_go_build.prg     - Golang build project
    + plug_go_fmt.prg       - Golang formatting
    + plug_go_init.prg      - a start plugin for .go files
    + plug_go_run.prg       - Golang run code
    + plug_go_spis.prg      - Golang functions list
    + plug_hbc_ext_fb2.prg    - file commander plugin, simple fb2 reader (.fb2)
    + plug_hbc_ext_fb2zip.prg - file commander plugin, simple fb2 reader (.fb2.zip)
    + plug_hbc_fb2_quick.prg  - file commander plugin, fb2 quick view
    + plug_hbc_files_compare.prg   - file commander plugin, compares files
    + plug_hbc_folders_compare.prg - file commander plugin, compares folders
    + plug_hbc_ftp.prg      - file commander plugin for ftp access
    + plug_hbc_latin.prg    - file commander plugin, converts file names
    + plug_hbc_menu.prg     - a plugin, which complements file commander context menu
    + plug_hbp_init.prg     - a start plugin for .hbp files
    + plug_java_init.prg    - a start plugin for .java files
    + plug_lisp_init.prg    - a start plugin for .lisp files
    + plug_palette.prg      - a current palette viewer
    + plug_php_init.prg     - a start plugin for .php files
    + plug_prg_compile.prg  - Harbour compiling
    + plug_prg_init.prg     - a start plugin for .prg files
    + plug_py_spis.prg      - Python functions list
    + plug_prg_run.prg      - Harbour run
    + plug_prg_run1c.prg    - Harbour for 1c
    + plug_selection.prg    - Additional operations on selected region
    + plug_sh_init.prg      - a start plugin for .sh files
    + plug_vcs.prg          - a plugin to work with Git and Fossil
    + plug_webservices.prg  - Some web services access

### Usage

  The Hbedit may be used as a class, as a library to incorporate it to your application.
  You just need to compile with -d__BUILT_IN and link the Tedit source files,
  except hbedit.prg, hbcommander.prg, fview.prg, and put following lines:

      oEdit := TEdit():New( Memoread("my.txt"), "my.txt" )
      oEdit:Edit()

  to edit, for example, a file "my.txt".

  Also, compiled and linked with hbedit.prg, it is a standalone editor.

### Hbedit command line parameters

  hbedit [-f iniFileName] [-gN] [-xy=xPos,yPos] [-size=nCols,nRows] [-ro] [files...]

  - -f iniFileName      - a name of ini file to use instead of hbedit.ini
  - -m                  - start in a file manager mode
  - -gN                 - goto line N; If N is negative it is a number of lines before the end
  - -xy=xPos,yPos       - initial window position in pixels (for Windows only)
  - -size=nCols,nRows   - number of columns and rows in an editor window
  - -ro                 - open file in a readonly mode
  - -d                  - open two files side-by-side in diff mode
  - -cp=CP              - sets CP codepage as default
  - -pal=paletteName    - sets paletteName as default palette
  - -his=N              - overrides 'savehis' option
  - files...            - the list of files to edit


### hbedit.ini

 hbedit.ini includes many important options, you may edit it to tune the editor.
 See detailed description at http://www.kresin.ru/en/hbedit.html

### Download
   You may download some ready binaries from http://www.kresin.ru/en/hbedit.html
