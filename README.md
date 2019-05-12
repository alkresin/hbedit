# hbedit
A fullscreen console multiplatform text editor.

### Project files

  + bld_edit.bat        - command file to build Hbedit for Windows (Borland C compiler).
  + bld_edit.sh         - shell script to build Hbedit for Linux.
  + bld_edit_full.bat   - command file to build full Hbedit for Windows (Borland C compiler),
                        its only difference is that the full version requests most of Harbour
                        functions to provide a possibility to use them in plugins.
  + bld_plugins.bat     - command file to build plugins.
  + bld_plugins.sh      - shell script to build plugins.
  + hbedit.hbp          - project file to build hbedit with hbmk2.
  + hbedit_full.hbp     - project file to build full hbedit with hbmk2.
  + hbedit.help         - Hbedit help file (Russian).
  + hbedit_en.help      - Hbedit help file (English).
  + hbedit.ini          - Hbedit ini file.

  + source/
    + hbfuncs.ch
    + hbfuncsfull.ch    - header files.

    + cfuncs.c
    + fcmd.prg
    + fedit.prg
    + ffiles.prg
    + fkeymaps.prg
    + fgetsys.prg
    + fmenu.prg
    + hilight.prg       - editor source files, which implements the TEdit class.
                        To include the TEdit in your application you need to link them all.

    + errorsys.prg
    + hbedit.prg        - a wrapper for TEdit class, which implements the editor.

  + source/plugins/     - plugins source files
    + plug_c_spis.prg       - C functions list
    + plug_go_fmt.prg       - Golang formatting
    + plug_go_run.prg       - Golang run code
    + plug_go_spis.prg      - Golang functions list
    + plug_hbp_init.prg     - a start plugin for .hbp files
    + plug_prg_compile.prg  - Harbour compiling
    + plug_prg_run.prg      - Harbour run
    + plug_prg_spis.prg     - Harbour functions list

### Usage

  The Hbedit may be used as a class, as a library to incorporate it to your application.
  You just need to compile and link the Tedit source files ( cfuncs.c, fcmd.prg, fedit.prg,
  ffiles.prg, fmenu.prg, hilight.prg ) and put following lines:

      oEdit := TEdit():New( Memoread("my.txt"), "my.txt" )
      oEdit:Edit()

  to edit, for example, a file "my.txt".

  Also, compiled and linked with hbedit.prg, it is a standalone editor.

### Hbedit command line parameters
  
  hbedit [-f iniFileName] [-gN] [-xy=xPos,yPos] [-size=nCols,nRows] [-ro] [files...]

  - -f iniFileName      - a name of ini file to use instead of hbedit.ini
  - -gN                 - goto line N; If N is negative it is a number of lines before the end
  - -xy=xPos,yPos       - initial window position in pixels (for Windows only)
  - [-size=nCols,nRows] - number of columns and rows in an editor window
  - [-ro]               - open file in a readonly mode
  - files...            - the list of files to edit


### hbedit.ini

 hbedit.ini includes many important options, you may edit it to tune the editor.

 One of them - codepages used. The list is in [CODEPAGES] section, you can include there those,
 which you need and delete others. Below is a list of possible values:

 BG866, BGISO, BGMIK, BGWIN, BIG5, CP950, CS852, CS852C, CSISO, CSKAMC,
 CSWIN, DE850, DE850M, DEISO, DEWIN, DK865, EL437, EL737, ELISO, ELWIN,
 EN, ES850, ES850C, ES850M, ESISO, ESMWIN, ESWIN, FI850, FR850, FR850C,
 FR850M, FRISO, FRWIN, GBK, HE862, HEWIN, HR646, HR852, HRISO, HRWIN,
 HU852, HU852C, HUISO, HUWIN, IS850, IS861, IT437, IT850, IT850M, ITISB,
 ITISO, ITWIN, LT775, LTWIN, NL850, NL850M, NO865, PL852, PLISO, PLMAZ,
 PLWIN, PT850, PT860, PTISO, RO852, ROISO, ROWIN, RU1251, RU866, RUISO,
 RUKOI8, SK852, SK852C, SKISO, SKKAMC, SKWIN, SL646, SL852, SLISO, SLWIN,
 SR646, SR646C, SRWIN, SV437C, SV850, SV850M, SVISO, SVWIN, TR857, TRISO,
 TRWIN, UA1125, UA1251, UA866, UAKOI8, UTF8.

### Download
   You may download some ready binaries from http://www.kresin.ru/en/hbedit.html
