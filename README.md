# hbedit
A fullscreen console multiplatform text editor.

### Project files

  + bld_edit.bat        - command file to build Hbedit for Windows (Borland C compiler).
  + bld_edit.sh         - shell script to build Hbedit for Linux.
  + bld_edit_full.bat   - command file to build full Hbedit for Windows (Borland C compiler),
                        its only difference is that the full version requests most of Harbour
                        functions to provide a possibility to use them in plugins.
  + bld_plugins.bat     - command file to build plugins.
  + hbedit.help         - Hbedit help file.
  + hbedit.ini          - Hbedit ini file.

  + source/
    + hbfuncs.ch
    + hbfuncsfull.ch    - header files.

    + cfuncs.c
    + fcmd.prg
    + fedit.prg
    + ffiles.prg
    + fgetsys.prg
    + fmenu.prg
    + hilight.prg       - editor source files, which implements the TEdit class.
                        To include the TEdit in your application you need to link them all.

    + errorsys.prg
    + hbedit.prg        - a wrapper for TEdit class, which implements the editor.

  + source/plugins/
    + plug_go_spis.prg
    + plug_prg_compile.prg
    + plug_prg_run.prg
    + plug_prg_spis.prg - plugins source files

### Usage

  The Hbedit may be used as a class, as a library to incorporate it to your application.
  You just need to compile and link the Tedit source files ( cfuncs.c, fcmd.prg, fedit.prg,
  ffiles.prg, fmenu.prg, hilight.prg ) and put following lines:

      oEdit := TEdit():New( Memoread("my.txt"), "my.txt" )
      oEdit:Edit()

  to edit, for example, a file "my.txt".

  Also, compiled and linked with hbedit.prg, it is a standalone editor.

### Hbedit command line parameters
  
  hbedit [-f iniFileName] [-gN] [-xy=xPos,yPos] [-size=nCols,nRows] [files...]

  - -f iniFileName      - a name of ini file to use instead of hbedit.ini
  - -gN                 - goto line N; If N is negative it is a number of lines before the end
  - -xy=xPos,yPos       - initial window position in pixels (for Windows only)
  - [-size=nCols,nRows] - number of columns and rows in an editor window
  - files...            - the list of files to edit
