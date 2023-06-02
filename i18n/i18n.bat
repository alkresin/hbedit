@set HB_INSTALL=c:\harbour\
@set SRC_DIR=..\source

%HB_INSTALL%\bin\harbour %SRC_DIR%\fdiff.prg %SRC_DIR%\fedit.prg %SRC_DIR%\fmenu.prg %SRC_DIR%\fview.prg %SRC_DIR%\hbcommander.prg -j -n -q -ic:\harbour\include

%HB_INSTALL%\bin\hbi18n.exe -m -ohbedit_ru_1.pot fdiff.pot fedit.pot fmenu.pot fview.pot hbcommander.pot
%HB_INSTALL%\bin\hbi18n.exe -a -ohbedit_ru_1.pot hbedit_ru_1.pot hbedit_ru866.pot
mv hbedit_ru_1.pot hbedit_ru866.pot

@del *.c
@del fdiff.pot
@del fedit.pot
@del fmenu.pot
@del fview.pot
@del hbcommander.pot
