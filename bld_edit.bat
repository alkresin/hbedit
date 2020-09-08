@echo off
set HB_INSTALL=c:\harbour\
set SRC_PATH=source

%HB_INSTALL%\bin\harbour %SRC_PATH%\hbedit.prg /n /q /dGTWVT -I%HB_INSTALL%\include %1 2>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fedit.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fmenu.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fcmd.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fgetsys.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\falert.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\ffiles.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fkeymaps.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fdiff.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\hilight.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fautoc.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\errorsys.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
bcc32 -ehbedit.exe -O2 -tW -I%HB_INSTALL%\include -L%HB_INSTALL%\lib\win\bcc hbdebug.lib hbrtl.lib gtwvt.lib gtgui.lib hbvm.lib hbpp.lib hbcommon.lib hbmacro.lib hbrdd.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbct.lib hbcpage.lib hbpcre.lib hbcplr.lib ws2_32.lib hbedit.c fedit.c fmenu.c fcmd.c fgetsys.c falert.c ffiles.c fkeymaps.c fdiff.c hilight.c fautoc.c errorsys.c %SRC_PATH%\cfuncs.c
del *.obj
del *.c
del *.tds
