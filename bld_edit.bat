@echo off
set HB_INSTALL=c:\harbour\
set SRC_PATH=source
set HB_LIBS=hbdebug.lib hbrtl.lib gtwvt.lib gtgui.lib hbvm.lib hbpp.lib hbcommon.lib hbmacro.lib rddleto.lib hbrdd.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbct.lib hbcpage.lib hbpcre.lib hbcplr.lib hbzlib.lib hbmzip.lib minizip.lib hbwin.lib hbnetio.lib

%HB_INSTALL%\bin\harbour %SRC_PATH%\hbedit.prg /n /q /dGTWVT -I%HB_INSTALL%\include %1 2>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fedit.prg /n /q /d_FULL -I%HB_INSTALL%\include %1 2>>harbour.out
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
%HB_INSTALL%\bin\harbour %SRC_PATH%\hbcommander.prg /n /q /dGTWVT -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fview.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\hbextcli.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out

bcc32 -ehbedit.exe -O2 -tW -I%HB_INSTALL%\include -L%HB_INSTALL%\lib\win\bcc %HB_LIBS% ws2_32.lib iphlpapi.lib hbedit.c fedit.c fmenu.c fcmd.c fgetsys.c falert.c ffiles.c fkeymaps.c fdiff.c hilight.c fautoc.c errorsys.c hbcommander.c fview.c hbextcli.c %SRC_PATH%\cfuncs.c %SRC_PATH%\trie\trie.c %SRC_PATH%\trie\hbtrie.c
del *.obj
del *.c
del *.tds
