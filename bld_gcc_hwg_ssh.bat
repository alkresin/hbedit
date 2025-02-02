@echo off
rem Uncomment two following lines to create libssh.a from a libssh2.dll for the first time
rem gendef libssh2.dll 2>dummy
rem dlltool -v --dllname libssh2.dll --def libssh2.def --output-lib libssh2.a 2>dummy
set HB_INSTALL=c:\harbour
set HWGUI_INSTALL=c:\papps\hwgui_uni\
set SRC_PATH=source
set HB_LIBS=-lhbvm -lhbrdd -lhbmacro -lhbrtl -lhbcpage -lhblang -lhbcommon -lrddntx  -lrddcdx -lrddfpt -lhbsix -lgtgui -lgthwg -lhbpcre -lhbcplr -lhbct -lhbzlib -lhbmzip -lminizip -lhbwin -lhbnetio -lhwgui -lprocmisc -lhbxml

%HB_INSTALL%\bin\harbour %SRC_PATH%\hbedit.prg /n /q /w /d__GTHWG__  -I%HB_INSTALL%\include %1 2>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fedit.prg /n /q /w /d_FULL /d__GTHWG__ /d_USE_SSH2 -I%HB_INSTALL%\include -I%HWGUI_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fmenu.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fcmd.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fgetsys.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\falert.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\ffiles.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fkeymaps.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fdiff.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\hilight.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fautoc.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\errorsys.prg /n /q -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\hbcommander.prg /n /q /w /d__GTHWG__  /d_USE_SSH2 -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\fview.prg /n /q /w /d_USE_SSH2 -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\hbextcli.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\hbcvf.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out
%HB_INSTALL%\bin\harbour %SRC_PATH%\ssh2\hbcssh2.prg /n /q /w -I%HB_INSTALL%\include %1 2>>harbour.out

gcc -I%HB_INSTALL%\include -D_USE_HB -DUNICODE -D__GTHWG__ -c hbedit.c fedit.c fmenu.c fcmd.c fgetsys.c falert.c ffiles.c fkeymaps.c fdiff.c hilight.c fautoc.c errorsys.c hbcommander.c fview.c hbextcli.c hbcvf.c hbcssh2.c %SRC_PATH%\ssh2\hb_ssh2.c %SRC_PATH%\cfuncs.c %SRC_PATH%\trie\trie.c %SRC_PATH%\trie\hbtrie.c
gcc -Wall -mwindows -ohbeditw.exe hbedit.o fedit.o fmenu.o fcmd.o fgetsys.o falert.o ffiles.o fkeymaps.o fdiff.o hilight.o fautoc.o errorsys.o hbcommander.o fview.o hbextcli.o hbcvf.o hbcssh2.o hb_ssh2.o cfuncs.o trie.o hbtrie.o -L. -L%HB_INSTALL%\lib\win\mingw -L%HWGUI_INSTALL%\lib -Wl,--allow-multiple-definition -Wl,--start-group %HB_LIBS% -luser32 -lwinspool -lcomctl32 -lcomdlg32 -lgdiplus -lgdi32 -lole32 -loleaut32 -luuid -lwinmm -lwsock32 -lws2_32 -liphlpapi -lssh2 -Wl,--end-group -lhbpp

del *.o
del *.c
