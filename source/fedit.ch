/*
 * TEdit() class header
 *
 * Copyright 2020-2025 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#ifdef __BUILT_IN
   #define _NO_HBC
#else
   #include "hbfuncsfull.ch"
   #ifdef __GTHWG__
      #ifdef __PLATFORM__UNIX
         #define __GTK__
      #endif
      #include "hwgextern.ch"
   #endif
#endif

#define HBEDIT_VERSION  "v2.7-15"