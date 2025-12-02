/*
 * SqliDyn - Harbour SQLite API
 * C bindings for sqlite3 shared library - dynamic linking
 *
 * Copyright 2025 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#ifdef _WIN32
#include <windows.h>
#define LIB_HANDLE HMODULE
#define GET_FUNCTION GetProcAddress
#define CLOSE_LIBRARY FreeLibrary
#else
#include <dlfcn.h>
#define LIB_HANDLE void*
#define GET_FUNCTION dlsym
#define CLOSE_LIBRARY dlclose
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "sqlit3.h"

typedef int (*psqlt_libver_t)();
typedef int (*psqlt_open_t)( const char *filename, SQLTConn **pDb );
typedef int (*psqlt_open_v2_t)( const char *filename, SQLTConn **pDb, int flags, const char *zVfs );
typedef int (*psqlt_close_t)( SQLTConn* db );
typedef int (*psqlt_exec_t)( SQLTConn*, const char *sql, int (*callback)(void*,int,char**,char**), void *, char **errmsg );
typedef int (*psqlt_prepare_v2_t)( SQLTConn *db, const char *zSql, int nByte, SQLTstmt **ppStmt, const char **pzTail );
typedef int (*psqlt_step_t)( SQLTstmt* );
typedef int (*psqlt_finalize_t)( SQLTstmt* );
typedef void* (*psqlt_column_blob_t)( SQLTstmt *, int iCol );
typedef double (*psqlt_column_double_t)( SQLTstmt*, int iCol );
typedef int (*psqlt_column_int_t)( SQLTstmt *, int iCol );
typedef long (*psqlt_column_int64_t)( SQLTstmt *, int iCol );
typedef unsigned char* (*psqlt_column_text_t)( SQLTstmt *, int iCol );
typedef int (*psqlt_column_type_t)( SQLTstmt*, int iCol );
typedef int (*psqlt_column_bytes_t)( SQLTstmt*, int iCol );
typedef int (*psqlt_bind_blob_t)( SQLTstmt*, int, const void*, int n, void(*)(void*) );
typedef int (*psqlt_bind_double_t)( SQLTstmt*, int, double );
typedef int (*psqlt_bind_int_t)( SQLTstmt*, int, int );
typedef int (*psqlt_bind_int64_t)( SQLTstmt*, int, long );
typedef int (*psqlt_bind_text_t)( SQLTstmt*,int,const char*,int,void(*)(void*) );
typedef int (*psqlt_clear_bindings_t)( SQLTstmt* );
typedef long (*psqlt_last_insert_rowid_t)( SQLTConn* );
typedef int (*psqlt_errcode_t)( SQLTConn *db );
typedef int (*psqlt_changes_t)( SQLTConn *db );

static psqlt_libver_t psqlt_libver = NULL;
static psqlt_open_t psqlt_open = NULL;
static psqlt_open_v2_t psqlt_open_v2 = NULL;
static psqlt_close_t psqlt_close = NULL;
static psqlt_exec_t psqlt_exec = NULL;
static psqlt_prepare_v2_t psqlt_prepare_v2 = NULL;
static psqlt_step_t psqlt_step = NULL;
static psqlt_finalize_t psqlt_finalize = NULL;
static psqlt_column_blob_t psqlt_column_blob = NULL;
static psqlt_column_double_t psqlt_column_double = NULL;
static psqlt_column_int_t psqlt_column_int = NULL;
static psqlt_column_int64_t psqlt_column_int64 = NULL;
static psqlt_column_text_t psqlt_column_text = NULL;
static psqlt_column_type_t psqlt_column_type = NULL;
static psqlt_column_bytes_t psqlt_column_bytes = NULL;
static psqlt_bind_blob_t psqlt_bind_blob = NULL;
static psqlt_bind_double_t psqlt_bind_double = NULL;
static psqlt_bind_int_t psqlt_bind_int = NULL;
static psqlt_bind_int64_t psqlt_bind_int64 = NULL;
static psqlt_bind_text_t psqlt_bind_text = NULL;
static psqlt_clear_bindings_t psqlt_clear_bindings = NULL;
static psqlt_last_insert_rowid_t psqlt_last_insert_rowid = NULL;
static psqlt_errcode_t psqlt_errcode = NULL;
static psqlt_changes_t psqlt_changes = NULL;

static char * errNoFunc = "Failed to get %s\n";
LIB_HANDLE pDll = NULL;

void c_writelog( const char * sFile, const char * sTraceMsg, ... )
{
   FILE *hFile;

   if( sFile == NULL )
   {
      hFile = fopen( "sqlidyn.log", "a" );
   }
   else
   {
      hFile = fopen( sFile, "a" );
   }

   if( hFile )
   {
      va_list ap;

      va_start( ap, sTraceMsg );
      vfprintf( hFile, sTraceMsg, ap );
      va_end( ap );

      fclose( hFile );
   }
}

#ifdef _WIN32

#include <windows.h>

static BOOL AddDirectoryToPath( const char* filePath ) {

   char* lastSlash;
   char* pathOnly = NULL;
   size_t pathLength = 0;
   char* currentPath = NULL;
   char* newPath = NULL;
   size_t newPathSize;
   BOOL result = FALSE;
   size_t neededSize = 0;

   lastSlash = strrchr( filePath, '\\' );
   if( ( lastSlash = strrchr( filePath, '\\' ) ) == 0 &&
      ( lastSlash = strrchr( filePath, '/' ) ) == 0 )
      return FALSE;

    // Выделяем память для пути без имени файла
    pathLength = lastSlash - filePath;
    pathOnly = (char*)malloc(pathLength + 1);

    // Копируем только путь
    //strncpy_s( pathOnly, pathLength + 1, filePath, pathLength );
    strncpy( pathOnly, filePath, pathLength );
    pathOnly[pathLength] = '\0';

    // Получаем текущее значение PATH
    neededSize = GetEnvironmentVariableA( "PATH", NULL, 0 );
    if( neededSize == 0 ) {
       // PATH не установлен, создаем новый
       if( SetEnvironmentVariableA("PATH", pathOnly ) )
          result = TRUE;
       free( pathOnly );
       return result;
    }

    // Выделяем память для текущего PATH и получаем текущее значение PATH
    currentPath = (char*) malloc( neededSize );
    GetEnvironmentVariableA( "PATH", currentPath, neededSize );

    // Проверяем, не содержится ли путь уже в PATH
    if( strstr(currentPath, pathOnly) != NULL ) {
       free( pathOnly );
       free( currentPath );
       return TRUE; // Уже есть, считаем успехом
    }

    // Выделяем память для нового PATH (текущий PATH + ; + новый путь)
    newPathSize = strlen(currentPath) + strlen(pathOnly) + 2; // +2 для ';' и '\0'
    newPath = (char*) malloc( newPathSize );

    // Формируем новый PATH
    snprintf( newPath, newPathSize, "%s;%s", currentPath, pathOnly );

    // Устанавливаем новое значение PATH
    if( SetEnvironmentVariableA( "PATH", newPath ) )
        result = TRUE;

    // Освобождаем память
    free( pathOnly );
    free( currentPath );
    free( newPath );

    return result;
}

static void FindAndOpenLib( const char* szDllName ) {

   if( !szDllName ) {
      pDll = LoadLibraryA( "sqlite3.dll" );
      return;
   }

   AddDirectoryToPath( szDllName );
   pDll = LoadLibraryA( szDllName );
}
#else

#include <dirent.h>
#include <fnmatch.h>

#define MAX_PATH_LEN 1024

// Функция для поиска файлов по паттерну в указанной директории
static char* FindlibraryInDir( const char* dir_path, const char* pattern ) {

    DIR* dir = opendir(dir_path);
    if (dir == NULL) {
        return NULL;
    }

    struct dirent* entry;
    char* found_path = NULL;

    while ((entry = readdir(dir)) != NULL) {
        if (fnmatch(pattern, entry->d_name, 0) == 0) {
            // Нашли подходящий файл
            found_path = malloc(MAX_PATH_LEN);
            snprintf(found_path, MAX_PATH_LEN, "%s/%s", dir_path, entry->d_name);
            break;
        }
    }

    closedir(dir);
    return found_path;
}

static void FindAndOpenLib( const char* szDllName ) {

   if( szDllName ) {
      pDll = dlopen( szDllName, RTLD_LAZY | RTLD_GLOBAL );
      if( !pDll )
         c_writelog( NULL, "Failed to load %s\n", szDllName );
      return;
   }

   // Пробуем основные варианты
   const char* try_names[] = {
      "libslite3.so",      // Основное имя
      "libslite3.so.0",    // Наиболее распространенная версия
      NULL
   };

   for (int i = 0; try_names[i] != NULL; i++) {
      pDll = dlopen( try_names[i], RTLD_LAZY | RTLD_GLOBAL );
      if( pDll )
         return;
   }

   // Если простой способ не сработал, используем расширенный поиск
   const char* search_paths[] = {
      "/usr/lib",
      "/usr/lib64",
      "/usr/local/lib",
      "/usr/local/lib64",
      "/lib",
      "/lib64",
      "/usr/lib/x86_64-linux-gnu",  // Для Debian/Ubuntu
      "/usr/lib/aarch64-linux-gnu", // Для ARM64
      NULL
   };

   const char* patterns[] = {
      "libsqlite3.so.*",      // Основной паттерн
      NULL
   };

   // Ищем в стандартных путях
   for (int i = 0; search_paths[i] != NULL; i++) {
      for (int j = 0; patterns[j] != NULL; j++) {
         char* lib_path = FindlibraryInDir(search_paths[i], patterns[j]);
         if (lib_path != NULL) {
            pDll = dlopen( lib_path, RTLD_LAZY | RTLD_GLOBAL );
            free(lib_path);
            if( pDll )
               return;
         }
      }
   }

   // Также проверяем пути из ld.so.conf
   FILE* ldconfig = popen("/sbin/ldconfig -p", "r");
   if (ldconfig != NULL) {
      char line[256];
      while (fgets(line, sizeof(line), ldconfig) != NULL) {
         if (strstr(line, "libsqlite3.so") != NULL) {
            // Парсим путь из вывода ldconfig
            char* path_start = strchr(line, '>');
            if (path_start != NULL) {
                path_start += 2; // Пропускаем "> "
                char* path_end = strchr(path_start, ' ');
                if (path_end != NULL) {
                   *path_end = '\0';

                   pDll = dlopen(path_start, RTLD_LAZY | RTLD_GLOBAL);
                   if( pDll ) {
                      pclose(ldconfig);
                      return;
                   }
                }
             }
          }
      }
      pclose(ldconfig);
   }
}
#endif

int sqlt_Init( const char* szDllName ) {

   if( pDll )
      return 0;

   FindAndOpenLib( szDllName );
   if( !pDll ) {
      c_writelog( NULL, "Failed to load library\n" );
      return 1;
   }

   if( !psqlt_open ) {
      char *szFunc = "sqlite3_open";
      psqlt_open = (psqlt_open_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_open ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return 2;
      }
   }
   if( !psqlt_open_v2 ) {
      char *szFunc = "sqlite3_open_v2";
      psqlt_open_v2 = (psqlt_open_v2_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_open_v2 ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return 2;
      }
   }
   if( !psqlt_close ) {
      char *szFunc = "sqlite3_close";
      psqlt_close = (psqlt_close_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_close ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return 2;
      }
   }
   if( !psqlt_exec ) {
      char *szFunc = "sqlite3_exec";
      psqlt_exec = (psqlt_exec_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_exec ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return 2;
      }
   }
   if( !psqlt_prepare_v2 ) {
      char *szFunc = "sqlite3_prepare_v2";
      psqlt_prepare_v2 = (psqlt_prepare_v2_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_prepare_v2 ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return 2;
      }
   }
   if( !psqlt_step ) {
      char *szFunc = "sqlite3_step";
      psqlt_step = (psqlt_step_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_step ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return 2;
      }
   }
   if( !psqlt_finalize ) {
      char *szFunc = "sqlite3_finalize";
      psqlt_finalize = (psqlt_finalize_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_finalize ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return 2;
      }
   }

   return 0;
}

void sqlt_Exit() {

   if( pDll )
      CLOSE_LIBRARY( pDll );
   pDll = NULL;
}

int sqlt_libVersion() {

   if( !pDll )
      return -1;
   if( !psqlt_libver ) {
      char *szFunc = "sqlite3_libversion_number";
      psqlt_libver = (psqlt_libver_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_libver ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }
   return psqlt_libver();
}

SQLTConn * sqlt_Create( char * szName ) {

   SQLTConn *db;

   if( !pDll )
      return NULL;

   if( psqlt_open( szName, &db ) == SQLITE_OK )
      return db;

   psqlt_close( db );
   return NULL;

}

SQLTConn * sqlt_Open( char * szName, int iFlags ) {

   SQLTConn *db;

   if( !pDll )
      return NULL;
   if( psqlt_open_v2( szName, &db, iFlags, NULL ) == SQLITE_OK )
      return db;

   psqlt_close( db );
   return NULL;

}

void sqlt_Close( SQLTConn *db ) {

   psqlt_close( db );
}

int sqlt_Exec( SQLTConn *db, char *szQuery ) {

   if( !pDll )
      return -1;
   return psqlt_exec( db, szQuery, 0, 0, NULL );
}

SQLTstmt * sqlt_Prepare( SQLTConn *db, char *szQuery ) {

   SQLTstmt *stmt;
   int iRes;

   if( !pDll )
      return NULL;

   iRes = psqlt_prepare_v2( db, szQuery, -1, &stmt, 0 );
   if( iRes != SQLITE_OK )
      return NULL;
   return stmt;

}

int sqlt_Step( SQLTstmt *stmt ) {

   return psqlt_step( stmt );
}

int sqlt_Finalize( SQLTstmt *stmt ) {

   return psqlt_finalize( stmt );
}

void * sqlt_Column_blob( SQLTstmt *stmt, int iCol ) {

   if( !psqlt_column_blob ) {
      char *szFunc = "sqlite3_column_blob";
      psqlt_column_blob = (psqlt_column_blob_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_column_blob ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return NULL;
      }
   }

   return psqlt_column_blob( stmt, iCol-1 );
}

double sqlt_Column_double( SQLTstmt *stmt, int iCol ) {

   if( !psqlt_column_double ) {
      char *szFunc = "sqlite3_column_double";
      psqlt_column_double = (psqlt_column_double_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_column_double ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_column_double( stmt, iCol-1 );
}

int sqlt_Column_int( SQLTstmt *stmt, int iCol ) {

   if( !psqlt_column_int ) {
      char *szFunc = "sqlite3_column_int";
      psqlt_column_int = (psqlt_column_int_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_column_int ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_column_int( stmt, iCol-1 );
}

long sqlt_Column_int64( SQLTstmt *stmt, int iCol ) {

   if( !psqlt_column_int64 ) {
      char *szFunc = "sqlite3_column_int64";
      psqlt_column_int64 = (psqlt_column_int64_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_column_int64 ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return (long) psqlt_column_int64( stmt, iCol-1 );
}

unsigned char * sqlt_Column_text( SQLTstmt *stmt, int iCol ) {

   if( !psqlt_column_text ) {
      char *szFunc = "sqlite3_column_text";
      psqlt_column_text = (psqlt_column_text_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_column_text ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return NULL;
      }
   }

   return psqlt_column_text( stmt, iCol-1 );
}

int sqlt_Column_type( SQLTstmt *stmt, int iCol ) {

   if( !psqlt_column_type ) {
      char *szFunc = "sqlite3_column_type";
      psqlt_column_type = (psqlt_column_type_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_column_type ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_column_type( stmt, iCol-1 );
}

int sqlt_Column_bytes( SQLTstmt *stmt, int iCol ) {

   if( !psqlt_column_bytes ) {
      char *szFunc = "sqlite3_column_bytes";
      psqlt_column_bytes = (psqlt_column_bytes_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_column_bytes ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_column_bytes( stmt, iCol-1 );
}

int sqlt_Bind_blob( SQLTstmt *stmt, int iPos, void * value, int iLen ) {

   if( !psqlt_bind_blob ) {
      char *szFunc = "sqlite3_bind_blob";
      psqlt_bind_blob = (psqlt_bind_blob_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_bind_blob ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_bind_blob( stmt, iPos, value, iLen, SQLITE_TRANSIENT );
}

int sqlt_Bind_double( SQLTstmt *stmt, int iPos, double dValue ) {

   if( !psqlt_bind_double ) {
      char *szFunc = "sqlite3_bind_double";
      psqlt_bind_double = (psqlt_bind_double_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_bind_double ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_bind_double( stmt, iPos, dValue );
}

int sqlt_Bind_int( SQLTstmt *stmt, int iPos, int iValue ) {

   if( !psqlt_bind_int ) {
      char *szFunc = "sqlite3_bind_int";
      psqlt_bind_int = (psqlt_bind_int_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_bind_int ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_bind_int( stmt, iPos, iValue );
}

int sqlt_Bind_int64( SQLTstmt *stmt, int iPos, long lValue ) {

   if( !psqlt_bind_int64 ) {
      char *szFunc = "sqlite3_bind_int64";
      psqlt_bind_int64 = (psqlt_bind_int64_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_bind_int64 ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_bind_int64( stmt, iPos, lValue );
}

int sqlt_Bind_text( SQLTstmt *stmt, int iPos, char * szValue ) {

   if( !psqlt_bind_text ) {
      char *szFunc = "sqlite3_bind_text";
      psqlt_bind_text = (psqlt_bind_text_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_bind_text ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_bind_text( stmt, iPos, szValue, -1, SQLITE_TRANSIENT );
}

int sqlt_Clear_bindings( SQLTstmt *stmt ) {

   if( !psqlt_clear_bindings ) {
      char *szFunc = "sqlite3_clear_bindings";
      psqlt_clear_bindings = (psqlt_clear_bindings_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_clear_bindings ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_clear_bindings( stmt );
}

long sqlt_Last_insert_rowid( SQLTConn *db ) {

   if( !psqlt_last_insert_rowid ) {
      char *szFunc = "sqlite3_last_insert_rowid";
      psqlt_last_insert_rowid = (psqlt_last_insert_rowid_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_last_insert_rowid ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return (long) psqlt_last_insert_rowid( db );
}

int sqlt_Errcode( SQLTConn *db ) {

   if( !psqlt_errcode ) {
      char *szFunc = "sqlite3_errcode";
      psqlt_errcode = (psqlt_errcode_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_errcode ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_errcode( db );
}

int sqlt_Changes( SQLTConn *db ) {

   if( !psqlt_changes ) {
      char *szFunc = "sqlite3_changes";
      psqlt_changes = (psqlt_errcode_t)GET_FUNCTION( pDll, szFunc );
      if( !psqlt_changes ) {
         c_writelog( NULL, errNoFunc, szFunc );
         return -1;
      }
   }

   return psqlt_changes( db );
}