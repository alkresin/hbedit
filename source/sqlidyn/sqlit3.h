/*
 * SqliDyn - Harbour SQLite API
 * Header file
 *
 * Copyright 2025 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

typedef struct sqlite3 SQLTConn;
typedef struct sqlite3_stmt SQLTstmt;

typedef void (*sqlite3_destructor_type)(void*);
#define SQLITE_STATIC      ((sqlite3_destructor_type)0)
#define SQLITE_TRANSIENT   ((sqlite3_destructor_type)-1)

#define SQLITE_OK           0

#define SQLITE_OPEN_READONLY         0x00000001  /* Ok for sqlite3_open_v2() */
#define SQLITE_OPEN_READWRITE        0x00000002  /* Ok for sqlite3_open_v2() */
#define SQLITE_OPEN_CREATE           0x00000004  /* Ok for sqlite3_open_v2() */
#define SQLITE_OPEN_DELETEONCLOSE    0x00000008  /* VFS only */
#define SQLITE_OPEN_EXCLUSIVE        0x00000010  /* VFS only */

extern int sqlt_Init( const char* szDllName );
extern void sqlt_Exit();
extern int sqlt_libVersion();
extern SQLTConn * sqlt_Create( char * szName );
extern SQLTConn * sqlt_Open( char * szName, int iFlags );
extern void sqlt_Close( SQLTConn *db );
extern int sqlt_Exec( SQLTConn *db, char *szQuery );
extern SQLTstmt * sqlt_Prepare( SQLTConn *db, char *szQuery );
extern int sqlt_Step( SQLTstmt *stmt );
extern int sqlt_Finalize( SQLTstmt *stmt );
extern void * sqlt_Column_blob( SQLTstmt *stmt, int iCol );
extern double sqlt_Column_double( SQLTstmt *stmt, int iCol );
extern int sqlt_Column_int( SQLTstmt *stmt, int iCol );
extern long sqlt_Column_int64( SQLTstmt *stmt, int iCol );
extern unsigned char * sqlt_Column_text( SQLTstmt *stmt, int iCol );
extern int sqlt_Column_type( SQLTstmt *stmt, int iCol );
extern int sqlt_Column_bytes( SQLTstmt *stmt, int iCol );
extern int sqlt_Bind_blob( SQLTstmt *stmt, int iPos, void * value, int iLen );
extern int sqlt_Bind_double( SQLTstmt *stmt, int iPos, double dValue );
extern int sqlt_Bind_int( SQLTstmt *stmt, int iPos, int iValue );
extern int sqlt_Bind_int64( SQLTstmt *stmt, int iPos, long lValue );
extern int sqlt_Bind_text( SQLTstmt *stmt, int iPos, char * szValue );
extern int sqlt_Clear_bindings( SQLTstmt *stmt );
extern long sqlt_Last_insert_rowid( SQLTConn *db );
extern int sqlt_Errcode( SQLTConn *db );
extern int sqlt_Changes( SQLTConn *db );