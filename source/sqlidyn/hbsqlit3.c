/*
 * SqliDyn - Harbour SQLite API
 * Harbour bindings for sqlite3 shared library - dynamic linking
 *
 * Copyright 2025 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "item.api"

#include "sqlit3.h"

/* sqlt_Init( [ cDllName ] )
 */
HB_FUNC( SQLT_INIT ) {

   hb_retni( sqlt_Init( (HB_ISCHAR(1))?  hb_parc(1) : NULL ) );
}

/* sqlt_Exit()
 */
HB_FUNC( SQLT_EXIT ) {

   sqlt_Exit();
}

/* sqlt_LibVersion() -> nVersion
 */
HB_FUNC( SQLT_LIBVERSION ) {

   hb_retni( sqlt_libVersion() );
}

/* sqlt_Create( cDbName ) -> pDb
 */
HB_FUNC( SQLT_CREATE ) {

   hb_retptr( sqlt_Create( (char*) hb_parc(1) ) );
}

/* sqlt_Open( cDbName[, iOpt] ) -> pDb
 */
HB_FUNC( SQLT_OPEN ) {

   hb_retptr( sqlt_Open( (char*)hb_parc(1), HB_ISNUM(2)? hb_parni(2) : 0 ) );
}

/* sqlt_Close( pDb )
 */
HB_FUNC( SQLT_CLOSE ) {

   sqlt_Close( hb_parptr(1) );
}

/* sqlt_Exec( pDb, cQuery ) -> iRes
 */
HB_FUNC( SQLT_EXEC ) {

   hb_retni( sqlt_Exec( hb_parptr(1), (char*)hb_parc(2) ) );
}

/* sqlt_Prepare( pDb, cQuery ) -> pStmt
 */
HB_FUNC( SQLT_PREPARE ) {

   hb_retptr( sqlt_Prepare( hb_parptr(1), (char*)hb_parc(2) ) );
}

/* sqlt_Step( pStmt ) -> iRes
 */
HB_FUNC( SQLT_STEP ) {

   hb_retni( sqlt_Step( hb_parptr(1) ) );
}

/* sqlt_Finalize( pStmt ) -> iRes
 */
HB_FUNC( SQLT_FINALIZE ) {

   hb_retni( sqlt_Finalize( hb_parptr(1) ) );
}

/* sqlt_ColumnBlob( pStmt, iCol ) -> cValue
 */
HB_FUNC( SQLT_COLUMNBLOB ) {

   hb_retclen( (char*) sqlt_Column_blob( hb_parptr(1), hb_parni(2) ),
      sqlt_Column_bytes( hb_parptr(1), hb_parni(2) ) );
}

/* sqlt_ColumnDouble( pStmt, iCol ) -> iValue
 */
HB_FUNC( SQLT_COLUMNDOUBLE ) {

   hb_retnd( sqlt_Column_double( hb_parptr(1), hb_parni(2) ) );
}

/* sqlt_ColumnInt( pStmt, iCol ) -> iValue
 */
HB_FUNC( SQLT_COLUMNINT ) {

   hb_retni( sqlt_Column_int( hb_parptr(1), hb_parni(2) ) );
}

/* sqlt_ColumnInt64( pStmt, iCol ) -> lValue
 */
HB_FUNC( SQLT_COLUMNINT64 ) {

   hb_retnint( sqlt_Column_int64( hb_parptr(1), hb_parni(2) ) );
}

/* sqlt_ColumnText( pStmt, iCol ) -> cValue
 */
HB_FUNC( SQLT_COLUMNTEXT ) {

   hb_retclen( (char*) sqlt_Column_text( hb_parptr(1), hb_parni(2) ),
      sqlt_Column_bytes( hb_parptr(1), hb_parni(2) ) );
}

/* sqlt_ColumnType( pStmt ) -> iType
 */
HB_FUNC( SQLT_COLUMNTYPE ) {

   hb_retni( sqlt_Column_type( hb_parptr(1), hb_parni(2) ) );
}

/* sqlt_ColumnBytes( pStmt ) -> iType
 */
HB_FUNC( SQLT_COLUMNBYTES ) {

   hb_retni( sqlt_Column_bytes( hb_parptr(1), hb_parni(2) ) );
}

/* sqlt_BindBlob( pStmt, iPos, szValue ) -> iRes
 */
HB_FUNC( SQLT_BINDBLOB ) {

   hb_retni( sqlt_Bind_blob( hb_parptr(1), hb_parni(2), (char*)hb_parc(3), hb_parclen(3) ) );
}

/* sqlt_BindDouble( pStmt, iPos, dValue ) -> iRes
 */
HB_FUNC( SQLT_BINDDOUBLE ) {

   hb_retni( sqlt_Bind_double( hb_parptr(1), hb_parni(2), hb_parnd(3) ) );
}

/* sqlt_BindInt( pStmt, iPos, iValue ) -> iRes
 */
HB_FUNC( SQLT_BINDINT ) {

   hb_retni( sqlt_Bind_int( hb_parptr(1), hb_parni(2), hb_parni(3) ) );
}

/* sqlt_BindInt64( pStmt, iPos, lValue ) -> iRes
 */
HB_FUNC( SQLT_BINDINT64 ) {

   hb_retni( sqlt_Bind_int64( hb_parptr(1), hb_parni(2), hb_parnint(3) ) );
}

/* sqlt_BindText( pStmt, iPos, szValue ) -> iRes
 */
HB_FUNC( SQLT_BINDTEXT ) {

   hb_retni( sqlt_Bind_text( hb_parptr(1), hb_parni(2), (char*)hb_parc(3) ) );
}

/* sqlt_ClearBindings( pStmt ) -> iRes
 */
HB_FUNC( SQLT_CLEARBINDINGS ) {

   hb_retni( sqlt_Clear_bindings( hb_parptr(1) ) );
}

/* sqlt_LastInsertRowid( pDb )
 */
HB_FUNC( SQLT_LASTINSERTROWID ) {

   hb_retni( sqlt_Last_insert_rowid( hb_parptr(1) ) );
}

/* sqlt_Errcode( pDb ) -> iRes
 */
HB_FUNC( SQLT_ERRCODE ) {

   hb_retni( sqlt_Errcode( hb_parptr(1) ) );
}

/* sqlt_Changes( pDb ) -> iRows
 */
HB_FUNC( SQLT_CHANGES ) {

   hb_retni( sqlt_Changes( hb_parptr(1) ) );
}