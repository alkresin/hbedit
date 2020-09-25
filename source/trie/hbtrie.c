/*
 * Harbour wrappers for trie.c
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbapi.h"
#include "hbvm.h"
#include "hbapiitm.h"
#include "trie.h"

/*
 * hTrie := trie_Create( [lCase] )
 */
HB_FUNC( TRIE_CREATE )
{

   hb_retptr( ( void * ) trie_Create( (HB_ISNIL(1))? 1 : hb_parl(1) ) );
}


HB_FUNC( TRIE_OPEN )
{
   hb_retptr( ( void * ) trie_Open( (char *) hb_parc( 1 ) ) );
}

/*
 * trie_Close( hTrie )
 */
HB_FUNC( TRIE_CLOSE )
{
   trie_Close( (TRIE *) hb_parptr( 1 ) );
}

/*
 * trie_Add( hTrie, cWord )
 */
HB_FUNC( TRIE_ADD )
{
   TRIE * trie = (TRIE *) hb_parptr( 1 );

   trie_Add( trie, (char *) hb_parc( 2 ) );
}

/*
 * lExist := trie_Exist( hTrie, cWord )
 */
HB_FUNC( TRIE_EXIST )
{
   TRIE * trie = (TRIE *) hb_parptr( 1 );
   hb_retl( trie_Exist( trie, (char *) hb_parc( 2 ) ) );
}

/*
 * nCount := trie_Count( hTrie, cWord )
 */
HB_FUNC( TRIE_COUNT )
{
   TRIE * trie = (TRIE *) hb_parptr( 1 );
   hb_retni( trie_Count( trie, (char *) hb_parc( 2 ) ) );
}

/*
 * cList := trie_List( hTrie, cWord [,@nCount] )
 */
HB_FUNC( TRIE_LIST )
{
   TRIE * trie = (TRIE *) hb_parptr( 1 );
   int iCou;
   char * cBuff = trie_List( trie, (char *) hb_parc( 2 ), &iCou );

   hb_storni( iCou, 3 );
   if( iCou )
   {
      hb_retc( cBuff );
      free( cBuff );
   }
}

/*
 * trie_Trace( hTrie, cWord )
 */
HB_FUNC( TRIE_TRACE )
{
   TRIE * trie = (TRIE *) hb_parptr( 1 );
   trie_Trace( trie, (char *) hb_parc( 2 ) );
}

HB_FUNC( TRIE_SAVE )
{

   trie_Save( (TRIE *) hb_parptr( 1 ), (char *) hb_parc( 2 ) );
}
