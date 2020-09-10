/*
 * A header for trie.c
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define SUFFIX_LEN         4
#define TRIE_PAGE_SIZE   512
#define TRIE_PAGES_ADD     8
#define LIST_BUFF_LEN   1024
#define MAX_WORD_LEN     128

typedef struct trieITEM
{
   char              letter;
   struct trieITEM *  right;
   struct trieITEM *   next;
   char  suffix[SUFFIX_LEN];
} TRIEITEM;

typedef TRIEITEM TRIEPAGE[TRIE_PAGE_SIZE];

typedef struct trieBASE
{
   //struct trieITEM ** [TRIE_PAGE_SIZE] pages;
   TRIEPAGE **         pages;
   int                iPages;
   int             iLastPage;
   int             iLastItem;
   int                 bUtf8;
   int                 bCase;
} TRIE;

extern TRIE * trie_Create( int bCase );
extern void trie_Close( TRIE * trie );
extern void trie_Add( TRIE * trie, char * szWord );
extern int trie_Count( TRIE * trie, char * szWord );
extern char * trie_List( TRIE * trie, char * szWord, int * iCount );
extern int trie_Exist( TRIE * trie, char * szWord );
extern void trie_Trace( TRIE * trie, char * szWord );
