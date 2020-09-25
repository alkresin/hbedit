/*
 * An implementation of the trie (prefix tree).
 *
 * Copyright 2020 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include "trie.h"

void _writelog( const char * sFile, const char * sTraceMsg, ... )
{
   FILE *hFile;

   hFile = fopen( sFile, "a" );

   if( hFile )
   {
      va_list ap;

      va_start( ap, sTraceMsg );
      vfprintf( hFile, sTraceMsg, ap );
      va_end( ap );

      fclose( hFile );
   }

}

static int scmpi( char * sz1, char * sz2, int iLen )
{
   int i;

   for( i=0; i<iLen; i++ )
   {
      if( tolower( sz1[i] ) != tolower( sz2[i] ) )
         return 1;
   }
   return 0;
}

static TRIEITEM * CreateTrieItem( TRIE * trie, char * szWord )
{
   TRIEITEM * p;
   int iLen = strlen( szWord ) - 1;

   if( trie->iLastItem == TRIE_PAGE_SIZE )
   {
      trie->iLastPage ++;
      if( trie->iLastPage == trie->iPages )
      {
         trie->iPages += TRIE_PAGES_ADD;
         trie->pages = (TRIEPAGE **) realloc( (void*)trie->pages, trie->iPages * sizeof( TRIEITEM** ) );
         memset( trie->pages + trie->iPages - TRIE_PAGES_ADD, 0, TRIE_PAGES_ADD * sizeof( TRIEITEM** ) );
      }
      trie->pages[trie->iLastPage] = (TRIEPAGE *) malloc( TRIE_PAGE_SIZE * sizeof( TRIEITEM ) );
      trie->iLastItem = 0;
   }

   p = **(trie->pages + trie->iLastPage) + trie->iLastItem;
   trie->iLastItem ++;
   trie->iItems ++;
   p->letter = *szWord;
   p->right = NULL;
   p->next = NULL;
   memset( p->suffix, 0, sizeof( p->suffix ) );

   //_writelog( "ac.log", "cr: %lu %d %c\r\n", &(p->letter), strlen(szWord), *szWord );
   if( !iLen )
      p->suffix[0] = '\n';
   else if( iLen <= SUFFIX_LEN )
      memcpy( p->suffix, szWord+1, iLen );
   else
      p->next = CreateTrieItem( trie, szWord+1 );

   return p;
}

TRIE * trie_Create( int bCase )
{
   TRIE * trie = (TRIE*) malloc( sizeof(TRIE) );
   int iPages = TRIE_PAGES_ADD;

   trie->pages = (TRIEPAGE **) malloc( iPages * sizeof( TRIEITEM** ) );
   memset( trie->pages, 0, iPages * sizeof( TRIEITEM** ) );
   trie->iPages = iPages;
   trie->iLastPage = trie->iLastItem = trie->iItems = 0;
   trie->bUtf8 = 0;
   trie->bCase = bCase;

   trie->pages[0] = (TRIEPAGE *) malloc( TRIE_PAGE_SIZE * sizeof( TRIEITEM ) );
   memset( trie->pages[0], 0, TRIE_PAGE_SIZE * sizeof( TRIEITEM ) );

   return trie;
}

TRIE * trie_Open( char * szFileName )
{
   TRIE * trie = (TRIE*) malloc( sizeof(TRIE) );
   int iPages = TRIE_PAGES_ADD;
   long lSize;
   FILE *hFile;
   char * cBuff;
   int bCase;

   hFile = fopen( szFileName, "rb" );
   if( !hFile )
      return NULL;

   fseek( hFile, 0, SEEK_END );
   lSize = ftell( hFile );
   fseek( hFile, 0, SEEK_SET );

   if( !lSize )
   {
      fclose( hFile );
      return NULL;
   }

   cBuff = (char*) malloc( lSize+1 );
   fwrite( cBuff, 1, lSize, hFile );

   fclose( hFile );

   trie->pages = (TRIEPAGE **) malloc( iPages * sizeof( TRIEITEM** ) );
   memset( trie->pages, 0, iPages * sizeof( TRIEITEM** ) );
   trie->iPages = iPages;
   trie->iLastPage = trie->iLastItem = trie->iItems = 0;
   trie->bUtf8 = 0;
   trie->bCase = bCase;

   trie->pages[0] = (TRIEPAGE *) malloc( TRIE_PAGE_SIZE * sizeof( TRIEITEM ) );
   memset( trie->pages[0], 0, TRIE_PAGE_SIZE * sizeof( TRIEITEM ) );

   free( cBuff );

   return trie;
}

void trie_Close( TRIE * trie )
{
   int i;

   for( i = 0; i < trie->iPages; i++ )
      if( trie->pages[i] )
         free( trie->pages[i] );
   free( trie->pages );
   free( trie );
}

static void Add2Buff( char **pBuff, int *piBuffLen, int *piPos, char *szWord )
{
   int iLen = strlen( szWord );
   if( *piPos + iLen + 1 > *piBuffLen )
   {
      (*piBuffLen) += LIST_BUFF_LEN;
      *pBuff = (char*) realloc( *pBuff, *piBuffLen );
   }
   memcpy( *pBuff+*piPos, szWord, iLen );
   //_writelog( "ac.log", "2buf: %d %d %s\r\n", *piPos, iLen, szWord );
   (*piPos) += iLen;
   (*pBuff)[*piPos] = '\n';
   (*piPos) ++;
}

static int ListItems( TRIEITEM * p, char **cBuff, int *piBuffLen, int *piPos, char *szWord, int iLevel )
{
   int iCou = 0, iLen = strlen( szWord );
   char szBuff[MAX_WORD_LEN];

   //_writelog( "ac.log", "li0> %d %s\r\n", iLevel, szWord );
   while( 1 )
   {
      if( p->suffix[0] )
      {
         memcpy( szBuff, szWord, iLen );
         iCou ++;
         szBuff[iLen] = p->letter;
         szBuff[iLen+1] = szBuff[iLen+1+SUFFIX_LEN] = '\0';
         if( p->suffix[0] != '\n' )
            memcpy( szBuff+iLen+1, p->suffix, SUFFIX_LEN );
         //_writelog( "ac.log", "li1> %d %c %s\r\n", iLevel, p->letter, szBuff );
         Add2Buff( cBuff, piBuffLen, piPos, szBuff );
      }
      if( p->next )
      {
         memcpy( szBuff, szWord, iLen );
         szBuff[iLen] = p->letter;
         szBuff[iLen+1] = '\0';
         //_writelog( "ac.log", "li2> %s\r\n", szBuff );
         iCou += ListItems( p->next, cBuff, piBuffLen, piPos, szBuff, iLevel );
      }
      if( p->right )
         p = p->right;
      else
         break;
   }
   return iCou;

}

static int CountItems( TRIEITEM * p )
{
   int i = 0;

   while( 1 )
   {
      if( p->suffix[0] )
         i ++;
      if( p->next )
         i += CountItems( p->next );
      if( p->right )
         p = p->right;
      else
         break;
   }
   return i;
}

void trie_Trace( TRIE * trie, char * szWord )
{
   int iLen = strlen( szWord ), i, n = 0;
   TRIEITEM * p = **(trie->pages);
   char c;
   char s[512];

   if( !trie->iItems )
      return;
   memset( s, 0, 512 );
   for( i=0; i<iLen; i++ )
   {
      c = (trie->bCase)? szWord[i] : tolower(szWord[i]);
      while( 1 )
      {
         if( c == ( (trie->bCase)? p->letter : tolower(p->letter) ) )
         {
            if( i > 0 )
               _writelog( "trace.log", "%s|\r\n", s );
            if( p->suffix[0] )
               _writelog( "trace.log", "%s%c+%c%c%c%c\r\n", s, szWord[i],
                  (p->suffix[0]&&p->suffix[0]!='\n')? p->suffix[0]:' ', (p->suffix[1])? p->suffix[1]:' ',
                  (p->suffix[2])? p->suffix[2]:' ',(p->suffix[3])? p->suffix[3]:' ' );
            else
               _writelog( "trace.log", "%s%c\r\n", s, szWord[i] );
            break;
         }
         else if( !p->right )
         {
            _writelog( "trace.log", "%s====\r\n", s );
            return;
         }
         else
         {
            n += 4;
            if( n > 0 )
               memset( s, 32, n );
            p = p->right;
         }
      }
      if( p->next )
         p = p->next;
      else
      {
         i ++;
         break;
      }
   }
}

static int FindItem( TRIE * trie, char * szWord, TRIEITEM ** pp )
{
   int iLen = strlen( szWord ), i = 0, iSuffLen;
   TRIEITEM * p = **(trie->pages);
   char c;

   while( 1 )
   {
      c = (trie->bCase)? szWord[i] : tolower(szWord[i]);
      while( 1 )
      {
         if( c == ( (trie->bCase)? p->letter : tolower(p->letter) ) )
         {
            break;
         }
         else if( !p->right )
         {
            //_writelog( "ac.log", "fi1> %d %s\r\n", i, szWord );
            *pp = NULL;
            return -1;
         }
         else
            p = p->right;
      }
      if( ++i < iLen && p->next )
         p = p->next;
      else
         break;
   }

   if( p->suffix[0] )
   {
      iSuffLen = (p->suffix[SUFFIX_LEN-1])? SUFFIX_LEN : strlen(p->suffix);
      if( iSuffLen < ( iLen = strlen( szWord+i ) ) ||
         ( ( (trie->bCase)? memcmp( p->suffix, szWord+i, iLen ) :
         scmpi( p->suffix, szWord+i, iLen ) ) != 0 ) )
      {
         //_writelog( "ac.log", "fi> %d %d %d %c%c%c%c %s %s\r\n", i, iSuffLen, iLen,
         //   p->suffix[0], p->suffix[1], p->suffix[2], p->suffix[3], szWord+i, szWord );
         *pp = NULL;
         return -1;
      }
   }

   //_writelog( "ac.log", "fiOK> %d %s\r\n", i, szWord );
   *pp = p;
   return i;
}

static int AddItem( TRIE * trie, char * szWord )
{
   int iLen = strlen( szWord ), i;
   TRIEITEM * p = **(trie->pages);
   char c, cTemp[SUFFIX_LEN+1];

   //_writelog( "trace.log", ">>%s\r\n", szWord );
   for( i=0; i<iLen; i++ )
   {
      c = (trie->bCase)? szWord[i] : tolower(szWord[i]);
      while( 1 )
      {
         if( c == ( (trie->bCase)? p->letter : tolower(p->letter) ) )
         {
            break;
         }
         else if( !p->right )
         {
            p->right = CreateTrieItem( trie, szWord + i );
            //_writelog( "ac.log", "A1>%c %d %s %s\r\n", p->letter, i, szWord, szWord+i );
            return -1;
         }
         else
            p = p->right;
      }
      if( i+1 < iLen )
      {
         if( p->next )
            p = p->next;
         else
         {
            if( p->suffix[0] && p->suffix[0] != '\n' )
            {
               i ++;
               memcpy( cTemp, p->suffix, SUFFIX_LEN );
               cTemp[SUFFIX_LEN] = '\0';
               p->next = CreateTrieItem( trie, cTemp );
               //_writelog( "ac.log", "A2>%c %d %s %s\r\n", p->letter, i, szWord, szWord+i );
               memset( p->suffix, 0, sizeof( p->suffix ) );
               AddItem( trie, szWord );
            }
            else
            {
               p->next = CreateTrieItem( trie, szWord + i + 1 );
               //_writelog( "ac.log", "A3>%c %d %s %s\r\n", p->letter, i, szWord, szWord+i+1 );
            }
            return i;
         }
      }
   }
   if( p->suffix[0] )
   {
      if( p->suffix[0] != '\n' )
      {
         memcpy( cTemp, p->suffix, SUFFIX_LEN );
         cTemp[SUFFIX_LEN] = '\0';
         p->next = CreateTrieItem( trie, cTemp );
         //_writelog( "ac.log", "A4>%c %d %s %s\r\n", p->letter, i, szWord, szWord+i );
         memset( p->suffix, 0, sizeof( p->suffix ) );
         AddItem( trie, szWord );
      }
   }
   else
      p->suffix[0] = '\n';
   return i;
}

void trie_Add( TRIE * trie, char * szWord )
{
   if( !trie->iLastPage && !trie->iLastItem )
      CreateTrieItem( trie, szWord );
   else
      AddItem( trie, szWord );
}

int trie_Count( TRIE * trie, char * szWord )
{
   int i, iCou = 0;
   TRIEITEM * p;

   if( !trie->iItems )
      return 0;

   if( !szWord )
      szWord = "";
   if( strlen( szWord ) > 0 )
   {
      i = FindItem( trie, szWord, &p );
      if( i < 0 )
         return 0;

      if( p->suffix[0] )
         iCou ++;
      if( p->next )
         iCou += CountItems( p->next );
   }
   else
      iCou = CountItems( **(trie->pages) );

   return iCou;
}

char * trie_List( TRIE * trie, char * szWord, int * iCount )
{
   int i, iCou = 0, iBuffLen = LIST_BUFF_LEN, iPos = 0;
   TRIEITEM * p;
   char * cBuff;
   char szBuff[MAX_WORD_LEN];

   *iCount = 0;
   if( !trie->iItems )
      return NULL;

   if( !szWord )
      szWord = "";
   if( strlen( szWord ) == 0 )
      i = 0;
   else
   {
      i = FindItem( trie, szWord, &p );
      if( i < 0 )
         return NULL;
   }

   cBuff = (char*) malloc( iBuffLen );
   //_writelog( "ac.log", "\r\n-- tl0> %d %s --\r\n", i, szWord );

   if( i > 0 )
   {
      if( p->suffix[0] )
      {
         memcpy( szBuff, szWord, i );
         iCou ++;
         szBuff[i] = szBuff[i+SUFFIX_LEN] = '\0';
         if( p->suffix[0] != '\n' )
            memcpy( szBuff+i, p->suffix, SUFFIX_LEN );
         //_writelog( "ac.log", "tl1> %d %s %s\r\n", i, szBuff, szWord );
         Add2Buff( &cBuff, &iBuffLen, &iPos, szBuff );
      }
      if( p->next )
         iCou += ListItems( p->next, &cBuff, &iBuffLen, &iPos, szWord, i );
   }
   else
      iCou += ListItems( **(trie->pages), &cBuff, &iBuffLen, &iPos, szWord, i );

   cBuff[iPos-1] = '\0';
   *iCount = iCou;
   if( !iCou )
   {
      free( cBuff );
      cBuff = NULL;
   }
   return cBuff;
}

void trie_Save( TRIE * trie, char * szFileName )
{
   int iBuffLen = LIST_BUFF_LEN, iPos;
   char * cBuff;
   FILE *hFile;

   cBuff = (char*) malloc( iBuffLen );

   sprintf( cBuff, "hbtrie 1.0/%d/%d/%d/%d/\n", trie->iLastPage, trie->iItems, trie->bUtf8, trie->bCase );
   iPos = strlen( cBuff );

   if( trie->iItems )
   {
      ListItems( **(trie->pages), &cBuff, &iBuffLen, &iPos, "", 0 );
      cBuff[iPos-1] = '\0';
   }

   hFile = fopen( szFileName, "wb" );
   fwrite( cBuff, 1, iPos-1, hFile );
   fclose( hFile );

   free( cBuff );

   return;
}

int trie_Exist( TRIE * trie, char * szWord )
{

   int i;
   TRIEITEM * p;

   if( !trie->iItems )
      return 0;
   i = FindItem( trie, szWord, &p );
   return ( i >= 0 );
}
