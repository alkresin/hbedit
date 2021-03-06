/*
 * Hilighting class from HwGUI, adapted for a console editor (hbxml stuff removed)
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"

#define HILIGHT_KEYW1   1
#define HILIGHT_KEYW2   2
#define HILIGHT_KEYW3   3
#define HILIGHT_KEYW4   4
#define HILIGHT_KEYW5   5
#define HILIGHT_QUOTE   6
#define HILIGHT_SCOMM   7
#define HILIGHT_SLEFT   8
#define HILIGHT_MCOMM   9
#define HILIGHT_BLOCK  10

#define MAX_ITEMS    1024

STATIC cSpaces := e" \t\x9", cQuotes := e"\"\'"

CLASS HiliBase

   DATA   oEdit
   DATA   lCase      INIT .F.      // A flag - are the keywords case sensitive
   DATA   aLineStru, nItems, nLine

   METHOD New()   INLINE  Self
   METHOD End()
   METHOD DO()    INLINE  ( ::nItems := 0, Nil )

ENDCLASS

METHOD End() CLASS HiliBase

   ::oEdit := Nil

   RETURN Nil

CLASS Hili INHERIT HiliBase

   DATA   hHili
   DATA   cKeywords1, cKeywords2   // A list of keywords (commands), divided by space
   DATA   cKeywords3, cKeywords4, cKeywords5
   DATA   cScomm                   // A string, which starts single line comments
   DATA   cSleft
   DATA   cMcomm1, cMcomm2         // Start and end strings for multiline comments

   DATA   lMultiComm
   DATA   cQuo  INIT cQuotes
   DATA   aDop, nDopChecked

   METHOD New( hHili, cKeywords1, cKeywords2, cKeywords3, cKeywords4, cKeywords5, ;
      cSComm, cSLeft, cMComm, lCase, cQuo )
   METHOD SET( oEdit )
   METHOD DO( nLine, lCheck )
   METHOD UpdSource( nLine )  INLINE  ( ::nDopChecked := nLine - 1 )
   METHOD AddItem( nPos1, nPos2, nType )

ENDCLASS

METHOD New( hHili, cKeywords1, cKeywords2, cKeywords3, cKeywords4, cKeywords5,;
      cSComm, cSLeft, cMComm, lCase, cQuo ) CLASS Hili
   LOCAL nPos

   ::aLineStru := Array( 20, 3 )

   IF !Empty( hHili )
      ::hHili := hHili
      cKeywords1 := hb_hGetDef( hHili, "keywords1", "" )
      cKeywords2 := hb_hGetDef( hHili, "keywords2", "" )
      cKeywords3 := hb_hGetDef( hHili, "keywords3", "" )
      cKeywords4 := hb_hGetDef( hHili, "keywords4", "" )
      cKeywords5 := hb_hGetDef( hHili, "keywords5", "" )
      cSComm := hb_hGetDef( hHili, "scomm", "" )
      cSLeft := hb_hGetDef( hHili, "startline", "" )
      cMComm := hb_hGetDef( hHili, "mcomm", "" )
      lCase := hb_hGetDef( hHili, "case", .F. )
      cQuo := hb_hGetDef( hHili, "quotes", "" )
   ENDIF
   IF !Empty( cKeywords1 )
      ::cKeywords1 := " " + AllTrim( cKeywords1 ) + " "
   ENDIF
   IF !Empty( cKeywords2 )
      ::cKeywords2 := " " + AllTrim( cKeywords2 ) + " "
   ENDIF
   IF !Empty( cKeywords3 )
      ::cKeywords3 := " " + AllTrim( cKeywords3 ) + " "
   ENDIF
   IF !Empty( cKeywords4 )
      ::cKeywords4 := " " + AllTrim( cKeywords4 ) + " "
   ENDIF
   IF !Empty( cKeywords5 )
      ::cKeywords5 := " " + AllTrim( cKeywords5 ) + " "
   ENDIF

   IF !Empty( cSComm )
      ::cScomm := AllTrim( cScomm )
   ENDIF
   IF !Empty( cSleft )
      ::cSleft := AllTrim( cSleft )
   ENDIF

   IF !Empty( cMComm )
      ::cMcomm1 := AllTrim( cMcomm )
      IF !Empty( ::cMcomm1 ) .AND. ( nPos := At( " ", ::cMcomm1 ) ) > 0
         ::cMcomm2 := LTrim( SubStr( ::cMcomm1,nPos + 1 ) )
         ::cMcomm1 := Trim( Left( ::cMcomm1,nPos - 1 ) )
      ENDIF
   ENDIF
   IF ValType( lCase ) == 'L'
      ::lCase := lCase
   ENDIF
   IF !Empty( cQuo )
      ::cQuo := AllTrim( cQuo )
   ENDIF

   IF !::lCase
      IF !Empty( ::cKeywords1 )
         ::cKeywords1 := Lower( ::cKeywords1 )
      ENDIF
      IF !Empty( ::cKeywords2 )
         ::cKeywords2 := Lower( ::cKeywords2 )
      ENDIF
      IF !Empty( ::cKeywords3 )
         ::cKeywords3 := Lower( ::cKeywords3 )
      ENDIF
      IF !Empty( ::cKeywords4 )
         ::cKeywords4 := Lower( ::cKeywords4 )
      ENDIF
      IF !Empty( ::cKeywords5 )
         ::cKeywords5 := Lower( ::cKeywords5 )
      ENDIF
   ENDIF

   RETURN Self

METHOD SET( oEdit ) CLASS Hili
   LOCAL oHili := ::New()

   oHili:hHili      := ::hHili
   oHili:cKeywords1 := ::cKeywords1
   oHili:cKeywords2 := ::cKeywords2
   oHili:cKeywords3 := ::cKeywords3
   oHili:cKeywords4 := ::cKeywords4
   oHili:cKeywords5 := ::cKeywords5
   oHili:cScomm     := ::cScomm
   oHili:cSleft     := ::cSleft
   oHili:cMcomm1    := ::cMcomm1
   oHili:cMcomm2    := ::cMcomm2
   oHili:lCase      := ::lCase
   oHili:oEdit      := oEdit

   RETURN oHili

/*  Scans the cLine and fills an array :aLineStru with hilighted items
 *  lComm set it to .T., if a previous line was a part of an unclosed multiline comment
 *  lCheck - if .T., checks for multiline comments only
 */

METHOD DO( nLine, lCheck ) CLASS Hili
   LOCAL aText, cLine, nLen, nLenS, nLenM, i, lComm, lUtf8 := ::oEdit:lUtf8
   LOCAL cs, cm, cf
   LOCAL nPos, nPos1, nPrev, cWord, c, lFirst := .T., nStartOffs := 1, nStartPos := 1

   ::nItems := 0
   ::lMultiComm := .F.

   IF lCheck == Nil
      lCheck := .F.
   ELSEIF lCheck .AND. Empty( ::cMcomm1 )
      RETURN Nil
   ENDIF

   aText := ::oEdit:aText
   cLine := aText[nLine]
   nLen := cp_Len( lUtf8, cLine )

   IF Empty( ::aDop )
      ::aDop := Array( Len( ::oEdit:aText ) )
      ::nDopChecked := 0
   ELSEIF Len( ::aDop ) < Len( aText )
      ::aDop := ASize( ::aDop, Len( aText ) )
   ENDIF
   IF ::nDopChecked < nLine - 1
      FOR i := ::nDopChecked + 1 TO nLine - 1
         ::Do( i, .T. )
         ::aDop[i] := iif( ::lMultiComm, 1, 0 )
      NEXT
   ENDIF
   lComm := iif( nLine == 1, .F. , !Empty( ::aDop[nLine - 1] ) )
   IF ::nDopChecked < nLine
      ::nDopChecked := nLine
   ENDIF
   ::aDop[nLine] := 0

   IF Empty( ::cMcomm1 )
      cm := ""
   ELSE
      cm := Left( ::cMcomm1, 1 )
      nLenM := Len( ::cMcomm1 )
   ENDIF

   IF !Empty( lComm )
      IF ( nPos := At( ::cMcomm2, cLine ) ) == 0
         IF !lCheck; ::AddItem( 1, cp_Len( lUtf8,cLine ), HILIGHT_MCOMM ); ENDIF
         ::lMultiComm := .T.
         ::aDop[nLine] := 1
         RETURN Nil
      ELSE
         IF !lCheck; ::AddItem( 1, nPos+Len(::cMcomm2)-1, HILIGHT_MCOMM ); ENDIF
         nPos += nLenM
      ENDIF
   ELSE
      nPos := 1
   ENDIF

   IF Empty( ::cScomm )
      cs := ""
   ELSE
      cs := Left( ::cScomm, 1 )
      nLenS := Len( ::cScomm )
   ENDIF

   IF Empty( ::cSleft )
      cf := ""
   ELSE
      cf := Left( ::cSleft, 1 )
   ENDIF

   DO WHILE nPos <= nLen
      //DO WHILE nPos <= nLen .AND. cp_Substr( lUtf8, cLine, nPos, 1 ) $ cSpaces; nPos ++ ; ENDDO
      DO WHILE nPos <= nLen .AND. cedi_Peek( lUtf8, cLine, nPos, @nStartOffs, @nStartPos ) $ cSpaces; nPos ++ ; ENDDO
      DO WHILE nPos <= nLen
         IF ( c := cedi_Peek( lUtf8, cLine, nPos, @nStartOffs, @nStartPos ) ) == ""
            RETURN Nil

         ELSEIF c $ ::cQuo
            nPos1 := nPos
            IF ( nPos := cp_At( lUtf8, c, cLine, nPos1 + 1 ) ) == 0
               nPos := nLen
            ENDIF
            IF !lCheck; ::AddItem( nPos1, nPos, HILIGHT_QUOTE ); ENDIF

         ELSEIF c == cs .AND. cedi_Substr( lUtf8, cLine, nPos, nLenS, nStartOffs, nStartPos ) == ::cScomm
            IF !lCheck; ::AddItem( nPos, cp_Len( lUtf8, cLine ), HILIGHT_SCOMM ); ENDIF
            nPos := nLen + 1
            EXIT

         ELSEIF c == cm .AND. cedi_Substr( lUtf8, cLine, nPos, nLenM, nStartOffs, nStartPos ) == ::cMcomm1
            nPos1 := nPos
            IF ( nPos := cp_At( lUtf8, ::cMcomm2, cLine, nPos1 + 1 ) ) == 0
               nPos := nLen
               ::lMultiComm := .T.
               ::aDop[nLine] := 1
            ENDIF
            IF !lCheck; ::AddItem( nPos1, nPos+Len(::cMcomm2)-1, HILIGHT_MCOMM ); ENDIF
            nPos += nLenM - 1

         ELSEIF lFirst .AND. c == cf .AND. cedi_Substr( lUtf8, cLine, nPos, Len(::cSleft), nStartOffs, nStartPos ) == ::cSleft
            nPos1 := nPos
            IF ( !Empty(::cScomm) .AND. ( nPos := cp_At( lUtf8, ::cScomm, cLine, nPos1 + 1 ) ) > 0 ) .OR. ;
               ( !Empty(::cMcomm1) .AND. ( nPos := cp_At( lUtf8, ::cMcomm1, cLine, nPos1 + 1 ) ) > 0 )
               nPos --
            ELSE
               nPos := nLen
            ENDIF
            ::AddItem( nPos1, nPos, HILIGHT_SLEFT )

         ELSEIF !lCheck .AND. IsLetter( c )
            nPos1 := nPos
            nPrev := nPos
            nPos := cp_NextPos( lUtf8, cLine, nPos )
            DO WHILE IsLetter( c := cedi_Substr( lUtf8, cLine,nPos,1, nStartOffs, nStartPos ) ) .OR. IsDigit( c )
               nPrev := nPos
               nPos := cp_NextPos( lUtf8, cLine, nPos )
            ENDDO
            cWord := " " + iif( ::lCase, cedi_Substr( lUtf8, cLine, nPos1, nPos - nPos1, nStartOffs, nStartPos ), ;
               Lower( cp_Substr( lUtf8, cLine, nPos1, nPos - nPos1 ) ) ) + " "
            nPos := nPrev
            IF !Empty( ::cKeywords1 ) .AND. cWord $ ::cKeywords1
               ::AddItem( nPos1, nPos, HILIGHT_KEYW1 )
            ELSEIF !Empty( ::cKeywords2 ) .AND. cWord $ ::cKeywords2
               ::AddItem( nPos1, nPos, HILIGHT_KEYW2 )
            ELSEIF !Empty( ::cKeywords3 ) .AND. cWord $ ::cKeywords3
               ::AddItem( nPos1, nPos, HILIGHT_KEYW3 )
            ELSEIF !Empty( ::cKeywords4 ) .AND. cWord $ ::cKeywords4
               ::AddItem( nPos1, nPos, HILIGHT_KEYW4 )
            ELSEIF !Empty( ::cKeywords5 ) .AND. cWord $ ::cKeywords5
               ::AddItem( nPos1, nPos, HILIGHT_KEYW5 )
            ENDIF
         ENDIF
         nPos ++
         lFirst := .F.
      ENDDO
   ENDDO
   IF !lCheck
      ::nLine := nLine
   ENDIF

   RETURN Nil

METHOD AddItem( nPos1, nPos2, nType ) CLASS Hili

   IF ::nItems > MAX_ITEMS
      RETURN Nil
   ELSEIF ::nItems >= Len( ::aLineStru )
      AAdd( ::aLineStru, Array( 3 ) )
   ENDIF
   ::nItems ++
   ::aLineStru[::nItems,1] := nPos1
   ::aLineStru[::nItems,2] := nPos2
   ::aLineStru[::nItems,3] := nType

   RETURN Nil

STATIC FUNCTION IsLetter( c )

   RETURN Len( c ) > 1 .OR. ( c >= "A" .AND. c <= "Z" ) .OR. ( c >= "a" .AND. c <= "z" ) .OR. ;
      c == "_" .OR. Asc( c ) >= 128
