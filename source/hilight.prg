/*
 * Hilighting class from HwGUI, adapted for a console editor (hbxml stuff removed)
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"

#define HILIGHT_GROUPS  4
#define HILIGHT_KEYW    1
#define HILIGHT_FUNC    2
#define HILIGHT_QUOTE   3
#define HILIGHT_COMM    4

#define MAX_ITEMS    1024

STATIC cSpaces := e" \t", cQuotes := e"\"\'"

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

   DATA   cCommands                // A list of keywords (commands), divided by space
   DATA   cFuncs                   // A list of keywords (functions), divided by space
   DATA   cScomm                   // A string, which starts single line comments
   DATA   cMcomm1, cMcomm2         // Start and end strings for multiline comments

   DATA   lMultiComm
   DATA   aDop, nDopChecked

   METHOD New( hHili, cCommands, cFuncs, cSComm, cMComm, lCase )
   METHOD SET( oEdit )
   METHOD DO( nLine, lCheck )
   METHOD UpdSource( nLine )  INLINE  ( ::nDopChecked := nLine - 1 )
   METHOD AddItem( nPos1, nPos2, nType )

ENDCLASS

METHOD New( hHili, cCommands, cFuncs, cSComm, cMComm, lCase ) CLASS Hili
   LOCAL nPos

   ::aLineStru := Array( 20, 3 )

   IF !Empty( hHili )
      cCommands := hb_hGetDef( hHili, "commands", "" )
      cFuncs := hb_hGetDef( hHili, "funcs", "" )
      cSComm := hb_hGetDef( hHili, "scomm", "" )
      cMComm := hb_hGetDef( hHili, "mcomm", "" )
      lCase := hb_hGetDef( hHili, "case", .F. )
   ENDIF
   IF !Empty( cCommands )
      ::cCommands := " " + AllTrim( cCommands ) + " "
   ENDIF
   IF !Empty( cFuncs )
      ::cFuncs := " " + AllTrim( cFuncs ) + " "
   ENDIF
   IF !Empty( cSComm )
      ::cScomm := AllTrim( cScomm )
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

   IF !::lCase
      IF !Empty( ::cCommands )
         ::cCommands := Lower( ::cCommands )
      ENDIF
      IF !Empty( ::cFuncs )
         ::cFuncs := Lower( ::cFuncs )
      ENDIF
   ENDIF

   RETURN Self

METHOD SET( oEdit ) CLASS Hili
   LOCAL oHili := Hili():New()

   oHili:cCommands := ::cCommands
   oHili:cFuncs    := ::cFuncs
   oHili:cScomm    := ::cScomm
   oHili:cMcomm1   := ::cMcomm1
   oHili:cMcomm2   := ::cMcomm2
   oHili:oEdit     := oEdit

   RETURN oHili

/*  Scans the cLine and fills an array :aLineStru with hilighted items
 *  lComm set it to .T., if a previous line was a part of an unclosed multiline comment
 *  lCheck - if .T., checks for multiline comments only
 */

METHOD DO( nLine, lCheck ) CLASS Hili
   LOCAL aText, cLine, nLen, nLenS, nLenM, i, lComm
   LOCAL cs, cm
   LOCAL nPos, nPos1, nPrev, cWord, c

   ::nItems := 0
   ::lMultiComm := .F.

   IF lCheck == Nil
      lCheck := .F.
   ELSEIF lCheck .AND. Empty( ::cMcomm1 )
      RETURN Nil
   ENDIF

   aText := ::oEdit:aText
   cLine := aText[nLine]
   nLen := cp_Len( ::oEdit:lUtf8, cLine )

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
   ::nDopChecked := nLine
   ::aDop[nLine] := 0

   IF Empty( ::cMcomm1 )
      cm := ""
   ELSE
      cm := Left( ::cMcomm1, 1 )
      nLenM := Len( ::cMcomm1 )
   ENDIF

   IF lComm != Nil .AND. lComm
      IF ( nPos := At( ::cMcomm2, cLine ) ) == 0
         IF !lCheck; ::AddItem( 1, cp_Len( ::oEdit:lUtf8,cLine ), HILIGHT_COMM ); ENDIF
         ::lMultiComm := .T.
         ::aDop[nLine] := 1
         RETURN Nil
      ELSE
         IF !lCheck; ::AddItem( 1, nPos, HILIGHT_COMM ); ENDIF
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


   DO WHILE nPos <= nLen
      DO WHILE nPos <= nLen .AND. cp_Substr( ::oEdit:lUtf8, cLine, nPos, 1 ) $ cSpaces; nPos ++ ; ENDDO
      DO WHILE nPos <= nLen
         IF ( c := cp_Substr( ::oEdit:lUtf8,cLine,nPos,1 ) ) $ cQuotes
            nPos1 := nPos
            IF ( nPos := cp_At( ::oEdit:lUtf8, c, cLine, nPos1 + 1 ) ) == 0
               nPos := cp_Len( ::oEdit:lUtf8, cLine )
            ENDIF
            IF !lCheck; ::AddItem( nPos1, nPos, HILIGHT_QUOTE ); ENDIF

         ELSEIF c == cs .AND. cp_Substr( ::oEdit:lUtf8, cLine, nPos, nLenS ) == ::cScomm
            IF !lCheck; ::AddItem( nPos, cp_Len( ::oEdit:lUtf8, cLine ), HILIGHT_COMM ); ENDIF
            nPos := cp_Len( ::oEdit:lUtf8, cLine ) + 1
            EXIT

         ELSEIF c == cm .AND. cp_Substr( ::oEdit:lUtf8, cLine, nPos, nLenM ) == ::cMcomm1
            nPos1 := nPos
            IF ( nPos := cp_At( ::oEdit:lUtf8, ::cMcomm2, cLine, nPos1 + 1 ) ) == 0
               nPos := cp_Len( ::oEdit:lUtf8, cLine )
               ::lMultiComm := .T.
               ::aDop[nLine] := 1
            ENDIF
            IF !lCheck; ::AddItem( nPos1, nPos, HILIGHT_COMM ); ENDIF
            nPos += nLenM - 1

         ELSEIF !lCheck .AND. IsLetter( c )
            nPos1 := nPos
            nPrev := nPos
            nPos := cp_NextPos( ::oEdit:lUtf8, cLine, nPos )
            DO WHILE IsLetter( cp_Substr( ::oEdit:lUtf8, cLine,nPos,1 ) )
               nPrev := nPos
               nPos := cp_NextPos( ::oEdit:lUtf8, cLine, nPos )
            ENDDO
            cWord := " " + iif( ::lCase, cp_Substr( ::oEdit:lUtf8, cLine, nPos1, nPos - nPos1 ), ;
               Lower( cp_Substr( ::oEdit:lUtf8, cLine, nPos1, nPos - nPos1 ) ) ) + " "
            nPos := nPrev
            IF !Empty( ::cCommands ) .AND. cWord $ ::cCommands
               ::AddItem( nPos1, nPos, HILIGHT_KEYW )
            ELSEIF !Empty( ::cFuncs ) .AND. cWord $ ::cFuncs
               ::AddItem( nPos1, nPos, HILIGHT_FUNC )
            ENDIF

         ENDIF
         nPos := cp_NextPos( ::oEdit:lUtf8, cLine, nPos )
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
